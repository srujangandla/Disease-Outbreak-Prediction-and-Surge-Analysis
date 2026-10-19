library(here)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(randomForest)

set.seed(42)
fig_dir   <- here("outputs", "figures", "regression")
tab_dir   <- here("outputs", "tables")
model_dir <- here("outputs", "models")
for (d in c(fig_dir, tab_dir, model_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Data and predictors -------------------------------------------------
md <- read_csv(here("data", "final", "modeling_data.csv"), show_col_types = FALSE) %>%
  mutate(
    has_history = as.integer(has_history),
    across(c(preci_missing, lai_missing, temp_missing), as.integer),
    log_days_since_prev = log1p(days_since_prev),
    log_n_prior = log1p(n_prior_reports)
  )

# Target: log_cases. Excluded on purpose: Cases, high_outbreak, date, split,
# district, state_ut (too many levels), year_index (cannot extrapolate to new years),
# interaction terms (the Random Forest finds interactions itself).
predictors <- c("Disease", "season", "zone", "Latitude", "Longitude",
                "Temp_C", "preci_log", "LAI", "month_sin", "month_cos",
                "prev_log_cases", "roll3_log_cases", "log_days_since_prev", "log_n_prior",
                "has_history", "preci_missing", "lai_missing", "temp_missing")
f <- reformulate(predictors, response = "log_cases")

train  <- filter(md, split == "train")
test   <- filter(md, split == "test")
stress <- filter(md, split == "stress")

# Factor levels fixed from the training data
lv <- list(Disease = sort(unique(train$Disease)),
           season  = sort(unique(train$season)),
           zone    = sort(unique(train$zone)))
set_levels <- function(d) {
  d <- d %>% mutate(Disease = factor(Disease, levels = lv$Disease),
                    season  = factor(season,  levels = lv$season),
                    zone    = factor(zone,    levels = lv$zone))
  n_bad <- sum(is.na(d$Disease) | is.na(d$season) | is.na(d$zone))
  if (n_bad > 0) message("Dropping ", n_bad, " rows with categories unseen in training.")
  d %>% filter(!is.na(Disease), !is.na(season), !is.na(zone))
}
train <- set_levels(train); test <- set_levels(test); stress <- set_levels(stress)
cat("Rows - train:", nrow(train), "| test:", nrow(test), "| stress:", nrow(stress), "\n")

as_df <- function(d) as.data.frame(d[, c("log_cases", predictors)])

# ---- 2. Baseline: disease median from training data -------------------------
base <- train %>% group_by(Disease) %>% summarise(Baseline = median(log_cases), .groups = "drop")

# ---- 3. Multiple linear regression ------------------------------------------
lm_fit <- lm(f, data = as_df(train))
sm <- summary(lm_fit)
cat("\nLinear regression: R2 =", round(sm$r.squared, 3),
    "| adjusted R2 =", round(sm$adj.r.squared, 3), "\n")

coefs <- as.data.frame(sm$coefficients)
ci <- confint(lm_fit)[rownames(coefs), , drop = FALSE]
coefs <- cbind(term = rownames(coefs), coefs, ci)
rownames(coefs) <- NULL
write_csv(coefs, file.path(tab_dir, "regression_lm_coefficients.csv"))

# ---- 4. Random forest (mtry chosen on time-ordered validation) --------------
tr_in <- filter(train, year <= 2015)
tr_va <- filter(train, year >  2015)
grid  <- c(3, 6, 9, 12)
val_rmse <- sapply(grid, function(m) {
  fit <- randomForest(f, data = as_df(tr_in), ntree = 300, mtry = m)
  sqrt(mean((tr_va$log_cases - predict(fit, tr_va[, predictors]))^2))
})
best_mtry <- grid[which.min(val_rmse)]
cat("\nmtry validation RMSE:\n"); print(data.frame(mtry = grid, rmse = round(val_rmse, 4)))
cat("Chosen mtry:", best_mtry, "\n")

rf_fit <- randomForest(f, data = as_df(train), ntree = 500, mtry = best_mtry, importance = TRUE)
cat("Random forest out-of-bag R2:", round(tail(rf_fit$rsq, 1), 3), "\n")

imp <- importance(rf_fit, type = 1)
rf_importance <- data.frame(variable = rownames(imp), pct_inc_mse = imp[, 1]) %>%
  arrange(desc(pct_inc_mse))
write_csv(rf_importance, file.path(tab_dir, "regression_rf_importance.csv"))

# ---- 5. Predictions on test and stress sets ---------------------------------
sets <- list(test = test, stress = stress)
pred_df <- bind_rows(lapply(names(sets), function(s) {
  d <- sets[[s]]
  d %>%
    left_join(base, by = "Disease") %>%
    transmute(set = s, Disease, year, has_history, actual = log_cases, Baseline,
              `Linear Regression` = as.numeric(predict(lm_fit, newdata = as.data.frame(d[, predictors]))),
              `Random Forest`     = as.numeric(predict(rf_fit, newdata = as.data.frame(d[, predictors]))))
}))
write_csv(pred_df, file.path(tab_dir, "regression_predictions.csv"))

long <- pred_df %>%
  pivot_longer(c(Baseline, `Linear Regression`, `Random Forest`),
               names_to = "model", values_to = "pred")

# ---- 6. Evaluation ----------------------------------------------------------
summ <- function(d, ...) {
  d %>% group_by(...) %>%
    summarise(n = n(),
              RMSE = sqrt(mean((actual - pred)^2)),
              MAE = mean(abs(actual - pred)),
              R2 = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2),
              MAE_cases = mean(abs(expm1(actual) - expm1(pred))),
              .groups = "drop")
}
metrics_table <- summ(long, set, model)
by_history    <- summ(long, set, model, has_history)
write_csv(metrics_table, file.path(tab_dir, "regression_metrics.csv"))
write_csv(by_history,    file.path(tab_dir, "regression_metrics_by_history.csv"))

cat("\nModel comparison (log scale; MAE_cases is on the original scale):\n")
print(as.data.frame(metrics_table), digits = 3)
cat("\nBy whether the district-disease pair has an earlier report:\n")
print(as.data.frame(by_history), digits = 3)

# ---- 7. Figures -------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))

p <- ggplot(filter(long, set == "test", model != "Baseline"), aes(pred, actual)) +
  geom_point(alpha = 0.3, colour = "steelblue") +
  geom_abline(colour = "red") +
  facet_wrap(~model) +
  labs(title = "Predicted vs actual log(Cases), test years 2018-2019",
       x = "Predicted", y = "Actual")
ggsave(file.path(fig_dir, "01_pred_vs_actual_test.png"), p, width = 9, height = 5, dpi = 300)

p <- ggplot(data.frame(fitted = fitted(lm_fit), resid = resid(lm_fit)), aes(fitted, resid)) +
  geom_point(alpha = 0.3, colour = "steelblue") +
  geom_hline(yintercept = 0, colour = "red") +
  labs(title = "Linear regression residuals vs fitted (training)", x = "Fitted", y = "Residual")
ggsave(file.path(fig_dir, "02_lm_residuals.png"), p, width = 8, height = 5, dpi = 300)

p <- ggplot(rf_importance, aes(reorder(variable, pct_inc_mse), pct_inc_mse)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Random forest permutation importance", x = NULL, y = "% increase in MSE")
ggsave(file.path(fig_dir, "03_rf_importance.png"), p, width = 8, height = 6, dpi = 300)

p <- ggplot(filter(long, set == "test"), aes(model, abs(actual - pred), fill = model)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
  labs(title = "Absolute error on test years (log scale)", x = NULL, y = "Absolute error")
ggsave(file.path(fig_dir, "04_error_by_model.png"), p, width = 8, height = 5, dpi = 300)

# ---- 8. Save models ---------------------------------------------------------
saveRDS(lm_fit, file.path(model_dir, "lm_regression.rds"))
saveRDS(rf_fit, file.path(model_dir, "rf_regression.rds"))
message("Regression finished. Tables, figures and models saved under outputs/.")