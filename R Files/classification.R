library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(randomForest)

set.seed(42)
fig_dir   <- here("outputs", "figures", "classification")
tab_dir   <- here("outputs", "tables")
model_dir <- here("outputs", "models")
for (d in c(fig_dir, tab_dir, model_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

md <- read_csv(here("data", "final", "modeling_data.csv"), show_col_types = FALSE) %>%
  mutate(
    high_outbreak = factor(high_outbreak, levels = c("Low", "High")),
    has_history = as.integer(has_history),
    across(c(preci_missing, lai_missing, temp_missing), as.integer),
    log_days_since_prev = log1p(days_since_prev),
    log_n_prior = log1p(n_prior_reports)
  )

# Never used as predictors: Cases, log_cases, date, split, district, state_ut, year_index
predictors <- c("Disease", "season", "zone", "Latitude", "Longitude",
                "Temp_C", "preci_log", "LAI", "month_sin", "month_cos",
                "prev_log_cases", "roll3_log_cases", "log_days_since_prev", "log_n_prior",
                "has_history", "preci_missing", "lai_missing", "temp_missing")
f <- reformulate(predictors, response = "high_outbreak")

train  <- filter(md, split == "train")
test   <- filter(md, split == "test")
stress <- filter(md, split == "stress")

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
as_df <- function(d) as.data.frame(d[, c("high_outbreak", predictors)])
yn <- function(d) as.integer(d$high_outbreak == "High")

cat("Rows - train:", nrow(train), "| test:", nrow(test), "| stress:", nrow(stress), "\n")
cat("High outbreaks - train:", sum(yn(train)), "| test:", sum(yn(test)),
    "| stress:", sum(yn(stress)), "\n")

# ---- 2. Metric helpers ------------------------------------------------------
auc_roc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
pr_auc <- function(y, p) {            # average precision
  o <- order(p, decreasing = TRUE); y <- y[o]
  prec <- cumsum(y) / seq_along(y)
  sum(prec[y == 1]) / sum(y)
}
class_metrics <- function(y, p, thr) {
  pred <- as.integer(p >= thr)
  tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1); tn <- sum(pred == 0 & y == 0)
  prec <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
  rec  <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
  f1   <- ifelse(is.na(prec) || is.na(rec) || prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
  data.frame(threshold = thr, accuracy = (tp + tn) / length(y), precision = prec,
             recall = rec, F1 = f1, specificity = tn / (tn + fp),
             ROC_AUC = auc_roc(y, p), PR_AUC = pr_auc(y, p), prevalence = mean(y))
}
best_threshold <- function(y, p) {
  grid <- seq(0.05, 0.95, by = 0.01)
  f1s <- sapply(grid, function(t) class_metrics(y, p, t)$F1)
  grid[which.max(f1s)]
}

# ---- 3. Model helpers -------------------------------------------------------
fit_lr <- function(d) glm(f, data = as_df(d), family = binomial)
fit_rf <- function(d, mtry, ntree) {
  randomForest(f, data = as_df(d), ntree = ntree, mtry = mtry, importance = TRUE)
}
prob_lr <- function(m, d) as.numeric(predict(m, newdata = as.data.frame(d[, predictors]), type = "response"))
prob_rf <- function(m, d) as.numeric(predict(m, newdata = as.data.frame(d[, predictors]), type = "prob")[, "High"])

# ---- 4. Tuning on a time-ordered validation split ---------------------------
tr_in <- filter(train, year <= 2015)
tr_va <- filter(train, year >  2015)
y_va  <- yn(tr_va)

grid <- c(3, 6, 9, 12)
val_pr <- sapply(grid, function(m) pr_auc(y_va, prob_rf(fit_rf(tr_in, m, 300), tr_va)))
best_mtry <- grid[which.max(val_pr)]
cat("\nmtry validation PR-AUC:\n"); print(data.frame(mtry = grid, pr_auc = round(val_pr, 4)))
cat("Chosen mtry:", best_mtry, "\n")

thr_lr <- best_threshold(y_va, prob_lr(fit_lr(tr_in), tr_va))
thr_rf <- best_threshold(y_va, prob_rf(fit_rf(tr_in, best_mtry, 300), tr_va))
cat("F1-optimal thresholds (validation) - LR:", thr_lr, "| RF:", thr_rf, "\n")

# ---- 5. Final models on all training years ----------------------------------
lr_fit <- fit_lr(train)
rf_fit <- fit_rf(train, best_mtry, 500)

sm <- summary(lr_fit)$coefficients
or_table <- data.frame(term = rownames(sm), odds_ratio = exp(sm[, 1]),
                       lower = exp(sm[, 1] - 1.96 * sm[, 2]),
                       upper = exp(sm[, 1] + 1.96 * sm[, 2]),
                       p_value = sm[, 4], row.names = NULL)
write_csv(or_table, file.path(tab_dir, "classification_lr_odds_ratios.csv"))

imp <- importance(rf_fit, type = 1)
rf_importance <- data.frame(variable = rownames(imp), mean_decrease_accuracy = imp[, 1]) %>%
  arrange(desc(mean_decrease_accuracy))
write_csv(rf_importance, file.path(tab_dir, "classification_rf_importance.csv"))

# ---- 6. Predictions and baselines -------------------------------------------
thr_cases <- read_csv(here("outputs", "tables", "outbreak_thresholds.csv"), show_col_types = FALSE)
thr_map <- setNames(thr_cases$outbreak_threshold, thr_cases$Disease)

sets <- list(test = test, stress = stress)
pred_df <- bind_rows(lapply(names(sets), function(s) {
  d <- sets[[s]]
  tc <- thr_map[as.character(d$Disease)]
  data.frame(set = s, Disease = d$Disease, year = d$year, has_history = d$has_history,
             actual = yn(d),
             `Logistic Regression` = prob_lr(lr_fit, d),
             `Random Forest` = prob_rf(rf_fit, d),
             `Previous report rule` = as.numeric(d$has_history == 1 & d$prev_log_cases > log1p(tc)),
             `Always Low` = 0,
             check.names = FALSE)
}))
write_csv(pred_df, file.path(tab_dir, "classification_predictions.csv"))

thresholds <- c(`Logistic Regression` = thr_lr, `Random Forest` = thr_rf,
                `Previous report rule` = 0.5, `Always Low` = 0.5)

# ---- 7. Evaluation ----------------------------------------------------------
eval_rows <- function(d, extra = NULL) {
  bind_rows(lapply(names(thresholds), function(m) {
    out <- class_metrics(d$actual, d[[m]], thresholds[[m]])
    if (m == "Always Low") { out$ROC_AUC <- 0.5; out$PR_AUC <- mean(d$actual) }
    data.frame(model = m, n = nrow(d), out, check.names = FALSE)
  }))
}
metrics_table <- bind_rows(lapply(names(sets), function(s) {
  data.frame(set = s, eval_rows(filter(pred_df, set == s)))
}))
write_csv(metrics_table, file.path(tab_dir, "classification_metrics.csv"))

hist_table <- bind_rows(lapply(names(sets), function(s) {
  bind_rows(lapply(c(0, 1), function(h) {
    d <- filter(pred_df, set == s, has_history == h)
    if (nrow(d) < 20 || sum(d$actual) == 0) return(NULL)
    data.frame(set = s, has_history = h, eval_rows(d))
  }))
}))
write_csv(hist_table, file.path(tab_dir, "classification_metrics_by_history.csv"))

cat("\nTest and stress performance:\n")
print(metrics_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nBy history (does the district-disease pair have an earlier report?):\n")
print(hist_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

cm <- function(model) {
  d <- filter(pred_df, set == "test")
  table(Actual = factor(d$actual, levels = c(0, 1), labels = c("Low", "High")),
        Predicted = factor(as.integer(d[[model]] >= thresholds[[model]]),
                           levels = c(0, 1), labels = c("Low", "High")))
}
cat("\nConfusion matrix, test - Logistic Regression:\n"); print(cm("Logistic Regression"))
cat("\nConfusion matrix, test - Random Forest:\n");        print(cm("Random Forest"))

# ---- 8. Figures -------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))

curve_points <- function(y, p, model) {
  th <- sort(unique(p), decreasing = TRUE)
  out <- t(sapply(th, function(t) {
    pred <- p >= t
    c(tpr = sum(pred & y == 1) / sum(y == 1),
      fpr = sum(pred & y == 0) / sum(y == 0),
      precision = sum(pred & y == 1) / sum(pred))
  }))
  data.frame(model = model, as.data.frame(out))
}
dt <- filter(pred_df, set == "test")
curves <- bind_rows(curve_points(dt$actual, dt[["Logistic Regression"]], "Logistic Regression"),
                    curve_points(dt$actual, dt[["Random Forest"]], "Random Forest"))

p <- ggplot(curves, aes(fpr, tpr, colour = model)) +
  geom_line(linewidth = 1) + geom_abline(linetype = "dashed") +
  labs(title = "ROC curves, test years 2018-2019", x = "False positive rate", y = "True positive rate")
ggsave(file.path(fig_dir, "01_roc_test.png"), p, width = 7, height = 5.5, dpi = 300)

p <- ggplot(curves, aes(tpr, precision, colour = model)) +
  geom_line(linewidth = 1) + geom_hline(yintercept = mean(dt$actual), linetype = "dashed") +
  labs(title = "Precision-recall curves, test years (dashed = prevalence)",
       x = "Recall", y = "Precision")
ggsave(file.path(fig_dir, "02_pr_test.png"), p, width = 7, height = 5.5, dpi = 300)

p <- ggplot(rf_importance, aes(reorder(variable, mean_decrease_accuracy), mean_decrease_accuracy)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Random forest permutation importance (classification)",
       x = NULL, y = "Mean decrease in accuracy")
ggsave(file.path(fig_dir, "03_rf_importance.png"), p, width = 8, height = 6, dpi = 300)

or_plot <- or_table %>% filter(term != "(Intercept)", is.finite(upper), upper < 50)
p <- ggplot(or_plot, aes(reorder(term, odds_ratio), odds_ratio)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), colour = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed") + coord_flip() +
  labs(title = "Logistic regression odds ratios (95% Wald interval)", x = NULL, y = "Odds ratio")
ggsave(file.path(fig_dir, "04_lr_odds_ratios.png"), p, width = 8, height = 7, dpi = 300)

# ---- 9. Save models ---------------------------------------------------------
saveRDS(lr_fit, file.path(model_dir, "lr_classification.rds"))
saveRDS(rf_fit, file.path(model_dir, "rf_classification.rds"))
message("Classification finished. Tables, figures and models saved under outputs/.")