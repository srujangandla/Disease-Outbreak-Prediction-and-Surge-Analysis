library(dplyr)
library(ggplot2)
library(ranger)

set.seed(42)
for (p in c("outputs/tables", "outputs/models",
            "outputs/figures/regression", "outputs/figures/classification"))
  dir.create(p, recursive = TRUE, showWarnings = FALSE)

MTRY_REG  <- 9
MTRY_CLF  <- 3
THR_LR    <- 0.22
THR_RF    <- 0.27
N_BOOT    <- 1000

d <- read.csv("data/final/modeling_data.csv", stringsAsFactors = TRUE)
d$high   <- as.integer(d$high_outbreak %in% c("High", 1, "1", TRUE, "TRUE"))
d$high_f <- factor(ifelse(d$high == 1, "High", "Low"), levels = c("Low", "High"))
d$is_monsoon  <- as.numeric(d$is_monsoon)
d$has_history <- as.numeric(d$has_history)

train  <- d %>% filter(split == "train")
test   <- d %>% filter(split == "test")
stress <- d %>% filter(split == "stress")

# Keep this list identical to the one used in 08_regression.R and 09_classification.R.
predictors <- c("prev_log_cases", "roll3_log_cases", "days_since_prev",
                "n_prior_reports", "has_history",
                "Temp_C", "preci_log", "LAI",
                "temp_x_precip", "temp_x_lai", "hot_wet",
                "season", "is_monsoon", "month_sin", "month_cos", "year_index",
                "Disease", "zone", "Latitude", "Longitude")
# is_monsoon is fully determined by season, so it is dropped for the linear models
lin_pred <- setdiff(predictors, "is_monsoon")

lm_fit  <- lm(log_cases ~ ., data = train[, c("log_cases", lin_pred)])
glm_fit <- suppressWarnings(
  glm(high_f ~ ., data = train[, c("high_f", lin_pred)], family = binomial()))
rf_reg  <- ranger(log_cases ~ ., data = train[, c("log_cases", predictors)],
                  num.trees = 500, mtry = MTRY_REG, seed = 42)
rf_clf  <- ranger(high_f ~ ., data = train[, c("high_f", predictors)],
                  num.trees = 500, mtry = MTRY_CLF, probability = TRUE, seed = 42)

saveRDS(list(lm = lm_fit, glm = glm_fit, rf_reg = rf_reg, rf_clf = rf_clf),
        "outputs/models/final_models.rds")

thr_tbl <- train %>% filter(high == 1) %>% group_by(Disease) %>%
  summarise(min_high = min(Cases), .groups = "drop")

add_preds <- function(df) {
  df$pred_base <- mean(train$log_cases)
  df$pred_lm   <- suppressWarnings(predict(lm_fit, df))
  df$pred_rf   <- predict(rf_reg, df)$predictions
  df$p_lr      <- suppressWarnings(predict(glm_fit, df, type = "response"))
  df$p_rf      <- predict(rf_clf, df)$predictions[, "High"]
  thr          <- thr_tbl$min_high[match(df$Disease, thr_tbl$Disease)]
  df$rule_prev <- as.integer(df$has_history == 1 & expm1(df$prev_log_cases) >= thr)
  df$rule_prev[is.na(df$rule_prev)] <- 0L
  df
}
test   <- add_preds(test)
stress <- add_preds(stress)


reg_metrics <- function(y, pred, model) {
  data.frame(model = model, n = length(y),
             RMSE = sqrt(mean((y - pred)^2)),
             MAE  = mean(abs(y - pred)),
             R2   = 1 - sum((y - pred)^2) / sum((y - mean(y))^2),
             MAE_cases = mean(abs(expm1(y) - expm1(pred))))
}

roc_auc <- function(y, s) {
  n1 <- sum(y == 1); n0 <- sum(y == 0); r <- rank(s)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

curve_points <- function(y, s) {
  o <- order(s, decreasing = TRUE); y <- y[o]; s <- s[o]
  tp <- cumsum(y); fp <- cumsum(1 - y)
  last <- c(s[-length(s)] != s[-1], TRUE)
  tp <- tp[last]; fp <- fp[last]
  data.frame(tpr = tp / sum(y), fpr = fp / sum(1 - y), precision = tp / (tp + fp))
}

pr_auc <- function(y, s) {
  cp <- curve_points(y, s)
  sum(diff(c(0, cp$tpr)) * cp$precision)
}

clf_metrics <- function(y, score, pred, model) {
  tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1); tn <- sum(pred == 0 & y == 0)
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA
  recall    <- tp / (tp + fn)
  f1 <- if (!is.na(precision) && precision + recall > 0)
    2 * precision * recall / (precision + recall) else 0
  data.frame(model = model, n = length(y),
             accuracy = (tp + tn) / length(y), precision = precision,
             recall = recall, F1 = f1, specificity = tn / (tn + fp),
             ROC_AUC = roc_auc(y, score), PR_AUC = pr_auc(y, score),
             prevalence = mean(y))
}

evaluate <- function(df, set_name, subset_label) {
  y <- df$high; n <- nrow(df)
  reg <- bind_rows(
    reg_metrics(df$log_cases, df$pred_base, "Baseline (train mean)"),
    reg_metrics(df$log_cases, df$pred_lm,   "Linear Regression"),
    reg_metrics(df$log_cases, df$pred_rf,   "Random Forest"))
  clf <- bind_rows(
    clf_metrics(y, df$p_lr, as.integer(df$p_lr >= THR_LR), "Logistic Regression"),
    clf_metrics(y, df$p_rf, as.integer(df$p_rf >= THR_RF), "Random Forest"),
    clf_metrics(y, df$rule_prev, df$rule_prev, "Previous report rule"),
    clf_metrics(y, rep(0, n), rep(0L, n), "Always Low"))
  list(reg = cbind(set = set_name, subset = subset_label, reg),
       clf = cbind(set = set_name, subset = subset_label, clf))
}

# ---------------------------------------------------------------- evaluate
reg_all <- list(); clf_all <- list()
for (s in c("test", "stress")) {
  df <- get(s)
  for (h in c("All", "has_history=1", "has_history=0")) {
    sub <- switch(h, "All" = df,
                  "has_history=1" = df[df$has_history == 1, ],
                  "has_history=0" = df[df$has_history == 0, ])
    r <- evaluate(sub, s, h)
    reg_all[[paste(s, h)]] <- r$reg
    clf_all[[paste(s, h)]] <- r$clf
  }
}
reg_tab <- bind_rows(reg_all); clf_tab <- bind_rows(clf_all)
num_cols <- function(x) mutate(x, across(where(is.numeric) & !matches("^n$"), ~ round(.x, 3)))
write.csv(num_cols(reg_tab), "outputs/tables/eval_regression.csv", row.names = FALSE)
write.csv(num_cols(clf_tab), "outputs/tables/eval_classification.csv", row.names = FALSE)

cat("\n=== Regression (log scale; MAE_cases on case scale) ===\n")
print(num_cols(reg_tab %>% filter(subset == "All")), row.names = FALSE)
cat("\n=== Surge classification ===\n")
print(num_cols(clf_tab %>% filter(subset == "All")), row.names = FALSE)


cm <- function(y, p) table(Actual = factor(ifelse(y == 1, "High", "Low"), c("Low", "High")),
                           Predicted = factor(ifelse(p == 1, "High", "Low"), c("Low", "High")))
pred_lr_test <- as.integer(test$p_lr >= THR_LR)
pred_rf_test <- as.integer(test$p_rf >= THR_RF)
cat("\nConfusion matrix, test, Logistic Regression:\n"); print(cm(test$high, pred_lr_test))
cat("\nConfusion matrix, test, Random Forest:\n");       print(cm(test$high, pred_rf_test))

cm_df <- bind_rows(
  as.data.frame(cm(test$high, pred_lr_test)) %>% mutate(model = "Logistic Regression"),
  as.data.frame(cm(test$high, pred_rf_test)) %>% mutate(model = "Random Forest"))
p_cm <- ggplot(cm_df, aes(Predicted, Actual, fill = Freq)) +
  geom_tile() + geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "#0f6b6b") +
  facet_wrap(~ model) + labs(title = "Confusion matrices (test set)") + theme_minimal()
ggsave("outputs/figures/classification/confusion_matrices_test.png", p_cm,
       width = 8, height = 4, dpi = 300)


curves <- bind_rows(
  cbind(curve_points(test$high, test$p_lr), model = "Logistic Regression"),
  cbind(curve_points(test$high, test$p_rf), model = "Random Forest"))

p_roc <- ggplot(curves, aes(fpr, tpr, colour = model)) +
  geom_line(linewidth = 1) + geom_abline(linetype = "dashed", colour = "grey50") +
  labs(title = "ROC curves (test set)", x = "False positive rate", y = "Recall") +
  theme_minimal()
p_pr <- ggplot(curves, aes(tpr, precision, colour = model)) +
  geom_line(linewidth = 1) + geom_hline(yintercept = mean(test$high), linetype = "dashed",
                                        colour = "grey50") +
  labs(title = "Precision-recall curves (test set)",
       subtitle = "Dashed line = prevalence (always-High baseline)",
       x = "Recall", y = "Precision") + theme_minimal()
ggsave("outputs/figures/classification/roc_test.png", p_roc, width = 6, height = 5, dpi = 300)
ggsave("outputs/figures/classification/pr_test.png", p_pr, width = 6, height = 5, dpi = 300)

p_pa <- ggplot(test, aes(pred_rf, log_cases)) +
  geom_point(alpha = 0.3) + geom_abline(colour = "red") +
  labs(title = "Random Forest: predicted vs actual log cases (test)",
       x = "Predicted log_cases", y = "Actual log_cases") + theme_minimal()
ggsave("outputs/figures/regression/rf_pred_vs_actual_test.png", p_pa,
       width = 6, height = 5, dpi = 300)

cat("\n=== Paired t-test on squared errors (test): Linear vs Random Forest ===\n")
print(t.test((test$log_cases - test$pred_lm)^2, (test$log_cases - test$pred_rf)^2,
             paired = TRUE))

cat("\n=== McNemar test (test): Logistic vs Random Forest correctness ===\n")
ok_lr <- pred_lr_test == test$high; ok_rf <- pred_rf_test == test$high
print(mcnemar.test(table(LR_correct = ok_lr, RF_correct = ok_rf)))

f1_at <- function(y, p) {
  tp <- sum(p == 1 & y == 1); fp <- sum(p == 1 & y == 0); fn <- sum(p == 0 & y == 1)
  if (tp == 0) 0 else 2 * tp / (2 * tp + fp + fn)
}
rmse <- function(y, p) sqrt(mean((y - p)^2))

n <- nrow(test)
boot <- t(replicate(N_BOOT, {
  i <- sample.int(n, n, replace = TRUE); b <- test[i, ]
  c(RMSE_diff   = rmse(b$log_cases, b$pred_rf) - rmse(b$log_cases, b$pred_lm),
    F1_diff     = f1_at(b$high, as.integer(b$p_rf >= THR_RF)) -
      f1_at(b$high, as.integer(b$p_lr >= THR_LR)),
    PR_AUC_diff = pr_auc(b$high, b$p_rf) - pr_auc(b$high, b$p_lr),
    ROC_AUC_diff = roc_auc(b$high, b$p_rf) - roc_auc(b$high, b$p_lr))
}))
obs <- c(RMSE_diff   = rmse(test$log_cases, test$pred_rf) - rmse(test$log_cases, test$pred_lm),
         F1_diff     = f1_at(test$high, pred_rf_test) - f1_at(test$high, pred_lr_test),
         PR_AUC_diff = pr_auc(test$high, test$p_rf) - pr_auc(test$high, test$p_lr),
         ROC_AUC_diff = roc_auc(test$high, test$p_rf) - roc_auc(test$high, test$p_lr))
boot_tab <- data.frame(metric = names(obs), rf_minus_linear = round(obs, 4),
                       ci_low  = round(apply(boot, 2, quantile, 0.025), 4),
                       ci_high = round(apply(boot, 2, quantile, 0.975), 4),
                       row.names = NULL)
boot_tab$ci_excludes_zero <- boot_tab$ci_low > 0 | boot_tab$ci_high < 0
cat("\n=== Bootstrap 95% CI for RF minus linear/logistic (test) ===\n")
cat("RMSE_diff < 0 favours RF; the other differences > 0 favour RF.\n")
print(boot_tab, row.names = FALSE)
write.csv(boot_tab, "outputs/tables/eval_bootstrap_differences.csv", row.names = FALSE)