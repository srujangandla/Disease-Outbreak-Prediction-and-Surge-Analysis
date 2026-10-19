library(dplyr)
library(ggplot2)
library(ranger)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures/regression", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures/classification", recursive = TRUE, showWarnings = FALSE)

d <- read.csv("data/final/modeling_data.csv", stringsAsFactors = TRUE)
d$high_outbreak <- factor(ifelse(d$high_outbreak %in% c("High", 1, "1", TRUE, "TRUE"),
                                 "High", "Low"), levels = c("Low", "High"))
d$is_monsoon <- as.numeric(d$is_monsoon)
d$has_history <- as.numeric(d$has_history)

train <- d %>% filter(split == "train")

# Predictors. Cases and log_cases are excluded because the target is built from them.
predictors <- c("prev_log_cases", "roll3_log_cases", "days_since_prev",
                "n_prior_reports", "has_history",
                "Temp_C", "preci_log", "LAI",
                "temp_x_precip", "temp_x_lai", "hot_wet",
                "season", "is_monsoon", "month_sin", "month_cos", "year_index",
                "Disease", "zone", "Latitude", "Longitude")

# Map each predictor to one of your factor themes
groups <- data.frame(
  variable = predictors,
  group = c(rep("Past cases", 5),
            "Temperature", "Rainfall", "Vegetation (LAI)",
            rep("Climate interaction", 3),
            rep("Season / time", 5),
            "Disease", rep("Location", 3)),
  stringsAsFactors = FALSE
)

# ---------- 1. Random Forest permutation importance ----------
set.seed(42)
rf_reg <- ranger(log_cases ~ ., data = train[, c("log_cases", predictors)],
                 num.trees = 500, importance = "permutation", seed = 42)
rf_clf <- ranger(high_outbreak ~ ., data = train[, c("high_outbreak", predictors)],
                 num.trees = 500, probability = TRUE,
                 importance = "permutation", seed = 42)

tidy_imp <- function(model, label) {
  data.frame(variable = names(model$variable.importance),
             importance = as.numeric(model$variable.importance),
             target = label, stringsAsFactors = FALSE) %>%
    left_join(groups, by = "variable") %>%
    arrange(desc(importance))
}
imp <- bind_rows(tidy_imp(rf_reg, "Case count (log_cases)"),
                 tidy_imp(rf_clf, "Surge (high_outbreak)"))
write.csv(imp, "outputs/tables/rf_variable_importance.csv", row.names = FALSE)

# Theme-level view (summed permutation importance; correlated predictors share credit)
group_imp <- imp %>%
  group_by(target, group) %>%
  summarise(importance = sum(pmax(importance, 0)), .groups = "drop") %>%
  group_by(target) %>%
  mutate(share = round(100 * importance / sum(importance), 1)) %>%
  arrange(target, desc(share))
print(group_imp)
write.csv(group_imp, "outputs/tables/rf_group_importance.csv", row.names = FALSE)

plot_imp <- function(target_label, file) {
  p <- imp %>% filter(target == target_label) %>% slice_max(importance, n = 15) %>%
    ggplot(aes(reorder(variable, importance), importance, fill = group)) +
    geom_col() + coord_flip() +
    labs(title = paste("Random Forest permutation importance:", target_label),
         x = NULL, y = "Importance", fill = "Theme") +
    theme_minimal()
  ggsave(file, p, width = 8, height = 5, dpi = 300)
}
plot_imp("Case count (log_cases)", "outputs/figures/regression/rf_importance_regression.png")
plot_imp("Surge (high_outbreak)", "outputs/figures/classification/rf_importance_surge.png")

# ---------- 2. Standardised linear / logistic coefficients ----------
# Interaction terms and lat/long are dropped here to limit collinearity
lin_vars <- setdiff(predictors, c("temp_x_precip", "temp_x_lai", "hot_wet",
                                  "Latitude", "Longitude"))
num_vars <- lin_vars[sapply(train[, lin_vars], is.numeric)]
tr_s <- train
tr_s[num_vars] <- scale(tr_s[num_vars])

# --- Linear Regression ---
lm_fit <- lm(log_cases ~ ., data = tr_s[, c("log_cases", lin_vars)])

# FIX: Extract rows explicitly estimated in the summary matrix to prevent dimension mismatches
lm_coef_matrix <- summary(lm_fit)$coefficients
lm_tab <- data.frame(term = rownames(lm_coef_matrix), lm_coef_matrix, row.names = NULL)

names(lm_tab) <- c("term", "estimate", "std_error", "t_value", "p_value")
lm_tab <- lm_tab %>% filter(term != "(Intercept)") %>% arrange(p_value)
print(head(lm_tab, 15))
write.csv(lm_tab, "outputs/tables/lm_standardised_coefficients.csv", row.names = FALSE)

# --- Logistic Regression ---
glm_fit <- glm(high_outbreak ~ ., data = tr_s[, c("high_outbreak", lin_vars)],
               family = binomial())

# FIX: Extract rows explicitly estimated in the summary matrix to prevent dimension mismatches
glm_coef_matrix <- summary(glm_fit)$coefficients
glm_tab <- data.frame(term = rownames(glm_coef_matrix), glm_coef_matrix, row.names = NULL)

names(glm_tab) <- c("term", "log_odds", "std_error", "z_value", "p_value")
glm_tab$odds_ratio <- exp(glm_tab$log_odds)
glm_tab <- glm_tab %>% filter(term != "(Intercept)") %>% arrange(p_value)
print(head(glm_tab, 15))
write.csv(glm_tab, "outputs/tables/logit_odds_ratios.csv", row.names = FALSE)