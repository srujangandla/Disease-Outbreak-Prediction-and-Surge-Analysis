# ==============================================================================
# Statistical Analysis & Multicollinearity Diagnostics
# ==============================================================================

library(here)
library(dplyr)
library(readr)
library(purrr)

# --- Configuration & Setup ---------------------------------------------------
tab_dir <- here("outputs", "tables")
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(42)

# --- Data Loading & Preprocessing --------------------------------------------
df <- read_csv(here("data", "final", "modeling_data.csv"), show_col_types = FALSE)

# Filter to training split if present
if ("split" %in% names(df)) {
  df <- df %>% filter(split == "train")
  message("Using training rows only.")
} else {
  message("No split column found: using ALL rows.")
}

# Ensure correct factor levels for the target variable
df <- df %>% mutate(high_outbreak = factor(high_outbreak, levels = c("Low", "High")))
cat("Rows used:", nrow(df), "\n")


# Excludes rows where climate variables were imputed to avoid statistical bias
climate_flags <- c(Temp_C = "temp_missing", preci_log = "preci_missing", LAI = "lai_missing")

get_valid_rows <- function(var_name) {
  flag_col <- climate_flags[var_name]
  if (!is.na(flag_col) && flag_col %in% names(df)) {
    return(!df[[flag_col]])
  }
  rep(TRUE, nrow(df))
}

#Distribution of the Outcome Variable
calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

cat("\n--- Distribution Metrics ---\n")
cat("Skewness - Cases:", round(calc_skewness(df$Cases), 2), 
    "| log_cases:", round(calc_skewness(df$log_cases), 2), "\n")

# Shapiro-Wilk Test for Normality (sampled down to 5000 rows max)
shapiro_res <- shapiro.test(sample(df$log_cases, min(5000, nrow(df))))
print(shapiro_res)


# 2. Kruskal-Wallis: Multi-group Categorical Tests against log_cases
kw_test <- function(var) {
  g <- df[[var]]
  t <- kruskal.test(df$log_cases ~ g)
  n <- sum(!is.na(g))
  
  data.frame(
    variable   = var, 
    H          = unname(t$statistic), 
    dof        = unname(t$parameter),
    p_value    = t$p.value, 
    epsilon_sq = unname(t$statistic) * (n + 1) / (n^2 - 1)
  )
}

kw_vars  <- intersect(c("Disease", "season", "zone", "state_ut"), names(df))
kw_table <- map_dfr(kw_vars, kw_test)

cat("\n--- Kruskal-Wallis Tests ---\n")
print(kw_table)

# Post-hoc pairwise comparison for seasons
if ("season" %in% names(df)) {
  cat("\nPairwise season comparisons (Holm adjusted):\n")
  pairwise_res <- suppressWarnings(
    pairwise.wilcox.test(df$log_cases, df$season, p.adjust.method = "holm", exact = FALSE)
  )
  print(pairwise_res)
}

#Spearman Rank Correlation with log_cases

num_vars <- intersect(c(
  "Temp_C", "preci_log", "LAI", "prev_log_cases", "roll3_log_cases",
  "days_since_prev", "n_prior_reports", "Latitude", "Longitude",
  "year_index", "month_sin", "month_cos"
), names(df))

sp_table <- map_dfr(num_vars, function(v) {
  valid_mask <- get_valid_rows(v) & complete.cases(df$log_cases, df[[v]])
  
  t <- suppressWarnings(
    cor.test(df$log_cases[valid_mask], df[[v]][valid_mask], method = "spearman", exact = FALSE)
  )
  
  data.frame(
    variable = v, 
    n        = sum(valid_mask), 
    rho      = unname(t$estimate), 
    p_value  = t$p.value
  )
}) %>%
  mutate(p_holm = p.adjust(p_value, "holm")) %>%
  arrange(desc(abs(rho)))

cat("\n--- Spearman Correlations with log_cases ---\n")
print(sp_table)

# 4. Chi-Square Test of Independence for Binary Outbreak Class
chi_test <- function(var) {
  tab <- table(droplevels(factor(df[[var]])), df$high_outbreak)
  t   <- chisq.test(tab, simulate.p.value = TRUE, B = 5000)
  
  data.frame(
    variable  = var, 
    chi_sq    = unname(t$statistic), 
    p_value   = t$p.value,
    cramers_v = sqrt(unname(t$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
  )
}

chi_vars  <- intersect(c("season", "zone"), names(df))
chi_table <- map_dfr(chi_vars, chi_test)

cat("\n--- Chi-Square Tests ---\n")
print(chi_table)

# Climate Profiling: High vs Low Outbreaks (Mann-Whitney U)
clim_vars <- intersect(names(climate_flags), names(df))

clim_table <- map_dfr(clim_vars, function(v) {
  valid_mask <- get_valid_rows(v) & !is.na(df[[v]])
  x <- df[[v]][valid_mask]
  g <- df$high_outbreak[valid_mask]
  
  t <- suppressWarnings(wilcox.test(x ~ g, exact = FALSE))
  
  data.frame(
    variable    = v, 
    n           = sum(valid_mask), 
    median_low  = median(x[g == "Low"]),
    median_high = median(x[g == "High"]), 
    p_value     = t$p.value
  )
})

cat("\n--- Climate Context by Outbreak Class ---\n")
print(clim_table)


# Multicollinearity Diagnostics (Variance Inflation Factors)

vif_vars <- intersect(c(
  "Temp_C", "preci_log", "LAI", "prev_log_cases", "roll3_log_cases",
  "days_since_prev", "n_prior_reports", "temp_x_precip", "temp_x_lai",
  "Latitude", "Longitude", "year_index", "month_sin", "month_cos"
), names(df))

# Isolate feature space and drop rows containing missing values
X <- df %>% select(all_of(vif_vars)) %>% filter(complete.cases(.))

vif_table <- data.frame(
  variable = vif_vars,
  vif = sapply(vif_vars, function(v) {
    formula_obj <- reformulate(setdiff(vif_vars, v), response = v)
    r_squared   = summary(lm(formula_obj, data = X))$r.squared
    1 / (1 - r_squared)
  })
) %>% arrange(desc(vif))

cat("\n--- Variance Inflation Factors (VIF > 5 indicates potential collinearity) ---\n")
print(vif_table)



#Exporting Outputs
write_csv(kw_table,   file.path(tab_dir, "stats_kruskal.csv"))
write_csv(sp_table,   file.path(tab_dir, "stats_spearman.csv"))
write_csv(chi_table,  file.path(tab_dir, "stats_chisq.csv"))
write_csv(clim_table, file.path(tab_dir, "stats_climate_by_outbreak.csv"))
write_csv(vif_table,  file.path(tab_dir, "stats_vif.csv"))

message("\nAll tables successfully generated and saved in: ", tab_dir)
