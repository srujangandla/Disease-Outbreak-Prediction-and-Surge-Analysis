library(here)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

# ---- 1. Load Data & Resolve Missing Variables -------------------------------
df <- read_csv(here("data", "processed", "cleaned_epiclim.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# DYNAMIC FIX 1: Generate missing 'preci_log' if it doesn't exist
if (!"preci_log" %in% names(df) && "preci" %in% names(df)) {
  message("Column 'preci_log' not found. Creating it via log1p(preci)...")
  df <- df %>% mutate(preci_log = log1p(preci)) # log1p safely handles 0 precipitation days
}

# DYNAMIC FIX 2: Map zone using state_ut if the column does not exist
if (!"zone" %in% names(df)) {
  message("Column 'zone' not found. Mapping zones based on 'state_ut'...")
  df <- df %>%
    mutate(
      state_clean = trimws(tolower(as.character(state_ut))),
      zone = case_when(
        state_clean %in% c("jammu & kashmir", "jammu and kashmir", "himachal pradesh", "punjab", "haryana", "delhi", "chandigarh", "uttarakhand", "uttaranchal") ~ "North",
        state_clean %in% c("uttar pradesh", "madhya pradesh", "chhattisgarh") ~ "Central",
        state_clean %in% c("bihar", "jharkhand", "odisha", "orissa", "west bengal", "andaman & nicobar islands", "andaman and nicobar") ~ "East",
        state_clean %in% c("rajasthan", "gujarat", "maharashtra", "goa", "dadra & nagar haveli", "daman & diu", "dadra and nagar haveli and daman and diu") ~ "West",
        state_clean %in% c("andhra pradesh", "telangana", "karnataka", "kerala", "tamil nadu", "puducherry", "pondicherry", "lakshadweep") ~ "South",
        state_clean %in% c("assam", "arunachal pradesh", "manipur", "meghalaya", "mizoram", "nagaland", "sikkim", "tripura") ~ "North-East",
        TRUE ~ "Other"
      )
    ) %>%
    select(-state_clean)
}

# Apply factor styling safely
df <- df %>%
  mutate(
    season = factor(season, levels = c("Winter", "Summer", "Monsoon", "Post-monsoon")),
    across(any_of(c("state_ut", "district", "Disease", "zone")), as.factor)
  )

# Imputation function based on zone and month
impute_median <- function(data, var) {
  data %>%
    group_by(zone, month) %>%
    mutate("{var}" := ifelse(is.na(.data[[var]]),
                             median(.data[[var]], na.rm = TRUE), .data[[var]])) %>%
    ungroup() %>%
    group_by(zone) %>%
    mutate("{var}" := ifelse(is.na(.data[[var]]),
                             median(.data[[var]], na.rm = TRUE), .data[[var]])) %>%
    ungroup() %>%
    mutate("{var}" := ifelse(is.na(.data[[var]]),
                             median(.data[[var]], na.rm = TRUE), .data[[var]]))
}

# Impute median values sequentially now that all targets exist
for (v in c("Temp_C", "preci", "preci_log", "LAI")) {
  if (v %in% names(df)) df <- impute_median(df, v)
}

# ---- 2. Deaths features -----------------------------------------------------
df <- df %>%
  mutate(
    deaths_clean = ifelse(deaths_reported, Deaths, NA_real_),
    cfr = ifelse(deaths_reported, Deaths / Cases, NA_real_),
    any_death = ifelse(deaths_reported, as.integer(Deaths > 0), NA_integer_)
  )

# ---- 3. Temporal / history features ----------------------------------------
df <- df %>%
  arrange(district, Disease, date) %>%
  group_by(district, Disease) %>%
  mutate(
    prev_log_cases   = lag(log_cases),
    days_since_prev  = as.numeric(date - lag(date)),
    roll3_log_cases  = {
      x <- lag(log_cases)
      (x + lag(x) + lag(x, 2)) / 3
    },
    n_prior_reports  = row_number() - 1L,
    cases_growth     = log_cases - lag(log_cases)
  ) %>%
  ungroup() %>%
  mutate(
    has_history = !is.na(prev_log_cases),
    prev_log_cases  = ifelse(is.na(prev_log_cases),
                             ave(log_cases, Disease, FUN = median), prev_log_cases),
    roll3_log_cases = ifelse(is.na(roll3_log_cases), prev_log_cases, roll3_log_cases),
    days_since_prev = ifelse(is.na(days_since_prev), 365, days_since_prev),
    cases_growth    = ifelse(is.na(cases_growth), 0, cases_growth)
  )

# ---- 4. Climate interactions ------------------------------------------------
df <- df %>%
  mutate(
    temp_x_precip   = Temp_C * preci_log,
    temp_x_lai      = Temp_C * LAI,
    hot_wet         = as.integer(Temp_C > 25 & preci_log > median(preci_log, na.rm = TRUE)),
    temp_c_centered = Temp_C - mean(Temp_C, na.rm = TRUE),
    is_monsoon      = as.integer(season == "Monsoon")
  )

# ---- 5. Targets -------------------------------------------------------------
thr <- df %>%
  group_by(Disease) %>%
  summarise(outbreak_threshold = quantile(Cases, 0.75, na.rm = TRUE), .groups = "drop")

df <- df %>%
  left_join(thr, by = "Disease") %>%
  mutate(high_outbreak = factor(ifelse(Cases > outbreak_threshold, "High", "Low"),
                                levels = c("Low", "High"))) %>%
  select(-outbreak_threshold)

# ---- 6. Assemble modelling dataset -----------------------------------------
cols_to_select <- c(
  "state_ut", "district", "Disease", "date", "year", "month", "zone", "season",
  "Cases", "log_cases", "high_outbreak",
  "Latitude", "Longitude", "Temp_C", "preci", "preci_log", "LAI",
  "preci_missing", "lai_missing", "temp_missing",
  "reporting_week", "day_of_year", "year_index", "month_sin", "month_cos", "is_monsoon",
  "prev_log_cases", "roll3_log_cases", "days_since_prev", "n_prior_reports",
  "cases_growth", "has_history",
  "temp_x_precip", "temp_x_lai", "hot_wet",
  "Deaths", "deaths_reported", "cfr", "any_death"
)

# Only select columns that actually exist in the dataframe up to this point
modeling_data <- df %>%
  select(any_of(cols_to_select))

cat("Rows:", nrow(modeling_data), " Cols:", ncol(modeling_data), "\n")
cat("\nRemaining NA counts (should be 0 except deaths-related):\n")
print(colSums(is.na(modeling_data))[colSums(is.na(modeling_data)) > 0])
cat("\nClass balance (high_outbreak):\n")
print(prop.table(table(modeling_data$high_outbreak)))
cat("\nRows with history:", sum(modeling_data$has_history), "\n")

dir.create(here("data", "final"), recursive = TRUE, showWarnings = FALSE)
write_csv(modeling_data, here("data", "final", "modeling_data.csv"))
message("Saved successfully: data/final/modeling_data.csv")
