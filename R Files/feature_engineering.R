library(here)
library(dplyr)
library(readr)


# ---- 1. Load data, split, time features -------------------------------------
df <- read_csv(here("data", "processed", "cleaned_epiclim.csv"), show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    split = case_when(year <= 2017 ~ "train", year <= 2019 ~ "test", TRUE ~ "stress"),
    year_index = year - min(year),
    day_of_year = as.integer(format(date, "%j")),
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12),
    is_monsoon = as.integer(season == "Monsoon")
  )
cat("Rows per split:\n"); print(table(df$split))

#Zone
state_key <- trimws(tolower(gsub("&", "and", as.character(df$state_ut))))
df$zone <- case_when(
  state_key %in% c("jammu and kashmir", "himachal pradesh", "punjab", "haryana", "delhi",
                   "chandigarh", "uttarakhand", "uttaranchal") ~ "North",
  state_key %in% c("uttar pradesh", "madhya pradesh", "chhattisgarh") ~ "Central",
  state_key %in% c("bihar", "jharkhand", "odisha", "orissa", "west bengal",
                   "andaman and nicobar islands", "andaman and nicobar",
                   "andaman nicobar islands") ~ "East",
  state_key %in% c("rajasthan", "gujarat", "maharashtra", "goa", "dadra and nagar haveli",
                   "daman and diu", "dadra nagar haveli", "daman diu",
                   "dadra and nagar haveli and daman and diu") ~ "West",
  state_key %in% c("andhra pradesh", "telangana", "karnataka", "kerala", "tamil nadu",
                   "puducherry", "pondicherry", "lakshadweep") ~ "South",
  state_key %in% c("assam", "arunachal pradesh", "manipur", "meghalaya", "mizoram",
                   "nagaland", "sikkim", "tripura") ~ "North-East",
  TRUE ~ "Other"
)
if (any(df$zone == "Other")) {
  print(unique(df$state_ut[df$zone == "Other"]))
  stop("Unmapped states found: fix the zone mapping above.")
}

#Impute climate using TRAIN medians only
impute_median <- function(data, var) {
  tr <- data %>% filter(split == "train")
  m1 <- tr %>% group_by(zone, month) %>%
    summarise(m1 = median(.data[[var]], na.rm = TRUE), .groups = "drop")
  m2 <- tr %>% group_by(zone) %>%
    summarise(m2 = median(.data[[var]], na.rm = TRUE), .groups = "drop")
  m3 <- median(tr[[var]], na.rm = TRUE)
  data %>%
    left_join(m1, by = c("zone", "month")) %>%
    left_join(m2, by = "zone") %>%
    mutate("{var}" := coalesce(.data[[var]], m1, m2, m3)) %>%
    select(-m1, -m2)
}
for (v in c("Temp_C", "preci", "LAI")) df <- impute_median(df, v)
df$preci_log <- log1p(df$preci)

#History features
disease_med <- df %>% filter(split == "train") %>%
  group_by(Disease) %>% summarise(disease_med = median(log_cases), .groups = "drop")

df <- df %>%
  left_join(disease_med, by = "Disease") %>%
  arrange(district, Disease, date) %>%
  group_by(district, Disease) %>%
  mutate(
    prev_log_cases  = lag(log_cases),
    days_since_prev = as.numeric(date - lag(date)),
    roll3_log_cases = (lag(log_cases, 1) + lag(log_cases, 2) + lag(log_cases, 3)) / 3,
    n_prior_reports = row_number() - 1L
  ) %>%
  ungroup() %>%
  mutate(
    has_history     = !is.na(prev_log_cases),
    prev_log_cases  = ifelse(is.na(prev_log_cases), disease_med, prev_log_cases),
    roll3_log_cases = ifelse(is.na(roll3_log_cases), prev_log_cases, roll3_log_cases),
    days_since_prev = ifelse(is.na(days_since_prev), 365, days_since_prev)
  ) %>%
  select(-disease_med)

median_preci_log <- median(df$preci_log[df$split == "train"])
df <- df %>%
  mutate(
    temp_x_precip = Temp_C * preci_log,
    temp_x_lai    = Temp_C * LAI,
    hot_wet       = as.integer(Temp_C > 25 & preci_log > median_preci_log)
  )

# ---- 6. Target: high outbreak = above disease-specific 75th percentile ------
thr <- df %>% filter(split == "train") %>%
  group_by(Disease) %>%
  summarise(n_train = n(), outbreak_threshold = quantile(Cases, 0.75, na.rm = TRUE),
            .groups = "drop")

dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(thr, here("outputs", "tables", "outbreak_thresholds.csv"))
cat("\nSurge thresholds (train only):\n"); print(as.data.frame(thr))

df <- df %>%
  left_join(select(thr, Disease, outbreak_threshold), by = "Disease") %>%
  mutate(high_outbreak = factor(ifelse(Cases > outbreak_threshold, "High", "Low"),
                                levels = c("Low", "High"))) %>%
  select(-outbreak_threshold)
stopifnot(!any(is.na(df$high_outbreak)))

cols_to_select <- c(
  "state_ut", "district", "Disease", "date", "year", "month", "zone", "season", "split",
  "Cases", "log_cases", "high_outbreak",
  "Latitude", "Longitude", "Temp_C", "preci", "preci_log", "LAI",
  "preci_missing", "lai_missing", "temp_missing",
  "reporting_week", "day_of_year", "year_index", "month_sin", "month_cos", "is_monsoon",
  "prev_log_cases", "roll3_log_cases", "days_since_prev", "n_prior_reports", "has_history",
  "temp_x_precip", "temp_x_lai", "hot_wet"
)
modeling_data <- df %>% select(all_of(cols_to_select))

cat("\nRows:", nrow(modeling_data), " Cols:", ncol(modeling_data), "(expected 35)\n")
cat("\nNA counts (should be none):\n")
print(colSums(is.na(modeling_data))[colSums(is.na(modeling_data)) > 0])
cat("\nHigh/Low share by split:\n")
print(round(prop.table(table(modeling_data$split, modeling_data$high_outbreak), 1), 3))
cat("\nHigh outbreaks per split:\n")
print(table(modeling_data$split, modeling_data$high_outbreak))
cat("\nRows with history:", sum(modeling_data$has_history), "\n")
print(count(modeling_data, zone))

dir.create(here("data", "final"), recursive = TRUE, showWarnings = FALSE)
write_csv(modeling_data, here("data", "final", "modeling_data.csv"))
message("Saved: data/final/modeling_data.csv")