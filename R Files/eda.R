library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(tibble) # Explicitly loaded for rownames_to_column

fig_dir <- here("outputs", "figures", "eda")
tab_dir <- here("outputs", "tables")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, name, w = 9, h = 5.5) {
  ggplot2::ggsave(
    file.path(fig_dir, paste0(name, ".png")), 
    plot = p,
    width = w, 
    height = h, 
    dpi = 300
  )
}

# Load data and dynamic column building
df <- read_csv(here("data", "processed", "cleaned_epiclim.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# DYNAMIC FIX: If 'zone' is missing, map it using 'state_ut'
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

# DYNAMIC FIX FOR MISSING preci_log: 
# Checks variations of precipitation column names and creates the log column if missing
if (!"preci_log" %in% names(df)) {
  target_col <- intersect(names(df), c("preci", "precip", "precipitation", "precip_mm"))[1]
  if (!is.na(target_col)) {
    message(paste("Creating 'preci_log' dynamically from raw column:", target_col))
    df <- df %>% mutate(preci_log = log1p(.data[[target_col]]))
  } else {
    message("Warning: No precipitation column found. Initialising 'preci_log' as NA.")
    df$preci_log <- NA_real_
  }
}

# Factor adjustments and variable conversions
df <- df %>%
  mutate(
    season = if("season" %in% names(.)) factor(season, levels = c("Winter", "Summer", "Monsoon", "Post-monsoon")) else NULL,
    across(any_of(c("state_ut", "district", "Disease", "zone")), as.factor)
  )

theme_set(theme_minimal(base_size = 12))

# ---- 1. Outcome distribution (raw vs log) -----------------------------------
p <- ggplot(df, aes(Cases)) +
  geom_histogram(bins = 60, fill = "steelblue") +
  scale_x_continuous(labels = comma) +
  labs(title = "Reported cases (raw) - heavily right-skewed", x = "Cases", y = "Count")
save_fig(p, "01_cases_raw")

p <- ggplot(df, aes(log_cases)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  labs(title = "Reported cases (log scale)", x = "log(Cases)", y = "Count")
save_fig(p, "02_cases_log")

# ---- 2. Disease burden ------------------------------------------------------
disease_summary <- df %>%
  group_by(Disease) %>%
  summarise(
    records = n(),
    total_cases = sum(Cases),
    median_cases = median(Cases),
    total_deaths = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(records))

write_csv(disease_summary, file.path(tab_dir, "eda_disease_summary.csv"))

p <- ggplot(disease_summary, aes(reorder(Disease, records), records)) +
  geom_col(fill = "steelblue") + 
  coord_flip() +
  labs(title = "Outbreak records by disease", x = NULL, y = "Records")
save_fig(p, "03_disease_counts")

p <- ggplot(df, aes(reorder(Disease, log_cases, median), log_cases)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.3) + 
  coord_flip() +
  labs(title = "log(Cases) by disease", x = NULL, y = "log(Cases)")
save_fig(p, "04_disease_boxplot")

# ---- 3. Temporal patterns ---------------------------------------------------
yearly <- df %>% count(year, Disease)
p <- ggplot(yearly, aes(year, n, fill = Disease)) +
  geom_col() +
  labs(title = "Records per year by disease", y = "Records", x = "Year")
save_fig(p, "05_yearly_records")

monthly <- df %>% count(month, Disease)
p <- ggplot(monthly, aes(factor(month), n, fill = Disease)) +
  geom_col() +
  labs(title = "Seasonality: records per month", x = "Month", y = "Records")
save_fig(p, "06_monthly_records")

if("season" %in% names(df)) {
  p <- ggplot(df, aes(season, log_cases, fill = season)) +
    geom_boxplot(show.legend = FALSE) +
    labs(title = "log(Cases) by season", x = NULL, y = "log(Cases)")
  save_fig(p, "07_season_boxplot")
}

# ---- 4. Geography -----------------------------------------------------------
zone_summary <- df %>%
  group_by(zone) %>%
  summarise(records = n(), mean_log_cases = mean(log_cases), .groups = "drop")

write_csv(zone_summary, file.path(tab_dir, "eda_zone_summary.csv"))

p <- ggplot(df, aes(zone, log_cases, fill = zone)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "log(Cases) by zone", x = NULL, y = "log(Cases)")
save_fig(p, "08_zone_boxplot")

top_states <- df %>% count(state_ut, sort = TRUE) %>% slice_head(n = 15)
p <- ggplot(top_states, aes(reorder(state_ut, n), n)) +
  geom_col(fill = "steelblue") + 
  coord_flip() +
  labs(title = "Top 15 states/UTs by record count", x = NULL, y = "Records")
save_fig(p, "09_top_states")

p <- ggplot(df, aes(Longitude, Latitude, colour = log_cases)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  labs(title = "Spatial distribution of outbreak records", colour = "log(Cases)")
save_fig(p, "10_spatial_scatter", h = 7)

# ---- 5. Climate relationships ----------------------------------------------
clim_long <- df %>%
  select(any_of(c("log_cases", "Temp_C", "preci_log", "LAI")))

if (ncol(clim_long) > 1) {
  clim_long <- clim_long %>%
    filter(if_any(-log_cases, ~ !is.na(.))) %>% 
    pivot_longer(-log_cases, names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))
  
  p <- ggplot(clim_long, aes(value, log_cases)) +
    geom_point(alpha = 0.15, size = 0.8) +
    geom_smooth(method = "loess", formula = y ~ x, colour = "red", se = FALSE) +
    facet_wrap(~variable, scales = "free_x") +
    labs(title = "Climate variables vs log(Cases)", x = NULL, y = "log(Cases)")
  save_fig(p, "11_climate_scatter", w = 11, h = 4.5)
}

if("season" %in% names(df)) {
  p <- ggplot(df %>% filter(!is.na(Temp_C)), aes(Temp_C, fill = season)) +
    geom_density(alpha = 0.5) +
    labs(title = "Temperature (C) distribution by season", x = "Temp (C)")
  save_fig(p, "12_temp_density_season")
}

# Correlation matrix
num_vars <- df %>% select(any_of(c("log_cases", "Deaths", "Temp_C", "preci_log", "LAI", "Latitude", "Longitude", "month", "year")))
cor_mat <- cor(num_vars, use = "pairwise.complete.obs")

cor_df <- as.data.frame(cor_mat) %>% 
  mutate(across(everything(), ~ round(.x, 3))) %>% 
  rownames_to_column("variable")

write_csv(cor_df, file.path(tab_dir, "eda_correlation_matrix.csv"))

cor_long <- as.data.frame(as.table(cor_mat))
p <- ggplot(cor_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", limits = c(-1, 1)) +
  labs(title = "Correlation matrix", x = NULL, y = NULL, fill = "r") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p, "13_correlation_heatmap", w = 8, h = 7)

# ---- 6. Deaths and missingness ---------------------------------------------
if("deaths_reported" %in% names(df)) {
  cfr <- df %>%
    filter(deaths_reported) %>%
    group_by(Disease) %>%
    summarise(
      records = n(),
      deaths = sum(Deaths, na.rm = TRUE),
      cases = sum(Cases, na.rm = TRUE),
      crude_cfr_pct = round(100 * deaths / cases, 2),
      .groups = "drop"
    )
  write_csv(cfr, file.path(tab_dir, "eda_cfr_by_disease.csv"))
}

miss <- df %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  filter(pct_missing > 0) %>% 
  arrange(desc(pct_missing))
write_csv(miss, file.path(tab_dir, "eda_missingness.csv"))

p <- ggplot(miss, aes(reorder(variable, pct_missing), pct_missing)) +
  geom_col(fill = "firebrick") + 
  coord_flip() +
  labs(title = "Missing data percentages", x = NULL, y = "% Missing")
save_fig(p, "14_missingness")

