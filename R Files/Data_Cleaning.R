library(dplyr)
library(here)


data_dir <- here("data")
all_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
target_file <- all_files[grepl("epiclim\\.csv", all_files, ignore.case = TRUE)][1]

if (is.na(target_file)) {
  stop("Error: Could not find 'Epiclim.csv' anywhere inside your 'data/' folder. Please check your file layout.")
}

cat("Loading file from verified path:", target_file, "\n")
data <- read.csv(target_file, stringsAsFactors = FALSE)
cat("Raw rows:", nrow(data), "\n")

# --- 2. DATA CLEANING & REFORMATTING ---
# Remove index column and duplicates
data <- data[, names(data) != "X"]
data <- distinct(data)

# Cases to numeric, keep valid values only
cases_num <- suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(data$Cases)))))
print(table(data$Cases[is.na(cases_num)], useNA = "ifany"))

data$Cases <- cases_num
data <- data[!is.na(data$Cases) & data$Cases > 0, ]

# Text cleaner function
tidy <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII", sub = " ")
  x <- gsub("[^A-Za-z0-9]+", " ", x)
  tools::toTitleCase(tolower(trimws(gsub("\\s+", " ", x))))
}

# Dynamic Month Parsing Check (Handles both numeric '05' and character 'May')
is_numeric_month <- !any(is.na(suppressWarnings(as.numeric(head(data$mon, 20)))))
date_format_string <- if (is_numeric_month) "%Y-%m-%d" else "%Y-%b-%d"

# Unified dplyr transformation pipeline
cleaned_data <- data %>%
  # Standardize text strings
  mutate(
    d_lower = tolower(trimws(Disease)),
    state_ut = tidy(state_ut),
    district = tidy(district)
  ) %>%
  # Categorize Disease
  mutate(Disease = case_when(
    grepl("dengue", d_lower) & grepl("chikungunya", d_lower) ~ "Dengue And Chikungunya",
    grepl("dengue", d_lower) & grepl("malaria", d_lower)     ~ "Other",
    grepl("pyrexia", d_lower)                          ~ "Other",
    grepl("dengue", d_lower)                           ~ "Dengue",
    grepl("chikungunya", d_lower)                      ~ "Chikungunya",
    grepl("cholera", d_lower)                          ~ "Cholera",
    grepl("malaria", d_lower)                          ~ "Malaria",
    grepl("diarrh|gastroenteritis", d_lower)           ~ "Acute Diarrhoeal Disease",
    grepl("encephalitis", d_lower)                     ~ "Acute Encephalitis Syndrome",
    TRUE                                               ~ "Other"
  )) %>%
  filter(Disease != "Other") %>%
  
  # Handle Dates safely using the dynamic parser format
  mutate(
    date = as.Date(paste(year, mon, day, sep = "-"), format = date_format_string),
    reporting_week = as.integer(gsub("\\D", "", week_of_outbreak))
  ) %>%
  filter(!is.na(date)) %>%
  
  # Filter geographical boundaries for India
  filter(Latitude >= 6 & Latitude <= 38 & Longitude >= 68 & Longitude <= 98) %>%
  
  # Process features, missing metrics, and target logging
  mutate(
    deaths_reported = !is.na(Deaths),
    Temp_C          = Temp - 273.15,
    temp_missing    = is.na(Temp),
    preci_missing   = is.na(preci),
    lai_missing     = is.na(LAI),
    month           = as.integer(format(date, "%m")),
    season = case_when(
      month %in% c(12, 1, 2)   ~ "Winter",
      month %in% c(3, 4, 5)    ~ "Summer",
      month %in% c(6, 7, 8, 9) ~ "Monsoon",
      TRUE                     ~ "Post-monsoon"
    ),
    log_cases = log1p(Cases)
  ) %>%
  
  # Sort and finalize
  arrange(state_ut, district, date) %>%
  select(state_ut, district, Disease, date, year, month, season, reporting_week,
         Cases, log_cases, Deaths, deaths_reported, Latitude, Longitude,
         preci, LAI, Temp_C, preci_missing, lai_missing, temp_missing)

dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
write.csv(cleaned_data, here("data", "processed", "cleaned_epiclim.csv"), row.names = FALSE)

# Post-processing Summary Checks
cat("\n--- Verification Summary ---\n")
cat("Cleaned rows:", nrow(cleaned_data), "\n")
print(summary(cleaned_data$Cases))
print(count(cleaned_data, Disease, sort = TRUE))
print(count(cleaned_data, year))
print(colSums(is.na(cleaned_data)))
cat("Districts:", n_distinct(cleaned_data$district), "\n")
