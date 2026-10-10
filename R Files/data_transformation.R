library(dplyr)
library(here)

data_dir <- here("data")
all_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
target_file <- all_files[grepl("cleaned_epiclim\\.csv", all_files, ignore.case = TRUE)]

if (is.na(target_file)) {
  stop("Error: Could not find 'cleaned_epiclim.csv' inside your 'data/' folder. Please run the previous data cleaning script first.")
}

cat("Loading file from verified path:", target_file, "\n")
data <- read.csv(target_file, stringsAsFactors = FALSE)
data$date <- as.Date(data$date)

# --- 2. REGION ASSIGNMENTS & TRANSFORMATIONS ---
# Region (approximate zones, used for grouping and plots)
s <- tolower(data$state_ut)
data$zone <- case_when(
  s %in% c("jammu and kashmir", "himachal pradesh", "punjab", "haryana", "delhi",
           "chandigarh", "uttarakhand", "uttar pradesh", "rajasthan") ~ "North",
  s %in% c("andhra pradesh", "telangana", "karnataka", "kerala", "tamil nadu",
           "puducherry", "lakshadweep", "andaman and nicobar islands") ~ "South",
  s %in% c("west bengal", "odisha", "bihar", "jharkhand") ~ "East",
  s %in% c("gujarat", "maharashtra", "goa", "dadra and nagar haveli",
           "daman and diu") ~ "West",
  s %in% c("madhya pradesh", "chhattisgarh") ~ "Central",
  s %in% c("assam", "arunachal pradesh", "manipur", "meghalaya", "mizoram",
           "nagaland", "tripura", "sikkim") ~ "Northeast",
  TRUE ~ "Unassigned"
)

cat("States not assigned to a zone:\n")
print(unique(data$state_ut[data$zone == "Unassigned"]))

# Skew reduction
data$preci_log <- log1p(data$preci)

# Time features
data$year_index  <- data$year - min(data$year)
data$day_of_year <- as.integer(format(data$date, "%j"))
data$month_sin   <- sin(2 * pi * data$month / 12)
data$month_cos   <- cos(2 * pi * data$month / 12)

# Categorical variables as factors
data$Disease <- factor(data$Disease)
data$season  <- factor(data$season, levels = c("Winter", "Summer", "Monsoon", "Post-monsoon"))
data$zone    <- factor(data$zone)
data$state_ut <- factor(data$state_ut)
data$district <- factor(data$district)

# Final order
data <- data %>% arrange(date, state_ut, district)

# --- 3. SAFE EXPORT & CHECKS ---
# Define precise output directory using project root navigation
output_dir <- here("data", "processed")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(data, file.path(output_dir, "transformed_epiclim.csv"), row.names = FALSE)

# Post-processing Summary Checks
cat("\n--- Verification Summary ---\n")
str(data)
cat("\nCases (raw) vs log_cases:\n")
print(summary(data$Cases)); print(summary(data$log_cases))
cat("\nPrecipitation before and after log:\n")
print(summary(data$preci)); print(summary(data$preci_log))
print(count(data, zone))
print(count(data, season))
