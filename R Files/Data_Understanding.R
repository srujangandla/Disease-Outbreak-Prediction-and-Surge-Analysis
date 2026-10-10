library(dplyr)

# Missing Values and Duplicates
missing_values <- data.frame(
  Column = names(data),
  Missing = colSums(is.na(data))
)
message("Missing Values:\n")
print(missing_values)

message("\nDuplicate Summary:\n")
cat("Total duplicate rows:", sum(duplicated(data)), "\n")

if (sum(duplicated(data)) > 0) {
  message("\nDuplicate Rows:\n")
  print(data[duplicated(data), ])
}

# Unique Value Counts Across All Variables
message("\n Unique Value Counts Per Variable:\n")
print(sapply(data, function(x) length(unique(x))))

# Disease Variable Analysis
message("\nUnique Diseases:\n")
print(unique(data$Disease))
cat("Total Number of Unique Diseases:", length(unique(data$Disease)), "\n")

message("\nDisease Frequency Distribution:\n")
data %>%
  count(Disease, sort = TRUE) %>%
  print()

# Geographic Distribution
message("\n--- Unique States / UTs ---")
print(unique(data$state_ut))
cat("Total Number of States/UTs:", length(unique(data$state_ut)), "\n")
cat("Total Number of Unique Districts:", length(unique(data$district)), "\n")

message("\n--- Top 20 Districts by Record Count ---")
data %>% 
  count(district, sort = TRUE) %>% 
  head(20) %>%
  print()

# Temporal Analysis
cat("Chronological years present in the dataset:\n")
print(sort(unique(data$year)))

cat("\nNumber of observations recorded per year:\n")
data %>% count(year) %>% print()

# Climate Variables
cat("\nSummary of Climatic Variables...\n")
cat("\nTemperature distribution metrics:\n")
print(summary(data$Temp))

cat("\nPrecipitation distribution metrics:\n")
print(summary(data$preci))

cat("\nLeaf Area Index (LAI) distribution metrics:\n")
print(summary(data$LAI))

# Spatial Coordinates
cat("\nSummary of Spatial Coordinates...\n")
cat("\nLatitude geographic boundaries:\n")
print(summary(data$Latitude))

cat("\nLongitude geographic boundaries:\n")
print(summary(data$Longitude))

# Health Outcomes
cat("\nSummary of Health Outcomes...\n")
cat("\nDistribution of reported cases:\n")
print(summary(data$Cases))

cat("\nDistribution of reported deaths:\n")
print(summary(data$Deaths))

# Value Ranges
cat("\n--- Absolute Value Ranges for Key Metrics ---\n")
cat("Cases range:       ", range(data$Cases, na.rm = TRUE), "\n")
cat("Deaths range:      ", range(data$Deaths, na.rm = TRUE), "\n")
cat("Temperature range: ", range(data$Temp, na.rm = TRUE), "\n")
cat("Precipitation range:", range(data$preci, na.rm = TRUE), "\n")
cat("LAI range:         ", range(data$LAI, na.rm = TRUE), "\n")

data$Cases <- as.numeric(as.character(data$Cases))

# Correlation Matrix
cat("\n=== Correlation Matrix for Key Environmental and Health Variables ===\n")
target_numeric <- data %>% select(Cases, Deaths, Temp, preci, LAI)
cor_matrix <- cor(target_numeric, use = "complete.obs")
print(round(cor_matrix, 3))
