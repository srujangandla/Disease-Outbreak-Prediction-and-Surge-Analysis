# Data Loading
library(here)

# This properly routes the path from your project root folder
data <- read.csv(here("data", "Epiclim.csv"))

# First Six Rows
print("First Six Rows:")
head(data)

cat("\nDimensions  :", dim(data))
cat("\nColumn Names:", paste(colnames(data), collapse = ", "), "\n\n")

# Structure overview
cat("Structure Overview\n")
str(data)

print("Statistical Summary:")
summary(data)
