library(dplyr)
library(arules)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

d <- read.csv("data/final/modeling_data.csv", stringsAsFactors = FALSE)
d$surge <- ifelse(d$high_outbreak %in% c("High", 1, "1", TRUE, "TRUE"), "High", "Low")

band <- function(x, label) {
  factor(ntile(x, 3), levels = 1:3,
         labels = paste0(label, "=", c("Low", "Mid", "High")))
}

items <- data.frame(
  Disease   = factor(d$Disease),
  Zone      = factor(d$zone),
  Season    = factor(d$season),
  Temp      = band(d$Temp_C, "Temp"),
  Rain      = band(d$preci_log, "Rain"),
  LAI       = band(d$LAI, "LAI"),
  PrevCases = band(d$prev_log_cases, "PrevCases"),
  Surge     = factor(d$surge)
)
tx <- as(items, "transactions")
print(summary(tx))

# ---------- A. rules that lead to a surge ----------
# Baseline surge rate is about 0.24, so lift above 1.2 is the interesting zone
rules_surge <- apriori(tx,
                       parameter  = list(supp = 0.01, conf = 0.30, minlen = 2, maxlen = 5),
                       appearance = list(rhs = "Surge=High", default = "lhs"))
rules_surge <- subset(rules_surge, lift > 1.2)
rules_surge <- rules_surge[!is.redundant(rules_surge)]
rules_surge <- sort(rules_surge, by = "lift")
cat("\nTop surge rules:\n")
inspect(head(rules_surge, 20))
write.csv(as(rules_surge, "data.frame"),
          "outputs/tables/rules_surge.csv", row.names = FALSE)

# ---------- B. disease / season / zone / climate patterns ----------
rules_pat <- apriori(tx,
                     parameter  = list(supp = 0.03, conf = 0.60, minlen = 2, maxlen = 4),
                     appearance = list(none = c("Surge=High", "Surge=Low"), default = "both"))
rules_pat <- subset(rules_pat, lift > 1.3)
rules_pat <- rules_pat[!is.redundant(rules_pat)]
rules_pat <- sort(rules_pat, by = "lift")
cat("\nTop disease and climate pattern rules:\n")
inspect(head(rules_pat, 20))
write.csv(as(rules_pat, "data.frame"),
          "outputs/tables/rules_patterns.csv", row.names = FALSE)