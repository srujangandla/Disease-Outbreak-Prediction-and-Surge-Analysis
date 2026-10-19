# Predicting Disease Outbreaks and Identifying the Factors Behind Case Surges

*What makes some disease outbreaks in India much larger than others, and can climate, location, season and history help us tell in advance?*

An end-to-end data science project in **R** that analyses district-level outbreak reports from India, combined with climate data, using regression, classification, clustering and association rule mining. Built as a college project in Artificial Intelligence and Data Science.

---

## Table of contents

1. [Why this project](#why-this-project)
2. [An honest note about the data](#an-honest-note-about-the-data)
3. [Research questions](#research-questions)
4. [The dataset](#the-dataset)
5. [Approach](#approach)
6. [Repository structure](#repository-structure)
7. [Getting started](#getting-started)
8. [Key design decisions](#key-design-decisions)
9. [Results](#results)
10. [Limitations](#limitations)
11. [Lessons learned](#lessons-learned)
12. [Project status](#project-status)
13. [Reproducibility and version control](#reproducibility-and-version-control)

---

## Why this project

Every year, outbreaks of diarrhoeal disease, dengue, chikungunya, cholera and malaria hit districts across India. Some stay small. Others grow into large events that overwhelm local health systems. Public health teams constantly ask the same question:* Is this going to be a big one?*

Climate is often blamed. Heavy rain, warm temperatures, and green vegetation are all linked to mosquito-borne and water-borne diseases. But how strong is that link when you test it on real district-level reports? And how much of an outbreak's size can be explained by something much simpler, like what happened in the same district last time?

I wanted to answer these questions with data instead of assumptions, and to do it in a way that is **reproducible, leakage-free, and honest about its limits**.

## An honest note about the data

The EpiClim dataset records **outbreaks that were reported**. A row exists only when an outbreak happened and was recorded. It is not a week-by-week surveillance series with a value for every district in every week.

That shapes what this project can and cannot claim:

- **It can** study what makes a *reported* outbreak large, and what separates large outbreaks from small ones for the same disease.
- **It cannot** forecast whether an outbreak will occur in a given district next week, because the data says nothing about the weeks and places where nothing was reported.

I started with the wider framing ("predict future surges") and changed it once I looked at the data. Most districts have only a handful of records over 14 years, so week-ahead forecasting was never realistic. Wording the project to match what the data supports is part of doing the analysis properly.

## Research questions

1. **Patterns:** How do reported outbreaks vary across time, place, and disease? Is there seasonality?
2. **Size prediction:** Can we predict the size of a reported outbreak (log of case counts)?
3. **Surge prediction:** can we predict whether an outbreak is *unusually large for its disease*?
4. **Factors:** Which variables are most associated with large outbreaks: previous case counts, temperature, precipitation, season, disease, location?
5. **Structure:** Do districts group into meaningful profiles (K-Means), and do certain combinations of conditions recur in large outbreaks (association rules)?

## The dataset

| Item | Detail |
|------|--------|
| Source | EpiClim: outbreak reports from India with matching climate variables (*add the dataset's citation and link here*) |
| Raw size | 8,985 rows, 15 columns |
| Period | 2009 to 2022 |
| Coverage | 36 states and union territories, hundreds of districts |
| Diseases | 22 raw labels, standardised to 8 groups (Acute Diarrhoeal Disease, Dengue, Chikungunya, Cholera, Malaria, Acute Encephalitis Syndrome, Dengue and Chikungunya, Other) |
| Outcomes | Reported cases and deaths |
| Climate | Temperature (Kelvin in the raw file), precipitation, leaf area index (LAI) |

> **Data files:** the raw CSV is not guaranteed to be redistributable. Download EpiClim from its original source and place it at `data/raw/EpiClim.csv`. Check its licence before committing it to a public repository.

## Approach

```
EpiClim raw data
      |
Cleaning  ->  Feature engineering (train-only statistics)
      |
Exploratory analysis  ->  Statistical analysis
      |
 +----+------------------------------+
 |                                   |
Regression                    Classification
(log cases)                   (high outbreak or not)
 |  Linear Regression          |  Logistic Regression
 |  Random Forest              |  Random Forest
 +----------------+------------+
                  |
     Model evaluation + factor importance
                  |
     K-Means clustering + Association rules
                  |
          Final interpretation
```

**How I defined a "surge":** a reported outbreak counts as a high outbreak if its case count is above the **75th percentile for its own disease**, calculated from the training years only. Diseases differ a lot in typical size, so a single global threshold would just flag the most common diseases.

**How I split the data:** by time, never at random.

| Set | Years | Role |
|-----|-------|------|
| Train | 2009 to 2017 | Fit models, tune settings, compute thresholds and imputation values |
| Test | 2018 to 2019 | Final, untouched evaluation |
| Stress | 2020 to 2022 | Separate robustness check (sparse, COVID-era reporting) |

**What the models see:** disease, season, zone, coordinates, temperature, precipitation, vegetation, month, and **history features** built only from earlier reports of the same district and disease (previous size, rolling average, days since the last report, number of prior reports).

**What counts as a good result:** every model is compared against simple baselines, such as the disease median for regression and "was the last report in this district large?" for classification. A model that cannot beat a one-line rule has not learned anything useful, and I report that plainly.

## Repository structure

```
Predicting_Disease_Outbreaks/
├── Predicting_Disease_Outbreaks.Rproj
├── data/
│   ├── raw/            
│   ├── processed/     
│   └── final/         
├── R/
│   ├── 01_data_loading.R
│   ├── 02_data_understanding.R
│   ├── 03_data_cleaning.R
│   ├── 04_data_transformation.R
│   ├── 05_eda.R
│   ├── 06_feature_engineering.R
│   ├── 07_statistical_analysis.R
│   ├── 08_regression.R
│   ├── 09_classification.R
│   ├── 10_clustering.R
│   ├── 11_association_rules.R
│   ├── 12_model_evaluation.R
│   └── 13_factor_analysis.R
├── notebooks/          disease_outbreak_analysis.Rmd
├── reports/            final_report.Rmd
├── outputs/
│   ├── figures/        eda/, regression/, classification/, clustering/
│   ├── tables/         metrics, tests, importance, thresholds, clusters
│   └── models/         saved model objects (.rds)
├── docs/               project_notes.Rmd 
└── README.md
```

## Getting started

### Prerequisites

- R (4.x recommended) and RStudio
- The following packages:

```r
install.packages(c(
  "here", "dplyr", "readr", "tidyr", "ggplot2", "scales", "tibble",
  "randomForest", "cluster", "arules", "rmarkdown", "knitr"
))
```

### Run the analysis

1. Clone the repository and open `Predicting_Disease_Outbreaks.Rproj` in RStudio. Opening the project is important: paths are resolved from the project root.
2. Put `EpiClim.csv` in `data/raw/`.
3. Restart R, then run the scripts in numeric order:

```r
source("R/03_data_cleaning.R")
source("R/05_eda.R")
source("R/06_feature_engineering.R")
source("R/07_statistical_analysis.R")
source("R/08_regression.R")
source("R/09_classification.R")
source("R/10_clustering.R")
```

Tip: run each script as a whole file, in a fresh session. Running it line by line in an old session is a good way to get results that come from stale objects.

### Build the report

```r
# Report from saved outputs
rmarkdown::render("reports/final_report.Rmd")

# Re-run the whole pipeline from the raw CSV, then build the report
rmarkdown::render("reports/final_report.Rmd", params = list(run_pipeline = TRUE))

# Version that also shows the R code (notebook style)
rmarkdown::render("reports/final_report.Rmd", params = list(show_code = TRUE))
```

## Key design decisions

These are the choices that protect the results from fooling me.

- **No data leakage.** Thresholds, imputation values and tuning use training years only. History features use earlier reports only. Outcome-related columns (cases, deaths) are never predictors.
- **Time-ordered evaluation.** Predicting 2018 from a model that has seen 2019 would be cheating, so the split follows the calendar.
- **Log scale for case counts.** Raw cases are extremely right-skewed (the median is a few dozen, while a few records run into the tens of thousands).
- **No invented data.** Rows with unusable case counts are removed, not replaced with zero. `Deaths` is about 72% missing and is never filled in.
- **Imputation after the split.** Missing climate values are flagged, then filled using training-set medians by zone and month.
- **Metrics that fit the problem.** For the classifier I report precision, recall, F1 and PR-AUC rather than accuracy alone, because always predicting "not a surge" already scores about 75% accuracy.
- **Baselines everywhere.** Every model is judged against a simple rule.
- **Association, not causation.** Importance scores, odds ratios and association rules describe patterns in the data. They do not prove that any factor causes larger outbreaks.

## Results

To BE Added After Final Validated Run

Planned sections once results exist:

- The strongest factors associated with large outbreaks, and how consistent they are across methods.
- How much climate adds beyond disease, season and history.
- A description of each district cluster in plain language.
- The most reliable association rules for high outbreaks (support, confidence, lift).

Figures and tables are written to `outputs/figures/` and `outputs/tables/`.

## Limitations

- **Event-based data:** no information on periods or places without a reported outbreak, so the project cannot forecast whether an outbreak will occur.
- **Non-independence:** repeated reports from the same district mean p-values and confidence intervals are indicative, not exact.
- **District names:** spelling variants of the same district can split history features and clusters.
- **Uneven reporting:** reporting practices differ by state and over time, and 2020 to 2022 is sparse and unusual.
- **Coarse climate data:** variables are grid or satellite based, and a share of values were missing and imputed. The leaf area index scale still needs to be confirmed against the dataset documentation.
- **Possible weak climate signal:** early correlations between cases and climate were close to zero. If the final results confirm this, that is a finding, not a failure.

## Lessons learned

A few things I got wrong on the first pass, and fixed, because they are the sort of mistake that quietly ruins a project:

- My duplicate check included the row-index column, so it could never find a duplicate.
- A "growth" feature (this report minus the previous report) contained the very thing I was trying to predict. It would have produced near-perfect scores that meant nothing.
- Sorting a text column of week names puts "10th week" before "2nd week".
- Computing medians and thresholds on the full dataset lets test years influence training.
- The honest answer to "what can this data do?" was different from the one I started with, and it was better to change the question than to bend the data.

## Project status

- [x] Data loading and understanding
- [x] Data cleaning
- [x] Exploratory data analysis
- [x] Feature engineering (train-only statistics, leakage fixes)
- [x] Statistical analysis script
- [x] Regression script
- [x] Classification script
- [x] Clustering script
- [ ] Validate scripts 06 to 10 on the full data and review results
- [ ] Association rule mining (`11_association_rules.R`)
- [ ] Model evaluation summary (`12_model_evaluation.R`)
- [ ] Factor analysis summary (`13_factor_analysis.R`)
- [ ] Final report and conclusions
- [ ] Resolve open data questions (leaf area index scale, largest case record, district name variants)

`04_data_transformation.R` is kept for reference; its features are now built inside `06_feature_engineering.R`.

## Reproducibility and version control

Work is tracked in Git with a tag per stage, so the history shows how the project developed.

Random seeds are fixed (`set.seed(42)`), paths use the `here` package, and the report records `sessionInfo()` at the end. For stricter reproducibility, consider locking package versions with `renv`.

The reasoning behind each decision, and the list of data problems found, lives in [`docs/project_notes.Rmd`](docs/project_notes.Rmd).


**Author:** Srujan, student of Artificial Intelligence and Data Science.

**Data:** EpiClim dataset. *(Add the full citation, link and licence.)*

**Tools:** R, RStudio, R Markdown, and the packages `dplyr`, `ggplot2`, `randomForest`, `cluster`, `arules`, `here`, `readr` and `tidyr`.


This Project is Open Source and project Licensed Under Mit License.
