<img src="https://raw.githubusercontent.com/lshpaner/kfreR/refs/heads/main/assets/kfre_logo_r.svg" width="200" style="border: none; outline: none; box-shadow: none;" oncontextmenu="return false;">

## Table of Contents

- [KFRE Risk Prediction Tools (R)](#kfre-risk-prediction-tools-r)
- [Installation](#installation)
- [GitHub (Development)](#github-development)
- [Dependencies](#dependencies)
- [Quick Start](#quick-start)
  1. [Toy Data](#1-toy-data)
  2. [Vectorized Predictions with `RiskPredictor`](#2-vectorized-predictions-with-riskpredictor)
  3. [Single-Person Predictions](#3-single-person-predictions)
  4. [Add KFRE Risk Columns to a DataFrame](#4-add-kfre-risk-columns-to-a-dataframe)
  5. [CKD Staging & ESRD Outcome Labels](#5-ckd-staging--esrd-outcome-labels)
  6. [uPCR → uACR Conversion](#6-upcr--uacr-conversion)
  7. [Evaluation Metrics (AUC-ROC, AP, Brier…)](#7-evaluation-metrics-auc-roc-ap-brier)
  8. [Plot ROC / PR Curves](#8-plot-roc--pr-curves)
- [Running Tests](#running-tests)
- [API Surface (Exports)](#api-surface-exports)
- [Notes on Parity with Python](#notes-on-parity-with-python)
- [References](#references)
- [License](#license)


## KFRE Risk Prediction Tools (R)

`kfre` is an R implementation of helpers around the Kidney Failure Risk Equation (KFRE), including:

- Vectorized and per-person KFRE predictions (4-, 6-, and 8-variable models; 2- and 5-year horizons)
- Convenience wrappers to add KFRE risk columns to a data.frame
- uPCR → uACR conversion utility
- CKD stage labeling utilities
- Evaluation helpers (AUC-ROC, Average Precision, Brier, etc.)
- Publication-style ROC and Precision–Recall plots

## Installation

```r
install.packages("kfreR")
```

## GitHub (Development)

```r
# install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/kfreR")
```

## Dependencies

Core imports: `R6`, `stats`, `ggplot2`, `pROC`, `precrec`
Suggested for tests/vignettes: `testthat (>= 3.0.0)`, `knitr`, `rmarkdown`

## Quick Start

### 1. Toy data

```r
toy <- data.frame(
  age = c(55, 72),
  sex_txt = c("male", "female"),
  eGFR = c(45, 28),
  uACR = c(120, 800),
  dm = c(1, 0),
  htn = c(1, 1),
  albumin = c(4.2, 3.4),
  phosphorous = c(3.3, 4.6),
  bicarbonate = c(24, 22),
  calcium = c(9.1, 9.8),
  stringsAsFactors = FALSE
)

cols <- list(
  age = "age",
  sex = "sex_txt",
  eGFR = "eGFR",
  uACR = "uACR",
  dm = "dm",
  htn = "htn",
  albumin = "albumin",
  phosphorous = "phosphorous",
  bicarbonate = "bicarbonate",
  calcium = "calcium"
)
```

### 2. Vectorized predictions with `RiskPredictor`

```r
rp <- RiskPredictor$new(df = toy, columns = cols)

# 4-variable KFRE (2-year), North America constants
p4_2y <- rp$predict_kfre(
  years = 2, is_north_american = TRUE,
  use_extra_vars = FALSE, num_vars = 4
)

# 6-variable KFRE (5-year)
p6_5y <- rp$predict_kfre(
  years = 5, is_north_american = TRUE,
  use_extra_vars = TRUE, num_vars = 6
)

# 8-variable KFRE (2-year)
p8_2y <- rp$predict_kfre(
  years = 2, is_north_american = TRUE,
  use_extra_vars = TRUE, num_vars = 8
)

p4_2y
p6_5y
p8_2y
```

### 3. Single-person predictions

```r
# Male, 55yo, 2-year risk (4-var)
rp$kfre_person(
  age = 55, is_male = TRUE,
  eGFR = 45, uACR = 120,
  is_north_american = TRUE, years = 2
)

# Female, 72yo, 5-year risk (6-var)
rp$kfre_person(
  age = 72, is_male = FALSE,
  eGFR = 28, uACR = 800,
  is_north_american = TRUE, years = 5,
  dm = 0, htn = 1
)

# Female, 72yo, 2-year risk (8-var)
rp$kfre_person(
  age = 72, is_male = FALSE,
  eGFR = 28, uACR = 800,
  is_north_american = TRUE, years = 2,
  albumin = 3.4, phosphorous = 4.6, bicarbonate = 22, calcium = 9.8
)
```

### 4. Add KFRE risk columns to a `data.frame`

```r
toy_kfre <- add_kfre_risk_col(
  df = toy,
  age_col = "age",
  sex_col = "sex_txt",
  eGFR_col = "eGFR",
  uACR_col = "uACR",
  dm_col = "dm",
  htn_col = "htn",
  albumin_col = "albumin",
  phosphorous_col = "phosphorous",
  bicarbonate_col = "bicarbonate",
  calcium_col = "calcium",
  num_vars = c(4, 6, 8),
  years = c(2, 5),
  is_north_american = TRUE,
  copy = TRUE
)

names(toy_kfre)
head(toy_kfre)
# Adds:
# kfre_4var_2year, kfre_4var_5year,
# kfre_6var_2year, kfre_6var_5year,
# kfre_8var_2year, kfre_8var_5year
```

### 5. CKD staging & ESRD outcome labels

```r
# ESRD outcome within 2 years (duration is in days → converted to years)
out <- data.frame(
  eGFR = c(95, 25),
  ESRD_flag = c(1, 1),
  followup_days = c(200, 1000)
)

out <- class_esrd_outcome(
  df = out,
  col = "ESRD_flag",
  years = 2,
  duration_col = "followup_days",
  prefix = "esrd",
  create_years_col = TRUE
)
# Adds: ESRD_duration_years and esrd_2_year_outcome

# CKD stage labels
out <- class_ckd_stages(
  df = out,
  egfr_col = "eGFR",
  stage_col = "stage",
  combined_stage_col = "stage_combined"
)

table(out$stage)
table(out$stage_combined)
```

### 6. uPCR → uACR conversion

```r
df_pcr <- data.frame(
  sex = c("female","male","female"),
  dm  = c(1,0,1),
  htn = c(1,1,0),
  pcr = c(150, 600, 50)
)

acr <- upcr_uacr(
  df_pcr,
  sex_col = "sex",
  diabetes_col = "dm",
  hypertension_col = "htn",
  upcr_col = "pcr",
  female_str = "female"
)

acr
```

### 7. Evaluation metrics (AUC-ROC, AP, Brier…)

Your data.frame must include:

- Outcome columns named like `*_2_year_outcome` / `*_5_year_outcome`
- Prediction columns named `kfre_{n}var_{year}year`, e.g. `kfre_4var_2year`

```r
met <- eval_kfre_metrics(
  df = toy_kfre,                 # must contain truth + prediction columns
  n_var_list = c(4, 6, 8),
  outcome_years = c(2, 5),
  decimal_places = 4
)

met
# Rows: Metrics; Cols: "{2_year|5_year}_{4|6|8}_var_kfre"
```

### 8. Plot ROC / PR curves

```r
# Basic: compute & plot both ROC and PR (no files written)
plot_kfre_metrics(
  df = toy_kfre,
  num_vars = c(4, 6, 8),
  plot_type = "all_plots",
  mode = "both",              # compute + plot
  show_years = c(2, 5)
)

# Save to disk (PNG/SVG)
plot_kfre_metrics(
  df = toy_kfre,
  num_vars = c(4, 6),
  plot_type = "auc_roc",
  mode = "both",
  show_years = c(2, 5),
  save_plots = TRUE,
  image_path_png = "plots",
  image_prefix = "kfre"
)
```

## Running Tests

If you’ve cloned the repo:

```r
library(devtools)
devtools::load_all(".")
devtools::test()
```

You should see unit tests for both the end-to-end flow and the evaluation utilities.

## API surface (exports)

- `RiskPredictor` (R6)
    - `$predict_kfre(years, is_north_american, use_extra_vars, num_vars)`
    - `$kfre_person(...)`
- `Wrappers:`
    - `predict_kfre(df, columns, years, is_north_american, use_extra_vars, num_vars)`
    - `add_kfre_risk_col(...)`
- `Utilities:`
- `upcr_uacr(...)`
- `perform_conversions(...)`
- `class_esrd_outcome(...)`
- `class_ckd_stages(...)`
- `eval_kfre_metrics(...)`
- `plot_kfre_metrics(...)`

## Notes on parity with Python

The R implementations are designed to mirror the Python versions (naming, shapes, 
and expected columns). Where packages differ (e.g., ROC/PR computation), we use 
`pROC` and `precrec` to maintain metric parity.

## References

- Tangri N, Grams ME, Levey AS, et al. (2016). Multinational assessment of 
accuracy of equations for predicting risk of kidney failure: A meta-analysis. 
JAMA, 315(2), 164–174. doi:10.1001/jama.2015.18202

- Tangri N, Stevens LA, Griffith J, et al. (2011). A predictive model for 
progression of chronic kidney disease to kidney failure. JAMA, 305(15), 
1553–1559. doi:10.1001/jama.2011.451

- Sumida K, Nadkarni GN, Grams ME, et al. (2020). Conversion of urine 
protein-creatinine ratio or urine dipstick protein to urine albumin-creatinine 
ratio for use in CKD screening and prognosis. Ann Intern Med, 173(6), 426–435. 
doi:10.7326/M20-0529

## License

`kfre` is distributed under the MIT License. See [`LICENSE`](LICENSE) for more information.
