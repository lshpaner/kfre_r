# Add KFRE risk columns to a data frame

Adds KFRE risk columns for selected model sizes and horizons using the
4, 6, or 8 variable equations.

## Usage

``` r
add_kfre_risk_col(
  df,
  age_col = NULL,
  sex_col = NULL,
  eGFR_col = NULL,
  uACR_col = NULL,
  dm_col = NULL,
  htn_col = NULL,
  albumin_col = NULL,
  phosphorous_col = NULL,
  bicarbonate_col = NULL,
  calcium_col = NULL,
  num_vars = 8,
  years = c(2, 5),
  is_north_american = FALSE,
  copy = TRUE,
  precision = NULL
)
```

## Arguments

- df:

  Data frame with predictor columns.

- age_col:

  Column name for age.

- sex_col:

  Column name for sex, text or integer accepted.

- eGFR_col:

  Column name for eGFR, mL/min/1.73 m^2.

- uACR_col:

  Column name for uACR, mg/g.

- dm_col:

  Optional column name for diabetes indicator.

- htn_col:

  Optional column name for hypertension indicator.

- albumin_col:

  Optional column name for serum albumin, g/dL.

- phosphorous_col:

  Optional column name for serum phosphorus, mg/dL.

- bicarbonate_col:

  Optional column name for bicarbonate, mmol/L.

- calcium_col:

  Optional column name for calcium, mg/dL.

- num_vars:

  Integer or vector, one of 4, 6, 8.

- years:

  Integer or vector, any of 2, 5.

- is_north_american:

  Logical, use North American calibration.

- copy:

  Logical, if TRUE work on a copy of `df`.

- precision:

  Optional integer, digits to round probabilities.

## Value

The input data frame with added `kfre_<n>var_<y>year` columns.

## References

Tangri, N., Stevens, L. A., Griffith, J., Tighiouart, H., Djurdjev, O.,
Naimark, D., Levin, A., & Levey, A. S. (2011). A predictive model for
progression of chronic kidney disease to kidney failure. *JAMA*,
305(15), 1553–1559.
[doi:10.1001/jama.2011.451](https://doi.org/10.1001/jama.2011.451)

Tangri, N., Grams, M. E., Levey, A. S., et al. (2016). Multinational
assessment of the accuracy of the Kidney Failure Risk Equation in people
with chronic kidney disease. *JAMA*, 315(2), 164–174.
[doi:10.1001/jama.2015.18202](https://doi.org/10.1001/jama.2015.18202)

## Examples

``` r
df <- data.frame(
  age = 60L, sex = 1L, eGFR = 30, uACR = 500,
  dm = 1L, htn = 0L, albumin = 40,
  phosphorous = 1.1, bicarbonate = 24, calcium = 9.2
)

add_kfre_risk_col(
  df,
  age_col = "age", sex_col = "sex",
  eGFR_col = "eGFR", uACR_col = "uACR",
  num_vars = 4, years = 2
)
#>   age sex eGFR uACR dm htn albumin phosphorous bicarbonate calcium
#> 1  60   1   30  500  1   0      40         1.1          24     9.2
#>   kfre_4var_2year
#> 1      0.05770765
```
