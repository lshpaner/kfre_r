# Label CKD stages or ESRD outcomes.

Label CKD stages or ESRD outcomes.

## Usage

``` r
class_esrd_outcome(
  df,
  col,
  years,
  duration_col,
  prefix = NULL,
  create_years_col = TRUE
)
```

## Arguments

- df:

  Data frame with an eGFR column.

- col:

  Column name with ESRD event indicator, 0 or 1.

- years:

  Integer horizon, 2 or 5.

- duration_col:

  Column name with follow up time in days.

- prefix:

  Optional prefix for the derived outcome column.

- create_years_col:

  Logical, add a `<years>_year_outcome` column.

## Value

The modified data frame with added label columns.

## Examples

``` r
df <- data.frame(
  eGFR = c(90, 45, 25, 10),
  esrd = c(0, 0, 1, 0),
  followup_days = c(365, 800, 500, 1200)
)
class_esrd_outcome(
  df,
  col = "esrd",
  years = 2,
  duration_col = "followup_days"
)
#>   eGFR esrd followup_days ESRD_duration_years 2_year_outcome
#> 1   90    0           365           0.9993155              0
#> 2   45    0           800           2.1902806              0
#> 3   25    1           500           1.3689254              1
#> 4   10    0          1200           3.2854209              0
```
