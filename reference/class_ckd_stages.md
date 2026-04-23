# Label CKD stages or ESRD outcomes.

Label CKD stages or ESRD outcomes.

## Usage

``` r
class_ckd_stages(
  df,
  egfr_col = "eGFR",
  stage_col = NULL,
  combined_stage_col = NULL
)
```

## Arguments

- df:

  Data frame input.

- egfr_col:

  Column name for eGFR, mL/min/1.73 m^2.

- stage_col:

  Output column name for detailed CKD stages.

- combined_stage_col:

  Output column for combined stages 3 to 5.

## Value

The modified data frame with added label columns.

## Examples

``` r
df <- data.frame(eGFR = c(92, 58, 42, 28, 12))
class_ckd_stages(df, egfr_col = "eGFR")
#>   eGFR
#> 1   92
#> 2   58
#> 3   42
#> 4   28
#> 5   12
```
