# Perform unit and code conversions used by KFRE helpers.

Perform unit and code conversions used by KFRE helpers.

## Usage

``` r
perform_conversions(
  df,
  reverse = FALSE,
  convert_all = FALSE,
  upcr_col = NULL,
  calcium_col = NULL,
  phosphate_col = NULL,
  albumin_col = NULL
)
```

## Arguments

- df:

  Data frame with source columns to convert.

- reverse:

  Logical, reverse the conversion if `TRUE`.

- convert_all:

  Logical, convert all known columns if `TRUE`.

- upcr_col:

  Column name for urine protein creatinine ratio.

- calcium_col:

  Optional column for serum calcium, mg/dL.

- phosphate_col:

  Optional column for serum phosphorus, mg/dL.

- albumin_col:

  Optional column for serum albumin, g/dL.

## Value

A data frame with converted columns.

## Examples

``` r
df <- data.frame(
  upcr = c(100, 400),         # mg/g (or g/g scaled accordingly)
  albumin = c(40, 38),        # g/L
  phosphorous = c(1.1, 1.3),  # mmol/L
  calcium = c(9.2, 8.8)       # mg/dL
)
perform_conversions(df)
#>   upcr albumin phosphorous calcium
#> 1  100      40         1.1     9.2
#> 2  400      38         1.3     8.8
```
