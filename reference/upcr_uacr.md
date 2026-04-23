# Convert UPCR to UACR with clinical covariates

Implements the equation reported by Sumida et al. for converting urine
protein–creatinine ratio to urine albumin–creatinine ratio, with
adjustments for sex, diabetes, and hypertension.

## Usage

``` r
upcr_uacr(
  df,
  sex_col,
  diabetes_col,
  hypertension_col,
  upcr_col,
  female_str = "female"
)
```

## Arguments

- df:

  A data.frame containing the required columns.

- sex_col:

  Column name with sex labels, character. Example: "sex".

- diabetes_col:

  Column name with diabetes indicator, 0 or 1, or logical. Example:
  "diabetes".

- hypertension_col:

  Column name with hypertension indicator, 0 or 1, or logical. Example:
  "hypertension".

- upcr_col:

  Column name with UPCR values. Units should match those used to derive
  the model in Sumida et al. (mg/g). If your data use different units,
  convert before calling.

- female_str:

  String that denotes female in `sex_col`. Default is "female".

## Value

A numeric vector of UACR values, length `nrow(df)`. Non valid rows
return `NA_real_`.

## Details

The function applies a piecewise log transformation of UPCR with cut
points at 50 and 500, and adds covariate adjustments for sex, diabetes,
and hypertension, then exponentiates to return UACR. Valid rows require
non missing diabetes and hypertension indicators. Sex is mapped to an
indicator using `female_str`.

## References

Sumida, K., Nadkarni, G. N., Grams, M. E., Sang, Y., Ballew, S. H.,
Coresh, J., Matsushita, K., Surapaneni, A., Brunskill, N., Chadban, S.
J., Chang, A. R., Cirillo, M., Daratha, K. B., Gansevoort, R. T., Garg,
A. X., Iacoviello, L., Kayama, T., Konta, T., Kovesdy, C. P., Lash, J.,
Lee, B. J., Major, R. W., Metzger, M., Miura, K., Naimark, D. M. J.,
Nelson, R. G., Sawhney, S., Stempniewicz, N., Tang, M., Townsend, R. R.,
Traynor, J. P., Valdivielso, J. M., Wetzels, J., Polkinghorne, K. R.,
and Heerspink, H. J. L. (2020). Conversion of urine protein-creatinine
ratio or urine dipstick protein to urine albumin-creatinine ratio for
use in chronic kidney disease screening and prognosis. *Annals of
Internal Medicine*, 173(6), 426-435.
[doi:10.7326/M20-0529](https://doi.org/10.7326/M20-0529)

## Examples

``` r
df <- data.frame(
  sex = c("female", "male"),
  diabetes = c(1, 0),
  hypertension = c(0, 1),
  upcr = c(100, 400)  # mg/g (or same ratio units)
)
upcr_uacr(df, "sex", "diabetes", "hypertension", "upcr")
#> [1]  15.6529 154.9863

```
