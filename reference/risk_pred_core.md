# KFRE risk prediction for a single person

Computes the Kidney Failure Risk Equation probability at 2 or 5 years.

## Usage

``` r
risk_pred_core(
  age,
  sex,
  eGFR,
  uACR,
  is_north_american,
  dm = NULL,
  htn = NULL,
  albumin = NULL,
  phosphorous = NULL,
  bicarbonate = NULL,
  calcium = NULL,
  years = 2
)
```

## Arguments

- age:

  Numeric age in years.

- sex:

  Integer sex indicator, 1 for male, 0 for female.

- eGFR:

  Estimated glomerular filtration rate, mL/min/1.73 \\\text{m}^{2}\\.

- uACR:

  Urine albumin to creatinine ratio, mg/g.

- is_north_american:

  Logical, patient from a North American cohort.

- dm:

  Optional integer diabetes indicator, 1 yes, 0 no.

- htn:

  Optional integer hypertension indicator, 1 yes, 0 no.

- albumin:

  Optional serum albumin, g/dL, required for 8 variable model.

- phosphorous:

  Optional serum phosphorus, mg/dL, 8 variable model.

- bicarbonate:

  Optional serum bicarbonate, mmol/L, 8 variable model.

- calcium:

  Optional serum calcium, mg/dL, 8 variable model.

- years:

  Integer, prediction horizon, 2 or 5.

## Value

Numeric probability between 0 and 1.

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
risk_pred_core(60, 1, 45, 120, TRUE, dm = 1, htn = 1, years = 2)
#> [1] 0.01071585
```
