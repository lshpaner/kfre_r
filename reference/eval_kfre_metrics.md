# Summarize KFRE performance metrics by model size and horizon

Builds a wide table of Precision, Sensitivity, Specificity, AUC, Brier,
and Average Precision for specified KFRE variants at 2 and 5 years.

## Usage

``` r
eval_kfre_metrics(df, n_var_list, outcome_years = 2, decimal_places = 6)
```

## Arguments

- df:

  Data frame with truth and probability columns.

- n_var_list:

  Integer vector of models to evaluate, any of 4, 6, 8.

- outcome_years:

  Integer vector of horizons, any of 2, 5.

- decimal_places:

  Integer digits to round displayed values.

## Value

Data frame of metrics with one column per model-horizon.
