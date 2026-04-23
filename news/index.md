# Changelog

## kfre 0.0.2

CRAN release: 2025-08-28

## kfre 0.0.1

Initial CRAN submission.

#### Added

- Core risk API:
  [`risk_pred_core()`](https://lshpaner.github.io/kfre_r/reference/risk_pred_core.md)
  (2- and 5-year KFRE).
- R6 helper: `RiskPredictor` with `predict_kfre()` and `kfre_person()`.
- Dataframe helper:
  [`add_kfre_risk_col()`](https://lshpaner.github.io/kfre_r/reference/add_kfre_risk_col.md)
  to append `kfre_<n>var_<y>year` columns.
- Performance utilities:
  [`eval_kfre_metrics()`](https://lshpaner.github.io/kfre_r/reference/eval_kfre_metrics.md)
  and
  [`plot_kfre_metrics()`](https://lshpaner.github.io/kfre_r/reference/plot_kfre_metrics.md).
- Outcome & staging:
  [`class_esrd_outcome()`](https://lshpaner.github.io/kfre_r/reference/class_esrd_outcome.md)
  and
  [`class_ckd_stages()`](https://lshpaner.github.io/kfre_r/reference/class_ckd_stages.md).
- Misc:
  [`apply_precision()`](https://lshpaner.github.io/kfre_r/reference/apply_precision.md),
  unit tests incl. Seoul demo (skipped if local data missing).
