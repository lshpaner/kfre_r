# kfre 0.0.2

# kfre 0.0.1

Initial CRAN submission.

### Added
- Core risk API: `risk_pred_core()` (2- and 5-year KFRE).
- R6 helper: `RiskPredictor` with `predict_kfre()` and `kfre_person()`.
- Dataframe helper: `add_kfre_risk_col()` to append `kfre_<n>var_<y>year` columns.
- Performance utilities: `eval_kfre_metrics()` and `plot_kfre_metrics()`.
- Outcome & staging: `class_esrd_outcome()` and `class_ckd_stages()`.
- Misc: `apply_precision()`, unit tests incl. Seoul demo (skipped if local data missing).
