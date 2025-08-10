# scripts/run_perform_eval_demo.R
# Demo runner for perform_eval functions. Shows plots and prints tables.

suppressPackageStartupMessages({
  library(kfreR)
})

set.seed(123)

# 1) Make a toy dataset with truth columns and KFRE probabilities
n <- 1000
df <- data.frame(
  `2_year_outcome` = rbinom(n, 1, 0.30),
  `5_year_outcome` = rbinom(n, 1, 0.50),
  eGFR = pmax(5, rnorm(n, mean = 55, sd = 20)),
  esrd_flag = rbinom(n, 1, 0.15),
  duration_days = sample(30:2000, n, replace = TRUE),
  kfre_4var_2year = runif(n),
  kfre_6var_2year = runif(n),
  kfre_8var_2year = runif(n),
  kfre_4var_5year = runif(n),
  kfre_6var_5year = runif(n),
  kfre_8var_5year = runif(n),
  check.names = FALSE
)

# 2) Derive ESRD outcomes for a given year using your helper
df <- class_esrd_outcome(
  df,
  col = "esrd_flag",
  years = 2,
  duration_col = "duration_days",
  prefix = NULL,
  create_years_col = TRUE
)

# 3) Classify CKD stages from eGFR
df <- class_ckd_stages(
  df,
  egfr_col = "eGFR",
  stage_col = "ckd_stage",
  combined_stage_col = "ckd_stage_3_5"
)

cat("CKD stage distribution:\n")
print(table(df$ckd_stage, useNA = "ifany"))
cat("\nCombined CKD 3 to 5 distribution:\n")
print(table(df$ckd_stage_3_5, useNA = "ifany"))

# 4) Compute metrics table
metrics_tab <- eval_kfre_metrics(
  df,
  n_var_list = c(4, 6, 8),
  outcome_years = c(2, 5),
  decimal_places = 4
)
cat("\nMetrics table:\n")
print(metrics_tab)

# 5) Plot metrics, interactive windows, no saving
invisible(
  plot_kfre_metrics(
    df,
    num_vars = c(4, 6, 8),
    mode = "both",
    show_years = c(2, 5),
    plot_type = "all_plots",
    plot_combinations = FALSE,
    show_subplots = FALSE,
    save_plots = FALSE,
    fig_size = c(10, 6),
    decimal_places = 3
  )
)

# 6) Optional: save plots to a temp directory for later inspection
out_dir_png <- file.path(tempdir(), "kfre_demo_png")
out_dir_svg <- file.path(tempdir(), "kfre_demo_svg")
dir.create(out_dir_png, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_svg, recursive = TRUE, showWarnings = FALSE)

invisible(
  plot_kfre_metrics(
    df,
    num_vars = c(4, 6),
    mode = "both",
    show_years = c(2, 5),
    plot_type = "all_plots",
    plot_combinations = TRUE,
    show_subplots = FALSE,
    bbox_inches = "tight",
    save_plots = TRUE,
    image_path_png = out_dir_png,
    image_path_svg = out_dir_svg,
    image_prefix = "kfre_demo",
    fig_size = c(8,6),
    decimal_places = 3,
    open_new_device  = FALSE
  )
)

cat("\nSaved combined plots to:\n")
cat(sprintf("PNG: %s\n", out_dir_png))
cat(sprintf("SVG: %s\n", out_dir_svg))
cat("\nDone.\n")
