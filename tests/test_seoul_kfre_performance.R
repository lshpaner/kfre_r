# scripts/test_seoul_kfre_performance.R
# Standalone runner for the Seoul Excel file.
# Prints metrics to console and shows plots in the RStudio Plots pane.

suppressPackageStartupMessages({
  library(readxl)
  # If running inside the package project, this loads the local code in R/
  if (requireNamespace("devtools", quietly = TRUE)) {
    try(devtools::load_all(quiet = TRUE), silent = TRUE)
  }
  library(kfreR)
})

message("== kfreR Seoul KFRE runner ==")

# 1) Locate Excel file
paths <- c(
  file.path("data", "kfre_3_5_preds.xlsx"),
  file.path("..", "data", "kfre_3_5_preds.xlsx")
)
path <- paths[file.exists(paths)][1]
if (is.na(path)) {
  stop("Could not find 'data/kfre_3_5_preds.xlsx'. Put the file under ./data/ and rerun.")
}
message(sprintf("Reading: %s", normalizePath(path, winslash = "/")))

# 2) Read
df <- readxl::read_xlsx(path)
stopifnot(is.data.frame(df), nrow(df) > 0L)

# 3) Detect available outcome years and KFRE columns
years_avail <- integer()
for (yr in c(2L, 5L)) {
  if (any(grepl(sprintf(".*%d_year_outcome$", yr), names(df)))) years_avail <- c(years_avail, yr)
}
if (length(years_avail) == 0L) {
  stop("No '*_year_outcome' columns found. Expected something like '2_year_outcome' or '5_year_outcome'.")
}

vars_avail <- integer()
for (nv in c(4L, 6L, 8L)) {
  has_any <- any(grepl(sprintf("^kfre_%dvar_(%s)year$", nv, paste(years_avail, collapse = "|")), names(df)))
  if (has_any) vars_avail <- c(vars_avail, nv)
}
if (length(vars_avail) == 0L) {
  stop("No 'kfre_*var_*year' columns found to plot or score.")
}

message(sprintf("Detected outcome years: %s", paste(years_avail, collapse = ", ")))
message(sprintf("Detected KFRE models: %s vars", paste(vars_avail, collapse = ", ")))

# 3a) Ensure numeric types for relevant columns if Excel read them as character
num_candidates <- unlist(lapply(vars_avail, function(nv) sprintf("kfre_%dvar_%dyear", nv, years_avail)))
num_candidates <- c(num_candidates, sprintf("%d_year_outcome", years_avail))
num_candidates <- intersect(num_candidates, names(df))
if (length(num_candidates)) {
  for (cn in num_candidates) {
    if (!is.numeric(df[[cn]])) {
      suppressWarnings(df[[cn]] <- as.numeric(df[[cn]]))
    }
  }
}

# 4) Optional feature engineering if present
if (all(c("esrd_flag", "duration_days") %in% names(df))) {
  df <- class_esrd_outcome(df, col = "esrd_flag", years = 2, duration_col = "duration_days", create_years_col = TRUE)
}

if ("eGFR" %in% names(df)) {
  df <- class_ckd_stages(df, egfr_col = "eGFR", stage_col = "ckd_stage", combined_stage_col = "ckd_stage_3_5")
}

# 5) Compute and print metrics
metrics_tab <- eval_kfre_metrics(
  df,
  n_var_list = vars_avail,
  outcome_years = years_avail,
  decimal_places = 6
)

cat("\n== Metrics table ==\n")
print(metrics_tab)

# 6) Plot to RStudio Plots pane, no popups
# If you want separate windows sized by fig_size, set open_new_device = TRUE
cat("\n== Plotting ROC and PR to Plots pane ==\n")
invisible(
  plot_kfre_metrics(
    df,
    num_vars = c(4, 6),            # overlay 4 and 6 on the same axes
    mode = "plot",
    show_years = years_avail,
    plot_type = "all_plots",
    plot_combinations = TRUE,      # overlay models
    save_plots = FALSE,
    open_new_device = FALSE,       # keep in Plots pane
    fig_size = c(15, 4)            # RStudio pane may stretch, adjust pane or size as needed
  )
)

# 7) Also save copies to tempdir with explicit size
out_png <- file.path(tempdir(), "seoul_kfre_png")
out_svg <- file.path(tempdir(), "seoul_kfre_svg")
dir.create(out_png, recursive = TRUE, showWarnings = FALSE)
dir.create(out_svg, recursive = TRUE, showWarnings = FALSE)

cat("\n== Saving combined plots to tempdir ==\n")
invisible(
  plot_kfre_metrics(
    df,
    num_vars = vars_avail,
    mode = "plot",
    show_years = years_avail,
    plot_type = "all_plots",
    plot_combinations = TRUE,
    save_plots = TRUE,
    image_path_png = out_png,
    image_path_svg = out_svg,
    image_prefix = "seoul_kfre",
    fig_size = c(8, 8),
    open_new_device = FALSE
  )
)

cat("\nSaved PNGs to: ", out_png, "\n", sep = "")
cat("Saved SVGs to: ", out_svg, "\n", sep = "")
cat("\nDone.\n")
