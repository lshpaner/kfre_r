testthat::test_that("Seoul Excel demo runs when deps and file exist", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("readxl")

  excel_path <- file.path("data", "kfre_3_5_preds.xlsx")
  testthat::skip_if(!file.exists(excel_path), "Local Excel file not available")

  df <- readxl::read_xlsx(excel_path)
  testthat::expect_true(is.data.frame(df))
  testthat::expect_true(nrow(df) > 0L)

  years_avail <- integer()
  for (yr in c(2L, 5L)) {
    if (any(grepl(sprintf(".*%d_year_outcome$", yr), names(df)))) {
      years_avail <- c(years_avail, yr)
    }
  }
  testthat::skip_if(length(years_avail) == 0L, "No *_year_outcome columns found")

  vars_avail <- integer()
  for (nv in c(4L, 6L, 8L)) {
    has_any <- any(grepl(
      sprintf(
        "^kfre_%dvar_(%s)year$",
        nv, paste(years_avail, collapse = "|")
      ),
      names(df)
    ))
    if (has_any) vars_avail <- c(vars_avail, nv)
  }
  testthat::skip_if(length(vars_avail) == 0L, "No kfre_*var_*year columns found")

  # Coerce numerics if needed
  num_candidates <- unlist(lapply(
    vars_avail, function(nv) sprintf("kfre_%dvar_%dyear", nv, years_avail)
  ))
  num_candidates <- c(num_candidates, sprintf("%d_year_outcome", years_avail))
  num_candidates <- intersect(num_candidates, names(df))
  if (length(num_candidates)) {
    for (cn in num_candidates) {
      if (!is.numeric(df[[cn]])) suppressWarnings(df[[cn]] <- as.numeric(df[[cn]]))
    }
  }

  # Optional feature engineering
  if (all(c("esrd_flag", "duration_days") %in% names(df))) {
    df <- class_esrd_outcome(
      df,
      col = "esrd_flag", years = 2,
      duration_col = "duration_days", create_years_col = TRUE
    )
  }
  if ("eGFR" %in% names(df)) {
    df <- class_ckd_stages(
      df,
      egfr_col = "eGFR",
      stage_col = "ckd_stage",
      combined_stage_col = "ckd_stage_3_5"
    )
  }

  # Metrics should compute
  metrics_tab <- eval_kfre_metrics(
    df,
    n_var_list = vars_avail, outcome_years = years_avail, decimal_places = 6
  )
  testthat::expect_true(is.data.frame(metrics_tab))
  testthat::expect_true(nrow(metrics_tab) >= 1L)

  # Plotting should not error
  testthat::expect_silent(
    plot_kfre_metrics(
      df,
      num_vars = vars_avail,
      mode = "plot",
      show_years = years_avail,
      plot_type = "all_plots",
      plot_combinations = TRUE,
      save_plots = FALSE,
      open_new_device = FALSE,
      fig_size = c(12, 4)
    )
  )
})
