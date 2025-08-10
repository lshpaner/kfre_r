test_that("class_esrd_outcome works as expected", {
  df <- data.frame(
    esrd_flag = c(1, 1, 0, 1),
    duration_days = c(200, 1000, 100, 500),
    stringsAsFactors = FALSE
  )
  out <- class_esrd_outcome(df, col = "esrd_flag", years = 2, duration_col = "duration_days", prefix = NULL, create_years_col = TRUE)
  expect_true("2_year_outcome" %in% names(out))
  expect_equal(sum(out[["2_year_outcome"]] == 1L), 2L)
  # idempotent overwrite
  out2 <- class_esrd_outcome(out, col = "esrd_flag", years = 2, duration_col = "duration_days", prefix = NULL, create_years_col = FALSE)
  expect_true("2_year_outcome" %in% names(out2))
})

test_that("class_ckd_stages produces expected labels", {
  df <- data.frame(eGFR = c(95, 65, 52, 32, 16, 10))
  out <- class_ckd_stages(df, egfr_col = "eGFR", stage_col = "ckd_stage", combined_stage_col = "ckd_stage_3_5")
  expect_true(all(c("ckd_stage","ckd_stage_3_5") %in% names(out)))
  expect_equal(out$ckd_stage[1], "CKD Stage 1")
  expect_equal(out$ckd_stage[6], "CKD Stage 5")
  expect_equal(out$ckd_stage_3_5[1], "Not Classified")
  expect_equal(out$ckd_stage_3_5[6], "CKD Stage 3 - 5")
})

test_that("eval_kfre_metrics returns transposed metrics table with correct labels", {
  set.seed(123)
  n <- 200
  truth_2 <- rbinom(n, 1, 0.3)
  truth_5 <- rbinom(n, 1, 0.5)
  df <- data.frame(
    `2_year_outcome` = truth_2,
    `5_year_outcome` = truth_5,
    kfre_4var_2year = runif(n),
    kfre_6var_2year = runif(n),
    kfre_8var_2year = runif(n),
    kfre_4var_5year = runif(n),
    kfre_6var_5year = runif(n),
    kfre_8var_5year = runif(n),
    check.names = FALSE
  )
  
  tab <- eval_kfre_metrics(df, n_var_list = c(4, 6, 8), outcome_years = c(2, 5), decimal_places = 4)
  expect_true(is.data.frame(tab))
  # Columns are outcomes by n_var like "2_year_4_var_kfre"
  expect_true(all(c("2_year_4_var_kfre","5_year_8_var_kfre") %in% colnames(tab)))
  # Rows are metric names
  expect_true(all(c("Precision/PPV","Average Precision","Sensitivity","Specificity","AUC ROC","Brier Score") %in% rownames(tab)))
  # Ranges reasonable
  for (nm in c("Precision/PPV","Average Precision","Sensitivity","Specificity","AUC ROC")) {
    expect_true(all(tab[nm, ] >= 0 & tab[nm, ] <= 1, na.rm = TRUE))
  }
  expect_true(all(tab["Brier Score", ] >= 0 & tab["Brier Score", ] <= 1, na.rm = TRUE))
})

test_that("plot_kfre_metrics(mode = 'prep') returns expected structure", {
  set.seed(42)
  n <- 120
  df <- data.frame(
    `2_year_outcome` = rbinom(n, 1, 0.3),
    `5_year_outcome` = rbinom(n, 1, 0.5),
    kfre_4var_2year = runif(n),
    kfre_6var_2year = runif(n),
    kfre_4var_5year = runif(n),
    kfre_6var_5year = runif(n),
    check.names = FALSE
  )
  res <- plot_kfre_metrics(
    df,
    num_vars = c(4, 6),
    mode = "prep",
    show_years = c(2, 5),
    plot_type = "all_plots",
    save_plots = FALSE
  )
  expect_true(is.list(res))
  expect_true(all(c("y_true","preds","outcomes") %in% names(res)))
  expect_equal(length(res$y_true), 2L)     # 2 and 5 years
  expect_true(all(c("4var","6var") %in% names(res$preds)))
  expect_equal(length(res$preds$`4var`), 2L)
  expect_equal(res$outcomes, c("2-year","5-year"))
})

test_that("plot_kfre_metrics errors on invalid inputs", {
  df <- data.frame(`2_year_outcome` = c(0,1), kfre_4var_2year = c(0.1, 0.9), check.names = FALSE)
  expect_error(plot_kfre_metrics(df, num_vars = 4, mode = "plot", show_years = 5),
               "Missing:")
  expect_error(plot_kfre_metrics(df, num_vars = 4, mode = "plot", show_years = 7),
               "must be any of")
  expect_error(eval_kfre_metrics(df, n_var_list = 10, outcome_years = 2),
               "Invalid variable number")
})
