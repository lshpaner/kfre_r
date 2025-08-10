testthat::test_that("kfre end to end mirrors Python script", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("R6")

  ###############################################
  # Toy dataset and column map
  ###############################################
  toy <- data.frame(
    age = c(55, 72),
    sex_txt = c("male", "female"),
    eGFR = c(45, 28),
    uACR = c(120, 800),
    dm = c(1, 0),
    htn = c(1, 1),
    albumin = c(4.2, 3.4),
    phosphorous = c(3.3, 4.6),
    bicarbonate = c(24, 22),
    calcium = c(9.1, 9.8),
    stringsAsFactors = FALSE
  )

  cols <- list(
    age = "age",
    sex = "sex_txt",
    eGFR = "eGFR",
    uACR = "uACR",
    dm = "dm",
    htn = "htn",
    albumin = "albumin",
    phosphorous = "phosphorous",
    bicarbonate = "bicarbonate",
    calcium = "calcium"
  )

  # Use internal R6 via ::: so it works in installed-check context
  rp <- kfre:::RiskPredictor$new(df = toy, columns = cols)

  ###############################################
  # Vectorized predictions with precision, 4, 6, 8 variable models
  ###############################################
  p4 <- rp$predict_kfre(
    years = 2, is_north_american = TRUE,
    use_extra_vars = FALSE, num_vars = 4,
    precision = 10
  )
  p6 <- rp$predict_kfre(
    years = 5, is_north_american = TRUE,
    use_extra_vars = TRUE, num_vars = 6,
    precision = 11
  )
  p8 <- rp$predict_kfre(
    years = 2, is_north_american = TRUE,
    use_extra_vars = TRUE, num_vars = 8,
    precision = 10
  )

  ###############################################
  # kfre_person parity with vectorized outputs, precision applied
  ###############################################
  fmt <- function(x, n = 10) sprintf(paste0("%.", n, "f"), x)

  # row 0
  p0_4 <- rp$kfre_person(
    age = toy$age[1], is_male = TRUE,
    eGFR = toy$eGFR[1], uACR = toy$uACR[1],
    is_north_american = TRUE, years = 2,
    precision = 10
  )
  p0_6 <- rp$kfre_person(
    age = toy$age[1], is_male = TRUE,
    eGFR = toy$eGFR[1], uACR = toy$uACR[1],
    is_north_american = TRUE, years = 5,
    dm = toy$dm[1], htn = toy$htn[1],
    precision = 11
  )
  p0_8 <- rp$kfre_person(
    age = toy$age[1], is_male = TRUE,
    eGFR = toy$eGFR[1], uACR = toy$uACR[1],
    is_north_american = TRUE, years = 2,
    albumin = toy$albumin[1], phosphorous = toy$phosphorous[1],
    bicarbonate = toy$bicarbonate[1], calcium = toy$calcium[1],
    precision = 10
  )

  # row 1
  p1_4 <- rp$kfre_person(
    age = toy$age[2], is_male = FALSE,
    eGFR = toy$eGFR[2], uACR = toy$uACR[2],
    is_north_american = TRUE, years = 2,
    precision = 10
  )
  p1_6 <- rp$kfre_person(
    age = toy$age[2], is_male = FALSE,
    eGFR = toy$eGFR[2], uACR = toy$uACR[2],
    is_north_american = TRUE, years = 5,
    dm = toy$dm[2], htn = toy$htn[2],
    precision = 11
  )
  p1_8 <- rp$kfre_person(
    age = toy$age[2], is_male = FALSE,
    eGFR = toy$eGFR[2], uACR = toy$uACR[2],
    is_north_american = TRUE, years = 2,
    albumin = toy$albumin[2], phosphorous = toy$phosphorous[2],
    bicarbonate = toy$bicarbonate[2], calcium = toy$calcium[2],
    precision = 10
  )

  # parity with vectorized predictions, ultra tight
  testthat::expect_equal(p0_4, p4[1], tolerance = 1e-12)
  testthat::expect_equal(p1_4, p4[2], tolerance = 1e-12)
  testthat::expect_equal(p0_6, p6[1], tolerance = 1e-12)
  testthat::expect_equal(p1_6, p6[2], tolerance = 1e-12)
  testthat::expect_equal(p0_8, p8[1], tolerance = 1e-12)
  testthat::expect_equal(p1_8, p8[2], tolerance = 1e-12)

  ###############################################
  # add_kfre_risk_col across all models and years, precision applied
  ###############################################
  toy_kfre <- kfre::add_kfre_risk_col(
    df = toy,
    age_col = "age",
    sex_col = "sex_txt",
    eGFR_col = "eGFR",
    uACR_col = "uACR",
    dm_col = "dm",
    htn_col = "htn",
    albumin_col = "albumin",
    phosphorous_col = "phosphorous",
    bicarbonate_col = "bicarbonate",
    calcium_col = "calcium",
    num_vars = c(4, 6, 8),
    years = c(2, 5),
    is_north_american = TRUE,
    copy = TRUE,
    precision = 20
  )

  expected_cols <- c(
    "kfre_4var_2year", "kfre_4var_5year",
    "kfre_6var_2year", "kfre_6var_5year",
    "kfre_8var_2year", "kfre_8var_5year"
  )
  testthat::expect_true(all(expected_cols %in% names(toy_kfre)))

  # Compare with aligned rounding, zero tolerance
  testthat::expect_equal(
    as.numeric(round(toy_kfre$kfre_4var_2year, 10)),
    as.numeric(round(p4, 10)),
    tolerance = 0
  )
  testthat::expect_equal(
    as.numeric(round(toy_kfre$kfre_6var_5year, 11)),
    as.numeric(round(p6, 11)),
    tolerance = 0
  )
  testthat::expect_equal(
    as.numeric(round(toy_kfre$kfre_8var_2year, 10)),
    as.numeric(round(p8, 10)),
    tolerance = 0
  )

  ###############################################
  # Extras: exercise remaining exported helpers
  ###############################################
  raw_vec <- c(0.123456789, 0.987654321)
  testthat::expect_identical(kfre::apply_precision(raw_vec, 6), round(raw_vec, 6))

  df_conv <- data.frame(
    uPCR_val = c(10, 20),
    Calcium_val = c(9.5, 10.0),
    Phosphate_val = c(3.2, 4.1),
    Albumin_val = c(4.0, 3.5)
  )
  conv_out <- kfre::perform_conversions(
    df_conv,
    convert_all = FALSE,
    upcr_col = "uPCR_val",
    calcium_col = "Calcium_val",
    phosphate_col = "Phosphate_val",
    albumin_col = "Albumin_val"
  )
  testthat::expect_true(all(c(
    "uPCR_mg_g", "Calcium_mg_dl", "Phosphate_mg_dl", "Albumin_g_dl"
  ) %in% names(conv_out)))

  df_o <- data.frame(
    eGFR = c(95, 25),
    ESRD_flag = c(1, 1),
    days = c(200, 1000)
  )
  df_o <- kfre::class_esrd_outcome(
    df_o,
    col = "ESRD_flag", years = 2, duration_col = "days",
    prefix = "esrd", create_years_col = TRUE
  )
  testthat::expect_true(all(c("ESRD_duration_years", "esrd_2_year_outcome") %in% names(df_o)))

  df_o <- kfre::class_ckd_stages(
    df_o,
    egfr_col = "eGFR",
    stage_col = "stage", combined_stage_col = "stage_combined"
  )
  testthat::expect_true(all(df_o$stage %in% c(
    "CKD Stage 1", "CKD Stage 2", "CKD Stage 3a",
    "CKD Stage 3b", "CKD Stage 4", "CKD Stage 5", "Not Classified"
  )))
  testthat::expect_true(all(df_o$stage_combined %in% c("CKD Stage 3 - 5", "Not Classified")))

  df_pcr <- data.frame(
    sex = c("female", "male", "female"),
    dm = c(1, 0, 1),
    htn = c(1, 1, 0),
    pcr = c(150, 600, 50)
  )
  acr <- kfre::upcr_uacr(
    df_pcr,
    sex_col = "sex", diabetes_col = "dm",
    hypertension_col = "htn", upcr_col = "pcr", female_str = "female"
  )
  testthat::expect_equal(length(acr), nrow(df_pcr))
})
