risk_pred_core <- function(age, sex, eGFR, uACR, is_north_american,
                           dm = NULL, htn = NULL, albumin = NULL, phosphorous = NULL, bicarbonate = NULL, calcium = NULL,
                           years = 2) {
  six <- !is.null(dm) && !is.null(htn)
  eight <- !is.null(albumin) && !is.null(phosphorous) && !is.null(bicarbonate) && !is.null(calcium)
  if (six) {
    alpha <- list(`TRUE_2` = 0.9750, `TRUE_5` = 0.9240, `FALSE_2` = 0.9830, `FALSE_5` = 0.9370)
    rf <- list(age = -0.2218, sex = 0.2553, eGFR = -0.5541, uACR = 0.4562)
    dm_factor <- -0.1475
    htn_factor <- 0.1426
  } else if (eight) {
    alpha <- list(`TRUE_2` = 0.9780, `TRUE_5` = 0.9301, `FALSE_2` = 0.9827, `FALSE_5` = 0.9245)
    rf <- list(age = -0.1992, sex = 0.1602, eGFR = -0.4919, uACR = 0.3364)
    albumin_factor <- -0.3441
    phosph_factor <- +0.2604
    bicarb_factor <- -0.07354
    calcium_factor <- -0.2228
  } else {
    alpha <- list(`TRUE_2` = 0.9750, `TRUE_5` = 0.9240, `FALSE_2` = 0.9832, `FALSE_5` = 0.9365)
    rf <- list(age = -0.2201, sex = 0.2467, eGFR = -0.5567, uACR = 0.4510)
  }
  uACR <- pmax(uACR, 1e-6)
  log_uACR <- log(uACR)
  risk_score <- rf$age * (age / 10 - 7.036) + rf$sex * (sex - 0.5642) + rf$eGFR * (eGFR / 5 - 7.222) + rf$uACR * (log_uACR - 5.137)
  if (six) risk_score <- risk_score + dm_factor * (dm - 0.5106) + htn_factor * (htn - 0.8501)
  if (eight) risk_score <- risk_score + albumin_factor * (albumin - 3.997) + phosph_factor * (phosphorous - 3.916) + bicarb_factor * (bicarbonate - 25.57) + calcium_factor * (calcium - 9.355)
  a <- alpha[[paste0(isTRUE(is_north_american), "_", as.integer(years))]]
  1 - (a^exp(risk_score))
}
RiskPredictor <- R6::R6Class(
  "RiskPredictor",
  public = list(
    df = NULL, columns = NULL,
    initialize = function(df = NULL, columns = NULL) {
      self$df <- df
      self$columns <- columns
    },
    predict_kfre = function(years, is_north_american, use_extra_vars = FALSE, num_vars = 4, precision = NULL) {
      cols <- self$columns
      d <- self$df
      sex_num <- as.integer(tolower(d[[cols$sex]]) == "male")
      if (use_extra_vars && num_vars == 6) {
        res <- risk_pred_core(d[[cols$age]], sex_num, d[[cols$eGFR]], d[[cols$uACR]], is_north_american, dm = d[[cols$dm]], htn = d[[cols$htn]], years = years)
      } else if (use_extra_vars && num_vars == 8) {
        res <- risk_pred_core(d[[cols$age]], sex_num, d[[cols$eGFR]], d[[cols$uACR]], is_north_american, albumin = d[[cols$albumin]], phosphorous = d[[cols$phosphorous]], bicarbonate = d[[cols$bicarbonate]], calcium = d[[cols$calcium]], years = years)
      } else {
        res <- risk_pred_core(d[[cols$age]], sex_num, d[[cols$eGFR]], d[[cols$uACR]], is_north_american, years = years)
      }
      if (is.null(precision)) res else round(res, precision)
    },
    kfre_person = function(age, is_male, eGFR, uACR, is_north_american, years = 2, dm = NULL, htn = NULL, albumin = NULL, phosphorous = NULL, bicarbonate = NULL, calcium = NULL, precision = NULL) {
      res <- risk_pred_core(age, as.integer(is_male), eGFR, uACR, is_north_american, dm = dm, htn = htn, albumin = albumin, phosphorous = phosphorous, bicarbonate = bicarbonate, calcium = calcium, years = years)
      if (is.null(precision)) res else round(res, precision)
    }
  )
)
add_kfre_risk_col <- function(df, age_col = NULL, sex_col = NULL, eGFR_col = NULL, uACR_col = NULL, dm_col = NULL, htn_col = NULL, albumin_col = NULL, phosphorous_col = NULL, bicarbonate_col = NULL, calcium_col = NULL, num_vars = 8, years = c(2, 5), is_north_american = FALSE, copy = TRUE, precision = NULL) {
  df_used <- if (isTRUE(copy)) data.frame(df, check.names = FALSE) else df
  cols <- list(age = age_col, sex = sex_col, eGFR = eGFR_col, uACR = uACR_col, dm = dm_col, htn = htn_col, albumin = albumin_col, phosphorous = phosphorous_col, bicarbonate = bicarbonate_col, calcium = calcium_col)
  rp <- RiskPredictor$new(df = df_used, columns = cols)
  nvars <- if (identical(num_vars, "all")) c(4, 6, 8) else as.integer(num_vars)
  yrs <- if (identical(years, "all")) c(2, 5) else as.integer(years)
  for (nv in nvars) {
    for (yy in yrs) {
      cname <- sprintf("kfre_%svar_%syear", nv, yy)
      df_used[[cname]] <- rp$predict_kfre(years = yy, is_north_american = is_north_american, use_extra_vars = (nv > 4), num_vars = nv, precision = precision)
    }
  }
  df_used
}
