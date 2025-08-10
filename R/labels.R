class_esrd_outcome <- function(df, col, years, duration_col, prefix=NULL, create_years_col=TRUE) {
  years_col <- duration_col
  if (isTRUE(create_years_col)) {
    years_col <- "ESRD_duration_years"
    df[[years_col]] <- df[[duration_col]] / 365.25
  }
  cname <- if (is.null(prefix)) sprintf("%s_year_outcome", years) else sprintf("%s_%s_year_outcome", prefix, years)
  if (cname %in% names(df)) df[[cname]] <- NULL
  df[[cname]] <- ifelse(df[[col]] == 1 & df[[years_col]] <= years, 1, 0)
  df
}
class_ckd_stages <- function(df, egfr_col="eGFR", stage_col=NULL, combined_stage_col=NULL) {
  if (!is.null(stage_col)) {
    x <- df[[egfr_col]]
    df[[stage_col]] <- ifelse(x >= 90, "CKD Stage 1",
                         ifelse(x >= 60, "CKD Stage 2",
                           ifelse(x >= 45, "CKD Stage 3a",
                             ifelse(x >= 30, "CKD Stage 3b",
                               ifelse(x >= 15, "CKD Stage 4", "CKD Stage 5")))))
    df[[stage_col]][is.na(x)] <- "Not Classified"
  }
  if (!is.null(combined_stage_col)) {
    x <- df[[egfr_col]]
    df[[combined_stage_col]] <- ifelse(x < 60, "CKD Stage 3 - 5", "Not Classified")
  }
  df
}
