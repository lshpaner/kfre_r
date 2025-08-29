#' Perform unit and code conversions used by KFRE helpers.
#'
#' @param df Data frame with source columns to convert.
#' @param reverse Logical, reverse the conversion if `TRUE`.
#' @param convert_all Logical, convert all known columns if `TRUE`.
#' @param upcr_col Column name for urine protein creatinine ratio.
#' @param calcium_col Optional column for serum calcium, mg/dL.
#' @param phosphate_col Optional column for serum phosphorus, mg/dL.
#' @param albumin_col Optional column for serum albumin, g/dL.
#' @return A data frame with converted columns.
#' @export
#' @examples
#' df <- data.frame(
#'   upcr = c(100, 400),         # mg/g (or g/g scaled accordingly)
#'   albumin = c(40, 38),        # g/L
#'   phosphorous = c(1.1, 1.3),  # mmol/L
#'   calcium = c(9.2, 8.8)       # mg/dL
#' )
#' perform_conversions(df)
perform_conversions <- function(
    df,
    reverse = FALSE,
    convert_all = FALSE,
    upcr_col = NULL,
    calcium_col = NULL,
    phosphate_col = NULL,
    albumin_col = NULL) {
  conv_factors <- list(
    "uPCR" = 1 / 0.11312, "Calcium" = 4, "Phosphate" = 3.1,
    "Albumin" = 1 / 10
  )
  conv_suffix <- list(
    "uPCR" = if (!reverse) "mg_g" else "mmol_L",
    "Calcium" = if (!reverse) "mg_dl" else "mmol_L",
    "Phosphate" = if (!reverse) "mg_dl" else "mmol_L",
    "Albumin" = if (!reverse) "g_dl" else "g_L"
  )
  cols <- list(
    "uPCR" = upcr_col, "Calcium" = calcium_col, "Phosphate" = phosphate_col,
    "Albumin" = albumin_col
  )
  if (convert_all) {
    for (nm in names(conv_factors)) {
      idx <- which(grepl(tolower(nm), tolower(names(df)), fixed = TRUE))
      if (length(idx)) cols[[nm]] <- names(df)[idx[1]]
    }
  }
  out <- df
  for (nm in names(cols)) {
    col <- cols[[nm]]
    if (!is.null(col) && col %in% names(df)) {
      factor <- conv_factors[[nm]]
      suf <- conv_suffix[[nm]]
      vals <- df[[col]]
      new_vals <- if (reverse) vals / factor else vals * factor
      new_col <- paste0(nm, "_", suf)
      out[[new_col]] <- new_vals
      message(sprintf(
        "Converted '%s' to new column '%s' with factor %s", col, new_col,
        factor
      ))
    }
  }
  out
}
