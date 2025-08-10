#' Convert UPCR to UACR with clinical covariates
#'
#' Implements the equation reported by Sumida et al. for converting urine protein–creatinine ratio
#' to urine albumin–creatinine ratio, with adjustments for sex, diabetes, and hypertension.
#'
#' @param df A data.frame containing the required columns.
#' @param sex_col Column name with sex labels, character. Example: "sex".
#' @param diabetes_col Column name with diabetes indicator, 0 or 1, or logical. Example: "diabetes".
#' @param hypertension_col Column name with hypertension indicator, 0 or 1, or logical. Example: "hypertension".
#' @param upcr_col Column name with UPCR values. Units should match those used to derive the model
#'   in Sumida et al. (mg/g). If your data use different units, convert before calling.
#' @param female_str String that denotes female in \code{sex_col}. Default is "female".
#'
#' @return A numeric vector of UACR values, length \code{nrow(df)}. Non valid rows return \code{NA_real_}.
#'
#' @details
#' The function applies a piecewise log transformation of UPCR with cut points at 50 and 500,
#' and adds covariate adjustments for sex, diabetes, and hypertension, then exponentiates to return UACR.
#' Valid rows require non missing diabetes and hypertension indicators. Sex is mapped to an indicator
#' using \code{female_str}.
#'
#' @references
#' Sumida, K., Nadkarni, G. N., Grams, M. E., Sang, Y., Ballew, S. H., Coresh, J., Matsushita, K., Surapaneni, A.,
#' Brunskill, N., Chadban, S. J., Chang, A. R., Cirillo, M., Daratha, K. B., Gansevoort, R. T., Garg, A. X.,
#' Iacoviello, L., Kayama, T., Konta, T., Kovesdy, C. P., Lash, J., Lee, B. J., Major, R. W., Metzger, M., Miura, K.,
#' Naimark, D. M. J., Nelson, R. G., Sawhney, S., Stempniewicz, N., Tang, M., Townsend, R. R., Traynor, J. P.,
#' Valdivielso, J. M., Wetzels, J., Polkinghorne, K. R., and Heerspink, H. J. L. (2020).
#' Conversion of urine protein–creatinine ratio or urine dipstick protein to urine albumin–creatinine ratio
#' for use in chronic kidney disease screening and prognosis. \emph{Annals of Internal Medicine}, 173(6), 426–435.
#' \doi{10.7326/M20-0529}
#'
#' @examples
#' \dontrun{
#' df$uacr <- upcr_uacr(df, "sex", "diabetes", "hypertension", "upcr")
#' }
#'
#' @export
#' 
upcr_uacr <- function(df, sex_col, diabetes_col, hypertension_col, upcr_col, female_str="female") {
  upcr <- as.numeric(df[[upcr_col]])
  female <- as.integer(df[[sex_col]] == female_str)
  diabetic_mask <- !is.na(df[[diabetes_col]])
  hypertensive_mask <- !is.na(df[[hypertension_col]])
  valid <- diabetic_mask & hypertensive_mask
  uacr <- rep(NA_real_, nrow(df))
  idx <- which(valid)
  uacr[idx] <- exp(
    5.2659 +
    0.2934 * log(pmin(upcr[idx] / 50, 1)) +
    1.5643 * log(pmax(pmin(upcr[idx] / 500, 1), 0.1)) +
    1.1109 * log(pmax(upcr[idx] / 500, 1)) -
    0.0773 * female[idx] +
    0.0797 * as.integer(df[[diabetes_col]][idx]) +
    0.1265 * as.integer(df[[hypertension_col]][idx])
  )
  uacr
}
