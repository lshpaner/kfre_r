#' Apply rounding with optional precision
#'
#' Rounds a numeric vector to a given number of decimal places,
#' or returns it unchanged if `n` is NULL.
#'
#' @param x Numeric vector to be rounded.
#' @param n Integer number of digits to round to, or NULL to skip rounding.
#'
#' @return Numeric vector with rounding applied when `n` is not NULL.
#' @keywords internal
#'
#' @examples
#' apply_precision(c(1.2345, 6.789), 2)
#' apply_precision(c(1.2345, 6.789), NULL)
apply_precision <- function(x, n = NULL) {
  if (is.null(n)) {
    return(x)
  }
  round(x, digits = n)
}
