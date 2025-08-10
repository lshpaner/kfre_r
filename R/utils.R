#' Apply precision (round helper)
#'
#' Simple wrapper around base \code{round()} used by tests and examples.
#'
#' @param x Numeric vector.
#' @param n Integer number of digits to keep. If NULL, return x unchanged.
#' @return Numeric vector rounded to \code{n}.
#' @export
apply_precision <- function(x, n = NULL) {
  if (is.null(n)) x else round(x, digits = n)
}
