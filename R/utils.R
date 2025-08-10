#' Utility helper.
#'
#' @param x 
#' @param n 
#' @return 
#' @examples
#' # apply_precision example
#' # apply_precision()
apply_precision <- function(x, n = NULL) {
  if (is.null(n)) {
    return(x)
  }
  round(x, digits = n)
}
