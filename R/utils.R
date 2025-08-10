apply_precision <- function(x, n = NULL) {
  if (is.null(n)) {
    return(x)
  }
  round(x, digits = n)
}
