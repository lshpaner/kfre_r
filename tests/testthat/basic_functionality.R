testthat::skip_on_cran()
testthat::skip_if_not_installed("devtools")
testthat::skip_on_cran()
# Load and test package
if (interactive() && requireNamespace('devtools', quietly = TRUE)) try(devtools::load_all(quiet = TRUE), silent = TRUE)
invisible(NULL)

citation("kfre")
