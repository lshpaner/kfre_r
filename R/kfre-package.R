#' kfre: KFRE Risk Prediction Tools (R)
#'
#' Implements the Kidney Failure Risk Equation (KFRE; Tangri et al.) to compute
#' 2- and 5-year risk (4-, 6-, and 8-variable models), add risk columns to data
#' frames, classify CKD stages/ESRD outcomes, and evaluate & plot performance.
#'
#' @section Key functions:
#' - [risk_pred_core()]
#' - [add_kfre_risk_col()]
#' - [eval_kfre_metrics()], [plot_kfre_metrics()]
#' - [class_esrd_outcome()], [class_ckd_stages()]
#'
#' @author Leonid Shpaner \email{lshpaner@ucla.edu}
#'   (\href{https://orcid.org/0009-0007-5311-8095}{ORCID}) \cr
#'   \url{https://github.com/lshpaner/kfre_r} \cr
#'   \url{https://www.leonshpaner.com/}
#'
#' @seealso \url{https://github.com/lshpaner/kfre_r}
#' @docType package
#' @name kfre
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
