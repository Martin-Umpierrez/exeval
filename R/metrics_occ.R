#' Compute pharmacokinetic evaluation metrics by occasion
#'
#' Generic function to compute prediction error metrics by occasion (OCC).
#' The method dispatched depends on the class of \code{x}.
#'
#' @param simulations An object containing individual simulations.
#' @param assessment ..
#' @param tool ..
#' @param ... Additional arguments passed to specific methods.
#'
#' @return An object of class \code{EvalMetricsPPK}.
#'
#' @export
metrics_occ <- function(simulations, assessment, tool, ...) {
  UseMethod("metrics_occ")
}
