#' @method metrics_plot EvalPPK
#' @export
metrics_plot.EvalPPK <- function(x, type, ...) {
  metrics_plot(x$metrics, type = type, ...)
}
