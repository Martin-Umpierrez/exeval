#' Plot pharmacokinetic evaluation metrics
#' @param x Object containing evaluation metrics
#' @param ... Additional arguments
#' @export
metrics_plot <- function(x, ...) {
  UseMethod("metrics_plot")
}
