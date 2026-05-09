#' @export
print.EvalMetricsPPK <- function(x, ...) {

  cat("Population Pharmacokinetic Evaluation Metrics\n")
  cat("============================================\n")
  cat("Evaluation type :", attr(x, "eval_type"), "\n")
  cat("Assessment      :", attr(x, "assessment"), "\n\n")

  cat("Mean metrics by OCC\n")
  print(x$metrics_means)

  invisible(x)
}
