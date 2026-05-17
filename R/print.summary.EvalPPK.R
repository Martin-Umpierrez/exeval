#' @export

print.summary.EvalPPK <- function(x, ...) {

  cat("\n")
  cat("External PK Evaluation Summary\n")
  cat(rep("=", 35), "\n", sep = "")

  # --------------------------------
  # Metadata
  # --------------------------------
  if (!is.null(x$metadata)) {
    cat("\nMetadata\n")
    print(x$metadata, row.names = FALSE)
  }

  # --------------------------------
  # Applied filters
  # --------------------------------
  cat("\nSummary settings\n")

  if (is.null(x$occ_filter)) {
    cat("OCC filter      : All occasions\n")
  } else {
    cat("OCC filter      :", x$occ_filter, "\n")
  }

  cat("By OCC          :", x$by_occ, "\n")
  cat("Poor threshold  :", x$poor_threshold, "%\n")

  # --------------------------------
  # Global metrics
  # --------------------------------
  cat("\nGlobal Metrics\n")
  print(x$global_metrics, row.names = FALSE)

  # --------------------------------
  # Fit distribution
  # --------------------------------
  cat("\nFit Distribution\n")
  print(x$fit_distribution, row.names = FALSE)

  # --------------------------------
  # Poor fit IDs
  # --------------------------------
  if (nrow(x$poor_fit_ids) > 0) {
    cat("\nPoorly Fitted IDs\n")
    print(x$poor_fit_ids, row.names = FALSE)
  } else {
    cat("\nNo poorly fitted IDs found.\n")
  }

  invisible(x)
}
