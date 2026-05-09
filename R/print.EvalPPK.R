#' print method for EvalPPK class
#'
#' @param x EvalPPK object
#' @param ... additional arguments (not used)
#'
#' @export
print.EvalPPK <- function(x, ...) {
  info <- attr(x, 'attributes')
  nn <- max(nchar(paste(info$argument, info$value)))

  # Data summary (Number of IDs, Number of Observations, Max OCC, Ref OCC)
  data_info <- info[1:4, ]
  data_info <- data_info[data_info$value != "", ]
  if(nrow(data_info) > 0){
    cat(rep("=", nn + 4), "\n", sep = "")
    cat('Data summary\n')
    print(data_info)
    cat(rep("=", nn + 4), "\n\n", sep = "")
  }
  cat(rep("=", nn + 4), "\n", sep = "")
  cat('Evaluation information')
  cat("\n")
  print(info[5:8, ])
  cat(rep("=", nn + 4), "\n", sep = "")
  cat("\n")
  cat(rep("=", nn + 4), "\n", sep = "")
  cat('Evaluation metrics')
  cat("\n")
  print( x$metrics$metrics_means )
  cat(rep("=", nn + 4), "\n", sep = "")
}
