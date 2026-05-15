#' Screen individual fit quality
#'
#' Classifies individual prediction performance based on absolute individual
#' prediction error (APE) and filters observations according to selected fit
#' quality categories.
#'
#' Fit categories are defined as:
#' \itemize{
#'   \item Excellent: absolute IPE <= 15%
#'   \item Acceptable: absolute IPE > 15% and <= 30%
#'   \item Poor: absolute IPE > 30% and <= 50%
#'   \item Very Poor: absolute IPE > 50%
#' }
#'
#' This function operates at the observation level (ID/OCC/TIME), making it
#' useful for identifying specific poorly or well fitted samples.
#'
#' @param metrics Output from [metrics_occ()] or a compatible data frame
#'   containing at least `ID`, `OCC`, `TIME`, and `IPE`.
#' @param occ Optional numeric occasion to filter.
#'   If `NULL` (default), all occasions are included.
#' @param fit_classes Character vector indicating which fit categories to retain.
#'   Default includes all categories.
#' @param ids_only Logical. If `TRUE`, returns only unique subject IDs matching
#'   the selected criteria.
#'
#' @return A data frame containing filtered observations with fit classification,
#'   or a vector of IDs if `ids_only = TRUE`.
#'
#' @examples
#' \dontrun{
#' # Return all classified observations
#' screen_fit(metrics)
#'
#' # Return only poorly fitted observations
#' screen_fit(
#'   metrics,
#'   fit_classes = c("Poor", "Very Poor")
#' )
#'
#' # Return only very poorly fitted observations from OCC 1
#' screen_fit(
#'   metrics,
#'   occ = 1,
#'   fit_classes = "Very Poor"
#' )
#'
#' # Return only IDs with excellent fit
#' screen_fit(
#'   metrics,
#'   fit_classes = "Excellent",
#'   ids_only = TRUE
#' )
#' }
#'
#' @export

screen_fit <- function(metrics,
                       occ = NULL,
                       fit_classes = c(
                         "Excellent",
                         "Acceptable",
                         "Poor",
                         "Very Poor"
                       ),
                       ids_only = FALSE) {

  if (inherits(metrics, "EvalMetricsPPK")) {
    df <- metrics$metrics
  } else if (is.data.frame(metrics)) {
    df <- metrics
  } else {
    stop(
      "'metrics' must be an EvalMetricsPPK object or a data.frame containing IPE."
    )
  }

  required_cols <- c("ID", "OCC", "TIME", "IPE")

  if (!all(required_cols %in% names(df))) {
    stop(
      "Input data must contain: ",
      paste(required_cols, collapse = ", ")
    )
  }

  valid_classes <- c(
    "Excellent",
    "Acceptable",
    "Poor",
    "Very Poor"
  )

  if (!all(fit_classes %in% valid_classes)) {
    stop(
      "'fit_classes' must contain only: ",
      paste(valid_classes, collapse = ", ")
    )
  }

  if (!is.null(occ)) {
    df <- df %>%
      dplyr::filter(OCC == occ)
  }

  if (nrow(df) == 0) {
    stop("No observations found for selected filters.")
  }

  df <- df %>%
    dplyr::mutate(
      Abs_IPE = abs(IPE),
      Fit_Class = dplyr::case_when(
        Abs_IPE <= 15 ~ "Excellent",
        Abs_IPE <= 30 ~ "Acceptable",
        Abs_IPE <= 50 ~ "Poor",
        Abs_IPE > 50 ~ "Very Poor"
      )
    ) %>%
    dplyr::filter(Fit_Class %in% fit_classes) %>%
    dplyr::arrange(ID, OCC, TIME)

  if (nrow(df) == 0) {
    message("No matching observations found.")
    return(dplyr::tibble())
  }

  if (ids_only) {
    return(unique(df$ID))
  }

  return(df)
}
