#' Screen fit quality at the observation level
#'
#' Classifies predictive performance based on absolute individual prediction
#' error (\code{IPE}) and filters observations according to selected fit
#' quality categories.
#'
#' Fit categories are defined as:
#' \itemize{
#'   \item \code{Excellent}: absolute IPE <= 15\%
#'   \item \code{Acceptable}: absolute IPE > 15\% and <= 30\%
#'   \item \code{Poor}: absolute IPE > 30\% and <= 50\%
#'   \item \code{Very Poor}: absolute IPE > 50\%
#' }
#'
#' This function operates at the observation level (\code{ID/OCC/TIME}),
#' making it useful for identifying specific poorly or well-fitted samples
#' rather than aggregated summaries.
#'
#' Supported inputs include:
#' \itemize{
#'   \item Objects of class \code{EvalPPK}, typically returned by [exeval_ppk()]
#'   \item Compatible data frames containing observation-level metrics
#' }
#'
#' @param x Input object to screen. Supported inputs include:
#' \itemize{
#'   \item an object of class \code{EvalPPK}, typically returned by
#'   [exeval_ppk()]
#'
#'   \item a data frame containing at least \code{ID}, \code{OCC},
#'   \code{TIME}, and \code{IPE}
#' }
#' 
#' @param occ Optional numeric occasion (\code{OCC}) used to filter
#' observations. If \code{NULL} (default), all occasions are included.
#' 
#' @param fit_classes Character vector specifying which fit quality
#' categories to retain. Default includes all categories.
#' 
#' @param ids_only Logical. If \code{TRUE}, returns unique subject
#' identifiers matching the selected criteria instead of observation-level
#' records.
#'
#' @return A filtered data frame containing fit classification results,
#' or a vector of IDs if \code{ids_only = TRUE}.
#' 
#' @seealso [exeval_ppk()], [summary.EvalPPK()]
#' 
#' @examples
#' \dontrun{
#' # Screen full evaluation object
#' res <- exeval_ppk(...)
#' screen_fit(res)
#'
#'
#' # Return only poorly fitted observations
#' screen_fit(
#'   res,
#'   fit_classes = c("Poor", "Very Poor")
#' )
#'
#' # Return only very poorly fitted observations from OCC 1
#' screen_fit(
#'   res,
#'   occ = 1,
#'   fit_classes = "Very Poor"
#' )
#'
#' # Return only IDs with excellent fit
#' screen_fit(
#'   res,
#'   fit_classes = "Excellent",
#'   ids_only = TRUE
#' )
#' }
#'
#' @export

screen_fit <- function(x,
                       occ = NULL,
                       fit_classes = c(
                         "Excellent",
                         "Acceptable",
                         "Poor",
                         "Very Poor"
                       ),
                       ids_only = FALSE) {

  # --------------------------------
  # Input dispatch
  # --------------------------------
  if (inherits(x, "EvalPPK")) {
    
    df <- x$metrics$metrics
    
  } else if (is.data.frame(x)) {
    
    df <- x
    
  } else {
    stop(
      "'x' must be an EvalPPK object or a data.frame containing IPE."
    )
  }

  # --------------------------------
  # Required columns
  # --------------------------------
  required_cols <- c("ID", "OCC", "TIME", "IPE")

  if (!all(required_cols %in% names(df))) {
    stop(
      "Input data must contain: ",
      paste(required_cols, collapse = ", ")
    )
  }
  # --------------------------------
  # Validate fit classes
  # --------------------------------
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
  # --------------------------------
  # OCC filter
  # --------------------------------
  if (!is.null(occ)) {
    df <- df |>
      dplyr::filter(OCC == occ)
  }

  if (nrow(df) == 0) {
    stop("No observations found for selected filters.")
  }
  # --------------------------------
  # Fit classification
  # --------------------------------
  df <- df |>
    dplyr::mutate(
      Abs_IPE = abs(IPE),
      Fit_Class = dplyr::case_when(
        Abs_IPE <= 15 ~ "Excellent",
        Abs_IPE <= 30 ~ "Acceptable",
        Abs_IPE <= 50 ~ "Poor",
        Abs_IPE > 50 ~ "Very Poor"
      )
    ) |>
    dplyr::filter(Fit_Class %in% fit_classes) |>
    dplyr::arrange(ID, OCC, TIME)
  # --------------------------------
  # Empty result
  # --------------------------------
  if (nrow(df) == 0) {
    message("No matching observations found.")
    return(dplyr::tibble())
  }
  # --------------------------------
  # IDs only
  # --------------------------------
  if (ids_only) {
    return(unique(df$ID))
  }

  return(df)
}
