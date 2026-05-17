#' Summarize external PK evaluation results
#'
#' Generates a structured summary of an \code{EvalPPK} object, including
#' metadata, global performance metrics, fit quality distribution,
#' and poorly fitted IDs.
#'
#' @param object An object of class \code{EvalPPK}.
#' @param occ Optional numeric occasion to summarize.
#' If \code{NULL} (default), all occasions are included.
#' @param by_occ Logical. If \code{TRUE}, returns summaries stratified by OCC.
#' Cannot be used together with \code{occ}.
#' @param poor_threshold Numeric threshold defining poor fit based on
#' absolute IPE. Default is 50.
#' @param top_n Number of poorly fitted IDs to return. Default is 10.
#' @param ... Additional arguments (not used).
#'
#' @return An object of class \code{summary.EvalPPK}.
#' @export

summary.EvalPPK <- function(object,
                            occ = NULL,
                            by_occ = TRUE,
                            poor_threshold = 50,
                            top_n = 10,
                            ...) {

  # --------------------------------
  # Input validation
  # --------------------------------
  if (!inherits(object, "EvalPPK")) {
    stop("'object' must be an EvalPPK object.")
  }

  if (!is.null(occ) && by_occ) {
    stop("'occ' and 'by_occ' cannot be used together.")
  }

  # --------------------------------
  # Extract data
  # --------------------------------
  df <- object$metrics$metrics
  metadata <- attr(object, "attributes")

  if (!is.null(occ)) {
    df <- df %>%
      dplyr::filter(OCC == occ)
  }

  if (nrow(df) == 0) {
    stop("No observations found for selected filters.")
  }

  # --------------------------------
  # Global metrics
  # --------------------------------
  if (by_occ) {

    global_metrics <- df %>%
      dplyr::group_by(OCC) %>%
      dplyr::summarise(
        rBIAS = mean(IPE, na.rm = TRUE),
        MAIPE = mean(APE, na.rm = TRUE),
        rRMSE = sqrt(mean(RMSE, na.rm = TRUE)) * 100,
        IF20 = mean(abs(IPE) <= 20, na.rm = TRUE) * 100,
        IF30 = mean(abs(IPE) <= 30, na.rm = TRUE) * 100,
        .groups = "drop"
      )

  } else {

    global_metrics <- df %>%
      dplyr::summarise(
        rBIAS = mean(IPE, na.rm = TRUE),
        MAIPE = mean(APE, na.rm = TRUE),
        rRMSE = sqrt(mean(RMSE, na.rm = TRUE)) * 100,
        IF20 = mean(abs(IPE) <= 20, na.rm = TRUE) * 100,
        IF30 = mean(abs(IPE) <= 30, na.rm = TRUE) * 100
      )
  }

  # --------------------------------
  # Fit classification
  # --------------------------------
  df_fit <- df %>%
    dplyr::mutate(
      Fit_Class = dplyr::case_when(
        abs(IPE) <= 15 ~ "Excellent",
        abs(IPE) <= 30 ~ "Acceptable",
        abs(IPE) <= 50 ~ "Poor",
        TRUE ~ "Very Poor"
      )
    )

  if (by_occ) {

    fit_distribution <- df_fit %>%
      dplyr::count(OCC, Fit_Class) %>%
      dplyr::group_by(OCC) %>%
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 1)
      ) %>%
      dplyr::ungroup()

  } else {

    fit_distribution <- df_fit %>%
      dplyr::count(Fit_Class) %>%
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 1)
      )
  }

  # --------------------------------
  # Poor fit IDs
  # --------------------------------
  if (by_occ) {

    poor_fit_ids <- df %>%
      dplyr::filter(abs(IPE) >= poor_threshold) %>%
      dplyr::group_by(OCC, ID) %>%
      dplyr::summarise(
        n_poor = dplyr::n(),
        mean_abs_IPE = round(mean(abs(IPE), na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(
        OCC,
        dplyr::desc(n_poor),
        dplyr::desc(mean_abs_IPE)
      ) %>%
      dplyr::group_by(OCC) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::ungroup()

  } else {

    poor_fit_ids <- df %>%
      dplyr::filter(abs(IPE) >= poor_threshold) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        n_poor = dplyr::n(),
        OCCs = paste(sort(unique(OCC)), collapse = ", "),
        mean_abs_IPE = round(mean(abs(IPE), na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(
        dplyr::desc(n_poor),
        dplyr::desc(mean_abs_IPE)
      ) %>%
      dplyr::slice_head(n = top_n)
  }

  # --------------------------------
  # Output
  # --------------------------------
  out <- list(
    metadata = metadata,
    global_metrics = global_metrics,
    fit_distribution = fit_distribution,
    poor_fit_ids = poor_fit_ids,
    occ_filter = occ,
    by_occ = by_occ,
    poor_threshold = poor_threshold
  )

  class(out) <- "summary.EvalPPK"

  return(out)
}

