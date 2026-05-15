#' Plot fit quality distribution
#'
#' Visualizes the distribution of model predictive performance based on
#' individual prediction error (IPE).
#'
#' Two visualization modes are available:
#' \itemize{
#'   \item `"fit_class"`: bar plot showing the number of observations within
#'   predefined fit quality categories.
#'   \item `"histogram"`: histogram of individual prediction error values.
#' }
#'
#' Fit quality categories are defined according to absolute IPE:
#' \itemize{
#'   \item Excellent: absolute IPE <= 15%
#'   \item Acceptable: absolute IPE > 15% and <= 30%
#'   \item Poor: absolute IPE > 30% and <= 50%
#'   \item Very Poor: absolute IPE > 50%
#' }
#'
#' @param metrics Output from [metrics_occ()] or a compatible data frame
#'   containing at least `IPE` and `OCC`.
#' @param occ Optional numeric occasion to filter.
#'   If `NULL` (default), all occasions are included.
#' @param type Character string indicating the type of plot to generate:
#'   `"fit_class"` (default) or `"histogram"`.
#' @param signed Logical. Only used when `type = "histogram"`.
#'   If `TRUE`, the histogram is generated using signed IPE values.
#'   If `FALSE` (default), absolute IPE values are used.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' # Fit classification bar plot
#' plot_fit_distribution(metrics)
#'
#' # Fit classification for a specific occasion
#' plot_fit_distribution(
#'   metrics,
#'   occ = 2
#' )
#'
#' # Histogram of absolute IPE
#' plot_fit_distribution(
#'   metrics,
#'   type = "histogram"
#' )
#'
#' # Histogram of signed IPE
#' plot_fit_distribution(
#'   metrics,
#'   type = "histogram",
#'   signed = TRUE
#' )
#' }
#'
#' @export

plot_fit_distribution <- function(metrics,
                                  occ = NULL,
                                  type = c("fit_class", "histogram"),
                                  signed = FALSE) {

  type <- match.arg(type)

  if (inherits(metrics, "EvalMetricsPPK")) {
    df <- metrics$metrics
  } else if (is.data.frame(metrics)) {
    df <- metrics
  } else {
    stop(
      "'metrics' must be an EvalMetricsPPK object or a data.frame containing IPE."
    )
  }

  required_cols <- c("IPE", "OCC")

  if (!all(required_cols %in% names(df))) {
    stop(
      "Input data must contain: ",
      paste(required_cols, collapse = ", ")
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
      Abs_IPE = abs(IPE)
    )

  if (type == "fit_class") {

    df <- df %>%
      dplyr::mutate(
        Fit_Class = dplyr::case_when(
          Abs_IPE <= 15 ~ "Excellent",
          Abs_IPE <= 30 ~ "Acceptable",
          Abs_IPE <= 50 ~ "Poor",
          Abs_IPE > 50 ~ "Very Poor"
        ),
        Fit_Class = factor(
          Fit_Class,
          levels = c("Very Poor", "Poor","Acceptable","Excellent")
        )
      )

    fit_colors <- c(
      "Excellent" = "paleturquoise",
      "Acceptable" = "darkseagreen",
      "Poor" = "wheat",
      "Very Poor" = "lightcoral"
    )

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = Fit_Class, fill=Fit_Class)
    ) +
      ggplot2::geom_bar() +
      ggplot2::scale_fill_manual(values = fit_colors, drop = FALSE) +
      ggplot2::labs(
        title = ifelse(
          is.null(occ),
          "Fit quality distribution",
          paste("Fit quality distribution - OCC", occ)
        ),
        x = "Fit Class",
        y = "Number of Observations"
      ) +
      ggplot2::theme_bw()

  } else {

    if (signed) {
      x_var <- "IPE"
      x_lab <- "Individual Prediction Error (%)"
    } else {
      x_var <- "Abs_IPE"
      x_lab <- "Absolute Individual Prediction Error (%)"
    }

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data[[x_var]])
    ) +
      ggplot2::geom_histogram(
        bins = 30
      ) +
      ggplot2::labs(
        title = ifelse(
          is.null(occ),
          "IPE distribution",
          paste("IPE distribution - OCC", occ)
        ),
        x = x_lab,
        y = "Number of Observations"
      ) +
      ggplot2::theme_bw()
  }

  return(p)
}
