#' seect_best_models
#'
#' Select Top Ranked Models
#'
#' #' This function selects the best models from a dataframe of metrics by models based on a specified ranking metric
#' (e.g., minimum, maximum, or absolute value of a given metric). It can filter the best models by the
#' `OCC` value if specified, or return the top models for all `OCC` values.
#'
#' @param data A dataframe obtained from `combine_metrics()`, containing the combined metrics data.
#' @param metric A character string specifying the metric to use for ranking.
#' @param top_n An integer specifying the number of top models to select per OCC.
#' @param occ_eval A numeric or character value specifying the `OCC` to filter the models by. Default is `NULL`, which means no filtering by `OCC`.
#' @param rank_criteria A character string specifying how to rank the models:
#'   - `'min'`: Select models with the lowest metric values (e.g., MAIPE).
#'   - `'max'`: Select models whose metric values are higher (e.g., IF30).
#'   - `'abs'` : Selects models with the smallest absolute metric values (eg rBIAS)
#'
#' @return A data frame with the top-ranked models for each OCC.
#' @export
#'
#' @examples
#' #' set.seed(123)  # Para reproducibilidad
#' generate_fake_metrics <- function(n_occasions = 3) {
#' data.frame(
#' OCC = rep(1:n_occasions),  # Simula varias ocasiones
#' rBIAS = rnorm(n_occasions, mean = 0, sd = 10),
#' rBIAS_lower = rnorm(n_occasions, mean = -5, sd = 5),
#' rBIAS_upper = rnorm(n_occasions, mean = 5, sd = 5),
#' MAIPE = runif(n_occasions, min = 10, max = 50),
#' IF20 = runif(n_occasions, min = 20, max = 80),
#' IF30 = runif(n_occasions, min = 30, max = 90)
#' )
#' }
#' # Save Results of metrics
#' simulation1 <- list(metrics_means = generate_fake_metrics())
#' simulation2 <- list(metrics_means = generate_fake_metrics())
#' # List of models
#' models_list <- list(
#' list(model_name = "Test_Model1", metrics_list = simulation1),
#' list(model_name = "Test_Model2", metrics_list = simulation2)
#' )
#'combined_results <- combine_metrics(models_list)
#' top_models <- select_best_models(combined_results, metric='MAIPE', occ_eval=2)
select_best_models <-
  function(data, metric, top_n = 3, occ_eval=NULL , rank_criteria = 'min') {
    if (is.null(occ_eval)){
      ranked_models <- data %>%
        dplyr::group_by(OCC) %>%
        dplyr::arrange(
          if (rank_criteria == 'min') !!sym(metric)
          else if (rank_criteria == 'max') desc(!!sym(metric))
          else abs(!!sym(metric))
        ) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::ungroup()
    }
    else {
      ranked_models<- data %>%
        dplyr::group_by(OCC) %>%
        dplyr::arrange(
          if (rank_criteria == 'min') !!dplyr::sym(metric)
          else if (rank_criteria == 'max') dplyr::desc(!!sym(metric))
          else abs(!!dplyr::sym(metric))
        ) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::filter(OCC==occ_eval) %>%
        dplyr::ungroup()

    }
    return(ranked_models)
  }
