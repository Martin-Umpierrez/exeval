#' combine_metrics
#'
#' Combine Model Metrics for a given list of models previously evaluated Metrics for Individual Predictions by Occasion
#'
#' This function takes a list of models, each containing a model name and a list of metric results,
#' and merges them into a single data frame.
#'
#' @param models A list of lists, where each inner list contains:
#'   - `model_name`: A string representing the model's name. It will then be used
#'   - `metrics_list`: A list containing the `metrics_means` data frame from [metrics_occ()].
#' @return A data frame containing the combined metrics for all models, with an additional `Model` column.
#'
#' @export
#' @examples
#'
#' set.seed(123)
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
#'print(combined_results)

combine_metrics <-
function(models) {
  combined_data <- NULL

  # Extract all the evaluation type in the models to be compared
  eval_types <- sapply(models, function(model) model$metrics_list$eval_type)

  # Check que todos sean iguales
  if (length(unique(eval_types)) > 1) {
    stop("Error: Models must have the same 'eval_type' to be combined.")
  }


  for (entry in models) {
    model_name <- entry$model_name
    metrics_list <- entry$metrics_list

    # add model name
    metrics_data <- metrics_list$metrics_means %>% mutate(Model = model_name)

    # combined with data
    combined_data <- if (!is.null(combined_data)) {
      bind_rows(combined_data, metrics_data)
    } else {
      metrics_data
    }
  }

  return(combined_data)
}
