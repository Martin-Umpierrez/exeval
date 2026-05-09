
#' Update MAP estimations across occasions for each IDs
#'
#' The `actualize_model` function updates MAP (Maximum A Posteriori) estimations
#' across occasions based on a selected evaluation type. It processes posterior estimations
#' using the specified strategy and returns the updated model for each ID on every single OCC
#'
#' @param actualization_map A list containing the MAP estimations obtained in `run_MAP_estimations`
#'   Must include an element named `map_estimations` which stores the posterior estimations
#'   for each occasion.
#' @param evaluation_type A character vector specifying the evaluation type to use for updating.
#'   Options include:
#'   - `"Progressive"`: Use posterior results progressively across all previous occasions.
#'   - `"Most_Recent_Progressive"`: Use only the most recent posterior for updating.
#'   - `"Cronologic_Ref"`: Use a chronological reference for posterior updates.
#'   - `"Most_Recent_Ref"`: Use the most recent chronological reference for updates.
#'   Defaults to `"Progressive"`.
#'
#' @return A list with two elements:
#'   - `ind_model`: A list containing posterior estimations (`a.posteriori`) for each occasion.
#'   - `eval_type`: The evaluation type used for the process.
#'
#' @details
#' This function evaluates posterior estimations iteratively based on the specified
#' `evaluation_type`. It ensures compatibility with the evaluation type used during
#' the creation of the input `actualization_map`.
#'
#' The function dynamically names the posterior estimation results, following the pattern
#' `a.posteriori_occX_Y`, where `X` and `Y` represent the occasions used in the estimation.
#'
#' @examples
#' \dontrun{
#' # Example input data
#' actualization_map <- list(
#'   eval_type = "Progressive",
#'   map_estimations = list(
#'     "map.estimation.occ_0_1" = NULL,
#'     "map.estimation.occ_0_1_2" = list() # Replace with real MAP estimation object
#'   )
#' )
#'
#' # Run the function
#' result <- actualize_model(actualization_map, evaluation_type = "Progressive")
#' print(result)
#'}
#' @importFrom magrittr %>%
#' @importFrom mapbayr use_posterior
#' @export

actualize_model <-
function(actualization_map,
                            evaluation_type = c("Progressive",
                                                "Most_Recent_Progressive",
                                                "Cronologic_Ref",
                                                "Most_Recent_Ref")) {

  if ( actualization_map$eval_type != evaluation_type) {
    stop(" Select the same evaluation type used in run_map")
  }

  # Evaluationtype :
  evaluation_type <- match.arg(evaluation_type)

  # list for save estimations
  posterior_estimations <- list()

  if(!"map_estimations" %in% names(actualization_map)) {
    stop("There is no element `map_estimation` in the entry ")
  }

  map_estimations <- actualization_map$map_estimations
  num_estimations <- length(map_estimations)

  # loop over estimations
  if(evaluation_type=="Progressive") {
    for (i in 1:(num_estimations)) {
      # current estimation
      previous_numbers <- paste0(1:i, collapse = "_")

      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_0_",previous_numbers)]]

      # check current_map_estimation not null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation %>% mapbayr::use_posterior()

        # save result with dynamic name like a.posteriori_occ1_2, a.posteriori_occ2_3, etc.
        posterior_name <- paste0("a.posteriori_occ", i, "_", i + 1)
        posterior_estimations[[posterior_name]] <- posterior_result
      } else {
        message(paste0("Estimation for OCC ", i, " is null, skiping to next."))
      }
    }
  }

  else if (evaluation_type=="Most_Recent_Progressive") {
    for (i in 1:(num_estimations)) {

      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_", i)]]

      # check current_map_estimation is null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation %>% mapbayr::use_posterior()

        # save result with dynamic name like a.posteriori_occ1_2, a.posteriori_occ2_3, etc.
        posterior_name <- paste0("a.posteriori_occ", i, "_", i + 1)
        posterior_estimations[[posterior_name]] <- posterior_result
      } else {
        message(paste0("Estimation for OCC ", i, " is null, skiping to next."))
      }
    }
  }

  else if (evaluation_type=="Cronologic_Ref") {
    for (i in 1:(num_estimations)) {
      previous_numbers <- paste0(1:i, collapse = "_")
      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_0_",previous_numbers)]]

      # check current_map_estimation is null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation %>% mapbayr::use_posterior()

        # save result with dynamic name like a.posteriori_occ1_2, a.posteriori_occ2_3, etc.
        posterior_name <- paste0("a.posteriori_occ", i, "_", i + 1)
        posterior_estimations[[posterior_name]] <- posterior_result
      } else {
        message(paste0("Estimation for OCC ", i, " is null, skiping to next."))
      }
    }
  }

  else if (evaluation_type=="Most_Recent_Ref") {
    for (i in 1:(num_estimations)) {

      previous_numbers <- paste0((occ_ref-1):i, collapse = "_")
      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_",previous_numbers)]]

      # check current_map_estimation is null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation %>% mapbayr::use_posterior()

        # save result with dynamic name like a.posteriori_occ1_2, a.posteriori_occ2_3, etc.
        posterior_name <- paste0("a.posteriori_occ", i, "_", i + 1)
        posterior_estimations[[posterior_name]] <- posterior_result
      } else {
        message(paste0("Estimation for OCC ", i, " is null, skiping to next."))
      }
    }
  }

  # results
  return(list(ind_model=posterior_estimations,
              eval_type=evaluation_type))
}
