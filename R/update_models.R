
#' Update MAP estimations across occasions for each IDs
#'
#' The `update_map_models` function updates population pharmacokinetic models using MAP posterior estimations
#' across occasions based on a selected evaluation type. Depending on the selected evaluation
#' strategy, posterior information is propagated across occasions to generate
#' individual models ready for simulation. The function returns individual models updated with posterior parameter estimates for each ID at each occasion.
#'
#' @param map_results A list object containing the MAP estimations obtained in `run_MAP_estimations`
#'   Must include an element named `map_estimations` which stores the posterior estimations
#'   for each occasion.
#'
#' @param evaluation_type A character vector specifying the evaluation type to use for updating.
#'   Options include:
#'   - `"Progressive"`: Use posterior results progressively across all previous occasions.
#'   - `"Most_Recent_Progressive"`: Use only the most recent posterior for updating.
#'   - `"Cronologic_Ref"`: Use a chronological reference for posterior updates.
#'   - `"Most_Recent_Ref"`: Use the most recent chronological reference for updates.
#'   Defaults to `"Progressive"`.
#'
#' @return A list containing:
#' \describe{
#'   \item{ind_model}{A list of updated individual posterior models for each
#'   occasion and ID, ready for simulation.}
#'   \item{eval_type}{The evaluation strategy used.}
#' }
#'
#'
#' @details
#' This function evaluates posterior estimations iteratively based on the specified
#' `evaluation_type`. It ensures compatibility with the evaluation type used during
#' the creation of the input `map_results`.
#'
#' The function dynamically names the posterior estimation results, following the pattern
#' `a.posteriori_occX_Y`, where `X` and `Y` represent the occasions used in the estimation.
#'
#' The output is intended to be used with [run_pk_simulations()]
#'
#' @examples
#' \dontrun{
#' # Example input data
#' map_results <- list(
#'   eval_type = "Progressive",
#'   map_estimations = list(
#'     "map.estimation.occ_0_1" = NULL,
#'     "map.estimation.occ_0_1_2" = list() # Replace with real MAP estimation object
#'   )
#' )
#'
#' # Run the function
#' result <- update_map_models(map_results, evaluation_type = "Progressive")
#' print(result)
#'}
#' @importFrom magrittr %>%
#' @importFrom mapbayr use_posterior
#' @export

update_map_models <-
function(map_results, evaluation_type = c("Progressive",
                                                "Most_Recent_Progressive",
                                                "Cronologic_Ref",
                                                "Most_Recent_Ref")) {

  if ( map_results$eval_type != evaluation_type) {
    stop(" Select the same evaluation type used in run_map")
  }

  # Evaluationtype :
  evaluation_type <- match.arg(evaluation_type)

  # list for save estimations
  posterior_estimations <- list()

  if(!"map_estimations" %in% names(map_results)) {
    stop("There is no element `map_estimation` in the entry ")
  }

  map_estimations <- map_results$map_estimations
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
