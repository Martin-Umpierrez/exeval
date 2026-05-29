#' Update MAP estimation objects with posterior individual parameters
#'
#' Converts MAP estimation results obtained with [run_MAP_estimations()]
#' into individualized posterior models using \pkg{mapbayr}. Depending on the
#' selected evaluation strategy, posterior information is propagated across
#' occasions to support posterior predictive simulations.
#' 
#' This function applies [mapbayr::use_posterior()] to each MAP estimation
#' object and returns a list of updated individual models that can be used
#' for posterior predictive simulations.
#' 
#' The evaluation strategy must match the one originally used in
#' [run_MAP_estimations()].
#' 
#' @param map_results Named list returned by [run_MAP_estimations()].
#' Must contain at least the elements \code{map_estimations} and
#' \code{eval_type}.
#'
#' @param evaluation_type Character string specifying the evaluation strategy.
#' Must match the strategy used when generating \code{map_results}.
#' Available options are:
#' \itemize{
#'   \item \code{"sequential_updating"}
#'   \item \code{"stepwise_updating"}
#'   \item \code{"sequential_reference_updating"}
#'   \item \code{"backward_reference_updating"}
#' }
#'   
#'
#' @return A named list containing:
#' \describe{
#'   \item{ind_model}{List of posterior individualized model objects created
#'   using \code{mapbayr::use_posterior()}.}
#'
#'   \item{eval_type}{Character string indicating the evaluation strategy
#'   used.}
#' }
#'
#' @details
#' This function applies \code{mapbayr::use_posterior()} to each MAP
#' estimation object contained in \code{map_results}, generating individualized
#' posterior models for subsequent simulation.
#'
#' Posterior information is propagated across occasions according to the
#' selected \code{evaluation_type}, which must match the strategy originally
#' used in [run_MAP_estimations()].
#'
#' Posterior model objects are dynamically named following the pattern
#' \code{a.posteriori_occX_Y}, where \code{X} and \code{Y} indicate the
#' occasions linked by the posterior update.
#'
#' The resulting objects are intended for use with [run_pk_simulations()].
#'
#' @examples
#' \donttest{
#' data("exeval_models", package = "exeval")
#' data("tacrolimus_pk1_kidney", package = "exeval")
#'
#' dd <- tacrolimus_pk1_kidney |> subset(ID < 6)
#'
#' fit <- run_MAP_estimations(
#'   model = exeval_models$Model_code[[2]],
#'   model_name = "TAC_Zuo2013",
#'   data = dd,
#'   evaluation_type = "sequential_updating"
#' )
#'
#' post <- update_map_models(
#'   map_results = fit,
#'   evaluation_type = "sequential_updating"
#' )
#' }
#'
#' @importFrom mapbayr use_posterior
#' @export

update_map_models <-
function(map_results, evaluation_type = c("sequential_updating",
                                                "stepwise_updating",
                                                "sequential_reference_updating",
                                                "backward_reference_updating")) {

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
  if(evaluation_type=="sequential_updating") {
    for (i in 1:(num_estimations)) {
      # current estimation
      previous_numbers <- paste0(1:i, collapse = "_")

      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_0_",previous_numbers)]]

      # check current_map_estimation not null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation |> mapbayr::use_posterior()

        # save result with dynamic name like a.posteriori_occ1_2, a.posteriori_occ2_3, etc.
        posterior_name <- paste0("a.posteriori_occ", i, "_", i + 1)
        posterior_estimations[[posterior_name]] <- posterior_result
      } else {
        message(paste0("Estimation for OCC ", i, " is null, skiping to next."))
      }
    }
  }

  else if (evaluation_type=="stepwise_updating") {
    for (i in 1:(num_estimations)) {

      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_", i)]]

      # check current_map_estimation is null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation |> mapbayr::use_posterior()

        # save result with dynamic name like a.posteriori_occ1_2, a.posteriori_occ2_3, etc.
        posterior_name <- paste0("a.posteriori_occ", i, "_", i + 1)
        posterior_estimations[[posterior_name]] <- posterior_result
      } else {
        message(paste0("Estimation for OCC ", i, " is null, skiping to next."))
      }
    }
  }

  else if (evaluation_type=="sequential_reference_updating") {
    for (i in 1:(num_estimations)) {
      previous_numbers <- paste0(1:i, collapse = "_")
      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_0_",previous_numbers)]]

      # check current_map_estimation is null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation |> mapbayr::use_posterior()

        # save result with dynamic name like a.posteriori_occ1_2, a.posteriori_occ2_3, etc.
        posterior_name <- paste0("a.posteriori_occ", i, "_", i + 1)
        posterior_estimations[[posterior_name]] <- posterior_result
      } else {
        message(paste0("Estimation for OCC ", i, " is null, skiping to next."))
      }
    }
  }

  else if (evaluation_type=="backward_reference_updating") {
    for (i in 1:(num_estimations)) {

      previous_numbers <- paste0((occ_ref-1):i, collapse = "_")
      current_map_estimation <- map_estimations[[paste0("map.estimation.occ_",previous_numbers)]]

      # check current_map_estimation is null
      if (!is.null(current_map_estimation)) {
        # use posterior for next OCC
        posterior_result <- current_map_estimation |> mapbayr::use_posterior()

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
