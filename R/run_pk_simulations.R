#' Run PK simulations for external model evaluation
#' 
#' Simulates concentration-time profiles for external model evaluation using
#' population predictions (a priori), individualized posterior predictions
#' (Bayesian forecasting), or both, depending on the selected simulation
#' strategy.
#'
#' @param individual_model Named list returned by [update_map_models()]
#' containing individualized posterior models used for Bayesian forecasting.
#' Required when \code{assessment} includes Bayesian forecasting.
#'
#' @param map_results Named list returned by [run_MAP_estimations()]
#' containing treatment/event datasets, evaluation metadata, and the
#' population model required for simulation.
#'
#' @param assessment Character string specifying the simulation strategy.
#' Available options are:
#' \itemize{
#'   \item \code{"a_priori"}: simulates concentration-time profiles using the
#'   population model only.
#'   \item \code{"Bayesian_forecasting"}: simulates concentration-time profiles
#'   using individualized posterior models.
#'   \item \code{"Complete"}: performs both a priori and Bayesian forecasting
#'   simulations.
#' }
#' @param seed Optional integer used to set the random number generator seed
#' for reproducible a priori simulations. If \code{NULL} (default), the
#' current random number generator state is used.
#' 
#' 
#' @param verbose Logical. If \code{TRUE}, progress messages are printed during
#' execution. If \code{FALSE}, simulation errors are returned as warnings..
#'
#' @return A named list containing:
#' \describe{
#'   \item{simulation_results}{List of simulated concentration-time profiles
#'   for each individual and evaluation occasion.}
#'
#'   \item{ttoocc}{Treatment/event datasets grouped by occasion and used
#'   as simulation inputs.}
#'
#'   \item{eval_type}{Character string indicating the evaluation strategy
#'   inherited from [run_MAP_estimations()].}
#'
#'   \item{events_tto}{Event datasets used for each simulation.}
#'
#'   \item{assessment}{Character string indicating the selected simulation
#'   strategy.}
#' }
#'
#' @details
#' This function performs pharmacokinetic simulations at the observation times
#' available in the external evaluation dataset.
#'
#' Depending on the selected \code{assessment}, simulations are performed using:
#' \itemize{
#'   \item the population model for a priori predictions,
#'   \item individualized posterior models for Bayesian forecasting,
#'   \item or both approaches for complete external evaluation.
#' }
#'
#' This function represents the final simulation step in the external
#' evaluation workflow following [run_MAP_estimations()] and, when posterior
#' predictions are required, [update_map_models()].
#' 
#' Reproducibility of stochastic a priori simulations can be controlled using
#' the \code{seed} argument.
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
#'
#' sim <- run_pk_simulations(
#'   individual_model = post,
#'   map_results = fit,
#'   assessment = "Complete",
#'   seed = 123
#' )
#' }
#'
#' @seealso [run_MAP_estimations()], [update_map_models()]
#' @export
#'
run_pk_simulations <- function(individual_model,
                                map_results,
                                assessment = c("a_priori","Bayesian_forecasting","Complete"),
                                seed = NULL,
                                verbose = FALSE) {

  assessment <- match.arg(assessment)
  evaluation_type <-map_results$eval_type
  # Initialize empty lists to store simulation outputs
  simulation_results <- list()
  event.tto <- list()
  treatment.occ.list <- list()


  # 1. A priori simulations
  if (assessment %in% c("a_priori", "Complete")) {

    population_model <- map_results$pop_model

    if (is.null(population_model)) {
      stop("Population model not found in map_results.")
    }

    tto_apriori <- map_results$apriori_treatments

    treatment_list <- tto_apriori[["tto_1"]]
    treatment.occ.list[["OCC1"]] <- treatment_list
    events_apriori <- tto_apriori[["apriori_occ_1"]]

    event.tto.byocc <- list()

    for (id_name in names(events_apriori)) {
      id_number <- sub(".*ID", "", id_name)

      tryCatch({
        
        if (!is.null(seed)) {
          set.seed(seed)
        }
        
        treatment <- tto_apriori[["apriori_occ_1"]][[paste0("ev.tto.occ1_ID", id_number)]]
        start <- min(treatment$TIME)
        end <- max(treatment$TIME)
        sim_results <- individual_sim(population_model, treatment, start, end)
        sim_results@data <- subset(sim_results@data, OCC == 1)

        simulation_results[[paste0("OCC_1_ID", id_number)]] <- sim_results
        event.tto.byocc[[paste0("ID_", id_number)]] <- treatment

      }, error = function(e) {
        if (verbose) {
        message(paste0("Could not simulate ID_", id_number, " in OCC1 (a_priori): ", e$message))
        } else {
          warning(paste0("Could not simulate ID_", id_number, " in OCC1 (a_priori): ", e$message),
                  call. = FALSE, immediate. = FALSE)
        }
      })
    }

    event.tto[["OCC_1"]] <- event.tto.byocc
  }

  # 2. Bayesian forecasting simulations
  if (assessment %in% c("Bayesian_forecasting", "Complete")) {

    posterior_estimations <- individual_model$ind_model
    tto_by_occ <- map_results$treatments_by_occ

    for (occasion_name in names(posterior_estimations)) {
      occ_posterior <- posterior_estimations[[occasion_name]]
      occ_number <- sub(".*_", "", occasion_name)
      tto.occ.names <- paste0("OCC", occ_number)

      event.tto.byocc <- list()
      treatment.occ.list[[tto.occ.names]] <- tto_by_occ[[paste0("tto_", occ_number)]]

      for (id_identifyer in names(occ_posterior)) {
        id_posterior <- occ_posterior[[id_identifyer]]
        id_number <- sub("ID_", "", id_identifyer)

        tryCatch({
          treatment <- tto_by_occ[[paste0("tto_occ_", occ_number)]][[paste0("ev.tto.occ", occ_number, "_ID", id_number)]]
          start <- min(treatment$TIME)
          end <- max(treatment$TIME)
          sim_results <- individual_sim(id_posterior, treatment, start, end)
          sim_results@data <- subset(sim_results@data, OCC == occ_number)

          simulation_results[[paste0("OCC_", occ_number, "_ID", id_number)]] <- sim_results
          event.tto.byocc[[paste0("ID_", id_number)]] <- treatment

        }, error = function(e) {
          if(verbose) {
            message(paste0("Could not process OCC_", occ_number, " ID_", id_number, ": ", e$message))
          } else {
            warning(paste0("Could not process OCC_", occ_number, " ID_", id_number, ": ", e$message),
                           call. = FALSE, immediate. = FALSE)
            }
        })
      }

      event.tto[[paste0("OCC_", occ_number)]] <- event.tto.byocc
    }
  }


  # Return unified output structure
  out <-list(
    simulation_results = simulation_results,
    ttoocc = treatment.occ.list,
    eval_type=evaluation_type,
    events_tto = event.tto,
    assessment = assessment
  )

  class(out) <- "mapbayr"
  return(out)

}






