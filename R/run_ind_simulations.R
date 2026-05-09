#' Run Individual Simulations
#'
#' Simulates individual concentrations based on posterior estimations and treatment schedules for each occasion.
#'
#' @param individual_model A list object generated from the `actualize_model` function. It contains posterior estimations for every ID on each occasion.
#' @param tto_occ A list object generated from the `run_map_estimations` function. It contains treatment schedules for every ID in each occasion.
#' @param assessment Character string. Specifies the type of prediction to perform. Options are:
#'   \itemize{
#'     \item "a_priori": Simulates concentrations using the population model without individual data.
#'     \item "Bayesian_Forecasting": Simulates concentrations using individual parameter estimates (posterior mode).
#'     \item "Complete": Performs both a priori and Bayesian forecasting simulations.
#'   }
#' @param verbose Logical. If TRUE, messages are printed during execution.
#'   If FALSE (default), errors are stored as warnings accessible with `warnings()`.
#'
#' @return A list containing:
#' \item{simulation_results}{A list of simulation results for each occasion and individual.}
#' \item{ttoocc}{A list of treatments organized by occasion.}
#' \item{assessment}{A string indicating the type of assessment to perform.} Options are \code{"a_priori"}, \code{"Bayesian_forecasting"}, or \code{"Complete"}.
#' \item{eval_type}{The evaluation type used.}
#'
#' @details This function performs individual simulations for multiple occasions and individuals at the times an observation is available.
#' For each posterior estimation, it retrieves the corresponding treatment schedule and executes a simulation
#' over the specified time range. The results are stored for each individual within each occasion.
#'
#' @examples
#' \dontrun{
#' # Assuming `individual_model` is created with actualize_model
#' # and `tto_occ` is created with run_map_estimations:
#'
#' results <- run_ind_simulations(individual_model, tto_occ)
#'
#' # Access simulation results
#' sim_results <- results$simulation_results
#'
#' # Access treatment by occasion
#' treatments <- results$ttoocc
#'
#' # Access treatment events
#' events <- results$events_tto
#'}
#' @seealso \code{\link{actualize_model}}, \code{\link{run_MAP_estimations}}
#'
#' @export

run_ind_simulations <- function(individual_model,
                                tto_occ,
                                assessment = c("a_priori","Bayesian_forecasting","Complete"),
                                verbose= FALSE) {

  assessment <- match.arg(assessment)
  evaluation_type <-tto_occ$eval_type
  # Listas vacías para acumular resultados
  simulation_results <- list()
  event.tto <- list()
  treatment.occ.list <- list()


  ## 1. Simulaciones a priori
  if (assessment %in% c("a_priori", "Complete")) {

    population_model <- tto_occ$model
    tto_apriori <- tto_occ$apriori_treatments

    treatment_list <- tto_apriori[["tto_1"]]
    treatment.occ.list[["OCC1"]] <- treatment_list
    events_apriori <- tto_apriori[["apriori_occ_1"]]

    event.tto.byocc <- list()

    for (id_name in names(events_apriori)) {
      id_number <- sub(".*ID", "", id_name)

      tryCatch({
        set.seed(12345)
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

  ## 2. simulations
  if (assessment %in% c("Bayesian_forecasting", "Complete")) {

    posterior_estimations <- individual_model$ind_model
    tto_by_occ <- tto_occ$treatments_by_occ

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


  ## Return unified output
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






