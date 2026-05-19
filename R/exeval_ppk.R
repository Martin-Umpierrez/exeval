#' External evaluation for Population Pharmacokinetic (popPK) Models
#'
#' Performs external evaluation of popPK models by conducting MAP estimations and individual simulations
#' for each occasion using different evaluation strategies (see evaluation_type)
#'
#' @param model Either:
#' \itemize{
#'   \item A character string containing the pharmacokinetic model code written
#'         in \code{mrgsolve} format.
#'   \item A pre-compiled \code{mrgmod} object (S3 class from \code{mrgsolve}).
#' }
#' If a character string is provided, \code{model_name} must also be specified.
#' @param model_name Character string. Name of the model.
#' Required only if \code{model} is provided as character model code.#'
#' @param drug_name Character string. Used only for reporting purposes.
#' @param tool Character string. Specifies the tool to use for estimation. Currently "mapbayr" is the only option.
#' @param check_compile Logical. If `TRUE`, checks if the model compiles correctly in `mapbayr`.
#' @param data Data frame. Contains the input data for the estimations, including columns like ID, TIME, and OCC.
#' @param num_occ Integer. Number of occasions (OCC) to include in the analysis. If `NULL`, all unique occasions in `data` are used.
#' @param num_ids Integer. Number of unique IDs to include in the analysis. If `NULL`, all IDs are included.
#' @param sampling Logical. If `TRUE`, randomly samples the specified number of IDs from the data.
#' @param occ_ref Integer. Reference occasion for evaluation types that require a reference. Must be consistent with `evaluation_type`.
#' @param evaluation_type Character string. Specifies the evaluation type. Options are:
#'   \itemize{
#'     \item "sequential_updating": Uses all data up to each occasion.
#'     \item "Most_Recent_sequential_updating": Uses only the most recent occasion.
#'     \item "Cronologic_Ref": Uses all data up to a reference occasion.
#'     \item "Most_Recent_Ref": Uses the most recent occasion relative to a reference.
#'   }
#' @param method Character vector. Specifies optimization methods for `mapbayr`. Options are "L-BFGS-B" or "newuoa".
#' @param assessment Character string. Specifies the type of prediction to perform. Options are:
#'   \itemize{
#'     \item "a_priori": Simulates concentrations using the population model without individual data.
#'     \item "Bayesian_Forecasting": Simulates concentrations using individual parameter estimates (posterior mode).
#'     \item "Complete": Performs both a priori and Bayesian forecasting simulations.
#'   }
#' @param verbose Logical. If TRUE, messages are printed during execution.
#'   If FALSE (default), errors are stored as warnings accessible with `warnings()`
#'
#' @return A list containing:
#' \describe{
#'   \item{metrics}{Evaluation metrics}
#'   \item{estimations}{MAP estimation results for each subset of the data.}
#'   \item{simulation}{A list of simulation results for each occasion and individual.}
#'   \item{updates}{A list containing posterior estimations (`a.posteriori`) for each occasion.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' data("tacrolimus_pk1_kidney", package = "preDose")
#' data("model_tacHAN2011", package = "preDose")
#'
#' dd <- tacrolimus_pk1_kidney |> subset(ID < 6)
#' mm <- get_model_code('Han_etal_2011')
#'
#' res <- exeval_ppk(model_name = "tacrolimus_HAN2011",
#'                  model = mm,
#'                  data = dd,
#'                  evaluation_type= "sequential_updating",
#'                  assessment='Bayesian_forecasting' )
#'
#' res # Print the results
#' }

exeval_ppk <-  function(model,
                        data,
                        model_name=NULL,
                        drug_name=NULL,
                        tool = "mapbayr",
                        check_compile = TRUE,
                        num_occ = NULL, ### Para lixoft definimos solo occ
                        num_ids= NULL,
                        sampling = TRUE,
                        occ_ref = NULL , ### Se usa solo si evaluation_type es basado en una referencia
                        evaluation_type = c("sequential_updating", "Most_Recent_sequential_updating","Cronologic_Ref","Most_Recent_Ref"), ## Como se va a hacer la eval externa
                        method = c("L-BFGS-B", "newuoa"),
                        assessment = c("a_priori","Bayesian_forecasting", "Complete"),
                        verbose=FALSE) {

  ## Run estimation, simulation and predicton erro computation in every OCC
  est <- run_MAP_estimations(model, model_name, tool, check_compile,
                             data, num_occ, num_ids, sampling, occ_ref, evaluation_type,
                             method
                             )
  updt <- update_map_models(est, evaluation_type)
  sims <- run_pk_simulations(updt, est, assessment)

  # Compute evaluation metrics
  metrics <- metrics_occ(sims, assessment=assessment,tool=tool )


  argument = c('Num IDs', 'Observations','Max Num Occasion',
               'Num of Ref Occasion','Drug Name', 'Model Name', 'Evaluation', 'Assessment')
  value    = c(dplyr::n_distinct(data$ID),
               length(data %>% filter(EVID==0)),
               max(data$OCC, na.rm = TRUE),
               ifelse(is.null(occ_ref), "", occ_ref),
               ifelse(is.null(drug_name), "", drug_name),
               ifelse(is.null(model_name), "", model_name),
               match.arg(evaluation_type),
               match.arg(assessment))
  info = data.frame(argument, value)

  structure(
    list(metrics=metrics, estimates=est, updates=updt, simulations=sims),
    class = 'EvalPPK',
    attributes = info)

}
