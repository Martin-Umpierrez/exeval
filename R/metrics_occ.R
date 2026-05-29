#' Compute external evaluation performance metrics
#'
#' Computes predictive performance metrics from simulation outputs generated
#' during external model evaluation.
#'
#' This function compares simulated predictions with observed concentrations
#' and calculates individual- and occasion-level prediction error metrics.
#'
#' @param simulations Named list returned by [run_pk_simulations()]
#' containing simulation outputs and treatment/event data.
#' 
#' @param assessment Character string specifying the prediction strategy
#' used to generate the simulations.
#' Available options are:
#' \itemize{
#'   \item \code{"a_priori"}: evaluates predictions generated from the
#'   population model.
#'
#'   \item \code{"Bayesian_forecasting"}: evaluates predictions generated
#'   from individualized posterior models.
#'
#'   \item \code{"Complete"}: evaluates both a priori and Bayesian forecasting
#'   predictions.
#' }
#' @param tool Character string specifying the simulation backend.
#' Currently only \code{"mapbayr"} is supported
#' @param ... Additional arguments (currently unused).
#'
#'
#' @details
#' Individual predictions are matched with observed concentrations using
#' subject identifier (\code{ID}), occasion (\code{OCC}), and observation
#' time (\code{TIME}).
#'
#' The following metrics are calculated:
#' \itemize{
#'   \item \code{IPE}: individual prediction error (%)
#'   \item \code{APE}: absolute prediction error (%)
#'   \item \code{rRMSE}: relative root mean squared error (%)
#'   \item \code{rBIAS}: relative bias (%)
#'   \item \code{MAIPE}: mean absolute individual prediction error (%)
#'   \item \code{IF20}: percentage of predictions within 20\% of observations
#'   \item \code{IF30}: percentage of predictions within 30\% of observations
#' }
#' 
#' Individual observations are additionally classified into fit quality
#' categories (\code{Excellent}, \code{Acceptable}, \code{Poor},
#' \code{Very Poor}) based on absolute prediction error.
#' 
#' @return A named list containing:
#' \describe{
#'   \item{metrics}{Data frame containing individual prediction errors and
#'   fit classifications for each subject, occasion, and observation time.}
#'
#'   \item{metrics_means}{Data frame containing summary performance metrics
#'   aggregated by occasion.}
#' }
#' 
#' @seealso [run_pk_simulations()], [plot.EvalPPK()], [summary.EvalPPK()]
#' 
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
#'   assessment = "Complete"
#' )
#'
#' mm <- metrics_occ(
#'   simulations = sim,
#'   assessment = "Complete"
#' )
#' }
#' @export
#' 
metrics_occ <- function(simulations,
                       assessment = c("a_priori",
                                      "Bayesian_forecasting",
                                      "Complete"),
                       tool = "mapbayr", ...)  {

  # Create evaluation type
  evaluation_type <-simulations$eval_type
  # Robust asignment of arguments
  assessment <- match.arg(assessment)
  tool <- match.arg(tool)

  # MAPbayr + B.Forecasting
  if (tool == "mapbayr"  && assessment== "Bayesian_forecasting" ) {
    list.simulation<- simulations[["simulation_results"]]
    combine <- lapply(list.simulation, function(x) slot(x, "data"))
    df_simulaciones <- do.call(rbind, combine)
    df_simulaciones <- df_simulaciones |> rename(Ind_Prediction = DV) |>
      select (ID, OCC, TIME, Ind_Prediction) |> filter(Ind_Prediction>0)


    listtratamientos <- simulations[["ttoocc"]]
    df_ttos <- do.call(rbind,listtratamientos) |> filter(EVID==0) |>
      select(ID, OCC, TIME, DV)

    df_merged = left_join(df_simulaciones, df_ttos, by=c("ID", "OCC","TIME"))
    metrics = df_merged |> mutate(
      IPE = ((Ind_Prediction- DV)/DV) *100,
      APE= abs(((Ind_Prediction-DV)/DV))*100,
      RMSE = (((Ind_Prediction-DV)^2)/((DV)^2))
    ) |> filter(!is.na(DV)) |> distinct()

  }

  # MAPbayr + "apriori"
  else if (tool == "mapbayr"  && assessment== "a_priori" ) {
    list.simulation = simulations[["simulation_results"]]
    combine= lapply(list.simulation, function(x) slot(x, "data"))
    df_simulaciones <- do.call(rbind, combine)
    df_simulaciones = df_simulaciones |> rename(Ind_Prediction = CP) |>
      select (ID, OCC, TIME, Ind_Prediction) |> filter(Ind_Prediction>0)

    listtratamientos = simulations[["ttoocc"]]
    df_ttos = do.call(rbind, listtratamientos) |> filter(EVID==0) |>
      select(ID, OCC, TIME, DV)

    df_merged = left_join(df_simulaciones, df_ttos, by=c("ID", "OCC","TIME"))

    metrics = df_merged |> mutate(
      IPE = ((Ind_Prediction- DV)/DV) *100,
      APE= abs(((Ind_Prediction-DV)/DV))*100,
      RMSE = (((Ind_Prediction-DV)^2)/((DV)^2))
    ) |> filter(!is.na(DV)) |> distinct()
  }

  # MAPbayr + "Complete": "a priori" + Bayesian Forecasting
  else if (tool == "mapbayr" && assessment == "Complete") {
    list.simulation <- simulations[["simulation_results"]]
    combine <- lapply(list.simulation, function(x) slot(x, "data"))
    df_simulaciones <- do.call(rbind, combine)

    # rename predictions
    df_simulaciones <- df_simulaciones |>
      mutate(Ind_Prediction = ifelse(OCC == 1, CP, DV)) |>  # CP para apriori (OCC1), DV para posteriores
      select(ID, OCC, TIME, Ind_Prediction) |>
      filter(Ind_Prediction > 0)

    # get tto for every OCC
    listtratamientos <- simulations[["ttoocc"]]
    df_ttos <- do.call(rbind, listtratamientos) |>
      filter(EVID == 0) |>
      select(ID, OCC, TIME, DV)


    df_merged <- left_join(df_simulaciones, df_ttos, by = c("ID", "OCC", "TIME"))

    metrics <- df_merged |>
      mutate(
        IPE = ((Ind_Prediction - DV) / DV) * 100,
        APE = abs((Ind_Prediction - DV) / DV) * 100,
        RMSE = ((Ind_Prediction - DV)^2) / ((DV)^2)
      ) |>
      filter(!is.na(DV)) |>
      distinct()
  }
  
   metrics <- metrics |> 
    dplyr::mutate(
      Fit_Class = dplyr::case_when(
        abs(IPE) <= 15 ~ "Excellent",
        abs(IPE) <= 30 ~ "Acceptable",
        abs(IPE) <= 50 ~ "Poor",
        abs(IPE) > 50 ~ "Very Poor")) |> 
    dplyr::arrange(ID, OCC, TIME)

  if (!exists("metrics") || nrow(metrics) == 0) {
    stop("The 'metrics' object could not be generated. Please check the data and arguments.")
  }

  metrics_means <- metrics |>
    group_by(OCC) |>
    summarise(
      rBIAS= mean(IPE),
      rBIAS_lower = mean(IPE)-( qt(0.975, df = length(IPE) - 1) *(sd(IPE) / sqrt(length(IPE)))),
      rBIAS_upper = mean(IPE) +( qt(0.975, df = length(IPE) - 1) *(sd(IPE) / sqrt(length(IPE)))),
      MAIPE= mean(APE),
      rRMSE= sqrt(mean(RMSE)) *100,
      IF20= sum(abs(IPE) <= 20) *100 / length(IPE),
      IF30= sum(abs(IPE) <= 30) *100 / length(IPE),
      OCC= first(OCC) )

  return(
    list(metrics = metrics,
         metrics_means = metrics_means
    )
  )


}
