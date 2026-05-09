#' Compute pharmacokinetic evaluation metrics by occasion (MAPbayr)
#'
#' Computes prediction error metrics by occasion (OCC) for individual
#' simulations generated using MAP Bayesian estimation with \pkg{mapbayr}.
#'
#' @param simulations An object of class \code{"mapbayr"} returned by
#'   \code{\link{run_ind_simulations}}.
#' @param assessment Character string specifying the type of prediction.
#'   One of \code{"a_priori"}, \code{"Bayesian_forecasting"}, or
#'   \code{"Complete"}.
#' @param tool Character string specifying the estimation tool.
#'   Currently only \code{"mapbayr"} is supported.
#' @param ... Additional arguments (not used).
#'
#' @return An object of class \code{EvalMetricsPPK} containing:
#' \itemize{
#'   \item \code{metrics}: Individual-level prediction errors by ID and OCC.
#'   \item \code{metrics_means}: Summary metrics by OCC.
#' }
#'
#' @details
#' This method extracts individual predictions and observed concentrations
#' from simulation outputs, merges them by ID, OCC and time, and computes
#' relative bias (rBIAS), relative RMSE (rRMSE), mean absolute individual
#' prediction error (MAIPE), and the percentages of predictions within
#' 20\% and 30\% of the observations (IF20 and IF30).
#'
#' @seealso \code{\link{metrics_occ}}, \code{\link{run_ind_simulations}}
#'
#' @method metrics_occ mapbayr
#' @export
metrics_occ.mapbayr <- function(simulations,
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
    df_simulaciones <- df_simulaciones %>% rename(Ind_Prediction = DV) %>%
      select (ID, OCC, TIME, Ind_Prediction) %>% filter(Ind_Prediction>0)


    listtratamientos <- simulations[["ttoocc"]]
    df_ttos <- do.call(rbind,listtratamientos) %>% filter(EVID==0) %>%
      select(ID, OCC, TIME, DV)

    df_merged = left_join(df_simulaciones, df_ttos, by=c("ID", "OCC","TIME"))
    metrics = df_merged %>% mutate(
      IPE = ((Ind_Prediction- DV)/DV) *100,
      APE= abs(((Ind_Prediction-DV)/DV))*100,
      RMSE = (((Ind_Prediction-DV)^2)/((DV)^2))
    ) %>% filter(!is.na(DV)) %>% distinct()

  }

  # MAPbayr + "apriori"
  else if (tool == "mapbayr"  && assessment== "a_priori" ) {
    list.simulation = simulations[["simulation_results"]]
    combine= lapply(list.simulation, function(x) slot(x, "data"))
    df_simulaciones <- do.call(rbind, combine)
    df_simulaciones = df_simulaciones %>% rename(Ind_Prediction = CP) %>%
      select (ID, OCC, TIME, Ind_Prediction) %>% filter(Ind_Prediction>0)

    listtratamientos = simulations[["ttoocc"]]
    df_ttos = do.call(rbind, listtratamientos) %>% filter(EVID==0) %>%
      select(ID, OCC, TIME, DV)

    df_merged = left_join(df_simulaciones, df_ttos, by=c("ID", "OCC","TIME"))

    metrics = df_merged %>% mutate(
      IPE = ((Ind_Prediction- DV)/DV) *100,
      APE= abs(((Ind_Prediction-DV)/DV))*100,
      RMSE = (((Ind_Prediction-DV)^2)/((DV)^2))
    ) %>% filter(!is.na(DV)) %>% distinct()
  }

  # MAPbayr + "Complete": "a priori" + Bayesian Forecasting
  else if (tool == "mapbayr" && assessment == "Complete") {
    list.simulation <- simulations[["simulation_results"]]
    combine <- lapply(list.simulation, function(x) slot(x, "data"))
    df_simulaciones <- do.call(rbind, combine)

    # rename predictions
    df_simulaciones <- df_simulaciones %>%
      mutate(Ind_Prediction = ifelse(OCC == 1, CP, DV)) %>%  # CP para apriori (OCC1), DV para posteriores
      select(ID, OCC, TIME, Ind_Prediction) %>%
      filter(Ind_Prediction > 0)

    # get tto for every OCC
    listtratamientos <- simulations[["ttoocc"]]
    df_ttos <- do.call(rbind, listtratamientos) %>%
      filter(EVID == 0) %>%
      select(ID, OCC, TIME, DV)


    df_merged <- left_join(df_simulaciones, df_ttos, by = c("ID", "OCC", "TIME"))

    metrics <- df_merged %>%
      mutate(
        IPE = ((Ind_Prediction - DV) / DV) * 100,
        APE = abs((Ind_Prediction - DV) / DV) * 100,
        RMSE = ((Ind_Prediction - DV)^2) / ((DV)^2)
      ) %>%
      filter(!is.na(DV)) %>%
      distinct()
  }

  if (!exists("metrics") || nrow(metrics) == 0) {
    stop("The 'metrics' object could not be generated. Please check the data and arguments.")
  }

  metrics_means <- metrics %>%
    group_by(OCC) %>%
    summarise(
      rBIAS= mean(IPE),
      rBIAS_lower = mean(IPE)-( qt(0.975, df = length(IPE) - 1) *(sd(IPE) / sqrt(length(IPE)))),
      rBIAS_upper = mean(IPE) +( qt(0.975, df = length(IPE) - 1) *(sd(IPE) / sqrt(length(IPE)))),
      MAIPE= mean(APE),
      rRMSE= sqrt(mean(RMSE)) *100,
      IF20= sum(abs(IPE) <= 20) *100 / length(IPE),
      IF30= sum(abs(IPE) <= 30) *100 / length(IPE),
      OCC= first(OCC) )

  eval_metrics_ppk(
    metrics = metrics,
    metrics_means = metrics_means,
    eval_type = evaluation_type,
    assessment = assessment,
    tool = tool
  )
}
