#' @importFrom methods slot
#' @importFrom stats qt sd
#' @importFrom rlang .data
NULL


utils::globalVariables(c(
  "APE", "CP", "Cc", "DV", "EVID", "ID", "IF20", "IF30",
  "IPE", "Ind_Prediction", "MAIPE", "Model", "OCC", "RMSE",
  "TIME", "dummy1", "dummy2", "id", "occ_ref",
  "original_id_output", "posterior_model", "prop",
  "rBIAS", "rBIAS_lower", "rBIAS_upper",
  "time", "tramo", "yy"
))







individual_sim <-
function(posterior_model, treatment, start,
         end, ss_fixed = FALSE, ss_n = NULL, tad = FALSE) {

  posterior_model <- posterior_model %>%
    mrgsolve::data_set(treatment) %>%
    mrgsolve::update(start = start, end = end)

  sim_result <- mrgsolve::mrgsim(posterior_model)
  return(sim_result)
}

#####metrics_occasion <-
##function(simresults) {
  ##simresults$ID_mapping |>
    #dplyr::mutate(ID = as.numeric(ID), original_id_output = as.numeric(original_id_output)) |>
    #dplyr::right_join( simresults$res$Cc, by = join_by(ID == id)) |>
    #dplyr::select(-ID) |>
    # cbind(simresults$data) |>
    #dplyr::inner_join( simresults$data, by = join_by(original_id_output == ID, time) ) |>
    #dplyr::rename( ID = original_id_output ) |>
    #dplyr::select(ID, time, Cc, DV, OCC) |>
    #dplyr::mutate(DV = as.numeric(DV)) |>   # compute metrics
    #dplyr::mutate(
      #IPE = ((Cc- DV)/DV) *100,
      #APE= abs(((Cc- DV)/DV))*100,
      #RMSE = (((Cc-DV)^2)/((DV)^2))
    #)
#######}
check_OCC_capture <- function(modelo) {
  # get model_code
  if (inherits(modelo, "mrgmod")) {
    codigo <- modelo@code
  } else if (is.character(modelo)) {
    codigo <- modelo
  } else {
    stop("modelo must be either character model code or a mrgmod object.")
  }

  # normalizar a vector de líneas
  if (length(codigo) == 1) {
    lineas <- strsplit(codigo, "\n")[[1]]
  } else {
    lineas <- codigo
  }

  # split every line of the model code
  lineas <- strsplit(modelo, "\n")[[1]]

  # starts of $CAPTURE
  inicio_capture <- grep("^\\s*\\$CAPTURE\\b", lineas)

  if (length(inicio_capture) == 0) {
    stop("Error: The $CAPTURE section was not found in the model.")
  }

  # lines from $CAPTURE to the end
  seccion_capture <- lineas[inicio_capture:length(lineas)]

  # Use word boundaries to ensure 'OCC' is matched as a whole word
  if (any(grepl("\\bOCC\\b", seccion_capture))) {
    message("Validation successful: 'OCC' is present in the $CAPTURE section.")
    return(TRUE)
  } else {
    stop("Error: 'OCC' is not present in the $CAPTURE section.")
  }
}

pop_sim <-
  function(population_model,
           treatment,
           start,
           end,
           ss_fixed = FALSE,
           ss_n = NULL,
           tad = FALSE) {

    population_model <- posterior_model %>%
      mrgsolve::data_set(treatment) %>%
      mrgsolve::update(start = start, end = end)

    sim_result <- mrgsolve::mrgsim(posterior_model)
    return(sim_result)
  }


eval_metrics_ppk <- function(metrics,
                             metrics_means,
                             eval_type,
                             assessment,
                             tool) {

  structure(
    list(
      metrics = metrics,
      metrics_means = metrics_means
    ),
    class = "EvalMetricsPPK",
    eval_type = eval_type,
    assessment = assessment,
    tool = tool
  )
}


#### Generate fake metrics for examples#####
generate_fake_metrics <- function(n_occasions = 3) {
  data.frame(
    OCC = rep(1:n_occasions),  # Simula varias ocasiones
    rBIAS = stats::rnorm(n_occasions, mean = 0, sd = 10),
    rBIAS_lower = stats::rnorm(n_occasions, mean = -5, sd = 5),
    rBIAS_upper = stats::rnorm(n_occasions, mean = 5, sd = 5),
    MAIPE = stats::runif(n_occasions, min = 10, max = 50),
    IF20 = stats::runif(n_occasions, min = 20, max = 80),
    IF30 = stats::runif(n_occasions, min = 30, max = 90)
  )
}


##### Extract observations and individual predictions ######
extract_predictions <- function(simulations) {

  assessment <- simulations$assessment

  list.sim <- simulations$simulation_results
  df_sim <- do.call(rbind, lapply(list.sim, function(x) slot(x, "data")))

  if (assessment == "Bayesian_forecasting") {

    df_sim <- df_sim %>%
      dplyr::mutate(
        Prediction = DV,
        Prediction_Type = "Posterior"
      )

  } else if (assessment == "a_priori") {

    df_sim <- df_sim %>%
      dplyr::mutate(
        Prediction = CP,
        Prediction_Type = "Apriori"
      )

  } else if (assessment == "Complete") {

    df_sim <- df_sim %>%
      dplyr::mutate(
        Prediction = ifelse(OCC == 1, CP, DV),
        Prediction_Type = ifelse(OCC == 1, "Apriori", "Posterior")
      )
  }

  df_sim <- df_sim %>%
    dplyr::select(ID, OCC, TIME, Prediction, Prediction_Type) %>%
    dplyr::filter(!is.na(Prediction))

  df_obs <- do.call(rbind, simulations$ttoocc) %>%
    dplyr::filter(EVID == 0) %>%
    dplyr::select(ID, OCC, TIME, DV) %>%
    dplyr::filter(!is.na(DV))

  df_final <- dplyr::left_join(
    df_sim,
    df_obs,
    by = c("ID", "OCC", "TIME")
  ) %>%
    dplyr::filter(!is.na(DV)) %>%
    dplyr::distinct()

  return(df_final)
}



