#' Run MAP Bayesian Estimation for External Model Evaluation
#'
#' Performs Maximum A Posteriori (MAP) Bayesian estimation using
#' \pkg{mapbayr} for external evaluation of pharmacokinetic models across
#' multiple dosing occasions.
#'
#' @details
#' The population model can be provided either as:
#' \itemize{
#'   \item a compiled \code{mrgsolve::mrgmod} object, or
#'   \item a character string containing \pkg{mrgsolve} model code.
#' }
#' 
#' When model code is supplied as a character string, the model is compiled
#' internally using \code{mrgsolve::mcode()}. In this case, a model name
#' must be provided via \code{model_name}.
#' 
#' The evaluation strategy defines which observations are used to inform
#' each MAP estimation:
#' \itemize{
#'   \item \code{"sequential_updating"}: cumulative observations up to each
#'         occasion (e.g., OCC1, OCC1+2, OCC1+2+3).
#'   \item \code{"stepwise_updating"}: observations from each occasion treated
#'         independently.
#'   \item \code{"sequential_reference_updating"}: cumulative observations up to
#'         the reference occasion \code{occ_ref}.
#'   \item \code{"backward_reference_updating"}: sequential backward updating
#'         from \code{occ_ref}.
#' }
#' 
#' @param model Population PK model, provided either as:
#' \itemize{
#'   \item A character string containing the pharmacokinetic model code written
#'         in \code{mrgsolve} format.
#'   \item A pre-compiled \code{mrgmod} object (S3 class from \code{mrgsolve}).
#' }
#' If a character string is provided, \code{model_name} must also be specified.
#' 
#' @param model_name Character string. Name used when compiling the model
#' with \code{mrgsolve::mcode()}. Required only when \code{model} is
#' provided as character model code.
#' 
#' @param tool Character string. Estimation engine to use.
#' Currently only \code{"mapbayr"} is supported.
#' 
#' @param check_compile Logical. If \code{TRUE}, validates model compatibility
#' with \pkg{mapbayr} before estimation.
#' 
#' @param data Data frame containing external evaluation data.
#' Must include at least \code{ID}, \code{OCC}, and \code{CMT}.
#' See [prepare_data()] for expected formatting and preprocessing.
#' 
#' @param num_occ Integer. Maximum number of occasions to include in the analysis.
#' If \code{NULL}, all available occasions in the data are used.
#' 
#' 
#' @param num_ids Integer. Number of subjects to include.
#' If \code{NULL}, all unique subjects are used.
#' 
#' @param sampling Logical. If \code{TRUE}, subjects are randomly sampled
#' when \code{num_ids} is specified. Otherwise, the first \code{num_ids}
#' subjects are selected.
#' 
#' @param occ_ref Integer. Reference occasion used for reference-based
#' evaluation strategies. Required when \code{evaluation_type} is
#' \code{"sequential_reference_updating"} or
#' \code{"backward_reference_updating"}, where MAP estimation is performed
#' relative to this occasion.
#' 
#' @param evaluation_type Character string specifying the evaluation strategy.
#' Available options are:
#' \itemize{
#'   \item \code{"sequential_updating"}: performs MAP estimation using all
#'         observations accumulated up to each occasion.
#'   \item \code{"stepwise_updating"}: performs MAP estimation using
#'         observations from each occasion independently.
#'   \item \code{"sequential_reference_updating"}: performs MAP estimation
#'         using cumulative observations up to the reference occasion
#'         defined by \code{occ_ref}.
#'   \item \code{"backward_reference_updating"}: performs MAP estimation
#'         by sequentially moving backward from the reference occasion
#'         defined by \code{occ_ref}.
#' }
#' @param method Character string specifying the optimization algorithm
#' passed to \code{mapbayr::mapbayest()} for MAP estimation.
#' Supported options are \code{"L-BFGS-B"} and \code{"newuoa"}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{data_by_occ}{List of input datasets partitioned according to the
#'   selected evaluation strategy, where each element contains the observations
#'   used for a specific MAP estimation.}
#'
#'   \item{treatments_by_occ}{List of treatment/event datasets grouped by
#'   occasion and subject, used for posterior predictive simulations.}
#'
#'   \item{apriori_treatments}{List of treatment/event datasets used for
#'   a priori predictive simulations.}
#'
#'   \item{map_estimations}{List of MAP estimation objects returned by
#'   \code{mapbayr::mapbayest()} for each evaluation subset.}
#'
#'   \item{eval_type}{Character string indicating the selected evaluation
#'   strategy.}
#'
#'   \item{pop_model}{Compiled population model (\code{mrgmod}) used for
#'   estimation.}
#' }
#' @seealso [mapbayr::mapbayest()], [mrgsolve::mcode()]
#' 
#' @export
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
#' }
run_MAP_estimations <-
function(model, model_name= NULL,
                                tool = "mapbayr",
                                check_compile = TRUE,
                                data, num_occ = NULL, ### Para lixoft definimos solo occ
                                num_ids= NULL,
                                sampling = TRUE,
                                occ_ref = NULL , ### Se usa solo si evaluation_type es basado en una referencia
                                evaluation_type = c("sequential_updating", "stepwise_updating",
                                                    "sequential_reference_updating","backward_reference_updating"), ## Como se va a hacer la eval externa
                                method = c("L-BFGS-B", "newuoa")) {

  # check data has the required columns
  if (!is.null(occ_ref) && !is.null(num_occ) && occ_ref != num_occ) {
    stop("occ_ref and num_occ must have the same value if both are specified.")
  }

  if (!is.null(occ_ref) && !is.null(num_occ) && occ_ref != num_occ) {
    stop("occ_ref and num_occ must have the same value if both are specified.")
  }

  if (!is.null(occ_ref) && (evaluation_type =="sequential_updating" || evaluation_type ==
                            "stepwise_updating")) {
    stop("occ_ref must be used wwith evaluation type sequential_reference_updating or backward_reference_updating")
  }

  if (tool == "mapbayr") {
    # check mrgsolve format
    if (inherits(model, "mrgmod")) {

      my_model <- model

    } else if (is.character(model)) {

      if (is.null(model_name)) {
        stop("model_name must be provided when model is character code.")
      }

      my_model <- mrgsolve::mcode(model_name, model)

    } else {
      stop("model must be either a mrgmod object or character model code.")
    }
    if (check_compile) {
      check_model <- mapbayr::check_mapbayr_model(my_model, check_compile = TRUE)
      message("Model is ok for estimation")
      if (is.null(check_model)) {
        message("Check mrg model")
      }
    }
    # more checks
    check_OCC_capture(model)

    # check OCC and CMT exist in the external dataset
    if (!"OCC" %in% names(data)) {
      stop(" 'OCC' column is mandatory in the base data.")
    }

    if (!"CMT" %in% names(data)) {
      stop(" 'CMT' column is mandatory in the base data.")
    }
    # number of OCC
    if (is.null(num_occ)) {
      num_occ <- length(unique(data$OCC))
    } else {
      num_occ <- min(num_occ, length(unique(data$OCC)))
    }

    # number of IDs
    if (is.null(num_ids)) {
      num_ids <- length(unique(data$ID))
    } else {
      if (sampling) {
        # Random sampling without replace
        selected_ids <- sample(unique(data$ID), size = min(num_ids,
                                                           length(unique(data$ID))), replace = FALSE)
        data <- data|>dplyr::filter(ID %in% selected_ids)
      } else {
        num_ids <- min(num_ids, length(unique(data$ID)))
        selected_ids <- unique(data$ID)[1:num_ids]
        data <- data|> dplyr::filter(ID %in% selected_ids)
      }
    }

    # Get data until current OCC
    filtered_data <- data|>dplyr::filter(OCC <= num_occ)

    # construct data list to get the MAPs
    list_df_basedata <- list()
    if(evaluation_type=="sequential_updating")
    {
      for (i in 1:num_occ) {
        nombre_vector <- paste0("dfOCC", i)
        list_df_basedata[[nombre_vector]] <- filtered_data|>dplyr::filter(OCC <= i)
      }
    }
    else if (evaluation_type=="stepwise_updating")
    {
      for (i in 1:num_occ) {
        nombre_vector <- paste0("dfOCC", i)
        list_df_basedata[[nombre_vector]] <- filtered_data|>filter(OCC == i)
      }
    }
    else if (evaluation_type=="sequential_reference_updating")
    {
      for (i in 1:occ_ref) {
        nombre_vector <- paste0("dfOCC", i)
        list_df_basedata[[nombre_vector]] <- filtered_data|>filter(OCC <= i)
      }

    }
    else if (evaluation_type== "backward_reference_updating")
      for (i in occ_ref:1) {
        nombre_vector <- paste0("dfOCC", i)
        list_df_basedata[[nombre_vector]] <- filtered_data|>filter(OCC <= i)
      }
    # construct list of treatments by OCC
    list_ttos <- list()
    # add an if else sentece if data has ss or not , then how to compute ev tables
    if("SS" %in% names(data)|| "ss" %in% names(data)) {
    if (is.null(occ_ref)) {
      for (n in 2:num_occ) {
        vector_ttos <- paste0("tto_", n)
        list_ttos[[vector_ttos]] <- filtered_data|>filter(OCC == n)

        # contruct events per tto and ID
        num_ids_ttos <- length(unique(list_ttos[[vector_ttos]]$ID))
        lista_ttos_occ <- list()
        for (ids in 1:num_ids_ttos) {
          vector_eventos <- paste0("ev.tto.occ", n, "_ID", ids)
          lista_ttos_occ[[vector_eventos]] <- list_ttos[[vector_ttos]] |>
            filter(ID == ids)
        }

        # save tto list
        list_ttos[[paste0("tto_occ_", n)]] <- lista_ttos_occ
      }
    }
    else {
      vector_ttos <- paste0("tto_", occ_ref)
      list_ttos[[vector_ttos]] <- filtered_data|>filter(OCC == occ_ref)

      # contruct events per tto and ID
      num_ids_ttos <- length(unique(list_ttos[[vector_ttos]]$ID))
      lista_ttos_occ <- list()
      for (ids in 1:num_ids_ttos) {
        vector_eventos <- paste0("ev.tto.occ", occ_ref, "_ID", ids)
        lista_ttos_occ[[vector_eventos]] <- list_ttos[[vector_ttos]] |>
          filter(ID == ids)
      }

      # save tto
      list_ttos[[paste0("tto_occ_", occ_ref)]] <- lista_ttos_occ
    }
    }
    else {
      if (is.null(occ_ref)) {
        for (n in 2:num_occ) {
          vector_ttos <- paste0("tto_", n)
          list_ttos[[vector_ttos]] <- filtered_data|>filter(OCC <= n)

          # Generar los eventos para cada tratamiento y cada ID
          num_ids_ttos <- length(unique(list_ttos[[vector_ttos]]$ID))
          lista_ttos_occ <- list()
          for (ids in 1:num_ids_ttos) {
            vector_eventos <- paste0("ev.tto.occ", n, "_ID", ids)
            lista_ttos_occ[[vector_eventos]] <- list_ttos[[vector_ttos]] |>
              filter(ID == ids)  ##### REMOVE OF EVID==1 to get all times for simulation
          }

          # Guardar los tratamientos por OCC
          list_ttos[[paste0("tto_occ_", n)]] <- lista_ttos_occ
        }
      }
      else {
        vector_ttos <- paste0("tto_", occ_ref)
        list_ttos[[vector_ttos]] <- filtered_data|>filter(OCC == occ_ref)

        # Generar los eventos para cada tratamiento y cada ID
        num_ids_ttos <- length(unique(list_ttos[[vector_ttos]]$ID))
        lista_ttos_occ <- list()
        for (ids in 1:num_ids_ttos) {
          vector_eventos <- paste0("ev.tto.occ", occ_ref, "_ID", ids)
          lista_ttos_occ[[vector_eventos]] <- list_ttos[[vector_ttos]] |>
            filter(ID == ids) ##### REMOVE OF EVID==1 to get all times for simulation
        }

        # Guardar los tratamientos por OCC
        list_ttos[[paste0("tto_occ_", occ_ref)]] <- lista_ttos_occ
      }

    }

    # tto for OCC=1 o OCC=ref

    list_apriori <- list()

      occ_apriori<- ifelse(is.null(occ_ref),1,occ_ref)
      vector_ttos <- paste0("tto_", occ_apriori)
      apriori_data <- filtered_data|>filter(OCC== occ_apriori)
      list_apriori[[vector_ttos]] <- apriori_data

      # construct events per tto and id
      num_ids_apriori <- length(unique(list_apriori[[vector_ttos]]$ID))
      lista_ttos_apriori_occ <- list()
      for (ids in 1:num_ids_ttos) {
        vector_eventos <- paste0("ev.tto.occ", occ_apriori, "_ID", ids)
        lista_ttos_apriori_occ[[vector_eventos]] <- apriori_data |>
          filter(ID == ids)
      }

      # save tto
      list_apriori[[paste0("apriori_occ_", occ_apriori)]] <- lista_ttos_apriori_occ

    # get map estimates for each data set
    list_map <- list()

    if(evaluation_type=="sequential_updating") {
      for (j in 1:(num_occ - 1)) {
        previous_numbers <- paste0(1:j, collapse = "_")
        map.result <- paste0("map.estimation.occ_0_",previous_numbers)
        list_map[[map.result]] <- mapbayr::mapbayest(my_model,
                                                     data = list_df_basedata[[paste0("dfOCC", j)]],
                                                     method = method)
      }
    }

    else if (evaluation_type== "stepwise_updating")  {
      for (j in 1:(num_occ - 1)) {
        map.result <- paste0("map.estimation.occ_", j)
        list_map[[map.result]] <- mapbayr::mapbayest(my_model,
                                                     data = list_df_basedata[[paste0("dfOCC", j)]],
                                                     method = method)
      }
    }
    else if (evaluation_type== "sequential_reference_updating")  {
      for (j in 1:(occ_ref - 1)) {
        previous_numbers <- paste0(1:j, collapse = "_")
        map.result <- paste0("map.estimation.occ_0_",previous_numbers)
        list_map[[map.result]] <- mapbayr::mapbayest(my_model,
                                                     data = list_df_basedata[[paste0("dfOCC", j)]],
                                                     method = method)
      }
    }
    else if (evaluation_type== "backward_reference_updating")  {
      for (j in (occ_ref - 1):1) {
        previous_numbers <- paste0((occ_ref-1):j, collapse = "_")
        map.result <- paste0("map.estimation.occ_",previous_numbers)
        list_map[[map.result]] <- mapbayr::mapbayest(my_model,
                                                     data = list_df_basedata[[paste0("dfOCC", j)]],
                                                     method = method)
      }
    }

    return(list(
      data_by_occ = list_df_basedata,
      treatments_by_occ = list_ttos,
      apriori_treatments = list_apriori,
      map_estimations = list_map,
      eval_type = evaluation_type,
      pop_model = my_model
    ))
  }
}
