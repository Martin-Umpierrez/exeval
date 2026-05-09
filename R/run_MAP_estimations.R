#' Run MAP Estimations
#'
#' Performs Maximum A Posteriori (MAP) estimations for pharmacokinetic models
#' using either `mapbayr` or `lixoftConnectors`.
#'
#' @details
#' The function accepts pharmacokinetic models either as raw \code{mrgsolve}
#' model code or as a pre-loaded \code{mrgmod} object. When a character string
#' is supplied, the model is compiled internally using \code{mrgsolve::mcode()}.
#' If a \code{mrgmod} object is provided, it is used directly for MAP estimation.
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
#' @param tool Character string. Specifies the tool to use for estimation. Currently using "mapbayr".
#' @param check_compile Logical. If `TRUE`, checks if the model compiles correctly in `mapbayr`.
#' @param data Data frame. Contains the input data for the estimations, including columns like ID, TIME, and OCC.
#' @param num_occ Integer. Number of occasions (OCC) to include in the analysis. If `NULL`, all unique occasions in `data` are used.
#' @param num_ids Integer. Number of unique IDs to include in the analysis. If `NULL`, all IDs are included.
#' @param sampling Logical. If `TRUE`, randomly samples the specified number of IDs from the data.
#' @param occ_ref Integer. Reference occasion for evaluation types that require a reference. Must be consistent with `evaluation_type`.
#' @param evaluation_type Character string. Specifies the evaluation type. Options are:
#'   \itemize{
#'     \item "Progressive": Uses all data up to each occasion.
#'     \item "Most_Recent_Progressive": Uses only the most recent occasion.
#'     \item "Cronologic_Ref": Uses all data up to a reference occasion.
#'     \item "Most_Recent_Ref": Uses the most recent occasion relative to a reference.
#'   }
#' @param names_occ Character string. Name of the OCC column in the data. Defaults to "OCC".
#' @param names_id Character string. Name of the ID column in the data. Defaults to "ID".
#' @param names_time Character string. Name of the TIME column in the data. Defaults to "TIME".
#' @param names_evid Character string. Name of the EVID column in the data. Defaults to "EVID".
#' @param method Character vector. Specifies optimization methods for `mapbayr`. Options are "L-BFGS-B" or "newuoa".
#'
#' @return A list containing:
#' \describe{
#'   \item{data_by_occ}{Filtered datasets by occasion.}
#'   \item{treatments_by_occ}{List of treatments grouped by occasion.}
#'   \item{treatments_apriori}{List of "a priori" treatments }
#'   \item{map_estimations}{MAP estimation results for each subset of the data.}
#'   \item{eval_type}{The evaluation type used.}
#'   \item{pop_model}{The population PK or PKPD model}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example using mapbayr
#' run_MAP_estimations(
#'   model_name = "example_model",
#'   model_code = "example_code",
#'   tool = "mapbayr",
#'   data = example_data,
#'   evaluation_type = "Progressive"
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
                                evaluation_type = c("Progressive", "Most_Recent_Progressive",
                                                    "Cronologic_Ref","Most_Recent_Ref"), ## Como se va a hacer la eval externa
                                names_occ = "OCC",
                                names_id = "ID",
                                names_time = "TIME",
                                names_evid = "EVID",
                                method = c("L-BFGS-B", "newuoa")) {

  # check data has the required columns
  if (!is.null(occ_ref) && !is.null(num_occ) && occ_ref != num_occ) {
    stop("occ_ref and num_occ must have the same value if both are specified.")
  }

  if (!is.null(occ_ref) && !is.null(num_occ) && occ_ref != num_occ) {
    stop("occ_ref and num_occ must have the same value if both are specified.")
  }

  if (!is.null(occ_ref) && (evaluation_type =="Progressive" || evaluation_type ==
                            "Most_Recent_Progressive")) {
    stop("occ_ref must be used wwith evaluation type Cronologic_Ref or Most_Recent_Ref")
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
    verificar_OCC(model)

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
    if(evaluation_type=="Progressive")
    {
      for (i in 1:num_occ) {
        nombre_vector <- paste0("dfOCC", i)
        list_df_basedata[[nombre_vector]] <- filtered_data|>dplyr::filter(OCC <= i)
      }
    }
    else if (evaluation_type=="Most_Recent_Progressive")
    {
      for (i in 1:num_occ) {
        nombre_vector <- paste0("dfOCC", i)
        list_df_basedata[[nombre_vector]] <- filtered_data|>filter(OCC == i)
      }
    }
    else if (evaluation_type=="Cronologic_Ref")
    {
      for (i in 1:occ_ref) {
        nombre_vector <- paste0("dfOCC", i)
        list_df_basedata[[nombre_vector]] <- filtered_data|>filter(OCC <= i)
      }

    }
    else if (evaluation_type== "Most_Recent_Ref")
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
        lista_ttos_occ[[vector_eventos]] <- list_ttos[[vector_ttos]] %>%
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
            lista_ttos_occ[[vector_eventos]] <- list_ttos[[vector_ttos]] %>%
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
          lista_ttos_occ[[vector_eventos]] <- list_ttos[[vector_ttos]] %>%
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
        lista_ttos_apriori_occ[[vector_eventos]] <- apriori_data %>%
          filter(ID == ids)
      }

      # save tto
      list_apriori[[paste0("apriori_occ_", occ_apriori)]] <- lista_ttos_apriori_occ

    # get map estimates for each data set
    list_map <- list()

    if(evaluation_type=="Progressive") {
      for (j in 1:(num_occ - 1)) {
        previous_numbers <- paste0(1:j, collapse = "_")
        map.result <- paste0("map.estimation.occ_0_",previous_numbers)
        list_map[[map.result]] <- mapbayr::mapbayest(my_model,
                                                     data = list_df_basedata[[paste0("dfOCC", j)]],
                                                     method = method)
      }
    }

    else if (evaluation_type== "Most_Recent_Progressive")  {
      for (j in 1:(num_occ - 1)) {
        map.result <- paste0("map.estimation.occ_", j)
        list_map[[map.result]] <- mapbayr::mapbayest(my_model,
                                                     data = list_df_basedata[[paste0("dfOCC", j)]],
                                                     method = method)
      }
    }
    else if (evaluation_type== "Cronologic_Ref")  {
      for (j in 1:(occ_ref - 1)) {
        previous_numbers <- paste0(1:j, collapse = "_")
        map.result <- paste0("map.estimation.occ_0_",previous_numbers)
        list_map[[map.result]] <- mapbayr::mapbayest(my_model,
                                                     data = list_df_basedata[[paste0("dfOCC", j)]],
                                                     method = method)
      }
    }
    else if (evaluation_type== "Most_Recent_Ref")  {
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
