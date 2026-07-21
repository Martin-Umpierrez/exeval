#' External evaluation workflow for population PK, PKPD models
#'
#' Runs the complete external evaluation workflow for population
#' pharmacokinetic (popPK) or pharmacokinetic-pharmacodynamic (PKPD) models,
#' including MAP estimation, posterior model updating, simulation, and
#' prediction error metric calculation.
#' 
#' This function serves as the main high-level interface for the
#' \pkg{exeval} workflow. 
#'
#' @param model Population PK model provided as one of the following:
#' \itemize{
#'   \item a character string containing \pkg{mrgsolve} model code,
#'   \item a compiled \code{mrgsolve::mrgmod} object,
#'   \item or a model label matching an entry in the internal
#'   \code{exeval_models} database.
#' }
#' 
#' If model code is supplied as a character string, \code{model_name} must
#' also be provided.
#' 
#' @param data Data frame containing the external evaluation dataset.
#' Must include at least \code{ID}, \code{OCC}, and \code{CMT}.
#' See [prepare_data()] for expected input formatting.
#' 
#' @param model_name Character string. Name used when compiling the model
#' with \code{mrgsolve::mcode()}. Required only when \code{model} is
#' provided as character code.
#' 
#' @param drug_name Character string used for reporting purposes only.
#' 
#' @param tool Character string. Specifies the tool to use for estimation. Currently "mapbayr" is the only option.
#' 
#' @param tool Character string specifying the estimation backend.
#' Currently only \code{"mapbayr"} is supported.
#'
#' @param check_compile Logical. If \code{TRUE}, checks model compatibility
#' with \pkg{mapbayr} before estimation.
#' 
#' @param num_occ Integer. Maximum number of occasions to include in the
#' evaluation. If \code{NULL}, all available occasions are used.
#'
#' @param num_ids Integer. Number of subjects to include.
#' If \code{NULL}, all unique subjects are used.
#'
#' @param sampling Logical. If \code{TRUE}, subjects are randomly sampled
#' when \code{num_ids} is specified. Otherwise, the first subjects are used.
#' 
#' @param occ_ref Integer. Reference occasion used for
#' \code{"sequential_reference_updating"} and
#' \code{"backward_reference_updating"} evaluation strategies.
#' 
#' @param history_occ Integer. Number of previous occasions used to inform
#' MAP estimation when \code{evaluation_type = "stepwise_updating"}.
#' The default value (\code{1}) reproduces the original stepwise strategy,
#' where only the immediately preceding occasion is used.
#' Larger values create a moving window of previous occasions. If
#' \code{history_occ} exceeds the number of available previous occasions,
#' all available observations are used.
#' 
#' @param evaluation_type Character string specifying the evaluation strategy:
#' \itemize{
#'   \item \code{"sequential_updating"}: cumulative MAP updating across occasions.
#'   \item \code{"stepwise_updating"}:
#'   performs MAP estimation using a moving window of previous occasions.
#'   The number of previous occasions is controlled by
#'   \code{history_occ}.
#'   \item \code{"sequential_reference_updating"}: cumulative MAP updating up to
#'   a reference occasion.
#'   \item \code{"backward_reference_updating"}: backward updating from a
#'   reference occasion.
#' }
#' 
#' @param method Character string specifying the optimization algorithm passed
#' to \code{mapbayr::mapbayest()}. Supported options are
#' \code{"L-BFGS-B"} and \code{"newuoa"}.
#' 
#' @param assessment Character string specifying the simulation strategy.
#' Available options are:
#' \itemize{
#'   \item \code{"a_priori"}: simulates predictions using the population model
#'   without individual posterior information.
#'
#'   \item \code{"Bayesian_forecasting"}: simulates predictions using
#'   individualized posterior parameter estimates obtained from MAP estimation.
#'
#'   \item \code{"Complete"}: performs both a priori and Bayesian forecasting
#'   simulations.
#' }
#' @param verbose Logical. If \code{TRUE}, progress messages are printed during
#' execution.
#' 
#' @param progress Logical.
#' If \code{TRUE}, prints a concise workflow summary and progress
#' messages during the external evaluation.
#' 
#' @details
#' This function executes the complete external evaluation workflow:
#' \enumerate{
#'   \item MAP estimation via [run_MAP_estimations()]
#'   \item posterior model updating via [update_map_models()]
#'   \item PK simulations via [run_pk_simulations()]
#'   \item prediction error metric calculation via [metrics_occ()]
#' }
#'
#' The returned object is an \code{EvalPPK} object containing all intermediate
#' results and summary metadata.
#'
#' @return An object of class \code{EvalPPK} containing:
#' \describe{
#'   \item{metrics}{Prediction error metrics returned by [metrics_occ()].}
#'
#'   \item{estimates}{MAP estimation results returned by
#'   [run_MAP_estimations()].}
#'
#'   \item{updates}{Posterior individualized models returned by
#'   [update_map_models()].}
#'
#'   \item{simulations}{Simulation outputs returned by
#'   [run_pk_simulations()].}
#' }
#'
#' Additional workflow metadata are stored as object attributes.
#'
#' @seealso [run_MAP_estimations()], [update_map_models()],
#' [run_pk_simulations()], [metrics_occ()]
#'
#' @examples
#' \donttest{
#' data("tacrolimus_pk1_kidney", package = "exeval")
#' data("model_tacHAN2011", package = "exeval")
#'
#' dd <- tacrolimus_pk1_kidney |> subset(ID < 6)
#'
#' res <- exeval_ppk(model="TAC_Han2011",
#'                  data = dd,
#'                  evaluation_type= "sequential_updating",
#'                  assessment='Bayesian_forecasting' )
#'
#' print(res) # Print the results
#' }
#' @export

exeval_ppk <-  function(model,
                        data,
                        model_name=NULL,
                        drug_name=NULL,
                        tool = "mapbayr",
                        check_compile = TRUE,
                        num_occ = NULL, 
                        num_ids= NULL,
                        sampling = TRUE,
                        occ_ref = NULL , 
                        history_occ = 1 , 
                        evaluation_type = c("sequential_updating", "stepwise_updating","sequential_reference_updating","backward_reference_updating"), ## Como se va a hacer la eval externa
                        method = c("L-BFGS-B", "newuoa"),
                        assessment = c("a_priori","Bayesian_forecasting", "Complete"),
                        verbose=FALSE,
                        progress=TRUE) {
  t0 <- Sys.time()
  
  
  if(model %in% exeval_models$Label){
    model_name <- model
    model      <- exeval_models$Model_code[exeval_models$Label == model]
  }

  if (progress) {
    
    cat(
      "────────────────────────────────────────────────────────────\n",
      sprintf("%34s\n", paste0("exeval ", utils::packageVersion("exeval"))),
      sprintf("%44s\n", "External Model Evaluation Workflow"),
      "────────────────────────────────────────────────────────────\n\n",
      sprintf("%-17s : %s\n", "Population model", model_name),
      sprintf("%-17s : %s\n", "Drug", ifelse(is.null(drug_name), "-", drug_name)),
      sprintf("%-17s : %s\n", "Evaluation", match.arg(evaluation_type)),
      sprintf("%-17s : %s\n", "Assessment", match.arg(assessment)),
      "\n",
      sprintf("%-17s : %d subjects\n",
              "External dataset",
              dplyr::n_distinct(data$ID)),
      sprintf("%-17s   %d occasions\n",
              "",
              ifelse(
                is.null(num_occ),
                max(data$OCC, na.rm = TRUE),
                num_occ)),
      "\n",
      "────────────────────────────────────────────────────────────\n\n",
      "Running MAP Bayesian estimation...\n\n",
      sep = ""
    )
    
  }
  
  
  
  
  ## Run estimation, simulation and predicton erro computation in every OCC
  est <- run_MAP_estimations(model, model_name, tool, check_compile,
                             data, num_occ, num_ids, sampling, occ_ref, history_occ, evaluation_type,
                             method
                             )
  if(progress)
    cat("\n✓ MAP estimation completed\n\n")
  
  if(progress)
    cat("Updating Individual Models...\n")
  
  updt <- update_map_models(est, evaluation_type)
  
  if(progress)
    cat("✓ Model updating completed\n\n")
  
  if(progress)
    cat("Running PK/PD simulations...\n")
  
  sims <- run_pk_simulations(updt, est, assessment)

  # Compute evaluation metrics
  if(progress)
    cat("✓ Simulations completed\n\n")
  
  if(progress)
    cat("Computing metrics...\n")
  
  metrics <- metrics_occ(sims, assessment=assessment,tool=tool )
  
  if(progress)
    cat("✓ Metrics computed\n")
  
  elapsed <- difftime(Sys.time(), t0, units = "secs")
  
  if(progress){
    
    cat(
      "\n",
      "────────────────────────────────────────────────────────────\n",
      "✓ External evaluation completed successfully\n\n",
      sprintf("Elapsed time : %.1f s\n", as.numeric(elapsed)),
      "────────────────────────────────────────────────────────────\n",
      sep=""
    )
    
  }

  argument = c('Num IDs', 'Num of Observations Evaluated','Max Num Occasion',
               'Num of Ref Occasion','Drug Name', 'Model Name', 'Evaluation', 'Assessment')
  value    = c(dplyr::n_distinct(data$ID),
               nrow(metrics$metrics),
               ifelse(
                 is.null(num_occ),
                 max(data$OCC, na.rm = TRUE),
                 num_occ),
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




#' S3 print method for \code{EvalPPK} objects
#'
#' Prints a formatted representation of an \code{EvalPPK} object, including
#' dataset characteristics, evaluation settings, and performance metrics.
#' 
#' @param x An object of class \code{EvalPPK}.
#' @param ... additional arguments (not used)
#' 
#' @rdname exeval_ppk
#' 
#' @export
print.EvalPPK <- function(x, ...) {
  info <- attr(x, 'attributes')
  nn <- max(nchar(paste(info$argument, info$value)))

  # Data summary (Number of IDs, Number of Observations, Max OCC, Ref OCC)
  data_info <- info[1:4, ]
  data_info <- data_info[data_info$value != "", ]
  if(nrow(data_info) > 0){
    cat(rep("=", nn + 4), "\n", sep = "")
    cat('Data summary\n')
    print(data_info)
    cat(rep("=", nn + 4), "\n\n", sep = "")
  }
  cat(rep("=", nn + 4), "\n", sep = "")
  cat('Evaluation information')
  cat("\n")
  print(info[5:8, ])
  cat(rep("=", nn + 4), "\n", sep = "")
  cat("\n")
  cat(rep("=", nn + 4), "\n", sep = "")
  cat('Evaluation metrics')
  cat("\n")
  print( x$metrics$metrics_means )
  cat(rep("=", nn + 4), "\n", sep = "")
}


#' S3 print method for \code{summary.EvalPPK} objects
#'
#' Prints a formatted representation of a summary generated from an
#' \code{EvalPPK} object, including metadata, applied summary settings,
#' global performance metrics, fit distribution, and poorly fitted
#' individuals.
#'
#' @param x An object of class \code{summary.EvalPPK}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @rdname exeval_ppk
#' @export

print.summary.EvalPPK <- function(x, ...) {

  cat("\n")
  cat("External PK Evaluation Summary\n")
  cat(rep("=", 35), "\n", sep = "")

  # --------------------------------
  # Metadata
  # --------------------------------
  if (!is.null(x$metadata)) {
    cat("\nMetadata\n")
    print(x$metadata, row.names = FALSE)
  }

  # --------------------------------
  # Applied filters
  # --------------------------------
  cat("\nSummary settings\n")

  if (is.null(x$occ_filter)) {
    cat("OCC filter      : All occasions\n")
  } else {
    cat("OCC filter      :", x$occ_filter, "\n")
  }

  cat("By OCC          :", x$by_occ, "\n")
  cat("Poor threshold  :", x$poor_threshold, "%\n")

  # --------------------------------
  # Global metrics
  # --------------------------------
  cat("\nGlobal Metrics\n")
  print(x$global_metrics, row.names = FALSE)

  # --------------------------------
  # Fit distribution
  # --------------------------------
  cat("\nFit Distribution\n")
  print(x$fit_distribution, row.names = FALSE)

  # --------------------------------
  # Poor fit IDs
  # --------------------------------
  if (nrow(x$poor_fit_ids) > 0) {
    cat("\nPoorly Fitted IDs\n")
    print(x$poor_fit_ids, row.names = FALSE)
  } else {
    cat("\nNo poorly fitted IDs found.\n")
  }

  invisible(x)
}



#' Summarize external evaluation results
#'
#' Generates a structured summary of an \code{EvalPPK} object, including
#' global performance metrics, fit quality classification, and identification
#' of poorly fitted individuals.
#'
#' Summary outputs can be generated across all occasions, for a specific
#' occasion, or stratified by occasion.
#'
#' @param object An object of class \code{EvalPPK}.
#'
#' @param occ Optional numeric occasion to summarize.
#' If \code{NULL} (default), all available occasions are included.
#'
#' @param by_occ Logical. If \code{TRUE}, summaries are stratified by
#' occasion (\code{OCC}). Cannot be used together with \code{occ}.
#'
#' @param poor_threshold Numeric threshold defining poor fit based on
#' absolute individual prediction error (\code{|IPE|}). Default is \code{50}.
#'
#' @param top_n Integer. Number of poorly fitted individuals to report.
#' Default is \code{10}.
#'
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.EvalPPK} containing:
#' \describe{
#'   \item{metadata}{Evaluation metadata inherited from the original
#'   \code{EvalPPK} object.}
#'
#'   \item{global_metrics}{Summary performance metrics across all observations
#'   or stratified by occasion.}
#'
#'   \item{fit_distribution}{Distribution of fit quality categories based on
#'   absolute prediction error.}
#'
#'   \item{poor_fit_ids}{Table of individuals exceeding the selected poor-fit
#'   threshold.}
#' }
#'
#' @rdname exeval_ppk
#' @export

summary.EvalPPK <- function(object,
                            occ = NULL,
                            by_occ = TRUE,
                            poor_threshold = 50,
                            top_n = 10,
                            ...) {

  # --------------------------------
  # Input validation
  # --------------------------------
  if (!inherits(object, "EvalPPK")) {
    stop("'object' must be an EvalPPK object.")
  }

  if (!is.null(occ) && by_occ) {
    stop("'occ' and 'by_occ' cannot be used together.")
  }

  # --------------------------------
  # Extract data
  # --------------------------------
  df <- object$metrics$metrics
  metadata <- attr(object, "attributes")

  if (!is.null(occ)) {
    df <- df  |> 
      dplyr::filter(OCC == occ)
  }

  if (nrow(df) == 0) {
    stop("No observations found for selected filters.")
  }

  # --------------------------------
  # Global metrics
  # --------------------------------
  if (by_occ) {

    global_metrics <- df |>
      dplyr::group_by(OCC) |>
      dplyr::summarise(
        rBIAS = mean(IPE, na.rm = TRUE),
        MAIPE = mean(APE, na.rm = TRUE),
        rRMSE = sqrt(mean(RMSE, na.rm = TRUE)) * 100,
        IF20 = mean(abs(IPE) <= 20, na.rm = TRUE) * 100,
        IF30 = mean(abs(IPE) <= 30, na.rm = TRUE) * 100,
        .groups = "drop"
      )

  } else {

    global_metrics <- df |>
      dplyr::summarise(
        rBIAS = mean(IPE, na.rm = TRUE),
        MAIPE = mean(APE, na.rm = TRUE),
        rRMSE = sqrt(mean(RMSE, na.rm = TRUE)) * 100,
        IF20 = mean(abs(IPE) <= 20, na.rm = TRUE) * 100,
        IF30 = mean(abs(IPE) <= 30, na.rm = TRUE) * 100
      )
  }

  # --------------------------------
  # Fit classification
  # --------------------------------
  df_fit <- df |>
    dplyr::mutate(
      Fit_Class = dplyr::case_when(
        abs(IPE) <= 15 ~ "Excellent",
        abs(IPE) <= 30 ~ "Acceptable",
        abs(IPE) <= 50 ~ "Poor",
        TRUE ~ "Very Poor"
      )
    )

  if (by_occ) {

    fit_distribution <- df_fit |>
      dplyr::count(OCC, Fit_Class) |>
      dplyr::group_by(OCC) |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 1)
      ) |>
      dplyr::ungroup()

  } else {

    fit_distribution <- df_fit |>
      dplyr::count(Fit_Class) |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 1)
      )
  }

  # --------------------------------
  # Poor fit IDs
  # --------------------------------
  if (by_occ) {

    poor_fit_ids <- df |>
      dplyr::filter(abs(IPE) >= poor_threshold) |>
      dplyr::group_by(OCC, ID) |>
      dplyr::summarise(
        n_poor = dplyr::n(),
        mean_abs_IPE = round(mean(abs(IPE), na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      dplyr::arrange(
        OCC,
        dplyr::desc(n_poor),
        dplyr::desc(mean_abs_IPE)
      ) |>
      dplyr::group_by(OCC) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::ungroup()

  } else {

    poor_fit_ids <- df |>
      dplyr::filter(abs(IPE) >= poor_threshold) |>
      dplyr::group_by(ID) |>
      dplyr::summarise(
        n_poor = dplyr::n(),
        OCCs = paste(sort(unique(OCC)), collapse = ", "),
        mean_abs_IPE = round(mean(abs(IPE), na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      dplyr::arrange(
        dplyr::desc(n_poor),
        dplyr::desc(mean_abs_IPE)
      ) |>
      dplyr::slice_head(n = top_n)
  }

  # --------------------------------
  # Output
  # --------------------------------
  out <- list(
    metadata = metadata,
    global_metrics = global_metrics,
    fit_distribution = fit_distribution,
    poor_fit_ids = poor_fit_ids,
    occ_filter = occ,
    by_occ = by_occ,
    poor_threshold = poor_threshold
  )

  class(out) <- "summary.EvalPPK"

  return(out)
}




#' S3 plot method for \code{EvalPPK} objects
#'
#' Generates visualization plots for external evaluation results stored in an
#' \code{EvalPPK} object, including prediction error metrics, fit quality
#' distributions, and forecasting performance summaries.
#' @param x An object of class \code{EvalPPK}.
#' 
#' @param type Character string specifying the type of plot to generate.
#' Available options are:
#' \itemize{
#'   \item \code{"bias_barplot"}: bar plot of relative bias
#'   (\code{rBIAS}) with confidence intervals.
#'
#'   \item \code{"bias_pointrange"}: point-range plot of
#'   relative bias (\code{rBIAS}) with confidence intervals.
#'
#'   \item \code{"MAIPE_barplot"}: bar plot of mean absolute individual
#'   prediction error (\code{MAIPE}) by occasion.
#'
#'   \item \code{"bias_boxplot"}: boxplot of individual prediction errors
#'   (\code{IPE}) by occasion.
#'
#'   \item \code{"bias_violin"}: violin plot of individual prediction
#'   errors (\code{IPE}) by occasion.
#'
#'   \item \code{"bias_dotplot"}: jittered dot plot of individual prediction
#'   errors (\code{IPE}) by occasion.
#'
#'   \item \code{"bias_density"}: density plot of individual prediction
#'   errors across occasions.
#'
#'   \item \code{"IF20_plot"}: bar plot of \code{IF20} values with
#'   reference threshold.
#'
#'   \item \code{"IF30_plot"}: bar plot of \code{IF30} values with
#'   reference threshold.
#'
#'   \item \code{"IF_plot"}: combined visualization of both
#'   \code{IF20} and \code{IF30}.
#'
#'   \item \code{"error_plot"}: stacked bar plot showing the proportion
#'   of observations within predefined prediction error categories.
#'
#'   \item \code{"fit_class"}: bar plot showing the distribution of fit
#'   quality categories.
#'
#'   \item \code{"fit_histogram"}: histogram of individual prediction
#'   error values.
#' }
#' 
#' @param occ Optional numeric occasion (\code{OCC}) to filter the plot.
#' If \code{NULL} (default), all available occasions are included.
#' 
#' 
#' @param signed Logical. Only used when
#' \code{type = "fit_histogram"}.
#' If \code{TRUE}, signed individual prediction errors are plotted.
#' If \code{FALSE} (default), absolute individual prediction errors are used.
#'
#' @param ... Additional arguments passed to or from other methods.
#'
#' @details
#' This method provides visualization tools for assessing predictive
#' performance of external model evaluations, including bias, precision,
#' forecasting success, and fit quality classification.
#' 
#' @return A \code{ggplot2} object, except for \code{"IF_plot"}, which
#' returns a combined plot object generated with \pkg{ggpubr}.
#'
#' @import ggplot2 dplyr
#' @importFrom scales brewer_pal
#' @rdname exeval_ppk
#' @export
#'
plot.EvalPPK <- function(x,
                         type = c(
                           "bias_barplot",
                           "bias_pointrange",
                           "MAIPE_barplot",
                           "bias_boxplot",
                           "bias_violin",
                           "bias_dotplot",
                           "bias_density",
                           "IF20_plot",
                           "IF30_plot",
                           "IF_plot",
                           "error_plot",
                           "fit_class",
                           "fit_histogram"
                                        ),
                         occ = NULL, signed = FALSE, ...) {

  type <- match.arg(type)
  mm <- x$metrics
  pp <- NULL


  if (type == 'fit_class') {
    # plot_fit_distribution(x = x,
    #                       occ = occ,
    #                       type = "fit_class",
    #                       signed = signed)
    fit_colors <- c(
      "Excellent" = "paleturquoise",
      "Acceptable" = "darkseagreen",
      "Poor" = "wheat",
      "Very Poor" = "lightcoral"
    )
    
    titu <- NULL
    if(is.null(occ) ) { 
      occ <- unique(mm[[1]]$OCC) 
      titu <- "Fit quality distribution"
    } else { 
      titu <- paste("Fit quality distribution - OCC", occ)
        }
    
    pp <- mm[[1]] |>
      dplyr::filter(OCC %in% occ) |> 
      dplyr::mutate( Abs_IPE = abs(IPE)) |> 
      ggplot2::ggplot(
      ggplot2::aes(x = Fit_Class, fill=Fit_Class)) +
      ggplot2::geom_bar() +
      ggplot2::scale_fill_manual(values = fit_colors, drop = FALSE) +
      ggplot2::labs(
        title = titu,
        x = "Fit Class",
        y = "Number of Observations"
      ) +
      ggplot2::theme_bw()
  }

  if (type == 'fit_histogram') {

    titu <- NULL
    if(is.null(occ) ) { 
      occ <- unique(mm[[1]]$OCC) 
      titu <- "IPE distribution"
    } else { 
      titu <- paste("IPE distribution - OCC", occ)
    }
    
    if (signed) {
      x_var <- "IPE"
      x_lab <- "Individual Prediction Error (%)"
    } else {
      x_var <- "Abs_IPE"
      x_lab <- "Absolute Individual Prediction Error (%)"
    }
    
    pp <- mm[[1]] |>
      dplyr::filter(OCC %in% occ) |> 
      dplyr::mutate( Abs_IPE = abs(IPE)) |> 
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[x_var]]) ) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::labs(
        title = titu, 
        x = x_lab,
        y = "Number of Observations"
      ) +
      ggplot2::theme_bw()
  }


  if (type == 'bias_barplot') {
    pp <- mm[[2]] |>
      mutate(OCC = factor(OCC) ) |>
      ggplot( aes(x =OCC, y = rBIAS, fill = OCC) ) +
      geom_col( ) +
      geom_errorbar(aes(ymin = rBIAS_lower, ymax = rBIAS_upper), width = 0.2) +
      geom_hline(data= data.frame(yy =c(-20, 20)), aes(yintercept= yy), linetype = "dashed", color='firebrick') +
      scale_fill_brewer(palette = 'Dark2')
  }
  else if (type == 'bias_pointrange') {
    pp <- mm[[2]] |>
      mutate(OCC = factor(OCC)) |>
      ggplot(aes(x = OCC, y = rBIAS, ymin = rBIAS_lower, ymax = rBIAS_upper)) +
      geom_errorbar(aes(ymin = rBIAS_lower, ymax = rBIAS_upper, color = OCC), width = 0.2) +
      geom_point(aes(color = OCC), size = 3) +
      geom_hline(yintercept = c(-20, 20), linetype = "dashed", color = 'firebrick', alpha = 0.7) +
      scale_color_brewer(palette = "Dark2") +
      theme_minimal() +
      labs(x = "OCC", y = "rBIAS (%)", color = "OCC") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  else if (type == 'MAIPE_barplot') {
    pp <-   mm[[2]] |>  # rBIAS_boxplot
      mutate(OCC = factor(OCC) ) |>
      ggplot (aes(x=OCC, y=MAIPE, fill=OCC)) + geom_col() +
      geom_hline(data= data.frame(yy =c(30)), aes(yintercept= yy), linetype = "dashed", color='firebrick') +
      scale_fill_brewer(palette = 'Dark2')
  } else if (type == 'bias_boxplot') {
    pp <-   mm[[1]] |>  # rBIAS_boxplot
      mutate(OCC = factor(OCC) ) |>
      ggplot (aes(x=OCC, y=IPE, fill=OCC)) + geom_boxplot() +
      geom_hline(data= data.frame(yy =c(-20, 20)), aes(yintercept= yy), linetype = "dashed", color='firebrick') +
      scale_fill_brewer(palette = 'Dark2')

  } else if (type == 'bias_dotplot') {
    pp <- mm[[1]] |>
      mutate(OCC = factor(OCC)) |>
      ggplot(aes(x = OCC, y = IPE, color = OCC)) +
      geom_jitter(width = 0.2, alpha = 0.6) +
      geom_hline(data = data.frame(yy = c(-20, 20)), aes(yintercept = yy),
                 linetype = "dashed", color = "firebrick") +
      scale_color_brewer(palette = "Dark2") +
      labs(y = "Individual Prediction Error (%)", title = "rBias dotplot")
  } else if (type == 'bias_density') {
    pp <- mm[[1]] |>
      mutate(OCC = factor(OCC)) |>
      ggplot(aes(x = IPE, fill = OCC)) +
      geom_density(alpha = 0.4) +
      geom_vline(xintercept = c(-20, 20), linetype = "dashed", color = "firebrick") +
      scale_fill_brewer(palette = "Dark2") +
      labs(x = "Individual Prediction Error (%)", title = " Distribution of rBias per OCC")
  } else if (type == 'bias_violin') {
    pp <- mm[[1]] |> # rBIAS_violinplot
      mutate(OCC = factor(OCC) ) |>
      ggplot( aes(x=OCC, y=IPE, fill=OCC)) + geom_violin() +
      scale_fill_brewer(palette = 'Dark2')
  } else if (type ==  'IF20_plot') {
    pp <- mm[[2]] |> #  IF20_plot
      mutate(OCC = factor(OCC) ) |>
      ggplot(aes(x=OCC, y=IF20))+
      geom_col( aes(fill=OCC) )+
      geom_hline( aes(yintercept= 35), linetype = "dashed", colour= 'firebrick') +
      scale_fill_brewer(palette = "Dark2")+
      labs(title="IF20- Bayesian Forecasting",y="IF20(%)")+
      theme(plot.title = element_text(size = rel(1), colour = "black")) +
      theme(plot.title = element_text(size = 10, face = "bold"))
  }
  else if (type ==  'IF30_plot') {
    pp <- mm[[2]] |> #  IF20_plot
      mutate(OCC = factor(OCC) ) |>
      ggplot(aes(x=OCC, y=IF30))+
      geom_col( aes(fill=OCC) )+
      geom_hline( aes(yintercept= 50), linetype = "dashed", colour= 'firebrick') +
      scale_fill_brewer(palette = "Dark2")+
      labs(title="IF30- Bayesian Forecasting",y="IF30(%)")+
      theme(plot.title = element_text(size = rel(1), colour = "black")) +
      theme(plot.title = element_text(size = 10, face = "bold"))
  }
  else if(type== "IF_plot") {
    mm[[2]]$dummy1 <- "IF20(%)"
    mm[[2]]$dummy2 <- "IF30(%)"

    plot_resumen_bayes_IF20 <- mm[[2]] |>
      mutate(OCC = factor(OCC) ) |>
      ggplot(aes(x=OCC, y=IF20))+
      geom_col( aes(fill=OCC) )+
      geom_hline(data = data.frame(yy = c(35)), aes(yintercept = yy), linetype = "dashed", color = 'blue')+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.25, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        strip.text = element_text(size=12, face="bold"),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_rect(color = "black", fill = "white"),
        panel.background = element_rect(fill = "white")
      ) + guides(fill = guide_legend(title.position = "top", nrow = 2, ncol = 3)) +
      facet_grid(rows = vars(dummy1)) +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        strip.background = element_rect(fill = "gray80", color = "black"),
        strip.text = element_text(face = "bold", size = 10)
      )

    plot_resumen_bayes_IF30 <- mm[[2]] |>
      mutate(OCC = factor(OCC) ) |>
      ggplot(aes(x=OCC, y=IF30))+
      geom_col( aes(fill=OCC) )+
      geom_hline(data = data.frame(yy = c(50)), aes(yintercept = yy), linetype = "dashed", color = 'blue')+
      theme(
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.25, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        strip.text = element_text(size=12, face="bold"),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_rect(color = "black", fill = "white"),
        panel.background = element_rect(fill = "white")
      ) + guides(fill = guide_legend(title.position = "top", nrow = 2, ncol = 3)) +
      facet_grid(rows = vars(dummy2)) +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        strip.background = element_rect(fill = "gray80", color = "black"),
        strip.text = element_text(face = "bold", size = 10)
      )

    pp= ggpubr::ggarrange(plot_resumen_bayes_IF20,
                          plot_resumen_bayes_IF30,
                          common.legend = TRUE,
                          legend = "right",
                          nrow = 2, ncol = 1,
                          align = "v")
  }

  else if(type== "error_plot") {
    mm_plot <- mm[[1]] |>
      mutate(tramo = case_when(
        abs(IPE) > 50 ~ "50+",
        abs(IPE) > 30 & abs(IPE) <= 50 ~ "30+",
        abs(IPE) > 15 & abs(IPE) <= 30 ~ "15+",
        abs(IPE) <= 15 ~ "<15",
        TRUE ~ "cucu"
      )) |>
      mutate(tramo = factor(tramo, levels = c("50+", "30+", "15+", "<15"))) |>
      count(OCC, tramo) |>
      group_by(OCC) |>
      mutate(prop = n / sum(n)) |>
      ungroup()

    color_error <- c(
      "50+" = "lightcoral",
      "30+" = "wheat",
      "15+" = "darkseagreen",
      "<15" = "paleturquoise"
    )

    # final plot
    pp <- ggplot(mm_plot, aes(x = OCC, y = prop, fill = tramo)) +
      geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
      geom_text(aes(label = sprintf("%.2f", prop)),
                position = position_fill(vjust = 0.5), size = 3,
                fontface="bold") +
      scale_fill_manual(values = color_error, name = "Proportion within IPE bands") +
      scale_y_continuous(limits = c(0,1) ) +
      labs(
        title = "Relative Error Distribution by OCC",
        x = "OCC",
        y = "Proportion"
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.position = "right"
      )
  }

  return(pp)
}








