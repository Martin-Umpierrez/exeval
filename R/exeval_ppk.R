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
#'     \item "stepwise_updating": Uses only the most recent occasion.
#'     \item "sequential_reference_updating": Uses all data up to a reference occasion.
#'     \item "backward_reference_updating": Uses the most recent occasion relative to a reference.
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
#' data("tacrolimus_pk1_kidney", package = "exeval")
#' data("model_tacHAN2011", package = "exeval")
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
                        evaluation_type = c("sequential_updating", "stepwise_updating","sequential_reference_updating","backward_reference_updating"), ## Como se va a hacer la eval externa
                        method = c("L-BFGS-B", "newuoa"),
                        assessment = c("a_priori","Bayesian_forecasting", "Complete"),
                        verbose=FALSE) {

  if(model %in% exeval_models$Label){
    model_name <- model
    model      <- exeval_models$Model_code[exeval_models$Label == model]
  }

  ## Run estimation, simulation and predicton erro computation in every OCC
  est <- run_MAP_estimations(model, model_name, tool, check_compile,
                             data, num_occ, num_ids, sampling, occ_ref, evaluation_type,
                             method
                             )
  updt <- update_map_models(est, evaluation_type)
  sims <- run_pk_simulations(updt, est, assessment)

  # Compute evaluation metrics
  metrics <- metrics_occ(sims, assessment=assessment,tool=tool )


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




#' print method for EvalPPK class
#'
#' @param x EvalPPK object
#' @param ... additional arguments (not used)
#' @rdname exeval_ppk
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



#' Summarize external PK evaluation results
#'
#' Generates a structured summary of an \code{EvalPPK} object, including
#' metadata, global performance metrics, fit quality distribution,
#' and poorly fitted IDs.
#'
#' @param object An object of class \code{EvalPPK}.
#' @param occ Optional numeric occasion to summarize.
#' If \code{NULL} (default), all occasions are included.
#' @param by_occ Logical. If \code{TRUE}, returns summaries stratified by OCC.
#' Cannot be used together with \code{occ}.
#' @param poor_threshold Numeric threshold defining poor fit based on
#' absolute IPE. Default is 50.
#' @param top_n Number of poorly fitted IDs to return. Default is 10.
#' @param ... Additional arguments (not used).
#' @rdname exeval_ppk
#'
#' @return An object of class \code{summary.EvalPPK}.
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




#' plot.EvalPPK
#'
#' Generate Different Type of Metrics Plots
#'
#' This function creates various types of metrics plots for evaluating popPK Models , such as rBias, MAIPE, and IF20/IF30 values.
#'
#' @param x A list containing data frames with the required  metrics. Typically, `mm[[1]]` and `mm[[2]]` contain relevant data. Values comes from results from [metrics_occ()] function
#' @param type A character string specifying the type of plot to generate. Options are:
#'   \itemize{
#'     \item \code{"bias_barplot"}: Bar plot of relative bias (\code{rBIAS}) with error bars.
#'     \item \code{"bias_pointrange"}: pointrange for rBIAS.
#'     \item \code{"MAIPE_barplot"}: Bar plot of MAIPE values.
#'     \item \code{"bias_boxplot"}: Box plot of IPE values.
#'     \item \code{"bias_dotplot"}: Dotplot of rBIAS values. Variability on individual bias
#'     \item \code{"bias_density"}: Density Plot for rBias throughout occasions .
#'     \item \code{"bias_violin"}: Violin plot of IPE values.
#'     \item \code{"IF20_plot"}: Bar plot of IF20 values with reference line at 35%.
#'     \item \code{"IF30_plot"}: Bar plot of IF30 values with reference line at 50%.
#'     \item \code{"IF_plot"}: Combine both IF20 and IF30 plots.
#'     \item \code{"error_plot"}: Stacked bar plot showing the proportion of prediction errors within predefined IPE bands.
#'     \item \code{"fit_class"}: bar plot showing observations within fit quality categories.
#'     \item \code{"histogram"}: histogram of individual prediction error values.
#'   }
#' @param occ Optional numeric occasion to filter.
#'   If `NULL` (default), all occasions are included.
#' @param signed Logical. Only used when `type = "histogram"`.
#'   If `TRUE`, the histogram is generated using signed IPE values.
#'   If `FALSE` (default), absolute IPE values are used.
#' @param ... Additional arguments
#'
#' @return A ggplot object corresponding to the selected plot type.
#'
#' @details
#' The function utilizes ggplot2 for visualization and `scale_fill_brewer(palette = "Dark2")` for consistent color schemes.
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
    
    pp <- mm[[1]] |>
      dplyr::mutate( Abs_IPE = abs(IPE)) |> 
      dplyr::mutate(
        Fit_Class = dplyr::case_when(
          Abs_IPE <= 15 ~ "Excellent",
          Abs_IPE <= 30 ~ "Acceptable",
          Abs_IPE <= 50 ~ "Poor",
          Abs_IPE > 50 ~ "Very Poor"
        ),
        Fit_Class = factor(
          Fit_Class,
          levels = c("Very Poor", "Poor","Acceptable","Excellent")
        )
      ) |> 
    ggplot2::ggplot(
      ggplot2::aes(x = Fit_Class, fill=Fit_Class)) +
      ggplot2::geom_bar() +
      ggplot2::scale_fill_manual(values = fit_colors, drop = FALSE) +
      ggplot2::labs(
        title = ifelse(
          is.null(occ),
          "Fit quality distribution",
          paste("Fit quality distribution - OCC", occ)
        ),
        x = "Fit Class",
        y = "Number of Observations"
      ) +
      ggplot2::theme_bw()
  }

  if (type == 'fit_histogram') {
    # plot_fit_distribution(x = x,
    #                       occ = occ,
    #                       type = "fit_histogram",
    #                       signed = signed)
    
    if (signed) {
      x_var <- "IPE"
      x_lab <- "Individual Prediction Error (%)"
    } else {
      x_var <- "Abs_IPE"
      x_lab <- "Absolute Individual Prediction Error (%)"
    }
    
    pp <- mm[[1]] |>
      dplyr::mutate( Abs_IPE = abs(IPE)) |> 
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[x_var]]) ) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::labs(
        title = ifelse(
          is.null(occ),
          "IPE distribution",
          paste("IPE distribution - OCC", occ)
        ),
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
      labs(title="IF30- Bayesian Forecasting",y="IF20(%)")+
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
      "20+" = "wheat",
      "10+" = "darkseagreen",
      "<10" = "paleturquoise"
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








