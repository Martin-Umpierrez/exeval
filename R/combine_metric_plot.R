#' Plot combined model performance metrics
#'
#' Generates comparative visualizations of evaluation metrics across multiple
#' externally evaluated models combined with [combine_metrics()].
#'
#' This function is intended for model comparison workflows, allowing visual
#' inspection of predictive performance across evaluation occasions..
#'
#' @param cmetrics Named list returned by [combine_metrics()], containing
#' combined model performance metrics. 
#' 
#' @param type Character string specifying the plot type to generate.
#' Available options are:
#' \itemize{
#'   \item \code{"bias_barplot"}: bar plot of relative bias
#'   (\code{rBIAS}) with confidence intervals.
#'
#'   \item \code{"MAIPE_barplot"}: bar plot of mean absolute individual
#'   prediction error (\code{MAIPE}).
#'
#'   \item \code{"IF20_plot"}: bar plot of \code{IF20} values with
#'   reference threshold.
#'
#'   \item \code{"IF30_plot"}: bar plot of \code{IF30} values with
#'   reference threshold.
#' }
#' 
#' @details
#' Metrics are displayed separately for each evaluation occasion
#' (\code{OCC}) using faceted plots, enabling direct visual comparison
#' between candidate models.
#'
#' @return A \code{ggplot2} object.
#' 
#' @seealso [combine_metrics()], [plot.EvalPPK()]
#'
#' @examples
#' \dontrun{
#' #' set.seed(123)  # Para reproducibilidad
#' generate_fake_metrics <- function(n_occasions = 3) {
#' data.frame(
#' OCC = rep(1:n_occasions),  # Simula varias ocasiones
#' rBIAS = rnorm(n_occasions, mean = 0, sd = 10),
#' rBIAS_lower = rnorm(n_occasions, mean = -5, sd = 5),
#' rBIAS_upper = rnorm(n_occasions, mean = 5, sd = 5),
#' MAIPE = runif(n_occasions, min = 10, max = 50),
#' IF20 = runif(n_occasions, min = 20, max = 80),
#' IF30 = runif(n_occasions, min = 30, max = 90)
#' )
#' }
#' # Save Results of metrics
#' simulation1 <- list(metrics_means = generate_fake_metrics())
#' simulation2 <- list(metrics_means = generate_fake_metrics())
#' # List of models
#' models_list <- list(
#' list(model_name = "Test_Model1", metrics_list = simulation1),
#' list(model_name = "Test_Model2", metrics_list = simulation2)
#' )
#'combined_results <- combine_metrics(models_list)
#' combined_results <- combine_metrics(models_list)
#' combine_metric_plot(combined_results, type = 'bias_barplot')
#' }
#' 
#' @export

combine_metric_plot <-function(cmetrics,
                               type = c('bias_barplot',
                                      'MAIPE_barplot',
                                      'IF20_plot',
                                      'IF30_plot')) {
    pplot <- NULL
    cmetrics <- cmetrics$cmetrics
    if (type == 'bias_barplot') {
      n_occs <- length(unique(cmetrics$OCC))
      pplot <- cmetrics |>
        mutate(OCC = factor(OCC) ) |>
        ggplot(aes(x = Model, y = rBIAS, fill = Model)) +
      geom_col() +
        geom_errorbar(aes(ymin = rBIAS_lower, ymax = rBIAS_upper), width = 0.2) +
        geom_hline(data = data.frame(yy = c(-20, 20)), aes(yintercept = yy), linetype = "dashed", color = 'firebrick') +
        scale_fill_brewer(palette = "Dark2") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13)) +
        theme(axis.text.y = element_text(size = 13)) +
        guides(fill = guide_legend(title.position = "top", nrow = 8, ncol = 3)) +
        labs(y = "rBIAS(%)") +
        theme(legend.key.size = unit(0.25, "cm"), # alto de cuadrados de referencia
              legend.key.width = unit(0.4, "cm"), # ancho de cuadrados de referencia
              legend.position = "right", # ubicacion de leyenda
              legend.direction = "horizontal", # dirección de la leyenda
              legend.title = element_text(size = 13, face = "bold"), # tamaño de titulo de leyenda
              legend.text = element_text(size = 12), # tamaño de texto de leyenda
              axis.title.y = element_text(size = 13),
              strip.text = element_text(size = 13, face = "bold")) + # tamaño y estilo del texto del encabezado
        facet_wrap(~OCC, ncol = n_occs, labeller = labeller(OCC = function(x) paste0("OCC ", x)))
    } else if (type == 'MAIPE_barplot') {
      n_occs <- length(unique(cmetrics$OCC))
      pplot <-   cmetrics |> # rBIAS_boxplot
        mutate(OCC = factor(OCC) ) |>
        ggplot (aes(x=Model, y=MAIPE, fill=Model)) +
        geom_col() +
        geom_hline(data = data.frame(yy = c(30)), aes(yintercept = yy), linetype = "dashed", color = 'firebrick') +
        scale_fill_brewer(palette = "Dark2") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13)) +
        theme(axis.text.y = element_text(size = 13)) +
        guides(fill = guide_legend(title.position = "top", nrow = 8, ncol = 3)) +
        labs(y = "MAIPE(%)") +
        theme(legend.key.size = unit(0.25, "cm"), # alto de cuadrados de referencia
              legend.key.width = unit(0.4, "cm"), # ancho de cuadrados de referencia
              legend.position = "right", # ubicacion de leyenda
              legend.direction = "horizontal", # dirección de la leyenda
              legend.title = element_text(size = 13, face = "bold"), # tamaño de titulo de leyenda
              legend.text = element_text(size = 12), # tamaño de texto de leyenda
              axis.title.y = element_text(size = 13),
              strip.text = element_text(size = 13, face = "bold")) + # tamaño y estilo del texto del encabezado
        facet_wrap(~OCC, ncol = n_occs, labeller = labeller(OCC = function(x) paste0("OCC ", x)))
    }
    else if (type ==  'IF30_plot') {
      n_occs <- length(unique(cmetrics$OCC))
      pplot <- cmetrics |>  #  IF20_plot
        mutate(OCC = factor(OCC) ) |>
        ggplot(aes(x=Model, y=IF30)) +
        geom_col(aes(fill=Model) ) +
        geom_hline( aes(yintercept= 50), linetype = "dashed", colour= 'firebrick') +
        scale_fill_brewer(palette = "Dark2")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13)) +
        theme(axis.text.y = element_text(size = 13)) +
        guides(fill = guide_legend(title.position = "top", nrow = 8, ncol = 3)) +
        labs(title="IF30- Bayesian Forecasting", y="IF30(%)")+
        theme(legend.key.size = unit(0.25, "cm"), # alto de cuadrados de referencia
              legend.key.width = unit(0.4, "cm"), # ancho de cuadrados de referencia
              legend.position = "right", # ubicacion de leyenda
              legend.direction = "horizontal", # dirección de la leyenda
              legend.title = element_text(size = 13, face = "bold"), # tamaño de titulo de leyenda
              legend.text = element_text(size = 12), # tamaño de texto de leyenda
              axis.title.y = element_text(size = 13),
              strip.text = element_text(size = 13, face = "bold")) + # tamaño y estilo del texto del encabezado
        facet_wrap(~OCC, ncol=n_occs, labeller = labeller(OCC = function(x) paste0("OCC ", x)))
    }

    else if (type ==  'IF20_plot') {
      n_occs <- length(unique(cmetrics$OCC))
      pplot <- cmetrics |>  #  IF20_plot
        mutate(OCC = factor(OCC) ) |>
        ggplot(aes(x=Model, y=IF30))+
        geom_col( aes(fill=Model) )+
        geom_hline( aes(yintercept= 35), linetype = "dashed", colour= 'firebrick') +
        scale_fill_brewer(palette = "Dark2")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13)) +
        theme(axis.text.y = element_text(size = 13)) +
        guides(fill = guide_legend(title.position = "top", nrow = 8, ncol = 3)) +
        labs(title="IF20- Bayesian Forecasting",y="IF20(%)")+
        theme(legend.key.size = unit(0.25, "cm"), # alto de cuadrados de referencia
              legend.key.width = unit(0.4, "cm"), # ancho de cuadrados de referencia
              legend.position = "right", # ubicacion de leyenda
              legend.direction = "horizontal", # dirección de la leyenda
              legend.title = element_text(size = 13, face = "bold"), # tamaño de titulo de leyenda
              legend.text = element_text(size = 12), # tamaño de texto de leyenda
              axis.title.y = element_text(size = 13),
              strip.text = element_text(size = 13, face = "bold")) + # tamaño y estilo del texto del encabezado
        facet_wrap(~OCC, ncol = n_occs, labeller = labeller(OCC = function(x) paste0("OCC ", x)))
    }

    return(pplot)
  }
