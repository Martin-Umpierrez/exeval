#' Plot Combined Metrics
#'
#' This function generates different types of metrics plots based on the combined metrics from multiple models.
#'
#' @param cmetrics A dataframe obtained from `combine_metrics()`, containing the combined metrics data.
#' @param type A character string specifying the type of plot to generate. Options are:
#'   \itemize{
#'     \item \code{"bias_barplot"}: Bar plot of relative bias (\code{rBIAS}) with error bars.
#'     \item \code{"MAIPE_barplot"}: Bar plot of MAIPE values.
#'     \item \code{"IF20_plot"}: Bar plot of IF20 values with reference line at 35%.
#'     \item \code{"IF30_plot"}: Bar plot of IF30 values with reference line at 50%.
#'   }
#'
#' @return A `ggplot2` object representing the selected plot.
#' @export
#'
#' @examples
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
#' plot_combined(combined_results, type = 'bias_barplot')
plot_combined <-
function(cmetrics, type = c('bias_barplot',
                                      'MAIPE_barplot',
                                      'IF20_plot',
                                      'IF30_plot')) {
    pplot <- NULL
    if (type == 'bias_barplot') {
      n_occs <- length(unique(cmetrics$OCC))
      pplot <- cmetrics %>%
        mutate(OCC = factor(OCC) ) %>%
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
      pplot <-   cmetrics %>% # rBIAS_boxplot
        mutate(OCC = factor(OCC) ) %>%
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
      pplot <- cmetrics %>%  #  IF20_plot
        mutate(OCC = factor(OCC) ) %>%
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
      pplot <- cmetrics %>%  #  IF20_plot
        mutate(OCC = factor(OCC) ) %>%
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
