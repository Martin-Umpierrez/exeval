#' metrics_Plot
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
#'   }
#'
#' @return A ggplot object corresponding to the selected plot type.
#'
#' @details
#' The function utilizes ggplot2 for visualization and `scale_fill_brewer(palette = "Dark2")` for consistent color schemes.
#'
#' @import ggplot2 dplyr
#' @importFrom scales brewer_pal
#' @method metrics_plot EvalMetricsPPK
#' @param ... Additional arguments
#' @export
metrics_plot.EvalMetricsPPK <- function(x,
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
                                          "error_plot"
                                        ),
                                        ...) {

  type <- match.arg(type)
  mm <- x

  pp <- NULL

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
    mm_plot <- mm[[1]] %>%
      mutate(tramo = case_when(
        abs(IPE) > 30 ~ "30+",
        abs(IPE) > 20 & abs(IPE) <= 30 ~ "20+",
        abs(IPE) > 10 & abs(IPE) <= 20 ~ "10+",
        abs(IPE) <= 10 ~ "<10",
        TRUE ~ "cucu"
      )) %>%
      mutate(tramo = factor(tramo, levels = c("30+", "20+", "10+", "<10"))) %>%
      count(OCC, tramo) %>%
      group_by(OCC) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()

    color_error <- c(
      "30+" = "lightcoral",
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
