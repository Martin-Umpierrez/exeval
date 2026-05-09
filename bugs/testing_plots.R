
tt <- c("bias_barplot", "MAIPE_barplot", "bias_boxplot", "bias_violin",
    "bias_dotplot", "bias_density", "IF20_plot", "IF30_plot")

 metrics_Plot(res$metrics, type = tt[1] )

mm <- res$metrics


# ----------------------------------------------

# Ver el rBIAS o IPE medio, pointrange para media y su intervalo
regions <- data.frame(
  xmin = rep(-Inf,4),
  xmax = rep(Inf,4),
  ymin = c(-Inf, -30, 20, 30),
  ymax = c(-30, -20,  30, Inf),
  region = c("+30","+20","+20","+30")
)

ggplot(mm[[2]], aes( x =OCC, y = rBIAS ) ) +
   #Add background regions first (so they appear behind points)
  geom_rect(data = regions, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
  fill = region), alpha = 0.3, inherit.aes = FALSE) +
  geom_pointrange( aes( ymax=rBIAS_upper, ymin=rBIAS_lower) ) +
  scale_fill_manual(values = c( "+20" = "wheat",
                              "+30" = "Lightcoral")) +
  labs(x='Ocassion', fill='')+
  theme_bw()

# Ver la distribución del IPE por ocasion con raincluod

library(ggdist)
 #ggplot(data, aes(x = group, y = value, fill = group)) +
  ggplot(metriquitas[[1]], aes( x =factor(OCC), y = IPE ) ) +
  # Cloud (density)
  stat_halfeye(adjust = 0.5, width = 0.5, .width = 0,
               justification = -0.5, point_colour = NA) +
  # Rain (individual points)
  geom_point(size = 1.3, alpha = 0.3,
             position = position_jitter(seed = 1, width = 0.1)) +
  # Box plot
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5, color='darkseagreen') +
  geom_hline(data= data.frame(yy =c(-30, 30)), aes(yintercept= yy), linetype = "dashed", color='Lightcoral') +
  theme_bw() +
  theme(legend.position = "none") +
  scale_interval_color_discrete()


# Mirar errores vs predicciones para cada ocasion
mm[[1]] |>
  mutate(error = Ind_Prediction - DV) |>
  ggplot() +
  geom_point( aes(y=error, x=Ind_Prediction)) +
  facet_wrap(~OCC, scales='free')

# Combinado de
mm_plot <- mm[[1]] %>%
  mutate(tramo = case_when(
    abs(IPE) > 30 ~ "30+",        # peor
    abs(IPE) > 20 & abs(IPE) <= 30 ~ "20+", # intermedio-malo
    abs(IPE) > 10 & abs(IPE) <= 20 ~ "10+", # intermedio-bueno
    abs(IPE) <= 10 ~ "<10",       # excelente
    TRUE ~ "cucu"
  )) %>%
  mutate(tramo = factor(tramo, levels = c("30+", "20+", "10+", "<10"))) %>%
  count(OCC, tramo) %>%
  group_by(OCC) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Colores pastel de peor a mejor
colores_error <- c(
  "30+" = "lightcoral",       # peor
  "20+" = "wheat",            # intermedio
  "10+" = "darkseagreen",     # bueno
  "<10" = "paleturquoise"     # excelente
)

# Gráfico final
ggplot(mm_plot, aes(x = OCC, y = prop, fill = tramo)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.2f", prop)),
            position = position_fill(vjust = 0.5), size = 3,
            fontface="bold") +
  scale_fill_manual(values = colores_error, name = "Proportion within IPE bands") +
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

