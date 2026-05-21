
# Ejemplo de uso del paquete 

# como quedaron los modelos ? 
data("exeval_models")
exeval_models |> str()

# cargamos datos usando la label de modelos internos
data("tacrolimus_pk1_kidney", package = "exeval")
dd <- tacrolimus_pk1_kidney |> subset(ID < 6)

# "TAC_Han2011" is the label in exeval_models dataset, 
# which is used to specify the model in exeval_ppk function.

res <- exeval_ppk(model="TAC_Han2011",
                  data = dd,
                  evaluation_type= "sequential_updating",
                  assessment='Bayesian_forecasting' )


# testing methods
res 
summary(res) # tira warnings... format_tbl(), creo que son de screen_fit

res$metrics$metrics

# testing pltos
plot(res, type="bias_barplot")
plot(res, type="bias_pointrange")
plot(res, type="MAIPE_barplot")
plot(res, type="bias_boxplot")
plot(res, type="bias_dotplot")
plot(res, type="bias_density")
plot(res, type="bias_violin")
plot(res, type="IF20_plot")
plot(res, type="IF30_plot")
plot(res, type="IF_plot")
plot(res, type="error_plot")
plot(res, type="fit_class", occ=2)
plot(res, type="fit_histogram")



screen_fit(res, occ=2)



# combine_metrics, combine_metrics_plot 
generate_fake_metrics <- function(n_occasions = 3) {
  data.frame(
OCC = rep(1:n_occasions),  # Simula varias ocasiones
rBIAS = rnorm(n_occasions, mean = 0, sd = 10),
rBIAS_lower = rnorm(n_occasions, mean = -5, sd = 5),
rBIAS_upper = rnorm(n_occasions, mean = 5, sd = 5),
MAIPE = runif(n_occasions, min = 10, max = 50),
IF20 = runif(n_occasions, min = 20, max = 80),
IF30 = runif(n_occasions, min = 30, max = 90)
)
}

simulation1 <- list(metrics_means = generate_fake_metrics())
simulation2 <- list(metrics_means = generate_fake_metrics())

# List of "models"
models_list <- list(
list(model_name = "Test_Model1", metrics_list = simulation1),
list(model_name = "Test_Model2", metrics_list = simulation2)
)

combined_results <- combine_metrics(models_list)

# panchoooooooo
combined_results$topmodelspd

combine_metric_plot(combined_results, type = 'bias_barplot')
combine_metric_plot(combined_results, type = 'MAIPE_barplot')
combine_metric_plot(combined_results, type = 'IF20_plot')
combine_metric_plot(combined_results, type = 'IF30_plot')


