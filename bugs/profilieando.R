devtools::load_all()
#=======================

?mcode
data(model_tacHAN2011)


m1 <- mcode("conR", model_tacHAN2011, compile = TRUE)


my_model <- mrgsolve::mcode(model_name, model_code)


#=======================
# save(model_tacHAN2011, file = "data/model_tacHAN2011.rda")
# data_tacHAN2011 <- external_data_mapbayr
# save(data_tacHAN2011, file = "data/data_tacHAN2011.rda")
#========================

# profilieando
library(profvis)

# datos -------------
# Example of model evaluation using preDose package
data("data_tacHAN2011")
data("model_tacHAN2011")

dd <- data_tacHAN2011 |> subset(ID < 11)


# todo el wkf ---------------
res <- exeval_ppk(
  model = Han_etal_test,
  data = dd,
  model_name = "Test2",
  evaluation_type = "Progressive",
  assessment = 'Bayesian_forecasting'
)

p.all <- profvis(
  exeval_ppk(
    model_name = "tacrolimus_HAN2011",
    model_code = model_tacHAN2011,
    data = dd,
    evaluation_type = "Progressive",
    assessment = 'Bayesian_forecasting'
  )
)


# profile internal functions ---------------

# Ajustar modelo

profvis(
  {
    run_MAP_estimations(
      model = Han_etal_test,
      model_name = "Han_etal_test",
      tool = "mapbayr",
      data = dd,
      evaluation_type = "Progressive"
    )
  },
  interval = 0.01
) # Cargar dataset desde el paquete


map.est <- run_MAP_estimations(
  model_name = "Test_Model",
  model_code = model_tacHAN2011,
  tool = "mapbayr",
  data = dd,
  evaluation_type = "Progressive"
)

# Update models

p.act <- profvis(
  updt.md.prueba <- actualize_model(a, evaluation_type = "Progressive")
)
updt.md <- actualize_model(map.est, evaluation_type = "Progressive")


# notas:
# evaluation_tipe puede ser distinta en actualize_model y run_MAP_estimations??

# Simulate

sim = run_ind_simulations(updt.md, map.est, assessment = "Bayesian_forecasting")
sim_test = run_ind_simulations(updt.md.prueba, a, assessment = "Bayesian_forecasting")
# notas:
# sale este error: Could not process OCC_4 ID_13: error in evaluating the argument 'object' in selecting a method for function 'update': Zero rows in data after filtering.
# explicar argumentos
# Entiendo que todo este proceso podría ser una sola llamada para el usuario

# Metricas

metrics = metrics_occ(
  simulations = sim,
  assessment = "Bayesian_forecasting",
  tool = "mapbayr"
) # Simulate for every ID in every OCC

?metrics_Plot

metrics_Plot(mm = metrics, type = "IF30_plot")

# notas:
# crear method print para metrics
# revisar cada grafico que se produce

###################################################
res2<- exeval_ppk(model = Han_etal_test,
           model_name = "han",
           data = dd,
           evaluation_type = "Progressive",
           assessment = "Bayesian_forecasting")


metrics_plot(res, type="bias_barplot")

metrics_Plot(res$metrics, type= "bias_barplot")
