
analisis_sa<-function(aj){
# 1. Comprobacion pretratamiento ------------------------------------------


saj=summary(aj)

test_log=saj$preprocessing$log
prepro_model=saj$preprocessing$sarima$sarima_orders
prepro_out=saj$preprocessing$xregs
prepro_para=saj$preprocessing$sarima$coef_table

# Media, autorcorrelacion, normalidad y linearidad de los residuos --------------------------------------
prepro_residualtest=saj[["diagnostics"]][["preprocessing"]]

residuos=aj[["result"]][["preprocessing"]][["estimation"]][["res"]]
# 2.1. Series linearizadas: serie original eliminado atipicos y cal ----------------
# Series obtenidas de forma puramente estocastica
y_lin=aj$result$decomposition$stochastics

# intengo graficar la serie original y la linearizada??



# 2.2 Series finales ajustadas --------------------------------------------

# A diferencia de la parte estocástica estas se A diferencia de la parte estocástica
# estas series ya han "re-ensamblado" y repartido los efectos deterministas:
# LS va a la tendencia
# y los AO+TC van al irregular 

y_or=saj$final$series
y_aj=saj$final$sa
y_t=saj$final$trend
y_s=saj$final$seas
y_i=saj$final$irr

# grafico series superpuestas por años
y_ts <- y_or  # si ya es ts

serie_mensual <- data.frame(
  anio = floor(time(y_ts)),
  mes  = cycle(y_ts),              # 1..12
  valor_mensual = as.numeric(y_ts)
)


# serie_mensual=serie_mensual %>% dplyr::filter(anio>=2010&anio!=2020)

serie_mensual_idx <- serie_mensual %>%
  group_by(anio) %>%
  mutate(indice = 100 * valor_mensual / mean(valor_mensual, na.rm = TRUE)) %>%
  ungroup()

#promedio de todos los anos

media_estacional <- serie_mensual_idx %>%
  group_by(mes) %>%
  summarise(indice_medio = mean(indice, na.rm = TRUE), .groups = "drop")


# Periodograma ------------------------------------------------------------


sp_or <- spectrum(y_or, plot = FALSE, taper = 0)
sp_aj <- spectrum(y_aj, plot = FALSE, taper = 0)

# Frecuencias "objetivo" estacionales (ciclos/año). En mensual: 1..6
targets <- 1:6

# Coger, para cada target, el punto de spectrum más cercano (porque no siempre cae exacto)
nearest_spec <- function(sp, targets) {
  sapply(targets, function(tt) {
    idx <- which.min(abs(sp$freq - tt))
    sp$spec[idx]
  })
}

spec_or <- nearest_spec(sp_or, targets)
spec_aj <- nearest_spec(sp_aj, targets)

tabla_perio <- data.frame(
  freq = targets,
  periodo_meses = 12 / targets,
  spec_original = spec_or,
  spec_ajustada = spec_aj
) %>%
  mutate(
    ratio_aj_or = spec_ajustada / spec_original
  ) %>%
  mutate(
    freq = round(freq, 3),
    periodo_meses = round(periodo_meses, 2),
    spec_original = round(spec_original, 2),
    spec_ajustada = round(spec_ajustada, 2),
    ratio_aj_or = round(ratio_aj_or, 3)
  )


modelo_decom=saj$final$multiplicative

# 3. Comprobación estacionalidad residual ---------------------------------

# Test sobre los residuos para asegurar que no queda estacionalidad sin capturar. [2]
sa_test=saj[["diagnostics"]][["residual_tests"]]

# Test combinado de estacionalidad en la serie completa.
# Es crucial que el resultado para estacionalidad estable sea positivo en la serie original 
# y negativo en la serie ya ajustada. (NO LO ENCUENTRO)

# 4. Calidad de la descomposición -----------------------------------------
# Criterios: Descomposicion de la varianza 

# a) Minimización de la varianza en las señales (Tendencia y Estacional)
# b) Maximización de la varianza en el componente Irregular
# c) Others: se refiere al componente transitorio:
#    Si Others es grande revisar si la tendencia (t) es lo suficientemente
#  suave para tu análisis; si la serie ajustada (sa) te parece demasiado 
#  errática, es probable que la varianza de "others" esté ensuciando 
#  la señal. Posible solución: forzar modelo más parsimonioso: líneas aéreas
var_exp=saj[["diagnostics"]][["variance_decomposition"]]


return(list(saj=saj, #resumen
             test_log=test_log,
             prepro_model=prepro_model,
             prepro_out=prepro_out,
             prepro_para=prepro_para,
             
             prepro_residualtest=prepro_residualtest,
             residuos=residuos,
       y_lin=y_lin,
       y_or=y_or,
       y_aj=y_aj,
       y_t=y_t,
       y_s=y_s,
       y_i=y_i,
       
       sa_test=sa_test,
       var_exp=var_exp,
       
       y_or_superpuestos=serie_mensual_idx,
       media_estacional=media_estacional,
       tabla_perio=tabla_perio
       
       ))
}


