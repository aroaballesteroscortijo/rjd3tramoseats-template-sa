Sys.setenv(JAVA_HOME = "C:/JDemetra3/nbdemetra/jdk-21.0.6+7-jre")

rm(list = ls())

##### Cargar los paquetes necesarios ####

pacman::p_load(
  tidyverse, lubridate, forecast,
  tidyr, readxl, zoo, tsbox, dlm,
  openxlsx, purrr, seasonal, rjd3toolkit,
  rjd3workspace, rjd3tramoseats, rjd3nowcasting, openxlsx,
  'shiny','dygraphs'
)


# Rutas y ficheros --------------------------------------------------------


# ruta="Z:/DIVISION_ANALISIS_ECONOMICO/01_Seguimiento/01_Macro/Modelos/02_METCAP/2_MODELIZACION/4_TD/Análisis/pruebas/SA_rj3dem"

ruta <- dirname(rstudioapi::getActiveDocumentContext()$path)  # sin "/" al final

file_entrada <- file.path(ruta, "Datos_entrada", "Datos_entrada_indicadores_tot _sa.xlsx")
ruta_modelos <- file.path(ruta, "Datos_entrada", "Spec_template.xlsm")
out_xlxs     <- file.path(ruta, "Datos_salida",  "resumen_sa_CCAA.xlsx")
out_xlxs_ind <- file.path(ruta, "Datos_salida",  "resumen_sa_indicador.xlsx")
##### Funciones auxiliares ####
source(file.path(ruta, "Funciones_R", "leer_modelos.R"))
source(file.path(ruta, "Funciones_R", "Analisis_SA_function.R"))
source(file.path(ruta, "Funciones_R", "SA_procedure.R"))



# ---- Helpers para Excel con una hoja por CCAA ----

ALPHA_FAIL <- 0.1
RATIO_FAIL <- 0.5

norm_test_name <- function(x) tolower(gsub("[^a-z0-9]", "", x))

tests_to_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x) || is.matrix(x)) x <- list(Unico = x)
  if (!is.list(x)) return(NULL)
  x <- x[!sapply(x, is.null)]
  if (length(x) == 0) return(NULL)
  
  out <- dplyr::bind_rows(
    lapply(names(x), function(nm) {
      tb <- x[[nm]]
      if (is.matrix(tb)) tb <- as.data.frame(tb)
      if (!is.data.frame(tb)) return(NULL)
      tb <- tibble::rownames_to_column(tb, var = "test")
      tb <- tb %>%
        dplyr::mutate(test = sub("\\.{2,3}[0-9]+$", "", test))
      tb
    })
  )
  
  if ("Description" %in% names(out)) out <- dplyr::select(out, -Description)
  out
}

tests_flat <- function(x) {
  df <- tests_to_df(x)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!("P.value" %in% names(df))) return(NULL)
  
  p <- df[["P.value"]]
  if (is.character(p)) p <- gsub(",", ".", p, fixed = TRUE)
  p <- suppressWarnings(as.numeric(p))
  
  out <- df
  out$P.value <- p
  out$test <- as.character(out$test)
  out <- out[!is.na(out$P.value), c("test", "P.value")]
  if (nrow(out) == 0) return(NULL)
  out
}

summarise_residuos <- function(prepro_residualtest, alpha = ALPHA_FAIL) {
  df <- tests_flat(prepro_residualtest)
  if (is.null(df)) {
    return(list(
      falla_residuos_lb = NA,
      falla_normalidad = NA,
      test_residuos_lb_no_ok = "",
      test_normalidad_no_ok = ""
    ))
  }
  
  df$test_norm <- norm_test_name(df$test)
  
  is_lb     <- grepl("^lb$|ljungbox|ljung|box", df$test_norm)
  is_seaslb <- grepl("seaslb|seasonallb|seasljung|seasonalljung", df$test_norm)
  
  is_lb2  <- grepl("^lb2$|lb2", df$test_norm)
  is_kurt <- grepl("kurtosis|kurt", df$test_norm)
  is_dh   <- grepl("doornik|hansen|doornikhansen|dh", df$test_norm)
  
  bad_lb_group   <- df[df$P.value < alpha & (is_lb | is_seaslb), , drop = FALSE]
  bad_norm_group <- df[df$P.value < alpha & (is_lb | is_lb2 | is_kurt | is_dh), , drop = FALSE]
  
  list(
    falla_residuos_lb = nrow(bad_lb_group) > 0,
    falla_normalidad  = nrow(bad_norm_group) > 0,
    test_residuos_lb_no_ok = if (nrow(bad_lb_group) > 0)
      paste0(bad_lb_group$test, " (p=", signif(bad_lb_group$P.value, 3), ")", collapse = ", ")
    else "",
    test_normalidad_no_ok = if (nrow(bad_norm_group) > 0)
      paste0(bad_norm_group$test, " (p=", signif(bad_norm_group$P.value, 3), ")", collapse = ", ")
    else ""
  )
}

summarise_estacionalidad <- function(sa_test, alpha = ALPHA_FAIL) {
  df <- tests_flat(sa_test)
  if (is.null(df)) return(list(test_estacionalidad_no_ok = "", falla_estacionalidad = NA))
  bad <- df[df$P.value < alpha, , drop = FALSE]
  list(
    falla_estacionalidad = nrow(bad) > 0,
    test_estacionalidad_no_ok = if (nrow(bad) > 0)
      paste0(bad$test, " (p=", signif(bad$P.value, 3), ")", collapse = ", ")
    else ""
  )
}

summarise_periodogram <- function(tabla_perio, ratio_thr = RATIO_FAIL) {
  if (is.null(tabla_perio) || !is.data.frame(tabla_perio) || nrow(tabla_perio) == 0) {
    return(list(fail = NA, max_ratio = NA, n_bad = NA))
  }
  if (!("ratio_aj_or" %in% names(tabla_perio))) {
    return(list(fail = NA, max_ratio = NA, n_bad = NA))
  }
  r <- tabla_perio$ratio_aj_or
  r <- r[is.finite(r)]
  if (length(r) == 0) return(list(fail = NA, max_ratio = NA, n_bad = NA))
  
  list(
    fail      = any(r > ratio_thr, na.rm = TRUE),
    max_ratio = max(r, na.rm = TRUE),
    n_bad     = sum(r > ratio_thr, na.rm = TRUE)
  )
}

# ---- Extraer info de modelo y outliers ----

extract_model_txt <- function(node) {
  # SARIMA
  ord <- tryCatch(node$saj$preprocessing$sarima$sarima_orders, error = function(e) NULL)
  sar <- if (!is.null(ord)) {
    sprintf("SARIMA (%s,%s,%s)(%s,%s,%s)", ord$p, ord$d, ord$q, ord$bp, ord$bd, ord$bq)
  } else {
    "SARIMA NA"
  }
  
  lg <- tryCatch(isTRUE(node$saj$preprocessing$log), error = function(e) NA)
  ml <- tryCatch(isTRUE(node$saj$final$multiplicative), error = function(e) NA)
  
  paste0(
    sar,
    " | log=", if (isTRUE(lg)) "Sí" else if (isFALSE(lg)) "No" else "NA",
    " | mult=", if (isTRUE(ml)) "Sí" else if (isFALSE(ml)) "No" else "NA"
  )
}

extract_outliers_txt <- function(node) {
  ol <- tryCatch(node$prepro_out, error = function(e) NULL)
  if (is.null(ol) || !is.data.frame(ol) || nrow(ol) == 0) return("")
  
  # si los IDs están en rownames, úsalo
  ids <- rownames(ol)
  ids <- ids[!is.na(ids) & nzchar(ids)]
  
  # si no hay rownames útiles, intenta construir algo con columnas típicas
  if (length(ids) == 0) {
    nm <- names(ol)
    
    # intenta detectar columnas tipo/date
    type_col <- nm[tolower(nm) %in% c("type","outlier.type","outliertype")]
    date_col <- nm[tolower(nm) %in% c("date","time","period")]
    
    if (length(type_col) > 0 && length(date_col) > 0) {
      ids <- paste0(as.character(ol[[type_col[1]]]), "@", as.character(ol[[date_col[1]]]))
    } else {
      # fallback: primera columna
      ids <- as.character(ol[[nm[1]]])
    }
    
    ids <- ids[!is.na(ids) & nzchar(ids)]
  }
  
  paste(unique(ids), collapse = ", ")
}

# Salida resumen por ccaa -------------------------------------------------



make_resumen_ccaa <- function(salida_modelos, ccaa) {
  lista_ind <- salida_modelos[[ccaa]]$resumen_series_des
  ind_names <- names(lista_ind)
  
  rows <- lapply(ind_names, function(ind) {
    node <- lista_ind[[ind]]
    
   
    
    res_sum <- summarise_residuos(node$prepro_residualtest, alpha = ALPHA_FAIL)
    sea_sum <- summarise_estacionalidad(node$sa_test, alpha = ALPHA_FAIL)
    per_sum <- summarise_periodogram(node$tabla_perio, ratio_thr = RATIO_FAIL)
    
    modelo_txt <- extract_model_txt(node)
    outliers_txt <- extract_outliers_txt(node)
    
    
    data.frame(
      indicador = ind,
      
      falla_residuos_lb = res_sum$falla_residuos_lb,
      test_residuos_lb_no_ok = res_sum$test_residuos_lb_no_ok,
      falla_normalidad = res_sum$falla_normalidad,
      test_normalidad_no_ok = res_sum$test_normalidad_no_ok,
      
      falla_estacionalidad = sea_sum$falla_estacionalidad,
      test_estacionalidad_no_ok = sea_sum$test_estacionalidad_no_ok,
      
      falla_periodograma = per_sum$fail,
      max_ratio_aj_or = per_sum$max_ratio,
      n_picos_ratio_mala = per_sum$n_bad,
      
      modelo = modelo_txt,
      outliers = outliers_txt,
      
      
      stringsAsFactors = FALSE
    )
  })
  
  df <- dplyr::bind_rows(rows) %>%
    dplyr::mutate(
      falla_residuos_lb     = as.logical(falla_residuos_lb),
      falla_estacionalidad  = as.logical(falla_estacionalidad),
      falla_periodograma    = as.logical(falla_periodograma),
      falla_normalidad      = as.logical(falla_normalidad)
    ) %>%
    dplyr::mutate(
      n_principales =
        as.integer(dplyr::coalesce(falla_residuos_lb, FALSE)) +
        as.integer(dplyr::coalesce(falla_estacionalidad, FALSE)),
      n_total_fallos =
        as.integer(dplyr::coalesce(falla_residuos_lb, FALSE)) +
        as.integer(dplyr::coalesce(falla_estacionalidad, FALSE)) +
        as.integer(dplyr::coalesce(falla_periodograma, FALSE)) +
        as.integer(dplyr::coalesce(falla_normalidad, FALSE))
    ) %>%
    # dplyr::filter(n_total_fallos > 0) %>%
    dplyr::arrange(
      dplyr::desc(n_principales),
      dplyr::desc(dplyr::coalesce(falla_periodograma, FALSE)),
      dplyr::desc(dplyr::coalesce(falla_normalidad, FALSE)),
      dplyr::desc(dplyr::coalesce(max_ratio_aj_or, -Inf))
    ) %>%
    dplyr::select(-n_principales, -n_total_fallos)
  
  df
}

sanitize_sheet_name <- function(x) {
  x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
  x <- substr(x, 1, 31)
  if (nchar(x) == 0) x <- "Hoja"
  x
}


# --------- Salida resumen "por indicador" (1 hoja por indicador) ---------

make_resumen_indicador <- function(salida_modelos, indicador) {
  ccaa_names <- names(salida_modelos)
  
  rows <- lapply(ccaa_names, function(ccaa) {
    lista_ind <- salida_modelos[[ccaa]]$resumen_series_des
    if (is.null(lista_ind) || !(indicador %in% names(lista_ind))) return(NULL)
    
    node <- lista_ind[[indicador]]
    
    res_sum <- summarise_residuos(node$prepro_residualtest, alpha = ALPHA_FAIL)
    sea_sum <- summarise_estacionalidad(node$sa_test, alpha = ALPHA_FAIL)
    per_sum <- summarise_periodogram(node$tabla_perio, ratio_thr = RATIO_FAIL)
    
    modelo_txt   <- extract_model_txt(node)
    outliers_txt <- extract_outliers_txt(node)
    
    data.frame(
      ccaa = ccaa,
      
      falla_residuos_lb = as.logical(res_sum$falla_residuos_lb),
      test_residuos_lb_no_ok = res_sum$test_residuos_lb_no_ok,
      
      falla_estacionalidad = as.logical(sea_sum$falla_estacionalidad),
      test_estacionalidad_no_ok = sea_sum$test_estacionalidad_no_ok,
      
      falla_periodograma = as.logical(per_sum$fail),
      max_ratio_aj_or = per_sum$max_ratio,
      n_picos_ratio_mala = per_sum$n_bad,
      
      falla_normalidad = as.logical(res_sum$falla_normalidad),
      test_normalidad_no_ok = res_sum$test_normalidad_no_ok,
      
      modelo = modelo_txt,
      outliers = outliers_txt,
      
      stringsAsFactors = FALSE
    )
  })
  
  df <- dplyr::bind_rows(rows)
  
  # Orden y filtro (igual que en Shiny)
  df <- df %>%
    dplyr::mutate(
      n_principales =
        as.integer(dplyr::coalesce(falla_residuos_lb, FALSE)) +
        as.integer(dplyr::coalesce(falla_estacionalidad, FALSE)),
      n_total_fallos =
        as.integer(dplyr::coalesce(falla_residuos_lb, FALSE)) +
        as.integer(dplyr::coalesce(falla_estacionalidad, FALSE)) +
        as.integer(dplyr::coalesce(falla_periodograma, FALSE)) +
        as.integer(dplyr::coalesce(falla_normalidad, FALSE))
    ) %>%
    # dplyr::filter(n_total_fallos > 0) %>%   # <- quita si quieres todas las CCAA aunque no fallen
    dplyr::arrange(
      dplyr::desc(n_principales),
      dplyr::desc(dplyr::coalesce(falla_periodograma, FALSE)),
      dplyr::desc(dplyr::coalesce(falla_normalidad, FALSE)),
      dplyr::desc(dplyr::coalesce(max_ratio_aj_or, -Inf))
    ) %>%
    dplyr::select(-n_principales, -n_total_fallos)
  
  df
}




### Carga de especificaciones desde Modelos_nueva_plantilla.xlsx ####
#
# Genera specs_lista[["AND.AFI"]], specs_lista[["AND.IAS"]], etc.
# Cada elemento es una spec tramoseats completa (ARIMA + outliers + calendario).
# Tambien genera outliers_lista por compatibilidad con versiones anteriores.




### Variables de entrada y fechas ####
fecha_ini <- c(1995, 1)
fecha_ini_tasa <- c(1995, 2)
ano_ini <- 1995
fecha_fin_e <- c(2030, 12)
ano_fin <- 2030

# si se quieren eliminar los datos del covid
fecha_inicio_cv <- "2032-01-01"
fecha_fin_cv <- "2034-03-01"




# ESPECIFICACIÓN TRAMO BASE (fallback) -----------------------------------
# Las specs completas (ARIMA + outliers + calendario) se leen desde
# Modelos_nueva_plantilla.xlsx en specs_lista. Esta spec base sólo
# se usa como fallback para series sin entrada en el Excel.

especificacion_tramo <- rjd3tramoseats::tramoseats_spec()





##### MODELOS TRAMO PARA TODAS LAS CCAA ####

#### Vector de CCAA ####
# ccaa_vec <- c("AND","ARA","AST","BAL","CAN","CNT","CYL",
#               "CLM","CAT","CVA","EXT","GAL","MAD",
#               "MUR","NAV","PVA","RIO","CYM")

ccaa_vec <- unique(sub("\\..*$", "", names(specs_lista)))
resultados_modelos_tramo <- function(ccaa_vec, ruta,
                                     file_entrada,
                                     out_xlxs,
                                     out_xlxs_ind) {

  modelos_tramo <- vector("list", length(ccaa_vec))
  names(modelos_tramo) <- ccaa_vec

  for (i in seq_along(ccaa_vec)) {
    ccaa <- ccaa_vec[i]

    message("
=== Procesando CCAA: ", ccaa, " ===")

    file_entrada <- file_entrada

    # Extraer specs de esta CCAA desde specs_lista y renombrar
    # "AND.AFI" -> "AFI" para que sa_proc las encuentre por nombre de indicador
    claves_ccaa <- names(specs_lista)[startsWith(names(specs_lista),
                                                 paste0(ccaa, "."))]
    especificaciones_lista <- specs_lista[claves_ccaa]
    # Renombrar "AND.AFI" -> "AFI" para que desestacionalizar_tramo encuentre por indicador
    names(especificaciones_lista) <- sub(paste0("^", ccaa, "\\."), "", names(especificaciones_lista))
    

    # Ejecuta el ajuste / SA con specs individuales por indicador
    modelos_tramo[[i]] <- sa_proc(file_entrada,
                                  fecha_ini, ano_ini,
                                  fecha_fin_e,
                                  fecha_inicio_cv, fecha_fin_cv,
                                  especificacion_tramo,        # fallback
                                  ccaa,
                                  especificaciones_lista = especificaciones_lista)
  }

  return(modelos_tramo)
}

salida_modelos <- resultados_modelos_tramo(ccaa_vec, ruta,
                                           file_entrada,
                                           out_xlxs,
                                           out_xlxs_ind)

# ---- Excel: una hoja por CCAA ----


wb <- openxlsx::createWorkbook()

for (ccaa in names(salida_modelos)) {
  df_ccaa <- make_resumen_ccaa(salida_modelos, ccaa)
  sh <- sanitize_sheet_name(ccaa)
  
  openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(wb, sh, df_ccaa)
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
  openxlsx::setColWidths(wb, sh, cols = 1:ncol(df_ccaa), widths = "auto")
}

openxlsx::saveWorkbook(wb, out_xlxs, overwrite = TRUE)
message("Excel guardado en: ", out_xlxs)

# ---- Excel: una hoja por indicador ----

all_indicadores <- sort(unique(unlist(
  lapply(salida_modelos, function(x) names(x$resumen_series_des))
)))


wb <- openxlsx::createWorkbook()

for (ind in all_indicadores) {
  df_ind <- make_resumen_indicador(salida_modelos, ind)
  if (is.null(df_ind) || nrow(df_ind) == 0) next
  
  sh <- sanitize_sheet_name(ind)
  openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(wb, sh, df_ind)
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
  openxlsx::setColWidths(wb, sh, cols = 1:ncol(df_ind), widths = "auto")
}

openxlsx::saveWorkbook(wb, out_xlxs_ind, overwrite = TRUE)
message("Excel por indicador guardado en: ", out_xlxs_ind)



save(salida_modelos,file=paste0(ruta,"/Datos_salida/Salida_des.RData"))



# Saco las series ajustadas a un excel ------------------------------------

out_xlxs_sa <- file.path(
  ruta, "Datos_salida",
  paste0("series_SA_CCAA.xlsx")
)

wb <- openxlsx::createWorkbook()

for (ccaa in names(salida_modelos)) {
  
  # lista de series SA (ts) de esa CCAA
  l <- salida_modelos[[ccaa]]$series_des
  if (is.null(l) || length(l) == 0) next
  
  # convertir cada ts a data.frame con fecha + columna del indicador
  dfs <- lapply(names(l), function(ind) {
    x <- l[[ind]]
    if (!inherits(x, "ts")) return(NULL)
    df <- tsbox::ts_df(x)              # time, value
    names(df) <- c("fecha", ind)
    df
  })
  dfs <- dfs[!sapply(dfs, is.null)]
  if (length(dfs) == 0) next
  
  # juntar por fecha
  panel <- Reduce(function(a, b) dplyr::full_join(a, b, by = "fecha"), dfs) |>
    dplyr::arrange(fecha)
  
  # escribir hoja
  sh <- sanitize_sheet_name(ccaa)
  openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(wb, sh, panel)
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
  openxlsx::setColWidths(wb, sh, cols = 1:ncol(panel), widths = "auto")
}

openxlsx::saveWorkbook(wb, out_xlxs_sa, overwrite = TRUE)
message("Excel SA guardado en: ", out_xlxs_sa)
