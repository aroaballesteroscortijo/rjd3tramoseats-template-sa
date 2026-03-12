


hojas_ind <- excel_sheets(ruta_modelos)
hojas_ind <- hojas_ind[!hojas_ind %in% c("Instrucciones", "RESUMEN")]


# Funciones auxiliares ----------------------------------------------------



# "True"/"False"/NA/NULL → lógico
as_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.na(x))                      return(default)
  x <- trimws(as.character(x))
  if (x %in% c("NA", "nan", "NaN", "")) return(default)
  toupper(x) == "TRUE"
}

# valor → entero, NA si vacío
as_int <- function(x, default = NA_integer_) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.na(x))                      return(default)
  x <- trimws(as.character(x))
  if (x %in% c("NA", "nan", "NaN", "")) return(default)
  suppressWarnings(as.integer(as.numeric(x)))
}

# valor → string limpio, NULL si vacío
as_str <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.na(x))                      return(NULL)
  x <- trimws(as.character(x))
  if (x %in% c("NA", "nan", "NaN", "")) return(NULL)
  x
}

# Convierte cualquier representación de fecha a "yyyy-mm-dd"
# Cubre: string "yyyy-mm-dd", número Excel (días desde 1899-12-30),
# objeto Date/POSIXct de R
as_fecha <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NULL)
  
  # Número: días desde origen Excel (1899-12-30)
  if (is.numeric(x)) {
    d <- as.Date(as.integer(x), origin = "1899-12-30")
    return(format(d, "%Y-%m-%d"))
  }
  
  # Objeto Date o POSIXct de R
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(format(as.Date(x), "%Y-%m-%d"))
  }
  
  # String: limpiar y validar formato yyyy-mm-dd
  s <- trimws(as.character(x))
  if (s %in% c("NA", "nan", "NaN", "")) return(NULL)
  
  # Si tiene timestamp al final ("2020-03-01 00:00:00") quedarnos solo con fecha
  s <- sub("\\s+\\d{2}:\\d{2}:\\d{2}.*$", "", s)
  
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(s)
  
  # Intentar parsear como fecha genérica
  d <- suppressWarnings(as.Date(s))
  if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  
  NULL
}

# FUNCIÓN PRINCIPAL -------------------------------------------------------
TIPOS_VALIDOS= c("AO", "TC", "LS", "SO")

construir_spec <- function(fila, indicador) {

  ccaa <- as.character(fila[["CCAA"]])

# 1. Spec base vacía ------------------------------------------------------

  spec <- tramoseats_spec()


# 2. Transformación -------------------------------------------------------

  
transform_val <- as_str(fila[["transform"]])
  if (!is.null(transform_val)) {
    fn <- switch(transform_val,
      "None" = "None",
      "Log"  = "Log",
      "Auto" = "Auto",
      "Auto"
    )
    spec <- set_transform(spec, fun = fn)
  }


# 3. Modelo ARIMA --------------------------------------------------------

model_auto <- as_bool(fila[["model.auto"]], default = TRUE)
  spec <- set_automodel(spec, enabled = model_auto)

  if (!model_auto) {
    p  <- as_int(fila[["p"]],  default = 0L)
    d  <- as_int(fila[["d"]],  default = 1L)
    q  <- as_int(fila[["q"]],  default = 1L)
    bp <- as_int(fila[["bp"]], default = 0L)
    bd <- as_int(fila[["bd"]], default = 1L)
    bq <- as_int(fila[["bq"]], default = 1L)

    spec <- set_arima(spec,
                      p  = p,  d  = d,  q  = q,
                      bp = bp, bd = bd, bq = bq,
                      coef.type = "Undefined")
  }


# 4. Outliers -------------------------------------------------------------

  
 outlier_auto <- as_bool(fila[["outlier.auto"]], default = FALSE)
  auto_cv      <- suppressWarnings(as.numeric(as_str(fila[["auto.cv"]])))
  
  if (outlier_auto) {
    spec <- set_outlier(spec,
                        outliers.type = c("AO", "TC", "LS"),
                        cv            = if (!is.na(auto_cv)) auto_cv else NULL)
  } else {
    spec <- set_outlier(spec, outliers.type = NULL)
  }
  
  # Outliers manuales
  if (as_bool(fila[["outlier.user"]], default = FALSE)) {
    
    cols_type <- grep("^out_type_", names(fila), value = TRUE)
    cols_date <- grep("^out_date_", names(fila), value = TRUE)
    
    # Registrar pares ya añadidos para evitar duplicados
    pares_vistos <- character(0)
    
    for (k in seq_along(cols_type)) {
      tipo  <- as_str(fila[[cols_type[k]]])
      fecha <- as_fecha(fila[[cols_date[k]]])
      
      # Validar tipo
      if (is.null(tipo) || !(toupper(tipo) %in% TIPOS_VALIDOS)) next
      
      # Validar fecha
      if (is.null(fecha)) next
      
      # Evitar duplicados
      par_id <- paste0(toupper(tipo), "@", fecha)
      if (par_id %in% pares_vistos) {
        message("    [", ccaa, ".", indicador, "] Outlier duplicado ignorado: ", par_id)
        next
      }
      pares_vistos <- c(pares_vistos, par_id)
      
      spec <- add_outlier(spec, type = toupper(tipo), date = fecha)
    }
  }


# 5. Calendario -----------------------------------------------------------

    
 cal_auto <- as_bool(fila[["calendar.auto"]], default = TRUE)

  if (cal_auto) {
    spec <- set_tradingdays(spec,
                            option  = "WorkingDays",
                            test    = "Separate_T")
    spec <- set_easter(spec,
                       type    = "Standard",
                       test    = TRUE)
  } else {
    # Días hábiles
    td_type     <- as_str(fila[["td.type"]])
    td_test     <- as_bool(fila[["td.test"]],     default = FALSE)
    td_leapyear <- as_bool(fila[["td.leapyear"]], default = FALSE)
    leapyear_str <- if (isTRUE(td_leapyear)) "LeapYear" else "None"
    if (!is.null(td_type) && td_type != "None") {
      spec <- set_tradingdays(spec,
                              option   = td_type,
                              leapyear = leapyear_str,
                              test     = if (td_test) "Separate_T" else "None")
    } else {
      spec <- set_tradingdays(spec, option = "None")
    }

    # Semana Santa
    easter_test <- as_bool(fila[["easter.test"]], default = FALSE)
    easter_type <- as_str(fila[["easter.type"]])

    if (!is.null(easter_type) && easter_type != "Unused") {
      spec <- set_easter(spec,
                         type    = easter_type,
                         test    = easter_test)
    } else {
      spec <- set_easter(spec)
    }
  }

  spec
}


# Lectura de todas las espec ----------------------------------------------


specs_lista    <- list()   # specs_lista[["AND.AFI"]]  → spec completa rjd3
outliers_lista <- list()   # outliers_lista[["AND.AFI"]] → compatibilidad legado

for (indicador in hojas_ind) {

  message("\n=== Leyendo indicador: ", indicador, " ===")

  df <- read_excel(ruta_modelos,
                   sheet     = indicador,
                   skip      = 1,       # saltar fila de grupos
                   col_names = TRUE)

  for (i in seq_len(nrow(df))) {
    ccaa <- trimws(as.character(df$CCAA[i]))
    if (is.na(ccaa) || ccaa == "" || ccaa == "NA") next

    fila  <- as.list(df[i, ])
    clave <- paste0(ccaa, ".", indicador)

    specs_lista[[clave]] <- tryCatch(
      construir_spec(fila, indicador),
      error = function(e) {
        warning("  [", clave, "] ERROR: ", conditionMessage(e))
        NULL
      }
    )
    message("  [", clave, "] OK")

    # Outliers por separado (compatibilidad con hacer_spec_con_outliers)
    cols_type <- grep("^out_type_", names(fila), value = TRUE)
    cols_date <- grep("^out_date_", names(fila), value = TRUE)
    tipos  <- as.character(unlist(fila[cols_type]))
    fechas <- as.character(unlist(fila[cols_date]))
    validos <- which(
      !is.na(tipos)  & tipos  != "NA" & tipos  != "nan" &
      !is.na(fechas) & fechas != "NA" & fechas != "nan"
    )

    outliers_lista[[clave]] <- list(
      ccaa      = ccaa,
      indicador = indicador,
      out_types = tipos[validos],
      out_dates = fechas[validos]
    )
  }
}


# Resumen modelos leídos --------------------------------------------------


message("\n=== RESUMEN DE SPECS CONSTRUIDAS ===")
for (ind in hojas_ind) {
  claves_ind <- names(specs_lista)[endsWith(names(specs_lista), paste0(".", ind))]
  if (length(claves_ind) == 0) next
  nulos <- vapply(specs_lista[claves_ind], is.null, logical(1))
  n_ok  <- sum(!nulos)
  n_err <- sum(nulos)
  if (n_err > 0) {
    errs <- claves_ind[nulos]
    message(sprintf("  %-6s  OK: %2d  |  ERROR: %d  ->  %s",
                    ind, n_ok, n_err, paste(errs, collapse = ", ")))
  } else {
    message(sprintf("  %-6s  OK: %2d", ind, n_ok))
  }
}
nulos_total <- vapply(specs_lista, is.null, logical(1))
n_total_ok  <- sum(!nulos_total)
n_total_err <- sum(nulos_total)
message(sprintf("  %s", strrep("-", 35)))
message(sprintf("  TOTAL    OK: %2d  |  ERROR: %d", n_total_ok, n_total_err))
