# ============================
# Shiny app: Series + Residuos (con fechas) + Diagnósticos
# (tests con nombres + P.value < 0.1 en rojo + quitar Description)
# ============================
rm(list=ls())
library(shiny)
library(ggplot2)
library(plotly)
library(zoo)
library(DT)
library(dplyr)
library(tibble)

# ruta="Z:/DIVISION_ANALISIS_ECONOMICO/01_Seguimiento/01_Macro/Modelos/02_METCAP/2_MODELIZACION/4_TD/Análisis/pruebas/TD_CCAA_total/"
ruta <- dirname(rstudioapi::getActiveDocumentContext()$path)  # sin "/" al final

load(paste0(ruta,"/Datos_salida/Salida_des.RData"))
stopifnot(exists("salida_modelos"))

# ---- Helpers ----
ts_to_date <- function(x_ts) {
  stopifnot(inherits(x_ts, "ts"))
  tt <- time(x_ts)
  fr <- frequency(x_ts)
  
  if (fr == 12) {
    as.Date(as.yearmon(tt))
  } else if (fr == 4) {
    as.Date(as.yearqtr(tt))
  } else if (fr == 1) {
    as.Date(as.yearmon(tt))
  } else {
    as.Date(as.yearmon(tt))
  }
}

to_long_df <- function(x, nombre) {
  if (inherits(x, "ts")) {
    data.frame(
      fecha = ts_to_date(x),
      value = as.numeric(x),
      serie = nombre,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      fecha = as.Date(seq_along(x), origin = "1970-01-01"),
      value = as.numeric(x),
      serie = nombre,
      stringsAsFactors = FALSE
    )
  }
}

# Residuos como ts con el MISMO calendario que la serie del indicador (y_or)
residuos_as_ts <- function(residuos, ref_ts) {
  stopifnot(!is.null(residuos))
  residuos <- as.numeric(residuos)
  
  if (!inherits(ref_ts, "ts")) return(ts(residuos))
  
  n_ref <- length(ref_ts)
  n_res <- length(residuos)
  n <- min(n_ref, n_res)
  
  if (n_ref != n_res) {
    warning(sprintf(
      "Longitud distinta: y_or=%d vs residuos=%d. Se recorta a %d para alinear fechas.",
      n_ref, n_res, n
    ))
  }
  
  ts(residuos[seq_len(n)],
     start = start(ref_ts),
     frequency = frequency(ref_ts))
}

# Convierte un objeto de tests (df/matrix/list) a un DF "largo" con columna test (rownames)
tests_to_df <- function(x) {
  
  # Si ya viene vacío
  if (is.null(x)) return(NULL)
  
  # Si viene como df/matrix suelto -> lo metemos en lista
  if (is.data.frame(x) || is.matrix(x)) {
    x <- list(Unico = x)
  }
  
  # Si no es lista a estas alturas, no sabemos tratarlo
  if (!is.list(x)) return(NULL)
  
  # Quitar NULLs dentro de la lista
  x <- x[!sapply(x, is.null)]
  if (length(x) == 0) return(NULL)
  
  # Unir todo
  out <- bind_rows(
    lapply(names(x), function(nm) {
      tb <- x[[nm]]
      
      if (is.matrix(tb)) tb <- as.data.frame(tb)
      if (!is.data.frame(tb)) return(NULL)
      
      # rownames -> columna "test"
      tb <- tibble::rownames_to_column(tb, var = "test")
      
      # limpiar sufijos raros si los hay (tipo "...1")
      tb <- tb %>%
        mutate(
          test = sub("\\.{2,3}[0-9]+$", "", test),
          elemento = nm
        )
      
      tb
    })
  )
  
  # quitar columna Description si existe
  if ("Description" %in% names(out)) out <- dplyr::select(out, -Description)
  if ("elemento" %in% names(out)) out <- dplyr::select(out, -elemento)
  
  out
}


ALPHA_FAIL <- 0.1
RATIO_FAIL <- 0.5

# Devuelve data.frame con columnas: test, P.value (numérica)
tests_flat <- function(x) {
  df <- tests_to_df(x)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # detectar columna p-valor robusto
  nms <- names(df)
  
  p_idx <- match("P.value", nms)
  if (is.na(p_idx)) return(NULL)
  
  pcol <- nms[p_idx]
  p <- df[[pcol]]
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
  
  # OJO: aquí casamos por "contiene" para ser tolerantes a nombres reales
  is_lb     <- grepl("^lb$|ljungbox|ljung|box", df$test_norm)
  is_seaslb <- grepl("seaslb|seasonallb|seasljung|seasonalljung", df$test_norm)
  
  is_lb2    <- grepl("^lb2$|lb2", df$test_norm)
  is_kurt   <- grepl("kurtosis|kurt", df$test_norm)
  is_dh     <- grepl("doornik|hansen|doornikhansen|dh", df$test_norm)
  
  bad_lb_group <- df[df$P.value < alpha & (is_lb | is_seaslb), , drop = FALSE]
  bad_norm_group <- df[df$P.value < alpha & (is_lb | is_lb2 | is_kurt | is_dh), , drop = FALSE]
  
  list(
    falla_residuos_lb = nrow(bad_lb_group) > 0,
    falla_normalidad = nrow(bad_norm_group) > 0,
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
  if (is.null(df)) {
    return(list(test_estacionalidad_no_ok = "", falla_estacionalidad = NA))
  }
  
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
    fail = any(r > ratio_thr, na.rm = TRUE),
    max_ratio = max(r, na.rm = TRUE),
    n_bad = sum(r > ratio_thr, na.rm = TRUE)
  )
}

# Normaliza nombres de tests para poder "casar" LB / SEASLB / etc.
norm_test_name <- function(x) {
  tolower(gsub("[^a-z0-9]", "", x))
}


# ---- UI ----
ui <- fluidPage(
  titlePanel("Análisis ajuste estacional"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs != 'Resumen Indicador'",
        selectInput("ccaa", "CCAA", choices = names(salida_modelos), selected = names(salida_modelos)[1])
      ),
      
     conditionalPanel(
        condition = "input.tabs != 'Resumen CCAA'",
        selectInput("indicador", "Indicador", choices = NULL)
      ),
      
      
      
      # Controles Series =========================
      
      conditionalPanel(
        condition = "input.tabs == 'Series'",
        
        checkboxGroupInput(
          "componentes", "Series a mostrar",
          choices = c("Original", "Ajustada", "Tendencia", "Estacional", "Irregular"),
          selected = c("Original", "Ajustada", "Tendencia", "Irregular")
        ),
        
        checkboxInput("puntos", "Mostrar puntos", value = FALSE)
      ),
      
      
      # Controles Residuos ==========================
      
      conditionalPanel(
        condition = "input.tabs == 'Residuos'",
        checkboxInput("ver_residuos", "Mostrar residuos", value = TRUE)
      ),
      
      # Controles Meses superpuestos ==========================
      
     # En la UI, reemplaza el selectInput por:
     conditionalPanel(
       condition = "input.tabs == 'Grafico meses superpuestos'",
       div(
         style = "display: flex; gap: 6px; margin-bottom: 4px;",
         actionButton("anios_todos",   "Todos",  class = "btn btn-xs btn-default"),
         actionButton("anios_ninguno", "Ninguno", class = "btn btn-xs btn-default")
       ),
       selectizeInput(
         "anios_sel",
         "Años a mostrar:",
         choices  = NULL,
         multiple = TRUE,
         selected = NULL,
         options  = list(plugins = list("remove_button"))
       )
     ),

      # Controles periodograma==========================
      
      conditionalPanel(
        condition = "input.tabs == 'Periodograma'",
        checkboxGroupInput(
          "periodo_series", "Series en periodograma",
          choices = c("Original", "Ajustada"),
          selected = c("Original", "Ajustada")
        ),
        checkboxInput("periodo_logy", "Escala log en Y", value = TRUE)
      ),
      
      
      hr(),
      helpText("Tip: zoom/selección en plotly (arrastrar para zoom, doble click para reset).")
    ),
    
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Series", plotlyOutput("plot_series", height = "650px")),
        
        tabPanel("Grafico meses superpuestos", plotlyOutput("plot_estacionalidad", height = "550px")),
        
        tabPanel(
          "Residuos",
          conditionalPanel(
            condition = "input.ver_residuos == true",
            plotlyOutput("plot_residuos", height = "400px"),
            br(),
            h4("Outliers detectados (prepro_out)"),
            DTOutput("tabla_outliers")
          )
        ),
       
        
        tabPanel(
          "Periodograma",
          plotlyOutput("plot_periodograma", height = "450px"),
          br(),
          h4("Picos estacionales (comparación Original vs Ajustada)"),
          DTOutput("tabla_picos_comp")
        ),
        
        tabPanel(
          "Diagnósticos",
          h4("Tests de residuos (prepro_residualtest)"),
          DTOutput("tabla_residualtest"),
          br(),
          h4("Tests de estacionalidad (sa_test)"),
          DTOutput("tabla_sa_test"),
          br(),
          h4("Varianza explicada (var_exp)"),
          DTOutput("tabla_var_exp")
        ),
        tabPanel(
          "Modelo estimado",
          h4("SARIMA (órdenes)"),
          verbatimTextOutput("txt_sarima_orders"),
          br(),
          h4("Coeficientes"),
          DTOutput("tabla_sarima_coef"),
          verbatimTextOutput("txt_log_transform"),
          verbatimTextOutput("txt_multiplicative"),
          h4("Modelo SEATS (JD3_ARIMA)"),
          tags$pre(
            style = "white-space: pre; overflow-x: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa;",
            textOutput("txt_seats_model")
          ),
          br()
        ),
        tabPanel(
          "Resumen CCAA",
          h4("Indicadores que NO cumplen criterios (según CCAA seleccionada)"),
          DTOutput("tabla_resumen_ccaa")
        ),
        tabPanel(
          "Resumen Indicador",
          h4("CCAA donde el indicador seleccionado NO cumple criterios"),
          DTOutput("tabla_resumen_indicador")
        )
        
        
      ),
      br(),
      verbatimTextOutput("info")
    )
  )
)


# ---- SERVER ----
server <- function(input, output, session) {
  
  observe({
    req(input$ccaa)
    req(input$tabs != "Resumen CCAA") 
    ind_choices <- names(salida_modelos[[input$ccaa]]$resumen_series_des)
    updateSelectInput(session, "indicador", choices = ind_choices, selected = ind_choices[1])
  })
  
  node_reactive <- reactive({
    req(input$ccaa, input$indicador)
    salida_modelos[[input$ccaa]]$resumen_series_des[[input$indicador]]
  })
  

  
  


# Series ------------------------------------------------------------------

 
  datos_series <- reactive({
    node <- node_reactive()
    df <- rbind(
      to_long_df(node$y_or, "Original"),
      to_long_df(node$y_aj, "Ajustada"),
      to_long_df(node$y_t,  "Tendencia"),
      to_long_df(node$y_s,  "Estacional"),
      to_long_df(node$y_i,  "Irregular")
    )
    df$serie <- factor(df$serie, levels = c("Original","Ajustada","Tendencia","Estacional","Irregular"))
    df[df$serie %in% input$componentes, , drop = FALSE]
  })
  
  

# Grafico de meses superpuestos -------------------------------------------

  # Actualizar selector de años cuando cambia el nodo
  observe({
    node <- node_reactive()
    req(!is.null(node$y_or_superpuestos))
    anios_disponibles <- sort(unique(node$y_or_superpuestos$anio))
    updateSelectizeInput(session, "anios_sel",
                         choices  = anios_disponibles,
                         selected = character(0))
  })
  
  # Botón "Todos"
  observeEvent(input$anios_todos, {
    node <- node_reactive()
    req(!is.null(node$y_or_superpuestos))
    anios_disponibles <- sort(unique(node$y_or_superpuestos$anio))
    updateSelectizeInput(session, "anios_sel",
                         choices  = anios_disponibles,
                         selected = anios_disponibles)
  })
  
  # Botón "Ninguno"
  observeEvent(input$anios_ninguno, {
    node <- node_reactive()
    req(!is.null(node$y_or_superpuestos))
    anios_disponibles <- sort(unique(node$y_or_superpuestos$anio))
    updateSelectizeInput(session, "anios_sel",
                         choices  = anios_disponibles,
                         selected = character(0))
  })
# Residuos ----------------------------------------------------------------

 
  datos_residuos <- reactive({
    req(isTRUE(input$ver_residuos))
    node <- node_reactive()
    
    validate(need(!is.null(node$residuos), "Este indicador no tiene 'residuos'."))
    validate(need(length(node$residuos) > 0, "El vector 'residuos' está vacío."))
    validate(need(!is.null(node$y_or), "No existe y_or para alinear el calendario de residuos."))
    
    res_ts <- residuos_as_ts(node$residuos, node$y_or)
    to_long_df(res_ts, "Residuos")
  })
  

# Diagnósticos ------------------------------------------------------------


  residual_tests <- reactive({
    node <- node_reactive()
    rt <- node$prepro_residualtest
    validate(need(!is.null(rt), "No existe prepro_residualtest para este indicador."))
    rt
  })
  

# Test de estacionalidad --------------------------------------------------


  sa_test <- reactive({
    node <- node_reactive()
    st <- node$sa_test
    validate(need(!is.null(st), "No existe st para este indicador."))
    st
  })


# Proporción de varianza explicada ----------------------------------------


  var_exp <- reactive({
    node <- node_reactive()
    ve <- node$var_exp
    validate(need(!is.null(ve), "No existe var_exp para este indicador."))
    ve
  })
  

# Outliers detectados -----------------------------------------------------


  outliers_df <- reactive({
    node <- node_reactive()
    ol <- node$prepro_out
    validate(need(!is.null(ol), "No hay prepro_out (outliers) para este indicador."))
    validate(need(is.data.frame(ol), "prepro_out no es un data.frame."))
    ol
  })


# Modelo estimado, log, seas model ---------------------------------------------------------

    
  sarima_obj <- reactive({
    node <- node_reactive()
    s <- node$saj$preprocessing$sarima
    validate(need(!is.null(s), "No existe saj$preprocessing$sarima para este indicador."))
    s
  })
  
  output$txt_sarima_orders <- renderPrint({
    s <- sarima_obj()
    ord <- s$sarima_orders
    validate(need(!is.null(ord), "No existe sarima_orders en el objeto sarima."))
    
    # p,d,q y estacionales bp,bd,bq (según tu estructura)
    p <- ord$p; d <- ord$d; q <- ord$q
    P <- ord$bp; D <- ord$bd; Q <- ord$bq
    
    cat(sprintf("SARIMA model: (%s,%s,%s) (%s,%s,%s)\n", p, d, q, P, D, Q))
  })
  
  output$txt_log_transform <- renderPrint({
    node <- node_reactive()
    lg <- node$saj$preprocessing$log
    validate(need(!is.null(lg), "No existe saj$preprocessing$log para este indicador."))
    
    cat("¿Transformación logarítmica?:", if (isTRUE(lg)) "Sí" else "No", "\n")
  })
  
  
  output$txt_multiplicative <- renderPrint({
    node <- node_reactive()
    mult <- node$saj$final$multiplicative
    validate(need(!is.null(mult), "No existe saj$final$multiplicative para este indicador."))
    
    cat("Multiplicativo:", if (isTRUE(mult)) "Sí" else "No", "\n")
  })
  
  
  output$txt_seats_model <- renderText({
    node <- node_reactive()
    m <- node$saj$decomposition$model
    validate(need(!is.null(m), "No existe saj$decomposition$model para este indicador."))
    
    # Helpers: formateo bonito
    fmt <- function(x) paste(sprintf("%.6g", x), collapse = " ")
    drop_leading_one <- function(v) {
      if (length(v) > 0 && isTRUE(all.equal(v[1], 1))) v[-1] else v
    }
    
    ar <- m$ar
    ma <- m$ma
    delta <- m$delta
    vv <- m$var
    
    # AR/MA suelen venir con el 1 inicial (convención de polinomio), lo quitamos para mostrar coef
    ar_coef <- drop_leading_one(ar)
    ma_coef <- drop_leading_one(ma)
    
    txt <- c(
      sprintf("name: %s", if (!is.null(m$name)) m$name else "JD3_ARIMA"),
      "",
      sprintf("AR (orden %d): %s", length(ar_coef), if (length(ar_coef) > 0) fmt(ar_coef) else "(sin AR)"),
      sprintf("DIF / delta (len %d): %s", length(delta), fmt(delta)),
      sprintf("MA (orden %d): %s", length(ma_coef), if (length(ma_coef) > 0) fmt(ma_coef) else "(sin MA)"),
      sprintf("Var: %s", sprintf("%.6g", vv))
    )
    
    paste(txt, collapse = "\n")
  })


# Tabla picos estacionales ------------------------------------------------------------

  
  output$tabla_picos_comp <- DT::renderDT({
    node <- node_reactive()
    
    
    tabla <- node$tabla_perio
    
   
    # Redondeos (bonito)
    tabla <- tabla %>%
      mutate(
        freq = round(freq, 3),
        periodo_meses = round(periodo_meses, 2),
        spec_original = round(spec_original, 2),
        spec_ajustada = round(spec_ajustada, 2),
        ratio_aj_or = round(ratio_aj_or, 3)
      )
    
    dt <- DT::datatable(
      tabla,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE),
      selection = "none"
    )
    
    # Resaltar ratio alto
    if ("ratio_aj_or" %in% names(tabla)) {
      dt <- dt %>%
        DT::formatStyle(
          "ratio_aj_or",
          color = DT::styleInterval(0.5, c("black", "red")),
          fontWeight = DT::styleInterval(0.5, c("normal", "bold"))
        )
    }
    
    dt
  }, server = FALSE)
  

# Resumen ccaa --------------------------------------

 
  resumen_ccaa <- reactive({
    req(input$ccaa)
    
    lista_ind <- salida_modelos[[input$ccaa]]$resumen_series_des
    ind_names <- names(lista_ind)
    
    rows <- lapply(ind_names, function(ind) {
      node <- lista_ind[[ind]]
      
      res_sum <- summarise_residuos(node$prepro_residualtest, alpha = ALPHA_FAIL)
      sea_sum <- summarise_estacionalidad(node$sa_test, alpha = ALPHA_FAIL)
      per_sum <- summarise_periodogram(node$tabla_perio, ratio_thr = RATIO_FAIL)  # el que ya tienes/te pasé
      
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
        
        stringsAsFactors = FALSE
      )
    })
    
    df <- dplyr::bind_rows(rows)
    
    df <- df %>%
      dplyr::mutate(
        falla_residuos_lb     = as.logical(falla_residuos_lb),
        falla_estacionalidad  = as.logical(falla_estacionalidad),
        falla_periodograma    = as.logical(falla_periodograma),
        falla_normalidad      = as.logical(falla_normalidad)
      )
    
    
    # Filtrar: solo los que incumplen algo (tratando NA como FALSE)
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
      dplyr::filter(n_total_fallos > 0) %>%
      dplyr::arrange(
        dplyr::desc(n_principales),                         # 2 (peor) -> 1 -> 0
        dplyr::desc(dplyr::coalesce(falla_periodograma, FALSE)),
        dplyr::desc(dplyr::coalesce(falla_normalidad, FALSE)),
        dplyr::desc(dplyr::coalesce(max_ratio_aj_or, -Inf)) # dentro del grupo, peor ratio primero
      )
    
    
    df <- df %>% dplyr::select(-n_principales, -n_total_fallos)
    
    
    
    df
  })
  


  
  output$tabla_resumen_ccaa <- DT::renderDT({
    df <- resumen_ccaa()
    validate(need(nrow(df) > 0, "No hay indicadores que incumplan los criterios para esta CCAA."))
    
    dt <- DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        scrollX = TRUE,
        ordering = FALSE   # <- CLAVE
      ),
      selection = "none"
    )
    
    # TRUE en rojo
    for (col in c("falla_residuos_lb", "falla_normalidad", "falla_estacionalidad", "falla_periodograma")) {
      if (col %in% names(df)) {
        dt <- DT::formatStyle(
          dt, col,
          color = DT::styleEqual(c(TRUE, FALSE), c("red", "black")),
          fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal"))
        )
      }
    }
    
    dt
  }, server = FALSE)
  
 
  
  # Resumen  indicador ---------------------------------------------
  
  resumen_indicador <- reactive({
    req(input$indicador)
    
    ccaa_names <- names(salida_modelos)
    
    rows <- lapply(ccaa_names, function(ccaa) {
      lista_ind <- salida_modelos[[ccaa]]$resumen_series_des
      if (is.null(lista_ind) || !(input$indicador %in% names(lista_ind))) return(NULL)
      
      node <- lista_ind[[input$indicador]]
      
      res_sum <- summarise_residuos(node$prepro_residualtest, alpha = ALPHA_FAIL)
      sea_sum <- summarise_estacionalidad(node$sa_test, alpha = ALPHA_FAIL)
      per_sum <- summarise_periodogram(node$tabla_perio, ratio_thr = RATIO_FAIL)
      
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
        
        stringsAsFactors = FALSE
      )
    })
    
    df <- dplyr::bind_rows(rows)
    
    # Orden "peor→mejor" con misma importancia residuos_lb y estacionalidad
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
      dplyr::filter(n_total_fallos > 0) %>%   # solo CCAA donde falla algo
      dplyr::arrange(
        dplyr::desc(n_principales),
        dplyr::desc(dplyr::coalesce(falla_periodograma, FALSE)),
        dplyr::desc(dplyr::coalesce(falla_normalidad, FALSE)),
        dplyr::desc(dplyr::coalesce(max_ratio_aj_or, -Inf))
      ) %>%
      dplyr::select(-n_principales, -n_total_fallos)
    
    df
  })
  
  
  output$tabla_resumen_indicador <- DT::renderDT({
    df <- resumen_indicador()
    validate(need(nrow(df) > 0, "Este indicador no incumple criterios en ninguna CCAA (o no existe en algunas)."))
    
    dt <- DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        scrollX = TRUE,
        ordering = FALSE
      ),
      selection = "none"
    )
    
    # TRUE en rojo
    for (col in c("falla_residuos_lb", "falla_estacionalidad", "falla_periodograma", "falla_normalidad")) {
      if (col %in% names(df)) {
        dt <- DT::formatStyle(
          dt, col,
          color = DT::styleEqual(c(TRUE, FALSE), c("red", "black")),
          fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal"))
        )
      }
    }
    
    dt
  }, server = FALSE)
  
  # ---- PLOTS ----  
# Series ------------------------------------------------------------------

  
  output$plot_series <- renderPlotly({
    df <- datos_series()
    validate(need(nrow(df) > 0, "No hay series seleccionadas."))
    
    pal <- c(
      "Original"   = "#1f77b4",
      "Ajustada"   = "#ff7f0e",
      "Tendencia"  = "#2ca02c",
      "Estacional" = "#9467bd",
      "Irregular"  = "#7f7f7f"
    )
    
    p <- ggplot(df, aes(x = fecha, y = value, color = serie, group = serie)) +
      geom_line(alpha = 0.9, linewidth = 0.7) +
      scale_color_manual(values = pal, drop = FALSE) +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = NULL)
    
    if (isTRUE(input$puntos)) p <- p + geom_point(size = 1.2, alpha = 0.9)
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })


# Residuos ----------------------------------------------------------------


  
  output$plot_residuos <- renderPlotly({
    df <- datos_residuos()
    validate(need(nrow(df) > 0, "No hay residuos para mostrar."))
    
    p <- ggplot(df, aes(x = fecha, y = value)) +
      geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.7) +
      geom_line(alpha = 0.7, linewidth = 0.7) +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = "Residuo")
    
    if (isTRUE(input$puntos)) p <- p + geom_point(size = 1.2, alpha = 0.7)
    
    ggplotly(p, tooltip = c("x", "y"))
  })


# Grafico meses superpuestos ----------------------------------------------

  
  
  output$plot_estacionalidad <- renderPlotly({
    node <- node_reactive()
    
    df  <- node$y_or_superpuestos
    med <- node$media_estacional
    
    # Paleta fija basada en TODOS los años disponibles, no solo los seleccionados
    todos_anios <- sort(unique(df$anio))
    colores_fijos <- scales::hue_pal()(length(todos_anios))
    names(colores_fijos) <- as.character(todos_anios)
    
    # Filtrar por años seleccionados
    if (!is.null(input$anios_sel) && length(input$anios_sel) > 0) {
      df <- df[df$anio %in% as.numeric(input$anios_sel), ]
    }
    
    anios <- sort(unique(df$anio))
    
    p <- plot_ly()
    
    for (a in anios) {
      df_a <- df[df$anio == a, ]
      df_a <- df_a[order(df_a$mes), ]
      
      p <- add_trace(p,
                     x             = df_a$mes,
                     y             = df_a$indice,
                     type          = "scatter",
                     mode          = "lines",
                     name          = as.character(a),
                     line          = list(color = colores_fijos[as.character(a)], width = 1.5),
                     hovertemplate = paste0(a, ": %{y:.1f}<extra></extra>"),
                     showlegend    = FALSE
      )
    }
    
    med_ord <- med[order(med$mes), ]
    p <- add_trace(p,
                   x             = med_ord$mes,
                   y             = med_ord$indice_medio,
                   type          = "scatter",
                   mode          = "lines",
                   name          = "Media",
                   line          = list(color = "black", width = 2.5),
                   hovertemplate = "Media: %{y:.1f}<extra></extra>",
                   showlegend    = FALSE
    )
    
    p %>% layout(
      title = "Estacionalidad: meses superpuestos (normalizado por año)",
      xaxis = list(tickvals = 1:12, ticktext = month.abb, title = "Mes"),
      yaxis = list(title = "Índice (media anual = 100)"),
      shapes = list(list(
        type = "line", x0 = 1, x1 = 12,
        y0 = 100, y1 = 100,
        line = list(dash = "dot", color = "grey50")
      )),
      hovermode = "closest"
    )
  })

# Periodograma ------------------------------------------------------------


  output$plot_periodograma <- renderPlotly({
    node <- node_reactive()
    
    validate(need(!is.null(input$periodo_series) && length(input$periodo_series) > 0,
                  "Selecciona al menos una serie para el periodograma."))
    
    y_or <- node$y_or
    y_aj <- node$y_aj
    
    
    
    validate(need(inherits(y_or, "ts"), "y_or no es un objeto ts."))
    validate(need(inherits(y_aj, "ts"), "y_aj no es un objeto ts."))
    validate(need(frequency(y_or) == frequency(y_aj), "y_or y y_aj tienen distinta frecuencia."))
    validate(need(length(y_or) > 10 && length(y_aj) > 10, "Series demasiado cortas para periodograma."))
    
    # Alinear longitudes si difieren
    n <- min(length(y_or), length(y_aj))
    y_or <- ts(as.numeric(y_or)[seq_len(n)], start = start(y_or), frequency = frequency(y_or))
    y_aj <- ts(as.numeric(y_aj)[seq_len(n)], start = start(y_aj), frequency = frequency(y_aj))
    
    # Calcula periodogramas solo de lo que se ha pedido
    dfs <- list()
    
    if ("Original" %in% input$periodo_series) {
      sp_or <- spectrum(y_or, plot = FALSE, taper = 0)
      dfs[["Original"]] <- data.frame(freq = sp_or$freq, spec = sp_or$spec, serie = "Original")
    }
    
    if ("Ajustada" %in% input$periodo_series) {
      sp_aj <- spectrum(y_aj, plot = FALSE, taper = 0)
      dfs[["Ajustada"]] <- data.frame(freq = sp_aj$freq, spec = sp_aj$spec, serie = "Ajustada")
    }
    
    df <- dplyr::bind_rows(dfs)
    
    p <- ggplot(df, aes(x = freq, y = spec, color = serie)) +
      geom_line(linewidth = 0.9) +
      theme_minimal(base_size = 13) +
      labs(
        x = "Frecuencia (ciclos/año)",
        y = "Potencia (densidad espectral)",
        color = NULL,
        title = "Periodograma"
      )
    p <- p +
      geom_vline(xintercept = 1:6, linetype = 3)
    
  
    
    if (isTRUE(input$periodo_logy)) {
      p <- p + scale_y_log10()
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(hovermode = "x unified")
  })
  
  # ---- TABLAS (DT) ----
  

# Tabla residual test -----------------------------------------------------

  
  output$tabla_residualtest <- DT::renderDT({
    
    tabla_dt <- tests_to_df(residual_tests())
    validate(need(!is.null(tabla_dt) && nrow(tabla_dt) > 0, "No hay tests de residuos para mostrar."))
    
    dt <- DT::datatable(
      tabla_dt,
      rownames = FALSE,
      options = list(
        dom = "t",        # solo la tabla (sin info, sin paginación, sin buscador)
        paging = FALSE ,
        selection = "none"   # sin paginación
      )
      
    ) %>%
      DT::formatRound(columns = intersect(c("Statistic", "P.value"), names(tabla_dt)), digits = 3)
    
    if ("P.value" %in% names(tabla_dt)) {
      dt <- dt %>%
        DT::formatStyle("P.value", color = DT::styleInterval(0.1, c("red", "black")))
    }
    
    dt
    
  }, server = FALSE)
  

# Tablas seasonal test ----------------------------------------------------


  output$tabla_sa_test <- DT::renderDT({
    
    tabla_dt <- tests_to_df(sa_test())
    validate(need(!is.null(tabla_dt) && nrow(tabla_dt) > 0, "No hay tests de estacionalidad para mostrar."))
    
    dt <- DT::datatable(
      tabla_dt,
      rownames = FALSE,
      options = list(
        dom = "t",        # solo la tabla (sin info, sin paginación, sin buscador)
        paging = FALSE ,
        selection = "none"   # sin paginación
      )
    ) %>%
      DT::formatRound(columns = intersect(c("Statistic", "P.value"), names(tabla_dt)), digits = 3)
    
    if ("P.value" %in% names(tabla_dt)) {
      dt <- dt %>%
        DT::formatStyle("P.value", color = DT::styleInterval(0.1, c("red", "black")))
    }
    
    dt
    
  }, server = FALSE)


# Tabla varianza explicada ------------------------------------------------

  
  output$tabla_var_exp <- DT::renderDT({
    
    ve <- var_exp()
    validate(need(!is.null(ve), "No existe var_exp para este indicador."))
    
    # var_exp: matrix con dimnames -> data.frame con columna de componente
    tabla_dt <- as.data.frame(ve)
    tabla_dt <- tibble::rownames_to_column(tabla_dt, var = "componente")
    
    DT::datatable(
      tabla_dt,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE),
      selection = "none"
    ) %>%
      DT::formatRound(columns = setdiff(names(tabla_dt), "componente"), digits = 3)
    
  }, server = FALSE)
  

# Tabla outliers ----------------------------------------------------------

 
  output$tabla_outliers <- DT::renderDT({
    df <- outliers_df()
    
    # Si el identificador/tiempo del outlier viene en rownames, lo pasamos a columna
    if (!is.null(rownames(df)) && any(nzchar(rownames(df)))) {
      df <- tibble::rownames_to_column(df, var = "outlier")
    }
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE),  # sin buscador/selector/paginación
      selection = "none"
    ) %>%
      DT::formatRound(
        columns = intersect(c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)"), names(df)),
        digits = 5
      ) %>%
      { if ("Pr(>|t|)" %in% names(df))
        DT::formatStyle(., "Pr(>|t|)", color = DT::styleInterval(0.1, c("red", "black")))
        else .
      }
    
  }, server = FALSE)


# Tabla coef estimation ---------------------------------------------------

    
  output$tabla_sarima_coef <- DT::renderDT({
    s <- sarima_obj()
    df <- s$coef_table
    validate(need(!is.null(df), "No existe coef_table en el objeto sarima."))
    validate(need(is.data.frame(df), "coef_table no es un data.frame."))
    
    # Si los nombres de los coeficientes vienen en rownames (phi(1), theta(1), ...)
    if (!is.null(rownames(df)) && any(nzchar(rownames(df)))) {
      df <- tibble::rownames_to_column(df, var = "coef")
    }
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE),
      selection = "none"
    ) %>%
      DT::formatRound(
        columns = intersect(c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)"), names(df)),
        digits = 5
      ) %>%
      { if ("Pr(>|t|)" %in% names(df))
        DT::formatStyle(., "Pr(>|t|)", color = DT::styleInterval(0.1, c("red", "black")))
        else .
      }
    
  }, server = FALSE)
  
  
  
  
}

shinyApp(ui, server)
