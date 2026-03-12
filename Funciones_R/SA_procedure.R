sa_proc<-function(file_entrada,
                        fecha_ini,ano_ini,
                        fecha_fin_e,
                        fecha_inicio_cv,
                        fecha_fin_cv, especificacion_tramo, ccaa,
                        especificaciones_lista = NULL){
  
  # fecha_inicio_bucle=ymd(fecha_max)
  
  # fecha_fin_e=c(year(fecha_inicio_bucle),
  #               month(fecha_inicio_bucle)+3
  # ) 
#### 2.Rutas, variables de entrada y funciones auxiliares #######

  #### 3.leemos datos #####
  
  metadatos=readxl::read_excel(file_entrada, 
                               sheet=ccaa,
                               range=cell_limits(c(1, 4), c(14, 16)),
                               col_names=TRUE)
  
  
  series=readxl::read_excel(file_entrada, 
                            sheet=ccaa,
                            
                            range=cell_limits(c(15,1), c(NA, 16)),
                            col_names = FALSE
  )
  
  
  bad <- -999.99
  num <- sapply(series, is.numeric)
  
  bad <- -999.99
  
  # con tolerancia (recomendado)
  series <- series %>% mutate(across(where(is.numeric), ~ ifelse(dplyr::near(., bad), NA_real_, .)))
  
  
  nombres=names(metadatos)
  names(series)=c('date','anyo','mes',nombres,'fecha_trim')
  
  selector=metadatos[12,]
  seleccion=metadatos[,which(selector==1)]
  seriess=series%>%dplyr::select(all_of(c('date',names(seleccion))))
  
  SA=seleccion[1,]
  tipos=seleccion[10,] 
  
  dlg=seleccion[11,]
  ref=seleccion[13,]
  
  trimestrales=seleccion[,which(tipos==2)]
  soft=seleccion[,which(tipos==4)]
  frecuencia=ifelse(tipos%in%c(4,6),12,4)
  
  
  seriess[seriess==-999.99]=NA
 
  
  
  seriesf=seriess[,-1]
  
  # anualf=na.omit(seriesf[which((tipos==1))])
  # 
  # trimf=seriesf[which((tipos==4))]
  
  
  #### 4. Transformaciones series ####
  
  nombress=names(seriesf)
  
  datos=sapply(nombress, function(n) {assign(n, list(), envir = .GlobalEnv) })
  for (i in 1:length(nombress)){
    datos[[i]]=(ts(seriesf[,i],frequency = frecuencia[i],start=c(fecha_ini)))
    
  }
  
  
  
  desestacionalizar_tramo <- function(lista_ts) {

    sa_list   <- vector("list", length(lista_ts))
    sum_list  <- vector("list", length(lista_ts))
    names(sa_list)  <- names(lista_ts)
    names(sum_list) <- names(lista_ts)

    for (i in seq_along(lista_ts)) {
      x          <- lista_ts[[i]]
      nombre_ind <- names(lista_ts)[i]

      # Seleccionar spec: la individual del indicador si existe, si no el fallback
      if (!is.null(especificaciones_lista) &&
          nombre_ind %in% names(especificaciones_lista)) {
        spec_usar <- especificaciones_lista[[nombre_ind]]
        message("    [", ccaa, ".", nombre_ind, "] spec individual")
      } else {
        spec_usar <- especificacion_tramo
        message("    [", ccaa, ".", nombre_ind, "] spec fallback")
      }

      # Ajuste protegido: si falla un indicador no para toda la CCAA
      resultado <- tryCatch({
        aj  <- rjd3tramoseats::tramoseats(x, spec = spec_usar)
        dec <- rjd3toolkit::sa_decomposition(aj)
        sa  <- window(dec$sa, end = end(x))
        list(sa = sa, resumen = analisis_sa(aj))
      }, error = function(e) {
        warning("[", ccaa, ".", nombre_ind, "] ERROR en tramoseats: ", conditionMessage(e))
        NULL
      })

      if (!is.null(resultado)) {
        sa_list[[i]]  <- resultado$sa
        sum_list[[i]] <- resultado$resumen
      }
    }

    return(list(sa_list = sa_list, sum_list = sum_list))
  }
  
  
  # separar mensuales y trimestrales
  mensuales   <- datos[sapply(datos, frequency) == 12]
  
  # desestacionalizar solo las mensuales
  res_mensuales <- desestacionalizar_tramo(mensuales)
 
  
  return(list(
              series_des=res_mensuales$sa_list,
              resumen_series_des=res_mensuales$sum_list))
}
