get_last_date_update_local_data <- function(local_data){
  if (is.null(local_data) || nrow(local_data) == 0) return(NULL)
  if (!"created_at" %in% names(local_data)) return(NULL)
  
  tryCatch({
    max_date <- max(local_data$created_at, na.rm = TRUE)
    if (!is.finite(max_date)) return(NULL)
    max_date
  }, error = function(e) NULL)
}

#' Parsear Entradas de Epicollect5 a Dataframe
#'
#' @param entries_list Lista de entradas crudas de la API.
#' @param clean_names_flag Lógico. Si es TRUE, limpia los nombres de las columnas.
#' @return Un tibble plano y formateado.
#' @export
parse_epicollect_entries <- function(entries_list, clean_names_flag = FALSE) {
  if (is.null(entries_list) || length(entries_list) == 0) {
    message("   ⚠️ [TEST parse_epicollect_entries] entries_list está vacío o es NULL.")
    return(tibble::tibble())
  }
  
  parsed_dataframe <- tryCatch({
    purrr::map_dfr(entries_list, function(single_entry) {
      # 1. Aplicamos la función auxiliar para corregir la localización
      flat_entry <- flatten_ec5_location(single_entry)
      
      # 2. Convertimos a fila de tibble de forma segura
      df_entry <- dplyr::as_tibble(flat_entry) 
      
      # 3. 🔥 CONTROL DEFENSIVO DE TIPOS (Evita el choque dbl vs chr)
      # Buscamos las columnas que terminen en _lat o _lng (o contengan lat/long) generadas por flatten
      col_coordenadas <- grep("lat|lng|long|localizacion", names(df_entry), value = TRUE, ignore.case = TRUE)
      
      if (length(col_coordenadas) > 0) {
        df_entry <- df_entry %>%
          dplyr::mutate(dplyr::across(
            dplyr::all_of(col_coordenadas), 
            ~ as.numeric(as.character(.))
          ))
      }
      
      return(df_entry)
    })
  }, error = function(e) {
    message("   ❌ [ERROR en tryCatch]: ", e$message)
    return(tibble::tibble())
  })
  
  message("   📥 [TEST parse_epicollect_entries] Filas parsed_dataframe: ", nrow(parsed_dataframe))
  
  # Bloque de formateo posterior (Fechas y tipado)
  if (!is.null(parsed_dataframe) && nrow(parsed_dataframe) > 0) {
    
    if ("created_at" %in% names(parsed_dataframe)) {
      parsed_dataframe$created_at <- lubridate::ymd_hms(parsed_dataframe$created_at, tz = "UTC")
    }
    if ("uploaded_at" %in% names(parsed_dataframe)) {
      parsed_dataframe$uploaded_at <- lubridate::ymd_hms(parsed_dataframe$uploaded_at, tz = "UTC")
    }
    if ("6_Hora" %in% names(parsed_dataframe)) {
      parsed_dataframe$`6_Hora` <- as.character(parsed_dataframe$`6_Hora`)
    }
    
    message("   📥 [TEST parse_epicollect_entries] Luego IF parsed_dataframe: ", nrow(parsed_dataframe))
    
    if (clean_names_flag) {
      parsed_dataframe <- parsed_dataframe %>% janitor::clean_names()
    }
    
    message("   📥 [TEST parse_epicollect_entries] Luego IF parsed_dataframe luego clean_names_flag: ", 
            nrow(parsed_dataframe))
  }
  
  return(parsed_dataframe)
}

#' Aplanar Campos de Localización de Epicollect5
#'
#' @param single_entry Lista. Un elemento o registro individual proveniente de la API.
#' @return Lista con los campos de localización extraídos a nivel de raíz.
#' @export
flatten_ec5_location <- function(single_entry) {
  if (!is.list(single_entry)) return(single_entry)
  
  for (name in names(single_entry)) {
    # Detectamos si el campo interno es una lista y contiene datos geográficos
    if (is.list(single_entry[[name]]) && "latitude" %in% names(single_entry[[name]])) {
      loc <- single_entry[[name]]
      
      # Extraemos las variables clave de forma plana
      single_entry[[paste0(name, "_lat")]] <- loc$latitude
      single_entry[[paste0(name, "_lng")]] <- loc$longitude
      single_entry[[paste0(name, "_acc")]] <- loc$accuracy
      
      # Eliminamos el objeto lista original para evitar duplicación vertical en as_tibble
      single_entry[[name]] <- NULL 
    }
  }
  return(single_entry)
}



merge_data_branch_entries <- function(df_form, df_branch) {
  if (is.null(df_form) || nrow(df_form) == 0) return(df_branch)
  if (is.null(df_branch) || nrow(df_branch) == 0) return(df_form)
  
  # Forzamos nombres en minúsculas en caliente para que el Join nunca se rompa por casing
  names(df_form)   <- tolower(names(df_form))
  names(df_branch) <- tolower(names(df_branch))
  
  # Buscamos la mejor llave primaria del padre (usualmente ec5_uuid)
  llave_padre <- intersect(c("ec5_uuid", "uuid"), names(df_form))
  if (length(llave_padre) == 0) llave_padre <- names(df_form)[1] else llave_padre <- llave_padre[1]
  
  # Buscamos la llave foránea en el hijo (usualmente ec5_branch_owner_uuid)
  llave_hijo <- intersect(c("ec5_branch_owner_uuid", "branch_owner_uuid"), names(df_branch))
  if (length(llave_hijo) == 0) llave_hijo <- names(df_branch)[1] else llave_hijo <- llave_hijo[1]
  
  # Mapeo dinámico de llaves por si difieren textualmente
  by_vector <- setNames(llave_padre, llave_hijo)
  
  df_joined <- df_branch %>%
    dplyr::left_join(df_form, by = by_vector, suffix = c("_branch", "_form"))
  
  return(df_joined)
}
# FUNCIÓN MAESTRA ORQUESTRADORA CON FLAGS DE DEBUGGING
# FUNCIÓN MAESTRA MODIFICADA CON COMPROBACIÓN DE DESCARGA DE BRANCH
sync_and_merge_epicollect <- function(project_slug, form_ref, branch_ref, dir_entries = NULL, dir_branch = NULL, delimiter = ",", api_token = NULL) {
  
  message("\n=======================================================")
  message("🚀 INICIANDO PROCESO DE SINCRONIZACIÓN Y FUSIÓN")
  message("=======================================================\n")
  
  safe_nrow <- function(df) {
    if (is.null(df)) return("NULL")
    if (is.list(df) && !is.data.frame(df)) return(paste("Lista con", length(df), "elementos"))
    return(nrow(df))
  }
  
  # =================================================================
  # 1. Sincronización Segura del Formulario Padre
  # =================================================================
  message("📋 [1/3] PROCESANDO FORMULARIO PADRE...")
  df_form_local <- NULL
  last_date_form <- NULL
  if (!is.null(dir_entries) && file.exists(dir_entries)) {
    df_form_local <- get_data_local_csv(dir_entries, delimiter = delimiter)
    last_date_form <- get_last_date_update_local_data(df_form_local)
  }
  api_form_list <- get_data_API(project_slug, form_ref, last_date_local_update = last_date_form, api_token = api_token)
  df_form_api   <- parse_epicollect_entries(api_form_list)
  df_form_final <- dplyr::bind_rows(df_form_local, df_form_api)
  
  if (!is.null(df_form_final) && nrow(df_form_final) > 0) {
    names(df_form_final) <- tolower(names(df_form_final))
    id_col <- intersect(c("ec5_uuid", "uuid"), names(df_form_final))
    if (length(id_col) > 0) df_form_final <- df_form_final %>% dplyr::distinct(!!sym(id_col[1]), .keep_all = TRUE)
  }
  
  # =================================================================
  # 2. TEST Y SINCRONIZACIÓN DE LA BRANCH (HIJO) 
  # =================================================================
  message("\n🌿 [2/3] PROCESANDO FORMULARIO HIJO (BRANCH)...")
  
  df_branch_local <- NULL
  last_date_branch <- NULL
  if (!is.null(dir_branch) && file.exists(dir_branch)) {
    df_branch_local <- get_data_local_csv(dir_branch, delimiter = delimiter)
    last_date_branch <- get_last_date_update_local_data(df_branch_local)
    message("   🔹 [TEST BRANCH] Filas locales previas: ", safe_nrow(df_branch_local))
    message("   🔹 [TEST BRANCH] Filtrando API desde fecha: ", ifelse(is.null(last_date_branch), "Todo el histórico", as.character(last_date_branch)))
  }
  
  # ------ ACTIVACIÓN DE LLAMADA REAL A LA API PARA BRANCH ------
  message("   📡 [TEST BRANCH] Conectando a API Epicollect5...")
  
  # Usamos la función wrapper dedicada para traer la estructura hijo
  api_branch_list <- get_branch_data(project_slug, form_ref, branch_ref, last_date_local_update = last_date_branch, api_token = api_token)
  
  # TEST 1: Verificar respuesta cruda (Suele venir paginada o estructurada como lista)
  message("   📊 [TEST BRANCH] Respuesta cruda de la API: ", safe_nrow(api_branch_list))
  
  # Parsear a dataframe limpio
  df_branch_api   <- parse_epicollect_entries(api_branch_list)
  
  # TEST 2: Verificar cuántas filas nuevas trajo realmente la API ya formateadas
  message("   📥 [TEST BRANCH] Filas NUEVAS descargadas de la API: ", safe_nrow(df_branch_api))
  
  # Combinamos lo viejo de tu CSV con lo nuevo de la API
  df_branch_final <- dplyr::bind_rows(df_branch_local, df_branch_api)
  message("   ➕ [TEST BRANCH] Filas combinadas (Local + API): ", safe_nrow(df_branch_final))
  
  # ------ FIN DEL FLUJO DE DESCARGA REAL ------
  
  if (!is.null(df_branch_final) && nrow(df_branch_final) > 0) {
    names(df_branch_final) <- tolower(names(df_branch_final))
    id_col_b <- intersect(c("ec5_uuid", "uuid"), names(df_branch_final))
    if (length(id_col_b) > 0) {
      df_branch_final <- df_branch_final %>% dplyr::distinct(!!sym(id_col_b[1]), .keep_all = TRUE)
      message("   ✨ [TEST BRANCH] Filas finales tras remover duplicados por ID: ", safe_nrow(df_branch_final))
    } else {
      df_branch_final <- df_branch_final %>% dplyr::distinct()
      message("   ⚠️ [TEST BRANCH] Filas finales tras distinct() genérico: ", safe_nrow(df_branch_final))
    }
  }
  
  # =================================================================
  # 3. Fusión de Estructuras Relacionales
  # =================================================================
  message("\n🔀 [3/3] EJECUTANDO FUSIÓN (MERGE) Y NORMALIZACIÓN...")
  df_merged <- merge_data_branch_entries(df_form_final, df_branch_final)
  message("   🔹 Filas tras merge_data_branch_entries: ", safe_nrow(df_merged))
  
  if (is.null(df_merged) || nrow(df_merged) == 0) return(df_merged)
  
  names(df_merged) <- tolower(names(df_merged))
  columnas_deseadas <- c("fecha_plantado", "especie", "latitud", "longitud")
  columnas_existentes <- intersect(columnas_deseadas, names(df_merged))
  
  if (length(columnas_existentes) > 0) {
    df_merged <- df_merged %>% dplyr::relocate(dplyr::all_of(columnas_existentes))
  }
  df_limpio <- normalize_tree_variables(df_merged)
  return(df_limpio)
}

#' Normalizar y Renombrar Variables para el Monitor de Plantación
#' 
#' Toma el dataframe unificado, remueve los prefijos numéricos de Epicollect5 
#' (ej: "13_especie" -> "especie") y estandariza los nombres de negocio 
#' que esperan los gráficos del server.R.
#' 
#' @param df Dataframe unificado (merge de formulario y branch)
#' @return Un tibble con nombres limpios y estandarizados.
normalize_tree_variables <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
  
  # 1. Asegurar que sea un tibble/dataframe limpio
  df <- dplyr::as_tibble(df)
  
  # 2. Eliminar la columna corrupta si existiera (la que tiene comas pegadas en el nombre)
  columnas_corruptas <- names(df)[stringr::str_detect(names(df), ",")]
  if (length(columnas_corruptas) > 0) {
    df <- df %>% dplyr::select(-dplyr::all_of(columnas_corruptas))
  }
  
  # 3. Limpieza base de nombres (pasar a minúsculas y quitar caracteres extraños)
  names(df) <- tolower(names(df))
  # 4. Remover dinámicamente los prefijos numéricos de Epicollect (ej: "13_especie" -> "especie")
  # Buscamos patrones que arranquen con números seguidos de un guión bajo
  names(df) <- stringr::str_remove(names(df), "^[0-9]+_")
  
  # 5. Mapeo explícito y seguro para el negocio (Estandarización Final)
  # Si quedan remanentes o nombres diferentes, los renombramos con validación previa:
  
  # Especie
  if ("especie" %in% names(df)) {
    # Ya está limpio por el paso 4
  } else if ("13_especie" %in% names(df)) {
    df <- df %>% dplyr::rename(especie = `13_especie`)
  }
  
  # Fecha de Plantado
  if ("fecha_plantado" %in% names(df)) {
    # Ya está limpio
  } else if ("5_fecha_plantado" %in% names(df)) {
    df <- df %>% dplyr::rename(fecha_plantado = `5_fecha_plantado`)
  }
  # cat(head(df$fecha_plantado))
  # Foto del Plantado
  if ("foto_del_plantado" %in% names(df)) {
    # Ya está limpio
  } else if ("16_foto_del_plantado" %in% names(df)) {
    df <- df %>% dplyr::rename(foto_del_plantado = `16_foto_del_plantado`)
  }
  # Sitio / Lugar (El error dice que busca 'sitio', pero tu columna se llama '3_lugar' -> 'lugar')
  if ("lugar" %in% names(df) && !("sitio" %in% names(df))) {
    df <- df %>% dplyr::rename(sitio = lugar)
  } else if ("3_lugar" %in% names(df) && !("sitio" %in% names(df))) {
    df <- df %>% dplyr::rename(sitio = `3_lugar`)
  }
  
  # Latitud y Longitud (Vienen como lat_11_localizacion_del_ o similares)
  col_lat <- names(df)[stringr::str_detect(names(df), "^lat_")]
  col_lng <- names(df)[stringr::str_detect(names(df), "^long_|^lng_")]
  
  if (length(col_lat) > 0 && !("latitud" %in% names(df))) {
    df <- df %>% dplyr::rename(latitud = !!dplyr::sym(col_lat[1]))
  }
  if (length(col_lng) > 0 && !("longitud" %in% names(df))) {
    df <- df %>% dplyr::rename(longitud = !!dplyr::sym(col_lng[1]))
  }
  
  # 6. Conversión explícita de tipos de datos para asegurar el éxito en el Server
  if ("fecha_plantado" %in% names(df)) {
    df$fecha_plantado  <- as.Date(df$fecha_plantado, format = "%d/%m/%Y")
  }
  if ("latitud" %in% names(df)) df$latitud <- as.numeric(df$latitud)
  if ("longitud" %in% names(df)) df$longitud <- as.numeric(df$longitud)
  
  return(df)
}
  