# Library
library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)
library(purrr)
library(janitor)
# Epicollecfunction:
#  Conjunto de funciones que cumplen lo siguiente:
#  - Para poder extraer datos de epicollect5


# FUNCIONES PRINCIPALES ####


#' Sincronizar, Parsear y Unir Datos de Formulario y Branch de Epicollect5
#'
#' @param project_slug Carácter. El slug del proyecto.
#' @param form_ref Carácter. La referencia del formulario principal.
#' @param branch_ref Carácter. La referencia de la branch (subformulario).
#' @param dir_entries Carácter opcional. Ruta al archivo local de entradas (Form).
#' @param dir_branch Carácter opcional. Ruta al archivo local de la branch.
#' @param delimiter Carácter. Separador del CSV local ("," o ";"). Por defecto ",".
#' @param api_token Carácter opcional. Token de autenticación de Epicollect5.
#'
#' @return Un tibble con la unión (left_join) de los datos de Form y Branch, o NULL si falla.
#' @export
sync_and_merge_epicollect <- function(project_slug, 
                                      form_ref, 
                                      branch_ref, 
                                      dir_entries = NULL, 
                                      dir_branch = NULL, 
                                      delimiter = ",", 
                                      api_token = NULL) {
  
  message("--- Iniciando proceso de sincronización Epicollect5 ---")
  
  # 1. Obtención de Datos del Formulario Principal (Entries) ####
  message("[1/3] Sincronizando Formulario Principal...")
  df_form_local <- NULL
  last_date_form <- NULL
  
  if (!is.null(dir_entries) && file.exists(stringr::str_replace_all(dir_entries, "\\\\", "/"))) {
    df_form_local <- get_data_local_csv(dir_entries, delimiter = delimiter)
    last_date_form <- get_last_date_update_local_data(df_form_local)
  }
  
  # Consultar delta a la API
  api_form_list <- get_data_API(project_slug, form_ref, last_date_local_update = last_date_form, api_token = api_token)
  df_form_api <- parse_epicollect_entries(api_form_list)
  
  # Combinar Histórico Local + Delta Nuevo de la API
  df_form_final <- dplyr::bind_rows(df_form_local, df_form_api)
  if (!is.null(df_form_final)) df_form_final <- dplyr::distinct(df_form_final, ec5_uuid, .keep_all = TRUE)
  
  # 2. Obtención de Datos del Subformulario (Branch) ####
  message("[2/3] Sincronizando Subformulario (Branch)...")
  df_branch_local <- NULL
  last_date_branch <- NULL
  
  if (!is.null(dir_branch) && file.exists(stringr::str_replace_all(dir_branch, "\\\\", "/"))) {
    df_branch_local <- get_data_local_csv(dir_branch, delimiter = delimiter)
    last_date_branch <- get_last_date_update_local_data(df_branch_local)
  }
  
  # Consultar delta a la API
  api_branch_list <- get_branch_data(project_slug, form_ref, branch_ref, last_date_local_update = last_date_branch, api_token = api_token)
  df_branch_api <- parse_epicollect_entries(api_branch_list)
  
  # Combinar Histórico Local + Delta Nuevo de la API
  df_branch_final <- dplyr::bind_rows(df_branch_local, df_branch_api)
  if (!is.null(df_branch_final)) df_branch_final <- dplyr::distinct(df_branch_final, ec5_uuid, .keep_all = TRUE)
  
  # 3. Unión Relacional de Ambos Data Frames ####
  
  message("[3/3] Ejecutando fusión relacional (Merge)...")
  df_merged <- merge_data_branch_entries(df_form_final, df_branch_final)
  
  message("--- Proceso completado con éxito ---")
  return(df_merged)
}


#' Leer Archivo CSV de Forma Segura
#'
#' @param file_path Carácter. La ruta hacia el archivo CSV.
#' @param delimiter Carácter. El separador de campos. Por defecto es ",".
#'
#' @return Un tibble (data frame) o NULL si ocurre un error.
#' @export
get_data_local_csv <- function(file_path, delimiter = ",") {
  
  sanitized_path <- stringr::str_replace_all(file_path, "\\\\", "/")
  
  if (!file.exists(sanitized_path)) {
    warning(paste("File not found at:", sanitized_path))
    return(NULL)
  }
  
  data <- tryCatch({
    if (delimiter == ";") {
      readr::read_csv2(sanitized_path, show_col_types = FALSE)
    } else {
      readr::read_csv(sanitized_path, show_col_types = FALSE)
    }
  }, error = function(e) {
    warning(paste("Failed to read CSV file:", e$message))
    return(NULL)
  })
  
  return(data)
  
}


#' Obtener Datos Incrementales desde la API de Epicollect5
#'
#' @param project_slug Carácter. El nombre amigable en la URL del proyecto.
#' @param form_ref Carácter. Referencia del formulario principal.
#' @param last_date_local_update POSIXct/dttm. Fecha máxima que tenemos guardada localmente.
#' @param api_token Carácter opcional. Token Bearer si el proyecto es privado.
#'
#' @return Una lista con todos los registros nuevos combinados, o NULL si ocurre un error terminal.
#' @export
get_data_API <- function(project_slug, form_ref, last_date_local_update = NULL, api_token = NULL) {
  
  initial_params <- list(
    form_ref = form_ref,
    per_page = 500
  )
  
  # Formatear la fecha local al formato esperado por el endpoint principal de Epicollect5
  if (!is.null(last_date_local_update)) {
    initial_params$filter_by <- "created_at"
    initial_params$filter_from <- format(as.POSIXct(last_date_local_update, tz = "UTC"), "%Y-%m-%d %H:%M:%S GMT")
  }
  
  # Delegamos la lógica repetitiva al núcleo compartido
  .fetch_epicollect_pages(
    project_slug = project_slug,
    query_params = initial_params,
    api_token = api_token
  )
}

#' Obtener Datos de una Branch (Sub-formulario) en Epicollect5
#'
#' @param project_slug Carácter. El slug del proyecto.
#' @param form_ref Carácter. La referencia del formulario principal.
#' @param branch_ref Carácter. La referencia específica de la branch (sub-formulario).
#' @param last_date_local_update POSIXct/dttm. Fecha máxima que tenemos guardada localmente.
#' @param api_token Carácter opcional. Token Bearer si el proyecto es privado.
#'
#' @return Una lista con todas las entradas de la branch combinadas, o NULL si ocurre un error terminal.
#' @export
get_branch_data <- function(project_slug, form_ref, branch_ref, last_date_local_update = NULL, api_token = NULL) {
  
  initial_params <- list(
    form_ref = form_ref,
    branch_ref = branch_ref,
    per_page = 500
  )
  
  # Formatear la fecha al formato ISO8601 estricto requerido específicamente por los subformularios
  if (!is.null(last_date_local_update)) {
    initial_params$filter_by <- "created_at"
    initial_params$filter_from <- format(as.POSIXct(last_date_local_update, tz = "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")
  }
  
  # Delegamos al mismo núcleo compartiendo toda la infraestructura de red
  .fetch_epicollect_pages(
    project_slug = project_slug,
    query_params = initial_params,
    api_token = api_token
  )
}


#### FUNCIONES SECUNDARIAS ####

#' Obtener la Fecha de Actualización Más Reciente
#'
#' @param local_data Data frame o tibble. El conjunto de datos locales que contiene la columna 'created_at'.
#'
#' @return Un objeto POSIXct/dttm con la fecha más reciente, o NULL si ocurre un error o los datos están vacíos.
#' @export

get_last_date_update_local_data <- function(local_data){

  if (is.null(local_data) || nrow(local_data) == 0) {
    warning("El conjunto de datos está vacío o es NULL.")
    return(NULL)
  }
  
  if (!"created_at" %in% names(local_data)) {
    warning("La columna 'created_at' no existe en el conjunto de datos proporcionado.")
    return(NULL)
  }
  
  last_update <- tryCatch({
    
    max_date <- max(local_data$created_at, na.rm = TRUE)
    
    if (!is.finite(max_date)) {
      warning("Todos los registros en 'created_at' son NA.")
      return(NULL)
    }
    
    max_date
  }, error = function(e) {
    warning(paste("Error al calcular la fecha más reciente:", e$message))
    return(NULL)
  })
  
  return(last_update)
  
}

#' Convertir Lista de Entradas Epicollect5 a Data Frame
#'
#' @param entries_list Lista. La lista cruda extraída del campo 'entries' de la API.
#' @param clean_names_flag Lógico. Si es TRUE, normaliza los nombres de las columnas a formato snake_case. Por defecto es FALSE.
#'
#' @return Un tibble (data frame) estructurado y limpio, o NULL si la lista está vacía.
#' @export
parse_epicollect_entries <- function(entries_list, clean_names_flag = FALSE) {
  
  # 1. Programación defensiva: Verificar integridad de la entrada
  if (is.null(entries_list) || length(entries_list) == 0) {
    warning("parse_epicollect_entries: La lista de entradas está vacía o es NULL.")
    return(NULL)
  }
  
  # 2. Transformación eficiente de lista anidada a estructura de filas utilizando purrr
  parsed_dataframe <- tryCatch({
    
    # map_dfr mapea cada elemento y los une automáticamente por filas (row-bind)
    # Convertimos cada sublista explícitamente a tibble para conservar los tipos primitivos
    purrr::map_dfr(entries_list, function(single_entry) {
      dplyr::as_tibble(single_entry)
    })
    
  }, error = function(e) {
    warning(paste("Error al transformar la estructura de la lista a data frame:", e$message))
    return(NULL)
  })
  
  # 3. Opcional: Limpieza y estandarización de nombres en producción (Shiny)
  # Epicollect genera nombres incómodos como `2_Agente` o `28_Comisin_de_monito`
  if (clean_names_flag && !is.null(parsed_dataframe)) {
    parsed_dataframe <- parsed_dataframe %>%
      janitor::clean_names()
  }
  
  return(parsed_dataframe)
}

#' Núcleo de Descarga y Paginación Resiliente de Epicollect5
#' 
#' Función interna encargada de manejar de forma automatizada las solicitudes HTTP, 
#' control de reintentos, límites de tasa de la API (429) y la paginación incremental.
#' 
#' @param project_slug Carácter. El identificador del proyecto.
#' @param query_params Lista. Parámetros de consulta (form_ref, branch_ref, etc.).
#' @param api_token Carácter opcional. Token Bearer si el proyecto es privado.
#' 
#' @return Una lista combinada con todos los registros descargados o NULL si hay fallo.
.fetch_epicollect_pages <- function(project_slug, query_params, api_token = NULL) {
  max_attempts <- 5
  current_page <- 1
  all_entries <- list()
  has_more_pages <- TRUE
  
  base_url <- sprintf("https://five.epicollect.net/api/export/entries/%s", project_slug)
  
  # Configurar cabeceras de autenticación si existe Token
  request_headers <- c()
  if (!is.null(api_token)) {
    request_headers <- httr::add_headers(Authorization = paste("Bearer", api_token))
  }
  
  while (has_more_pages) {
    attempt <- 1
    page_success <- FALSE
    parsed_json <- NULL
    
    # Inyectar dinámicamente la página actual a los parámetros base
    query_params$page <- current_page
    
    # Bucle de resiliencia ante errores de red y Rate Limit (429)
    repeat {
      resp <- try(httr::GET(base_url, query = query_params, config = request_headers, httr::timeout(20)), silent = TRUE)
      
      if (inherits(resp, "try-error")) {
        if (attempt >= max_attempts) {
          warning("fetch_epicollect_pages: Error de red persistente tras múltiples intentos.")
          return(NULL)
        }
        Sys.sleep(2 * attempt)
        attempt <- attempt + 1
        next
      }
      
      status <- httr::status_code(resp)
      
      # Manejo específico de Límite de Tasa (Too Many Requests)
      if (status == 429) {
        if (attempt >= max_attempts) {
          warning(sprintf("fetch_epicollect_pages: Límite de tasa (429) excedido en la página %d", current_page))
          return(NULL)
        }
        wait_time <- 10 * attempt
        message(sprintf("Rate limit alcanzado. Esperando %ds en página %d...", wait_time, current_page))
        Sys.sleep(wait_time)
        attempt <- attempt + 1
        next
      }
      
      # Manejo de otros errores HTTP (400, 404, 500, etc.)
      if (status != 200) {
        if (attempt >= max_attempts) {
          warning(sprintf("fetch_epicollect_pages: Estado HTTP %s inesperado en la página %d", status, current_page))
          return(NULL)
        }
        Sys.sleep(2 * attempt)
        attempt <- attempt + 1
        next
      }
      
      # Des-serialización segura del contenido JSON
      parsed_text <- try(httr::content(resp, as = "text", encoding = "UTF-8"), silent = TRUE)
      if (inherits(parsed_text, "try-error")) return(NULL)
      
      parsed_json <- try(jsonlite::fromJSON(parsed_text, simplifyVector = FALSE), silent = TRUE)
      if (inherits(parsed_json, "try-error")) return(NULL)
      
      page_success <- TRUE
      break
    }
    
    # Evaluar contenido de la página y control de la paginación secuencial
    if (page_success && !is.null(parsed_json)) {
      entries_fetched <- parsed_json$data$entries
      
      if (length(entries_fetched) > 0) {
        all_entries <- c(all_entries, entries_fetched)
      }
      
      current_page_meta <- parsed_json$meta$current_page
      last_page_meta <- parsed_json$meta$last_page
      
      if (!is.null(current_page_meta) && !is.null(last_page_meta) && current_page_meta < last_page_meta) {
        current_page <- current_page + 1
        Sys.sleep(0.5) # Pausa preventiva de cortesía
      } else {
        has_more_pages <- FALSE
      }
    } else {
      has_more_pages <- FALSE
    }
  }
  
  return(all_entries)
}


#' Unir Data Frames de Formulario y Branch mediante llaves UUID de Epicollect5
#'
#' @param df_form Data frame/tibble. Datos del formulario principal conteniendo `ec5_uuid`.
#' @param df_branch Data frame/tibble. Datos de la branch conteniendo `ec5_branch_owner_uuid`.
#'
#' @return Un tibble con los datos vinculados mediante un left_join.
#' @export
merge_data_branch_entries <- function(df_form, df_branch) {
  
  if (is.null(df_form) || nrow(df_form) == 0) {
    warning("merge_data_branch_entries: El data frame de Formulario está vacío o es NULL. Imposible unir.")
    return(df_branch)
  }
  
  if (is.null(df_branch) || nrow(df_branch) == 0) {
    warning("merge_data_branch_entries: El data frame de Branch está vacío o es NULL. Retornando solo Formulario.")
    return(df_form)
  }
  
  # Validación estricta de llaves primarias y foráneas antes del join
  if (!"ec5_uuid" %in% names(df_form)) {
    stop("Falta la columna clave 'ec5_uuid' en el Formulario Principal.")
  }
  
  if (!"ec5_branch_owner_uuid" %in% names(df_branch)) {
    stop("Falta la columna de vinculación 'ec5_branch_owner_uuid' en la Branch.")
  }
  
  # Se ejecuta un left_join para conservar la estructura jerárquica: de la Branch hacia su Formulario Padre
  # Nota: Se usa sufijo para evitar colisiones en metadatos comunes compartidos (ej. created_at, title)
  df_joined <- df_branch %>%
    dplyr::left_join(df_form, by = c("ec5_branch_owner_uuid" = "ec5_uuid"), suffix = c("_branch", "_form"))
  
  return(df_joined)
}



library(testthat)
library(dplyr)
library(tibble)


# TEST 1: VERIFICACIÓN DE LA UNIÓN RELACIONAL (Form + Branch) ####

context("Test 1: Fusión relacional de llaves UUID (Merge)")

test_that("La unión une correctamente un formulario padre con múltiples branches hijas usando los sufijos correctos", {
  
  # SIMULACIÓN (Mock) de datos del Formulario Principal (Entries)
  mock_form <- tibble::tibble(
    ec5_uuid = c("uuid-padre-001", "uuid-padre-002"),
    created_at = c("2026-01-01 10:00:00", "2026-01-02 11:00:00"),
    inspector = c("Juan Perez", "Maria Lopez")
  )
  
  # SIMULACIÓN (Mock) de datos de la Branch (Subformulario - ej: múltiples árboles por formulario)
  mock_branch <- tibble::tibble(
    ec5_uuid = c("uuid-hijo-abc", "uuid-hijo-def", "uuid-hijo-ghi"),
    ec5_branch_owner_uuid = c("uuid-padre-001", "uuid-padre-001", "uuid-padre-002"),
    created_at = c("2026-01-01 10:05:00", "2026-01-01 10:12:00", "2026-01-02 11:15:00"),
    especie_arbol = c("Fresno", "Jacarandá", "Lapacho")
  )
  
  # Ejecución de la función bajo prueba
  resultado_merge <- merge_data_branch_entries(mock_form, mock_branch)
  
  # VALIDACIONES DE QA:
  # 1. El número de filas final debe ser igual al de las branches (la unidad de análisis más baja)
  expect_equal(nrow(resultado_merge), 3)
  
  # 2. Comprobar que la relación relacional 1:N funcionó (el padre 001 debe repetirse dos veces)
  expect_equal(sum(resultado_merge$ec5_branch_owner_uuid == "uuid-padre-001"), 2)
  
  # 3. Comprobar que los sufijos evitaron la colisión de la columna duplicada del sistema 'created_at'
  expect_true("created_at_branch" %in% names(resultado_merge))
  expect_true("created_at_form" %in% names(resultado_merge))
  
  # 4. Comprobar que la información se alineó correctamente por fila
  fresno_row <- resultado_merge %>% dplyr::filter(especie_arbol == "Fresno")
  expect_equal(fresno_row$inspector, "Juan Perez")
})


# TEST 2: INTEGRACIÓN CON EXISTENCIA DE DATOS LOCALES (Estrategia Delta) ####

context("Test 2: Flujo incremental con datos locales preexistentes")

test_that("La función maestra lee datos locales, extrae la fecha máxima y no duplica registros al fusionar", {
  
  # 1. Creamos un entorno controlado para inyectar los mocks locales
  mock_env <- new.env(parent = asNamespace("dplyr")) # o el entorno base de tu app
  
  # 2. Mock de lectura local (Simula el contenido de los archivos CSV)
  mock_env$get_data_local_csv <- function(file_path, delimiter) {
    if (file_path == CONSTANTS$DIRECTORY_ENTRIES) {
      return(tibble::tibble(ec5_uuid = "uuid-historico-form", created_at = as.POSIXct("2026-06-01 UTC")))
    }
    if (file_path == CONSTANTS$DIRECTORY_BRANCH) {
      return(tibble::tibble(ec5_uuid = "uuid-historico-branch", ec5_branch_owner_uuid = "uuid-historico-form", created_at = as.POSIXct("2026-06-01 UTC")))
    }
    return(NULL)
  }
  
  # 3. Mock de la extracción de fechas basada en el mock de arriba
  mock_env$get_last_date_update_local_data <- function(local_data) {
    return(as.POSIXct("2026-06-01 UTC"))
  }
  
  # 4. Mock de las funciones de la API
  mock_env$get_data_API <- function(project_slug, form_ref, last_date_local_update, api_token) {
    expect_equal(last_date_local_update, as.POSIXct("2026-06-01 UTC"))
    return(list(list(ec5_uuid = "uuid-nuevo-form", created_at = "2026-07-01 09:00:00")))
  }
  
  mock_env$get_branch_data <- function(project_slug, form_ref, branch_ref, last_date_local_update, api_token) {
    expect_equal(last_date_local_update, as.POSIXct("2026-06-01 UTC"))
    return(list(list(ec5_uuid = "uuid-nuevo-branch", ec5_branch_owner_uuid = "uuid-nuevo-form", created_at = "2026-07-01 09:05:00")))
  }
  
  mock_env$parse_epicollect_entries <- function(entries_list) {
    if (is.null(entries_list) || length(entries_list) == 0) return(NULL)
    # Simulación rápida de parseo para el test
    if (entries_list[[1]]$ec5_uuid == "uuid-nuevo-form") {
      return(tibble::tibble(ec5_uuid = "uuid-nuevo-form", created_at = as.POSIXct("2026-07-01 UTC")))
    }
    return(tibble::tibble(ec5_uuid = "uuid-nuevo-branch", ec5_branch_owner_uuid = "uuid-nuevo-form", created_at = as.POSIXct("2026-07-01 UTC")))
  }
  
  mock_env$merge_data_branch_entries <- merge_data_branch_entries
  
  # Vinculamos temporalmente la función maestra a nuestro entorno con Mocks
  environment(sync_and_merge_epicollect) <- mock_env
  
  # 5. Ejecución limpia (Ya no necesitamos with_mock de funciones base)
  resultado_final <- sync_and_merge_epicollect(
    project_slug = CONSTANTS$PROYECT_SLUG,
    form_ref     = CONSTANTS$FORM_REF,
    branch_ref   = CONSTANTS$FORM_REF_BRANCH_REF,
    dir_entries  = CONSTANTS$DIRECTORY_ENTRIES, # Pasamos los nombres para activar los ifs del mock
    dir_branch   = CONSTANTS$DIRECTORY_BRANCH
  )
  
  # VALIDACIONES FINALES DE INTEGRACIÓN:
  expect_s3_class(resultado_final, "data.frame")
  expect_true("uuid-historico-branch" %in% resultado_final$ec5_uuid)
  expect_true("uuid-nuevo-branch" %in% resultado_final$ec5_uuid)
})


