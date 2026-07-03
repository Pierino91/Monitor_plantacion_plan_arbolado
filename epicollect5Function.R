# Library ####
library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)
library(purrr)
library(janitor)
library(data.table)
library(tibble)

# Epicollectfunction:
#  Conjunto de funciones para extraer, sincronizar y unir datos de Epicollect5
#  (Formulario Principal + Branch/Subformulario) de forma incremental (delta).


# FUNCIONES PRINCIPALES ####

#' Sincronizar, Parsear y Unir Datos de Formulario y Branch de Epicollect5
#'
#' Orquesta el flujo completo: lee el histórico local (si existe), calcula la
#' fecha de corte, descarga el delta desde la API de Epicollect5, deduplica
#' priorizando el registro más reciente y finalmente une (join) Formulario
#' Principal con su Branch mediante las llaves UUID.
#'
#' @param project_slug Carácter (largo 1). El slug del proyecto en Epicollect5.
#' @param form_ref Carácter (largo 1). La referencia (`ref`) del formulario principal.
#' @param branch_ref Carácter (largo 1). La referencia (`ref`) de la branch (subformulario).
#' @param dir_entries Carácter (largo 1) opcional. Ruta al CSV local histórico del Form.
#' @param dir_branch Carácter (largo 1) opcional. Ruta al CSV local histórico de la Branch.
#' @param delimiter Carácter (largo 1). Separador del CSV local (","  o ";"). Por defecto ",".
#' @param api_token Carácter (largo 1) opcional. Token Bearer de Epicollect5 si el proyecto es privado.
#'
#' @return Un `tibble` (`tbl_df`) resultante del `left_join` Branch -> Form.
#'   Si `df_form` está vacío se retorna únicamente `df_branch` (y viceversa).
#' @export
sync_and_merge_epicollect <- function(project_slug,
                                      form_ref,
                                      branch_ref,
                                      dir_entries = NULL,
                                      dir_branch = NULL,
                                      delimiter = ",",
                                      api_token = NULL) {
  
  message("--- Iniciando proceso de sincronizacion Epicollect5 ---")
  
  # 1. Obtencion de Datos del Formulario Principal (Entries) ####
  message("[1/3] Sincronizando Formulario Principal...")
  df_form_local <- NULL
  last_date_form <- NULL
  
  if (!is.null(dir_entries) && file.exists(stringr::str_replace_all(dir_entries, "\\\\", "/"))) {
    df_form_local <- get_data_local_csv(dir_entries, delimiter = delimiter)
    last_date_form <- get_last_date_update_local_data(df_form_local)
  }
  
  api_form_list <- get_data_API(project_slug, form_ref, last_date_local_update = last_date_form, api_token = api_token)
  df_form_api <- parse_epicollect_entries(api_form_list)
  
  df_form_final <- dplyr::bind_rows(df_form_local, df_form_api)
  
  # Guard reforzado: solo deduplicamos si hay filas Y existe la llave (evita error
  # de dplyr::distinct sobre un tibble vacio de 0 columnas devuelto por el edge-case de la API)
  if (!is.null(df_form_final) && nrow(df_form_final) > 0 && "ec5_uuid" %in% names(df_form_final)) {
    # Se ordena por fecha descendente ANTES de deduplicar: si la API trae una version
    # editada de un registro que ya teniamos en local, debe prevalecer la mas reciente
    df_form_final <- df_form_final %>%
      dplyr::arrange(dplyr::desc(created_at)) %>%
      dplyr::distinct(ec5_uuid, .keep_all = TRUE)
  }
  
  # 2. Obtencion de Datos del Subformulario (Branch) ####
  message("[2/3] Sincronizando Subformulario (Branch)...")
  df_branch_local <- NULL
  last_date_branch <- NULL
  
  if (!is.null(dir_branch) && file.exists(stringr::str_replace_all(dir_branch, "\\\\", "/"))) {
    df_branch_local <- get_data_local_csv(dir_branch, delimiter = delimiter)
    last_date_branch <- get_last_date_update_local_data(df_branch_local)
  }
  
  api_branch_list <- get_branch_data(project_slug, form_ref, branch_ref, last_date_local_update = last_date_branch, api_token = api_token)
  df_branch_api <- parse_epicollect_entries(api_branch_list)
  
  df_branch_final <- dplyr::bind_rows(df_branch_local, df_branch_api)
  
  if (!is.null(df_branch_final) && nrow(df_branch_final) > 0 && "ec5_uuid" %in% names(df_branch_final)) {
    df_branch_final <- df_branch_final %>%
      dplyr::arrange(dplyr::desc(created_at)) %>%
      dplyr::distinct(ec5_uuid, .keep_all = TRUE)
  }
  
  # 3. Union Relacional de Ambos Data Frames ####
  message("[3/3] Ejecutando fusion relacional (Merge)...")
  df_merged <- merge_data_branch_entries(df_form_final, df_branch_final)
  
  message("--- Proceso completado con exito ---")
  return(df_merged)
}


#' Leer Archivo CSV de Forma Segura
#'
#' @param file_path Carácter (largo 1). Ruta hacia el archivo CSV local.
#' @param delimiter Carácter (largo 1). Separador de campos ("," o ";"). Por defecto ",".
#'
#' @return Un `tibble` con el contenido del CSV, o `NULL` si el archivo no existe
#'   o falla la lectura (columnas y tipos inferidos por `readr`).
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


#' Obtener Datos Incrementales del Formulario Principal desde la API de Epicollect5
#'
#' @param project_slug Carácter (largo 1). Slug del proyecto en la URL.
#' @param form_ref Carácter (largo 1). Referencia del formulario principal.
#' @param last_date_local_update `POSIXct`/`dttm` opcional (largo 1). Fecha máxima ya almacenada localmente.
#' @param api_token Carácter (largo 1) opcional. Token Bearer si el proyecto es privado.
#'
#' @return Una `list` con los registros nuevos (posiblemente vacía, `list()`),
#'   o `NULL` si ocurrió un error terminal de red/HTTP en la primera página.
#' @export
get_data_API <- function(project_slug, form_ref, last_date_local_update = NULL, api_token = NULL) {
  
  initial_params <- list(
    form_ref = form_ref,
    per_page = 500
  )
  
  if (!is.null(last_date_local_update)) {
    initial_params$filter_by <- "created_at"
    initial_params$filter_from <- format(as.POSIXct(last_date_local_update, tz = "UTC"), "%Y-%m-%d %H:%M:%S GMT")
  }
  
  .fetch_epicollect_pages(
    project_slug = project_slug,
    query_params = initial_params,
    api_token = api_token
  )
}

#' Obtener Datos de una Branch (Sub-formulario) en Epicollect5
#'
#' @param project_slug Carácter (largo 1). Slug del proyecto.
#' @param form_ref Carácter (largo 1). Referencia del formulario principal (padre).
#' @param branch_ref Carácter (largo 1). Referencia específica de la branch.
#' @param last_date_local_update `POSIXct`/`dttm` opcional (largo 1). Fecha máxima ya almacenada localmente.
#' @param api_token Carácter (largo 1) opcional. Token Bearer si el proyecto es privado.
#'
#' @return Una `list` con las entradas de la branch (posiblemente vacía, `list()`),
#'   o `NULL` si ocurrió un error terminal de red/HTTP en la primera página.
#' @export
get_branch_data <- function(project_slug, form_ref, branch_ref, last_date_local_update = NULL, api_token = NULL) {
  
  initial_params <- list(
    form_ref = form_ref,
    branch_ref = branch_ref,
    per_page = 500
  )
  
  # Formato ISO8601 estricto requerido especificamente por el endpoint de subformularios
  if (!is.null(last_date_local_update)) {
    initial_params$filter_by <- "created_at"
    initial_params$filter_from <- format(as.POSIXct(last_date_local_update, tz = "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")
  }
  
  .fetch_epicollect_pages(
    project_slug = project_slug,
    query_params = initial_params,
    api_token = api_token
  )
}


# FUNCIONES SECUNDARIAS ####

#' Obtener la Fecha de Actualización Más Reciente de un Dataset Local
#'
#' @param local_data `data.frame`/`tibble`. Dataset local que debe contener la columna `created_at`
#'   (como `POSIXct`/`Date`, o como texto parseable en formato `YYYY-MM-DD HH:MM:SS`).
#'
#' @return Un `POSIXct` (largo 1) con la fecha más reciente, o `NULL` si los datos están
#'   vacíos, falta la columna, o todos los valores son `NA`/no parseables.
#' @export
get_last_date_update_local_data <- function(local_data) {
  
  if (is.null(local_data) || nrow(local_data) == 0) {
    warning("El conjunto de datos esta vacio o es NULL.")
    return(NULL)
  }
  
  if (!"created_at" %in% names(local_data)) {
    warning("La columna 'created_at' no existe en el conjunto de datos proporcionado.")
    return(NULL)
  }
  
  created_at_vec <- local_data$created_at
  
  # Coercion defensiva: si 'created_at' llego como texto (comun al leer CSV en modo laxo),
  # se parsea explicitamente para evitar que max() compare lexicograficamente strings mal ordenados
  if (!inherits(created_at_vec, "POSIXct") && !inherits(created_at_vec, "Date")) {
    created_at_vec <- suppressWarnings(lubridate::ymd_hms(created_at_vec, tz = "UTC", quiet = TRUE))
  }
  
  last_update <- tryCatch({
    max_date <- max(created_at_vec, na.rm = TRUE)
    
    if (!is.finite(max_date)) {
      warning("Todos los registros en 'created_at' son NA o no pudieron ser parseados.")
      return(NULL)
    }
    
    max_date
  }, error = function(e) {
    warning(paste("Error al calcular la fecha mas reciente:", e$message))
    return(NULL)
  })
  
  return(last_update)
}

#' Convertir Lista de Entradas Epicollect5 a Data Frame
#'
#' Transforma la lista anidada devuelta por la API (o por `.fetch_epicollect_pages`)
#' en un `tibble` plano. Usa `data.table::rbindlist` como camino rápido (óptimo para
#' +10.000 registros) con `dplyr::bind_rows` como respaldo ante estructuras anidadas.
#'
#' @param entries_list `list`. Lista cruda extraída del campo `entries` de la API.
#'   Puede ser `NULL` o `list()` (caso sin resultados nuevos).
#' @param clean_names_flag Lógico (largo 1). Si `TRUE`, normaliza los nombres de columnas
#'   a snake_case vía `janitor::clean_names()`. Por defecto `FALSE`.
#'
#' @return Un `tibble`: vacío (0 filas, 0 columnas) si `entries_list` está vacía/NULL;
#'   estructurado con los datos si hay registros; o `NULL` únicamente ante un error
#'   irrecuperable de transformación (tanto `rbindlist` como `bind_rows` fallan).
#' @export
parse_epicollect_entries <- function(entries_list, clean_names_flag = FALSE) {
  
  # 1. Programacion defensiva: lista vacia -> tibble vacio ESTRUCTURADO (no NULL),
  #    para que dplyr::bind_rows(), nrow() y %in% names() aguas abajo nunca reciban NULL.
  #    No se fuerzan columnas fijas (ec5_uuid, etc.) porque los formularios de Epicollect5
  #    tienen esquema dinamico definido por el usuario; el guard de nrow()>0 en
  #    sync_and_merge_epicollect() protege el resto del pipeline ante este caso.
  if (is.null(entries_list) || length(entries_list) == 0) {
    warning("parse_epicollect_entries: La lista de entradas esta vacia o es NULL. Se retorna un tibble vacio.")
    return(tibble::tibble())
  }
  
  # 2. Saneamiento: un campo JSON null llega como list() vacio dentro de la entrada,
  #    lo que puede colapsar la longitud de esa fila y romper el row-bind. Se normaliza a NA.
  sanitized_list <- lapply(entries_list, function(single_entry) {
    is_null_field <- vapply(single_entry, is.null, logical(1))
    if (any(is_null_field)) single_entry[is_null_field] <- NA
    single_entry
  })
  
  # 3. Transformacion eficiente de lista anidada a data frame
  parsed_dataframe <- tryCatch({
    # Camino rapido: data.table::rbindlist es sustancialmente mas rapido y usa menos memoria
    # que purrr::map_dfr + as_tibble para +10.000 registros. map_dfr reconstruye/realoca la
    # tabla en cada iteracion (costo cuadratico); rbindlist pre-asigna memoria una sola vez en C.
    # fill=TRUE tolera formularios con campos condicionales (ramas/preguntas opcionales),
    # muy comunes en Epicollect5.
    dt <- data.table::rbindlist(sanitized_list, fill = TRUE, use.names = TRUE)
    tibble::as_tibble(dt)
  }, error = function(e) {
    # Camino de respaldo: columnas con listas anidadas (preguntas multiple-choice, adjuntos)
    # pueden hacer fallar a rbindlist. dplyr::bind_rows es mas tolerante a costa de velocidad.
    warning(paste("rbindlist fallo (posibles campos anidados), usando dplyr::bind_rows como respaldo:", e$message))
    tryCatch({
      dplyr::bind_rows(sanitized_list)
    }, error = function(e2) {
      warning(paste("Error critico al transformar la lista a data frame:", e2$message))
      NULL
    })
  })
  
  if (is.null(parsed_dataframe)) return(NULL)
  
  # 4. Limpieza y estandarizacion opcional de nombres (Shiny)
  if (clean_names_flag) {
    parsed_dataframe <- parsed_dataframe %>% janitor::clean_names()
  }
  
  return(parsed_dataframe)
}

#' Núcleo de Descarga y Paginación Resiliente de Epicollect5
#'
#' Función interna que maneja las solicitudes HTTP, reintentos, rate limiting (429,
#' con circuit breaker global y respeto de `Retry-After`), errores no reintentables
#' (4xx) y paginación incremental. Ante cualquier fallo terminal retorna los
#' registros ya descargados en vez de descartar el progreso acumulado.
#'
#' @param project_slug Carácter (largo 1). Identificador del proyecto.
#' @param query_params `list`. Parámetros de consulta (`form_ref`, `branch_ref`, `filter_from`, etc.).
#' @param api_token Carácter (largo 1) opcional. Token Bearer si el proyecto es privado.
#'
#' @return Una `list` con todos los registros descargados (puede ser `list()` si no
#'   hay resultados, o una lista parcial si la descarga se interrumpió por un fallo
#'   terminal en una página intermedia). Nunca retorna `NULL`.
#' @keywords internal
.fetch_epicollect_pages <- function(project_slug, query_params, api_token = NULL) {
  
  if (!is.character(project_slug) || length(project_slug) != 1 || !nzchar(project_slug)) {
    warning(".fetch_epicollect_pages: 'project_slug' invalido o vacio.")
    return(list())
  }
  
  max_attempts_per_page  <- 5
  max_global_rate_limits <- 15   # Circuit breaker: evita reintentar indefinidamente si la API bloqueo la IP
  current_page           <- 1
  all_entries             <- list()
  has_more_pages          <- TRUE
  total_rate_limit_hits   <- 0
  
  base_url <- sprintf("https://five.epicollect.net/api/export/entries/%s", project_slug)
  
  request_headers <- NULL
  if (!is.null(api_token) && nzchar(api_token)) {
    request_headers <- httr::add_headers(Authorization = paste("Bearer", api_token))
  }
  
  while (has_more_pages) {
    attempt <- 1
    page_success <- FALSE
    parsed_json <- NULL
    
    query_params$page <- current_page
    
    repeat {
      
      # --- Circuit breaker global de Rate Limit: corta el proceso si la IP parece bloqueada ---
      if (total_rate_limit_hits >= max_global_rate_limits) {
        warning(sprintf(
          ".fetch_epicollect_pages: abortado tras %d bloqueos por Rate Limit (429), posible bloqueo de IP. Se retornan %d registros ya obtenidos.",
          total_rate_limit_hits, length(all_entries)
        ))
        return(all_entries)
      }
      
      resp <- try(httr::GET(base_url, query = query_params, config = request_headers, httr::timeout(20)), silent = TRUE)
      
      # --- Fallos de red / timeout ---
      if (inherits(resp, "try-error")) {
        if (attempt >= max_attempts_per_page) {
          warning(sprintf(
            ".fetch_epicollect_pages: error de red persistente en la pagina %d tras %d intentos. Se retornan %d registros ya obtenidos.",
            current_page, max_attempts_per_page, length(all_entries)
          ))
          return(all_entries)
        }
        Sys.sleep(2 * attempt)
        attempt <- attempt + 1
        next
      }
      
      status <- httr::status_code(resp)
      
      # --- Rate limit (429): respeta cabecera Retry-After si esta presente ---
      if (status == 429) {
        total_rate_limit_hits <- total_rate_limit_hits + 1
        
        if (attempt >= max_attempts_per_page) {
          warning(sprintf(
            ".fetch_epicollect_pages: limite de tasa (429) excedido en la pagina %d. Se retornan %d registros ya obtenidos.",
            current_page, length(all_entries)
          ))
          return(all_entries)
        }
        
        retry_after_header <- httr::headers(resp)[["retry-after"]]
        wait_time <- if (!is.null(retry_after_header) && !is.na(suppressWarnings(as.numeric(retry_after_header)))) {
          as.numeric(retry_after_header)
        } else {
          10 * attempt
        }
        
        message(sprintf("Rate limit alcanzado. Esperando %ds en pagina %d...", wait_time, current_page))
        Sys.sleep(wait_time)
        attempt <- attempt + 1
        next
      }
      
      # --- Errores de cliente no reintentables (reintentar no soluciona nada) ---
      if (status %in% c(400, 401, 403, 404, 422)) {
        warning(sprintf(
          ".fetch_epicollect_pages: error de cliente HTTP %d en la pagina %d (no reintentable). Se retornan %d registros ya obtenidos.",
          status, current_page, length(all_entries)
        ))
        return(all_entries)
      }
      
      # --- Otros errores HTTP (5xx, etc.): si son reintentables ---
      if (status != 200) {
        if (attempt >= max_attempts_per_page) {
          warning(sprintf(
            ".fetch_epicollect_pages: estado HTTP %s inesperado en la pagina %d tras %d intentos. Se retornan %d registros ya obtenidos.",
            status, current_page, max_attempts_per_page, length(all_entries)
          ))
          return(all_entries)
        }
        Sys.sleep(2 * attempt)
        attempt <- attempt + 1
        next
      }
      
      # --- Deserializacion segura del contenido JSON ---
      parsed_text <- try(httr::content(resp, as = "text", encoding = "UTF-8"), silent = TRUE)
      if (inherits(parsed_text, "try-error") || is.null(parsed_text) || !nzchar(parsed_text)) {
        warning(sprintf(".fetch_epicollect_pages: respuesta vacia o ilegible en la pagina %d. Se retornan %d registros ya obtenidos.", current_page, length(all_entries)))
        return(all_entries)
      }
      
      parsed_json <- try(jsonlite::fromJSON(parsed_text, simplifyVector = FALSE), silent = TRUE)
      if (inherits(parsed_json, "try-error")) {
        warning(sprintf(".fetch_epicollect_pages: JSON malformado en la pagina %d. Se retornan %d registros ya obtenidos.", current_page, length(all_entries)))
        return(all_entries)
      }
      
      page_success <- TRUE
      break
    }
    
    if (page_success) {
      
      # Edge case: JSON valido pero sin data$entries (o data ausente) -> se trata como
      # pagina sin resultados nuevos, NO como error fatal que rompa el bind_rows aguas abajo
      entries_fetched <- tryCatch({
        candidate <- parsed_json[["data"]][["entries"]]
        if (is.null(candidate)) list() else candidate
      }, error = function(e) list())
      
      if (length(entries_fetched) > 0) {
        all_entries <- c(all_entries, entries_fetched)
      }
      
      current_page_meta <- tryCatch(parsed_json[["meta"]][["current_page"]], error = function(e) NULL)
      last_page_meta     <- tryCatch(parsed_json[["meta"]][["last_page"]], error = function(e) NULL)
      
      if (!is.null(current_page_meta) && !is.null(last_page_meta) && current_page_meta < last_page_meta) {
        current_page <- current_page + 1
        Sys.sleep(0.5) # Pausa preventiva de cortesia
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
#' @param df_form `data.frame`/`tibble`. Datos del formulario principal, debe contener `ec5_uuid`.
#' @param df_branch `data.frame`/`tibble`. Datos de la branch, debe contener `ec5_branch_owner_uuid`.
#'
#' @return Un `tibble` con el `left_join` Branch -> Form (una fila por registro de Branch).
#'   Si `df_form` está vacío/NULL retorna `df_branch` sin modificar; si `df_branch` está
#'   vacío/NULL retorna `df_form` sin modificar. Lanza `stop()` si faltan las columnas llave.
#' @export
merge_data_branch_entries <- function(df_form, df_branch) {
  
  if (is.null(df_form) || nrow(df_form) == 0) {
    warning("merge_data_branch_entries: El data frame de Formulario esta vacio o es NULL. Imposible unir, se retorna solo Branch.")
    return(df_branch)
  }
  
  if (is.null(df_branch) || nrow(df_branch) == 0) {
    warning("merge_data_branch_entries: El data frame de Branch esta vacio o es NULL. Retornando solo Formulario.")
    return(df_form)
  }
  
  if (!"ec5_uuid" %in% names(df_form)) {
    stop("Falta la columna clave 'ec5_uuid' en el Formulario Principal.")
  }
  
  if (!"ec5_branch_owner_uuid" %in% names(df_branch)) {
    stop("Falta la columna de vinculacion 'ec5_branch_owner_uuid' en la Branch.")
  }
  
  # Coercion defensiva de las llaves a character: un join entre tipos disimiles
  # (ej. character vs factor) obliga a dplyr a copiar/recodificar ambos vectores
  # completos en memoria antes de poder compararlos
  df_form <- df_form %>% dplyr::mutate(ec5_uuid = as.character(ec5_uuid))
  df_branch <- df_branch %>% dplyr::mutate(ec5_branch_owner_uuid = as.character(ec5_branch_owner_uuid))
  
  # dplyr::left_join (>=1.0) implementa internamente un hash-join de complejidad O(n+m):
  # no requiere indices explicitos adicionales y evita el producto cartesiano intermedio
  # que generaria una aproximacion ingenua tipo merge(sort=TRUE)
  df_joined <- df_branch %>%
    dplyr::left_join(df_form, by = c("ec5_branch_owner_uuid" = "ec5_uuid"), suffix = c("_branch", "_form"))
  
  return(df_joined)
}


# TESTS ####

library(testthat)
library(dplyr)
library(tibble)


# TEST 1: VERIFICACION DE LA UNION RELACIONAL (Form + Branch) ####

context("Test 1: Fusion relacional de llaves UUID (Merge)")

test_that("La union une correctamente un formulario padre con multiples branches hijas usando los sufijos correctos", {
  
  mock_form <- tibble::tibble(
    ec5_uuid = c("uuid-padre-001", "uuid-padre-002"),
    created_at = c("2026-01-01 10:00:00", "2026-01-02 11:00:00"),
    inspector = c("Juan Perez", "Maria Lopez")
  )
  
  mock_branch <- tibble::tibble(
    ec5_uuid = c("uuid-hijo-abc", "uuid-hijo-def", "uuid-hijo-ghi"),
    ec5_branch_owner_uuid = c("uuid-padre-001", "uuid-padre-001", "uuid-padre-002"),
    created_at = c("2026-01-01 10:05:00", "2026-01-01 10:12:00", "2026-01-02 11:15:00"),
    especie_arbol = c("Fresno", "Jacaranda", "Lapacho")
  )
  
  resultado_merge <- merge_data_branch_entries(mock_form, mock_branch)
  
  expect_equal(nrow(resultado_merge), 3)
  expect_equal(sum(resultado_merge$ec5_branch_owner_uuid == "uuid-padre-001"), 2)
  expect_true("created_at_branch" %in% names(resultado_merge))
  expect_true("created_at_form" %in% names(resultado_merge))
  
  fresno_row <- resultado_merge %>% dplyr::filter(especie_arbol == "Fresno")
  expect_equal(fresno_row$inspector, "Juan Perez")
})


# TEST 2: INTEGRACION CON EXISTENCIA DE DATOS LOCALES (Estrategia Delta) ####

context("Test 2: Flujo incremental con datos locales preexistentes")

test_that("La funcion maestra lee datos locales, extrae la fecha maxima y no duplica registros al fusionar", {
  
  mock_env <- new.env(parent = asNamespace("dplyr"))
  
  mock_env$get_data_local_csv <- function(file_path, delimiter) {
    if (file_path == CONSTANTS$DIRECTORY_ENTRIES) {
      return(tibble::tibble(ec5_uuid = "uuid-historico-form", created_at = as.POSIXct("2026-06-01 UTC")))
    }
    if (file_path == CONSTANTS$DIRECTORY_BRANCH) {
      return(tibble::tibble(ec5_uuid = "uuid-historico-branch", ec5_branch_owner_uuid = "uuid-historico-form", created_at = as.POSIXct("2026-06-01 UTC")))
    }
    return(NULL)
  }
  
  mock_env$get_last_date_update_local_data <- function(local_data) {
    return(as.POSIXct("2026-06-01 UTC"))
  }
  
  mock_env$get_data_API <- function(project_slug, form_ref, last_date_local_update, api_token) {
    expect_equal(last_date_local_update, as.POSIXct("2026-06-01 UTC"))
    return(list(list(ec5_uuid = "uuid-nuevo-form", created_at = "2026-07-01 09:00:00")))
  }
  
  mock_env$get_branch_data <- function(project_slug, form_ref, branch_ref, last_date_local_update, api_token) {
    expect_equal(last_date_local_update, as.POSIXct("2026-06-01 UTC"))
    return(list(list(ec5_uuid = "uuid-nuevo-branch", ec5_branch_owner_uuid = "uuid-nuevo-form", created_at = "2026-07-01 09:05:00")))
  }
  
  mock_env$parse_epicollect_entries <- function(entries_list) {
    if (is.null(entries_list) || length(entries_list) == 0) return(tibble::tibble())
    if (entries_list[[1]]$ec5_uuid == "uuid-nuevo-form") {
      return(tibble::tibble(ec5_uuid = "uuid-nuevo-form", created_at = as.POSIXct("2026-07-01 UTC")))
    }
    return(tibble::tibble(ec5_uuid = "uuid-nuevo-branch", ec5_branch_owner_uuid = "uuid-nuevo-form", created_at = as.POSIXct("2026-07-01 UTC")))
  }
  
  mock_env$merge_data_branch_entries <- merge_data_branch_entries
  
  environment(sync_and_merge_epicollect) <- mock_env
  
  resultado_final <- sync_and_merge_epicollect(
    project_slug = CONSTANTS$PROYECT_SLUG,
    form_ref     = CONSTANTS$FORM_REF,
    branch_ref   = CONSTANTS$FORM_REF_BRANCH_REF,
    dir_entries  = CONSTANTS$DIRECTORY_ENTRIES,
    dir_branch   = CONSTANTS$DIRECTORY_BRANCH
  )
  
  expect_s3_class(resultado_final, "data.frame")
  expect_true("uuid-historico-branch" %in% resultado_final$ec5_uuid)
  expect_true("uuid-nuevo-branch" %in% resultado_final$ec5_uuid)
})


# TEST 3: EDGE CASES DE parse_epicollect_entries ####
context("Test 3: Robustez ante datos vacios/heterogeneos")

test_that("parse_epicollect_entries retorna tibble vacio (no NULL) ante input NULL o lista vacia", {
  
  # Capturamos el warning esperado usando expect_warning
  expect_warning(
    resultado_null <- parse_epicollect_entries(NULL),
    "La lista de entradas esta vacia o es NULL"
  )
  
  expect_warning(
    resultado_vacio <- parse_epicollect_entries(list()),
    "La lista de entradas esta vacia o es NULL"
  )
  
  # Validamos las dimensiones y tipos
  expect_s3_class(resultado_null, "tbl_df")
  expect_equal(nrow(resultado_null), 0)
  
  expect_s3_class(resultado_vacio, "tbl_df")
  expect_equal(nrow(resultado_vacio), 0)
})
