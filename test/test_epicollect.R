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
