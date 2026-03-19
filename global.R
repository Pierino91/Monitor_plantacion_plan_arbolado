#### START FILE: global.R ####
#### Funciones ####

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bsicons)
library(bslib)
library(htmltools)
library(readr)
library(jsonlite)
library(purrr)
library(DT)
library(httr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(ggplot2)
library(plotly)
library(reactable)
library(RColorBrewer)
library(leaflet.minicharts)
library(tidyr)
library(bs4Dash)
<<<<<<< HEAD
library(openxlsx)
library(xml2)

=======
>>>>>>> a385992d86956dd641f394aaca59a2e5ff0ca8e3

# --------------------- CONSTANTES -----------------------#
refrescar_min <- 720
META_ANUAL_PLANTACION <- 4000
CAPTURA_DE_CARBONO_POR_ARBOL  <- 10
pal <- colorRampPalette(brewer.pal(8, "Set2"))
colores <- pal(20)

# --------------------- SIMPLE CACHE FOR API CALLS ---------------------#
# A lightweight in-memory cache to avoid refetching the same URL repeatedly
.api_cache <- new.env(parent = emptyenv())

cache_get <- function(key) {
  if (exists(key, envir = .api_cache)) {
    obj <- get(key, envir = .api_cache)
    # obj should be a list: list(time = POSIXct, value = <data>)
    return(obj)
  }
  NULL
}

cache_set <- function(key, value) {
  assign(key, list(time = Sys.time(), value = value), envir = .api_cache)
}

# get_data with basic caching and retry/backoff
get_data <- function(url, ttl = 90, max_attempts = 3) {
  # ttl in seconds
  key <- url
  cached <- cache_get(key)
  if (!is.null(cached)) {
    age <- as.numeric(difftime(Sys.time(), cached$time, units = "secs"))
    if (age < ttl) {
      return(cached$value)
    }
  }

  attempt <- 1
  repeat {
    resp <- try(GET(url, timeout(20)), silent = TRUE)
    if (inherits(resp, "try-error")) {
      if (attempt >= max_attempts) {
        warning("get_data: request failed after attempts: ", conditionMessage(attr(resp, "condition")))
        return(NULL)
      }
      Sys.sleep(1 * attempt)
      attempt <- attempt + 1
      next
    }

    if (status_code(resp) != 200) {
      if (attempt >= max_attempts) {
        warning(sprintf("get_data: unexpected status %s for %s", status_code(resp), url))
        return(NULL)
      }
      Sys.sleep(1 * attempt)
      attempt <- attempt + 1
      next
    }

    parsed <- try(content(resp, as = "text", encoding = "UTF-8"), silent = TRUE)
    if (inherits(parsed, "try-error")) {
      return(NULL)
    }

    # attempt to parse json to list or dataframe
    parsed_json <- try(fromJSON(parsed, simplifyVector = FALSE), silent = TRUE)
    if (inherits(parsed_json, "try-error")) {
      # store raw text to cache to avoid refetch storms
      cache_set(key, parsed)
      return(parsed)
    }

    cache_set(key, parsed_json)
    return(parsed_json)
  }
}

# ----------- get_all_entries (uses get_data) ----------- #
# This function iterates paginated endpoints and merges results.
get_all_entries <- function(first_link) {
  if (is.null(first_link) || first_link == "") return(data.frame())

  all_entries <- list()
  all_lugar <- data.frame()

  next_link <- first_link
  while (!is.null(next_link) && next_link != "") {
    res <- get_data(next_link)
    if (is.null(res)) break

    # Expected structure: res$results or res$data depending on API
    # adapt safely
    entries <- NULL
    if (!is.null(res$results)) entries <- res$results
    if (is.null(entries) && !is.null(res$data)) entries <- res$data
    if (is.null(entries) && is.list(res)) entries <- res

    if (is.null(entries)) break

    # try to coerce to data.frame where possible
    df <- try(as.data.frame(entries, stringsAsFactors = FALSE), silent = TRUE)
    if (!inherits(df, "try-error")) {
      all_entries <- append(all_entries, list(df))
    }

    # handle next link if present
    if (!is.null(res$links) && !is.null(res$links$`next`)) {
      next_link <- res$links$`next`
    } else {
      next_link <- NULL
    }

    Sys.sleep(0.2)
  }

  if (length(all_entries) == 0) return(data.frame())

  combined <- bind_rows(all_entries)
  combined
}

# ----------- Vecinales ----------- #
# load once and keep in memory
vecinales <- tryCatch({
  st_read("www/Vecinales.kml", quiet = TRUE) %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_transform(4326) %>%
    rename(nombre = "Name")
}, error = function(e) {
  warning("Could not load Vecinales.kml: ", conditionMessage(e))
  NULL
})

<<<<<<< HEAD
VECINALES <- st_read("www/Vecinales.kml", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(4326)%>%
  rename(nombre ="Name")

# ----------- Unidades Municipales ----------- #

UNIDADES_MUNICIPALES <- 
  st_read("www/Unidades de gestión de SSPP.kml", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(4326) %>%
  rename(nombre ="Name")

# ----------- Densidades poblacionales ----------- #

densidades_poblacionales <-  read_xml("www/Radios_censales/Intervalo_densidades.qml")

# xml_find_all(densidades_poblacionales, ".//symbol")
categorias <- c(0, 10, 25, 50, 100, 150, 200, 250, Inf)
escala <- c(
  "#fff5f0",
  "#fee0d2",
  "#fcbba1",
  "#fc9272",
  "#fb6a4a",
  "#ef3b2c",
  "#cb181d",
  "#a50f15",
  "#67000d"
)

radio_censales <- st_read("www/Radios_censales/Radios_censales.shp") %>%  
  st_transform(4326)

pal <- colorBin(
  palette = escala,
  domain = radio_censales$Den_hab.ha,
  bins = categorias
)

# ----------- URL DEL ENDPOINT ----------- #

endpoint_api <- "https://five.epicollect.net/api/export/entries/plan-de-arbolado-parana-2024-2028?form_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80&branch_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80_6821e6b4761cf"

endpoint_api_entrada <- "https://five.epicollect.net/api/export/entries/plan-de-arbolado-parana-2024-2028?form_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80"

endpoint_api_monitor <- "https://five.epicollect.net/api/export/entries/plan-de-arbolado-parana-2024-2028?form_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80&branch_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80_68c1939a86125"
=======
# ----------- END global.R ####
#### END FILE ####
>>>>>>> a385992d86956dd641f394aaca59a2e5ff0ca8e3
