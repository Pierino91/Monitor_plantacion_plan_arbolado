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
library(sf)
library(dplyr)
library(ggplot2)
library(plotly)
library(reactable)
library(RColorBrewer)
library(leaflet.minicharts)
library(tidyr)
library(bs4Dash)
library(shiny)
library(leaflet)
library(reactable)

# --------------------- CONSTANTES -----------------------#
refrescar_min <- 720
META_ANUAL_PLANTACION <- 4000
CAPTURA_DE_CARBONO_POR_ARBOL  <- 10
pal <- colorRampPalette(brewer.pal(8, "Set2"))
colores <- pal(20)


# ----------- FUNCIONES DE CARGA DE DATOS API ----------- #

get_data <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- fromJSON(content(response, as = "text"))
    return(content)
  } else {
    warning(paste("Error al acceder al enlace:", url, " - Código de estado: ", status_code(response)))
    return(NULL)
  }
}



get_all_entries <- function(base_url) {
  
  all_entries <- tibble()  # Inicializar el tibble vacío
  all_lugar <- tibble()    # Inicializar el tibble para coordenadas si existen
  
  # Obtener el primer conjunto de datos
  operarios_crudo_get <- get_data(base_url)
  # operarios_crudo_get <- get_data(all_data$`https://five.epicollect.net/api/export/entries/escuela-separacion-de-residuos?form_ref=1e5048d2af5c4071b39e53e008ba5935_64429dd5c8e88`$links$first)
  
  
  while (!is.null(operarios_crudo_get$links$self)) {
    
    if (!is.null(operarios_crudo_get$data$entries)) {
      
      # Convertir a tibble
      entries <- as_tibble(operarios_crudo_get$data$entries)
      
      # Convertir todas las columnas numéricas a character
      entries <- entries %>%
        mutate(across(where(is.numeric), as.character))
      
      # Detectar si hay una columna con datos de ubicación y procesarla
      col_lugar <- names(entries)[sapply(entries, is.list)]  
      
      if (length(col_lugar) > 0) {
        # Extraer datos de latitud, longitud y precisión
        lugar_df <- entries[col_lugar] %>%
          map_dfr(~ tibble(
            latitude = as.character(.x$latitude),
            longitude = as.character(.x$longitude),
            accuracy = as.character(.x$accuracy)
          ))
        
        all_lugar <- bind_rows(all_lugar, lugar_df)
        
        # Eliminar la columna de ubicación del dataframe original
        entries <- select(entries, -all_of(col_lugar))
      }
      
      # Acumular los datos
      all_entries <- bind_rows(all_entries, entries)
    }
    
    # Obtener el siguiente enlace
    next_link <- operarios_crudo_get$links$`next`
    
    # Si no hay más páginas, salir del bucle
    if (is.null(next_link)) {
      break
    }
    
    # Obtener el siguiente conjunto de datos
    operarios_crudo_get <- get_data(next_link)
    Sys.sleep(1)
  }
  
  # Retornar los datos combinados si existe información de ubicación
  if (nrow(all_lugar) > 0) {
    return(cbind(all_entries, all_lugar))
  } else {
    return(all_entries)
  }
}
# ----------- Vecinales ----------- #

vecinales <- st_read("www/Vecinales.kml", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(4326)%>%
  rename(nombre ="Name")





# ----------- URL DEL ENDPOINT ----------- #

endpoint_api <- "https://five.epicollect.net/api/export/entries/plan-de-arbolado-parana-2024-2028?form_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80&branch_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80_6821e6b4761cf"

endpoint_api_entrada <- "https://five.epicollect.net/api/export/entries/plan-de-arbolado-parana-2024-2028?form_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80"

endpoint_api_monitor <- "https://five.epicollect.net/api/export/entries/plan-de-arbolado-parana-2024-2028?form_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80&branch_ref=d82673133a804a53bf373c6c41be5f99_6821e5dd2ef80_68c1939a86125"