# Librerías Core de la Aplicación UI/Server
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(sf)
library(dplyr)
library(reactable)
library(ggplot2)
library(plotly)
library(lubridate)
library(openxlsx)



# Inyección de Capas Arquitectónicas
source("config/constants.R")
source("R/data_access_local.R")
source("R/data_access_api.R")
source("R/data_processing.R")
source("R/geo_processing.R")

# Formatos Visuales Complementarios
paleta_graficos <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
colores_graficos <- paleta_graficos(20)
