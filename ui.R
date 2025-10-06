library(bs4Dash)
library(shiny)
library(leaflet)
library(reactable)

source("global.R")

ui <- dashboardPage(
  title = "Plan Arbolado - 2023 a 2027 - Subsecretaría de Ambiente",
  
  # Encabezado del dashboard
  header = dashboardHeader(
    skin = "light",
    status = "success",
    border = TRUE
  ),
  
  # Barra lateral (opcional - vacía o con menú futuro)
  sidebar = dashboardSidebar(
    skin = "light",
    status = "success",
    title = "Plan Arbolado",
    bs4SidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("tree")),
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("Evolución temporal", tabName = "evol", icon = icon("chart-simple")),
      menuItem("Especies plantadas", tabName = "especies", icon = icon("seedling")),
      menuItem("Monitoreo", tabName = "monitor", icon = icon("desktop"))
      
    )
  ),
  # Cuerpo del dashboard
  body = dashboardBody(
    
    # Estilos personalizados
    tags$head(
      tags$link(rel = "stylesheet", 
                href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap"),
      tags$style(HTML("
        body {
          background-color: white;
          font-family: 'Montserrat', sans-serif;
        }
        .leaflet-container {
          height: 100%;
        }
        #mapa {
          margin-top: 20px;
          border: 3px solid #4caf50;
          border-radius: 8px;
        }
        .sidebar-panel {
          display: flex;
          flex-direction: column;
          justify-content: flex-start;
          height: 100px;
          margin-top: 20px;
        }
        #Tipo_de_residuo {
          margin-top: 0;
          border: 2px solid #007aff;
          border-radius: 5px;
          padding: 5px;
        }"
      ))
    ),
    
    # Título principal
    tags$h1("Plan Arbolado - 2023 a 2027 - Subsecretaría de Ambiente", 
            style = "text-align: center; margin-top: 20px; font-family: 'Montserrat'; color: #4caf50;"),
    
    # Contenido principal en pestañas
    bs4TabItems(
      bs4TabItem(
        tabName = "resumen",
        h2("Resumen"),
        
        # Fila de valueBoxes
        fluidRow(
          # Última actualización
          bs4Card(
            title = tagList(icon("calendar-alt"), " Última actualización"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 6,
            tags$div(
              style = "font-size: 30px; font-weight: bold; text-align: center;",
              uiOutput("fecha_max")
            )
          ),
          
          # Árboles plantados + barra de progreso
          bs4Card(
            title = tagList(icon("tree"), " Árboles Plantados"),
            status = "success",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 6,
            tags$div(
              style = "font-size: 28px; font-weight: bold; text-align: center; margin-bottom: 10px;",
              textOutput("cant_arboles_total")
            ),
            uiOutput("barra_progreso")
          ),
          
          # Factor de captura de carbono
          bs4Card(
            title = tagList(icon("leaf"), " Captura estimada de CO₂"),
            status = "olive",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 6,
            tags$div(
              style = "font-size: 30px; font-weight: bold; text-align: center;",
              uiOutput("factor_de_captura_carbono")
            ),
            tags$p("kg CO₂ eq/anual 🌿", style = "font-size: 20px; text-align: center; margin-top: 10px;")
          )
        ),
        # Gráfico
        # fluidRow(
        #   box(
        #     title = "Plantaciones a lo largo del tiempo",
        #     status = "success",
        #     solidHeader = TRUE,
        #     width = 12,
        #     plotOutput("grafica_plantaciones_arboles_tiempo", height = "300px")
        #   )
        # ),
      ),
    bs4TabItem(
      tabName = "map",
      h2("Distribución espacial"),
      # Fila de valueBoxes
      fluidRow(
      # Mapa
        box(
          title = "Mapa de árboles plantados",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          leafletOutput("mapa_arboles", height = "600px")
        ),
        box(
          title = "Fotos",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          sliderInput("index_slider", "Seleccionar índice:",
                      min = 1, max = 10, value = 1, step = 1), # max lo ajustaremos en server
          actionButton("previous", "Previous"),
          actionButton("next", "Next"),
          uiOutput("image")
        )
      )  
    ),
    bs4TabItem(
      tabName = "evol",
      h2("Evolución temporal del plantado de árboles"),
      # title = "Plantaciones a lo largo del tiempo",
      # Fila de valueBoxes
      fluidRow(
          box(
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("grafico_tiempo", height = "300px")
          ),
            box(
              title = "Detalle de plantaciones",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              uiOutput("mensaje_tabla_especies"),
              reactableOutput("tabla_especies")
          )          
      )
    ),
    bs4TabItem(
      tabName = "especies",
      h2("Especies plantadas"),
      fluidRow(
        uiOutput("selector_sitio"),
        tableOutput("datos_filtrados"), 
        box(
          status = "success",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("grafico_especie", height = "600px")
        )
      )
     ),
    bs4TabItem(
      tabName = "monitor",
      h2("Equipo de monitoreo"),
      fluidRow(
        # uiOutput("selector_sitio_monitoreo"),
        box(
          title = "Tabla de monitoreo",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          # uiOutput("mensaje_tabla_especies"),
          reactableOutput("tabla_monitoreo")
        )  
      )
    )
   )
  ),
  
  controlbar = NULL,
  footer = bs4DashFooter(left = "Subsecretaría de Ambiente", right = "Paraná, 2023 - 2027")
)

# # install.packages('rsconnect')
# 
# library(rsconnect)
# rsconnect::setAccountInfo(name='secretaria-de-ambiente-y-salud',
#                           token='59A4D29D0B62C7DC32941F343F767900',
#                           secret='67dK/Pwf3vnv46IiKiftaxO7xjhg1Sfz8m00TwhY')
#
#
# rsconnect::deployApp(appName="Plan_arbolado_2023_2027") 

