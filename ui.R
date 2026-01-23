source("global.R")

ui <- dashboardPage(
  title = "Plan Arbolado - 2023 a 2027 - Secretaría de Recursos Hídricos y Gestión Ambiental",
  help = NULL,
  
  # Encabezado del dashboard
  header = dashboardHeader(
    skin = "light",
    status = "success",
    border = TRUE,
    title = dashboardBrand(
      title = "Plan Arbolado 2023-2027",
      color = "success",
      opacity = 1
    )
  ),
  
  # Barra lateral con menú
  sidebar = dashboardSidebar(
    skin = "light",
    status = "success",
    title = "Plan Arbolado",
    bs4SidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("tree")),
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("Base de Datos", tabName = "datos", icon = icon("table"))
    )
  ),
  
  # Cuerpo del dashboard
  body = dashboardBody(
    
    # Estilos personalizados mejorados
    tags$head(
      tags$link(rel = "stylesheet", 
                href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700;800&display=swap"),
      tags$style(HTML("
        /* ============================================
           VARIABLES CSS
        ============================================ */
        :root {
          --primary-green: #2e7d32;
          --light-green: #4caf50;
          --lighter-green: #66bb6a;
          --bg-light: #f5f7fa;
          --bg-green-tint: #e8f5e9;
          --text-dark: #2c3e50;
          --shadow-sm: 0 2px 8px rgba(0, 0, 0, 0.06);
          --shadow-md: 0 4px 16px rgba(0, 0, 0, 0.08);
          --shadow-lg: 0 8px 24px rgba(76, 175, 80, 0.25);
          --transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        }
        
        /* ============================================
           ESTILOS GENERALES
        ============================================ */
        body {
          background: linear-gradient(135deg, var(--bg-light) 0%, var(--bg-green-tint) 100%);
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          color: var(--text-dark);
        }
        
  
        /* ============================================
           TÍTULO PRINCIPAL
        ============================================ */
        .main-title {
          background: linear-gradient(135deg, var(--primary-green) 0%, var(--light-green) 100%);
          color: white;
          padding: 36px 48px;
          margin: -15px -15px 36px -15px;
          border-radius: 0 0 24px 24px;
          box-shadow: var(--shadow-lg);
          text-align: center;
          font-weight: 800;
          font-size: 36px;
          letter-spacing: -0.8px;
          position: relative;
          overflow: hidden;
        }
        
        .main-title::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background: linear-gradient(45deg, transparent 30%, rgba(255, 255, 255, 0.1) 50%, transparent 70%);
          animation: shine 3s infinite;
        }
        
        @keyframes shine {
          0% { transform: translateX(-100%); }
          100% { transform: translateX(100%); }
        }
        
        .main-subtitle {
          font-size: 17px;
          font-weight: 400;
          margin-top: 10px;
          opacity: 0.96;
          letter-spacing: 0.3px;
        }
        
        /* ============================================
           TÍTULOS DE SECCIÓN
        ============================================ */
        .section-title {
          color: var(--primary-green);
          font-weight: 700;
          margin-bottom: 28px;
          font-size: 30px;
          letter-spacing: -0.5px;
          display: flex;
          align-items: center;
          gap: 12px;
        }
        
        .section-title::before {
          content: '';
          width: 6px;
          height: 32px;
          background: linear-gradient(180deg, var(--primary-green), var(--lighter-green));
          border-radius: 3px;
        }
        
        /* ============================================
           CAJAS DE INFORMACIÓN
        ============================================ */
        .small-box {
          border-radius: 16px;
          box-shadow: var(--shadow-md);
          transition: var(--transition);
          overflow: hidden;
          position: relative;
        }
        
        .small-box:hover {
          transform: translateY(-4px);
          box-shadow: 0 12px 32px rgba(0, 0, 0, 0.12);
        }
        
        .small-box .inner {
          padding: 24px 20px;
        }
        
        .small-box h3 {
          font-size: 42px;
          font-weight: 800;
          margin: 0 0 8px 0;
          text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        .small-box p {
          font-size: 15px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin: 0;
        }
        
        .small-box .icon {
          font-size: 72px;
          opacity: 0.25;
        }
        
        /* ============================================
           CAJAS PRINCIPALES (BOX)
        ============================================ */
        .box {
          border-radius: 16px;
          box-shadow: var(--shadow-md);
          border: none;
          margin-bottom: 24px;
          transition: var(--transition);
        }
        
        .box:hover {
          box-shadow: 0 8px 24px rgba(0, 0, 0, 0.1);
        }
        
        .box-header {
          background: linear-gradient(135deg, var(--light-green) 0%, var(--lighter-green) 100%);
          color: white;
          border-radius: 16px 16px 0 0;
          padding: 20px 24px;
          font-weight: 700;
          font-size: 19px;
          letter-spacing: -0.3px;
        }
        
        .box-body {
          padding: 24px;
          background: white;
          border-radius: 0 0 16px 16px;
        }
        
        /* ============================================
           GRÁFICOS
        ============================================ */
        .plotly {
          border-radius: 12px;
        }
        
        /* ============================================
           TABLAS
        ============================================ */
        .ReactTable {
          border-radius: 16px;
          overflow: hidden;
          box-shadow: var(--shadow-sm);
          border: 1px solid #e8f5e9;
        }
        
        .ReactTable .rt-thead {
          background: linear-gradient(135deg, var(--light-green) 0%, var(--lighter-green) 100%);
          color: white;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          font-size: 13px;
        }
        
        .ReactTable .rt-tbody .rt-tr {
          transition: var(--transition);
          border-bottom: 1px solid #f0f0f0;
        }
        
        .ReactTable .rt-tbody .rt-tr:hover {
          background: #f1f8f4;
          transform: scale(1.01);
        }
        
        .ReactTable .rt-td {
          padding: 14px 12px;
          font-size: 14px;
        }
        
        /* ============================================
           MAPA LEAFLET
        ============================================ */
        .leaflet-container {
          border-radius: 12px;
          box-shadow: inset 0 2px 8px rgba(0, 0, 0, 0.05);
        }
        
        /* ============================================
           PANEL DE CONTROL (CONTROLBAR)
        ============================================ */
        .control-sidebar {
          background: white;
          box-shadow: -4px 0 16px rgba(0, 0, 0, 0.08);
        }
        
        .control-sidebar label {
          font-weight: 600;
          color: var(--primary-green);
          margin-bottom: 10px;
          display: block;
          font-size: 14px;
          letter-spacing: 0.3px;
        }
        
        .control-sidebar .selectize-input,
        .control-sidebar input[type='text'],
        .control-sidebar input[type='date'] {
          border: 2px solid #e8f5e9;
          border-radius: 10px;
          padding: 10px 14px;
          transition: var(--transition);
          font-size: 14px;
        }
        
        .control-sidebar .selectize-input:focus,
        .control-sidebar input:focus {
          border-color: var(--light-green);
          box-shadow: 0 0 0 3px rgba(76, 175, 80, 0.1);
          outline: none;
        }
        
        .control-sidebar .btn-success {
          background: linear-gradient(135deg, var(--primary-green), var(--light-green));
          border: none;
          border-radius: 10px;
          padding: 12px;
          font-weight: 700;
          letter-spacing: 0.5px;
          transition: var(--transition);
          text-transform: uppercase;
          font-size: 13px;
        }
        
        .control-sidebar .btn-success:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(76, 175, 80, 0.3);
        }
        
        .filtros-stats {
          margin: 20px 15px;
          padding: 20px;
          background: linear-gradient(135deg, #e8f5e9 0%, #f1f8f4 100%);
          border-radius: 12px;
          text-align: center;
          border: 2px solid #c8e6c9;
        }
        
        .filtros-stats h5 {
          margin: 0 0 10px 0;
          color: var(--primary-green);
          font-size: 13px;
          text-transform: uppercase;
          letter-spacing: 1px;
          font-weight: 700;
        }
        
        .filtros-stats-number {
          font-size: 32px;
          font-weight: 800;
          color: var(--primary-green);
          line-height: 1;
        }
        
        /* ============================================
           ANIMACIONES
        ============================================ */
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        .card, .box, .small-box {
          animation: fadeInUp 0.6s cubic-bezier(0.4, 0, 0.2, 1);
        }
        
        /* ============================================
           SCROLLBAR PERSONALIZADO
        ============================================ */
        ::-webkit-scrollbar {
          width: 12px;
          height: 12px;
        }
        
        ::-webkit-scrollbar-track {
          background: #f5f5f5;
          border-radius: 10px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(135deg, var(--light-green) 0%, var(--lighter-green) 100%);
          border-radius: 10px;
          border: 2px solid #f5f5f5;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(135deg, var(--primary-green) 0%, var(--light-green) 100%);
        }
        
        /* ============================================
           FOOTER
        ============================================ */
        .main-footer {
          background: white;
          border-top: 3px solid #e8f5e9;
          padding: 24px 16px;
          box-shadow: 0 -4px 12px rgba(0, 0, 0, 0.04);
        }
        
        /* ============================================
           RESPONSIVE
        ============================================ */
        @media (max-width: 768px) {
          .main-title {
            font-size: 26px;
            padding: 24px 20px;
          }
          
          .main-subtitle {
            font-size: 15px;
          }
          
          .section-title {
            font-size: 24px;
          }
          
          .box-header {
            font-size: 17px;
            padding: 16px 20px;
          }
          
          .small-box h3 {
            font-size: 34px;
          }
          
          #imageModal .modal-content {
            max-width: 95%;
            max-height: 90vh;
          }
          
          #imageModal .close-modal {
            top: 10px;
            right: 15px;
            font-size: 40px;
          }
        }
        
        /* ============================================
           MODAL IMAGEN - VISUAL PREMIUM
        ============================================ */
        .modal-image-viewer .modal-dialog {
          max-width: 92vw;
        }
        
        .modal-image-viewer .modal-content {
          background: rgba(15, 15, 15, 0.95);
          border-radius: 20px;
          padding: 20px;
          box-shadow: 0 30px 80px rgba(0, 0, 0, 0.7);
          animation: zoomFadeIn 0.35s ease-out;
        }
        
        .modal-image-viewer .modal-body {
          padding: 0;
        }
        
        .modal-img-zoom {
          max-width: 100%;
          max-height: 85vh;
          border-radius: 16px;
          box-shadow: 0 12px 40px rgba(0, 0, 0, 0.6);
          transition: transform 0.35s ease, box-shadow 0.35s ease;
          cursor: zoom-out;
        }
        
        .modal-img-zoom:hover {
          transform: scale(1.03);
          box-shadow: 0 20px 60px rgba(0, 0, 0, 0.75);
        }
        
        /* Botón cerrar */
        .close-modal {
          position: absolute;
          top: 14px;
          right: 20px;
          background: transparent;
          border: none;
          color: #ffffff;
          font-size: 42px;
          font-weight: 300;
          cursor: pointer;
          z-index: 10;
          transition: transform 0.2s ease, opacity 0.2s ease;
        }
        
        .close-modal:hover {
          transform: scale(1.15);
          opacity: 0.8;
        }
        
        /* Animación entrada */
        @keyframes zoomFadeIn {
          from {
            opacity: 0;
            transform: scale(0.92);
          }
          to {
            opacity: 1;
            transform: scale(1);
          }
        }
        
        /* ============================================
           UTILIDADES
        ============================================ */
        .text-center {
          text-align: center;
        }
        
        .mb-4 {
          margin-bottom: 24px;
        }
        
        .mt-4 {
          margin-top: 24px;
        }
        /* ============================================
           FIX ALTURA DINÁMICA CON CONTROLBAR
           (EVITA TAPAR CONTENIDO)
        ============================================ */
        .content-wrapper,
        .main-sidebar,
        .control-sidebar,
        .main-footer {
          min-height: 100vh;
        }
        
        .control-sidebar {
          position: relative;
          height: auto;
        }
        
        body:not(.control-sidebar-slide-open) .control-sidebar {
          display: none;
        }
      "))
    ),
    
    tags$script(HTML("
        function mostrarImagen(src) {
          $('#modal-img').attr('src', src);
          $('#modal-imagen').modal('show');
        }
      ")),
    
    # Modal para mostrar imágenes en tamaño completo
    tags$div(
      id = "modal-imagen",
      class = "modal fade modal-image-viewer",
      tabindex = "-1",
      role = "dialog",
      tags$div(
        class = "modal-dialog modal-dialog-centered modal-xl",
        tags$div(
          class = "modal-content",
          tags$button(
            type = "button",
            class = "close-modal",
            "×",
            onclick = "$('#modal-imagen').modal('hide')"
          ),
          tags$div(
            class = "modal-body text-center",
            tags$img(
              id = "modal-img",
              src = "",
              class = "modal-img-zoom"
            )
          )
        )
      )
    ),
    
    # Título principal con logo
    div(
      style = "
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 20px;
        margin-top: 20px;
      ",
      
      tags$img(
        src = "PlantandoFuturo.png",
        height = "250px"
      ),
      
      div(
        style = "display: flex; flex-direction: column; align-items: center;",
        
        div(
          class = "main-title",
          "Plan Arbolado 2023-2027 - Plantado"
        ),
        
        div(
          class = "main-subtitle",
          "Secretaría de Recursos Hídricos y Gestión Ambiental · Paraná"
        )
      )
    ),
    
    # Contenido en pestañas
    bs4TabItems(
      # ============================================
      # TAB: RESUMEN
      # ============================================
      bs4TabItem(
        tabName = "resumen",
        
        fluidRow(
          column(
            width = 12,
            tags$h2(
              class = "section-title",
              "Resumen General"
            )
          )
        ),
        
        # Fila de valueBoxes principales
        fluidRow(
          bs4Card(
            title = tagList(icon("calendar-alt"), " Última plantación registrada"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 4,
            tags$div(
              style = "font-size: 30px; font-weight: bold; text-align: center;",
              uiOutput("fecha_max")
            )
          ),
          bs4Card(
            title = tagList(icon("tree"), " Árboles Plantados"),
            status = "success",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 4,
            tags$div(
              style = "font-size: 28px; font-weight: bold; text-align: center; margin-bottom: 10px;",
              textOutput("cant_arboles_total")
            ),
            uiOutput("barra_progreso")
          ),
          bs4Card(
            title = tagList(icon("leaf"), " Captura estimada de CO₂"),
            status = "olive",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 4,
            tags$div(
              style = "font-size: 30px; font-weight: bold; text-align: center;",
              uiOutput("factor_de_captura_carbono")
            ),
            tags$p("kg CO₂ eq/anual 🌿", style = "font-size: 20px; text-align: center; margin-top: 10px;")
          )
        ),
        
        # Gráficos de análisis
        fluidRow(
          bs4Card(
            title = "Árboles por sitio",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            maximizable = TRUE,
            collapsible = TRUE,
            closable = FALSE,
            plotlyOutput("grafico_sitio", height = "350px")
          ),
          bs4Card(
            title = "Cronología",
            status = "success",
            solidHeader = TRUE,
            maximizable = TRUE,
            collapsible = TRUE,
            closable = FALSE,
            width = 4,
            plotlyOutput("grafico_tiempo", height = "350px")
          ),
          bs4Card(
            title = "Especies",
            status = "success",
            solidHeader = TRUE,
            maximizable = TRUE,
            collapsible = TRUE,
            closable = FALSE,
            width = 4,
            plotlyOutput("grafico_especie", height = "350px")
          )
        )
      ),
      
      # ============================================
      # TAB: MAPA
      # ============================================
      bs4TabItem(
        tabName = "map",
        
        tags$h2(
          class = "section-title",
          "Distribución Espacial"
        ),
        
        # Mapa interactivo
        fluidRow(
          box(
            title = "Mapa de árboles plantados",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            uiOutput("selector_sitio_mapa"),
            leafletOutput("mapa_arboles", height = "600px")
          )
        ),
        
        # Galería de fotos
        fluidRow(
          box(
            title = "Galería de Fotos",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            sliderInput("index_slider", "Seleccionar índice:",
                        min = 1, max = 10, value = 1, step = 1),
            actionButton("previous", "Anterior", class = "btn-success"),
            actionButton("next", "Siguiente", class = "btn-success"),
            uiOutput("image")
          )
        )
      ),
      
      # ============================================
      # TAB: BASE DE DATOS
      # ============================================
      bs4TabItem(
        tabName = "datos",
        
        tags$h2(
          class = "section-title",
          "Base de Datos"
        ),
        
        fluidRow(
          bs4Card(
            title = "Base de Datos Completa",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            maximizable = TRUE,
            downloadButton("descargar_datos", "Descargar CSV (Detalle de plantaciones)", class = "btn-success mb-3"),
            downloadButton("descargar_datos_crudos", "Descargar CSV (Datos crudo)", class = "btn-success mb-3")
            
          ),
          bs4Card(
            title = "Detalle de plantaciones",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            reactableOutput("tabla_especies")
          )
        )
      )
    )
  ),
  
  # Panel de control lateral
  controlbar = dashboardControlbar(
    skin = "light",
    overlay = FALSE,  
    collapsed = FALSE,
    width = 360,
    
    tags$div(
      style = "padding: 28px 20px 16px 20px;",
      tags$h4(
        icon("filter", style = "margin-right: 8px;"),
        "Filtros de Plantación",
        style = "color: #2e7d32; font-weight: 800; margin: 0; text-align: center; font-size: 20px; letter-spacing: -0.3px;"
      )
    ),
    
    tags$hr(style = "margin: 16px 20px; border: none; border-top: 2px solid #e8f5e9;"),
    
    # Selector de sitio
    tags$div(
      style = "padding: 16px 20px;",
      tags$label(
        icon("map-marker-alt", style = "margin-right: 6px;"),
        "Seleccionar Sitio:",
        style = "font-weight: 600; color: #2e7d32; margin-bottom: 10px; display: block; font-size: 14px;"
      ),
      uiOutput("selector_sitio_controlbar")
    ),
    
    # Selector de especie
    tags$div(
      style = "padding: 16px 20px;",
      tags$label(
        icon("leaf", style = "margin-right: 6px;"),
        "Filtrar por Especie:",
        style = "font-weight: 600; color: #2e7d32; margin-bottom: 10px; display: block; font-size: 14px;"
      ),
      uiOutput("selector_especie_controlbar")
    ),
    
    # Selector de fecha
    tags$div(
      style = "padding: 16px 20px;",
      tags$label(
        icon("calendar-alt", style = "margin-right: 6px;"),
        "Filtrar por Fecha:",
        style = "font-weight: 600; color: #2e7d32; margin-bottom: 10px; display: block; font-size: 14px;"
      ),
      uiOutput("selector_fecha_controlbar")
    ),
    
    tags$hr(style = "margin: 16px 20px; border: none; border-top: 2px solid #e8f5e9;"),
    
    # Botón limpiar filtros
    tags$div(
      style = "padding: 16px 20px;",
      actionButton(
        "limpiar_filtros",
        "Limpiar Filtros",
        icon = icon("eraser"),
        class = "btn-success btn-block",
        style = "width: 100%; padding: 12px; font-weight: 700;"
      )
    ),
    
    # Total de registros
    tags$div(
      class = "filtros-stats",
      tags$h5("Registros encontrados"),
      uiOutput("total_registros_filtrados")
    )
  ),
  
  # Footer
  footer = bs4DashFooter(
    left = tags$div(
      style = "
        width: 100%;
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 12px 0;
      ",
      tags$img(
        src = "Logo.png",
        height = "160px",
        style = "display: block; filter: drop-shadow(0 4px 8px rgba(0, 0, 0, 0.1));"
      )
    ),
    right = tags$div(
      style = "color: #757575; font-size: 13px; font-weight: 500;",
      "Paraná, 2023 - 2027"
    )
  )
)

# rsconnect::deployApp(appName="Plan_arbolado_2023_2027")