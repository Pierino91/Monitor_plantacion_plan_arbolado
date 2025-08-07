source("global.R")

function(input, output, session) {
  refrescar_min <- 30
  # Reactive timer para actualización automática
  auto_update <- reactiveTimer(refrescar_min * 60 * 100)
  
  datos_branch <- reactive({
    auto_update()  # Actualiza cada vez que el timer se activa
    # Obtener el primer enlace del endpoint
    tryCatch({
      first_link <- get_data(endpoint_api)$links$first
      datos <- get_all_entries(first_link)
      
      # Si no es dataframe o viene vacío, devolvés NULL
      if (is.null(datos) || nrow(datos) == 0) {
        return(NULL)
      }
      
      return(datos)
    }, error = function(e) {
      # Podrías agregar un log o mensaje interno aquí
      warning("Error al obtener los datos: ", conditionMessage(e))
      return(NULL)
    })
  })
  
  datos_entradas <- reactive({
    auto_update()  # Actualiza cada vez que el timer se activa
    # Obtener el primer enlace del endpoint
    tryCatch({
      first_link <- get_data(endpoint_api_entrada)$links$first
      datos <- get_all_entries(first_link)
      
      # Si no es dataframe o viene vacío, devolvés NULL
      if (is.null(datos) || nrow(datos) == 0) {
        return(NULL)
      }
      
      return(datos)
    }, error = function(e) {
      # Podrías agregar un log o mensaje interno aquí
      warning("Error al obtener los datos: ", conditionMessage(e))
      return(NULL)
    })
  })
  
  output$selector_sitio <- renderUI({
    req(plantaciones_arboles())  
    datos <- plantaciones_arboles()
    sitios <- unique(datos$sitio)
    selectInput("sitio", "Elegí una columna", choices = c("TODAS", sitios))
  })
  
  # Procesamiento de datos reactivo
  plantaciones_arboles <- reactive({
    datos <- datos_branch()
    datos_tec <- datos_entradas()
    # # test
    # datos <- get_all_entries(endpoint_api)
    # datos_tec <- get_all_entries(endpoint_api_entrada)
    
    
    if (is.null(datos)) return(NULL)
    # datos_limpios <- 
      datos %>%
      merge(datos_tec, by.x ="ec5_branch_owner_uuid", by.y="ec5_uuid") %>%
      rename_with(~ gsub("^\\d+_", "", .x)) %>%
      rename(
        latitud = "latitude",
        longitud = "longitude",
        especie = "Especie",
        edad = "Edad_meses",
        altura = "Altura_cm",
        foto_plantado = "Foto_del_plantado",
        obs = "Observacion",
        origen = "Origen",
        lugar_especifico = "Tipo_de_intervenc",
        sitio = "Lugar",
        otro_sitio ="Si_eligi_otro_siti",
        fecha_plantado = "Fecha_plantado",
        agente = "Agente",
        momento_relevado_arbol = "created_at.x"
      ) %>%
      mutate(
        momento_relevado_arbol = ymd_hms(momento_relevado_arbol, tz = "UTC"),
        fecha_plantado = dmy(fecha_plantado, tz = "UTC"),
        especie = factor(especie),
        lugar_especifico = factor(lugar_especifico),
        altura = as.numeric(altura),
        obs = as.character(obs),
        latitud = as.numeric(latitud),
        longitud = as.numeric(longitud)
      ) %>%
      arrange(fecha_plantado, momento_relevado_arbol) %>%
      mutate(ID = row_number())  %>%
      select(agente, sitio, fecha_plantado, latitud, longitud, especie, foto_plantado, lugar_especifico, altura, momento_relevado_arbol, obs,ID)
      
  })
  
  output$fecha_max <- renderUI({
    datos <- plantaciones_arboles()
    fecha_max <- if (!is.null(datos) && nrow(datos) > 0) {
      max(datos$fecha_plantado, na.rm = TRUE)
    } else {
      "Sin datos"
    }
    tags$h3(style = "font-size: 32px; margin: 0;", fecha_max)
  })
  
  output$cant_arboles_total <- renderText({
    datos <- plantaciones_arboles()
    cant_arboles_total <- if (!is.null(datos) && nrow(datos) > 0) {
      nrow(datos)
    } else {
      "Sin datos"
    }
    cant_arboles_total
    # tags$h3(style = "font-size: 32px; margin: 0;", cant_arboles_total)
  })
  
  output$barra_progreso <- renderUI({
    datos <- plantaciones_arboles()
    total <- if (!is.null(datos)) nrow(datos) else 0
    porc <- if (META_ANUAL_PLANTACION > 0) {
      round((total / META_ANUAL_PLANTACION) * 100, 1)
    } else {
      0
    }
    shinyWidgets::progressBar(
      id = "progreso_arboles",
      value = porc,
      display_pct = TRUE,
      status = ifelse(porc >= 80, "success", ifelse(porc >= 50, "warning", "danger"))
    )
  })
  
  output$factor_de_captura_carbono <- renderUI({
    datos <- plantaciones_arboles()
    total <- if (!is.null(datos)) nrow(datos) else 0
    factor_de_captura_carbono <- round((total) * CAPTURA_DE_CARBONO_POR_ARBOL , 1)
    tags$h3(style = "font-size: 32px; margin: 0;", factor_de_captura_carbono)
  })
  

  # Variables para graficar y mostrar
  plantaciones_especies_tiempo <- reactive({
    datos <- plantaciones_arboles()
    if (is.null(datos)) return(NULL)
    datos |>
      select(fecha_plantado, especie) |>
      filter(!is.na(fecha_plantado)) |>
      arrange(fecha_plantado) |>
      mutate(
        mes = month(fecha_plantado, label = TRUE, abbr = FALSE)
      ) |>
      group_by(mes, especie) |>
      reframe(n_especies = n())
  })
  
  plantaciones_tiempo <- reactive({
    datos <- plantaciones_especies_tiempo()
    if (is.null(datos)) return(NULL)
    datos |>
      ungroup() |>
      group_by(mes) |>
      reframe(
        n = sum(n_especies),
        acumulado = cumsum(n)
      )
  })
  
  resumen_especie <- reactive({
    req(input$sitio)
    if(input$sitio != "TODAS"){
      datos <- plantaciones_arboles() %>% 
        filter(sitio == input$sitio)
    }else{
      datos <- plantaciones_arboles() 
    }
    
    # print(input$selector_sitio)
    
    if (is.null(datos)) return(NULL)
    datos |>
      group_by(especie) |>
      reframe(n = n()) |>
      ungroup() |>
      mutate(
        porcentaje = n / sum(n) * 100,
        etiqueta = paste0(round(porcentaje, 1), "%"),
        ymax = cumsum(n),
        ymin = c(0, head(ymax, n = -1)),
        label_position = (ymax + ymin) / 2
      )
  })
  
  plantacion_lugarEspecifico_especies <- reactive({
    datos <- plantaciones_arboles()
    if (is.null(datos)) return(NULL)
    datos |>
      select(sitio, especie) |>
      group_by(sitio, especie) |>
      reframe(Cantidad = n()) |>
      rename("Especies" = "especie", "Lugar específico" = "sitio")
  })
  
  #### Mapa ####
  
  mapa_arboles <- reactive({
    datos <- plantaciones_arboles()
    if (is.null(datos)) return(NULL)
    datos |>
      filter(!is.na(latitud)) |>
      st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
  })
  
  index <- reactiveVal(1)
  
  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index()+1, nrow(plantaciones_arboles())))
  })
  
  output$image <- renderUI({
    
    imgs <- 
      plantaciones_arboles() %>%
      # datos_limpios %>%
      slice(index())%>%
      # slice(1)%>%
      select(foto_plantado, especie, sitio) 
      
      # .[index()]
      
      tags$img(src = imgs$foto_plantado, width = "720", height = "1280", alt = "Foto de árbol")
      
      tagList(
        tags$h4(imgs$especie),  # título o texto informativo
        tags$h3(imgs$sitio),  # título o texto informativo
        tags$img(src = imgs$foto_plantado, width = "100%", style = "max-width: 500px;")
      )
    # # test
    #  x <- imgs %>%
    #    pull(foto_plantado) %>%
    #    .[1]
    
  })
  
  
  
  
  ##### ------------ Salidas ------------ #####
  
  output$mapa_arboles <- renderLeaflet({
    datos <- mapa_arboles()
    leaflet() %>%
      addTiles() %>%
      {
        if (is.null(datos) || nrow(datos) == 0) {
          addPopups(., lng = 0, lat = 0, popup = "Sin datos georreferenciados")
        } else {
          addCircleMarkers(
            .,
            data = datos,
            radius = 3,
            color = "#4caf50",
            fillOpacity = 0.7,
            popup = ~paste0(
              "ID: ", ID, "<br>Especie: ", especie,
              "<br><img src='", foto_plantado, "' width='192' height='256'>"
            )
          )
        }
      }
  })
  # Grafica en el tiempo 
  
  output$grafico_tiempo <- renderPlotly({
    
    datos_esp <- plantaciones_especies_tiempo()
    datos_tiempo <- plantaciones_tiempo()
    
    if (is.null(datos_esp) || nrow(datos_esp) == 0) {
      plot_ly() |> 
        layout(
          annotations = list(
            text = "Sin datos disponibles",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(size = 20)
          ),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        )
    } else {
      plot_ly() |> 
        add_bars(
          data = datos_esp,
          x = ~mes,
          y = ~n_especies,
          color = ~especie,
          colors = "Set2",
          name =  ~especie,
          opacity = 0.7
        ) |>
        add_lines(
          data = datos_tiempo,
          x = ~mes,
          y = ~acumulado,
          name = "Acumulado",
          line = list(width = 2, color = 'black')
        ) |>
        add_markers(
          data = datos_tiempo,
          x = ~mes,
          y = ~acumulado,
          name = "Puntos Acumulado",
          marker = list(size = 6, color = 'black')
        ) |>
        layout(
          xaxis = list(title = "Mes"),
          yaxis = list(title = "Árboles"),
          showlegend = FALSE,
          barmode = 'stack'
        )
    }
    
  })
  
  output$tabla_especies <- renderReactable({
    datos <- plantacion_lugarEspecifico_especies()
    if (is.null(datos) || nrow(datos) == 0) {
      return(NULL)  
    }
    reactable(datos)
  })
  
  output$mensaje_tabla_especies <- renderUI({
    datos <- plantacion_lugarEspecifico_especies()
    if (is.null(datos) || nrow(datos) == 0) {
      tags$p("No hay datos para mostrar")
    } else {
      NULL  # No muestra mensaje si hay datos
    }
  })
  
  # Grafico d eespecies:
  # TODO hacer para filtre de acuerdo al sitio
  
  output$grafico_especie <- renderPlotly({
    datos <- resumen_especie()
    if (is.null(datos) || nrow(datos) == 0) {
      plot_ly() |> 
        layout(
          annotations = list(
            text = "Sin datos disponibles",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(size = 20)
          ),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        )
    } else {
      plot_ly(
        datos,
        x = ~especie,
        y = ~n,
        type = 'bar',
        color = ~especie,
        textinfo = 'label+percent',
        insidetextorientation = 'radial'
      ) |> layout(showlegend = FALSE)
    }
  })
  


}