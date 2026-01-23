source("global.R")

function(input, output, session) {
  # Reactive timer para actualización automática
  
  #### Procesamiento ####
  
  auto_update <- reactiveTimer(refrescar_min * 60 * 100)
  
  datos_branch <- reactive({
    auto_update()  # Actualiza cada vez que el timer se activa
    # Obtener el primer enlace del endpoint
    tryCatch({
      first_link <- get_data(endpoint_api)$links$first
      datos_crudo <- get_all_entries(first_link)
      
      # Si no es dataframe o viene vacío, devolvés NULL
      if (is.null(datos_crudo) || nrow(datos_crudo) == 0) {
        return(NULL)
      }
      
      return(datos_crudo)
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
      # datos <- get_all_entries(first_link)
      datos_tec <- get_all_entries(first_link)
      
      # Si no es dataframe o viene vacío, devolvés NULL
      if (is.null(datos_tec) || nrow(datos_tec) == 0) {
        return(NULL)
      }
      
      return(datos_tec)
    }, error = function(e) {
      # Podrías agregar un log o mensaje interno aquí
      warning("Error al obtener los datos: ", conditionMessage(e))
      return(NULL)
    })
  })
  
  plantaciones_arboles_sin_filtro <- reactive({
    datos_crudo <- datos_branch()
    datos_tec <- datos_entradas()
    # # test
     # datos <- get_all_entries(endpoint_api)
     # datos_tec <- get_all_entries(endpoint_api_entrada)
    
    
    if (is.null(datos_crudo)) return(NULL)
    datos_proc <-
      datos_crudo %>%
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
        fecha_plantado = ymd(dmy(fecha_plantado, tz = "UTC")),
        especie = factor(especie),
        lugar_especifico = factor(lugar_especifico),
        altura = as.numeric(altura),
        obs = as.character(obs),
        latitud = as.numeric(latitud),
        longitud = as.numeric(longitud)
      ) %>%
      arrange(momento_relevado_arbol) %>%
      mutate(ID = row_number())  %>%
      select(agente, sitio, fecha_plantado, latitud, longitud, especie, foto_plantado, lugar_especifico, altura, momento_relevado_arbol, obs,ID)
    
    return(datos_proc)
  })
  
  

  
 
#### Control barr #####

plantaciones_arboles <- reactive({
    
    datos <- plantaciones_arboles_sin_filtro()
    if (is.null(datos)) return(NULL)
    
    # Aplicar filtro de sitio
    if (!is.null(input$filtro_sitio) && input$filtro_sitio != "TODAS") {
      datos <- datos %>% filter(sitio == input$filtro_sitio)
    }
    
    # Aplicar filtro de presencia
    if (!is.null(input$filtro_especie) && input$filtro_especie != "TODAS") {
      datos <- datos %>% filter(especie == input$filtro_especie)
    }
    
    # Aplicar filtro de fecha
    if (!is.null(input$filtro_fecha) &&
        length(input$filtro_fecha) == 2 &&
        !any(is.na(input$filtro_fecha))) {
      datos <- datos %>% filter((fecha_plantado) >= input$filtro_fecha[[1]] &
                                (fecha_plantado) <= input$filtro_fecha[[2]])
      
    }
    
    return(datos)
  })


  output$selector_sitio_controlbar <- renderUI({
    req(plantaciones_arboles_sin_filtro())
    datos <- plantaciones_arboles_sin_filtro()
    sitios <- unique(as.character(datos$sitio))
    
    selectInput(
      "filtro_sitio",
      NULL,
      choices = c("TODAS" = "TODAS", sitios),
      selected = "TODAS"
    )
  })
  
  output$selector_especie_controlbar <- renderUI({
    req(plantaciones_arboles_sin_filtro())
    datos <- plantaciones_arboles_sin_filtro()
    especies <- unique(as.character(datos$especie))
    
    selectInput(
      "filtro_especie",
      NULL,
      choices = c("TODAS" = "TODAS", especies),
      selected = "TODAS"
    )
  })
  
  output$selector_fecha_controlbar <- renderUI({
    req(plantaciones_arboles_sin_filtro())
    datos <- plantaciones_arboles_sin_filtro()
    
    fecha_min <- ymd_hms(min(datos$fecha_monitoreo, na.rm = TRUE)) - days(1)
    fecha_max <- ymd_hms(max(datos$fecha_monitoreo, na.rm = TRUE)) + days(1)
    
    dateRangeInput(
      "filtro_fecha",
      label = "",
      separator = " a ",
      start = fecha_min,
      end = fecha_max,
      min = fecha_min,
      max = fecha_max
    )
  })
  

  output$total_registros_filtrados <- renderUI({
    datos <- plantaciones_arboles()
    total <- if (!is.null(datos)) nrow(datos) else 0
    
    tags$h3(
      style = "margin: 0; color: #2e7d32; font-size: 32px; font-weight: 700;",
      total
    )
  })
  

  observeEvent(input$limpiar_filtros, {
    datos <- plantaciones_arboles_sin_filtro()
    fecha_min <- ymd_hms(min(datos$fecha_monitoreo, na.rm = TRUE)) - days(1)
    fecha_max <- ymd_hms(max(datos$fecha_monitoreo, na.rm = TRUE)) + days(1)
    
    req(plantaciones_arboles())
    updateSelectInput(session, "filtro_sitio", selected = "TODAS")
    updateSelectInput(session, "filtro_especie", selected = "TODOS")
    updateDateRangeInput(session, "filtro_fecha", start = fecha_min, end = fecha_max, min = fecha_min, max = fecha_max)
  })

  
#### UI ####
#### Resumen ####

  output$total_arboles <- renderText({
    req(input$sitio, plantaciones_arboles())
    datos <- plantaciones_arboles()
    
    if (input$sitio == "TODAS") {
      total <- nrow(datos)
    } else {
      total <- sum(datos$sitio == input$sitio, na.rm = TRUE)
    }
    paste("Total de árboles:", total)
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
    # cant_arboles_total
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
  
  output$grafico_sitio <- renderPlotly({
    req(plantaciones_arboles())
    
    datos <- plantaciones_arboles() %>%
      group_by(sitio) %>%
      summarise(cantidad = n()) %>%
      arrange(desc(cantidad))
    
    if (nrow(datos) == 0) {
      plot_ly() |> 
        layout(
          annotations = list(
            text = "Sin datos disponibles",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 20)
          ),
          xaxis = list(showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE)
        )
    } else {
      plot_ly(
        data = datos,
        x = ~sitio,
        y = ~cantidad,
        type = "bar",
        color = ~sitio,
        text = ~cantidad,
        textposition = "auto"
      ) |> 
        layout(
          showlegend = FALSE,
          yaxis = list(title = "Total de árboles"),
          xaxis = list(title = "Sitio")
        )
    }
  })
  
  output$grafico_tiempo <- renderPlotly({
    
    req(plantaciones_arboles())
    datos_proc <- plantaciones_arboles()
    
    if (is.null(datos_proc)) return(NULL)
    
        # datos_tiempo <- datos |>
    #   # datos_limpios |>
    #   select(fecha_plantado, especie) |>
    #   filter(!is.na(fecha_plantado)) |>
    #   group_by(fecha_plantado, especie) |>
    #   reframe(n_especies = n())
    
    datos <- 
      datos_proc |>
      mutate(
        mes = as.Date(
          paste0("01-", month(fecha_plantado), "-", year(fecha_plantado)),
          format = "%d-%m-%Y"
        )
      ) |>
      group_by(
        mes
      ) |>
      reframe(
        n = n()
      ) |>
      ungroup(
      ) |>
      mutate(
        acumulado = cumsum(n)
      ) |>
      filter(
        # mes>as.Date("2025-01-01")
      )
    
    
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
      plot_ly() |> 
        # Barras mensuales
        add_bars(
          data = datos,
          x = ~mes,
          y = ~n,
          marker = list(
            color = "#2e7d32",
            line = list(color = "#1b5e20", width = 0.5)
          ),
          opacity = 0.85,
          yaxis = "y",
          name = "Mensual"
        ) |>
        # Línea acumulada
        add_lines(
          data = datos,
          x = ~mes,
          y = ~acumulado,
          name = "Acumulado",
          line = list(width = 3, color = "#66bb6a"),
          yaxis = "y2"
        ) |>
        # Puntos acumulados
        add_markers(
          data = datos,
          x = ~mes,
          y = ~acumulado,
          marker = list(
            size = 6,
            color = "#66bb6a",
            line = list(color = "white", width = 1)
          ),
          yaxis = "y2",
          showlegend = FALSE
        ) |>
        layout(
          title = list(
            text = paste0(
              "<b>Evolución mensual de árboles plantados</b>",
              "<br><span style='color:#4caf50;font-size:16px;'>",
              "Seguimiento operativo y acumulado histórico</span>"
            ),
            x = 0.01,
            y = 0.95
          ),
          paper_bgcolor = "#f5f7fa",
          plot_bgcolor  = "#e8f5e9",
          xaxis = list(
            title = "Período",
            showgrid = FALSE,
            zeroline = FALSE,
            tickfont = list(color = "#2c3e50")
          ),
          yaxis = list(
            title = "Árboles plantados (mensual)",
            showgrid = TRUE,
            gridcolor = "rgba(0,0,0,0.05)",
            zeroline = FALSE,
            tickfont = list(color = "#2c3e50"),
            titlefont = list(color = "#2c3e50")
          ),
          yaxis2 = list(
            title = "Total acumulado",
            overlaying = "y",
            side = "right",
            showgrid = FALSE,
            tickfont = list(color = "#2c3e50"),
            titlefont = list(color = "#2c3e50")
          ),
          margin = list(l = 60, r = 80, t = 80, b = 50),
          showlegend = FALSE,
          barmode = "stack"
        )
      
    }
    
  })
  
  output$grafico_especie <- renderPlotly({

    
    datos_esp <- plantaciones_arboles() %>%
      count(especie, name = "n") %>%   # más rápido que group_by + summarise
      arrange(desc(n))
    
    total <- sum(datos_esp$n)          # se calcula UNA vez
    
    datos_esp <- datos_esp %>%
      mutate(
        porcentaje = n / total * 100,
        porcentaje_lbl = sprintf("%.1f%%", porcentaje),
        categoria = case_when(
          porcentaje < 5  ~ "Baja",
          porcentaje < 10 ~ "Media",
          porcentaje < 20 ~ "Alta",
          TRUE            ~ "Muy alta"
        ),
        hover_txt = paste0(
          "Individuos: ", porcentaje_lbl,
          "<br>Estado: ", categoria
        )
      )
    
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

    
    plot_ly(
      datos_esp,
      x = ~especie,
      y = ~n,
      type = "bar",
      color = ~categoria,
      colors = c(
        "Muy alta"    = "#2e7d32",
        "Alta"   = "#9ccc65",
        "Media"    = "#ef6c00",
        "Baja" = "#c62828"
      ),
      text = ~porcentaje_lbl,
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{x}</b>",
        "<br>Total: %{y}",
        "<br>%{customdata}",
        "<extra></extra>"
      ),
      customdata = ~hover_txt
    ) |>
      layout(
        title = list(
          text = paste0(
            "<b>Distribución de árboles por especie</b>",
            "<br><span style='color:#4caf50;font-size:12px;'>",
            "Clasificación por nivel de representación</span>"
          ),
          x = 0.01
        ),
        paper_bgcolor = "#f5f7fa",
        plot_bgcolor  = "#e8f5e9",
        xaxis = list(
          title = "Especie",
          tickangle = -45,
          showgrid = FALSE,
          categoryorder = "array",
          categoryarray = datos_esp$especie   # vector ya ordenado
        ),
        yaxis = list(
          title = "Total de árboles",
          showgrid = TRUE,
          gridcolor = "rgba(0,0,0,0.05)"
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.15
        ),
        margin = list(l = 60, r = 30, t = 120, b = 120),
        showlegend = TRUE
      )
    
    }
    
    # datos_esp <- datos_proc %>%
    #   group_by(especie) %>%
    #   summarise(n = n(), .groups = "drop") %>%
    #   mutate(
    #     porcentaje = n / sum(n) * 100
    #   )
    # 
    # if (is.null(datos_esp) || nrow(datos_esp) == 0) {
    #   
    #   plot_ly() |> 
    #     layout(
    #       annotations = list(
    #         text = "Sin datos disponibles",
    #         x = 0.5,
    #         y = 0.5,
    #         showarrow = FALSE,
    #         font = list(size = 20)
    #       ),
    #       xaxis = list(showticklabels = FALSE, zeroline = FALSE),
    #       yaxis = list(showticklabels = FALSE, zeroline = FALSE)
    #     )
    #   
    # } else {
    #   
    #   plot_ly(
    #     datos_esp |> arrange(desc(n)),
    #     x = ~especie,
    #     y = ~n,
    #     type = "bar",
    #     marker = list(
    #       color = "#4caf50",
    #       line = list(color = "#2e7d32", width = 0.5)
    #     ),
    #     text = ~paste0(round(porcentaje, 1), "%"),
    #     textposition = "outside",
    #     hovertemplate = paste0(
    #       "<b>%{x}</b>",
    #       "<br>Total: %{y}",
    #       "<br>Participación: %{customdata:.1f}%",
    #       "<extra></extra>"
    #     ),
    #     customdata = ~porcentaje
    #   ) |>
    #     layout(
    #       title = list(
    #         text = paste0(
    #           "<b>Distribución de árboles por especie</b>",
    #           "<br><span style='color:#4caf50;font-size:12px;'>",
    #           "Cantidad absoluta y participación relativa</span>"
    #         ),
    #         x = 0.01,
    #         y = 0.95
    #       ),
    #       paper_bgcolor = "#f5f7fa",
    #       plot_bgcolor  = "#e8f5e9",
    #       xaxis = list(
    #         title = "Especie",
    #         tickangle = -45,
    #         showgrid = FALSE,
    #         tickfont = list(color = "#2c3e50")
    #       ),
    #       yaxis = list(
    #         title = "Total de árboles",
    #         showgrid = TRUE,
    #         gridcolor = "rgba(0,0,0,0.05)",
    #         zeroline = FALSE,
    #         tickfont = list(color = "#2c3e50"),
    #         titlefont = list(color = "#2c3e50")
    #       ),
    #       margin = list(l = 60, r = 30, t = 80, b = 120),
    #       showlegend = FALSE
    #     )
    # }

  })
  
  
#### Mapa  ####
  
  data_mapa_arboles <- reactive({
    req(plantaciones_arboles())
    datos <- plantaciones_arboles()
    # test
    # datos <- datos
    
    if (is.null(datos)) return(NULL)
    # datos <-
    datos |>
      filter(!is.na(latitud)) |>
      st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
  })
  
  output$selector_sitio_mapa <- renderUI({
    req(plantaciones_arboles()) 
    selectInput(
      "sitio_mapa",
      "Elegí un mapa:",
      choices = c("Sitios puntuales", "Estadísticas por sitio"),
      selected = "Sitios puntuales"   # <- opción por defecto
    )  })
  
  output$mapa_arboles <- renderLeaflet({
    req(data_mapa_arboles())
    
    datos <- data_mapa_arboles()
    
    # Si input$sitio_mapa no está todavía disponible, salimos sin error
    if (is.null(input$sitio_mapa)) return(leaflet() %>% addTiles())
    
    if (input$sitio_mapa == "Sitios puntuales") {
      
      if (is.null(datos) || nrow(datos) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPopups(lng = 0, lat = 0, popup = "Sin datos georreferenciados")
        )
      }
      
      leaflet(datos) %>%
        
        addProviderTiles(providers$CartoDB.Positron, 
                         group = "Base") %>%
        
        addPolygons(
          data = vecinales,
          color = "#ff5722",
          weight = 1,
          fillOpacity = 0.25,
          popup = ~paste0("<b>Vecinal:</b> ", nombre),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#ff9800",
            bringToFront = TRUE
          ),
          group = "Vecinales"
        ) %>%
        
        addCircleMarkers(
          radius = 1,
          color = "#4caf50",
          fillOpacity = 0.7,
          popup = ~paste0(
            "ID: ", ID, 
            "<br>Especie: ", especie,
            "<br><img src='", foto_plantado,
            "' width='192' height='256'>"),
            group = "Plantaciones"
        ) %>%
        addLayersControl(
          overlayGroups = c("Vecinales", "Plantaciones"),
          options = layersControlOptions(collapsed = FALSE)
        )
    } else if (input$sitio_mapa == "Estadísticas por sitio") {
      
      if (is.null(datos) || nrow(datos) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPopups(lng = 0, lat = 0, popup = "Sin datos georreferenciados")
        )
      }
      
      # Agrupar datos
      chartdata <- datos %>%
        st_drop_geometry() %>%
        group_by(sitio, especie) %>%
        summarise(cantidad = n(), .groups = "drop_last") %>%
        tidyr::pivot_wider(
          names_from = especie,
          values_from = cantidad,
          values_fill = 0
        ) %>%
        left_join(
          datos %>%
            mutate(
              lng = st_coordinates(geometry)[, 1],
              lat = st_coordinates(geometry)[, 2]
            ) %>%
            st_drop_geometry() %>%
            group_by(sitio) %>%
            summarise(
              lng = mean(lng, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              .groups = "drop"
            ),
          by = "sitio"
        )
      
      # Identificar columnas de especies
      labels_especies <- names(chartdata)[!(names(chartdata) %in% c("sitio", "lng", "lat"))]
      if (length(labels_especies) == 0) {
        labels_especies <- "Sin_datos"
        chartdata$Sin_datos <- 1
      }
      
      n_especies <- length(labels_especies)
      
      # Crear paleta sin errores
      if (!is.numeric(n_especies) || n_especies == 0) {
        paleta <- "#CCCCCC"
      } else if (n_especies <= 12) {
        paleta <- RColorBrewer::brewer.pal(max(3, n_especies), "Set3")[seq_len(n_especies)]
      } else {
        paleta <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_especies)
      }
      
      paleta[is.na(paleta)] <- grDevices::rainbow(sum(is.na(paleta)))
      
      leaflet(chartdata) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMinicharts(
          lng = chartdata$lng,
          lat = chartdata$lat,
          chartdata = chartdata[, labels_especies],
          type = "pie",
          colorPalette = paleta,
          width = 70
        )
    } 
  })
  
  index <- reactiveVal(1)
  
  # Botones Previous / Next
  observeEvent(input[["previous"]], {
    index(max(index() - 1, 1))
    updateSliderInput(session, "index_slider", value = index())
  })
  
  observeEvent(input[["next"]], {
    index(min(index() + 1, nrow(plantaciones_arboles())))
    updateSliderInput(session, "index_slider", value = index())
  })
  
  # Cambiar índice cuando se mueva el slider
  observeEvent(input[["index_slider"]], {
    index(input[["index_slider"]])
  })
  
  # Ajustar slider cuando cambia dataset
  observe({
    n_filas <- nrow(plantaciones_arboles())
    updateSliderInput(session, "index_slider",
                      min = 1,
                      max = n_filas,
                      value = index(),
                      step = 1)
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
  
  # Grafico de especies:
  # TODO hacer para filtre de acuerdo al sitio
  
  
  
#### Base de datos #####

output$descargar_datos <- downloadHandler(
    filename = function() {
      paste0("arbolado_", Sys.Date(), ".csv")
    },
    content = function(file) {
      datos_sf <- data_mapa_arboles()
      
      if (!inherits(datos_sf, "sf")) {
        write.csv(datos_sf, file, row.names = FALSE)
      } else {
        datos <- cbind(st_drop_geometry(datos_sf), st_coordinates(datos_sf)) %>%
          rename(longitud = X, latitud = Y)
        write.csv(datos, file, row.names = FALSE)
      }
    }
  )
  
  output$descargar_datos_crudos <- downloadHandler(
    filename = function() {
      paste0("arbolado_", Sys.Date(), ".csv")
    },
    content = function(file) {
      datos <- plantaciones_arboles()
      write.csv(datos, file, row.names = FALSE)
    }
  )
  
 output$tabla_especies <- renderReactable({
    
     datos <- plantaciones_arboles()
     
     datos_sf <- datos |>
       filter(
         !is.na(latitud),
         !is.na(longitud)
       ) |>
       st_as_sf(
         coords = c("longitud", "latitud"),
         crs = 4326,
         remove = FALSE
       ) |>
       st_join(
         vecinales %>% select(vecinal = nombre),
         join = st_within
       ) |>
       st_drop_geometry(
       ) |>
       select(fecha_plantado, vecinal, especie
       ) |>
       group_by(fecha_plantado, vecinal, especie
       ) |>
       summarise(Cantidad = n(), .groups = "drop"
       ) |>
       rename(
         Especies = especie,
         Vecinal = vecinal,
         Fecha = fecha_plantado
       ) |>
       as.data.frame()
  

    if (is.null(datos) || nrow(datos) == 0) {
      return(NULL)  
    }
    
    reactable(datos_sf)
  })


}