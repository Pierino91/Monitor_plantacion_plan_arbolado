source("global.R")

function(input, output, session) {
  refrescar_min <- 30
  # Reactive timer para actualización automática
  auto_update <- reactiveTimer(refrescar_min * 60 * 100)
  
  datos_plantaciones <- reactive({
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
  
  # Procesamiento de datos reactivo
  plantaciones_arboles <- reactive({
    datos <- datos_plantaciones()
    if (is.null(datos)) return(NULL)
    datos |>
      rename(
        fecha_hora_arbol_relevado = "created_at",
        fecha_hora_arbol_sincronizado = "uploaded_at",
        latitud = "latitude",
        longitud = "longitude",
        especie = "9_Especie",
        edad = "10_Edad_meses",
        altura = "11_Altura_cm",
        origen = "12_Origen",
        foto_plantado = "16_Foto_del_plantado",
        lugar_especifico = "13_Tipo_de_intervenc",
        obs = "17_Observacion"
      ) |>
      select(fecha_hora_arbol_relevado, fecha_hora_arbol_sincronizado, latitud, longitud, especie, foto_plantado, lugar_especifico, altura, obs) |>
      mutate(
        fecha_hora_arbol_relevado = ymd_hms(fecha_hora_arbol_relevado, tz = "UTC"),
        especie = factor(especie),
        lugar_especifico = factor(lugar_especifico),
        altura = as.numeric(altura),
        obs = as.character(obs),
        latitud = as.numeric(latitud),
        longitud = as.numeric(longitud)
      ) |>
      arrange(fecha_hora_arbol_relevado, fecha_hora_arbol_sincronizado) |>
      mutate(ID = row_number())
  })
  
  output$fecha_max <- renderUI({
    datos <- plantaciones_arboles()
    fecha_max <- if (!is.null(datos) && nrow(datos) > 0) {
      max(datos$fecha_hora_arbol_relevado, na.rm = TRUE)
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
      select(fecha_hora_arbol_relevado, especie) |>
      filter(!is.na(fecha_hora_arbol_relevado)) |>
      arrange(fecha_hora_arbol_relevado) |>
      mutate(
        mes = month(fecha_hora_arbol_relevado, label = TRUE, abbr = FALSE)
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
    datos <- plantaciones_arboles()
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
      select(lugar_especifico, especie) |>
      group_by(lugar_especifico, especie) |>
      reframe(Cantidad = n()) |>
      rename("Especies" = "especie", "Lugar específico" = "lugar_especifico")
  })
  
  mapa_arboles <- reactive({
    datos <- plantaciones_arboles()
    if (is.null(datos)) return(NULL)
    datos |>
      filter(!is.na(latitud)) |>
      st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
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
      add_bars(
        data = datos_esp,
        x = ~mes,
        y = ~n_especies,
        color = ~especie,
        colors = "Set2",
        name = "Especies",
        opacity = 0.7
      ) %>%
        # Línea (geom_line)
        add_lines(
          data = datos_tiempo,
          x = ~mes,
          y = ~acumulado,
          name = "Acumulado",
          line = list(width = 2, color = 'black'),
          yaxis = "y2"
        ) %>%
        # Puntos (geom_point)
        add_markers(
          data = datos_tiempo,
          x = ~mes,
          y = ~acumulado,
          name = "Puntos Acumulado",
          marker = list(size = 6, color = 'black'),
          yaxis = "y2"
        ) %>%
        layout(
          xaxis = list(title = "Mes"),
          yaxis = list(title = "Árboles"),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = "Acumulado"
          ),
          legend = list(title = list(text = "<b>Especies</b>"))
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
        labels = ~especie,
        values = ~n,
        type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial'
      ) |> layout(showlegend = FALSE)
    }
  })
  


}