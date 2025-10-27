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
  
  datos_monitor <- reactive({
    auto_update()  # Actualiza cada vez que el timer se activa
    # Obtener el primer enlace del endpoint
    tryCatch({
      first_link <- get_data(endpoint_api_monitor)$links$first
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
      # datos_tec <- get_all_entries(first_link)
      
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
  arboles_monitoreo <- reactive({
    
    datos <- datos_monitor()
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
        estado = "Semforo_del_estad",
        obs = "Observacion_del_m",
        foto_monitoreo = "Foto_del_arbol",
        presencia  = "El_arbol_est_pres",
        sitio = "Lugar",
        agente = "Agente",
        momento_monitoreo = "created_at.x"
      ) %>%
      mutate(
        momento_monitoreo = ymd_hms(momento_monitoreo, tz = "UTC"),
        sitio = factor(sitio),
        obs = as.character(obs),
        # Imagen miniatura con click → popup
        foto_monitoreo = ifelse(
          foto_monitoreo != "",
          paste0("<img src='", foto_monitoreo, "' width='120' style='cursor:pointer;'/>"),
          ""
        )
      ) %>%
      select(agente, sitio, estado, obs, foto_monitoreo, presencia, momento_monitoreo)
      
  })

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
        fecha_plantado = ymd(dmy(fecha_plantado, tz = "UTC")),
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
  
  ####  UI #####
  
  output$selector_sitio <- renderUI({
    req(plantaciones_arboles()) 
    
    datos <- plantaciones_arboles()
    
    sitios <- unique(datos$sitio)
    fluidRow(
      column(8, selectInput("sitio", "Elegí un sitio:", choices = c("TODAS", sitios))),
      column(4, tags$h4(textOutput("total_arboles")))
    )  
    })
  
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
  

  # Variables para graficar y mostrar
  plantaciones_especies_tiempo <- reactive({
    datos <- plantaciones_arboles()
    if (is.null(datos)) return(NULL)
    datos |>
    # datos_limpios |>
      select(fecha_plantado, especie) |>
      filter(!is.na(fecha_plantado)) |>
      group_by(fecha_plantado, especie) |>
      reframe(n_especies = n())
  })
  
  plantaciones_tiempo <- reactive({
    datos <- plantaciones_especies_tiempo()
    if (is.null(datos)) return(NULL)
    datos |>
      ungroup() |>
      group_by(fecha_plantado) |>
      reframe(
        n = sum(n_especies, na.rm = TRUE),
        .groups = "drop") |>
      mutate(
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
      select(fecha_plantado, sitio, especie) |>
      group_by(fecha_plantado, sitio, especie) |>
      reframe(Cantidad = n()) |>
      rename("Especies" = "especie", 
             "Lugar específico" = "sitio",
             "Fecha" = "fecha_plantado"
             )
  })
  
  #### Mapa ####
  
  mapa_arboles <- reactive({
    datos <- plantaciones_arboles()
    if (is.null(datos)) return(NULL)
    datos |>
      filter(!is.na(latitud)) |>
      st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
  })
  
  #### Imagenes ####
  
  
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
  
  
  
  
  
  # Mostrar imagen
  # output$image <- renderUI({
  #   df <- plantaciones_arboles()
  #   idx <- index()
  #   
  #   if (nrow(df) >= idx) {
  #     # Supongamos que df tiene una columna llamada "imagen" con la ruta o el nombre del archivo
  #     tags$img(src = df$imagen[idx], width = "100%")
  #   } else {
  #     tags$p("No hay imagen disponible")
  #   }
  #   
    # # test
    #  x <- imgs %>%
    #    pull(foto_plantado) %>%
    #    .[1]
    
  
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
      # setView(mean(as.numeric(datos$longitud), na.rm = TRUE), 
      #         mean(as.numeric(datos$latitud), na.rm = TRUE),
      #         zoom = 16)%>%
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
        # Barras apiladas
        add_bars(
          data = datos_esp,
          x = ~fecha_plantado,
          y = ~n_especies,
          color = ~especie,
          colors = colores,
          name = ~especie,
          opacity = 0.7,
          yaxis = "y"
        ) |>
        # Línea acumulada (segundo eje)
        add_lines(
          data = datos_tiempo,
          x = ~fecha_plantado,
          y = ~acumulado,
          name = "Acumulado",
          line = list(width = 2, color = 'black'),
          yaxis = "y2"
        ) |>
        # Puntos acumulados (segundo eje)
        add_markers(
          data = datos_tiempo,
          x = ~fecha_plantado,
          y = ~acumulado,
          name = "Puntos Acumulado",
          marker = list(size = 6, color = 'black'),
          yaxis = "y2"
        ) |>
        layout(
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Árboles (por especie)"),
          yaxis2 = list(
            title = "Acumulado",
            overlaying = "y",
            side = "right",
            showgrid = FALSE
          ),
          margin = list(l = 0, r = 80, t = 0, b = 0), # 👈 más margen derecho e izquierdo
          showlegend = FALSE,
          barmode = 'stack',
          legend = list(orientation = "h", y = -0.2) # opcional, para ubicar leyenda abajo
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
  
  # Grafico de especies:
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
      ) |> layout(showlegend = FALSE,
                  yaxis = list(title = "Total de árboles") 
                  
      ) 
        
    }
    
  })
  
##### Monitoreo ####
  
  output$selector_sitio_monitor <- renderUI({
    req(arboles_monitoreo()) 
    datos <- arboles_monitoreo()
    datos$sitio <- as.character(datos$sitio)
    sitios <- unique(datos$sitio)
    print(sitios)
  selectInput("sitio_monitor", "Elegí un sitio:", choices = c("TODAS", sitios))
  })
  
  output$Tortas_Presencia <- renderPlotly({
    req(arboles_monitoreo())
    req(input$sitio_monitor)
    
    if(input$sitio_monitor != "TODAS"){
      datos <- arboles_monitoreo() %>% 
        filter(sitio == input$sitio_monitor)%>%
        select(presencia)%>%
        group_by(presencia)%>%
        reframe(n = n())%>%
        ungroup()%>%
        mutate(
          total=sum(n),
          porc=n/total*100
        ) 
    }else{
      datos <- arboles_monitoreo() %>%
        select(presencia)%>%
        group_by(presencia)%>%
        reframe(n = n())%>%
        ungroup()%>%
        mutate(
          total=sum(n),
          porc=n/total*100
        ) 
    }
    
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
      # Crear una lista de gráficos (uno por sitio)
        plot_ly(
          data =  datos,
          labels = ~presencia,
          values = ~n,
          type = 'pie',
          text = ~paste0(round(porc, 1), "%"),
          textposition = 'inside',
          textinfo = 'label+text',
          hoverinfo = 'label+percent+value',
          showlegend = TRUE
        )
    }
    
  })
  
  
  output$Barras_monitoreo <- renderPlotly({
    
    datos <- arboles_monitoreo(
    )%>%
      select(sitio, estado)%>%
      group_by(sitio,estado)%>%
      reframe(n = n())%>%
      ungroup()%>%
      group_by(sitio)%>%
      mutate(
        total=sum(n),
        porc=n/total*100
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
    plot_ly(
      datos,
      x = ~sitio,
      y = ~n,
      type = 'bar',
      color = ~estado,
      # text = ~paste0(round(porc, 1), "%"),  # 🔹 etiqueta con porcentaje
      textposition = 'outside',             # 🔹 texto fuera de la barra
      hoverinfo = 'text',
      hovertext = ~paste(
        "Sitio:", sitio,
        "<br>Estado:", estado,
        "<br>Cantidad:", n,
        "<br>Porcentaje:", round(porc, 1), "%"
      )
    ) |> 
      layout(
        yaxis = list(title = "Cantidad"),
        xaxis = list(title = "Sitio"),
        barmode = 'stack'  # o 'group' si preferís barras separadas
      )
  }
                
    
  })
  
  
  output$tabla_monitoreo <- renderReactable({
    
    datos <- arboles_monitoreo()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(NULL)  
    }
    reactable(datos, 
              rowStyle = function(index) {
                estado <- datos$estado[index]
                color <- if (estado == "Saludable") {
                  "#d4edda"   # verde claro
                } else if (estado == "Atención urgente") {
                  "#f8d7da"   # rojo claro
                } else if (estado == "Con problemas leves") {
                  "#fff3cd"   # amarillo claro
                } else {
                  "white"
                }
                list(background = color)
              },
      columns = list(
      `foto_monitoreo` = colDef(name = "Foto", html = TRUE),
      `agente`= colDef(name = "Agente responsable"),
      `sitio`= colDef(name = "Lugar"),
      `estado` = colDef(name = "Estado"),
      `obs` = colDef(name = "Observación"),
      `presencia` = colDef(name = "Presencia"),
      `momento_monitoreo` = colDef(name = "Fecha del Monitoreo")
    ),
    bordered = TRUE,
    highlight = TRUE,
    striped = TRUE
   )
  })

}