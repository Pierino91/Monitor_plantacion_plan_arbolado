# Carga de Unidades Estatales
UNIDADES_MUNICIPALES <- sf::st_read("www/Unidades de gestión de SSPP.kml", quiet = TRUE) %>%
  sf::st_zm(drop = TRUE, what = "ZM") %>%
  sf::st_transform(4326) %>%
  dplyr::rename(nombre = "Name")
# Carga de Vecinales
VECINALES <- sf::st_read("www/Vecinales.kml", quiet = TRUE) %>%
  sf::st_zm(drop = TRUE, what = "ZM") %>%
  sf::st_transform(4326) %>%
  dplyr::rename(nombre = "Name")
# Configuración Censo Radios Censales
densidades_poblacionales <- xml2::read_xml("www/Radios_censales/Intervalo_densidades.qml")
categorias <- c(0, 10, 25, 50, 100, 150, 200, 250, Inf)
escala <- c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d")

radio_censales <- sf::st_read("www/Radios_censales/Radios_censales.shp", quiet = TRUE) %>%  
  sf::st_transform(4326)

paleta_densidad <- leaflet::colorBin(
  palette = escala,
  domain = radio_censales$Den_hab.ha,
  bins = categorias
)

#' Agregar Capas Base y Polígonos Comunes al Mapa
#' Evita la redundancia de añadir polígonos base en cada tipo de vista
add_base_layers <- function(map, radio_censales_data, paleta) {
  map %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Base") %>%
    addPolygons(
      data = VECINALES,
      color = "#ff5722",
      weight = 1,
      fillOpacity = 0.25,
      popup = ~paste0("<b>Vecinal:</b> ", nombre),
      group = "VECINALES"
    ) %>%
    addPolygons(
      data = UNIDADES_MUNICIPALES,
      color = "#22cbff",
      weight = 1,
      fillOpacity = 0.25,
      popup = ~paste0("<b>Unidades municipales:</b> ", nombre),
      highlightOptions = highlightOptions(weight = 3, color = "#225cff", bringToFront = TRUE),
      group = "UNIDADES MUNICIPALES"
    ) %>%
    addPolygons(
      data = radio_censales_data,
      fillColor = ~paleta(Den_hab.ha),
      fillOpacity = 0.5,
      color = "#444444",
      weight = 1,
      group = "DENSIDAD POBLACIONAL"
    ) %>%
    addLegend(
      data = radio_censales_data,
      pal = paleta,
      values = ~Den_hab.ha,
      title = "Densidad hab/ha",
      opacity = 0.5,
      group = "DENSIDAD POBLACIONAL"
    )
}

#' Procesar datos espaciales para Minicharts de estadísticas
#' Extrae coordenadas de forma limpia aislando la lógica de manipulación de datos
prepare_chart_data <- function(datos) {
  # Agrupar conteo por sitio y especie
  resumen_especies <- datos %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(sitio, especie) %>%
    dplyr::summarise(cantidad = dplyr::n(), .groups = "drop_last") %>%
    tidyr::pivot_wider(names_from = especie, values_from = cantidad, values_fill = 0)
  
  # Calcular centroides o promedios geográficos por sitio
  coordenadas_sitio <- datos %>%
    dplyr::mutate(
      lng = sf::st_coordinates(geometry)[, 1],
      lat = sf::st_coordinates(geometry)[, 2]
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(sitio) %>%
    dplyr::summarise(
      lng = mean(lng, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE),
      .groups = "drop"
    )
  
  dplyr::left_join(resumen_especies, coordenadas_sitio, by = "sitio")
}