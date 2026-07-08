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
