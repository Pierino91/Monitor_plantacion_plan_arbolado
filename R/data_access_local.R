#' Leer Archivo CSV de Forma Segura
#' @param file_path Carácter. Ruta del archivo CSV.
#' @param delimiter Carácter. Separador de campos ("," o ";").
#' @return Un tibble o NULL si ocurre un error o el archivo no existe.
get_data_local_csv <- function(file_path, delimiter = ";") {
  sanitized_path <- stringr::str_replace_all(file_path, "\\\\", "/")
  
  if (!file.exists(sanitized_path)) {
    warning(paste("File not found at:", sanitized_path))
    return(NULL)
  }
  
  data <- tryCatch({
    if (delimiter == ";") {
      readr::read_csv2(sanitized_path, show_col_types = FALSE)
    } else {
      readr::read_csv(sanitized_path, show_col_types = FALSE)
    }
  }, error = function(e) {
    warning(paste("Failed to read CSV file:", e$message))
    return(NULL)
  })
  if ("created_at" %in% names(data)) {
    data$created_at <- lubridate::ymd_hms(data$created_at, tz = "UTC")
  }
  if ("uploaded_at" %in% names(data)) {
    data$uploaded_at <- lubridate::ymd_hms(data$uploaded_at, tz = "UTC")
  }
  if ("6_Hora" %in% names(data)) {
    data$`6_Hora` <- as.character(data$`6_Hora`)
  }
  return(data)
}