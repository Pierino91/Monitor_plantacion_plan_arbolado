#' Núcleo de Descarga y Paginación Resiliente de Epicollect5
.fetch_epicollect_pages <- function(project_slug, query_params, api_token = NULL) {
  max_attempts <- 5
  current_page <- 1
  all_entries <- list()
  has_more_pages <- TRUE
  
  base_url <- sprintf("https://five.epicollect.net/api/export/entries/%s", project_slug)
  request_headers <- c()
  if (!is.null(api_token)) {
    request_headers <- httr::add_headers(Authorization = paste("Bearer", api_token))
  }
  
  while (has_more_pages) {
    attempt <- 1
    page_success <- FALSE
    parsed_json <- NULL
    query_params$page <- current_page
    
    repeat {
      resp <- try(httr::GET(base_url, query = query_params, config = request_headers, httr::timeout(20)), silent = TRUE)
      if (inherits(resp, "try-error")) {
        if (attempt >= max_attempts) return(NULL)
        Sys.sleep(2 * attempt); attempt <- attempt + 1; next
      }
      
      status <- httr::status_code(resp)
      if (status == 429) { # Too Many Requests
        if (attempt >= max_attempts) return(NULL)
        Sys.sleep(10 * attempt); attempt <- attempt + 1; next
      }
      if (status != 200) {
        if (attempt >= max_attempts) return(NULL)
        Sys.sleep(2 * attempt); attempt <- attempt + 1; next
      }
      
      parsed_text <- try(httr::content(resp, as = "text", encoding = "UTF-8"), silent = TRUE)
      parsed_json <- try(jsonlite::fromJSON(parsed_text, simplifyVector = FALSE), silent = TRUE)
      page_success <- TRUE
      break
    }
    
    if (page_success && !is.null(parsed_json)) {
      entries_fetched <- parsed_json$data$entries
      if (length(entries_fetched) > 0) all_entries <- c(all_entries, entries_fetched)
      
      current_page_meta <- parsed_json$meta$current_page
      last_page_meta <- parsed_json$meta$last_page
      if (!is.null(current_page_meta) && !is.null(last_page_meta) && current_page_meta < last_page_meta) {
        current_page <- current_page + 1
        Sys.sleep(0.5)
      } else { has_more_pages <- FALSE }
    } else { has_more_pages <- FALSE }
  }
  return(all_entries)
}

get_data_API <- function(project_slug, form_ref, last_date_local_update = NULL, api_token = NULL) {
  initial_params <- list(form_ref = form_ref, per_page = 500)
  if (!is.null(last_date_local_update)) {
    initial_params$filter_by <- "created_at"
    initial_params$filter_from <- format(as.POSIXct(last_date_local_update, tz = "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")
  }
  .fetch_epicollect_pages(project_slug, initial_params, api_token)
}

get_branch_data <- function(project_slug, form_ref, branch_ref, last_date_local_update = NULL, api_token = NULL) {
  initial_params <- list(form_ref = form_ref, branch_ref = branch_ref, per_page = 500)
  if (!is.null(last_date_local_update)) {
    initial_params$filter_by <- "created_at"
    initial_params$filter_from <- format(as.POSIXct(last_date_local_update, tz = "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")
  }
  .fetch_epicollect_pages(project_slug, initial_params, api_token)
}