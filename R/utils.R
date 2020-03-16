walk_time_osrm <- function(input_df) {
  # checks
  # Assert that origin is a 1 x 2 tibble with lon and lat as column headings
  # Assert that dest is a 1 x 2 tibble with lon and lat as column headings
  origin_lng = input_df$origin_lng
  origin_lat = input_df$origin_lat
  dest_lng = input_df$dest_lng
  dest_lat = input_df$dest_lat
  
  # parse coords into required format
  origin_coords = paste(origin_lng, origin_lat, sep = ",")
  dest_coords = paste(dest_lng, dest_lat, sep = ",")
  coords = paste(origin_coords, dest_coords, sep = ';' )
  
  # Define OSRM routing source
  src = "https://routing.openstreetmap.de/routed-foot"
  service = "route"
  version = "v1"
  profile = "foot"
  params = "overview=false"
  
  # Define the request url
  url = paste(src, service, version, profile, coords, sep = '/')
  url = paste(url, params, sep = "?")

  api_response <- httr::RETRY('GET', url = url)
  
  osrm_error_msgs <- tibble::tribble(
    ~type,                                                                       ~description,
    "Ok",                                          "Request could be processed as expected.",
    "InvalidUrl",                                                           "URL string is invalid.",
    "InvalidService",                                                         "Service name is invalid.",
    "InvalidVersion",                                                            "Version is not found.",
    "InvalidOptions",                                                             "Options are invalid.",
    "InvalidQuery",                                    "The query string is synctactically malformed.",
    "InvalidValue",                            "The successfully parsed query parameters are invalid.",
    "NoSegment",          "One of the supplied input coordinates could not snap to street segment.",
    "TooBig", "The request size violates one of the service specific request size restrictions.",
    "NoRoute",                                                                  "No route found."
  )
  
  # Check for api call errors
  if (httr::http_error(api_response)) {
    msg = api_response$status_code
    stop(paste('Error accessing OSRM API:', msg))
  } 
  
  parsed_response <- jsonlite::fromJSON(httr::content(api_response, as = 'text'), simplifyDataFrame = TRUE)$routes
  result = parsed_response %>% dplyr::pull(duration)
  
  return (result)
}

get_wanted_routes <- function(stops, services, df_routes) {
  
  result = df_routes %>% 
    dplyr::filter(stop_name %in% stops, route_type %in% services) %>% 
    dplyr::arrange(route_short_name) %>% 
    dplyr::pull(route_short_name) %>% 
    unique()
  
  return (result)
  
}