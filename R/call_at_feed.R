call_at_feed <- function(API_KEY = "894db084a530418f844238723d6da4bf") {
  
  no_trip_updates = TRUE
  no_vehicles = TRUE
  
  while (no_trip_updates & no_vehicles) {
    api_response = httr::RETRY(
      'GET',
      url = 'https://api.at.govt.nz/v2/public/realtime',
      # httr::add_headers('Ocp-Apim-Subscription-Key' = "e3eb7709330740558c6ddba51a20e447"),
      httr::add_headers('Ocp-Apim-Subscription-Key' = API_KEY),
      httr::timeout(20),
      quiet = FALSE,
      times = 10,
      pause_cap = 240
    )
    
    if (httr::http_error(api_response)) {
      stop('Error in accessing Auckland Transport API')
    } else {
      print("Request for combined feed successful")
    }
    
    parsed_response = jsonlite::fromJSON(httr::content(api_response, as = 'text'), simplifyDataFrame = TRUE)
    
    if (!is.null(parsed_response$response$entity$trip_update)) {
      no_trip_updates = FALSE
    }
    if (!is.null(parsed_response$response$entity$vehicle)) {
      no_vehicles = FALSE
    }
  }
  
  return (parsed_response)
}
