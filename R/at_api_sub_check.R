### Subscription check
### Run up during start up to check if Auckland Transport API is working
at_api_sub_check <- function() {
  
  api_response2 = httr::RETRY(
    'GET',
    url = 'https://api.at.govt.nz/v2/gtfs/versions',
    # httr::add_headers('Ocp-Apim-Subscription-Key' = "e3eb7709330740558c6ddba51a20e447"),
    httr::add_headers('Ocp-Apim-Subscription-Key' = "894db084a530418f844238723d6da4bf"),
    httr::timeout(20),
    quiet = FALSE,
    times = 10,
    pause_cap = 240
  )
  
  parsed_response = jsonlite::fromJSON(httr::content(api_response2, as = 'text'), simplifyDataFrame = TRUE)
  print(parsed_response$response)
  
}

at_api_sub_check()
