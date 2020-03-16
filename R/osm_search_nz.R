osm_search_nz <- function(query, limit = 1) {
  
  countrycodes = 'nz'
  # email = 'matthew@harmonic.co.nz'
  # key = 'STDuONxEBMZJPaGZCwzVlPd8kYCfgVzz'
  
  # Target url
  # https://nominatim.openstreetmap.org/?q=22+nolan+road&countrycodes=nz&limit=1&format=json
  
  # base_url = 'http://open.mapquestapi.com/nominatim/v1/search.php/?'
  base_url = 'https://nominatim.openstreetmap.org/?'
  query_string = paste0('q=', gsub(' ', '+', query))
  country_params = paste0('countrycodes=', countrycodes)
  limit_params = paste0('limit=', limit)
  # email_params = paste0('email=', curl::curl_escape(email))
  # key_params = paste0('key=', key)
  format_params = 'format=json'
  
  # params = paste(query_string, country_params, email_params, key_params, limit_params, format_params, sep='&')
  params = paste(query_string, country_params, limit_params, format_params, sep='&')
  url = paste0(base_url, params)
                        
  api_response = httr::RETRY(
    'GET',
    url = url,
    quiet = FALSE,
    times = 10,
    pause_cap = 240
  )
  
  if (httr::http_error(api_response)) {
    stop('Error accessing OSM Nominatim API')
  } 
  
  parsed_response <- jsonlite::fromJSON(httr::content(api_response, as = 'text'), simplifyDataFrame = TRUE)
  
  if (length(parsed_response) == 0) {
    geocode = data.frame(lat = c(), lng = c())
  } else {
    geocode = parsed_response %>% 
      dplyr::select(lat, lng = lon) %>% 
      dplyr::mutate_all(as.numeric)
  }

  return (geocode)
}