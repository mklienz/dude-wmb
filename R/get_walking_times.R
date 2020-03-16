# Input - board times output
# Return - board times with a column containing walking times
get_walking_times <- function(df_selected_stops, user_location) {
  
  df = df_selected_stops %>% 
    dplyr::mutate(dest_lat = user_location$lat,
                  dest_lng = user_location$lng) %>%
    dplyr::rename(origin_lat = stop_lat,
                  origin_lng = stop_lon) %>%
    dplyr::group_by(stop_id, stop_name) %>%
    tidyr::nest() %>%
    dplyr::mutate(time_to_stop = round(purrr::map_dbl(data, walk_time_osrm)/60, 0)) %>%
    tidyr::unnest(cols = c(data)) %>% 
    dplyr::ungroup()
  
  return (df)
  
}