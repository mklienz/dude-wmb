append_active_vehicles <- function(active_vehicles, filtered_routes) {
  
  vehicles_per_route = active_vehicles %>% 
    dplyr::group_by(route_short_name, trip_headsign) %>% 
    dplyr::tally()
  
  df = filtered_routes %>% 
    dplyr::left_join(vehicles_per_route, by = c("route_short_name", "trip_headsign")) %>% 
    dplyr::mutate(n = tidyr::replace_na(n, 0))
  
  return (df)
}