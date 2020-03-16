get_active_vehicles2 <- function(api_response, df_last_stop_in_route, route_destinations, refresh_time) {
  
  trip_updates = api_response$response$entity$trip_update
  vehicle_pos = api_response$response$entity$vehicle
  
  # Flatten and tidy up trip responses
  stop_time_updates = dplyr::bind_cols(
    trip_updates$stop_time_update$arrival %>%
      dplyr::select(-uncertainty) %>%
      dplyr::rename_all(~ paste0('arrival_', .)),
    trip_updates$stop_time_update$departure %>%
      dplyr::select(-uncertainty) %>%
      dplyr::rename_all(~ paste0('departure_', .)),
    trip_updates$stop_time_update %>% dplyr::select(stop_sequence, stop_id)
  )
  
  tidy_trips = dplyr::bind_cols(
    trip_updates$trip %>% dplyr::select(-schedule_relationship),
    trip_updates$vehicle %>% dplyr::rename_all(~ paste0('vehicle_', .)),
    stop_time_updates,
    trip_updates %>% dplyr::select(timestamp, delay)
  ) %>%
    janitor::remove_empty('rows') %>%
    dplyr::mutate_at(c('stop_id', 'trip_id', 'route_id'),
                     ~ substring(., 0, regexpr('-', .) - 1)) %>%
    dplyr::left_join(df_last_stop_in_route, by = 'route_id') %>%
    dplyr::filter(stop_sequence < last_stop_seq) %>%
    dplyr::mutate(
      update_delay = dplyr::if_else(is.na(arrival_delay), departure_delay, arrival_delay),
      update_time = dplyr::if_else(is.na(arrival_time), departure_time, arrival_time)
    ) %>%
    dplyr::mutate(
      chosen_delay = dplyr::case_when(
        is.na(update_delay) ~ delay,
        timestamp > update_time ~ delay,
        TRUE ~ pmin(update_delay, delay)
      )
    )
  
  # Flatten and tidy up vehicle ids
  tidy_vehicles = dplyr::bind_cols(
    vehicle_pos$vehicle %>% dplyr::rename_all(~ paste0('vehicle_', .)),
    vehicle_pos$trip %>% dplyr::select(-schedule_relationship),
    vehicle_pos$position,
    vehicle_pos %>% dplyr::select(timestamp) %>% dplyr::rename_all(~ paste0('vehicle_', .))
  ) %>%
    janitor::remove_empty('rows') %>%
    dplyr::mutate_at(c('trip_id', 'route_id'),  ~ substring(., 0, regexpr('-', .) - 1))
  
  active_vehicles = dplyr::left_join(
    tidy_trips,
    tidy_vehicles,
    by = c(
      "trip_id",
      "start_time",
      "start_date",
      "route_id",
      "direction_id",
      "vehicle_id",
      "vehicle_label",
      "vehicle_license_plate"
    )
  ) %>%
    dplyr::filter(!is.na(latitude)) %>%
    dplyr::select(
      trip_id,
      route_id,
      start_time,
      direction_id,
      vehicle_id,
      stop_sequence,
      stop_id,
      timestamp,
      delay = chosen_delay,
      latitude,
      longitude
    ) %>%
    dplyr::mutate(
      timestamp = lubridate::with_tz(
        as.POSIXct(x = timestamp, origin = '1970-01-01', tz = 'UTC'),
        tzone = 'Pacific/Auckland')
    ) %>%
    dplyr::filter(timestamp > !!refresh_time - 900) %>%
    dplyr::left_join(route_destinations, by = 'route_id')
  
  return (active_vehicles)
}
