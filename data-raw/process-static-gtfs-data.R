# Matt Lie
# Script to process static gtfs data
# Data source from https://at.govt.nz/about-us/at-data-sources/general-transit-feed-specification/
# Script last updated: 4 Mar 2020
# Data last updated: 30 Jan 2020

library(magrittr)
library(dplyr)

# Process stop times
stop_times_temp <- readr::read_csv('./data-raw/at-gtfs/stop_times.txt', guess_max = 1000000)
# Insert some data checking assertions here; namely that data is not empty?

# Some stop times begin at 24:00:00, as they are "overnight"
# Convert the problems times to valid format times
probs = readr::problems(stop_times_temp) %>% 
  dplyr::mutate(new_hour = as.numeric(substr(actual, 1, 2)) - 24,
                rest = substr(actual, 3, 8),
                new_time = paste0('0', as.character(new_hour), rest))

stop_times_temp$row = rownames(stop_times_temp)

stop_times_need_time = stop_times_temp %>% dplyr::filter(is.na(departure_time))
stop_times_rem = stop_times_temp %>% dplyr::filter(!is.na(departure_time))

probs_wider = probs %>% 
  dplyr::mutate(new_time = readr::parse_time(new_time, format = "%AT")) %>% 
  dplyr::select(row, col, new_time) %>% 
  tidyr::pivot_wider(names_from = col, values_from = new_time) %>% 
  dplyr::mutate(row = as.character(row))

stop_times_need_time = stop_times_need_time %>% 
  dplyr::left_join(probs_wider, by = "row", suffix = c("", "_new")) %>% 
  dplyr::mutate(arrival_time = dplyr::if_else(is.na(arrival_time), arrival_time_new, arrival_time),
                departure_time = dplyr::if_else(is.na(departure_time), departure_time_new, departure_time)) %>% 
  dplyr::select(-c(arrival_time_new, departure_time_new))

stop_times = dplyr::bind_rows(stop_times_need_time, stop_times_rem) %>% 
  dplyr::select(c(trip_id, arrival_time, departure_time, stop_id, stop_sequence)) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::contains('_id')), ~substring(., 0, regexpr('-', .) - 1)) %>% 
  dplyr::distinct()

trips <- readr::read_csv('./data-raw/at-gtfs/trips.txt') %>% 
  dplyr::select(trip_id, service_id, route_id, trip_headsign) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::contains('_id')), ~substring(., 0, regexpr('-', .) - 1)) %>% 
  dplyr::mutate(trip_headsign = dplyr::if_else(trip_headsign == 'Otahuhu Train Station 1', 'Otahuhu', trip_headsign)) %>% 
  dplyr::distinct()

calendar = readr::read_csv('./data-raw/at-gtfs/calendar.txt') %>% 
  dplyr::select(service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::contains('_id')), ~substring(., 0, regexpr('-', .) - 1)) %>% 
  dplyr::distinct()

routes = readr::read_csv('./data-raw/at-gtfs/routes.txt') %>% 
  dplyr::select(route_id, route_short_name, route_type) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::contains('_id')), ~substring(., 0, regexpr('-', .) - 1)) %>% 
  dplyr::distinct()

stops = readr::read_csv('./data-raw/at-gtfs/stops.txt') %>% 
  dplyr::select(stop_id, stop_code, stop_name, stop_lat, stop_lon) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::contains('_id')), ~substring(., 0, regexpr('-', .) - 1)) %>% 
  dplyr::distinct()

df_trips_full = stop_times %>% 
  dplyr::left_join(trips, by = 'trip_id') %>% 
  dplyr::left_join(calendar, by = 'service_id') %>% 
  dplyr::left_join(routes, by = 'route_id') %>% 
  dplyr::left_join(stops %>% dplyr::select(stop_id, stop_name), by = 'stop_id') %>% 
  dplyr::filter(route_type != 4) %>% 
  dplyr::filter(trip_headsign != 'Schools')

# Handle dupe stop names
dupe_stops = df_trips_full %>% 
  dplyr::select(stop_id, stop_name) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(stop_id) %>% 
  dplyr::tally() %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::pull(stop_id)

clean_stop_name <- function(x) {
  x = gsub(' East', '', x)
  x = gsub(' West', '', x)
  x = gsub('Stop A ', '', x)
  x  = gsub("^\\s+|\\s+$", "", x)
  return (x)
}

df_trips_full = df_trips_full %>% 
  dplyr::mutate(
    stop_name = dplyr::if_else(stop_id %in% !!(dupe_stops), clean_stop_name(stop_name), stop_name)
  )

df_routes_in_stops = df_trips_full %>% 
  dplyr::select(stop_name, route_type, route_short_name, trip_headsign) %>% 
  dplyr::mutate(route_to = paste(route_short_name, 'to', trip_headsign)) %>% 
  dplyr::distinct() 

full_stop_names = df_routes_in_stops %>% 
  dplyr::arrange(stop_name) %>% 
  dplyr::pull(stop_name) %>% 
  unique()

df_stops = stops %>% 
  dplyr::select(stop_id, stop_name, stop_code, stop_lon, stop_lat) %>% 
  dplyr::filter(stop_name %in% full_stop_names)

saveRDS(full_stop_names, file = './data/full_stop_names.Rds')
saveRDS(df_stops, file = './data/df_stops.Rds')
saveRDS(df_routes_in_stops, file = './data/df_routes_in_stops.Rds')
saveRDS(df_trips_full, file = './data/df_trips_full.Rds')
