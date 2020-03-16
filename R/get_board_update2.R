get_board_update <- function(api_response, trips, current_time) {
  
  trip_updates = api_response$response$entity$trip_update

  board_times <- data.frame(
    trip_id = trip_updates$trip$trip_id,
    trip_start_time = trip_updates$trip$start_time,
    trip_last_stop = trip_updates$stop_time_update$stop_sequence,
    arrival_delay = trip_updates$stop_time_update$arrival$delay,
    departure_delay = trip_updates$stop_time_update$departure$delay,
    trip_status = trip_updates$trip$schedule_relationship
  ) %>%
    dplyr::mutate_if(is.factor, as.character) %>% 
    dplyr::mutate(trip_id = substring(trip_id, 0, regexpr('-', trip_id) - 1)) %>%
    dplyr::filter(trip_id %in% trips$trip_id) %>%
    dplyr::mutate(delay = dplyr::if_else(is.na(arrival_delay), departure_delay, arrival_delay)) %>%
    dplyr::mutate(trip_status = dplyr::case_when(
      trip_status == 0 ~ 'SCHEDULED',
      trip_status == 1 ~ 'ADDED SERVICE',
      trip_status == 2 ~ 'UNSCHEDULED',
      trip_status == 3 ~ 'CANCELLED',
      TRUE ~ 'UNKNOWN'
    )) %>% 
    dplyr::select(-c(arrival_delay, departure_delay)) %>% 
    dplyr::mutate(trip_start_time = round(hms::as_hms(trip_start_time), 0) - hms::as_hms(0))
  
  # print(nrow(trips))
  
  board_times = trips %>% 
    dplyr::full_join(board_times, by = 'trip_id')
  
  # print(nrow(board_times))
    
  board_times = board_times %>% 
    dplyr::mutate(delay = round(delay/60, 0),
                  due = as.numeric(round((arrival_time - !!current_time)/60, 0) + delay)) %>% 
    dplyr::filter(is.na(arrival_time) | arrival_time < !!current_time + 7200) %>% 
    dplyr::arrange(arrival_time)
  
  board_times = board_times %>%
    dplyr::mutate(
      due = dplyr::case_when(
        trip_status == 'CANCELLED' ~ 'Cancelled',
        trip_last_stop >= stop_sequence ~ 'Passed',
        trip_last_stop < stop_sequence & due <= 0 ~ 'Arriving',
        is.na(trip_last_stop) ~ 'Yet to start',
        TRUE ~ as.character(due)
      ),
      delay = dplyr::case_when(
        is.na(trip_last_stop) ~ 'NA',
        TRUE ~ as.character(delay)
      ),
    )    
  
  return(board_times)
}
