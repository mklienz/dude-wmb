server <- function(input, output, session) {
  ##### SIDEBAR
  
  ##### USER LOCATION VALUE
  
  # Set up null location
  location <- reactiveVal(value = data.frame(lat = c(), lng = c()))
  
  # Update location to search location when it changes
  observeEvent(search_location(), {
    location(search_location())
  })
  
  # Update location to map click location when it changes
  observeEvent(input$user_map_click, {
    new_location = as.data.frame(input$user_map_click) %>%
      dplyr::select(lat, lng)
    location(new_location)
  })
  
  # Handling search location
  search_location <- eventReactive(input$user_location_search, {
    if (input$user_location == "") {
      location = data.frame(lat = c(), lng = c())
    } else {
      location = osm_search_nz(input$user_location)
    }
    return (location)
  }, ignoreNULL = FALSE)
  
  # Create messages to send if the location searched for is not found
  # Isolated from location definiton as to keep existing location in tact
  location_validation <- eventReactive(input$user_location_search, {
    validate(need(
      nrow(search_location()) > 0,
      'Location could not be found, try again'
    ))
  })
  
  output$loc_validation <- renderText({
    loc_valid = location_validation()
  })
  
  ##### Location map
  
  # Generate initial default map
  # Centered on Queen St Farmers
  output$user_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.Voyager') %>%
      setView(lng = 174.7646424,
              lat = -36.8486696,
              zoom = 14)
    
  })
  
  # Observer to set view and marker to a location found by user search
  observe({
    proxy <- leafletProxy('user_map')
    df = location()
    
    if (nrow(df) > 0) {
      proxy %>%
        clearMarkers() %>%
        addMarkers(data = df,
                   lng = ~ lng,
                   lat = ~ lat) %>%
        setView(lng = df$lng,
                lat = df$lat,
                zoom = 15)
    }
  })
  
  # Observer to update location search UI to inform user that the click location is being used
  observeEvent(input$user_map_click, {
    updateSearchInput(session = session,
                      inputId = 'user_location',
                      value = 'Using map location')
  }, ignoreInit = TRUE)
  
  #### REFRESHING API DATA
  refresh_time <-
    reactiveVal(value = lubridate::with_tz(Sys.time(), tz = "Pacific/Auckland"))
  
  api_response <- reactive({
    waiter_show(html = tagList(
      spin_solar(),
      h4('Getting data from AT', style = 'color: #283D51;')
    ),
    color = waiter::transparent(0.7))
    input$refresh_now
    if (as.numeric(input$refresh_interval) > 0) {
      invalidateLater(as.numeric(input$refresh_interval) * 1000)
    }
    refresh_time(lubridate::with_tz(Sys.time(), tz = "Pacific/Auckland"))
    response = call_at_feed()
    waiter_hide()
    return(response)
  })
  
  observe({
    if (is.null(input$chosen_routes) ||
        input$chosen_routes == '' || length(route_list()) == 0) {
      shinyjs::disable('send_routes')
      updateActionButton(session,
                         inputId = 'send_routes',
                         label = 'Select some routes first')
    } else {
      shinyjs::enable('send_routes')
      updateActionButton(session,
                         inputId = 'send_routes',
                         label = 'Show me where these vehicles are')
    }
  })
  
  observeEvent(input$send_routes, {
    updateSelectizeInput(
      session,
      inputId = 'selected_routes',
      label = "Select routes that you're interested in:",
      choices = full_route_names,
      selected = input$chosen_routes
    )
    
    updateTabItems(session,
                   inputId = 'tabs',
                   selected = 'live-buses')
  })
  
  
  filtered_routes_table <- reactive({
    df <- route_destinations %>%
      dplyr::filter(route_short_name %in% !!input$selected_routes) %>%
      dplyr::arrange(route_short_name, trip_headsign) %>%
      dplyr::select(route_short_name, trip_headsign, route_type) %>%
      dplyr::distinct()
    return (df)
  })
  
  active_vehicles <- reactive({
    df = get_active_vehicles2(api_response(),
                              df_last_stop_in_route,
                              route_destinations,
                              refresh_time()) %>%
      dplyr::mutate(route_to = paste(route_short_name, 'to', trip_headsign))
    return(df)
  })
  
  active_routes_table <- reactive({
    df = append_active_vehicles(active_vehicles(), filtered_routes_table()) %>%
      dplyr::mutate(route_to = paste(route_short_name, 'to', trip_headsign))
  })
  
  output$table <- DT::renderDT({
    active_routes_table() %>%
      dplyr::select(route_to, n)
  },
  rownames = FALSE,
  colnames = c('Route/Destination' = 'route_to', 'Num Active' = 'n'),
  options = list(dom = 't', scrollX = TRUE))
  
  route_filters <- reactive({
    df = active_routes_table()[input$table_rows_selected, ] %>%
      dplyr::select(route_short_name, trip_headsign, route_to)
    return (df)
  })
  
  filtered_vehicles <- reactive({
    df = active_vehicles() %>%
      dplyr::filter(route_to %in% route_filters()$route_to) %>%
      dplyr::select(
        route_id,
        route_short_name,
        route_type,
        trip_headsign,
        delay,
        latitude,
        longitude
      ) %>%
      dplyr::mutate(route_to = as.character(paste(
        route_short_name, 'to', trip_headsign
      )))
    
    if (nrow(df) > 0) {
      colour_routes = data.frame(route_to = unique(df$route_to),
                                 colour = colours[1:length(unique(df$route_to))]) %>%
        dplyr::mutate_if(is.factor, as.character)
      df = df %>%
        dplyr::left_join(colour_routes, by = 'route_to')
    } else {
      df$colour = character(0)
    }
    
    return(df)
  })
  
  filtered_route_geoms <- reactive({
    df = filtered_vehicles() %>%
      dplyr::select(route_id, route_to, colour) %>%
      dplyr::distinct() %>%
      dplyr::left_join(df_route_geoms %>% dplyr::select(route_id, Shape__Len, geometry),
                       by = 'route_id')
    
    return(df)
    
  })
  
  # Build base map
  output$routes_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.Voyager') %>%
      setView(lng = 174.7646424,
              lat = -36.8486696,
              zoom = 15)
  })
  
  # Plotting stops for all of the routes requires that we collect the required stops
  stops_in_selected_routes <- reactive({
    df = df_routes_in_stops %>%
      dplyr::filter(route_to %in% filtered_vehicles()$route_to) %>%
      dplyr::left_join(df_stops, by = 'stop_name') %>%
      dplyr::left_join(filtered_vehicles() %>% dplyr::select(route_to, colour),
                       by = 'route_to') %>%
      dplyr::distinct()
  })
  
  observeEvent(input$table_rows_selected, {
    proxy <- leafletProxy('routes_map')
    proxy %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
  }, ignoreNULL = FALSE)
  
  # Add stops to map
  observeEvent(input$table_rows_selected, {
    proxy <- leafletProxy('routes_map')
    df = stops_in_selected_routes()
    
    proxy %>%
      addCircleMarkers(
        lng = df$stop_lon,
        lat = df$stop_lat,
        popup = df$stop_name,
        stroke = FALSE,
        # radius = 10,
        fillOpacity = 0.5,
        fillColor = df$colour,
        group = df$route_to
      )
    
    df3 = filtered_route_geoms() %>%
      st_as_sf() %>%
      group_by(route_to) %>%
      summarise(Shape__Len = max(Shape__Len),
                colour = first(colour))
    
    if (nrow(df3) > 0) {
      proxy %>%
        addPolylines(
          data = df3$geometry,
          group = df3$route_to,
          color = df3$colour
        )
    }
    
    df2 = filtered_vehicles() %>%
      dplyr::mutate(route_type = dplyr::if_else(route_type == 3, 'bus', 'train')) %>%
      dplyr::mutate(pop_up_msg = paste(sep = '<br/>', route_to, dplyr::if_else(
        delay > 0,
        paste('Behind by',
              as.character(round(abs(
                delay
              ) / 60)),
              'mins'),
        paste('Ahead by',
              as.character(round(abs(
                delay
              ) / 60)),
              'mins')
      )))
    
    df_legend = df2 %>%
      dplyr::select(route_to, colour) %>%
      dplyr::distinct()
    
    icons = awesomeIcons(
      # icon = 'bus',
      icon = df2$route_type,
      library = 'fa',
      markerColor = df2$colour,
      iconColor = '#fff'
    )
    
    if (nrow(df2) > 0) {
      proxy %>%
        addAwesomeMarkers(
          data = df2,
          lat = ~ latitude,
          lng = ~ longitude,
          icon = icons,
          popup = ~ pop_up_msg,
          group = ~ route_to
        ) %>%
        fitBounds(
          min(df2$longitude),
          min(df2$latitude),
          max(df2$longitude),
          max(df2$latitude)
        ) %>%
        addLegend(colors = df_legend$colour, labels = df_legend$route_to, position = 'topleft')
    }
    
  }, ignoreNULL = FALSE)
  
  # Observer - add Layer Controls
  observeEvent(input$table_rows_selected, {
    proxy <- leafletProxy('routes_map')
    df = filtered_vehicles()
    routes = df %>%
      dplyr::arrange(route_to) %>%
      dplyr::pull(route_to) %>%
      unique()
    proxy %>%
      addLayersControl(
        overlayGroups = c(routes),
        position = 'topleft',
        options = layersControlOptions(collapsed = FALSE)
      )
    
  }, ignoreNULL = FALSE)
  
  
  ######### MULTIPLE REAL TIME BOARDS
  
  # Observer to update route options UI when selected stops and/or services changes
  output$max_stops <- renderText({
    if (length(input$selected_stops) == 4) {
      return('Max Stops Reached')
    }
  })
  
  bookmarking_values <- reactiveValues()
  active_tab <- reactiveVal(NULL)
  
  observe({
    # Hide all inputs
    toExclude <- names(input)
    setBookmarkExclude(toExclude)
    
    active_tab(input$tabs)
    if (input$tabs == 'multi-boards') {
      bookmarking_values$stops = input$selected_stops
      bookmarking_values$services = input$selected_services
      bookmarking_values$routes = input$chosen_routes
    } else if (input$tabs == 'live-buses') {
      bookmarking_values$routes = input$selected_routes
    }
    
    # Save sidebar settings based on additional options from user
    if ('Refresh Settings' %in% input$saved_sidebar_settings |
        'Refresh Settings' %in% input$saved_sidebar_settings2) {
      bookmarking_values$refresh = input$refresh_interval
    }
    if ('Your Location' %in% input$saved_sidebar_settings |
        'Your Location' %in% input$saved_sidebar_settings2) {
      bookmarking_values$lat = location()$lat
      bookmarking_values$lng = location()$lng
    }
    
  })
  
  onBookmark(function(state) {
    state$values$active = active_tab()
    if (input$tabs == 'multi-boards') {
      state$values$stops = bookmarking_values$stops
      state$values$services = bookmarking_values$services
    } else if (input$tabs == 'live-buses') {
      state$values$routes = bookmarking_values$routes
    }
    
    # Save sidebar settings based on additional options from user
    if ('Refresh Settings' %in% input$saved_sidebar_settings |
        'Refresh Settings' %in% input$saved_sidebar_settings2) {
      state$values$refresh = bookmarking_values$refresh
    }
    if ('Your Location' %in% input$saved_sidebar_settings |
        'Your Location' %in% input$saved_sidebar_settings2) {
      state$values$lat = bookmarking_values$lat
      state$values$lng = bookmarking_values$lng
    }
    
  })
  
  observeEvent(input$multi_boards_bookmark, {
    session$doBookmark()
  })
  
  observeEvent(input$live_buses_bookmark, {
    session$doBookmark()
  })
  
  onBookmarked(showBookmarkUrlModal)
  
  onRestore(function(state) {
    # Perform some defensive programming so people can't mess with your urls
    # First active tabs states - if the value not in the url, don't proceed with the restore steps
    if (state$values$active %in% c('multi-boards', 'live-buses')) {
      # Load sidebar state
      if (!is.null(state$values$refresh)) {
        if (state$values$refresh %in% c(0, 30, 60, 300)) {
          updateSelectInput(
            session,
            inputId = 'refresh_interval',
            label = 'Auto Refresh Interval',
            choices = c(
              'Off' = 0,
              '30 seconds' = 30,
              '1 minute' = 60,
              '5 minutes' = 300
            ),
            selected = state$values$refresh
          )
        }
      }
      
      if (state$values$active == 'multi-boards') {
        updateTabItems(session,
                       inputId = 'tabs',
                       selected = 'multi-boards')
        # Stops loading
        new_stops = dplyr::intersect(state$values$stops, full_stop_names)
        if (length(new_stops) > 0) {
          bookmarking_values$stops = new_stops
          updateSelectizeInput(
            session,
            inputId = 'selected_stops',
            label = 'Select the stops you want to see the boards for:',
            choices = full_stop_names,
            selected = new_stops,
            options = list(
              plugins = list('remove_button'),
              maxItems = 4
            )
          )
        }
        # Services loading
        new_services = dplyr::intersect(state$values$services, c(3, 2))
        if (length(new_services) > 0) {
          updateCheckboxGroupInput(
            session,
            inputId = 'selected_services',
            label = NULL,
            choices = c('Bus' = 3, 'Train' = 2),
            selected = new_services,
            inline = TRUE
          )
        }
      } else if (state$values$active == 'live-buses') {
        updateTabItems(session,
                       inputId = 'tabs',
                       selected = 'live-buses')
        new_routes = dplyr::intersect(state$values$routes,
                                      unique(df_trips_full$route_short_name))
        if (length(new_routes) > 0) {
          updateSelectizeInput(
            session,
            inputId = 'selected_routes',
            label = "Select routes that you're interested in:",
            choices = full_route_names,
            selected = new_routes
          )
        }
      }
      
    }
    
  })
  
  onRestored(function(state) {
    if (!is.null(state$values$lat)) {
      if (is.numeric(c(state$values$lat, state$values$lng))) {
        if (state$values$lat > -47 &
            state$values$lat < -34 &
            state$values$lng > 150  & state$values$lng < 179) {
          new_loc = data.frame(lat = c(state$values$lat),
                               lng = c(state$values$lng))
          location(new_loc)
        }
      }
    }
    
  })
  
  route_list <- reactive({
    df = get_wanted_routes(
      stops = input$selected_stops,
      services = input$selected_services,
      df_routes = df_routes_in_stops
    )
  })
  
  output$select_routes_ui <- renderUI({
    # browser()
    if (length(route_list()) == 0) {
      strong(id = 'no_routes_notif', 'No routes found')
      
    } else {
      checkboxGroupInput(
        inputId = 'chosen_routes',
        label = NULL,
        choices = '',
        inline = TRUE
      )
    }
  })
  
  observe({
    if (length(dplyr::intersect(isolate(input$chosen_routes), route_list())) > 0) {
      update_selected = dplyr::intersect(input$chosen_routes, route_list())
    } else {
      update_selected = route_list()
    }
    updateCheckboxGroupInput(
      session = session,
      inputId = 'chosen_routes',
      label = NULL,
      choices = route_list(),
      inline = TRUE,
      selected = update_selected
    )
    
  })
  
  relevant_trips <- reactive({
    df = df_trips_full %>%
      dplyr::filter(stop_name %in% input$selected_stops) %>%
      dplyr::filter(route_short_name %in% input$chosen_routes)
    return(df)
  })
  
  # Reactive DF, walk times to selected stops depending on user location
  stop_walk_times <- reactive({
    df_selected_stops = df_stops %>%
      dplyr::filter(stop_name %in% input$selected_stops)
    
    user_location = location()
    
    if (nrow(user_location) > 0) {
      df = get_walking_times(df_selected_stops, user_location)
    } else {
      df = df_selected_stops %>%
        dplyr::mutate(time_to_stop = NA)
    }
    
    return(df)
  })
  
  board_trips <- reactive({
    # Generate current day and time
    day_of_week = tolower(weekdays(refresh_time()))
    current_time = round(hms::as_hms(refresh_time()), 0) - hms::as_hms(0)
    
    # Filter trips on day and time
    trips = relevant_trips() %>%
      dplyr::filter(!!as.name(day_of_week) == 1) %>%
      dplyr::filter(arrival_time > current_time) %>%
      dplyr::arrange(arrival_time)
    
    df_walk_times = stop_walk_times() %>%
      dplyr::select(stop_id, time_to_stop)
    
    # Get the board update
    df = get_board_update(api_response(), trips, current_time) %>%
      dplyr::left_join(df_walk_times,
                       by = 'stop_id') %>%
      dplyr::select(
        stop_name,
        route_short_name,
        trip_headsign,
        arrival_time,
        due,
        delay,
        time_to_stop
      ) %>%
      dplyr::rename('Time to stop (mins)' = time_to_stop)
    
    if (nrow(location()) == 0) {
      df = df %>% dplyr::select(-'Time to stop (mins)')
    }
    
    return(df)
  })
  
  output$board_table <-
    DT::renderDataTable(
      DT::datatable(
        board_trips(),
        rownames = FALSE,
        colnames = c(
          'Stop' = 'stop_name',
          'Route' = 'route_short_name',
          'Destination' = 'trip_headsign',
          'Scheduled Time' = 'arrival_time',
          'Due (mins)' = 'due',
          'Delay (mins)' = 'delay'
        ),
        filter = 'top',
        selection = 'none',
        style = 'bootstrap',
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          columnDefs = list(list(
            className = 'dt-center', targets = '_all'
          ))
        )
      )
    )
  
  # Text for board update time stamp, if nothing has been selected displays a help text
  last_update_text <- reactive({
    time = refresh_time()
    hour = lubridate::hour(time)
    min = lubridate::minute(time)
    string = paste0('Last updated: ',
                    ifelse(hour < 10, paste0('0', hour), hour),
                    ':',
                    ifelse(min < 10, paste0('0', min), min))
    return (string)
  })
  
  # Simple text output for update text
  output$board_update_time <- renderText({
    last_update_text()
  })
  
  observeEvent(input$find_stops_btn, {
    updateTabItems(
      session,
      inputId = 'tabs',
      selected = 'stop-locations'
    )
  })
  
  # Stops map
  stops_map_routes <- reactive({
    df = df_routes_in_stops %>%
      dplyr::filter(route_short_name %in% input$routes_for_stops)
    if (nrow(df) > 0) {
      select_colours = data.frame(route_short_name = unique(df$route_short_name),
                                  colour = colours[2:(length(unique(df$route_short_name)) + 1)]) %>%
        dplyr::mutate_if(is.factor, as.character)
      
      df = df %>%
        dplyr::left_join(select_colours, by = 'route_short_name')
    } else {
      df$colours = character(0)
    }
    return(df)
    
  })
  
  output$stops_map <- renderLeaflet({
    if (isolate(nrow(location())) > 0) {
      leaflet() %>%
        addProviderTiles(provider = 'CartoDB.Voyager') %>%
        setView(
          lng = isolate(location()$lng),
          lat = isolate(location()$lat),
          zoom = 15
        )
    } else {
      leaflet() %>%
        addProviderTiles(provider = 'CartoDB.Voyager') %>%
        setView(lng = 174.7646424,
                lat = -36.8486696,
                zoom = 15)
    }
  })
  
  # Move the map to center at the new location
  stops_map_center <- eventReactive(input$stop_location_search_search, {
    if (input$stop_location_search == "") {
      location = data.frame(lat = c(), lng = c())
    } else {
      location = osm_search_nz(input$stop_location_search)
    }
    return (location)
  })
  
  stop_location_validation <- eventReactive(input$stop_location_search_search, {
    validate(need(
      nrow(stops_map_center()) > 0,
      'Location could not be found, try again'
    ))
  })
  
  output$stop_loc_validation <- renderText({
    stop_loc_valid = stop_location_validation()
  })
  
  observeEvent(input$stop_location_search_search, {
    if (nrow(stops_map_center()) > 0) {
      proxy = leafletProxy('stops_map')
      proxy %>% 
        clearPopups() %>% 
        setView(lng = stops_map_center()$lng,
                lat = stops_map_center()$lat,
                zoom = 17) %>% 
        addPopups(lng = stops_map_center()$lng,
                  lat = stops_map_center()$lat,
                  popup = stringr::str_to_title(input$stop_location_search))
    }
  })
  
  # Plot the stops
  observe({
    proxy = leafletProxy('stops_map')
    if (!is.null(input$stops_map_zoom)) {
      map_bounds = input$stops_map_bounds
      proxy %>% 
        clearMarkers() %>% 
        clearControls()
      
      if (input$stops_map_zoom >= 14) {
        df = df_stops %>%
          dplyr::filter(
            stop_lat > min(map_bounds$north, map_bounds$south),
            stop_lat < max(map_bounds$north, map_bounds$south),
            stop_lon > min(map_bounds$east, map_bounds$west),
            stop_lon < max(map_bounds$east, map_bounds$west),
          )
        
        if (nrow(stops_map_routes()) > 0) {
          df_stops_in_routes = df %>%
            dplyr::filter(stop_name %in% stops_map_routes()$stop_name) %>%
            dplyr::left_join(stops_map_routes(), by = 'stop_name')
          
          df_stops_rest = df %>%
            dplyr::filter(!(stop_name %in% stops_map_routes()$stop_name))
          
          df_legend = stops_map_routes() %>%
            dplyr::select(route_short_name, colour) %>%
            dplyr::distinct() %>%
            dplyr::arrange(route_short_name) %>%
            rbind(c('Other', 'blue'))
          
          proxy %>%
            addCircleMarkers(
              data = df_stops_rest,
              lat = ~ stop_lat,
              lng = ~ stop_lon,
              popup = ~ stop_name,
              group = 'Other'
            ) %>%
            addCircleMarkers(
              data = df_stops_in_routes,
              lat = ~ stop_lat,
              lng = ~ stop_lon,
              popup = ~ stop_name,
              color = ~ colour,
              group = ~ route_short_name,
            ) %>%
            addLegend(
              colors = df_legend$colour,
              labels = df_legend$route_short_name,
              position = 'topleft'
            ) %>%
            addLayersControl(
              overlayGroups = (df_legend$route_short_name),
              position = 'topleft',
              options = layersControlOptions(collapsed = FALSE)
            )
        } else {
          proxy %>%
            addCircleMarkers(
              data = df,
              lat = ~ stop_lat,
              lng = ~ stop_lon,
              popup = ~ stop_name
            )
        }
      }
    }
  })
  
  observeEvent(input$help_multi_boards_title, {
    js$collapse('help_multi_boards')
  })
  
  observeEvent(input$help_live_buses_title, {
    js$collapse('help_live_buses')
  })
  
  observeEvent(input$help_stops_title, {
    js$collapse('help_stops')
  })
  
  observeEvent(input$save_boards_title, {
    js$collapse('save_board_view')
  })
  
  observeEvent(input$save_buses_title, {
    js$collapse('save_buses_view')
  })
  
  waiter_hide()
  
}