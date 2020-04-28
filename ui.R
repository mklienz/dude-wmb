ui <- function(request) {
  dashboardPage(
    skin = 'black',
    title = "Dude, Where's My Bus?",
    
    dashboardHeader(
      title = span(
        tagList(
          tags$img(src = "./dude-wmb.png", 
                   align = "center", 
                   height = 50))),
      titleWidth = 300,
      tags$li(class = 'dropdown',
              div(
                style = 'margin-right: 15px;',
                h3(textOutput('board_update_time'),
                   style = 'margin-top: 12px; margin-bottom: 12px; color: #fff;')
              ))
    ),
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        id = 'tabs',
        tags$li(class = 'header-spacer', br()),
        tags$li(class = 'header', 'TOOLS'),
        menuItem('Multiple Real Time Boards', tabName = 'multi-boards'),
        menuItem('Live Bus Locations', tabName = 'live-buses'),
        menuItem('Find A Stop', tabName = 'stop-locations'),
        tags$li(class = 'header', 'SETTINGS'),
        box(
          id = 'sidebar_refresh',
          title = 'Refresh Settings',
          width = NULL,
          collapsible = TRUE,
          solidHeader = TRUE,
          selectInput(
            inputId = 'refresh_interval',
            label = 'Auto Refresh Interval',
            choices = c(
              'Off' = 0,
              '30 seconds' = 30,
              '1 minute' = 60,
              '5 minutes' = 300
            ),
            selected = 0
          ),
          actionButton(inputId = 'refresh_now',
                       label = 'Refresh Now')
        ),
        box(
          id = 'sidebar_location',
          title = 'Your Location',
          width = NULL,
          collapsible = TRUE,
          solidHeader = TRUE,
          shinyWidgets::searchInput(
            inputId = 'user_location',
            label = 'Find your location (or click the map)',
            btnSearch = shiny::icon('search-location')
          ),
          textOutput('loc_validation'),
          leafletOutput('user_map', width = 264, height = 264) %>% shinycssloaders::withSpinner(type = 6),
          p(
            'Your location is only used to calculate distance and time from selected bus stops and is not stored. Providing your location is optional.'
          )
        )
      )
    ),
    dashboardBody(
      includeCSS('www/waiter.css'),
      includeCSS('www/at-style-clone.css'),
      includeCSS('www/sidebar-and-header.css'),
      includeCSS('www/body.css'),
      useShinyjs(),
      extendShinyjs(script = './www/utils.js', functions = c('collapse')),
      use_waiter(include_js = FALSE, spinners = c(1, 4)),
      tags$head(tags$link(rel = "shortcut icon", href = "dude-wmb.ico")),
      
      tabItems(
        tabItem('multi-boards',
                fluidRow(
                  box(
                    id = 'help_multi_boards',
                    width = 12,
                    title = actionLink(
                      "help_multi_boards_title",
                      "How does this work?",
                      icon = icon("question-circle")
                    ),
                    status = 'info',
                    collapsible = TRUE,
                    collapsed = TRUE,
                    p(
                      'The Multiple Real Time Boards view allows you to see when buses and/or trains of certain routes are due to arrive at selected stops.'
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    box(
                      id = 'box_stops',
                      width = NULL,
                      status = 'primary',
                      title = 'Stop Selection',
                      selectizeInput(
                        inputId = 'selected_stops',
                        label = 'Select the stops you want to see the boards for:',
                        choices = full_stop_names,
                        multiple = TRUE,
                        selected = 'Britomart Train Station',
                        options = list(plugins = list('remove_button'), maxItems = 4)
                      ),
                      textOutput('max_stops'),
                      actionButton(
                        inputId = 'find_stops_btn',
                        label = 'Help me find stops',
                        width = '100%'
                      )
                    ),
                    box(
                      id = 'box_services',
                      width = NULL,
                      status = 'primary',
                      title = 'Service Selection',
                      checkboxGroupInput(
                        inputId = 'selected_services',
                        label = NULL,
                        choices = c('Bus' = 3, 'Train' = 2),
                        selected = c(3, 2),
                        inline = TRUE
                      )
                    ),
                    box(
                      id = 'box_routes',
                      width = NULL,
                      status = 'primary',
                      title = 'Route Selection',
                      div(class = 'get-routes-ui', 'Select the routes you want to see information for:'),
                      uiOutput('select_routes_ui'),
                      actionButton(inputId = 'send_routes',
                                   label = 'Show me where these vehicles are')
                    ),
                    box(
                      id = 'save_board_view',
                      width = NULL,
                      status = 'primary',
                      collapsible = TRUE,
                      collapsed = TRUE,
                      title = actionLink(
                        "save_boards_title",
                        "Save This View",
                        icon = icon('save'),
                        style = 'color: #283d51;'
                      ),
                      p(
                        "Use this button to save this view with your selected stops and routes:"
                      ),
                      bookmarkButton(id = 'multi_boards_bookmark'),
                      # bookmarkButton(),
                      checkboxGroupInput(
                        inputId = 'saved_sidebar_settings',
                        label = 'Also save:',
                        choices = c('Refresh Settings', 'Your Location'),
                        inline = TRUE
                      ),
                      p(id = 'multi-boards-save-note', 'Route saving functionality is coming soon!')
                    )
                  ),
                  column(
                    width = 9,
                    box(
                      id = 'box_boards',
                      width = NULL,
                      status = 'primary',
                      title = 'Real Time Boards',
                      DT::dataTableOutput('board_table') %>% shinycssloaders::withSpinner(type = 6)
                    )
                  )
                )),
        tabItem('live-buses',
                fluidRow(
                  box(
                    id = 'help_live_buses',
                    width = 12,
                    title = actionLink(
                      "help_live_buses_title",
                      "How does this work?",
                      icon = icon("question-circle")
                    ),
                    status = 'info',
                    collapsible = TRUE,
                    collapsed = TRUE,
                    p(
                      'The Live Bus Locations view allows you to see where the buses and trains on selected routes are on their journey.'
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    box(
                      id = 'box_routes_2',
                      width = NULL,
                      status = 'primary',
                      title = 'Route Selection',
                      selectizeInput(
                        inputId = 'selected_routes',
                        label = "Select routes that you're interested in:",
                        choices = full_route_names,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(plugins = list('remove_button'))
                      )
                    ),
                    box(
                      id = 'box_active_vehicles',
                      width = NULL,
                      status = 'primary',
                      title = 'Active Vehicles',
                      DT::DTOutput('table')
                    ),
                    box(
                      id = 'save_buses_view',
                      width = NULL,
                      status = 'primary',
                      collapsible = TRUE,
                      collapsed = TRUE,
                      title = actionLink(
                        "save_buses_title",
                        "Save This View",
                        icon = icon('save'),
                        style = 'color: #283d51;'
                      ),
                      p("Use this button to save this view with your selected routes:"),
                      bookmarkButton(id = 'live_buses_bookmark'),
                      checkboxGroupInput(
                        inputId = 'saved_sidebar_settings2',
                        label = 'Also save:',
                        choices = c('Refresh Settings', 'Your Location'),
                        inline = TRUE
                      )
                    )
                  ),
                  column(
                    width = 9,
                    box(
                      id = 'box_routes_map',
                      width = NULL,
                      status = 'primary',
                      title = 'Vehicle Locations',
                      leafletOutput('routes_map', height = '700px')
                    )
                  )
                )),
        tabItem(tabName = 'stop-locations',
                fluidRow(
                  box(
                    id = 'help_stops',
                    width = 12,
                    title = actionLink(
                      "help_stops_title",
                      "How does this work?",
                      icon = icon("question-circle")
                    ),
                    status = 'info',
                    collapsible = TRUE,
                    collapsed = TRUE,
                    p(
                      'Use the map to help you find stops you are interested in; or optionally highlight stops that are visited by selected routes.'
                    )
                  )
                ),
                fluidRow(
                  box(
                    id = 'stops_map_box',
                    width = 12,
                    title = 'Find A Stop',
                    status = 'primary',
                    leafletOutput('stops_map', height = 700),
                    absolutePanel(
                      id = 'route_select',
                      class = 'panel',
                      fixed = FALSE,
                      draggable = FALSE,
                      top = 70,
                      left = 'auto',
                      right = 20,
                      bottom = 'auto',
                      width = '20%',
                      height = 'auto',
                      div(
                        style = 'padding: 10px',
                        h3('Route Selection', style = 'font-size: 18px; font-weight: 600; margin-top: 0px;'),
                        p('Stops serviced by selected routes will be displayed in a different colour to the rest of the stops'),
                        selectizeInput(
                          inputId = 'routes_for_stops',
                          label = NULL,
                          choices = full_route_names,
                          multiple = TRUE,
                          selected = NULL,
                          options = list(plugins = list('remove_button'), maxItems = 18)
                        ),
                        br(),
                        h3('Recenter Map', style = 'font-size: 18px; font-weight: 600; margin-top: 0px;'),
                        shinyWidgets::searchInput(
                          inputId = 'stop_location_search',
                          label = 'Enter a location to centre the map around',
                          btnSearch = shiny::icon('search-location')
                        ),
                        textOutput('stop_loc_validation')
                      )
                    )
                  )
                ))
      ),
      waiter_show_on_load(html = tagList(spin_solar(), h4('Loading...')), color = '#fff')
    )
  )
}