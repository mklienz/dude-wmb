library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(waiter)
library(leaflet)
library(DT)
library(sf)
library(dplyr)
library(janitor)
library(stringr)

# Source scripts
source('./R/get_active_vehicles2.R')
source('./R/append_active_vehicles.R')
source('./R/utils.R')
source('./R/get_board_update2.R')
source('./R/get_walking_times.R')
source('./R/osm_search_nz.R')
source('./R/call_at_feed.R')
source('./R/at_api_sub_check.R')

df_stops <- readRDS('./data/df_stops.Rds')
df_routes_in_stops <- readRDS('./data/df_routes_in_stops.Rds')
df_trips_full <- readRDS('./data/df_trips_full.Rds')
full_stop_names <- readRDS('./data/full_stop_names.Rds')
df_last_stop_in_route = df_trips_full %>%
  dplyr::group_by(route_id) %>%
  dplyr::summarise(last_stop_seq = max(stop_sequence))

df_route_geoms <-
  readRDS("./data/df_route_geoms.Rds")

route_destinations <- df_trips_full %>%
  dplyr::select(route_id, trip_headsign, route_type) %>%
  dplyr::distinct() %>%
  dplyr::left_join(as.data.frame(df_route_geoms %>% dplyr::select(route_id, route_short_name)), by = 'route_id')

full_route_names <- df_route_geoms %>%
  dplyr::arrange(route_short_name) %>%
  dplyr::pull(route_short_name) %>%
  unique()

colours = as.character(
  c(
    "blue",
    "red",
    "green",
    "orange",
    "purple",
    "pink",
    "black",
    "gray",
    "beige",
    "darkred",
    "darkblue",
    "darkgreen",
    "darkpurple",
    "lightred",
    "lightgreen",
    "lightblue",
    "lightgray",
    "cadetblue",
    "white"
  )
)

enableBookmarking(store = "url")