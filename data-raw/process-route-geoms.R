# Matt Lie
# Script to process bus and train routes GIS data
# Data source from https://data-atgis.opendata.arcgis.com/
# Script last updated: 4 Mar 2020
# Data last updated: 30 Jan 2020

library(sf)
library(dplyr)
library(janitor)

# Import shapefiles
bus_routes <- sf::st_read('./data-raw/at-routes-gis/Bus_Route.shp')
train_routes <- sf::st_read('./data-raw/at-routes-gis/Train_Route.shp')

# Import routes
routes <- readr::read_csv('./data-raw/at-gtfs/routes.txt') %>%
  dplyr::mutate_at(dplyr::vars(route_id), ~ substring(., 0, regexpr('-', .) - 1)) %>%
  dplyr::distinct() %>%
  janitor::remove_empty('cols') %>% 
  dplyr::select(-agency_id)

bus_route_name = as.character(levels(bus_routes$ROUTENAME))
train_route_name = as.character(levels(train_routes$ROUTENAME))

routes_with_geoms = routes %>%
  dplyr::filter(route_long_name %in% c(bus_route_name, train_route_name))

# Deal with trains having multiple geoms
less_trains = train_routes %>%
  dplyr::group_by(ROUTENAME) %>%
  dplyr::summarise(Shape__Len = max(Shape__Len))
bus_routes = bus_routes %>% dplyr::select(ROUTENAME, Shape__Len)
wanted_geoms = rbind(less_trains, bus_routes) %>% dplyr::mutate(ROUTENAME = as.character(ROUTENAME))

# Bind geometries onto the routes dataframe
routes_with_geoms = routes_with_geoms %>%
  dplyr::left_join(wanted_geoms, by = c('route_long_name' = 'ROUTENAME'))

routes_with_geoms$geometry = st_transform(routes_with_geoms$geometry, '+proj=longlat +datum=WGS84')

saveRDS(routes_with_geoms, './data/df_route_geoms.Rds')