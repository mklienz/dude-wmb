# Script to install and load required packages
packages = c(
  'httr',
  'jsonlite',
  'dplyr',
  'tidyr',
  'shiny',
  'shinydashboard',
  'shinyWidgets',
  'shinycssloaders',
  'shinyjs',
  'leaflet',
  'DT',
  'sf',
  'janitor',
  'stringr'
)

devtools::install_github("JohnCoene/waiter", upgrade = 'never')

packages.check <- lapply(
  packages, 
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

