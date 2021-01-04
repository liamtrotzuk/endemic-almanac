################################################################################
# entry point of the Shiny app
#
# Author: Liam Trotzuk
# Created: 2020-10-10 
################################################################################

#PACKAGES

# import libraries
library(shiny)
library(shinybusy)
library(shinyjs)
library(plotly)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(soilDB)
library(rlang)
library(rvest)
library(leaflet)
library(maps)
library(tigris)
library(sf)
library(htmlwidgets)

#from Stefan's framework
library(future)
library(glue)
library(highcharter)
library(lubridate)
library(purrr)
library(visNetwork)
library(shinyWidgets)

# MODULES
source("modules/module_calendar_pre.R")
source("modules/module_calendar_main.R")
source("modules/module_calendar_error.R")
source("modules/module_map_pre.R")
source("modules/module_map_main.R")
source("modules/module_plots_pre.R")
source("modules/module_plots_main.R")
