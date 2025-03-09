library(shiny)
library(leaflet)
library(bslib)  
library(shinyjs) 
library(shinycssloaders)
library(uuid)
library(plumber)
library(geojsonsf)
library(jsonlite)
library(sf)
library(ggplot2)
library(dplyr)
library(httr)
library(units)
library(shinyWidgets)
library(plotly)
library(leaflet.extras)
library(glue)

# Source module files
source("modules/map_page.R")
source("modules/preferences_page.R")
source("modules/alternative_approaches_page.R")
source("modules/exploration_page.R")
source("modules/downscaling_page.R")

custom_theme <- bs_theme(
  bootswatch = "lumen",
  "navbar-bg" = "#003c3c"
)

# Define UI
ui <- page_navbar(
  title = "ADD-TREES",
  useShinyjs(), 
  theme = custom_theme,
  
  # Link the external CSS file
  tags$head(
    tags$link(
      rel = "stylesheet", 
      href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600&display=swap"
    ),
    tags$link(
      rel = "stylesheet", 
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css"
    ),
    tags$link(
      rel = "stylesheet", 
      type = "text/css",
      href = "custom.css"
    )
  ),
  
  tabPanel(title = "Map", map_page_ui("map")),
  tabPanel(title = "Preferences", preferences_page_ui("prefs")),
  tabPanel(title = "Alternative Approaches", alternative_approaches_page_ui("alt") ),
  tabPanel(title = "Exploration", exploration_page_ui("explore")),
  tabPanel(title = "Downscaling", downscaling_page_ui("downscale")),
  
  nav_spacer(),
  nav_item(tags$a("AI for Net Zero",
                  href = "https://netzeroplus.ac.uk/",
                  target = "_blank")
  )
)

server <- function(input, output, session) {
  
  # Structured state for each page
  state <- reactiveValues(
    map = list(zoom = 13, lat = 50.820184, lng = -1.733029),
    prefs = list(),
    alt = list(),
    explore = list(),
    downscale = list()
  )
  
  # Pass only the required state to each page
  map_page_server("map", state$map)
  preferences_page_server("prefs", state$prefs)
  alternative_approaches_page_server("alt", state$alt)
  exploration_page_server("explore", state$explore)
  downscaling_page_server("downscale", state$downscale)
}

shinyApp(ui, server)
