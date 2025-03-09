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

library(raster)
library(leaflet.minicharts)
library(manipulateWidget)


# Source module files
source("global.R")  # Load global settings
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
# Define UI
ui <- fluidPage(
  
  useShinyjs(),  # Initialize shinyjs
  
  # Full-page loading screen (visible on app startup)
  div(id = "loading", 
      div(class = "spinner")
  ),
  
  # Wrap the entire content in a div with an ID
  div(id = "app_content",  
      # The main content of the app (page_navbar)
      page_navbar(
        title = "ADD-TREES",
        theme = custom_theme,
        
        # Link the external CSS files
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
        nav_spacer(),
        nav_item(tags$a("AI for Net Zero",
                        href = "https://netzeroplus.ac.uk/",
                        target = "_blank")
        )
      )
  )
)

server <- function(input, output, session) {
  

  state <- reactiveValues(
    map = list(
      zoom = 13, 
      lat = 50.820184, 
      lng = -1.733029
    ),
    setup = list(
      setupComplete = FALSE
    )
  )

  map_page_server("map", state$map)
  preferences_page_server("prefs", state$map)
  
  # Listen for the signal that map has been rendered
  observeEvent(input$mapRendered, {
    Sys.sleep(0.5)
    shinyjs::runjs('$("#loading").fadeOut(2000);')
    # Once the map is rendered, hide the loading screen and show the app content
  })
}


shinyApp(ui, server)
