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
source("config.R")  # Load config 

source("modules/map_page.R")
source("modules/preferences_page.R")
source("modules/alternative_approaches_page.R")
source("modules/exploration_page.R")
source("modules/downscaling_page.R")

source("utils/api_functions.R")

custom_theme <- bs_theme(
  bootswatch = "lumen",
  "navbar-bg" = "#003c3c"
)

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
        tabPanel(title = "Alternative Approaches", alt_page_ui("alt")),
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
    map_tab = list(
      initialized = FALSE,
      slider_defaults = list(NULL)
    ),
    pref_tab = list(
      initialized = FALSE
    ),
    initialized = FALSE
  )
    
  map_page_server("map", state)
  preferences_page_server("prefs", state)
  alt_page_server("alt", state)
  
  # Initialization that is required for the `loadingCompleted` state to be False
  observe({
    if (!is.null(input$mappageRendered) && input$mappageRendered 
      && !is.null(input$prefpageRendered) && input$prefpageRendered) {
    # req(!state$initialized)
    # if (state$map_tab$initialized && state$pref_tab$initialized) {
      
      # Hide the loading screen after both maps are rendered
      Sys.sleep(0.5)  # Small delay for smoother transition
      
      # Fade out the loading screen (can now be triggered after clicking back to map)
      shinyjs::runjs('$("#loading").fadeOut(1000);')  # Fade out the loading screen
      
      # Mark the loading as completed in the state
      state$initialized <- TRUE
    }
  })
  
}


shinyApp(ui, server)
