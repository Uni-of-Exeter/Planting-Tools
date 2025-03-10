library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

source("utils/api_functions.R")
source("config.R")

alt_page_ui <- function(id) {
  ns <- NS(id)
  div(id = "alt-page",
    useShinyjs(),
    tagList(
      fluidRow(
        column(6, leafletOutput(ns("map1")), class = "no-padding"),  # First Map
        column(6, leafletOutput(ns("map2")), class = "no-padding")   # Second Map
      ),
      fluidRow(
        column(6, leafletOutput(ns("map3")), class = "no-padding"),  # Third Map
        column(6, leafletOutput(ns("map4")), class = "no-padding")   # Fourth Map
      )
    )
  )
}

alt_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for the module
    
    output$map1 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
        syncWith("maps")
    })
    
    output$map2 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
        syncWith("maps")
    })
    
    output$map3 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
        syncWith("maps") 
    })
    
    output$map4 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
        syncWith("maps")
    })
  })
}
