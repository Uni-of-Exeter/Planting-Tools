library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

map_page_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "display: flex; height: 100vh; flex-direction: row; width: 100%;",
      sidebarPanel(
        id = ns("sidebar"),
        width = 3,
        
        HTML('
          <div class="alert alert-dismissible alert-info">
            <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
            <div class="d-flex align-items-center">
              <i class="bi bi-info-circle" style="font-size: 1.5rem; margin-right: 10px;"></i>
              <p class="mb-0">Use the checkboxes and sliders to enable/disable targets and adjust their values.</p>
            </div>
          </div>
        '),
        
        accordion(
          id = ns("main_accordion"),
          accordion_panel(
            "Targets",
            id = ns("targets_accordion"),
            sliderInput(ns("zoom_slider"), "Zoom Level", min = 1, max = 18, value = 10),
            sliderInput(ns("lat_slider"), "Latitude", min = -90, max = 90, value = 40.7128, step = 0.1),
            sliderInput(ns("lng_slider"), "Longitude", min = -180, max = 180, value = -74.006, step = 0.1),
            actionButton(ns("submit"), "Submit"),
            actionButton(ns("reset"), "Reset"),
            actionButton(ns("save"), "Save Strategy")
          ),
          accordion_panel(
            "Saved Strategies",
            uiOutput(ns("saved_strategies"))
          )
        )
      ),
      
      mainPanel(
        class = "main-panel",
        leafletOutput(ns("map"), height = "100%")
      )
    )
  )
}

map_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    
    Sys.sleep(3)
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(doubleClickZoom = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        setView(lng = state$lng, lat = state$lat, zoom = state$zoom)
    })
    # JS to detect when the map is rendered; fed back to main app
    observe({
      shinyjs::runjs('
        var mapCheckInterval = setInterval(function() {
          if (document.querySelector(".leaflet-container")) {
            clearInterval(mapCheckInterval);  // Stop checking once the map is rendered
            Shiny.setInputValue("mappageRendered", true, {priority: "event"});  // Set the input value
          }
        }, 100);
      ')
    })
  })
}

