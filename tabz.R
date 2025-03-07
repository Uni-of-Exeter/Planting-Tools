ui <- fluidPage(
  title = "ADD-TREES",
  theme = shinythemes::shinytheme("lumen"),
  
  tags$style(
    HTML(
      "
      .no-padding { 
        padding: 0px; 
      } 
      .leaflet-container { 
        height: 50vh !important; 
        width: 100% !important; 
      }
      
      .leaflet-control {
        background: rgba(255, 255, 255, 0.9); /* background-color to zero */
        box-shadow: none !important; /* remove box-shadow */
        border: none !important; /* remove border */
      }
      ")),
  
  fluidRow(
    column(6, leafletOutput("map1"), class="no-padding"),
    column(6, leafletOutput("map2"), class="no-padding")
  ),
  fluidRow(
    column(6, leafletOutput("map3"), class="no-padding"),
    column(6, leafletOutput("map4"), class="no-padding")
  ),
  
  # Absolute panel for the slider and controls
  absolutePanel(
    id = "year-slider",
    sliderInput("year", "Planting Year", 
                min = YEAR_MIN, 
                max = YEAR_MAX, 
                value = YEAR_DEFAULT, 
                step = 1, 
                animate = TRUE,
                ticks = TRUE,  
                animateOptions(interval = 100, loop = FALSE),
                sep = "",
                width = "100%"  # This will make the slider take 100% width of the container
    ),
    bottom = "50px", 
    left = "50%", 
    style = "background-color: rgba(255, 255, 255, 0.9); padding: 10px 20px 10px 20px; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.2); width: 33%; transform: translateX(-50%);",
    
    # Right-aligned toggle for Annual/Cumulative
    div(
      style = "display: flex; justify-content: right; align-items: center; width: 100%; height: 40px;",
      radioGroupButtons(
        inputId = "view_toggle",
        choices = c("Annual", "Cumulative"),
        selected = "Cumulative",
        status = "primary",
        justified = TRUE,
        width = '220px'
      )
    ),
  ),
)

server <- function(input, output) {
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
      addResetMapButton() %>%
      syncWith("maps")
  })
  
  output$map2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
    addLegend(
      position = "topright",
      colors = adjustcolor(unname(COLOUR_MAPPING), alpha.f = FILL_OPACITY),
      labels = names(COLOUR_MAPPING),
      title = "Planting Type",
      opacity = 1.0
    ) %>% syncWith("maps")
  })
  
  output$map3 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
      syncWith("maps")
  })
  
  output$map4 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
      syncWith("maps")
  })
}

shinyApp(ui, server)
