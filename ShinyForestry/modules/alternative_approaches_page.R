alt_page_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Page content with alt-page ID
    div(id = "alt-page",
        
        # Fluid row for the maps
        fluidRow(
          column(6, leafletOutput(ns("map2")), class = "no-padding"),  # First Map
          column(6, leafletOutput(ns("map3")), class = "no-padding")   # Second Map
        ),
        fluidRow(
          column(6, leafletOutput(ns("map4")), class = "no-padding"),  # Third Map
          column(6, leafletOutput(ns("map5")), class = "no-padding")   # Fourth Map
        ),
    )
  )
}

alt_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    
    # Render the first map (map2)
    output$map2 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        addResetMapButton() %>%
        setView(lng = -1.733029, lat = 50.820184, zoom = 13) %>%
        syncWith("maps_alt")  # Sync with map2
    })
    
    # Render the second map (map3)
    output$map3 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        setView(lng = -1.733029, lat = 50.820184, zoom = 13) %>%
        addLegend(
          position = "topright",
          colors = adjustcolor(unname(c("red", "green", "blue")), alpha.f = 0.5),
          labels = c("Category 1", "Category 2", "Category 3"),
          title = "Planting Type",
          opacity = 1.0
        ) %>%
        syncWith("maps_alt")  # Sync with map1
    })
    
    # Render the third map (map4)
    output$map4 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        addResetMapButton() %>%
        setView(lng = -1.733029, lat = 50.820184, zoom = 13) %>%
        syncWith("maps_alt")  # Sync with map2
    })
    
    # Render the fourth map (map5)
    output$map5 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        addResetMapButton() %>%        
        setView(lng = -1.733029, lat = 50.820184, zoom = 13) %>%
      syncWith("maps_alt")  # Sync with map2
    })
  })
}
