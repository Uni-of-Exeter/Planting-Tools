library(shiny)
library(raster)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(manipulateWidget)
library(shinythemes)
library(sf)
library(shinyWidgets)
library(dplyr)

setwd("/Users/paulwright/Documents/work/ADD-TREES/Planting-Tools/")

source("ShinyForestry/config.R")

FullTableNotAvail <- st_read(normalizePath(file.path(normalizePath(file.path(normalizePath(getwd()), "ShinyForestry/ElicitorOutput")), "FullTableNotAvail.geojson")), quiet=TRUE)

FullTableNotAvailONE <- FullTableNotAvail %>%
  mutate(id = paste0("ONE_", row_number()))
FullTableNotAvailTWO <- FullTableNotAvail %>%
  mutate(id = paste0("TWO_", row_number()))

ui <- fluidPage(
  title = "ADD-TREES",
  theme = shinythemes::shinytheme("lumen"),
  useShinyjs(), # Enable JavaScript functionalities
  # Custom styles for full-width maps and other elements
  tags$style(HTML("
    .no-padding {
      padding: 0px;  /* Remove padding inside the columns */
    }
    .leaflet-container {
      height: 100vh !important;
      width: 100% !important;
    }
    .leaflet-control {
      background: rgba(255, 255, 255, 0.9); /* background-color to zero */
      box-shadow: none !important; /* remove box-shadow */
      border: none !important; /* remove border */
    }
    /* Sidebar content adjustment */
    .sidebarPanel {
      height: 100%;
      overflow-y: auto;
    }
    /* Styling for the value boxes */
    .value-box {
      background-color: rgba(255, 255, 255, 0.9);
      padding: 10px;
      border-radius: 8px;
      border: 1px solid #ccc;
      box-shadow: 0px 4px 6px rgba(0,0,0,0.2);
      width: auto; /* Allow dynamic width */
      max-width: 300px; /* Prevent it from getting too wide */
      min-width: 150px; /* Prevent it from getting too small */
      height: auto;
      position: absolute;
      white-space: nowrap; /* Keeps text from wrapping */
    }
    
    /* Positioning for each value box */
    .value-box.map2 {
      top: 20px;
      left: 75%;
      transform: translateX(-50%);
    }
    
    .value-box.map1 {
      top: 20px;
      left: 25%;
      transform: translateX(-50%);
    }
    
    /* Improve table appearance */
    .value-box table {
      border-collapse: collapse;
      width: auto; /* Ensures table takes only necessary space */
    }

  ")),
  
  # Fluid row for the maps
  fluidRow(
    column(6, leafletOutput("map1"), class="no-padding"),  # First Map
    column(6, leafletOutput("map2"), class="no-padding")   # Second Map
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
  
  # Value boxes for map1 and map2
  div(id = "value-box-1", class = "value-box map1", 
      uiOutput("value_box_one"),
      actionButton("submit_one", "Choose this strategy", class = "btn btn-secondary")  # Start as grey
  ),
  
  div(id = "value-box-2", class = "value-box map2", 
      uiOutput("value_box_two"), 
      actionButton("submit_two", "Choose this strategy", class = "btn btn-secondary")  # Start as grey
  )
)


fetch_api_data <- function() {
  
  url <- paste0(API_URL, "/random_strategy")
  # Make the API request
  response <- httr::GET(url)
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    geojson <- api_response$geojson
    geojson_parsed <- st_read(geojson, quiet=TRUE)
    
    values <- api_response$values
    return(list(geojson_parsed, values))
  } 
  else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

server <- function(input, output) {
  
  # Render the first map (map1) with synchronization
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
      addResetMapButton() %>%
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
      syncWith("maps")  # Sync the map with map2
  })
  
  # Render the second map (map2) with synchronization
  output$map2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>%
      # Add legend
      addLegend(
        position = "topright",
        colors = adjustcolor(unname(COLOUR_MAPPING), alpha.f = FILL_OPACITY),
        labels = names(COLOUR_MAPPING),
        title = "Planting Type",
        opacity = 1.0
      ) %>%
      syncWith("maps")  # Sync the map with map1
  })
  
  new_data_one <- reactiveVal(NULL)
  new_vals_one <- reactiveVal(NULL)
  filtered_data_one <- reactiveVal(NULL)
  current_layers_one <- reactiveVal(list())
  
  new_data_two <- reactiveVal(NULL)
  new_vals_two <- reactiveVal(NULL)
  filtered_data_two <- reactiveVal(NULL)
  current_layers_two <- reactiveVal(list())
  
  initialize_or_update_map <- function(input_year) {
    # Fetch the data from the API when initializing or submitting
    # new_data_fetched <- st_read(fetch_api_data())  # Hit the API and get the data
    
    new_fetched_one <- fetch_api_data()  # Use GET otherwise
    new_fetched_two <- fetch_api_data()  # Use GET otherwise
    
    new_data_fetched_one <- new_fetched_one[[1]]
    new_data_fetched_one$parcel_id <- paste0(new_data_fetched_one$parcel_id, "_one")
    new_values_fetched_one <- new_fetched_one[[2]]
    
    new_data_fetched_two <- new_fetched_two[[1]]
    new_data_fetched_two$parcel_id <- paste0(new_data_fetched_two$parcel_id, "_two")
    new_values_fetched_two <- new_fetched_two[[2]]
    
    print("--")
    print(new_data_fetched_one)
    print(new_data_fetched_two)
    print("-/-")
    
    print("x")
    
    if (!is.null(new_data_fetched_one)) {
      # Apply the filter based on the selected year
      new_data_one(new_data_fetched_one)
      new_vals_one(new_values_fetched_one)
      
      print(new_data_fetched_one)
      
      filtered_data_subset_one <- new_data_fetched_one[new_data_fetched_one$planting_year <= input_year, ]
      filtered_data_one(filtered_data_subset_one)
      
      current_layers_one(filtered_data_subset_one$parcel_id)
      
      print("-- ONE --")
      print(new_data_fetched_one)
      print("-/- ONE -/-")
      
      # Render the leaflet map with the updated data
      
      leafletProxy("map1") %>%
        addPolygons(
          data = new_data_fetched_one,  # Use the full dataset as the base layer
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = AVAILABLE_PARCEL_COLOUR,
          fillOpacity = FILL_OPACITY,
          group = "parcelPolygons",  # Group the polygons
          layerId = ~parcel_id,  # Set unique IDs for each polygon
          label = ~parcel_id,
          # popup = "No planting"
        ) %>%
        
        # Add filtered polygons based on the selected year
        addPolygons(
          data = filtered_data_subset_one,  # Filtered data based on the year
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = ~unname(COLOUR_MAPPING[planting_type]),  # Use the planting type color
          fillOpacity = FILL_OPACITY,
          layerId = ~parcel_id,
          label = ~parcel_id,
          # popup = ~planting_type
        ) %>%
        
        # Add unavailable parcels layer
        addPolygons(
          data = FullTableNotAvailONE,  # Unavailable parcels
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = UNAVAILABLE_PARCEL_COLOUR,
          fillOpacity = FILL_OPACITY,
          group = "unavailablePolygons",
          layerId = ~id,
          # popup = "Unavailable for planting"
        ) %>%
        
        addPolygons(
          data = filtered_data_subset_one,  # Filtered data based on the year
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = ~unname(COLOUR_MAPPING[planting_type]),  # Use the planting type color
          fillOpacity = FILL_OPACITY,
          layerId = ~parcel_id,
          label = ~parcel_id,
          # popup = ~planting_type
        )
      
      
    } else {
      print("API fetch failed, no data to update.")
    }
    
    if (!is.null(new_data_fetched_two)) {
      # Apply the filter based on the selected year
      new_data_two(new_data_fetched_two)
      new_vals_two(new_values_fetched_two)
      
      print(new_data_fetched_two)
      
      filtered_data_subset_two <- new_data_fetched_two[new_data_fetched_two$planting_year <= input_year, ]
      filtered_data_two(filtered_data_subset_two)
      
      print("-- TWO --")
      print(new_data_fetched_two)
      print("-/- TWO -/-")
      
      leafletProxy("map2") %>%
        addPolygons(
          data = new_data_fetched_two,  # Use the full dataset as the base layer
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = AVAILABLE_PARCEL_COLOUR,
          fillOpacity = FILL_OPACITY,
          group = "parcelPolygons2",  # Group the polygons
          layerId = ~parcel_id,  # Set unique IDs for each polygon
          label = ~parcel_id,
          # popup = "No planting"
        ) %>%
        
        # Add filtered polygons based on the selected year
        addPolygons(
          data = filtered_data_subset_two,  # Filtered data based on the year
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = ~unname(COLOUR_MAPPING[planting_type]),  # Use the planting type color
          fillOpacity = FILL_OPACITY,
          layerId = ~parcel_id,
          label = ~parcel_id,
          # popup = ~planting_type
        ) %>%
        # Add unavailable parcels layer
        addPolygons(
          data = FullTableNotAvailTWO,  # Unavailable parcels
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = UNAVAILABLE_PARCEL_COLOUR,
          fillOpacity = FILL_OPACITY,
          group = "unavailablePolygonsTwo",
          layerId = ~id,
          # popup = "Unavailable for planting"
        ) %>%
        # 
        addPolygons(
          data = filtered_data_subset_two,  # Filtered data based on the year
          weight = 1,
          color = PARCEL_LINE_COLOUR,
          fillColor = ~unname(COLOUR_MAPPING[planting_type]),  # Use the planting type color
          fillOpacity = FILL_OPACITY,
          layerId = ~parcel_id,
          label = ~parcel_id,
          # popup = ~planting_type
        )
      
    } else {
      print("API fetch failed, no data to update.")
    }
  }
  
  initialize_or_update_map(YEAR_MIN)
  
  observe({
    input_year <- input$year
    selected_view <- input$view_toggle
    
    if (!is.null(new_data_one())) {
      # Access the most recently loaded data stored in the reactive `new_data`
      current_data_one <- new_data_one()
      
      # # Filter the data based on the selected year and update `filtered_data`
      # filtered_data_subset <- current_data[current_data$planting_year <= input_year, ]
      # filtered_data(filtered_data_subset)  # Update the reactive filtered data
      
      # **Modify Filtering Based on Selected View**
      filtered_data_subset_one <- if (selected_view == "Cumulative") {
        current_data_one[current_data_one$planting_year <= input_year, ]  # Up to the selected year
      } else {
        current_data_one[current_data_one$planting_year == input_year, ]  # Only the selected year
      }
      filtered_data_one(filtered_data_subset_one)  # Update the reactive filtered data
      
      # Get the current IDs of the filtered polygons
      current_ids_one <- unique(filtered_data_subset_one$parcel_id)
      
      # Get the current state of the layers (those that are already on the map)
      existing_layers_one <- current_layers_one()  # Use the reactive current_layers()
      
      # Find the IDs that need to be added and removed
      to_add_one <- setdiff(current_ids_one, existing_layers_one)
      to_remove_one <- setdiff(existing_layers_one, current_ids_one)  # Ensure this is a vector of IDs
      
      # try update polygons back with a new style
      if (length(to_remove_one) > 0) {
        # Get the data for all parcels that need to be recoloured from FullTable
        updated_data_one <- current_data_one[current_data_one$parcel_id %in% to_remove_one, ]  # Use `to_remove` to filter
        
        # Update the recolour of polygons in one go
        leafletProxy("map1") %>%
          addPolygons(
            data = updated_data_one,  # Use the updated data for the parcels in `to_remove`
            weight = 1,
            color = PARCEL_LINE_COLOUR,
            fillColor = AVAILABLE_PARCEL_COLOUR,
            fillOpacity = FILL_OPACITY,
            layerId = updated_data_one$parcel_id,  # Ensure to reassign the same layerId for all
            # label = updated_data$parcel_id
          )
      }
      
      # Add new polygons (those that are in filtered data but not on the map)
      if (length(to_add_one) > 0) {
        leafletProxy("map1") %>%
          addPolygons(
            data = current_data_one[current_data_one$parcel_id %in% to_add_one, ],  # Filtered data for new polygons
            weight = 1,
            color = PARCEL_LINE_COLOUR,
            fillColor = ~unname(COLOUR_MAPPING[planting_type]),  # Colour for filtered polygons
            fillOpacity = FILL_OPACITY,
            group = "filteredPolygons",  # Group for filtered polygons
            layerId = ~parcel_id,  # Use parcel_id as layerId to add new polygons
            label = ~parcel_id,
            # popup = ~planting_type
          )
      }
      
      current_layers_one(current_ids_one)
      
    }
  })
  
  observe({
    input_year <- input$year
    selected_view <- input$view_toggle
    
    if (!is.null(new_data_two())) {
      # Access the most recently loaded data stored in the reactive `new_data`
      current_data_two <- new_data_two()
      
      # # Filter the data based on the selected year and update `filtered_data`
      # filtered_data_subset <- current_data[current_data$planting_year <= input_year, ]
      # filtered_data(filtered_data_subset)  # Update the reactive filtered data
      
      filtered_data_subset_two <- if (selected_view == "Cumulative") {
        current_data_two[current_data_two$planting_year <= input_year, ]  # Up to the selected year
      } else {
        current_data_two[current_data_two$planting_year == input_year, ]  # Only the selected year
      }
      filtered_data_two(filtered_data_subset_two)  # Update the reactive filtered data
      
      # Get the current IDs of the filtered polygons
      current_ids_two <- unique(filtered_data_subset_two$parcel_id)
      
      # Get the current state of the layers (those that are already on the map)
      existing_layers_two <- current_layers_two()  # Use the reactive current_layers()
      
      to_add_two <- setdiff(current_ids_two, existing_layers_two)
      to_remove_two <- setdiff(existing_layers_two, current_ids_two)  # Ensure this is a vector of IDs
      
      if (length(to_remove_two) > 0) {
        # Get the data for all parcels that need to be recoloured from FullTable
        updated_data_two <- current_data_two[current_data_two$parcel_id %in% to_remove_two, ]  # Use `to_remove` to filter
        
        # Update the recolour of polygons in one go
        leafletProxy("map2") %>%
          addPolygons(
            data = updated_data_two,  # Use the updated data for the parcels in `to_remove`
            weight = 1,
            color = PARCEL_LINE_COLOUR,
            fillColor = AVAILABLE_PARCEL_COLOUR,
            fillOpacity = FILL_OPACITY,
            layerId = updated_data_two$parcel_id,  # Ensure to reassign the same layerId for all
            # label = updated_data$parcel_id
          )
      }
      
      # Add new polygons (those that are in filtered data but not on the map)
      if (length(to_add_two) > 0) {
        leafletProxy("map2") %>%
          addPolygons(
            data = current_data_two[current_data_two$parcel_id %in% to_add_two, ],  # Filtered data for new polygons
            weight = 1,
            color = PARCEL_LINE_COLOUR,
            fillColor = ~unname(COLOUR_MAPPING[planting_type]),  # Colour for filtered polygons
            fillOpacity = FILL_OPACITY,
            group = "filteredPolygons",  # Group for filtered polygons
            layerId = ~parcel_id,  # Use parcel_id as layerId to add new polygons
            label = ~parcel_id,
            # popup = ~planting_type
          )
      }
      current_layers_two(current_ids_two)
      
      # Need to do something similar for blocked
    }
  })
  
  output$value_box_two <- renderUI({
    current_value <- new_vals_two()
    
    # Generate table rows dynamically
    table_rows <- paste0(
      lapply(names(current_value), function(name) {
        display_name <- SLIDER_NAMES[[name]]$name
        unit <- SLIDER_NAMES[[name]]$unit
        value <- round(current_value[[name]], POPUP_SIGFIG)
        
        # Format each row with labels aligned left and values aligned right
        sprintf("<tr><td style='padding-right: 10px;'><b>%s:</b></td> 
              <td style='text-align:left;'>%s %s</td></tr>", 
                display_name, value, unit)
      }),
      collapse = "\n"
    )
    
    # Construct the legend-like content with tabbed format
    legend_html <- paste0(
      "<table style='width:100%;'>",  # Ensuring the table takes full width
      table_rows,  # Add dynamically generated rows
      "</table><br>"
    )
    
    HTML(legend_html)  # Return HTML to be rendered
  })
  
  output$value_box_one <- renderUI({
    # Get the current value of the reactive variable
    current_value <- new_vals_one()
    
    # Generate table rows dynamically
    table_rows <- paste0(
      lapply(names(current_value), function(name) {
        display_name <- SLIDER_NAMES[[name]]$name
        unit <- SLIDER_NAMES[[name]]$unit
        value <- round(current_value[[name]], POPUP_SIGFIG)
        
        # Format each row with labels aligned left and values aligned right
        sprintf("<tr><td style='padding-right: 10px;'><b>%s:</b></td> 
              <td style='text-align:left;'>%s %s</td></tr>", 
                display_name, value, unit)
      }),
      collapse = "\n"
    )
    
    # Construct the legend-like content with tabbed format
    legend_html <- paste0(
      "<table style='width:100%;'>",  # Ensuring the table takes full width
      table_rows,  # Add dynamically generated rows
      "</table><br>"
    )
    
    HTML(legend_html)  # Return HTML to be rendered
  })
  
  current_year <- reactive({
    input$year
  })
  
  observeEvent(input$submit_one, {
    # Change button color to green and disable both buttons
    shinyjs::addClass("submit_one", "btn-success")
    shinyjs::removeClass("submit_one", "btn-secondary")
    shinyjs::disable("submit_one")
    shinyjs::disable("submit_two")
    
    # Run the initialize function immediately
    initialize_or_update_map(current_year())
    
    # Reset the buttons back to their original state (inline)
    shinyjs::enable("submit_one")
    shinyjs::enable("submit_two")
    shinyjs::removeClass("submit_one", "btn-success")
    shinyjs::addClass("submit_one", "btn-secondary")  # Back to original color
    shinyjs::removeClass("submit_two", "btn-success")
    shinyjs::addClass("submit_two", "btn-secondary")  # Back to original color
  })
  
  observeEvent(input$submit_two, {
    # Change button color to green and disable both buttons
    shinyjs::addClass("submit_two", "btn-success")
    shinyjs::removeClass("submit_two", "btn-secondary")
    shinyjs::disable("submit_one")
    shinyjs::disable("submit_two")
    
    # Run the initialize function immediately
    initialize_or_update_map(current_year())
    
    # Reset the buttons back to their original state (inline)
    shinyjs::enable("submit_one")
    shinyjs::enable("submit_two")
    shinyjs::removeClass("submit_one", "btn-success")
    shinyjs::addClass("submit_one", "btn-secondary")  # Back to original color
    shinyjs::removeClass("submit_two", "btn-success")
    shinyjs::addClass("submit_two", "btn-secondary")  # Back to original color
  })
  
  
}

shinyApp(ui, server)