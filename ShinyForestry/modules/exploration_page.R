library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

source("utils/api_functions.R")
source("config.R")

exploration_page_ui <- function(id) {
  ns <- NS(id)
  div(id = "exploration-page",
      useShinyjs(),
      tagList(
        fluidRow(
          column(12, leafletOutput(ns("map1"), height="100vh")) # !TODO shouldn't have to say "100vh" here
        )
    ),
    map_and_slider_ui(id = ns("explore"), 2025, 2049, 2025, show_time_series=FALSE),
    div(class = "explore-box map1", uiOutput(ns("value_boxes")))
  )
}

exploration_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for the module
    
    output$map1 <- renderLeaflet({
      leaflet() %>%
        addResetMapButton() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = state$map$lng, lat = state$map$lat, zoom = state$map$zoom) %>%
        addLegend(
          position = "bottomright",
          colors = adjustcolor(unname(head(COLOUR_MAPPING, -1)), alpha.f = FILL_OPACITY),
          labels = names(head(COLOUR_MAPPING, -1)),
          title = "Planting Type",
          opacity = 1.0
        )
    })
    
    outputOptions(output, "map1", suspendWhenHidden = FALSE)
    
    current_year <- reactive({
      input[[ns("year")]]
    })
    
    # JS to detect when the map is rendered; fed back to main app
    # if you do #map1 .leaflet-container it doesn't seem to work.
    observe({
      shinyjs::runjs('
        var mapCheckInterval = setInterval(function() {
          if (document.querySelector(".leaflet-container")) {
            clearInterval(mapCheckInterval);  // Stop checking once the map is rendered
            Shiny.setInputValue("explrpageRendered", true, {priority: "event"});  // Set the input value
          }
        }, 100);
      ')
    })
    
    new_data_one <- reactiveVal(NULL)
    new_vals_one <- reactiveVal(NULL)
    filtered_data_one <- reactiveVal(NULL)
    current_layers_one <- reactiveVal(list())

    initialize_or_update_map <- function(input_year) {
      # Fetch the data from the API when initializing or submitting
      # new_data_fetched <- st_read(fetch_api_data())  # Hit the API and get the data
      
      new_fetched_one <- get_random_strategy()  # Use GET otherwise
      
      new_data_fetched_one <- new_fetched_one[[1]]
      new_data_fetched_one$parcel_id <- paste0(new_data_fetched_one$parcel_id, "_one")
      new_values_fetched_one <- new_fetched_one[[2]]
      
      if (!is.null(new_data_fetched_one)) {
        # Apply the filter based on the selected year
        new_data_one(new_data_fetched_one)
        new_vals_one(new_values_fetched_one)
        
        filtered_data_subset_one <- new_data_fetched_one[new_data_fetched_one$planting_year <= input_year, ]
        filtered_data_one(filtered_data_subset_one)
        
        current_layers_one(filtered_data_subset_one$parcel_id)
        
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
    }
    
    observe({
      req(!state$initialized)
      initialize_or_update_map(YEAR_MIN)
      state$exp_tab$initialized <- TRUE
    })
    
    # This is duplicated four times; modularise
    observe({
      req(input[[ns("year")]])
      input_year <-  input[[ns("year")]]
      selected_view <-  input[[ns("view_toggle")]]
      
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
    
    values <- reactiveValues(
      names = c("Carbon", "All", "Cossus_cossus", "Lucanus_cervus", "Lichens", "Area", "Visits", "X", "Y"),
      counts = setNames(rep(10, 9), c("Carbon", "All", "Cossus_cossus", "Lucanus_cervus", "Lichens", "Area", "Visits", "X", "Y"))
    )
    
    # observe +/- dynamically
    observe({
      lapply(values$names, function(name) {
        observeEvent(input[[paste0("inc_", name)]], {
          values$counts[[name]] <- values$counts[[name]] + 1
          print("increase")
        })
        
        observeEvent(input[[paste0("dec_", name)]], {
          values$counts[[name]] <- max(0, values$counts[[name]] - 1)
          print("decrease")
        })
      })
    })
    
    # Create value_boxes dynamically
    output$value_boxes <- renderUI({
      div(class = "explore-box-content",
          
          # Section for the +/- buttons
          div(class = "button-container",
              lapply(values$names, function(name) {
                tagList(
                  tags$div(style = "display: flex; justify-content: space-between; align-items: center; padding: 4px 10px;", 
                           actionButton(ns(paste0("inc_", name)), "+", class = "btn btn-outline-primary small-button"),
                           tags$span(style = "flex-grow: 1; text-align: center;", name),  # Center aligned name
                           actionButton(ns(paste0("dec_", name)), "-", class = "btn btn-outline-primary small-button")
                  )
                )
              })
          )
      )
    })
    
    
    
    
    
    
  })
}
