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
    # 2x2 Grid [need to change the CSS to move the value_box_...]
    # tagList( 
    #   fluidRow(
    #     column(3, leafletOutput(ns("map1")), class = "no-padding"),  # First Map
    #     column(3, leafletOutput(ns("map2")), class = "no-padding")   # Second Map
    #   ),
    #   fluidRow(
    #     column(6, leafletOutput(ns("map3")), class = "no-padding"),  # Third Map
    #     column(6, leafletOutput(ns("map4")), class = "no-padding")   # Fourth Map
    #   )
    # ),
    # 4 columns
    tagList(
      fluidRow(
        column(3, leafletOutput(ns("map1")), class = "no-padding", height="100vh"),  # First Map
        column(3, leafletOutput(ns("map2")), class = "no-padding", height="100vh"),   # Second Map
        # ),
        # fluidRow(
        column(3, leafletOutput(ns("map3")), class = "no-padding",  height="100vh"),  # Third Map
        column(3, leafletOutput(ns("map4")), class = "no-padding", height="100vh")   # Fourth Map
      )
    ),
    # actionButton(ns("sample"), "Sample", class = "btn btn-success floating-button"),
    # actionButton(ns("select_two"), "Choose Strategy 2", class = "btn btn-secondary floating-button map2"),
    # actionButton(ns("select_three"), "Choose Strategy 3", class = "btn btn-secondary floating-button map3"),
    # actionButton(ns("select_four"), "Choose Strategy 4", class = "btn btn-secondary floating-button map4"),
    div(id = "value-box-1", class = "value-box map1", uiOutput(ns("value_box_one"))),
    div(id = "value-box-2", class = "value-box map2", uiOutput(ns("value_box_two"))),
    div(id = "value-box-3", class = "value-box map3", uiOutput(ns("value_box_three"))),
    div(id = "value-box-4", class = "value-box map4", uiOutput(ns("value_box_four"))),
    map_and_slider_ui_sample(id = ns("alt"), 2025, 2049, 2025, show_time_series=FALSE)
  )
}

alt_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for the module
    
    output$map1 <- renderLeaflet({
      leaflet() %>%
        addResetMapButton() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = state$map$lng, lat = state$map$lat, zoom = state$alt_tab$map$zoom) %>%
        syncWith("maps")
    })
    
    output$map2 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = state$map$lng, lat = state$map$lat, zoom = state$alt_tab$map$zoom) %>%
        syncWith("maps")
    })
    
    output$map3 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = state$map$lng, lat = state$map$lat, zoom = state$alt_tab$map$zoom) %>%
        syncWith("maps") 
    })
    
    output$map4 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%  # Use your custom base map layer
        setView(lng = state$map$lng, lat = state$map$lat, zoom = state$alt_tab$map$zoom) %>%
        syncWith("maps")
    })
    
    outputOptions(output, "map1", suspendWhenHidden = FALSE)
    outputOptions(output, "map2", suspendWhenHidden = FALSE)
    outputOptions(output, "map3", suspendWhenHidden = FALSE)
    outputOptions(output, "map4", suspendWhenHidden = FALSE)
    
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
            Shiny.setInputValue("altpageRendered", true, {priority: "event"});  // Set the input value
          }
        }, 100);
      ')
    })
    
    new_data_one <- reactiveVal(NULL)
    new_vals_one <- reactiveVal(NULL)
    filtered_data_one <- reactiveVal(NULL)
    current_layers_one <- reactiveVal(list())
    
    new_data_two <- reactiveVal(NULL)
    new_vals_two <- reactiveVal(NULL)
    filtered_data_two <- reactiveVal(NULL)
    current_layers_two <- reactiveVal(list())
    
    new_data_three <- reactiveVal(NULL)
    new_vals_three <- reactiveVal(NULL)
    filtered_data_three <- reactiveVal(NULL)
    current_layers_three <- reactiveVal(list())
    
    new_data_four <- reactiveVal(NULL)
    new_vals_four <- reactiveVal(NULL)
    filtered_data_four <- reactiveVal(NULL)
    current_layers_four <- reactiveVal(list())
    
    initialize_or_update_map <- function(input_year) {
      # Fetch the data from the API when initializing or submitting
      # new_data_fetched <- st_read(fetch_api_data())  # Hit the API and get the data
      
      fetched_data <- get_alternative_approaches()
      # print("fetched_data")
      # print(fetched_data)
      
      new_fetched_one <- fetched_data[[1]]  # Use GET otherwise
      new_fetched_two <- fetched_data[[2]]  # Use GET otherwise
      new_fetched_three <- fetched_data[[3]]  # Use GET otherwise
      new_fetched_four <- fetched_data[[4]]  # Use GET otherwise
      
      new_data_fetched_one <- new_fetched_one$geojson
      new_data_fetched_one$parcel_id <- paste0(new_data_fetched_one$parcel_id, "_one")
      new_values_fetched_one <- new_fetched_one$values
      
      new_data_fetched_two <- new_fetched_two$geojson
      new_data_fetched_two$parcel_id <- paste0(new_data_fetched_two$parcel_id, "_two")
      new_values_fetched_two <- new_fetched_two$values

      new_data_fetched_three <- new_fetched_three$geojson
      new_data_fetched_three$parcel_id <- paste0(new_data_fetched_three$parcel_id, "_three")
      new_values_fetched_three <- new_fetched_three$values
      
      new_data_fetched_four <- new_fetched_four$geojson
      new_data_fetched_four$parcel_id <- paste0(new_data_fetched_four$parcel_id, "_four")
      new_values_fetched_four <- new_fetched_four$values
      
      if (!is.null(new_data_fetched_one)) {
        # Apply the filter based on the selected year
        new_data_one(new_data_fetched_one)
        new_vals_one(new_values_fetched_one)
        
        filtered_data_subset_one <- new_data_fetched_one[new_data_fetched_one$planting_year <= input_year, ]
        filtered_data_subset_one <- filtered_data_subset_one[!st_is_empty(filtered_data_subset_one$geometry), ] # ensure it's valid
        filtered_data_one(filtered_data_subset_one)
        
        current_layers_one(filtered_data_subset_one$parcel_id)
        
        # Render the leaflet map with the updated data
        # print("new_data_fetched_one")
        # print(new_data_fetched_one)
        # print("------------------")
        
        leafletProxy("map1") %>%
          addPolygons(
            data = new_data_fetched_one,  # Use the full dataset as the base layer
            weight = 1,
            color = PARCEL_LINE_COLOUR,
            fillColor = AVAILABLE_PARCEL_COLOUR,
            fillOpacity = FILL_OPACITY,
            group = "parcelPolygons1",  # Group the polygons
            layerId = ~parcel_id,  # Set unique IDs for each polygon
            label = ~parcel_id,
            # popup = "No planting"
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
          )
        
        # Add filtered polygons based on the selected year if any valid data exists
        if (nrow(filtered_data_subset_one) > 0) {
          leafletProxy("map1") %>%
            addPolygons(
              data = filtered_data_subset_one,  # Add filtered data if it's available
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Use the planting type color
              fillOpacity = FILL_OPACITY,
              layerId = ~parcel_id,
              label = ~parcel_id
            )
        }
      } else {
        print("API fetch failed, no data to update.")
      }
      
      if (!is.null(new_data_fetched_two)) {
        # Apply the filter based on the selected year
        new_data_two(new_data_fetched_two)
        new_vals_two(new_values_fetched_two)
        
        filtered_data_subset_two <- new_data_fetched_two[new_data_fetched_two$planting_year <= input_year, ]
        filtered_data_subset_two <- filtered_data_subset_two[!st_is_empty(filtered_data_subset_two$geometry), ] # ensure it's valid
        filtered_data_two(filtered_data_subset_two)
        
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
          )
        
        # Add filtered polygons based on the selected year if any valid data exists
        if (nrow(filtered_data_subset_two) > 0) {
          leafletProxy("map2") %>%
            addPolygons(
              data = filtered_data_subset_two,  # Add filtered data if it's available
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Use the planting type color
              fillOpacity = FILL_OPACITY,
              layerId = ~parcel_id,
              label = ~parcel_id
            )
        }
        
      } else {
        print("API fetch failed, no data to update.")
      }
      
      if (!is.null(new_data_fetched_three)) {
        # Apply the filter based on the selected year
        new_data_three(new_data_fetched_three)
        new_vals_three(new_values_fetched_three)
        
        filtered_data_subset_three <- new_data_fetched_three[new_data_fetched_three$planting_year <= input_year, ]
        filtered_data_subset_three <- filtered_data_subset_three[!st_is_empty(filtered_data_subset_three$geometry), ] # ensure it's valid
        filtered_data_three(filtered_data_subset_three)
        
        leafletProxy("map3") %>%
          addPolygons(
            data = new_data_fetched_three,  # Use the full dataset as the base layer
            weight = 1,
            color = PARCEL_LINE_COLOUR,
            fillColor = AVAILABLE_PARCEL_COLOUR,
            fillOpacity = FILL_OPACITY,
            group = "parcelPolygons3",  # Group the polygons
            layerId = ~parcel_id,  # Set unique IDs for each polygon
            label = ~parcel_id,
            # popup = "No planting"
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
          )
        
        # Add filtered polygons based on the selected year if any valid data exists
        if (nrow(new_data_fetched_three) > 0) {
          leafletProxy("map3") %>%
            addPolygons(
              data = new_data_fetched_three,  # Add filtered data if it's available
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Use the planting type color
              fillOpacity = FILL_OPACITY,
              layerId = ~parcel_id,
              label = ~parcel_id
            )
        }
        
      } else {
        print("API fetch failed, no data to update.")
      }
      
      if (!is.null(new_data_fetched_four)) {
        # Apply the filter based on the selected year
        new_data_four(new_data_fetched_four)
        new_vals_four(new_values_fetched_four)
        
        filtered_data_subset_four <- new_data_fetched_four[new_data_fetched_four$planting_year <= input_year, ]
        filtered_data_subset_four <- filtered_data_subset_four[!st_is_empty(filtered_data_subset_four$geometry), ] # ensure it's valid
        filtered_data_four(filtered_data_subset_four)
        
        leafletProxy("map4") %>%
          addPolygons(
            data = new_data_fetched_four,  # Use the full dataset as the base layer
            weight = 1,
            color = PARCEL_LINE_COLOUR,
            fillColor = AVAILABLE_PARCEL_COLOUR,
            fillOpacity = FILL_OPACITY,
            group = "parcelPolygons4",  # Group the polygons
            layerId = ~parcel_id,  # Set unique IDs for each polygon
            label = ~parcel_id,
            # popup = "No planting"
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
          )
        
        # Add filtered polygons based on the selected year if any valid data exists
        if (nrow(new_data_fetched_four) > 0) {
          leafletProxy("map4") %>%
            addPolygons(
              data = new_data_fetched_four,  # Add filtered data if it's available
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Use the planting type color
              fillOpacity = FILL_OPACITY,
              layerId = ~parcel_id,
              label = ~parcel_id
            )
        }
        
      } else {
        print("API fetch failed, no data to update.")
      }
      
      # print(new_vals_one)
      # print(new_vals_two)
      # print(new_vals_three)
      # print(new_vals_four)
      # print('done')
    }
    
    observe({
      req(!state$initialized)
      initialize_or_update_map(YEAR_MIN)
      state$alt_tab$initialized <- TRUE
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
        if (!is.null(to_remove_one) && !any(is.na(to_remove_one)) && length(to_remove_one) > 0) {
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
        if (!is.null(to_add_one) && !any(is.na(to_add_one)) && length(to_add_one) > 0) {
          leafletProxy("map1") %>%
            addPolygons(
              data = current_data_one[current_data_one$parcel_id %in% to_add_one, ],  # Filtered data for new polygons
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Colour for filtered polygons
              fillOpacity = FILL_OPACITY,
              group = "filteredPolygons",  # Group for filtered polygons
              layerId = ~parcel_id,  # Use parcel_id as layerId to add new polygons
              label = ~parcel_id,
              # popup = ~planting_types
            )
        }
        
        current_layers_one(current_ids_one)
        
      }
    })
    observe({
      req(input[[ns("year")]])
      
      input_year <-  input[[ns("year")]]
      selected_view <-  input[[ns("view_toggle")]]
      
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
        
        if (!is.null(to_remove_two) && !any(is.na(to_remove_two)) && length(to_remove_two) > 0) {
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
        if (!is.null(to_add_two) && !any(is.na(to_add_two)) && length(to_add_two) > 0) {
          leafletProxy("map2") %>%
            addPolygons(
              data = current_data_two[current_data_two$parcel_id %in% to_add_two, ],  # Filtered data for new polygons
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Colour for filtered polygons
              fillOpacity = FILL_OPACITY,
              group = "filteredPolygons",  # Group for filtered polygons
              layerId = ~parcel_id,  # Use parcel_id as layerId to add new polygons
              label = ~parcel_id,
              # popup = ~planting_types
            )
        }
        current_layers_two(current_ids_two)
        
        # Need to do something similar for blocked
      }
    })
    observe({
      req(input[[ns("year")]])
      
      input_year <-  input[[ns("year")]]
      selected_view <-  input[[ns("view_toggle")]]
      
      if (!is.null(new_data_three())) {
        # Access the most recently loaded data stored in the reactive `new_data`
        current_data_three <- new_data_three()
        
        # # Filter the data based on the selected year and update `filtered_data`
        # filtered_data_subset <- current_data[current_data$planting_year <= input_year, ]
        # filtered_data(filtered_data_subset)  # Update the reactive filtered data
        
        filtered_data_subset_three <- if (selected_view == "Cumulative") {
          current_data_three[current_data_three$planting_year <= input_year, ]  # Up to the selected year
        } else {
          current_data_three[current_data_three$planting_year == input_year, ]  # Only the selected year
        }
        filtered_data_three(filtered_data_subset_three)  # Update the reactive filtered data
        
        # Get the current IDs of the filtered polygons
        current_ids_three <- unique(filtered_data_subset_three$parcel_id)
        
        # Get the current state of the layers (those that are already on the map)
        existing_layers_three <- current_layers_three()  # Use the reactive current_layers()
        
        to_add_three <- setdiff(current_ids_three, existing_layers_three)
        to_remove_three <- setdiff(existing_layers_three, current_ids_three)  # Ensure this is a vector of IDs
        
        if (!is.null(to_remove_three) && !any(is.na(to_remove_three)) && length(to_remove_three) > 0) {
          # Get the data for all parcels that need to be recoloured from FullTable
          updated_data_three <- current_data_three[current_data_three$parcel_id %in% to_remove_three, ]  # Use `to_remove` to filter
          
          # Update the recolour of polygons in one go
          leafletProxy("map3") %>%
            addPolygons(
              data = updated_data_three,  # Use the updated data for the parcels in `to_remove`
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = AVAILABLE_PARCEL_COLOUR,
              fillOpacity = FILL_OPACITY,
              layerId = updated_data_three$parcel_id,  # Ensure to reassign the same layerId for all
              # label = updated_data$parcel_id
            )
        }
        
        # Add new polygons (those that are in filtered data but not on the map)
        if (!is.null(to_add_three) && !any(is.na(to_add_three)) && length(to_add_three) > 0) {
          leafletProxy("map3") %>%
            addPolygons(
              data = current_data_three[current_data_three$parcel_id %in% to_add_three, ],  # Filtered data for new polygons
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Colour for filtered polygons
              fillOpacity = FILL_OPACITY,
              group = "filteredPolygons",  # Group for filtered polygons
              layerId = ~parcel_id,  # Use parcel_id as layerId to add new polygons
              label = ~parcel_id,
              # popup = ~planting_types
            )
        }
        current_layers_three(current_ids_three)
        
        # Need to do something similar for blocked
      }
    })
    observe({
      req(input[[ns("year")]])
      
      input_year <-  input[[ns("year")]]
      selected_view <-  input[[ns("view_toggle")]]
      
      if (!is.null(new_data_four())) {
        # Access the most recently loaded data stored in the reactive `new_data`
        current_data_four <- new_data_four()
        
        # # Filter the data based on the selected year and update `filtered_data`
        # filtered_data_subset <- current_data[current_data$planting_year <= input_year, ]
        # filtered_data(filtered_data_subset)  # Update the reactive filtered data
        
        filtered_data_subset_four <- if (selected_view == "Cumulative") {
          current_data_four[current_data_four$planting_year <= input_year, ]  # Up to the selected year
        } else {
          current_data_four[current_data_four$planting_year == input_year, ]  # Only the selected year
        }
        filtered_data_four(filtered_data_subset_four)  # Update the reactive filtered data
        
        # Get the current IDs of the filtered polygons
        current_ids_four <- unique(filtered_data_subset_four$parcel_id)
        
        # Get the current state of the layers (those that are already on the map)
        existing_layers_four <- current_layers_four()  # Use the reactive current_layers()
        
        to_add_four <- setdiff(current_ids_four, existing_layers_four)
        to_remove_four <- setdiff(existing_layers_four, current_ids_four)  # Ensure this is a vector of IDs
        
        if (!is.null(to_remove_four) && !any(is.na(to_remove_four)) && length(to_remove_four) > 0) {
          # Get the data for all parcels that need to be recoloured from FullTable
          updated_data_four <- current_data_four[current_data_four$parcel_id %in% to_remove_four, ]  # Use `to_remove` to filter
          
          # Update the recolour of polygons in one go
          leafletProxy("map4") %>%
            addPolygons(
              data = updated_data_four,  # Use the updated data for the parcels in `to_remove`
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = AVAILABLE_PARCEL_COLOUR,
              fillOpacity = FILL_OPACITY,
              layerId = updated_data_four$parcel_id,  # Ensure to reassign the same layerId for all
              # label = updated_data$parcel_id
            )
        }
        
        # Add new polygons (those that are in filtered data but not on the map)
        if (!is.null(to_add_four) && !any(is.na(to_add_four)) && length(to_add_four) > 0) {
          leafletProxy("map4") %>%
            addPolygons(
              data = current_data_four[current_data_four$parcel_id %in% to_add_four, ],  # Filtered data for new polygons
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_types]),  # Colour for filtered polygons
              fillOpacity = FILL_OPACITY,
              group = "filteredPolygons",  # Group for filtered polygons
              layerId = ~parcel_id,  # Use parcel_id as layerId to add new polygons
              label = ~parcel_id,
              # popup = ~planting_types
            )
        }
        current_layers_four(current_ids_four)
        
        # Need to do something similar for blocked
      }
    })
    
    output$value_box_one <- renderUI({
      current_value <- new_vals_one()
      
      # Generate table rows dynamically
      table_rows <- paste0(
        lapply(names(current_value), function(name) {
          # Get the index of the slider name in state$map_tab$slider$names
          idx <- which(state$map_tab$slider$names == name)
          
          # Get the display name for the slider (from the slider names list)
          display_name <- state$map_tab$slider$names[idx]
          
          # Assuming unit is not explicitly provided, but if it is, you can access it similarly
          # If unit is not available, you can use a default unit like "units"
          unit <- "units"  # Replace with real logic if you have units elsewhere in your state
          
          # Get the current value for the slider
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
    output$value_box_two <- renderUI({
      current_value <- new_vals_two()
      
      # Generate table rows dynamically
      table_rows <- paste0(
        lapply(names(current_value), function(name) {
          # Get the index of the slider name in state$map_tab$slider$names
          idx <- which(state$map_tab$slider$names == name)
          
          # Get the display name for the slider (from the slider names list)
          display_name <- state$map_tab$slider$names[idx]
          
          # Assuming unit is not explicitly provided, but if it is, you can access it similarly
          # If unit is not available, you can use a default unit like "units"
          unit <- "units"  # Replace with real logic if you have units elsewhere in your state
          
          # Get the current value for the slider
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
    output$value_box_three <- renderUI({
      current_value <- new_vals_three()
      
      # Generate table rows dynamically
      table_rows <- paste0(
        lapply(names(current_value), function(name) {
          # Get the index of the slider name in state$map_tab$slider$names
          idx <- which(state$map_tab$slider$names == name)
          
          # Get the display name for the slider (from the slider names list)
          display_name <- state$map_tab$slider$names[idx]
          
          # Assuming unit is not explicitly provided, but if it is, you can access it similarly
          # If unit is not available, you can use a default unit like "units"
          unit <- "units"  # Replace with real logic if you have units elsewhere in your state
          
          # Get the current value for the slider
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
    output$value_box_four <- renderUI({
      current_value <- new_vals_four()
      
      # Generate table rows dynamically
      table_rows <- paste0(
        lapply(names(current_value), function(name) {
          # Get the index of the slider name in state$map_tab$slider$names
          idx <- which(state$map_tab$slider$names == name)
          
          # Get the display name for the slider (from the slider names list)
          display_name <- state$map_tab$slider$names[idx]
          
          # Assuming unit is not explicitly provided, but if it is, you can access it similarly
          # If unit is not available, you can use a default unit like "units"
          unit <- "units"  # Replace with real logic if you have units elsewhere in your state
          
          # Get the current value for the slider
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
    
    observeEvent(input[[ns("sample")]], {
      print('sample pressed')
      shinyjs::removeClass(ns("sample"), "btn-success")
      shinyjs::addClass(ns("sample"), "btn-secondary")
      shinyjs::disable(ns("sample"))
      
      initialize_or_update_map(current_year())
      
      shinyjs::addClass(ns("sample"), "btn-success")
      shinyjs::enable(ns("sample"))
    })
    
  })
}
