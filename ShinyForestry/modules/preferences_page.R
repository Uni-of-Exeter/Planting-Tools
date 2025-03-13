library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

source("utils/api_functions.R")
source("config.R")

FullTableNotAvail <- st_read(normalizePath(file.path(normalizePath(file.path(normalizePath(getwd()), "ElicitorOutput")), "FullTableNotAvail.geojson")), quiet=TRUE)

FullTableNotAvailONE <- FullTableNotAvail %>%
  mutate(id = paste0("ONE_", row_number()))
FullTableNotAvailTWO <- FullTableNotAvail %>%
  mutate(id = paste0("TWO_", row_number()))

preferences_page_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom styling for preferences page (load the preferences_page.css in the header)
    # Page content
    div(id = "preferences-page", 
        # Fluid row for the maps
        fluidRow(
          column(6, leafletOutput(ns("map1")), class = "no-padding"),  # First Map
          column(6, leafletOutput(ns("map2")), class = "no-padding")   # Second Map
        ),
        map_and_slider_ui(id = ns("prefs"), 2025, 2049, 2025, show_time_series=FALSE),
        # Value boxes for map1 and map2
        div(id = "value-box-1", class = "value-box map1", 
            uiOutput(ns("value_box_one")),
            actionButton(ns("submit_one"), "Choose this strategy", class = "btn btn-secondary") 
        ),
        div(id = "value-box-2", class = "value-box map2", 
            uiOutput(ns("value_box_two")), 
            actionButton(ns("submit_two"), "Choose this strategy", class = "btn btn-secondary")
        )
    )
  )
}



preferences_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the first map (map1) with synchronization
    output$map1 <- renderLeaflet({
      leaflet() %>%
        # enableTileCaching() %>% # this is great but seems to be slower on initial load
        addResetMapButton() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>% #, options = tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>%
        setView(lng = state$map$lng, lat = state$map$lat, zoom = state$map$zoom) %>%
        syncWith("pref_maps")  # Sync the map with map2
    })

    # Render the second map (map2) with synchronization
    output$map2 <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        # enableTileCaching() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>% #, options = tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>%
        setView(lng = state$map$lng, lat = state$map$lat, zoom = state$map$zoom) %>%
        addLegend(
          position = "topright",
          colors = adjustcolor(unname(head(COLOUR_MAPPING, -1)), alpha.f = FILL_OPACITY),
          labels = names(head(COLOUR_MAPPING, -1)),
          title = "Planting Type",
          opacity = 1.0
        ) %>%
        syncWith("pref_maps")  # Sync the map with map1
    })

    outputOptions(output, "map1", suspendWhenHidden = FALSE)
    outputOptions(output, "map2", suspendWhenHidden = FALSE)

    # JS to detect when the map is rendered; fed back to main app
    # if you do #map1 .leaflet-container it doesn't seem to work.
    observe({
      shinyjs::runjs('
        var mapCheckInterval = setInterval(function() {
          if (document.querySelector(".leaflet-container")) {
            clearInterval(mapCheckInterval);  // Stop checking once the map is rendered
            Shiny.setInputValue("prefpageRendered", true, {priority: "event"});  // Set the input value
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

    initialize_or_update_map <- function(input_year, choice=1) {
      # Fetch the data from the API when initializing or submitting
      # new_data_fetched <- st_read(fetch_api_data())  # Hit the API and get the data

      
      # this needs to be a POST endpoint
      fetched_data <- post_preference_choice(as.integer(choice))
      
      new_fetched_one <- fetched_data[[1]]
      new_fetched_two <- fetched_data[[2]]
      
      new_data_fetched_one <- new_fetched_one$geojson
      new_data_fetched_one$parcel_id <- paste0(new_data_fetched_one$parcel_id, "_one")
      new_values_fetched_one <- new_fetched_one$values

      new_data_fetched_two <- new_fetched_two$geojson
      new_data_fetched_two$parcel_id <- paste0(new_data_fetched_two$parcel_id, "_two")
      new_values_fetched_two <- new_fetched_two$values

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

      if (!is.null(new_data_fetched_two)) {
        # Apply the filter based on the selected year
        new_data_two(new_data_fetched_two)
        new_vals_two(new_values_fetched_two)

        filtered_data_subset_two <- new_data_fetched_two[new_data_fetched_two$planting_year <= input_year, ]
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

    observe({
      req(!state$initialized)
      initialize_or_update_map(YEAR_MIN)
      state$pref_tab$initialized <- TRUE
    })

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
      input[[ns("year")]]
    })

    observeEvent(input$submit_one, {
      # Change button color to green and disable both buttons
      shinyjs::addClass("submit_one", "btn-success")
      shinyjs::removeClass("submit_one", "btn-secondary")
      shinyjs::disable("submit_one")
      shinyjs::disable("submit_two")

      # Run the initialize function immediately
      initialize_or_update_map(current_year(), 1)

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
      initialize_or_update_map(current_year(), 2)

      # Reset the buttons back to their original state (inline)
      shinyjs::enable("submit_one")
      shinyjs::enable("submit_two")
      shinyjs::removeClass("submit_one", "btn-success")
      shinyjs::addClass("submit_one", "btn-secondary")  # Back to original color
      shinyjs::removeClass("submit_two", "btn-success")
      shinyjs::addClass("submit_two", "btn-secondary")  # Back to original color
    })

  })
}
