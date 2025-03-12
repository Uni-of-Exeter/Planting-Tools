library(shiny)
library(leaflet)
library(bslib)  # For Bootstrap-based accordion UI
library(shinyjs) # For sidebar toggling
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

source("utils/api_functions.R")
source("config.R")

FullTableNotAvail <- st_read(normalizePath(file.path(normalizePath(file.path(normalizePath(getwd()), "ElicitorOutput")), "FullTableNotAvail.geojson")), quiet=TRUE)

map_page_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "display: flex; height: 100vh; flex-direction: row; width: 100%;",
      tags$head(
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css")
      ),
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
            uiOutput(ns("dynamic_sliders")),
            actionButton(ns("submit_main"), "submit", class = "btn btn-secondary"),
            actionButton(ns("reset_main"), "reset", class = "btn btn-secondary"),
            actionButton(ns("save_main"), "Save in Session")
          ),
          accordion_panel(
            "Saved Strategies",
            uiOutput(ns("saved_strategies"))
          )
        )
      ),
      
      mainPanel(
        class = "main-panel",
        leafletOutput(ns("map"), height = "100%"),
        map_and_slider_ui(id = ns("map"), 2025, 2049, 2025, panel_width = "50%"),
      )
    )
  )
}

map_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A reactive expression to track the current year
    current_year <- reactive({
      input[[ns("year")]]
    })

    #----COPYPASTA
    
    new_data <- reactiveVal(NULL) # most recent data
    new_vals <- reactiveVal(NULL) # most recent values
    
    filtered_data <- reactiveVal(NULL) # data filtered by year
    filtered_data_blocked <- reactiveVal(NULL) # data filtered by year
    panel_expanded <- reactiveVal(FALSE)
    output_data <- reactiveVal(NULL)
    previously_blocked <- reactiveVal(data.frame(parcel_id = character(), blocked_until_year = integer()))
    
    current_layers <- reactiveVal(list()) # Keep track of layers currently on the map (filtered ones)
    
    saved_strategies <- reactiveVal(list()) # Store saved strategies as a named list (acting as a hashmap)
    strategy_counter <- reactiveVal(1)  # Counter to keep track of strategy keys
    plot_type <- reactiveVal("cumulative") 
    
    # Track the initial slider values when the submit button is clicked
    initial_values <- reactiveVal(list(
      carbon = NULL,
      species = NULL,
      species_goat_moth = NULL,
      species_stag_beetle = NULL,
      species_lichens = NULL,
      area = NULL,
      recreation = NULL
    ))
    
    clicked_polygons <- reactiveVal(data.frame(
      parcel_id = character(),  # Empty initially
      blocked_until_year = numeric(),
      stringsAsFactors = FALSE
    ))
    
    clicked_polygons_injest <- reactiveVal(data.frame(
      parcel_id = character(),  # Empty initially
      blocked_until_year = numeric(),
      stringsAsFactors = FALSE
    ))
    
    # JS to make sure plotly plot moves nicely
    # This is currently broken with the namespace
    #
    observe({
      # Trigger plot layout adjustment when the plot is shown
      shinyjs::runjs("
        // Listen for visibility change or toggle button click
        $('#map-map-time_series_plot').on('shown.bs.collapse', function() {
          setTimeout(function() {
            Plotly.relayout('map-map-areaPlot', { width: $('#map-map-time_series_plot').width() });
          }, 100);  // Add small delay to ensure layout changes
        });
    
        // Optionally, adjust layout when switching between Annual and Cumulative
        $('#view_toggle').on('change', function() {
          setTimeout(function() {
            Plotly.relayout('map-map-areaplot', { width: $('#map-map-time_series_plot').width() });
          }, 100);
        });
    
        // Handle window resizing
        $(window).resize(function() {
          setTimeout(function() {
            // Trigger plot resizing only if the plot is visible
            if ($('#map-map-time_series_plot').is(':visible')) {
              Plotly.relayout('map-map-areaPlot', { width: $('#map-map-time_series_plot').width() });
            }
          }, 5);  // Add small delay for resize to stabilize
        });
      ")
    })
    
    processed_data <- reactive({
      plot_data <- output_data()  # Get the combined data frame from the reactive expression
      
      # If no data, return NULL
      if (is.null(plot_data)) return(NULL)
      
      # Convert total_area to km²
      plot_data$total_area <- set_units(plot_data$total_area, "km^2")
      
      # Compute cumulative area for each planting type
      cumulative_data <- plot_data %>%
        arrange(planting_type, planting_year) %>%
        group_by(planting_type) %>%
        mutate(cumulative_area = cumsum(total_area)) %>%
        ungroup()
      
      # Return both datasets in a list (ready for instant switching)
      list(total = plot_data, cumulative = cumulative_data)
    })
    
    # Render time-series plot for both Conifer and Deciduous
    output[[ns("areaPlot")]] <- renderPlotly({
      data <- processed_data()  # Get precomputed data
      
      # If no data, return NULL
      if (is.null(data)) return(NULL)
      
      # Determine which dataset to use based on user selection
      selected_plot <- if (input[[ns("view_toggle")]] == "Cumulative") {
        data$cumulative %>% rename(y_value = cumulative_area)  # Rename for consistency
      } else {
        data$total %>% rename(y_value = total_area)
      }
      
      # Create the plot
      p <- ggplot(selected_plot, aes(x = planting_year, y = y_value, color = planting_type)) +
        geom_line(size = 1, alpha = FILL_OPACITY) +
        geom_point(size = 1.2) +
        scale_color_manual(values = COLOUR_MAPPING) +
        labs(title = ifelse(input[[ns("view_toggle")]] == "Cumulative",
                            " ",
                            " "),
             x = "Year",
             y = ifelse(input[[ns("view_toggle")]] == "Cumulative",
                        "Cumulative Area Planted",
                        "Total Area Planted"),
             color = "Planting Type") +
        theme_minimal(base_size = 10) +
        theme(
          legend.position = "none",
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent")
        ) 
      
      ggplotly(p) %>%
        layout(
          paper_bgcolor = "rgba(255,255,255,0)",  # Ensure full transparency
          plot_bgcolor = "rgba(255,255,255,0)"
        ) %>%
        config(
          displayModeBar = TRUE  # Keep the toolbar
        )
    })
    
    observeEvent(input[[ns("toggle_plot")]], {
      print("being clicked")
      shinyjs::toggle(id = ns("time_series_plot"), anim = TRUE)
      
      # Change button text dynamically
      new_label <- if (input[[ns("toggle_plot")]] %% 2 == 1) {
        "Hide Time-Series"
      } else {
        "Show Time-Series"
      }
      
      updateActionButton(session, ns("toggle_plot"), label = new_label)
    })
    
    # Initialize leaflet map or update it on submit
    initialize_or_update_map <- function(input_year, data = NULL, json_payload = NULL) {
      # Fetch the data from the API when initializing or submitting
      # new_data_fetched <- st_read(fetch_api_data())  # Hit the API and get the data
      
      new_fetched <- if (!is.null(data)) {
        list(data, NULL)
      } else {
        post_generate_parcels(json_payload)  # Use POST if json_payload is provided (this is just a placeholder)
      }
      
      new_data_fetched <- new_fetched[[1]]
      new_values_fetched <- new_fetched[[2]]
      
      if (!is.null(new_data_fetched)) {
        # Apply the filter based on the selected year
        new_data(new_data_fetched)
        new_vals(new_values_fetched)
        
        stopifnot(all(sort(names(new_values_fetched)) == sort(names(SLIDER_NAMES))))
        
        # Initialize clicked_polygons() with all parcel_ids and a default blocked_until_year
        clicked_polygons(data.frame(
          parcel_id = new_data_fetched$parcel_id,
          blocked_until_year = new_data_fetched$blocked_until_year,
          stringsAsFactors = FALSE
        ))
        
        clicked_polygons_injest(data.frame(
          parcel_id = new_data_fetched$parcel_id,
          blocked_until_year = new_data_fetched$blocked_until_year,
          stringsAsFactors = FALSE
        ))
        
        filtered_data_subset <- new_data_fetched[new_data_fetched$planting_year <= input_year, ]
        filtered_data(filtered_data_subset)
        
        current_layers(filtered_data_subset$parcel_id)
        
        # Update conifer and deciduous data based on the fetched data
        area_data <- new_data_fetched %>%
          dplyr::filter(!is.na(planting_year)) %>%
          # dplyr::mutate( # I don't think we need to do this as it's fine as it is.
          #   geometry = st_make_valid(geometry),  # Ensure valid geometries
          #   parcel_area = st_area(geometry)      # Calculate area of each polygon
          # ) %>%
          dplyr::group_by(planting_year, planting_type) %>%
          dplyr::summarise(total_area = sum(parcel_area, na.rm = TRUE), .groups = 'drop') %>%  # Avoid warning with `.groups`
          dplyr::arrange(planting_year)  # Ensures chronological order
        
        # Save the area data for later use in plots
        output_data(area_data)
        
        # Render the leaflet map with the updated data
        output$map <- renderLeaflet({
          
          legend_html <- paste0(
            "<b>Outcomes</b> (c.f. Targets)<br><br>",
            "<table style='width:100%; text-align:left;'>",
            paste0(
              lapply(names(new_vals()), function(name) {
                # Get the display name from SLIDER_NAMES
                display_name <- SLIDER_NAMES[[name]]$name
                # Get the unit for each slider
                unit <- SLIDER_NAMES[[name]]$unit
                # Get the current value
                value <- round(new_vals()[[name]], POPUP_SIGFIG)
                
                # Format the name, value, and unit into a table row
                sprintf("<tr><td>%s:</td> <td>%s %s</td></tr>", display_name, value, unit)
              }),
              collapse = "\n"
            ),
            "</table></div>"
          )
          
          leaflet() %>% 
            # addTiles() %>% 
            addProviderTiles(providers$CartoDB.Voyager) %>%  # Base map layer
            addResetMapButton() %>% 
            setView(lng = state$map$lng, lat = state$map$lat, zoom = state$map$zoom) %>% 
            # Add base layer (all parcels) from new_data
            addPolygons(
              data = new_data_fetched,  # Use the full dataset as the base layer
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
              data = filtered_data_subset,  # Filtered data based on the year
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
              data = FullTableNotAvail,  # Unavailable parcels
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = UNAVAILABLE_PARCEL_COLOUR,
              fillOpacity = FILL_OPACITY,
              group = "unavailablePolygons",
              # popup = "Unavailable for planting"
            ) %>%
            
            addPolygons(
              data = filtered_data_subset,  # Filtered data based on the year
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~unname(COLOUR_MAPPING[planting_type]),  # Use the planting type color
              fillOpacity = FILL_OPACITY,
              layerId = ~parcel_id,
              label = ~parcel_id,
              # popup = ~planting_type
            ) %>%
            
            addControl(html = legend_html, position='topright') %>% 
            
            # Add legend
            addLegend(
              position = "topright",
              colors = adjustcolor(unname(COLOUR_MAPPING), alpha.f = FILL_OPACITY),
              labels = names(COLOUR_MAPPING),
              title = "Planting Type",
              opacity = 1.0
            )
          
        })
      } else {
        print("API fetch failed, no data to update.")
      }
    }
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(doubleClickZoom = FALSE)) %>%
        # enableTileCaching() %>%
        addProviderTiles(providers$CartoDB.Voyager) #, options = tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>% 
      setView(lng = state$map$lng, lat = state$map$lat, zoom = state$map$zoom)
    })
    
    # Render the sliders
    observe({
      state$map_tab$slider_defaults <- get_slider_values()
      
      default_payload <<- list(
        carbon = state$map_tab$slider_defaults$carbon$default,
        species = state$map_tab$slider_defaults$species$default,
        species_goat_moth = state$map_tab$slider_defaults$species_goat_moth$default,
        species_stag_beetle = state$map_tab$slider_defaults$species_stag_beetle$default,
        species_lichens = state$map_tab$slider_defaults$species_lichens$default,
        area = state$map_tab$slider_defaults$area$default,
        recreation = state$map_tab$slider_defaults$recreation$default,
        blocked_parcels = list()
      )
      
      output$dynamic_sliders <- renderUI({
        tagList(
          fluidRow(
            column(CHECKBOX_COL, checkboxInput(ns("carbon_checkbox"), NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput(ns("carbon"), HTML(paste0("Tree Carbon Stored (tonnes of CO<sub>2</sub>):")),
                                           min = state$map_tab$slider_defaults$carbon$min, 
                                           max = state$map_tab$slider_defaults$carbon$max, 
                                           value = state$map_tab$slider_defaults$carbon$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput(ns("species_checkbox"), NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput(ns("species"), "Species Richness (All):",
                                           min = state$map_tab$slider_defaults$species$min, 
                                           max = state$map_tab$slider_defaults$species$max, 
                                           value = state$map_tab$slider_defaults$species$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput(ns("species_goat_moth_checkbox"), NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput(ns("species_goat_moth"), "Goat Moth (Presence, %):", 
                                           min = state$map_tab$slider_defaults$species_goat_moth$min, 
                                           max = state$map_tab$slider_defaults$species_goat_moth$max, 
                                           value = state$map_tab$slider_defaults$species_goat_moth$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput(ns("species_stag_beetle_checkbox"), NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput(ns("species_stag_beetle"), "Stag Beetle (Presence, %):", 
                                           min = state$map_tab$slider_defaults$species_stag_beetle$min, 
                                           max = state$map_tab$slider_defaults$species_stag_beetle$max, 
                                           value = state$map_tab$slider_defaults$species_stag_beetle$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput(ns("species_lichens_checkbox"), NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput(ns("species_lichens"), "Species Richness (Lichens):", 
                                           min = state$map_tab$slider_defaults$species_lichens$min, 
                                           max = state$map_tab$slider_defaults$species_lichens$max, 
                                           value = state$map_tab$slider_defaults$species_lichens$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput(ns("area_checkbox"), NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput(ns("area"), HTML(paste0("Area Planted (km<sup>2</sup>):")),
                                           min = state$map_tab$slider_defaults$area$min, 
                                           max = state$map_tab$slider_defaults$area$max, 
                                           value = state$map_tab$slider_defaults$area$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput(ns("recreation_checkbox"), NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput(ns("recreation"), "Recreation (visits per month):", 
                                           min = state$map_tab$slider_defaults$recreation$min, 
                                           max = state$map_tab$slider_defaults$recreation$max, 
                                           value = state$map_tab$slider_defaults$recreation$default
            ))
          )
        )
      })
      default_json_payload <- jsonlite::toJSON(default_payload, auto_unbox = TRUE, pretty = TRUE)
      initialize_or_update_map(YEAR_MIN, json_payload = default_json_payload)
      state$map_tab$initialized <- TRUE 
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
    
    # ----

    # Handle submit event to update the map
    observeEvent(input$submit_main, {
      print("submit clicked")
      shinyjs::disable("save_main")
      shinyjs::disable("reset_main")
      shinyjs::disable("submit_main")
      shinyjs::disable("carbon")
      shinyjs::disable("species")
      shinyjs::disable("species_goat_moth")
      shinyjs::disable("species_stag_beetle")
      shinyjs::disable("species_lichens")
      shinyjs::disable("area")
      shinyjs::disable("recreation")
      shinyjs::disable("carbon_checkbox")
      shinyjs::disable("species_checkbox")
      shinyjs::disable("species_goat_moth_checkbox")
      shinyjs::disable("species_stag_beetle_checkbox")
      shinyjs::disable("species_lichens_checkbox")
      shinyjs::disable("area_checkbox")
      shinyjs::disable("recreation_checkbox")

      # Save the initial values when the submit button is clicked
      initial_values(list(
        carbon = input$carbon,
        species = input$species,
        species_goat_moth = input$species_goat_moth,
        species_stag_beetle = input$species_stag_beetle,
        species_lichens = input$species_lichens,
        area = input$area,
        recreation = input$recreation
        # year = input$year
        # num_clicked_polygons = 0
      ))

      # Extract blocked parcels (if any exist)
      blocked_parcels <- clicked_polygons()
      blocked_parcels_filtered <- blocked_parcels[blocked_parcels$blocked_until_year > 0, ]

      # Create the payload
      payload <- list(
        carbon = as.numeric(input$carbon),
        species = as.numeric(input$species),
        species_goat_moth = as.numeric(input$species_goat_moth),
        species_stag_beetle = as.numeric(input$species_stag_beetle),
        species_lichens = as.numeric(input$species_lichens),
        area = as.numeric(input$area),
        recreation = as.numeric(input$recreation),
        blocked_parcels = if (nrow(blocked_parcels_filtered) > 0) {
          # Create a list of blocked parcels
          lapply(1:nrow(blocked_parcels_filtered), function(i) {
            list(
              parcel_id = blocked_parcels_filtered$parcel_id[i],
              blocked_until_year = as.numeric(blocked_parcels_filtered$blocked_until_year[i])
            )
          })
        } else {
          list()  # Return an empty list if no blocked parcels exist
        }
      )

      # Convert to JSON
      json_payload <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)

      print("json payload")
      print(json_payload)
      # Update the map by calling the same function
      initialize_or_update_map(current_year(), json_payload = json_payload)

      # Enable the save button when submit is pressed
      shinyjs::enable("save_main")
      shinyjs::enable("reset_main")

      shinyjs::enable("carbon")
      shinyjs::enable("species")
      shinyjs::enable("species_goat_moth")
      shinyjs::enable("species_stag_beetle")
      shinyjs::enable("species_lichens")
      shinyjs::enable("area")
      shinyjs::enable("recreation")
      shinyjs::enable("carbon_checkbox")
      shinyjs::enable("species_checkbox")
      shinyjs::enable("species_goat_moth_checkbox")
      shinyjs::enable("species_stag_beetle_checkbox")
      shinyjs::enable("species_lichens_checkbox")
      shinyjs::enable("area_checkbox")
      shinyjs::enable("recreation_checkbox")
    })
     
    # Implement clicking off
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      clicked_parcel_id <- click$id  # Get clicked parcel_id
      selected_year <- input[[ns("year")]]    # Get selected year

      print(clicked_parcel_id)
      
      # Ensure clicked_polygons() is not NULL or empty
      current_clicked <- clicked_polygons()

      if (is.null(current_clicked) || nrow(current_clicked) == 0 || !"parcel_id" %in% colnames(current_clicked)) {
        print("clicked_polygons() is empty, NULL, or missing parcel_id column.")
        return()  # Exit early to avoid errors
      }

      # Ensure clicked_parcel_id is not NULL
      if (is.null(clicked_parcel_id) || clicked_parcel_id == "") {
        print("Invalid clicked_parcel_id.")
        return()
      }

      # Check if the parcel exists in the list
      parcel_index <- which(current_clicked$parcel_id == clicked_parcel_id)

      if (length(parcel_index) > 0) {
        # If the parcel is already blocked until the selected year, reset it to 0
        if (current_clicked$blocked_until_year[parcel_index] == selected_year) {
          current_clicked$blocked_until_year[parcel_index] <- 0
          print(paste("Parcel", clicked_parcel_id, "unblocked (set to year 0)."))
        } else {
          # Otherwise, update it to the selected year
          current_clicked$blocked_until_year[parcel_index] <- selected_year
          print(paste("Updated Parcel:", clicked_parcel_id, "Blocked Until:", selected_year))
        }
      } else {
        print("Clicked parcel is not in the list.")  # Debugging message
        return()
      }

      # Update reactive value
      clicked_polygons(current_clicked)

      print(paste("Updated Parcel:", clicked_parcel_id, "Blocked Until:", selected_year))
    })

    observe({
      # Get the current slider values with namespacing
      current_values <- list(
        carbon = input$carbon,
        species = input$species,
        species_goat_moth = input$species_goat_moth,
        species_stag_beetle = input$species_stag_beetle,
        species_lichens = input$species_lichens,
        area = input$area,
        recreation = input$recreation
        # year = input$year
        # num_clicked_polygons = 0
      )

      # Compare current values with initial values
      values_changed <- !identical(current_values, initial_values())
      current_clicked <- clicked_polygons()
      current_clicked_in <- clicked_polygons_injest()

      # Check if values have changed or if there are any blocked parcels
      if (values_changed || (nrow(setdiff(current_clicked_in, current_clicked)) != 0)) {
        # Disable Save button as changes require submission
        shinyjs::disable("save_main")  # Disable the save button

        # Enable and highlight the Submit button (green)
        shinyjs::enable("submit_main")  # Enable the submit button
        shinyjs::addClass("submit_main", "btn-success")  # Add the green/active class

      } else {
        # Enable Save button if no changes have been made
        shinyjs::enable("save_main")  # Enable the save button

        print("disable submit")
        # Disable Submit button if no changes have been made
        shinyjs::disable("submit_main")  # Disable the submit button
        shinyjs::removeClass("submit_main", "btn-success")  # Remove the green/active class
        shinyjs::addClass("submit_main", "btn-secondary")  # Add the gray/disabled class
      }
    })

    # the blocking part of the code hasn't really been tested
    # there is currently a bug where if we click a parcel to block it, and then go > blocked_until_year,
    # it's recoloured to it's eventual colour (regardless of when that change was supposed to happen); or dark gery if it never happens
    # hackily fixed with:
    #
    # fillColor = ~ifelse(is.na(planting_year) | planting_year >= input_year, AVAILABLE_PARCEL_COLOUR, unname(COLOUR_MAPPING[planting_type])),  # Grey if not planted yet or NA
    #
    observe({
      input_year <- input[[ns("year")]]
      selected_view <- input[[ns("view_toggle")]]

      if (!is.null(new_data())) {
        # Access the most recently loaded data stored in the reactive `new_data`
        current_data <- new_data()

        # get clicked polygons
        clicked_info <- clicked_polygons()

        if (!is.null(clicked_info) && nrow(clicked_info) > 0) {
          # Blocked polygons: those whose blocked_until_year is greater than or equal to the input_year
          blocked_parcels <- clicked_info[clicked_info$blocked_until_year >= input_year, ]
          print(clicked_info[clicked_info$blocked_until_year > 0, ])
          # Unblocked parcels: those whose blocked_until_year is less than input_year
          unblocked_parcels <- clicked_info[clicked_info$blocked_until_year < input_year, ]
          unblocked_parcels <- unblocked_parcels[unblocked_parcels$blocked_until_year != 0, ]  # Ensure no "0" entries
        } else {
          blocked_parcels <- NULL
          unblocked_parcels <- NULL
        }

        # Handle unclicks
        prev_blocked <- previously_blocked()
        unclicked_parcels <- prev_blocked[!(prev_blocked$parcel_id %in% blocked_parcels$parcel_id), ]
        previously_blocked(blocked_parcels)

        # # Filter the data based on the selected year and update `filtered_data`
        # filtered_data_subset <- current_data[current_data$planting_year <= input_year, ]
        # filtered_data(filtered_data_subset)  # Update the reactive filtered data

        # **Modify Filtering Based on Selected View**
        filtered_data_subset <- if (selected_view == "Cumulative") {
          current_data[current_data$planting_year <= input_year, ]  # Up to the selected year
        } else {
          current_data[current_data$planting_year == input_year, ]  # Only the selected year
        }
        filtered_data(filtered_data_subset)  # Update the reactive filtered data

        # Get the current IDs of the filtered polygons
        current_ids <- unique(filtered_data_subset$parcel_id)

        # Get the current state of the layers (those that are already on the map)
        existing_layers <- current_layers()  # Use the reactive current_layers()

        # Find the IDs that need to be added and removed
        to_add <- setdiff(current_ids, existing_layers)
        to_remove <- setdiff(existing_layers, current_ids)  # Ensure this is a vector of IDs

        # try update polygons back with a new style
        if (length(to_remove) > 0) {
          # Get the data for all parcels that need to be recoloured from FullTable
          updated_data <- current_data[current_data$parcel_id %in% to_remove, ]  # Use `to_remove` to filter

          # Update the recolour of polygons in one go
          leafletProxy("map") %>%
            addPolygons(
              data = updated_data,  # Use the updated data for the parcels in `to_remove`
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = AVAILABLE_PARCEL_COLOUR,
              fillOpacity = FILL_OPACITY,
              layerId = updated_data$parcel_id,  # Ensure to reassign the same layerId for all
              # label = updated_data$parcel_id
            )
        }

        # Add new polygons (those that are in filtered data but not on the map)
        if (length(to_add) > 0) {
          leafletProxy("map") %>%
            addPolygons(
              data = current_data[current_data$parcel_id %in% to_add, ],  # Filtered data for new polygons
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

        current_layers(current_ids)
        # Need to do something similar for blocked

        # Now handle Blocked Parcels (those that are blocked until input_year)
        if (length(blocked_parcels) > 0) {
          blk <- current_data[current_data$parcel_id %in% blocked_parcels$parcel_id, ]

          # Add blocked polygons with a distinct style (red color for blocked)
          leafletProxy("map") %>%
            addPolygons(
              data = blk,  # Data for blocked parcels
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = BLOCKED_PARCEL_COLOUR,  # Indicating blocked parcels
              fillOpacity = FILL_OPACITY,
              group = "BlockedPolygons",
              layerId = blk$parcel_id,  # Reassign the same layerId for blocked polygons
              label = ~paste("Parcel ID:", parcel_id, "Blocked Until:", blocked_parcels$blocked_until_year[blocked_parcels$parcel_id == parcel_id])
            )
        }

        if (length(unclicked_parcels) > 0) {
          unclk <- current_data[current_data$parcel_id %in% unclicked_parcels$parcel_id, ]

          # Recolor unblocked polygons and update them (reassign color based on planting_type)
          leafletProxy("map") %>%
            addPolygons(
              data = unclk,  # Data for unblocked parcels
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~ifelse(is.na(planting_year) | planting_year >= input_year, AVAILABLE_PARCEL_COLOUR, unname(COLOUR_MAPPING[planting_type])),  # Grey if not planted yet or NA
              fillOpacity = FILL_OPACITY,
              group = "UnblockedPolygons",
              layerId = unclk$parcel_id,  # Reassign the same layerId for unblocked polygons
              label = ~paste("Parcel ID:", parcel_id)
            )
        }

        # Handle Unblocked Parcels (those whose blocked_until_year is less than input_year)
        if (length(unblocked_parcels) > 0) {
          unblk <- current_data[current_data$parcel_id %in% unblocked_parcels$parcel_id, ]

          # Recolor unblocked polygons and update them (reassign color based on planting_type)
          leafletProxy("map") %>%
            addPolygons(
              data = unblk,  # Data for unblocked parcels
              weight = 1,
              color = PARCEL_LINE_COLOUR,
              fillColor = ~ifelse(is.na(planting_year) | planting_year >= input_year, AVAILABLE_PARCEL_COLOUR, unname(COLOUR_MAPPING[planting_type])),  # Grey if not planted yet or NA
              fillOpacity = FILL_OPACITY,
              group = "UnblockedPolygons",
              layerId = unblk$parcel_id,  # Reassign the same layerId for unblocked polygons
              label = ~paste("Parcel ID:", parcel_id)
            )
        }
      }
    })
    # 
    # # Enable/Disable sliders
    observe({
      toggleState("carbon", input$carbon_checkbox)
      toggleState("species", input$species_checkbox)
      toggleState("species_goat_moth", input$species_goat_moth_checkbox)
      toggleState("species_stag_beetle", input$species_stag_beetle_checkbox)
      toggleState("species_lichens", input$species_lichens_checkbox)
      toggleState("area", input$area_checkbox)
      toggleState("recreation", input$recreation_checkbox)
    })
    
    # Observe the reset of the sliders
    observeEvent(input$reset_main, {
      print("resetting")
      print(reactiveValuesToList(state))
      
      # Reset map view using isolate() to avoid reactivity issues
      leafletProxy(ns("map")) %>%
        setView(lat = state$map$lat, 
                lng = state$map$lng, 
                zoom = state$map$zoom)  # Make sure to use zoom, not default
      
      # Reset sliders to default values
      updateSliderInput(session, "carbon", value = state$map_tab$slider_defaults$carbon$default)
      updateSliderInput(session, "species", value = state$map_tab$slider_defaults$species$default)
      updateSliderInput(session, "species_goat_moth", value = state$map_tab$slider_defaults$species_goat_moth$default)
      updateSliderInput(session, "species_stag_beetle", value = state$map_tab$slider_defaults$species_stag_beetle$default)
      updateSliderInput(session, "species_lichens", value = state$map_tab$slider_defaults$species_lichens$default)
      updateSliderInput(session, "area", value = state$map_tab$slider_defaults$area$default)
      updateSliderInput(session, "recreation", value = state$map_tab$slider_defaults$recreation$default)
      updateSliderInput(session, "year", value = YEAR_MIN)  # Reset year slider
      
      # Reset checkboxes to default values
      updateCheckboxInput(session, "carbon_checkbox", value = TRUE)
      updateCheckboxInput(session, "species_checkbox", value = TRUE)
      updateCheckboxInput(session, "species_goat_moth_checkbox", value = TRUE)
      updateCheckboxInput(session, "species_stag_beetle_checkbox", value = TRUE)
      updateCheckboxInput(session, "species_lichens_checkbox", value = TRUE)
      updateCheckboxInput(session, "area_checkbox", value = TRUE)
      updateCheckboxInput(session, "recreation_checkbox", value = TRUE)
    })
    
    
    # Saving, Loading, Deleting
    # Render the "Saved Strategies" accordion dynamically
    output$saved_strategies <- renderUI({
      strategies <- saved_strategies()
      
      if (length(strategies) == 0) {
        return(tagList(
          p("No strategies saved yet.")
        ))
      }
      
      # Create a list of strategies with UUID, Load, and Delete buttons
      strategy_items <- lapply(names(strategies), function(key) {
        # Create a simple line with UUID, Load button, and Delete button
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          span(key, style = "flex-grow: 1;"),  # UUID shown on the left
          actionButton(ns(paste("load_strategy", key, sep = "_")), label = NULL, icon = icon("play"), class = "btn btn-second"),
          actionButton(ns(paste("delete_strategy", key, sep = "_")), label = NULL, icon = icon("trash"), class = "btn btn-danger")
        )
      })
      
      tagList(strategy_items)
    })
    
    # Save strategy when the "Save" button is clicked
    # !TODO need to save the map stuff too!
    observeEvent(input$save_main, {
      # Save the current state (slider values)
      strategy <- list(
        saved_data = new_data(),
        clicked_polygons = clicked_polygons(),
        
        carbon = input$carbon,
        species = input$species,
        species_goat_moth = input$species_goat_moth,
        species_stag_beetle = input$species_stag_beetle,
        species_lichens = input$species_lichens,
        area = input$area,
        recreation = input$recreation,
        
        year = input$year,
        
        carbon_checkbox = input$carbon_checkbox,
        species_checkbox = input$species_checkbox,
        species_goat_moth_checkbox = input$species_goat_moth_checkbox,
        species_stag_beetle_checkbox = input$species_stag_beetle_checkbox,
        species_lichens_checkbox = input$species_lichens_checkbox,
        area_checkbox = input$area_checkbox,
        recreation_checkbox = input$recreation_checkbox
      )
      
      # Create a unique key for the new strategy using the counter
      strategy_key <- paste(UUIDgenerate())
      # Add this strategy to the saved strategies list with a unique key
      strategies <- saved_strategies()
      strategies[[strategy_key]] <- strategy
      print(strategies)
      saved_strategies(strategies)
    })
    
    observe({
      strategies <- saved_strategies()  # Reactive dependency
      print(paste("Registered strategies:", paste(names(strategies), collapse = ", ")))
      first_strategy <- names(strategies)[1]
      print(paste("Namespace for first strategy:", paste("load_strategy", first_strategy, sep = "_")))
      
      lapply(names(saved_strategies()), function(key) {
        # Dynamically handle load strategy for each strategy
        observeEvent(input[[paste("load_strategy", key, sep = "_")]], {
          strategy <- saved_strategies()[[key]]
          
          # Restore saved data
          new_data(strategy$saved_data)
          clicked_polygons(strategy$clicked_polygons)
          
          updateSliderInput(session, "carbon", value = strategy$carbon)
          updateSliderInput(session, "species", value = strategy$species)
          updateSliderInput(session, "species_goat_moth", value = strategy$species_goat_moth)
          updateSliderInput(session, "species_stag_beetle", value = strategy$species_stag_beetle)
          updateSliderInput(session, "species_lichens", value = strategy$species_lichens)
          updateSliderInput(session, "area", value = strategy$area)
          updateSliderInput(session, "recreation", value = strategy$recreation)
          
          updateCheckboxInput(session, "carbon_checkbox", value = strategy$carbon_checkbox)
          updateCheckboxInput(session, "species_checkbox", value = strategy$species_checkbox)
          updateCheckboxInput(session, "species_goat_moth_checkbox", value = strategy$species_goat_moth_checkbox)
          updateCheckboxInput(session, "species_stag_beetle_checkbox", value = strategy$species_stag_beetle_checkbox)
          updateCheckboxInput(session, "species_lichens_checkbox", value = strategy$species_lichens_checkbox)
          updateCheckboxInput(session, "area_checkbox", value = strategy$area_checkbox)
          updateCheckboxInput(session, "recreation_checkbox", value = strategy$recreation_checkbox)
          
          # Ensure the map updates with the loaded strategy
          initialize_or_update_map(YEAR_MIN, strategy$saved_data)        
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
    
    # Delete strategy when the "Delete" button is clicked
    observeEvent(saved_strategies(), {
      lapply(names(saved_strategies()), function(key) {
        observeEvent(input[[paste("delete_strategy", key, sep = "_")]], {

          strategies <- saved_strategies()
          strategies[[key]] <- NULL  # Remove the strategy from the list
          
          saved_strategies(strategies)
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
    
    
  })
}