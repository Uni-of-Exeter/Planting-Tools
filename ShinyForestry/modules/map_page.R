library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

source("utils/api_functions.R")
source("config.R")

FullTableNotAvail <- st_read(normalizePath(file.path(normalizePath(file.path(normalizePath(getwd()), "ElicitorOutput")), "FullTableNotAvail.geojson")), quiet=TRUE)

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
            uiOutput(ns("dynamic_sliders")),
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
        leafletOutput(ns("map"), height = "100%"),
        map_and_slider_ui(id = ns("map"), 2025, 2049, 2025)
      )
    )
  )
}

map_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
      # year = NULL
      # num_clicked_polygons = 0 # not sure if this is the best way, what about if polygons are blocked online... I guess that's fine.
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
    
    #----/COPYPASTA
    
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
            setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>% 
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
            column(CHECKBOX_COL, checkboxInput("carbon_checkbox", NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput("carbon", HTML(paste0("Tree Carbon Stored (tonnes of CO<sub>2</sub>):")),
                                           min = state$map_tab$slider_defaults$carbon$min, 
                                           max = state$map_tab$slider_defaults$carbon$max, 
                                           value = state$map_tab$slider_defaults$carbon$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput("species_checkbox", NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput("species", "Species Richness (All):",
                                           min = state$map_tab$slider_defaults$species$min, 
                                           max = state$map_tab$slider_defaults$species$max, 
                                           value = state$map_tab$slider_defaults$species$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput("species_goat_moth_checkbox", NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput("species_goat_moth", "Goat Moth (Presence, %):", 
                                           min = state$map_tab$slider_defaults$species_goat_moth$min, 
                                           max = state$map_tab$slider_defaults$species_goat_moth$max, 
                                           value = state$map_tab$slider_defaults$species_goat_moth$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput("species_stag_beetle_checkbox", NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput("species_stag_beetle", "Stag Beetle (Presence, %):", 
                                           min = state$map_tab$slider_defaults$species_stag_beetle$min, 
                                           max = state$map_tab$slider_defaults$species_stag_beetle$max, 
                                           value = state$map_tab$slider_defaults$species_stag_beetle$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput("species_lichens_checkbox", NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput("species_lichens", "Species Richness (Lichens):", 
                                           min = state$map_tab$slider_defaults$species_lichens$min, 
                                           max = state$map_tab$slider_defaults$species_lichens$max, 
                                           value = state$map_tab$slider_defaults$species_lichens$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput("area_checkbox", NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput("area", HTML(paste0("Area Planted (km<sup>2</sup>):")),
                                           min = state$map_tab$slider_defaults$area$min, 
                                           max = state$map_tab$slider_defaults$area$max, 
                                           value = state$map_tab$slider_defaults$area$default
            ))
          ),
          fluidRow(
            column(CHECKBOX_COL, checkboxInput("recreation_checkbox", NULL, value = TRUE)),
            column(SLIDER_COL, sliderInput("recreation", "Recreation (visits per month):", 
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
    
    
  })
}