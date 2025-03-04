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

setwd("/Users/paulwright/Documents/work/ADD-TREES/Planting-Tools/")

source("ShinyForestry/config.R")

FullTableNotAvail <- st_read(normalizePath(file.path(normalizePath(file.path(normalizePath(getwd()), "ShinyForestry/ElicitorOutput")), "FullTableNotAvail.geojson")), quiet=TRUE)


fetch_api_data <- function() {
  
  url <- paste0("http://127.0.0.1:8001/generate_parcels")
  # Make the API request
  response <- httr::GET(url)
  # Check if the response is successful
  if (httr::status_code(response) == 200) {

    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    geojson <- jsonlite::fromJSON(content_raw)
    geojson_parsed <- st_read(geojson, quiet=TRUE)
    
    return(geojson_parsed)
    } 
  else {
    stop(paste("Request failed with status:", httr::status_code(response)))
    }
}

# Function to fetch slider data
fetch_slider_values <- function() {
  url <- "http://127.0.0.1:8001/slider_values"
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    slider_data <- jsonlite::fromJSON(content_raw)
    
    return(slider_data)
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}
    
# Define UI
ui <- fluidPage(
  # ui <- navbarPage(
  title = "ADD-TREES",
  useShinyjs(), # Enable JavaScript functionalities
  theme = bs_theme(bootswatch = "lumen"), # Optional theming
  tags$style(HTML("
    body {
      margin: 0;
      padding: 0;
    }
    .container-fluid {
      padding: 0;
    }
    #sidebar {
      overflow-y: auto;  /* Make sidebar scrollable if content overflows */
      height: 100%;  /* Ensure the sidebar doesn't grow larger than the viewport */
      border-radius: 0px; /* delete for nice curved boxes */
    }
    #accordion-panel {
      max-height: 80vh;  /* Set maximum height for the accordion */
      overflow-y: auto;  /* Allow scrolling if the content exceeds max-height */
    }
    .main-panel {
      display: flex;
      flex-grow: 1;
      height: 100%;
      position: relative;  /* Required to position year slider */
      /* padding: 20px; */
      background-color: #f0f0f0;
    }
    .leaflet-container {
      width: 100%;
      height: 100%;
      position: relative;  /* Required to position year slider */
    }
    .leaflet-control{
      background: rgba(255, 255, 255, 0.9); /* background-colour to zero */
      box-shadow: none !important; /* remove box-shadow */
      border: none !important; /* remove border */
    }
    
    /* Make the Plotly mode bar fully transparent */
    /* target individual button containers */
    .js-plotly-plot .modebar > div {
        background: none !important;
        opacity: 1 !important;
    }

  ")),
  
  div(
    style = "display: flex; height: 100vh; flex-direction: row; width: 100%;",
    sidebarPanel(
      id = "sidebar",
      width = 3,
      div(
        style = "padding: 20px; background-color: #f0f0f0; border-radius: 8px;
                 box-shadow: 0px 4px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
        h3(HTML("<strong>ADD-TREES</strong>")),
        p("Use the checkboxes and sliders to enable/disable targets and adjust their values.")
      ),
      accordion(
        id = "main_accordion",
        accordion_panel(
          "Targets",
          id = "targets_accordion",
          tagList(
            fluidRow(
              column(CHECKBOX_COL, checkboxInput("carbon_checkbox", NULL, value = TRUE)),
              column(SLIDER_COL, sliderInput("carbon", "Tree carbon stored (tonnes of CO2):", min = CARBON_MIN, max = CARBON_MAX, value = CARBON_DEFAULT))
            ),
            fluidRow(
              column(CHECKBOX_COL, checkboxInput("species_checkbox", NULL, value = TRUE)),
              column(SLIDER_COL, sliderInput("species", "Species richness (All):", min = 0, max = 25, value = SPECIES_DEFAULT))
            ),
            fluidRow(
              column(CHECKBOX_COL, checkboxInput("species_goat_moth_checkbox", NULL, value = TRUE)),
              column(SLIDER_COL, sliderInput("species_goat_moth", "Goat Moth (Presence, %):", min = 0, max = 100, value = SPECIES_GM_DEFAULT))
            ),
            fluidRow(
              column(CHECKBOX_COL, checkboxInput("species_stag_beetle_checkbox", NULL, value = TRUE)),
              column(SLIDER_COL, sliderInput("species_stag_beetle", "Stag Beetle (Presence, %):", min = 0, max = 100, value = SPECIES_SB_DEFAULT))
            ),
            fluidRow(
              column(CHECKBOX_COL, checkboxInput("species_lichens_checkbox", NULL, value = TRUE)),
              column(SLIDER_COL, sliderInput("species_lichens", "Species Richness (Lichens):", min = 0, max = 5, value = SPECIES_LICHENS_DEFAULT))
            ),
            fluidRow(
              column(CHECKBOX_COL, checkboxInput("area_checkbox", NULL, value = TRUE)),
              column(SLIDER_COL, sliderInput("area", "Area planted (km²):", min = 0, max = 15, value = AREA_DEFAULT))
            ),
            fluidRow(
              column(CHECKBOX_COL, checkboxInput("recreation_checkbox", NULL, value = TRUE)),
              column(SLIDER_COL, sliderInput("recreation", "Recreation (visits per month):", min = 0, max = 20, value = RECREATION_DEFAULT))
            )
          ),
          actionButton("submit", "Submit"),
          actionButton("reset", "Reset"),
          actionButton("save", "Save Strategy")
        ),
        # accordion_panel(
        #   "Statutory Metrics",
        #   tagList(
        #     p("Empty.")
        #   )
        # ),
        # accordion_panel(
        #   "Outcomes",
        #   tagList(
        #     p("Empty.")
        #   )
        # ),
        # accordion(
        #   accordion_panel(
        #     "Radar Chart",
        #     plotlyOutput("radarPlot")
        #   )
        # ),
        accordion_panel(
          "Saved Strategies",
          uiOutput("saved_strategies")
        )
        # accordion_panel(
        #   "Dynamic Sliders",
        #   uiOutput("dynamic_sliders")  # Placeholder for dynamic sliders
        # )
      )
    ),
    mainPanel(
      class = "main-panel",
      leafletOutput("map", height = "100%"),
      # Absolute Panel for the year-slider, inside the map container
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
                    width = "100%" # This will make the slider take 100% width of the container
        ),
        bottom = "50px", 
        left = "50%", 
        style = "background-color: rgba(255, 255, 255, 0.9); padding: 10px 20px 10px 20px; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.2); width: 50%; transform: translateX(-50%);",
        
        # div(
        #   style = "display: flex; justify-content: center; width: 100%;",
        #   radioGroupButtons(
        #     inputId = "view_toggle",
        #     choices = c("Annual", "Cumulative"),
        #     selected = "Cumulative",
        #     status = "primary",
        #     justified = TRUE, # Makes them equal width
        #     width = '220px'
        #   )
        # ),
        # 
        # # Using an accordion-style panel here, like the sidebar
        # accordion(
        #   open = FALSE,
        #   accordion_panel(
        #     "Time-Series",
        #     multiple = FALSE,
        #     tagList(
        #       plotlyOutput("areaPlot", height = "250px")
        #     ),
        #     style = "border: none;",  # Remove border around the entire accordion header
        #   )
        # )
        
        div(
          style = "display: flex; justify-content: space-between; align-items: center; width: 100%; height: 40px;",
          
          # Left-Aligned Time-Series Toggle Button
          actionButton(
            inputId = "toggle_plot",
            label = "Show Time-Series",
            style = "
                width: 180px;
                height: 40px;
                line-height: 20px;
                display: flex;
                align-items: center;
                justify-content: center;
                margin-top: -17px;"
          ),
          
          # actionButton(
          #   inputId = "toggle_radar_plot",
          #   label = "Show Outcomes",
          #   style = "
          #       width: 160px; 
          #       height: 40px; 
          #       line-height: 20px; 
          #       display: flex; 
          #       align-items: center; 
          #       justify-content: center;
          #       margin-top: -17px;"
          # ),
          # Right-Aligned Annual/Cumulative Toggle
          radioGroupButtons(
            inputId = "view_toggle",
            choices = c("Annual", "Cumulative"),
            selected = "Cumulative",
            status = "primary",
            justified = TRUE,
            width = '220px'
          )
        ),
        
        # Time-Series Plot (Initially hidden, shown when toggled)... Plotly sometimes has an issue where it's overflowing
        div(id = "time_series_plot", style = "display: none",
            plotlyOutput("areaPlot", height = "300px")
        ),
        div(id = "radar_plot", style = "display: none",
            plotlyOutput("radarPlot", height = "300px")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  map_view <- reactiveValues(lat = LAT_DEFAULT, lon = LON_DEFAULT, zoom = ZOOM_DEFAULT)
  
  # slider_data <- reactiveVal(NULL)
  # 
  # user_inputs <- reactiveValues(sliders = list(), checkboxes = list())
  # 
  # slider_data <- fetch_slider_values()
  # 
  # # Initialize storage with default values
  # for (slider_name in names(slider_data)) {
  #   user_inputs$sliders[[slider_name]] <- (slider_data[[slider_name]]$min + slider_data[[slider_name]]$max) / 2  # Default slider position
  #   user_inputs$checkboxes[[slider_name]] <- TRUE  # Default checkbox state
  # }
  
  # output$dynamic_controls <- renderUI({
  #   data <- slider_data()
  #   if (is.null(data)) return(NULL)
  #   
  #   controls_list <- lapply(names(data), function(slider_name) {
  #     fluidRow(
  #       column(2, checkboxInput(
  #         inputId = paste0(slider_name, "_checkbox"),
  #         label = NULL,
  #         value = user_inputs$checkboxes[[slider_name]]
  #       )),
  #       column(10, sliderInput(
  #         inputId = slider_name,
  #         label = paste(slider_name, ":", data[[slider_name]]$min, "-", data[[slider_name]]$max),
  #         min = data[[slider_name]]$min,
  #         max = data[[slider_name]]$max,
  #         value = user_inputs$sliders[[slider_name]]
  #       ))
  #     )
  #   })
  #   
  #   do.call(tagList, controls_list)
  # })
  
  # !TODO do these need to be reset?
  new_data <- reactiveVal(NULL) # most recent data
  filtered_data <- reactiveVal(NULL) # data filtered by year
  panel_expanded <- reactiveVal(FALSE)
  output_data <- reactiveVal(NULL)
  
  current_layers <- reactiveVal(list()) # Keep track of layers currently on the map (filtered ones)
  # clicked_polygons <- reactiveVal(list()) # Reactive value to store clicked polygons
  clicked_polygons <- reactiveVal(data.frame(
    parcel_id = character(),  # Empty initially
    blocked_until_year = numeric(),
    stringsAsFactors = FALSE
  ))
  
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
    recreation = NULL,
    year = NULL
  ))
  
  # hacky JS to make sure plotly plot doesn't bug out
  observe({
    # Trigger plot layout adjustment when the plot is shown
    shinyjs::runjs("
    // Listen for visibility change or toggle button click
    $('#time_series_plot').on('shown.bs.collapse', function() {
      setTimeout(function() {
        Plotly.relayout('areaPlot', { width: $('#time_series_plot').width() });
      }, 100);  // Add small delay to ensure layout changes
    });

    // Optionally, adjust layout when switching between Annual and Cumulative
    $('#view_toggle').on('change', function() {
      setTimeout(function() {
        Plotly.relayout('areaPlot', { width: $('#time_series_plot').width() });
      }, 100);
    });
    
    // Handle window resizing
    $(window).resize(function() {
      setTimeout(function() {
        // Trigger plot resizing only if the plot is visible
        if ($('#time_series_plot').is(':visible')) {
          Plotly.relayout('areaPlot', { width: $('#time_series_plot').width() });
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
  
  output$radarPlot <- renderPlotly({
    data_values <- data.frame(
      Category = c("Carbon", "Species", "Goat Moth", "Stag Beetle", "Lichens", "Area", "Recreation"),
      Target = c(100, 100, 100, 100, 100, 100, 100),
      Actual = c(80, 77, 97, 74, 99, 104, 102) # mocked data
    )
      
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = "markers"  # Changed to lines+markers for better visual representation
    ) %>%
      add_trace(
        r = data_values$Target,
        theta = data_values$Category,
        name = "Target",
        fillcolor = 'rgba(0, 0, 0, 0.1)'  # Light fill color for the target
      ) %>%
      add_trace(
        r = data_values$Actual,
        theta = data_values$Category,
        name = "Actual",
        fillcolor = 'rgba(255, 0, 0, 0.2)'  # Red fill for actual
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 120), showgrid = TRUE),
          angularaxis = list(rotation = 270), # Keeps the rotation
          bgcolor = "rgba(255, 255, 255, 0)"  # Set transparent background for the polar chart itself
        ),
        showlegend = TRUE,
        # Set transparent background
        paper_bgcolor = "rgba(255, 255, 255, 0)",  # Transparent background for the entire plot area
        plot_bgcolor = "rgba(255, 255, 255, 0)"   # Transparent background for the plot area itself
      )
    
    fig
    })
    
  
  
  # Render time-series plot for both Conifer and Deciduous
  output$areaPlot <- renderPlotly({
    data <- processed_data()  # Get precomputed data
    
    # If no data, return NULL
    if (is.null(data)) return(NULL)
    
    # Determine which dataset to use based on user selection
    selected_plot <- if (input$view_toggle == "Cumulative") {
      data$cumulative %>% rename(y_value = cumulative_area)  # Rename for consistency
    } else {
      data$total %>% rename(y_value = total_area)
    }
    
    # Create the plot
    p <- ggplot(selected_plot, aes(x = planting_year, y = y_value, color = planting_type)) +
      geom_line(size = 1, alpha = FILL_OPACITY) +
      geom_point(size = 1.2) +
      scale_color_manual(values = COLOUR_MAPPING) +
      labs(title = ifelse(input$view_toggle == "Cumulative",
                          " ",
                          " "),
           x = "Year",
           y = ifelse(input$view_toggle == "Cumulative",
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

  
  observeEvent(input$toggle_plot, {
    shinyjs::toggle(id = "time_series_plot", anim = TRUE)

    # Change button text dynamically
    new_label <- if (input$toggle_plot %% 2 == 1) {
      # "📉 Hide Time-Series"
      "Hide Time-Series"
    } else {
      # "📈 Show Time-Series"
      "Show Time-Series"
    }

    updateActionButton(session, "toggle_plot", label = new_label)
  })
  
  observeEvent(input$toggle_radar_plot, {
    shinyjs::toggle(id = "radar_plot", anim = TRUE)
    # Change button text dynamically
    new_label <- if (input$toggle_radar_plot %% 2 == 1) {
      "Hide Outcomes"
    } else {
      "Show Outcomes"
    }
    
    updateActionButton(session, "toggle_radar_plot", label = new_label)
  })
  
  # Initialize leaflet map or update it on submit
  initialize_or_update_map <- function(input_year, data = NULL) {
    # Fetch the data from the API when initializing or submitting
    # new_data_fetched <- st_read(fetch_api_data())  # Hit the API and get the data

    new_data_fetched <- if (!is.null(data)) data else fetch_api_data()
    
    if (!is.null(new_data_fetched)) {
      # Apply the filter based on the selected year
      new_data(new_data_fetched)
      
      # Initialize clicked_polygons() with all parcel_ids and a default blocked_until_year
      clicked_polygons(data.frame(
        parcel_id = new_data_fetched$parcel_id,  
        blocked_until_year = 0,  
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
  
  # A reactive expression to track the current year
  current_year <- reactive({
    input$year
  })
  
  # Initialize the map on app start
  initialize_or_update_map(YEAR_MIN)
  
  # Handle submit event to update the map
  observeEvent(input$submit, {
    # Save the initial values when the submit button is clicked
    initial_values(list(
      carbon = input$carbon,
      species = input$species,
      species_goat_moth = input$species_goat_moth,
      species_stag_beetle = input$species_stag_beetle,
      species_lichens = input$species_lichens,
      area = input$area,
      recreation = input$recreation,
      year = input$year
    ))
    
    # Enable the save button when submit is pressed
    shinyjs::enable("save")
    
    # Update the map by calling the same function
    initialize_or_update_map(current_year())
    
    # target = as.numeric(unlist(initial_values()))[1:7]
    
    # output$radarPlot <- renderPlotly({
    #   data_values <- data.frame(
    #     Category = c("Carbon", "Species", "Goat Moth", "Stag Beetle", "Lichens", "Area", "Recreation"),
    #     Target = (target/target)*100.,
    #     Actual = ((as.numeric(unlist(initial_values()))[1:7] * runif(7, 0.8, 1.2))/target)*100. # mocked data
    #   )
    #   
    #   accordion_panel_close(id = "main_accordion", values = "Targets")
    #   
    #   fig <- plot_ly(
    #     type = 'scatterpolar',
    #     fill = 'toself',
    #     mode = "markers"
    #   ) %>%
    #     add_trace(
    #       r = data_values$Target,
    #       theta = data_values$Category,
    #       name = "Target",
    #       fillcolor = 'rgba(0, 0, 0, 0.1)'
    #     ) %>%
    #     add_trace(
    #       r = data_values$Actual,
    #       theta = data_values$Category,
    #       name = "Actual",
    #       fillcolor = 'rgba(255, 0, 0, 0.2)'
    #     ) %>%
    #     layout(
    #       polar = list(
    #         radialaxis = list(visible = TRUE, range = c(0, 120), showgrid = TRUE),
    #         angularaxis = list(rotation = 0) # Rotates start position for better readability
    #       ),
    #       showlegend = TRUE
    #     )
    #   
    #   fig
    # })
    
  })
  
  # Not Tested
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    clicked_parcel_id <- click$id  # Get clicked parcel_id
    selected_year <- input$year    # Get selected year
    
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
    
    # Check safely if the parcel exists in the list
    parcel_exists <- any(current_clicked$parcel_id == clicked_parcel_id, na.rm = TRUE)
    
    if (parcel_exists) {
      # Update the `blocked_until_year` for the clicked parcel
      current_clicked$blocked_until_year[current_clicked$parcel_id == clicked_parcel_id] <- selected_year
    } else {
      print("Clicked parcel is not in the list.")  # Debugging message
      return()
    }
    
    # Update reactive value
    clicked_polygons(current_clicked)
    
    print(paste("Updated Parcel:", clicked_parcel_id, "Blocked Until:", selected_year))
  })
  
  

  
  # Monitor changes to the sliders
  observe({
    # Get the current slider values
    current_values <- list(
      carbon = input$carbon,
      species = input$species,
      species_goat_moth = input$species_goat_moth,
      species_stag_beetle = input$species_stag_beetle,
      species_lichens = input$species_lichens,
      area = input$area,
      recreation = input$recreation,
      year = input$year
    )
    
    # Compare current values with initial values
    values_changed <- !identical(current_values, initial_values())
    
    # Disable the save button if values have changed
    if (values_changed) {
      shinyjs::disable("save")
    } else {
      shinyjs::enable("save")
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
    input_year <- input$year 
    selected_view <- input$view_toggle
    
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
      
      # Now handle Blocked Parcels (those that are blocked until input_year)
      if (length(blocked_parcels) > 0) {
        blk <- current_data[current_data$parcel_id %in% blocked_parcels$parcel_id, ]
        
        # Add blocked polygons with a distinct style (red color for blocked)
        leafletProxy("map") %>%
          addPolygons(
            data = blk,  # Data for blocked parcels
            weight = 1,
            color = 'black',
            fillColor = 'red',  # Indicating blocked parcels
            fillOpacity = FILL_OPACITY,
            group = "BlockedPolygons",
            layerId = blk$parcel_id,  # Reassign the same layerId for blocked polygons
            label = ~paste("Parcel ID:", parcel_id, "Blocked Until:", blocked_parcels$blocked_until_year[blocked_parcels$parcel_id == parcel_id])
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
  
  
  # Enable/Disable sliders
  observe({
    toggleState("carbon", input$carbon_checkbox)
    toggleState("species", input$species_checkbox)
    toggleState("species_goat_moth", input$species_goat_moth_checkbox)
    toggleState("species_stag_beetle", input$species_stag_beetle_checkbox)
    toggleState("species_lichens", input$species_lichens_checkbox)
    toggleState("area", input$area_checkbox)
    toggleState("recreation", input$recreation_checkbox)
  })
  
  # Render the "Saved Strategies" accordion dynamically
  output$saved_strategies <- renderUI({
    strategies <- saved_strategies()
    
    if (length(strategies) == 0) {
      return(tagList(
        # h4("Saved Strategies"),
        p("No strategies saved yet.")
      ))
    }
    
    # Create a list of strategies with UUID, Load, and Delete buttons
    strategy_items <- lapply(names(strategies), function(key) {
      # Create a simple line with UUID, Load button, and Delete button
      div(
        style = "display: flex; align-items: center; margin-bottom: 10px;",
        span(key, style = "flex-grow: 1;"),  # UUID shown on the left
        actionButton(paste("load_strategy", key, sep = "_"), icon("play")),  # Load button with spinner
        actionButton(paste("delete_strategy", key, sep = "_"), icon("trash"))  # Delete button
      )
    })
    
    tagList(strategy_items)
    
  })
  
  # Save strategy when the "Save" button is clicked
  # !TODO need to save the map stuff too!
  observeEvent(input$save, {
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
      
      year = input$year,  # Save the year as well
      
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
    saved_strategies(strategies)
  })
  
  # Load strategy values into sliders when the "Load" button is clicked
  # !TODO need to load the JSON in too
  observe({
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
        initialize_or_update_map(strategy$year, strategy$saved_data)        
      })
    })
  })
  
  # Observe the reset of the sliders
  observeEvent(input$reset, {

    leafletProxy("map") %>%
      setView(lat = LAT_DEFAULT, lng = LON_DEFAULT, zoom = ZOOM_DEFAULT)
    
    # Reset sliders to default values
    updateSliderInput(session, "carbon", value = CARBON_DEFAULT)
    updateSliderInput(session, "species", value = SPECIES_DEFAULT)
    updateSliderInput(session, "species_goat_moth", value = SPECIES_GM_DEFAULT)
    updateSliderInput(session, "species_stag_beetle", value = SPECIES_SB_DEFAULT)
    updateSliderInput(session, "species_lichens", value = SPECIES_LICHENS_DEFAULT)
    updateSliderInput(session, "area", value = AREA_DEFAULT)
    updateSliderInput(session, "recreation", value = RECREATION_DEFAULT)
    updateSliderInput(session, "year", value = YEAR_DEFAULT)  # Reset year slider
    
    # Reset checkboxes to default values
    updateCheckboxInput(session, "carbon_checkbox", value = TRUE)
    updateCheckboxInput(session, "species_checkbox", value = TRUE)
    updateCheckboxInput(session, "species_goat_moth_checkbox", value = TRUE)
    updateCheckboxInput(session, "species_stag_beetle_checkbox", value = TRUE)
    updateCheckboxInput(session, "species_lichens_checkbox", value = TRUE)
    updateCheckboxInput(session, "area_checkbox", value = TRUE)
    updateCheckboxInput(session, "recreation_checkbox", value = TRUE)
  })
  
  # Delete strategy when the "Delete" button is clicked
  observe({
    lapply(names(saved_strategies()), function(key) {
      # Dynamically handle delete strategy for each strategy
      observeEvent(input[[paste("delete_strategy", key, sep = "_")]], {
        strategies <- saved_strategies()
        
        # Remove the strategy by key (acting like hashmap deletion)
        strategies[[key]] <- NULL  # Remove the strategy with the key
        
        # Update the list with the new strategies
        saved_strategies(strategies)
      })
    })
  })

}

# !TODO Save strategy on first load
#       Proper reloading of the map on submit.
#         Resets everything... maybe we just want to update the polygons like add/remove?

shinyApp(ui, server)
