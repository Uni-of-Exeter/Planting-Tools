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

FolderSource <- normalizePath(".")

if (dir.exists("ShinyForestry")) {
  elicitor_folder <- normalizePath(file.path("ShinyForestry", "ElicitorOutput"))
  FolderSource <- normalizePath(file.path("ShinyForestry"))
} else {
  elicitor_folder <- normalizePath(file.path("ElicitorOutput"))
  FolderSource <- normalizePath(".")
}

source(file.path(FolderSource, "config.R"))

# more --> less: debug / info / warning / error / none
MAX_LIMIT_LOG_LEVEL <- "debug"
if (file.exists(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R"), mustWork = FALSE))) {
  source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
} else {
  source(normalizePath(file.path(FolderSource, "backend", "bayesian-optimization-functions.R")), local = TRUE)
}

# If the backend was already initialized, saved to disk, and environment variable is valid, use it
run_initalization_on_backend <- FALSE
backend_initialization_env_file <- normalizePath(file.path(elicitor_folder, "backend_env.rds"))
if (file.exists(backend_initialization_env_file)) {
  
  env <- readRDS(backend_initialization_env_file)
  # Ensure file is valid
  if (isFALSE(is.list(env))) {
    notif(paste(backend_initialization_env_file, "seems to be corrupted. Deleting it."))
    file.remove(backend_initialization_env_file)
    run_initalization_on_backend <- TRUE
  }
  
} else {
  run_initalization_on_backend <- TRUE
}

if (isTRUE(run_initalization_on_backend)) {
  
  # If a file does not exist, stop everything
  filenames <- c("land_parcels.shp.zip", "decision_units.json", "outcomes.json")
  sapply(filenames, function(filename) {
    filepath <- file.path(elicitor_folder, filename)
    if (isFALSE(file.exists(filepath))) {
      stop(filename, "does not exist")
    }
  }) |> invisible()
  
  library(tools)
  
  library(curl)
  # If the files exist and have correct hashes, do not upload them.
  for (filename in filenames) {
    filepath <- normalizePath(file.path(elicitor_folder, filename))
    md5 <- tools::md5sum(filepath)
    
    notif(paste("Checking if", filename, "exists in the backend and is the same one as on this computer"), log_level = "debug")
    response <- curl_fetch_memory(url = paste0(API_URL, "/exists?filename=", filename, "&md5sum=", md5))
    
    # Get the response information
    body <- rjson::fromJSON(rawToChar(response$content))
    status <- response$status_code
    if (status %in% c(400, 404)) {
      
      filepath <- normalizePath(filepath, winslash = "/")
      result <- system(paste0('curl ',
                              '-sSL ',
                              '-X PUT ',
                              '-H "accept: */*" ',
                              # '-H "Content-Type: multipart/form-data" ',
                              # '-F "file_to_upload=@', filepath, ';type=application/x-zip-compressed" ',
                              # '-F "file_to_upload=@', filepath, ';type=application/octet-stream" ',
                              '-F "file_to_upload=@', filepath, '" ',
                              API_URL, '/upload'),
                       intern = TRUE) |>
        rjson::fromJSON()
      
      if (result == "Success") {
        msg <- paste0("Upload of ", filename, ": ", result)
        notif(msg, log_level = "debug")
      } else if (names(result) == "error") {
        msg <- paste0("Error during upload of ", filename, ": ", result$error)
        notif(msg, log_level = "error")
        stop(msg)
      }
    } else if (status == 200) {
      notif(paste(filename, "is valid"), log_level = "debug")
    }
  }
  
  # Initialize the environment
  handle_PUT <- new_handle()
  handle_setopt(handle_PUT, customrequest = "PUT")
  url <- paste0(API_URL, "/initialize?MAX_LIMIT_LOG_LEVEL=", MAX_LIMIT_LOG_LEVEL)
  msg <- paste("initializing the backend", url, "...")
  notif(msg)
  response <- curl_fetch_memory(url = url,
                                handle = handle_PUT)
  notif(paste(msg, "done"))
  if (response$status_code != 200) {
    msg <- paste("Error initializing the backend, response object=", toString(response))
    notif(msg, log_level = "error")
    stop(msg)
  }
  
  # Save the RDS response to file, then read it
  msg <- "Reading the environment from the backend ..."
  notif(msg)
  
  # temp_file <- tempfile()
  # writeBin(response$content, temp_file)
  # env <- readRDS(temp_file)
  # file.remove(temp_file)
  
  env <- plumber::parser_rds()(value = response$content)
  list2env(as.list(env), envir = .GlobalEnv)
  
  # Save it to elicitor folder
  saveRDS(env, backend_initialization_env_file)
}

fetch_api_data_post <- function(json_payload) {
  
  # url <- "http://127.0.0.1:8000/generate_parcels"
  url <- paste0(API_URL, "/generate_parcels")
  
  # Make the API POST request with JSON payload
  response <- httr::POST(
    url,
    body = json_payload, 
    encode = "json",
    httr::content_type_json()
  )
  
  print(response)
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    geojson <- api_response$geojson
    geojson_parsed <- st_read(geojson, quiet=TRUE)
    
    Sys.sleep(1)
    values <- api_response$values
    return(list(geojson_parsed, values))
    
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

fetch_slider_values <- function() {
  url <- paste0(API_URL, "/slider_values")
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    return(api_response)
  } 
  else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

# Define UI
ui <- fluidPage(
  # ui <- navbarPage(
  title = "ADD-TREES",
  useShinyjs(), # Enable JavaScript functionalities
  theme = bs_theme(version = 5, bootswatch = "lumen"), # Optional theming
  tags$head(
    # Add the Google Fonts link for Outfit font
    tags$link(
      rel = "stylesheet", 
      href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600&display=swap"
    ),
    # Add the Bootstrap Icons library to ensure the icons are available
    tags$link(
      rel = "stylesheet", 
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css"
    )
  ),
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
    
    /* Loading overlay styles */
    #loading {
      position: fixed; 
      top: 0; 
      left: 0; 
      width: 100%; 
      height: 100%; 
      background-color: rgba(0, 60, 60, 1.0);  /* Dark background */
      color: white; 
      display: flex; 
      justify-content: center; 
      align-items: center; 
      font-size: 24px; 
      font-family: 'Outfit', sans-serif; /* Apply Outfit font */
      z-index: 9999;
      opacity: 1;  /* Initially fully visible */
    }
    
    /* Spinner styles */
    .spinner {
      border: 8px solid #00c896; 
      border-top: 8px solid #003c3c; 
      border-radius: 90%;
      width: 50px;
      height: 50px;
      animation: spin 2s linear infinite;
    }
  
    /* Spinner animation */
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

  ")),
  
  # Wrap everything in app_content and loading divs
  div(id = "loading", 
      div(class = "spinner")
  ),
  
  # Main content of the page wrapped in app_content
  div(id = "app_content", 
      # style = "display: none;",  # Initially hidden\
      style = "display: block;",  # Initially hidden
      div(
        style = "display: flex; height: 100vh; flex-direction: row; width: 100%;",
        sidebarPanel(
          id = "sidebar",
          width = 3,
          
          # Alert with info icon and message
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
            id = "main_accordion",
            accordion_panel(
              "Targets",
              id = "targets_accordion",
              uiOutput("dynamic_sliders"),
              actionButton("submit", "Submit"),
              actionButton("reset", "Reset"),
              actionButton("save", "Save Strategy")
            ),
            accordion_panel(
              "Saved Strategies",
              uiOutput("saved_strategies")
            )
          )
        ),
        mainPanel(
          class = "main-panel",
          leafletOutput("map", height = "100%"),
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
            
            div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%; height: 40px;",
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
              radioGroupButtons(
                inputId = "view_toggle",
                choices = c("Annual", "Cumulative"),
                selected = "Cumulative",
                status = "primary",
                justified = TRUE,
                width = '220px'
              )
            ),
            
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
)



server <- function(input, output, session) {
  
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  map_view <- reactiveValues(lat = LAT_DEFAULT, lon = LON_DEFAULT, zoom = ZOOM_DEFAULT)
  
  # !TODO do these need to be reset?
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
  initialize_or_update_map <- function(input_year, data = NULL, json_payload = NULL) {
    # Fetch the data from the API when initializing or submitting
    # new_data_fetched <- st_read(fetch_api_data())  # Hit the API and get the data
    
    new_fetched <- if (!is.null(data)) {
      list(data, NULL)
    } else {
      fetch_api_data_post(json_payload)  # Use POST if json_payload is provided (this is just a placeholder)
    }
    
    new_data_fetched <- new_fetched[[1]]
    new_values_fetched <- new_fetched[[2]]
    
    print(new_values_fetched)
    print(new_data_fetched)
    
    if (!is.null(new_data_fetched)) {
      # Apply the filter based on the selected year
      new_data(new_data_fetched)
      new_vals(new_values_fetched)
      
      stopifnot(all(sort(names(new_values_fetched)) == sort(names(SLIDER_NAMES))))
      
      print(new_data_fetched)
      
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
  
  # Initialisation Block
  observe({
    default_values <- fetch_slider_values()  # Fetch once at startup
    default_payload <<- list(
      carbon = default_values$carbon$default,
      species = default_values$species$default,
      species_goat_moth = default_values$species_goat_moth$default,
      species_stag_beetle = default_values$species_stag_beetle$default,
      species_lichens = default_values$species_lichens$default,
      area = default_values$area$default,
      recreation = default_values$recreation$default,
      blocked_parcels = list()
    )
    
    initial_values(
      list(
        carbon = default_values$carbon$default,
        species = default_values$species$default,
        species_goat_moth = default_values$species_goat_moth$default,
        species_stag_beetle = default_values$species_stag_beetle$default,
        species_lichens = default_values$species_lichens$default,
        area = default_values$area$default,
        recreation = default_values$recreation$default
      )
    )
    
    # Render UI for sliders dynamically
    # This also needs to use the mapping in the config.R!
    output$dynamic_sliders <- renderUI({
      tagList(
        fluidRow(
          column(CHECKBOX_COL, checkboxInput("carbon_checkbox", NULL, value = TRUE)),
          column(SLIDER_COL, sliderInput("carbon", HTML(paste0("Tree Carbon Stored (tonnes of CO<sub>2</sub>):")),
                                         min = default_values$carbon$min, 
                                         max = default_values$carbon$max, 
                                         value = default_values$carbon$default
          ))
        ),
        fluidRow(
          column(CHECKBOX_COL, checkboxInput("species_checkbox", NULL, value = TRUE)),
          column(SLIDER_COL, sliderInput("species", "Species Richness (All):",
                                         min = default_values$species$min, 
                                         max = default_values$species$max, 
                                         value = default_values$species$default
          ))
        ),
        fluidRow(
          column(CHECKBOX_COL, checkboxInput("species_goat_moth_checkbox", NULL, value = TRUE)),
          column(SLIDER_COL, sliderInput("species_goat_moth", "Goat Moth (Presence, %):", 
                                         min = default_values$species_goat_moth$min, 
                                         max = default_values$species_goat_moth$max, 
                                         value = default_values$species_goat_moth$default
          ))
        ),
        fluidRow(
          column(CHECKBOX_COL, checkboxInput("species_stag_beetle_checkbox", NULL, value = TRUE)),
          column(SLIDER_COL, sliderInput("species_stag_beetle", "Stag Beetle (Presence, %):", 
                                         min = default_values$species_stag_beetle$min, 
                                         max = default_values$species_stag_beetle$max, 
                                         value = default_values$species_stag_beetle$default
          ))
        ),
        fluidRow(
          column(CHECKBOX_COL, checkboxInput("species_lichens_checkbox", NULL, value = TRUE)),
          column(SLIDER_COL, sliderInput("species_lichens", "Species Richness (Lichens):", 
                                         min = default_values$species_lichens$min, 
                                         max = default_values$species_lichens$max, 
                                         value = default_values$species_lichens$default
          ))
        ),
        fluidRow(
          column(CHECKBOX_COL, checkboxInput("area_checkbox", NULL, value = TRUE)),
          column(SLIDER_COL, sliderInput("area", HTML(paste0("Area Planted (km<sup>2</sup>):")),
                                         min = default_values$area$min, 
                                         max = default_values$area$max, 
                                         value = default_values$area$default
          ))
        ),
        fluidRow(
          column(CHECKBOX_COL, checkboxInput("recreation_checkbox", NULL, value = TRUE)),
          column(SLIDER_COL, sliderInput("recreation", "Recreation (visits per month):", 
                                         min = default_values$recreation$min, 
                                         max = default_values$recreation$max, 
                                         value = default_values$recreation$default
          ))
        )
      )
    })
    
    # Initialize the map on app start
    default_json_payload <- jsonlite::toJSON(default_payload, auto_unbox = TRUE, pretty = TRUE)
    print(default_json_payload)
    initialize_or_update_map(YEAR_MIN, json_payload = default_json_payload)
    rv$setupComplete <- TRUE
  })
  
  # Once setup is complete, hide the loading screen and show the main content
  observe({
    if (rv$setupComplete) {
      print("setup complete")
      shinyjs::show(id = "app_content")  # Show the main app content
      Sys.sleep(3)
      shinyjs::runjs('$("#loading").fadeOut(2000);')
      # shinyjs::hide(id = "loading")  # Hide the loading screen
      # shinyjs::show(id = "app_content")  # Show the main app content
    }
  })
  # A reactive expression to track the current year
  current_year <- reactive({
    input$year
  })
  
  # Handle submit event to update the map
  observeEvent(input$submit, {
    
    shinyjs::disable("save")
    shinyjs::disable("reset")
    shinyjs::disable("submit")
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
    shinyjs::enable("save")
    shinyjs::enable("reset")
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
  
  # Not Tested
  # Implement clicking off
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
      recreation = input$recreation
      # year = input$year
      # num_clicked_polygons = nrow(clicked_polygons()[clicked_polygons()$blocked_until_year != 0, ]) # not sure if this is how to do it 
    )
    
    # Compare current values with initial values
    # In your server code:
    values_changed <- !identical(current_values, initial_values())
    current_clicked <- clicked_polygons()
    
    # Check if values have changed or if there are any blocked parcels
    # In your server code:
    values_changed <- !identical(current_values, initial_values())
    current_clicked <- clicked_polygons()
    current_clicked_in <- clicked_polygons_injest()
    
    # Check if values have changed or if there are any blocked parcels
    if (values_changed || (nrow(setdiff(current_clicked_in, current_clicked)) != 0)) { # !TODO this is broken now that I keep the clicked stuff
      # Disable Save button as changes require submission
      shinyjs::disable("save")  # Disable the save button
      
      # Enable and highlight the Submit button (green)
      shinyjs::enable("submit")  # Enable the submit button
      # shinyjs::removeClass("submit", "ben-secondary")  # Remove the gray/disabled class
      shinyjs::addClass("submit", "btn-success")  # Add the green/active class
      
    } else {
      # Enable Save button if no changes have been made
      shinyjs::enable("save")  # Enable the save button
      
      # Disable Submit button if no changes have been made
      shinyjs::disable("submit")  # Disable the submit button
      shinyjs::removeClass("submit", "btn-success")  # Remove the green/active class
      shinyjs::addClass("submit", "btn-secondary")  # Add the gray/disabled class
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