library(shiny)
library(leaflet)
library(bslib)  # For Bootstrap-based accordion UI
library(shinyjs) # For sidebar toggling
library(uuid)

source("config.R")

FolderSource <- normalizePath(getwd())
ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
CalculatedFilesFolder<-normalizePath(file.path(FolderSource, "CalculatedFiles"))

print(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson ..."))))
FullTable <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
load(normalizePath(file.path(CalculatedFilesFolder, "simul636YearType.RData")))
print(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))

print(dim(simul636YearType[[2]]))
print(dim(simul636YearType[[1]]))
print(simul636YearType[["YEAR"]][2])
print(names(simul636YearType[[1]])) # this is null so it's a matrix not a dataframe.

# Data Structure
# simul636YearType is a list containing 2 elements: YEAR and TYPE.
# simul636YearType[[1]] is a numeric matrix of dimensions (2000, 403), with values ranging from -1 to positive integers.
# simul636YearType[[2]] is a character matrix of dimensions (2000, 403), with categories like "Conifers," "Deciduous," and "NoPlanting," likely indicating different types of trees or planting states.

load("CalculatedFiles/SubsetMeetTargets.RData")


# I need to know what table does what, what does it look like
# what does a result look like. If I give some sliders, I want a result...
print(SubsetMeetTargets[[1]][25, ])
print(SubsetMeetTargets[[2]][25, ])
print(dim(SubsetMeetTargets[[2]]))
print(dim(SubsetMeetTargets[[1]]))
print(SubsetMeetTargets[["YEAR"]][2])
print(names(SubsetMeetTargets[[1]])) # this is null so it's a matrix not a dataframe.

library(sf)

generate_parcel_data <- function(FullTable) {
  # Ensure FullTable is an sf object
  if (!inherits(FullTable, "sf")) {
    stop("FullTable must be an sf object.")
  }
  
  # Get the number of rows in FullTable (this will be used as 'n')
  n <- nrow(FullTable)
  
  # Generate parcel_ids as a sequence of integers starting from 1
  parcel_ids <- sapply(1:n, function(x) UUIDgenerate())
  
  # Extract the geometry from FullTable
  geometries <- st_geometry(FullTable)  # Get all geometries from FullTable
  
  # Random planting years (for example, from 2025 to 2049)
  planting_years <- sample(2025:2049, n, replace = TRUE)
  
  # Random planting types ("Deciduous", "Conifer", "None")
  planting_types <- sample(c("Deciduous", "Conifer", "None"), n, replace = TRUE)
  
  # Random 'is_blocked' status (TRUE or FALSE)
  is_blocked <- sample(c(TRUE, FALSE), n, replace = TRUE)
  
  # Combine into an sf object
  parcel_data <- st_sf(
    parcel_id = parcel_ids,             # Unique ID for each parcel
    geometry = geometries,              # Geometry from FullTable
    planting_year = planting_years,     # Year of planting
    planting_type = planting_types,     # Type of planting
    is_blocked = is_blocked,            # Whether it's blocked
    crs = st_crs(FullTable)              # Use CRS from FullTable
  )
  
  return(parcel_data)
}

# Example usage:
# Assuming FullTable is already an sf object
parcel_data <- generate_parcel_data(FullTable)

# View the generated parcel data
print(parcel_data)


# Define UI
ui <- fluidPage(
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
  ")),
  
  div(
    style = "display: flex; height: 100vh; flex-direction: row; width: 100%;",
    sidebarPanel(
      id = "sidebar",
      width = 3,
      div(
        style = "padding: 20px; background-color: #f0f0f0; border-radius: 8px;
                 box-shadow: 0px 4px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
        h3(HTML("<strong>ADD-TREES</strong>: "), 
           "🌱"),
        p("Use the checkboxes and sliders to enable/disable targets and adjust their values.")
      ),
      accordion(
        accordion_panel(
          "🎯 Targets",
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
            ),
          ),
          actionButton("submit", "Submit"),
          actionButton("reset", "Reset"),
          actionButton("save", "Save in session")
        ),
        accordion_panel(
          "💾 Saved Strategies",
          uiOutput("saved_strategies")
        )
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
                    ticks = TRUE,  # Disable ticks to avoid formatting issues
                    animateOptions(interval = 100, loop = FALSE),
                    sep = "",
                    width = "100%"
        ),
        bottom = "50px", 
        left = "50%", 
        style = "background-color: rgba(255, 255, 255, 0.9); padding: 10px 20px 10px 20px; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.2); width: 50%; transform: translateX(-50%);"
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  
  # Reactive value to store clicked polygons
  clicked_polygons <- reactiveVal(list())
  
  # Store saved strategies as a named list (acting as a hashmap)
  saved_strategies <- reactiveVal(list())
  strategy_counter <- reactiveVal(1)  # Counter to keep track of strategy keys
  
  # Reactive expression to filter FullTable by the selected year
  filtered_data <- reactive({
    # Filter the parcel_data based on the selected year
    parcel_data_filtered <- parcel_data[parcel_data$planting_year <= input$year, ]
    return(parcel_data_filtered)  # Ensure you return the filtered data
  })
  
  # Initialize leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%  # Add the base map tiles once
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT) %>% 
      addPolygons(
        data = parcel_data,  # Base parcel data (static)
        weight = 1,
        color = "#000000",
        fillColor = "#dddde2",
        fillOpacity = 0.8,
        group = "parcelPolygons",  # Group the polygons
        layerId = parcel_data$parcel_id  # Set unique IDs for each polygon
      ) %>% 
      addPolygons(
        data = FullTableNotAvail,  # Unavailable polygons
        weight = 1,
        color = "#000000",
        fillColor = "#808080",
        fillOpacity = 0.8,
        group = "unavailablePolygons"
      )
  })

  # Keep track of layers currently on the map (filtered ones)
  current_layers <- reactiveVal(list())  # List of current layer IDs
  
  observeEvent(input$map_shape_click, {
    clicked_id <- input$map_shape_click$id
    print(paste("User clicked on parcel:", clicked_id))
  })
  
  observe({
    fdata <- filtered_data()  # Reactive filtering
    
    # Get the current IDs of the filtered polygons
    current_ids <- unique(fdata$parcel_id)
    
    # Get the current state of the layers (those that are already on the map)
    existing_layers <- current_layers()
    
    # Find the IDs that need to be added and removed
    to_add <- setdiff(current_ids, existing_layers)
    to_remove <- setdiff(existing_layers, current_ids)  # Ensure this is a vector of IDs
    
    # Remove polygons that are no longer in the filtered data
    # if (length(to_remove) > 0) {
    #     leafletProxy("map") %>% removeShape(layerId = to_remove)
    #   }
    # 
    # try update polygons back with a new style
    if (length(to_remove) > 0) {
      # Get the data for all parcels that need to be recolored from FullTable
      updated_data <- parcel_data[parcel_data$parcel_id %in% to_remove, ]  # Use `to_remove` to filter

      # Update the recolor of polygons in one go
      leafletProxy("map") %>%
        addPolygons(
          data = updated_data,  # Use the updated data for the parcels in `to_remove`
          weight = 1,
          color = "#000000",
          fillColor = "#dddde2",  # New color
          fillOpacity = 0.8,    # New opacity
          layerId = updated_data$parcel_id  # Ensure to reassign the same layerId for all
        )
    }
    
    # Add new polygons (those that are in filtered data but not on the map)
    if (length(to_add) > 0) {
      new_data <- fdata[fdata$parcel_id %in% to_add, ]
      print("New Data to Add:")
      print(new_data$parcel_id)
      
      leafletProxy("map") %>%
        addPolygons(
          data = new_data,  # Filtered data for new polygons
          weight = 1,
          color = "#000000",
          fillColor = "green",  # Color for filtered polygons
          fillOpacity = 0.6,
          group = "filteredPolygons",  # Group for filtered polygons
          layerId = ~parcel_id  # Use parcel_id as layerId to add new polygons
        )
    }
    
    # Update the state of current layers
    current_layers(current_ids)
    print("current_layers:")
    print(current_layers)
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
    
    # # Create accordion panels dynamically for each saved strategy
    # accordion_items <- lapply(names(strategies), function(key) {
    #   accordion_panel(
    #     paste(key),
    #     actionButton(paste("load_strategy", key, sep = "_"), paste("Load")),
    #     actionButton(paste("delete_strategy", key, sep = "_"), " Delete", icon = icon("trash"))
    #   )
    # })
    # 
    # # Wrap in an accordion
    # tagList(
    #   accordion(!!!accordion_items)  # Unquote-splice to pass all items
    # )
  })
  
  # Save strategy when the "Save" button is clicked
  observeEvent(input$save, {
    # Save the current state (slider values)
    strategy <- list(
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
  observe({
    lapply(names(saved_strategies()), function(key) {
      # Dynamically handle load strategy for each strategy
      observeEvent(input[[paste("load_strategy", key, sep = "_")]], {
        strategy <- saved_strategies()[[key]]
        updateSliderInput(session, "carbon", value = strategy$carbon)
        updateSliderInput(session, "species", value = strategy$species)
        updateSliderInput(session, "species_goat_moth", value = strategy$species_goat_moth)
        updateSliderInput(session, "species_stag_beetle", value = strategy$species_stag_beetle)
        updateSliderInput(session, "species_lichens", value = strategy$species_lichens)
        updateSliderInput(session, "area", value = strategy$area)
        updateSliderInput(session, "recreation", value = strategy$recreation)
        
        updateSliderInput(session, "year", value = strategy$year)  # Load year as well
        
        updateCheckboxInput(session, "carbon_checkbox", value = strategy$carbon_checkbox)
        updateCheckboxInput(session, "species_checkbox", value = strategy$species_checkbox)
        updateCheckboxInput(session, "species_goat_moth_checkbox", value = strategy$species_goat_moth_checkbox)
        updateCheckboxInput(session, "species_stag_beetle_checkbox", value = strategy$species_stag_beetle_checkbox)
        updateCheckboxInput(session, "species_lichens_checkbox", value = strategy$species_lichens_checkbox)
        updateCheckboxInput(session, "area_checkbox", value = strategy$area_checkbox)
        updateCheckboxInput(session, "recreation_checkbox", value = strategy$recreation_checkbox)
      })
    })
  })
  
  # Observe the reset of the sliders
  observeEvent(input$reset, {
    # Reset map view
    leafletProxy("map") %>%
      setView(lng = LON_DEFAULT, lat = LAT_DEFAULT, zoom = ZOOM_DEFAULT)
    
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

shinyApp(ui, server)
