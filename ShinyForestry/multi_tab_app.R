library(shiny)

FolderSource <- normalizePath("..")

if (dir.exists("ShinyForestry")) {
  elicitor_folder <- normalizePath(file.path("ShinyForestry", "ElicitorOutput"))
  FolderSource <- normalizePath(file.path("ShinyForestry"))
} else {
  elicitor_folder <- normalizePath(file.path("ElicitorOutput"))
  FolderSource <- normalizePath(".")
}

# Retrieve packages from DESCRIPTION (in plantingtools_folder)
plantingtools_folder <- normalizePath(file.path(FolderSource, ".."))
if (file.exists(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))) {
  packages <- read.dcf(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))[, "Imports"]
} else {
  packages <- read.dcf(normalizePath(file.path(FolderSource, "DESCRIPTION")))[, "Imports"]
}
packages <- unlist(strsplit(packages, ",\\s*"))  # Split and flatten
packages <- gsub("\\s*\\(.*\\)", "", packages)  # Remove version constraints
packages <- na.omit(packages)  # Remove any NAs

# Install packages if needed
install <- FALSE
for (pkg in packages) {
  if (isFALSE(require(pkg, character.only = TRUE))) {
    install <- TRUE
  }
}
if (isTRUE(install)) {
  remotes::install_deps(plantingtools_folder, upgrade = "never")
}


# Load packages
for(i in 1:length(packages)) {
  library(packages[i], character.only = TRUE)
}

source(file.path(FolderSource, "config.R"))

# more --> less: debug / info / warning / error / none
MAX_LIMIT_LOG_LEVEL <- "info"
if (file.exists(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R"), mustWork = FALSE))) {
  source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
} else {
  source(normalizePath(file.path(FolderSource, "backend", "bayesian-optimization-functions.R")), local = TRUE)
}

# If the backend was already initialized, saved to disk, and environment variable is valid, use it
run_initialization_on_backend <- FALSE
backend_initialization_env_file <- normalizePath(file.path(elicitor_folder, "backend_env.rds"))
if (file.exists(backend_initialization_env_file)) {
  
  env <- readRDS(backend_initialization_env_file)
  # Ensure file is valid
  if (isFALSE(is.environment(env))) { # if it's is.environment it breaks my frontend
    notif(paste(backend_initialization_env_file, "seems to be corrupted. Deleting it."))
    file.remove(backend_initialization_env_file)
    run_initialization_on_backend <- TRUE
  }
  
  # Check if backend was initialized
  url <- paste0(API_URL, "/check_initialized")
  response <- httr::GET(url)
  if (httr::status_code(response) == 404) {
    run_initialization_on_backend <- TRUE
  }
  
} else {
  run_initialization_on_backend <- TRUE
}

if (isTRUE(run_initialization_on_backend)) {
  
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
  url <-paste0(API_URL, "/initialize?MAX_LIMIT_LOG_LEVEL=", MAX_LIMIT_LOG_LEVEL)
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
  
  # Save it to elicitor folder
  saveRDS(env, backend_initialization_env_file)
}

# Load environment to the .GlobalEnv
list2env(as.list(env), envir = .GlobalEnv)

# Source module files
source("global.R")  # Load global settings
source("config.R")  # Load config

source("modules/map_page.R")
source("modules/preferences_page.R")
source("modules/alternative_approaches_page.R")
source("modules/exploration_page.R")
# source("modules/downscaling_page.R")

source("utils/api_functions.R")

custom_theme <- bs_theme(
  bootswatch = "lumen",
  "navbar-bg" = "#003c3c"
)

# Define UI
ui <- fluidPage(
  
  useShinyjs(),  # Initialize shinyjs

  # Full-page loading screen (visible on app startup)
  div(id = "loading", 
      div(class = "spinner")
  ),
  
  # Wrap the entire content in a div with an ID
  div(id = "app_content",  
      # The main content of the app (page_navbar)
      page_navbar(
        title = "ADD-TREES",
        theme = custom_theme,
        
        # Link the external CSS files
        tags$head(
          tags$link(
            rel = "stylesheet", 
            href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600&display=swap"
          ),
          tags$link(
            rel = "stylesheet", 
            href = "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css"
          ),
          tags$link(
            rel = "stylesheet", 
            type = "text/css",
            href = "custom.css"
          )
        ),
        
        tabPanel(title = "Map", map_page_ui("map")),
        tabPanel(title = "Preferences", preferences_page_ui("prefs")),
        tabPanel(title = "Alternative Approaches", alt_page_ui("alt")),
        tabPanel(title = "Exploration", exploration_page_ui("explore")),
        # tabPanel(title = "Downscaling", downscaling_page_ui("downscale")),
        
        nav_spacer(),
        nav_item(tags$a("AI for Net Zero",
                        href = "https://netzeroplus.ac.uk/",
                        target = "_blank")
        )
      )
  )
)

server <- function(input, output, session) {
  
  state <- reactiveValues(
    map = list(
      zoom = 13, 
      lat = 50.820184,
      lng = -1.733029
    ),
    map_tab = list(
      initialized = FALSE
      # slider_defaults = list(NULL) # to be populated by the initialisation API?
    ),
    pref_tab = list(
      initialized = FALSE
    ),
    alt_tab = list(
      initialized = FALSE,
      map = list(
        zoom = 12
      )
    ),
    exp_tab = list(
      initialized = FALSE
    ),
    initialized = FALSE
  )
    
  map_page_server("map", state)
  preferences_page_server("prefs", state)
  alt_page_server("alt", state)
  exploration_page_server("explore", state)
  # downscaling_page_server("downscale", state)
  
  # Initialization that is required for the `loadingCompleted` state to be False
  observe({
    if (!is.null(input$mappageRendered) && input$mappageRendered
        && !is.null(input$prefpageRendered) && input$prefpageRendered
        && !is.null(input$altpageRendered) && input$altpageRendered) {
        # && !is.null(input$explrpageRendered) && input$explrpageRendered) { # add other checks for other pages
        
      # Hide the loading screen after both maps are rendered
      Sys.sleep(0.5)  # Small delay for smoother transition
      
      # Fade out the loading screen (can now be triggered after clicking back to map)
      shinyjs::runjs('$("#loading").fadeOut(1000);')  # Fade out the loading screen
      
      # Mark the loading as completed in the state
      state$initialized <- TRUE
    }
  })
  
}


shinyApp(ui, server)
