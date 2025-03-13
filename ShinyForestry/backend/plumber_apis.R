library(plumber)
library(jsonlite)
library(future)


RNGversion("4.0.0")
set.seed(1)

normalizePath <- function(path, winslash = "/", mustWork = FALSE) {
  base::normalizePath(path, winslash = winslash, mustWork = mustWork)
}

sysinf <- Sys.info()
if (!is.null(sysinf)){
  os <- sysinf['sysname']
  if (os == 'Darwin')
    os <- "osx"
} else { ## mystery machine
  os <- .Platform$OS.type
  if (grepl("^darwin", R.version$os))
    os <- "osx"
  if (grepl("linux-gnu", R.version$os))
    os <- "linux"
}
os <- tolower(os)
if (os == "windows" || isTRUE(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())) {
  futureplan <- future::multisession
} else {
  futureplan <- future::multicore
}
# future:::ClusterRegistry("stop")

# more --> less: debug / info / warning / error / none
MAX_LIMIT_LOG_LEVEL <- "debug"

# FolderSource <- normalizePath("/Planting-Tools/ShinyForestry")
FolderSource <- normalizePath(file.path(".."))
# Overridden in server() block, necessary for source(...)
SESSION_FILE_SUFFIX <- ""

source(normalizePath(file.path(FolderSource, "functions.R")), local = TRUE)
source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
source(normalizePath(file.path(FolderSource, "preferTrees.R")), local = FALSE)


# Retrieve packages from DESCRIPTION (in plantingtools_folder)
plantingtools_folder <- normalizePath(file.path(FolderSource, ".."))
packages <- read.dcf(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))[, "Imports"]
packages <- unlist(strsplit(packages, ",\\s*"))  # Split and flatten
packages <- gsub("\\s*\\(.*\\)", "", packages)  # Remove version constraints
packages <- na.omit(packages)  # Remove any NAs

# Load packages in DESCRIPTION
for(ll in 1:length(packages)) {
  library(packages[ll], character.only = TRUE)
}

# Define file paths
ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
CalculatedFilesFolder <- normalizePath(file.path(FolderSource, "CalculatedFiles"))



#* Log full request information
#* @filter log_request
function(req) {
  # # Print full URI and other request details
  # cat("Full URI:", req$rook.url_scheme, "://", req$HTTP_HOST, req$PATH_INFO, "\n")
  # cat("Method:", req$REQUEST_METHOD, "\n")
  # cat("Query String:", req$QUERY_STRING, "\n")
  # cat("Headers:", paste(names(req$HEADERS), req$HEADERS, sep = ": ", collapse = "; "), "\n")
  # try(cat("POST Body (if any):", rawToChar(req$postBody), "\n"), silent = FALSE)
  
  notif(paste0(req$REQUEST_METHOD, " -- ",
               req$rook.url_scheme, "://", req$HTTP_HOST, req$PATH_INFO, req$QUERY_STRING, " -- ",
               "Headers = { ", paste(names(req$HEADERS), req$HEADERS, sep = ": ", collapse = "; "), " }"),
        ntfy = FALSE)
  
  # Forward the request to the next filter or endpoint
  forward()
}

# Function to generate parcel data
generate_parcel_data <- function(FullTable) {
  
  # Function to generate empty parcel data
  generate_empty_parcel_data <- function(FullTable) {
    if (!inherits(FullTable, "sf")) {
      stop("FullTable must be an sf object.")
    }
    
    n <- nrow(FullTable)
    parcel_ids <- 1:n
    geometries <- st_geometry(FullTable)
    
    empty_parcel_data <- st_sf(
      parcel_id = parcel_ids,
      geometry = geometries,
      crs = st_crs(FullTable)
    )
  }
  
  empty_parcel_data <- generate_empty_parcel_data(FullTable)
  
  n <- nrow(empty_parcel_data)
  parcel_areas <- FullTable$area
  planting_years <- sample(2025:2049, n, replace = TRUE)
  planting_types <- sample(c("Deciduous", "Conifer", NA), n, replace = TRUE)
  planting_years[is.na(planting_types)] <- NA
  blocked_until <- 0
  
  parcel_data <- st_sf(
    parcel_id = empty_parcel_data$parcel_id,
    geometry = empty_parcel_data$geometry,
    parcel_area = parcel_areas,
    planting_year = planting_years,
    planting_type = planting_types,
    blocked_until_year = blocked_until,
    crs = st_crs(FullTable)
  )
  
  print(parcel_data)
  return(parcel_data)
}

#* @apiTitle Parcel Data API
#* @apiDescription Returns parcel data in GeoJSON format.

# #* @get /health
# #* @serializer json
# function() {
#   list(status = "OK", message = "Plumber API is running")
#   res$status <- 200
# }

#* Health
#* @get /health
#* @response 200 Success: Service is healthy
function(res) {
  res$status <- 200
  return("OK")
}

#* Get slider values
#* @get /slider_values
#* @serializer json
function() {
  slider_info <- list(
    carbon = list(min = 500, max = 1000, default = 800),
    species = list(min = 0, max = 25, default = 10),
    species_goat_moth = list(min = 0, max = 100, default = 25),
    species_stag_beetle = list(min = 0, max = 100, default = 30),
    species_lichens = list(min = 0, max = 5, default = 2),
    area = list(min = 0, max = 15, default = 10),
    recreation = list(min = 0, max = 20, default = 15)
  )
  return(slider_info)
}


# Example Output
#                               parcel_id                       geometry
# 1  f4b4e4d0-7f09-4d26-a7b7-bab5b3151a57 POLYGON ((-1.756976 50.8314...
# 2  26841c10-5fdb-4988-8795-50ef9038ed5d POLYGON ((-1.766385 50.8160...
# 3  cb4fdd41-11ac-426e-b4d6-c04ea47ea14d POLYGON ((-1.765671 50.8316...
# 4  cae4aa35-d2a9-415c-86ef-873d281855d3 POLYGON ((-1.759141 50.8113...
# 5  bd20ad23-6046-46b8-8efa-ddb018d6d865 POLYGON ((-1.759423 50.8109...
# 6  26864357-0046-448e-8b08-ebf00c61fa0a POLYGON ((-1.761 50.83261, ...
# 7  447118a9-ede7-48cb-af07-c5fa822193e5 POLYGON ((-1.763823 50.825,...
# 8  2a110f23-eb3c-4608-a2e6-28c4e2fd673e POLYGON ((-1.762628 50.8344...
# 9  3451c88f-3298-4159-89d9-7bd79b4ff513 POLYGON ((-1.762547 50.8200...
# 10 fa5558f3-a884-44cd-a9db-6b1876ff68cc POLYGON ((-1.760264 50.8301...







# ---- GENERATE_PARCELS
# -- Expected Input
# {
#   "carbon": 800,
#   "species": 12,
#   "species_goat_moth": 90,
#   "species_stag_beetle": 20,
#   "species_lichens": 2,
#   "area": 7,
#   "recreation": 10,
#   "blocked_parcels": [
#     {
#       "parcel_id": "4fe067d1-d80d-4d33-8323-4a4403d2b4a5",
#       "blocked_until_year": 2025
#     },
#     {
#       "parcel_id": "9af563df-2be3-4520-bdd0-dbd26638a063",
#       "blocked_until_year": 2025
#     }
#   ]
# } 

# -- Expected Output list(values, geojson)
# {
#   "carbon": 852,
#   "species": 18,
#   "species_goat_moth": 91,
#   "species_stag_beetle": 22,
#   "species_lichens": 4,
#   "area": 5,
#   "recreation": 16,
# } 
#                               parcel_id parcel_area planting_year planting_type is_available blocked_until_year is_blocked                       geometry
# 1  bdd124e7-a162-4602-bff3-eb5e438d1440 0.022698955            NA          <NA>         TRUE                  0         NA POLYGON ((-1.756976 50.8314...
# 2  162f46c9-dd15-42eb-aa6d-fbbafe002bb6 0.036774571          2043       Conifer         TRUE                  0         NA POLYGON ((-1.766385 50.8160...
# 3  cc38292e-c59c-46b5-84b5-b4a015622d61 0.034369548          2038       Conifer         TRUE                  0         NA POLYGON ((-1.765671 50.8316...
# 4  558f048b-c156-4bf8-9f8f-5dbfce356210 0.027595724            NA          <NA>         TRUE                  0         NA POLYGON ((-1.759141 50.8113...
# 5  48fe3001-8443-4f08-b403-d2304a6c80a9 0.009152795            NA          <NA>         TRUE                  0         NA POLYGON ((-1.759423 50.8109...
# 6  7b66b5a2-8f68-4bba-adf8-6285fc96940a 0.021871169          2044     Deciduous         TRUE                  0         NA POLYGON ((-1.761 50.83261, ...
# 7  a41dfe2f-1856-4806-bf8d-7955d10565bc 0.015572843            NA          <NA>         TRUE                  0         NA POLYGON ((-1.763823 50.825,...
# 8  e52438b7-acf3-4428-b148-bd5b5ea7313e 0.017445100            NA          <NA>         TRUE                  0         NA POLYGON ((-1.762628 50.8344...
# 9  c033b36f-e7dd-4bdb-9deb-a008e442c413 0.015956941          2045       Conifer         TRUE                  0         NA POLYGON ((-1.762547 50.8200...
# 10 3d8adce4-14a0-4b35-8595-ef4645aed0db 0.035769157            NA          <NA>         TRUE                  0         NA POLYGON ((-1.760264 50.8301...


#* Generate parcel data
#* @post /generate_parcels
#* @param req The request body must contain `parcel_id` (string) and `blocked_until_year` (integer)
#* @serializer json
function(req) {
  
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) return(list(error = "Invalid JSON format."))
  )
  
  # Debug: Print the incoming body to inspect its structure
  print("Incoming body:")
  print(body)
  
  # Validate required fields
  required_fields <- c("carbon", "species", "species_goat_moth", "species_stag_beetle",
                       "species_lichens", "area", "recreation", "blocked_parcels")
  
  if (!all(required_fields %in% names(body))) {
    return(list(error = "Missing required fields in request body."))
  }
  
  # Extract blocked parcels
  blocked_parcels <- body$blocked_parcels
  
  # Debug: Check the structure of blocked_parcels
  print("Blocked parcels:")
  print(blocked_parcels)
  str(blocked_parcels)  # Check the structure of blocked_parcels
  
  # If blocked_parcels is a data frame, you can check directly for columns
  if (is.data.frame(blocked_parcels)) {
    # Validate each row in the data frame
    for (i in 1:nrow(blocked_parcels)) {
      parcel <- as.data.frame(blocked_parcels[i, , drop = FALSE])  # Convert to data frame row
      
      # Check if required columns exist
      if (!"parcel_id" %in% colnames(parcel) || !"blocked_until_year" %in% colnames(parcel)) {
        return(list(error = "Each parcel must have 'parcel_id' and 'blocked_until_year'."))
      }
      
      # Inspect parcel fields
      print(paste("Inspecting parcel", i))
      print(parcel)
      print(paste("Parcel ID: ", parcel$parcel_id))  # Debug: print parcel_id
    }
  } else if (is.list(blocked_parcels)) {
    # If blocked_parcels is an empty list, create an empty data frame
    if (length(blocked_parcels) == 0) {
      blocked_parcels <- data.frame(parcel_id = character(0), blocked_until_year = numeric(0))
    }
  } else {
    return(list(error = "`blocked_parcels` must be a data frame or a list."))
  }
  
  # Continue with backend logic
  parcel_data <- generate_parcel_data(FullTable)
  
  # Update blocked parcels in the backend logic
  for (i in 1:nrow(blocked_parcels)) {
    parcel <- as.data.frame(blocked_parcels[i, , drop = FALSE])  # Convert to data frame row
    parcel_data$is_blocked[parcel_data$parcel_id == parcel$parcel_id] <- TRUE
    parcel_data$blocked_until_year[parcel_data$parcel_id == parcel$parcel_id] <- parcel$blocked_until_year
  }
  
  # Create dummy payload
  payload <- list(
    carbon = as.numeric(body$carbon), # 3* runif(1, min = 0.9, max = 1.1)),
    species = as.numeric(body$species), # * runif(1, min = 0.9, max = 1.1)),
    species_goat_moth = as.numeric(body$species_goat_moth), # * runif(1, min = 0.9, max = 1.1)),
    species_stag_beetle = as.numeric(body$species_stag_beetle), # * runif(1, min = 0.9, max = 1.1)),
    species_lichens = as.numeric(body$species_lichens), # * runif(1, min = 0.9, max = 1.1)),
    area = as.numeric(body$area), # * runif(1, min = 0.9, max = 1.1)),
    recreation = as.numeric(body$recreation) # * runif(1, min = 0.9, max = 1.1))
  )
  
  # Convert to GeoJSON
  geojson <- geojsonsf::sf_geojson(parcel_data)
  
  return(list(
    values = payload,
    geojson = geojson
  ))
}

#* Generate parcel data
#* @get /random_strategy
#* @param req The request body must contain `parcel_id` (string) and `blocked_until_year` (integer)
#* @serializer json
function() {
  # Generate parcel data
  parcel_data <- generate_parcel_data()
  geojson <- geojsonsf::sf_geojson(parcel_data)
  
  values <- list(
    carbon = as.numeric(500),
    species = as.numeric(5), 
    species_goat_moth = as.numeric(4),
    species_stag_beetle = as.numeric(9),
    species_lichens = as.numeric(10),
    area = as.numeric(15),
    recreation = as.numeric(10) 
  )
  
  # Add a small random number between -0.5 and 0.5 to each value
  values <- lapply(values, function(x) x + runif(1, -0.5, 0.5))
  
  return(list(
    values = values,
    geojson = geojson
  ))
}

# Run this file with plumber: `plumber::plumb("ShinyForestry/backend/mock_strategy.R")$run(port=8010)`


# https://github.com/rstudio/plumber/issues/579#issuecomment-702432276
# https://stackoverflow.com/questions/76968662/plumber-accepting-a-file-and-a-string-file-name
#* Upload user Elicitor files
#* curl -X PUT -H "Content-Type: multipart/form-data" -F "file_to_upload=@land_parcels.shp.zip;type=application/x-zip-compressed" localhost:<port>/upload
#* curl -X PUT -H "Content-Type: multipart/form-data" -F "file_to_upload=@decision_units.json" localhost:<port>/upload
#* curl -X PUT -H "Content-Type: multipart/form-data" -F "file_to_upload=@outcomes.json" localhost:<port>/upload
#* @put /upload
#* @param file_to_upload:[file] File to upload
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
function(req, res, file_to_upload) {
  
  library(mime)
  suppressWarnings({
    multi <- mime::parse_multipart(req)
  })
  
  acceptable_file_names <- c("land_parcels.shp.zip", "decision_units.json", "outcomes.json")
  
  save_folder <- get_backend_folder_save_data()
  elicitor_output_folder <- normalizePath(file.path(save_folder, "ElicitorOutput"))
  
  if (isFALSE(dir.exists(elicitor_output_folder))) {
    dir.create(elicitor_output_folder, recursive = TRUE)
  }
  
  
  # Get file from binary content
  filename <- multi$file_to_upload$name
  
  
  if (isFALSE(filename %in% acceptable_file_names)) {
    res$status <- 400
    return(paste("Bad request: The file name is incorrect, it should be one of", acceptable_file_names, "and you sent", filename))
  }
  file_path <- normalizePath(file.path(elicitor_output_folder, filename))
  
  file.copy(from = multi$file_to_upload$datapath, to = file_path,
            overwrite = TRUE)
  
  # An Elicitor file was changed, Calculated files must be re-computed
  calculated_files_folder <- normalizePath(file.path("CalculatedFiles"))
  files_to_remove <- list.files(path = calculated_files_folder)
  file.remove(file.path(calculated_files_folder, files_to_remove))
  
  res$status <- 200
  return("Success")
}

#* Check if user input file exists and matches the user's file
#* curl -X GET localhost:<port>/exists?filename=<filename>&md5sum=<md5sum>
#* @get /exists
#* @param filename Name of user input file
#* @param md5sum md5 value of the file
#* @response 200 Success: The file exists and is correct
#* @response 400 Bad request: The file hash is incorrect
#* @response 404 Not found: The file was not found
function(res, filename, md5sum) {
  
  save_folder <- get_backend_folder_save_data()
  elicitor_output_folder <- normalizePath(file.path(save_folder, "ElicitorOutput"))
  file <- file.path(elicitor_output_folder, filename)
  if (file.exists(file)) {
    
    hash <- tools::md5sum(file)
    if (hash != md5sum) {
      res$status <- 400
      res$setHeader("Expected-MD5-Hash", hash)
      return("File hash mismatch. Expected hash is in header Expected-MD5-Hash")
    } else {
      res$status <- 200
      return(paste(filename, "exists")) 
    }
    
  } else {
    
    res$status <- 404
    return(paste(filename, "does not exist"))
    
  }
}

#* Code before server block. Returns an environment
#* curl -X PUT -H "Accept: text/plain" localhost:<port>/initialization
#* @put /initialize
#* @serializer rds
#* @param MAX_LIMIT_LOG_LEVEL more --> less: debug / info / warning / error / none
#* @response 200 Success: Initialized the app, did pre-processing
#* @response 403 Forbidden: Missing one or more input files from the elicitor
#* @response 500 Internal Server Error: One of the elicitor files is not readable
function(res, MAX_LIMIT_LOG_LEVEL = "debug") {
  
  library(future)
  plan(futureplan, workers = min(4, future::availableCores()))
  
  formals(notif)$max_limit_log_level <- MAX_LIMIT_LOG_LEVEL
  # plan(sequential)
  notif("Backend initialization ...")
  
  new_environment <- new.env()
  with(new_environment, {
    
    options(future.rng.onMisuse = "ignore")
    
    ANALYSISMODE<-FALSE
    SHOW_TITLES_ON_CLUSTERING_PAGE<-F
    
    RUN_BO<-FALSE
    RNGversion("4.0.0")
    set.seed(1)
    #fixed strategies list contains strategies pre-selected to be shown in the preference elicitation
    FIXED_STRATEGIES_LIST<-list(YEAR=matrix(0,0,1),TYPE=matrix(0,0,1),OUTPUTS=matrix(0,0,1))
    POLYGON_OPACITY<-0.6
    GREY_BACKGROUND_OPACITY<-0.3
    NOTAVAIL_OPACITY<-0.7
    
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { # mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    os <- tolower(os)
    if (os == "windows" || isTRUE(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())) {
      futureplan <- future::multisession
    } else {
      futureplan <- future::multicore
    }
    # futureplan <- future::sequential
    # future:::ClusterRegistry("stop")
    
    FolderSource <- normalizePath(file.path(".."))
    # FolderSource <- normalizePath(file.path(getwd(), "..", "ShinyForestry"))
    # if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
    #   FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
    # }
    
    STARTYEAR<-2025
    MAXYEAR<-2050-STARTYEAR-1
    SCENARIO <- 26
    
    # # Delete log and lock files
    # unlink(base::normalizePath(file.path(FolderSource, "log*"), mustWork = FALSE))
    # unlink(base::normalizePath(file.path(FolderSource, "*lockfile*"), mustWork = FALSE))
    # unlink(base::normalizePath(file.path(FolderSource, "task_id*"), mustWork = FALSE))
    
    # Overridden in server() block, necessary for source(...)
    SESSION_FILE_SUFFIX <- ""
    source(normalizePath(file.path(FolderSource, "functions.R")), local = TRUE)
    source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
    source(normalizePath(file.path(FolderSource, "preferTrees.R")), local = FALSE)
    
    
    # Retrieve packages from DESCRIPTION (in plantingtools_folder)
    plantingtools_folder <- normalizePath(file.path(FolderSource, ".."))
    packages <- read.dcf(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))[, "Imports"]
    packages <- unlist(strsplit(packages, ",\\s*"))  # Split and flatten
    packages <- gsub("\\s*\\(.*\\)", "", packages)  # Remove version constraints
    packages <- na.omit(packages)  # Remove any NAs
    
    # # Load packages in DESCRIPTION
    for(i in 1:length(packages)) {
      library(packages[i], character.only = TRUE)
    }
    
    if (RUN_BO) {
      dgpsi::init_py(verb = FALSE)
    }
    
    # handlers(global = TRUE)
    # Progress report with progressr
    progress_handlers <- list(
      handler_progress(
        format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
      )
    ) 
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      progress_handlers <- c(progress_handlers,
                             handler_rstudio())
    } else {
      progress_handlers <- c(progress_handlers, handler_txtprogressbar())
    }
    if (os == "windows") {
      progress_handlers <- c(progress_handlers, handler_winprogressbar())
    }
    handlers(
      progress_handlers,
      on_missing = "warning"
    )
    
    NAME_CONVERSION <- get_name_conversion()
    
    # Swap rows 83 and 84
    row83 <- NAME_CONVERSION[83, ]
    row84 <- NAME_CONVERSION[84, ]
    NAME_CONVERSION[83, ] <- row84
    NAME_CONVERSION[84, ] <- row83
    
    
    GreyPolygonWidth <- 1
    UnitPolygonColours <- 1
    # Sys.setenv(PROJ_LIB = "/usr/share/proj")
    # Sys.setenv(GDAL_DATA = "/usr/share/proj/")
    
    USER_PATH <- user_path()
    
    save_folder <- get_backend_folder_save_data()
    save_folder_elicitoroutput <- normalizePath(file.path(save_folder, "ElicitorOutput"))
    dir.create(save_folder_elicitoroutput, recursive = TRUE, showWarnings = FALSE)
    
    # ElicitorAppFolder <- normalizePath(file.path(USER_PATH, "Downloads"))
    # ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
    ElicitorAppFolder <- save_folder_elicitoroutput
    DataFilesFolder <- normalizePath(file.path(FolderSource, "JulesOP"))
    DownscalingImagesFolder<-normalizePath(file.path(FolderSource, "DownScalingImages"))
    
    # CalculatedFilesFolder <- normalizePath(file.path(FolderSource, "CalculatedFiles"))
    CalculatedFilesFolder <- normalizePath(file.path(save_folder, "CalculatedFiles"))
    
    # If the folder does not exist, create it
    if (isFALSE(dir.exists(CalculatedFilesFolder))) {
      dir.create(CalculatedFilesFolder)
    }
    
    # Loading big files takes up a lot of RAM that cannot be emptied.
    # So instead, loading happens in a new R process we can then shutdown
    # plan(futureplan, workers = 2)
    
    # Load files if any are missing
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson"))) ||
        !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson"))) ||
        !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))) {
      # 337th month
      # mean1 month corresponds to (maybe) January 2022
      # Jules Files loaded later as they take too much memory
      
      SquaresLoad <- value(future(sf::st_read(normalizePath(file.path(DataFilesFolder, "SEER", "Fishnet_1km_to_SEER_net2km.shp")))))
      Sqconv <- st_transform(SquaresLoad, crs = 4326)
      CorrespondenceJules <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "CorrespondanceSqToJules.csv")))[, -1]))
      seer2km <- value(future(st_read(normalizePath(file.path(DataFilesFolder, "SEER_net2km.shp")))))
      jncc100 <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "beta_JNCC100_interact_quad.csv")))))
      baseline_species_prob_40_unmanaged_conifers <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "baseline_species_prob_40_unmanaged_conifers.csv")), header = FALSE)))
      baseline_species_prob_40_unmanaged_deciduous <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "baseline_species_prob_40_unmanaged_deciduous.csv")), header = FALSE)))
      scenario_species_prob_40_unmanaged_conifers <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "scenario_species_prob_40_unmanaged_conifers.csv")), header = FALSE)))
      scenario_species_prob_40_unmanaged_deciduous <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "scenario_species_prob_40_unmanaged_deciduous.csv")), header = FALSE)))
      speciesprob_list <- list(list(planting = FALSE, # baseline
                                    tree_specie = "Conifers",
                                    table = baseline_species_prob_40_unmanaged_conifers),
                               
                               list(planting = FALSE, # baseline
                                    tree_specie = "Deciduous",
                                    table = baseline_species_prob_40_unmanaged_deciduous),
                               
                               list(planting = TRUE, # scenario
                                    tree_specie = "Conifers",
                                    table = scenario_species_prob_40_unmanaged_conifers),
                               
                               list(planting = TRUE, # scenario
                                    tree_specie = "Deciduous",
                                    table = scenario_species_prob_40_unmanaged_deciduous))
      # Frees RAM
      rm(baseline_species_prob_40_unmanaged_conifers, baseline_species_prob_40_unmanaged_deciduous,
         scenario_species_prob_40_unmanaged_conifers, scenario_species_prob_40_unmanaged_deciduous)
      
      climatecells <- read.csv(normalizePath(file.path(DataFilesFolder, "climate_cells.csv")))
    }
    
    notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip"))))
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")))) {
      res$status <- 403
      return("Please upload land_parcels.shp.zip")
    }
    notif(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "found. Trying to unzip and load files..."))
    
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))) {
      # UnZipDirName <- normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp"))
      UnZipDirName <- normalizePath(file.path("/tmp", "land_parcels.shp"))
      dir.create(UnZipDirName)
      tries <- 0
      while (inherits(suppressWarnings(try(unzip(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), exdir = UnZipDirName),
                                           silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read land_parcels.shp.zip")
        }
      }
      
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "unzipped. Loading files..." ))
      shconv <- sf::st_read(normalizePath(file.path(UnZipDirName, "land_parcels.shp")))
      if (is.null(shconv$extent)) {
        shconv$extent <- "NoExtent"
      }
      st_write(shconv, normalizePath(file.path(save_folder_elicitoroutput, "Parcels.geojson")))
    } else {
      shconv <- sf::st_read(normalizePath(file.path(save_folder_elicitoroutput, "Parcels.geojson")))
    }
    
    notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "decision_units.json"))))
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))) {
      res$status <- 403
      return("Please upload decision_units.json")
    }
    notif(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "found. Trying to load file if FullTableMerged.geojson does not exist..."))
    
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))) {
      sf_use_s2(FALSE)
      lsh <- dim(shconv)[1]
      
      tries <- 0
      while (inherits(suppressWarnings(try(AllUnits <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))$decision_unit_ids,
                                           silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read decision_units.json")
        }
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "loaded, processing ..." ))
      
      # Turn decision units to individual parcels, by making all valid ones (i.e. that are != -1) get a unique id instead
      indices <- which(AllUnits != -1)
      AllUnits[indices] <- seq_along(indices)
      
      Uni <- unique(AllUnits)
      # units is the list of decision units
      FullTab <- data.frame(extent = "NoExtent", x = rep(0, length(Uni)), y = rep(0, length(Uni)), area = rep(1, length(Uni)),
                            Carbon_Mean_Scenario26_TreeSpecieConifers = rep(15, length(Uni)),
                            Carbon_SD_Scenario26_TreeSpecieConifers = rep(1, length(Uni)), 
                            Carbon_Mean_Scenario26_TreeSpecieDeciduous = rep(15, length(Uni)),
                            Carbon_SD_Scenario26_TreeSpecieDeciduous = rep(1, length(Uni)), 
                            VisitsMean = rep(30, length(Uni)),
                            VisitsSD = rep(2, length(Uni)), BioMean_Sciurus_vulgaris = rep(0.5, length(Uni)),
                            BioSD_Sciurus_vulgaris = rep(0.02, length(Uni)), units = Uni)
      #We are looking at CO2 retained in 2050. Then JulesMeanY0 means that if we plant in year 0,
      #we will obtain JulesMeanYears$mean337, year 1: mean325, year 2: mean313,....
      #Note that JulesMeanY29 is considered "No Planting" so the values of 0 will not be changed
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      
      
      MER <- list()
      for (i in 1:length(Uni)) {
        SELLL <- shconv$geometry[AllUnits == Uni[i]]
        MER[[i]] <- suppressMessages(st_union(st_make_valid(SELLL)))
      }
      
      
      FullTable <- st_sf(FullTab, geometry = do.call(c, MER), crs = 4326)
      #
      keptLines <- sort(which(as.numeric(summary(sf::st_intersects(Sqconv, shconv))[, 1]) != 0))
      
      SELECTEDSquaresconv <- Sqconv$geometry[keptLines]
      LinesJules <- CorrespondenceJules[keptLines]
      # Find lines where Jules is not available, it means there are no trees, so replace by 0
      LinesJulesNoMinus1 <- which(LinesJules == (-1))
      LinesJules[LinesJulesNoMinus1] <- 1
      
      # Jules results for all years from 0 to MAXYEAR
      # Loading feather files takes a lot of RAM permanently. This makes it happen in another process we shut down,
      JulesMeanYears <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-mean-monthly.feather")))[,c("x","y",paste0("mean",seq(1,337,by=12)))]))
      JulesSDYears <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-sd-monthly.feather")))[,c("x","y",paste0("sd",seq(1,337,by=12)))]))
      JulesMean <- JulesMeanYears[, c("x", "y", "mean337")]
      JulesSD <- JulesSDYears[, c("x", "y", "sd337")]
      
      SelectedJulesMeanSq <- JulesMean[LinesJules, ]
      SelectedJulesMeanSq[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDSq <- JulesSD[LinesJules, ]
      SelectedJulesSDSq[LinesJulesNoMinus1, ] <- 0
      rm(JulesMean, JulesSD)
      
      
      SelectedJulesMeanYears<-JulesMeanYears[LinesJules,paste0("mean",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesMeanYears[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDYears<-JulesSDYears[LinesJules,paste0("sd",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesSDYears[LinesJulesNoMinus1, ] <- 0
      rm(JulesMeanYears, JulesSDYears)
      
      
      JulesMeanYears85 <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-mean-monthly.feather")))[,c("x","y",paste0("mean",seq(1,337,by=12)))]))
      JulesSDYears85 <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-sd-monthly.feather")))[,c("x","y",paste0("sd",seq(1,337,by=12)))]))
      JulesMean85 <- JulesMeanYears85[, c("x", "y", "mean337")]
      JulesSD85 <- JulesSDYears85[, c("x", "y", "sd337")]
      SelectedJulesMeanSq85 <- JulesMean85[LinesJules, ]
      SelectedJulesMeanSq85[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDSq85 <- JulesSD85[LinesJules, ]
      SelectedJulesSDSq85[LinesJulesNoMinus1, ] <- 0
      rm(JulesMean85, JulesSD85)
      
      
      SelectedJulesMeanYears85<-JulesMeanYears85[LinesJules,paste0("mean",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesMeanYears85[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDYears85<-JulesSDYears85[LinesJules,paste0("sd",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesSDYears85[LinesJulesNoMinus1, ] <- 0
      rm(JulesMeanYears85, JulesSDYears85)
      # gc()
      
      
      
      
      SELECTEDSquaresconvTab <- data.frame(idSq = seq_along(SELECTEDSquaresconv))
      SELECTEDSquaresconvTab <- st_sf(SELECTEDSquaresconvTab, geometry = SELECTEDSquaresconv, crs = 4326)
      
      
      FullTableCopy <- FullTable
      FullTableCopy$idPoly <- seq_along(FullTableCopy$geometry)
      
      #st_as_sf(data.frame(geometry = SELECTEDSquaresconv))
      #st_as_sf(data.frame(FullTable))
      
      
      INTT <- st_intersection(st_make_valid(SELECTEDSquaresconvTab), st_make_valid(FullTableCopy))
      INTT$area <- st_area(INTT) / 1e6
      
      # Bootstrap means and standard deviations (to avoid assumptions of independence)
      # As we have sum of Gaussians, we 
      # NBSIMS <- 500
      for (i in 1:length(FullTableCopy$geometry)) {
        SELLLines <- INTT$idPoly == i
        SELLSqs <- INTT$idSq[SELLLines]
        SELLWeights <- INTT$area[SELLLines]
        #    SellWeightsArr <- t(matrix(SELLWeights, length(SELLWeights), NBSIMS))
        SellWeightsArr <- (matrix(SELLWeights, length(SELLWeights), (MAXYEAR+1)))
        
        SelJulesMeans <- SelectedJulesMeanSq$mean337[SELLSqs]
        SelJulesSDs <- SelectedJulesSDSq$sd337[SELLSqs]
        
        SelJulesMeansYears<-SelectedJulesMeanYears[SELLSqs,]
        SelJulesSDsYears <- SelectedJulesSDYears[SELLSqs,]
        
        SelJulesMeans85 <- SelectedJulesMeanSq85$mean337[SELLSqs]
        SelJulesSDs85 <- SelectedJulesSDSq85$sd337[SELLSqs]
        
        SelJulesMeansYears85<-SelectedJulesMeanYears85[SELLSqs,]
        SelJulesSDsYears85 <- SelectedJulesSDYears85[SELLSqs,]
        
        
        if (length(SelJulesMeans) >= 1) {
          # SimuArr <- rmvnorm(NBSIMS, mean = SelJulesMeans, sigma = diag(SelJulesSDs^2))
          FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[i] <-    sum(SelJulesMeans*SELLWeights)#sum(colMeans(SimuArr * SellWeightsArr))
          FullTable$Carbon_SD_Scenario26_TreeSpecieConifers[i] <- sqrt(sum((SelJulesSDs*SELLWeights)^2))#sd(rowSums(SimuArr * SellWeightsArr))
          
          #JulesMeanY29 is not replaced here as it is used to men that there is no planting.
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-colSums(SelJulesMeansYears*SellWeightsArr)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-sqrt(colSums((SelJulesSDsYears*SellWeightsArr)^2))
          
          FullTable$Carbon_Mean_Scenario26_TreeSpecieDeciduous<-sum(SelJulesMeans85*SELLWeights)
          FullTable$Carbon_SD_Scenario26_TreeSpecieDeciduous<-sqrt(sum((SelJulesSDs85*SELLWeights)^2))
          
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-colSums(SelJulesMeansYears85*SellWeightsArr)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-sqrt(colSums((SelJulesSDsYears85*SellWeightsArr)^2))
          
          
          FullTable$area[i] <- sum(SELLWeights)
          
          # } else if (length(SelJulesMeans) == 1) {
          #  SimuArr <- rnorm(NBSIMS, mean = SelJulesMeans, sd = SelJulesSDs)
          # FullTable$JulesMean[i] <- sum(colMeans(SimuArr * SellWeightsArr))
          #  FullTable$JulesSD[i] <- sd(rowSums(SimuArr * SellWeightsArr))
          #  FullTable$area[i] <- sum(SELLWeights)
        } else {
          FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[i] <- 0
          FullTable$Carbon_SD_Scenario26_TreeSpecieConifers[i] <- 0
          FullTable$Carbon_Mean_Scenario26_TreeSpecieDeciduous[i]<-0
          FullTable$Carbon_SD_Scenario26_TreeSpecieDeciduous[i]<-0
          
          
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          
          
          FullTable$area[i] <- sum(SELLWeights)
        }
      }
      
      # Replace Biodiversity columns with correct ones
      msg <- "Converting biodiversity from square grid cells to our shapefile polygons ..."
      notif(msg)
      FullTable <- convert_bio_to_polygons_from_elicitor_and_merge_into_FullTable(Elicitor_table = FullTable,
                                                                                  speciesprob_list = speciesprob_list,
                                                                                  seer2km = seer2km,
                                                                                  jncc100 = jncc100,
                                                                                  climatecells = climatecells,
                                                                                  MAXYEAR = MAXYEAR,
                                                                                  max_limit_log_level = MAX_LIMIT_LOG_LEVEL)
      msg <- paste(msg, "done")
      notif(msg)
      # Free a lot of RAM
      rm(speciesprob_list)
      
      # Outcomes
      notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))))
      while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
        res$status <- 403
        return("Please upload outcomes.json")
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file ..."))
      
      # Read the outcomes from the Elicitor app
      tries <- 0
      while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))),
                                           silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read outcomes.json")
        }
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing ..."))
      
      outsomes_biodiversity_indices <- sapply(outcomes, function (x) x$category == "Biodiversity")
      SPECIES_ENGLISH <- unique(sapply(outcomes[outsomes_biodiversity_indices], function(x) x$`sub-category`))
      
      # SPECIES_ENGLISH <- c("Pollinators", "Reed Bunting", "Lapwing", "Invertebrate - snails")
      # c("Pollinators", "Herptiles")
      # SPECIES_ENGLISH <- unique(NAME_CONVERSION$Group_pretty)[c(2, 7)]
      
      # Default specie and group
      if (length(SPECIES_ENGLISH) == 0) {
        SPECIES_ENGLISH <- "All"
      }
      GROUPS <- c()
      # Separate the groups from SPECIES_ENGLISH, then merge them to SPECIES
      SPECIES <- SPECIES_ENGLISH
      for (i in 1:length(SPECIES_ENGLISH)) {
        ugly_english_specie <- get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
        # If it is a group
        if (ugly_english_specie %in% c(unique(NAME_CONVERSION$Group), unique(NAME_CONVERSION$Group_pretty), "All")) {
          SPECIES[i] <- get_ugly_group(ugly_english_specie, NAME_CONVERSION)
          GROUPS <- c(GROUPS, get_ugly_group(ugly_english_specie, NAME_CONVERSION))
        } else {
          # If it is a specie
          SPECIES[i] <- get_specie_from_english_specie(ugly_english_specie, NAME_CONVERSION)
        }
      }
      msg <- paste(msg, "done")
      notif(msg)
      
      # Add the richness
      msg <- "Adding richness columns ..."
      notif(msg, log_level = "debug")
      FullTable <- add_richness_columns(FullTable, groups = GROUPS, maxyear = MAXYEAR,
                                        NAME_CONVERSION = NAME_CONVERSION, SCENARIO = SCENARIO)
      msg <- paste(msg, "done")
      notif(msg)
      
      # FOR BACKWARD COMPATIBILITY
      # until the code works with the new biodiversity columns, keep the old ones:
      # BioMean_specie, BioSD_specie
      # BioMean_Birds, BioSD_Birds
      for (specie_or_group in SPECIES) {
        if (specie_or_group %in% NAME_CONVERSION$Specie) {
          
          old_col_mean_values <- FullTable %>%
            sf::st_drop_geometry() %>%
            dplyr::select(starts_with("Bio_Mean") &
                            contains(paste0("BioSpecie", specie_or_group, "_")) &
                            contains("Scenario26") &
                            contains("TreeSpecieConifers") &
                            contains("_Planting")) %>%
            pull()
          old_col_sd_values <- FullTable %>%
            sf::st_drop_geometry() %>%
            dplyr::select(starts_with("Bio_SD") &
                            contains(paste0("BioSpecie", specie_or_group, "_")) &
                            contains("Scenario26") &
                            contains("TreeSpecieConifers") &
                            contains("_Planting")) %>%
            pull()
          
          new_col_mean_name <- paste0("BioMean_", specie_or_group)
          new_col_sd_name <- paste0("BioSD_", specie_or_group)
          FullTable[[new_col_mean_name]] <- old_col_mean_values
          FullTable[[new_col_sd_name]] <- old_col_sd_values
          
        } else if (specie_or_group %in% c(NAME_CONVERSION$Group, "All")) {
          
          old_col_values <- FullTable %>%
            sf::st_drop_geometry() %>%
            dplyr::select(starts_with("Richness") &
                            contains(paste0("Group", specie_or_group, "_")) &
                            contains("TreeSpecieConifers") &
                            contains("Scenario26") &
                            contains("PlantingYear0")) %>%
            pull()
          
          new_col_name <- paste0("BioMean_", specie_or_group)
          FullTable[[new_col_name]] <- old_col_values
          
          old_col_values <- rep(0, length(old_col_values))
          new_col_name <- paste0("BioSD_", specie_or_group)
          FullTable[[new_col_name]] <- old_col_values
          
        }
      }
      
      # Only keep all possible biodiversity species (unions of species selected)
      species_to_keep <- intersect(SPECIES, NAME_CONVERSION$Specie)
      species_to_remove <- setdiff(NAME_CONVERSION$Specie, species_to_keep)
      species_to_remove_pattern <- paste0("Bio.*?(", paste0(species_to_remove, collapse = "|"), ").*")
      
      # Remove biodiversity columns with the species we don't need
      FullTable <- FullTable %>%
        dplyr::select(!matches(species_to_remove_pattern))
      
      # Move decision units with id -1 (Maintain current land use) from FullTable to FullTableNotAvail if we want to handle them in a special way
      # OR
      # Only delete lines with "units"=-1 from FullTable
      FullTableNotAvail <- FullTable %>%
        dplyr::filter(units == -1)
      FullTable <- FullTable %>%
        dplyr::filter(units != -1)
      
      # st_write(FullTable, normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
      st_write(FullTable, normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson")))
      # FullTableNotAvail <- data.frame(extent = NULL)
      # st_write(FullTableNotAvail, normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
      st_write(FullTableNotAvail, normalizePath(file.path(save_folder_elicitoroutput, "FullTableNotAvail.geojson")))
    }
    
    # notif(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson ..."))))
    # FullTable <- value(future(sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))))
    # FullTableNotAvail <- value(future(sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))))
    # notif(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))
    notif(paste("Loading", normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson and FullTableNotAvail.geojson ..."))))
    FullTable <- value(future(sf::st_read(normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson")))))
    FullTableNotAvail <- value(future(sf::st_read(normalizePath(file.path(save_folder_elicitoroutput, "FullTableNotAvail.geojson")))))
    notif(paste("Loading", normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))
    
    # future:::ClusterRegistry("stop")
    
    STDMEAN <- 0.05
    STDSTD <- 0.01
    
    # Random sampling
    NSamp <- 2000
    
    #Now the random sample contains the year of planting/
    msg <- paste0("Sampling ", NSamp, " random strategies ...")
    notif(msg)
    
    Uniqunits <- unique(FullTable$units)
    
    simul636YearType <- local({
      pb <- progressor(steps = NSamp, message = paste("Sampling", NSamp, "strategies ..."))
      simul636YearType <- foreach(
        i = 1:NSamp,
        .combine = combine_foreach_rbind,
        .multicombine = TRUE,
        .inorder = TRUE,
        .options.future = list(
          seed = TRUE
        )
      ) %dofuture% {
        
        # Part 1: simul636
        pp <- runif(1)
        RandSamp <- rmultinom(length(Uniqunits), 1, c(pp, 1 - pp))[1, ]
        
        result <- matrix(0, nrow = 1, ncol = dim(FullTable)[1])
        for (j in 1:length(Uniqunits)) {
          result[1, FullTable$units == Uniqunits[j]] <- RandSamp[j]
        }
        result_simul636 <- result
        
        
        
        # Part 2: simul636Year
        
        # Simul636Year is populated with the year of planting
        # once simul636Year works it will replace simul636
        # (MAXYEAR+1) is the code for no planting
        # Otherwise 0 to MAXYEAR is the year of planting if planted.
        result <- result_simul636
        
        result[1, result_simul636[1,]==0]<-(MAXYEAR+1)
        probb<-runif(1,0.2,0.6)
        size<-15*runif(1)
        result[1, result_simul636[1,]!=0]<-pmin(rnbinom(sum(result_simul636[1,]),size=size,prob=probb),MAXYEAR)
        
        result_simul636Year <- result
        
        
        
        # Part 3: simul636YearType
        
        #### Simul636YearType is a similar table but it stops at year MAXYEAR and
        #### instead, we pick a tree type (or no planting)
        result <- list(YEAR=result_simul636-1,TYPE=t(apply(result_simul636,2,as.character)))
        result[["TYPE"]][1, result_simul636[1,]==0]<-"NoPlanting"
        probbType<-runif(1,0.5)
        Planted<-(result_simul636[1,]==1)
        if(sum(Planted)>0){
          result[["TYPE"]][1, result_simul636[1,]==1]<-sample(c("Conifers","Deciduous"),sum(Planted),replace=TRUE,prob=c(probbType,1-probbType))}
        
        probb<-runif(1,0.2,0.6)
        size<-15*runif(1)
        DRAW<-pmin(rnbinom(sum(result_simul636[1,]),size=size,prob=probb),MAXYEAR)
        result$YEAR[1, result$TYPE[1,]!="NoPlanting"]<-DRAW
        
        result_simul636YearType <- result
        
        # End
        if (i %% (ceiling(NSamp / 100)) == 0) {pb(amount = ceiling(NSamp / 100))}
        return(result_simul636YearType)
      }
      # Avoid warning message from progressor function
      pb(amount = 0)
      return(simul636YearType)
    })
    
    msg <- paste(msg, "done")
    notif(msg)
    
    handlers(
      c(handler_shiny(),
        progress_handlers),
      on_missing = "ignore"
    )
    
    RREMBO_CONTROL <- list(
      # method to generate low dimensional data in RRembo::designZ ("LHS", "maximin", "unif"). default unif
      designtype = "LHS",
      # if TRUE, use the new mapping from the zonotope, otherwise the original mapping with convex projection. default TRUE
      reverse = FALSE)
    RREMBO_HYPER_PARAMETERS <- RRembo_defaults(d = 6,
                                               D = 3 * nrow(FullTable), # area + planting_year + tree_specie per parcel
                                               init = list(n = 100), budget = 100,
                                               control = RREMBO_CONTROL,
                                               max_limit_log_level = MAX_LIMIT_LOG_LEVEL)
    
    
    # Load the value into a reactive variable on the frontend directly
    Simul636YearOverrideReactive_toload_in_reactiveVal <- vector("list",dim(simul636YearType$YEAR)[2])
    Simul636YearTypeOverrideReactive_toload_in_reactiveVal <- vector("list",dim(simul636YearType$YEAR)[2])
    
    
    
    alphaLVL <- 0.9
    ILevel<- -(sqrt(alphaLVL/(1-alphaLVL)))
    
    MaxRounds <- 5
    ConvertSample <- sample(1:NSamp, 200)
    
    # Outcomes
    if (isFALSE(exists("outcomes", inherits = FALSE))) {
      msg <- paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
      notif(msg)
      while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
        Sys.sleep(5)
      }
      msg <- paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file...")
      notif(msg)
      
      # Read the outcomes from the Elicitor app
      tries <- 0
      while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
                                           , silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read outcomes.json")
        }
      }
      msg <- paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing ...")
      notif(msg)
      
      outsomes_biodiversity_indices <- sapply(outcomes, function (x) x$category == "Biodiversity")
      SPECIES_ENGLISH <- unique(sapply(outcomes[outsomes_biodiversity_indices], function(x) x$`sub-category`))
      
      # SPECIES_ENGLISH <- c("Pollinators", "Reed Bunting", "Lapwing", "Invertebrate - snails")
      # c("Pollinators", "Herptiles")
      # SPECIES_ENGLISH <- unique(NAME_CONVERSION$Group_pretty)[c(2, 7)]
      
      # Default specie and group
      if (length(SPECIES_ENGLISH) == 0) {
        SPECIES_ENGLISH <- "All"
      }
      # Separate the groups from SPECIES_ENGLISH, then merge them to SPECIES
      SPECIES <- SPECIES_ENGLISH
      for (i in 1:length(SPECIES_ENGLISH)) {
        ugly_english_specie <- get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
        # If it is a group
        if (ugly_english_specie %in% c(unique(NAME_CONVERSION$Group), unique(NAME_CONVERSION$Group_pretty), "All")) {
          SPECIES[i] <- get_ugly_group(ugly_english_specie, NAME_CONVERSION)
        }
      }
      for (i in 1:length(SPECIES_ENGLISH)) {
        ugly_english_specie <- get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
        # If it is a specie
        if (isFALSE(ugly_english_specie %in% c(unique(NAME_CONVERSION$Group), unique(NAME_CONVERSION$Group_pretty), "All"))) {
          SPECIES[i] <- get_specie_from_english_specie(ugly_english_specie, NAME_CONVERSION)
        }
      }
      
      msg <- paste(msg, "done")
      notif(msg)
    }
    
    # SPECIES <- c(NAME_CONVERSION[1:2, "Specie"], "Pollinators", "All")
    # SPECIES_ENGLISH <- c(NAME_CONVERSION[1:2, "English_specie"], "Pollinators", "All")
    N_SPECIES <- length(SPECIES)
    TARGETS <- c("Carbon", SPECIES, "Area", "Visits")
    N_TARGETS <- length(TARGETS)
    
    #Indicates if the quantity must be above (TRUE) or below the target (FALSE)
    AboveTargets<-rep(TRUE,N_TARGETS)
    AboveTargets[N_TARGETS-1]<-FALSE
    
    # slider_list <- list(
    #   sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25)
    # )
    # Add sliderInput("BioSliderSPECIE", "Average SPECIE % increase:", min = 0, max = 36, value = 25) for each specie
    
    verticalLayout_params <- c(list(sliderInput("SliderMain", "Tree carbon stored (tonnes of CO2):", min = -1, max = 870, value = -1)),
                               lapply(SPECIES, function(x, fulltable, NAME_CONVERSION_ARG) {
                                 NAME_CONVERSION <- NAME_CONVERSION_ARG
                                 # max_specie <- round(max(fulltable[, paste0("BioMean_", x)]))
                                 # value <- round(max_specie / 2)
                                 max_specie <- 36
                                 value <- 1
                                 
                                 # If it is a group
                                 if (x %in% c(NAME_CONVERSION$Group, NAME_CONVERSION$Group_pretty, "All")) {
                                   text <- paste0("Species richness (", get_pretty_group(x, NAME_CONVERSION), ")")
                                 } else {
                                   # If it is a specie
                                   text <- get_english_specie_from_specie(x, NAME_CONVERSION)
                                   text <- get_pretty_english_specie(text, NAME_CONVERSION)
                                   text <- paste(text, " (Presence, %):")
                                 }
                                 
                                 return(bquote(sliderInput(paste0("BioSlider", .(x)),
                                                           .(text),
                                                           min = 0,
                                                           max = .(max_specie),
                                                           value = .(value),
                                                           step = 0.5)))
                               }, fulltable = FullTable, NAME_CONVERSION_ARG = NAME_CONVERSION),
                               list(sliderInput("AreaSlider", HTML("Area planted (km<sup>2</sup>)"), min = 0, max = 25, value = 15,step=1)),
                               list(sliderInput("VisitsSlider", "Recreation (visits per month):", min = 0, max = 750, value = 400)))
    #SPECIES<-c("All","Acanthis_cabaret","Birds","Alauda_arvensis")
    SliderNames<- c("SliderMain",
                    paste0("BioSlider", SPECIES),
                    "AreaSlider","VisitsSlider")
    
    
    #### Precalculate simul636 table with years
    AllExtents<-sort(unique(FullTable$extent))
    
    
    #PrecalcCarbonAllExtents<-list()
    #PrecalcCarbonAllExtentsSD<-list()
    
    PrecalcCarbonAllExtentsType<-list()
    PrecalcCarbonAllExtentsSDType<-list()
    
    PrecalcCarbonAllExtentsType2Lines<-list()
    PrecalcCarbonAllExtentsSDType2Lines<-list()
    
    
    if(#file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData"))) &&
      #file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData"))) &&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData"))) &&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData"))) &&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData"))) &&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))) {
      # if (FALSE) {
      
      #load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
      #load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))
    } else {
      #if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))){
      #  file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
      #}
      #if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))){
      #  file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
      #}
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
      }
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
      }
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))
      }
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))
      }
      
      
      for (ext in AllExtents)
      {
        CarbonSelectedYear<-FullTable[FullTable$extent == ext,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedYear$geometry<-NULL
        CarbonSelectedSDYear<-FullTable[FullTable$extent == ext,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedSDYear$geometry<-NULL
        
        CarbonSelectedYear85<-FullTable[FullTable$extent == ext,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedYear85$geometry<-NULL
        CarbonSelectedSDYear85<-FullTable[FullTable$extent == ext,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedSDYear85$geometry<-NULL
        
        
        # PrecalcCarbonAllExtents[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        # PrecalcCarbonAllExtentsSD[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        
        PrecalcCarbonAllExtentsType[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        PrecalcCarbonAllExtentsSDType[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        
        PrecalcCarbonAllExtentsType2Lines[[ext]]<-matrix(0,2,dim(FullTable[FullTable$extent=="NoExtent",])[1])
        PrecalcCarbonAllExtentsSDType2Lines[[ext]]<-matrix(0,2,dim(FullTable[FullTable$extent=="NoExtent",])[1])
        
        # This precalculated table is used once when the map if first displayed. We use 2 identical lines to avoid issues with the
        # function call that expects a matrix in input.
        
        PrecalcCarbonAllExtentsType2Lines[[ext]][1,]<-CarbonSelectedYear85[,"Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear0"]
        PrecalcCarbonAllExtentsSDType2Lines[[ext]][1,]<-CarbonSelectedSDYear85[,"Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear0"]
        PrecalcCarbonAllExtentsType2Lines[[ext]][2,]<-PrecalcCarbonAllExtentsType2Lines[[ext]][1,]
        PrecalcCarbonAllExtentsSDType2Lines[[ext]][2,]<- PrecalcCarbonAllExtentsSDType2Lines[[ext]][1,]
        
        for(i in 1:dim(PrecalcCarbonAllExtentsType[[ext]])[1])
        {
          for(j in 1:dim(PrecalcCarbonAllExtentsType[[ext]])[2])
          {
            
            # PrecalcCarbonAllExtents[[ext]][i, j]<-CarbonSelectedYear[j,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",simul636YearType$YEAR[i, j])]
            #  PrecalcCarbonAllExtentsSD[[ext]][i, j]<-CarbonSelectedSDYear[j,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",simul636YearType$YEAR[i, j])]
            
            
            if(simul636YearType[["TYPE"]][i, j]=="NoPlanting"){
              PrecalcCarbonAllExtentsType[[ext]][i, j]<-0
              PrecalcCarbonAllExtentsSDType[[ext]][i, j]<-0
            }else{
              
              if(simul636YearType[["TYPE"]][i, j]=="Conifers"){
                PrecalcCarbonAllExtentsType[[ext]][i, j]<-CarbonSelectedYear[i,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",
                                                                                      simul636YearType$YEAR[i, j])]
                PrecalcCarbonAllExtentsSDType[[ext]][i, j]<-CarbonSelectedSDYear[i,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",
                                                                                          simul636YearType$YEAR[i, j])]
              }else{
                PrecalcCarbonAllExtentsType[[ext]][i, j]<-CarbonSelectedYear85[i,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",simul636YearType$YEAR[i, j])]
                PrecalcCarbonAllExtentsSDType[[ext]][i, j]<-CarbonSelectedSDYear85[i,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",simul636YearType$YEAR[i, j])]
                
              }
            }
          }
        }
      }
      
      #save(PrecalcCarbonAllExtents,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
      #save(PrecalcCarbonAllExtentsSD,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
      # Necessary
      save(PrecalcCarbonAllExtentsType,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
      save(PrecalcCarbonAllExtentsSDType,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
      save(PrecalcCarbonAllExtentsType2Lines,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))
      save(PrecalcCarbonAllExtentsSDType2Lines,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))
      
    }
    
    if (exists("SquaresLoad", inherits = FALSE)) {
      rm(SquaresLoad)
    }
    if (exists("Sqconv", inherits = FALSE)) {
      rm(Sqconv)
    }
    
    JulesMean <- 0;JulesSD <- 0;CorrespondenceJules <- 0;seer2km <- 0;jncc100 <- 0;speciesprob40 <- 0;climatecells <- 0
  })
  
  # Remove functions from environment (can't remove base functions though), they are already in the frontend and take a lot of space to return
  msg <- "Removing functions from environment ..."
  notif(msg, log_level = "debug")
  for (object_name in ls(new_environment)) {
    object <- get(object_name, envir = new_environment)
    if (isTRUE("function" %in% class(object))) {
      
      # Remove if not part of base package
      packages <- find(object_name)
      if (isFALSE("package:base" %in% packages)) {
        try(rm(list = object_name, envir = new_environment))
      }
      
    }
  }
  notif(paste(msg, "done"), log_level = "debug")
  plan(sequential)
  
  # Merge the environment into the global environment
  list2env(as.list(new_environment), envir = .GlobalEnv)
  
  rm(list = c("ElicitorAppFolder",
              "DataFilesFolder",
              "DownscalingImagesFolder",
              "CalculatedFilesFolder",
              "save_folder",
              "save_folder_elicitoroutput",
              "plantingtools_folder",
              "FolderSource"),
     envir = new_environment)
  
  notif("Backend initialization ... done")
  
  res$status <- 200
  return(new_environment)
}
