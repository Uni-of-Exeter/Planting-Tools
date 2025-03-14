library(plumber)
library(sf)
library(uuid)
library(geojsonsf)

# Define file paths
FolderSource <- normalizePath(getwd())
ElicitorAppFolder <- normalizePath(file.path(FolderSource, "../../ShinyForestry/ElicitorOutput"))
CalculatedFilesFolder <- normalizePath(file.path(FolderSource, "../../ShinyForestry/CalculatedFiles"))

print("Loading data...")
FullTable <- st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
print("Data loading complete.")

# Global variable for empty parcels
empty_parcel_data <- NULL

# Function to generate empty parcel data
generate_empty_parcel_data <- function(FullTable) {
  if (!inherits(FullTable, "sf")) {
    stop("FullTable must be an sf object.")
  }
  
  n <- nrow(FullTable)
  parcel_ids <- sapply(1:n, function(x) UUIDgenerate())
  geometries <- st_geometry(FullTable)
  
  empty_parcel_data <<- st_sf(
    parcel_id = parcel_ids,
    geometry = geometries,
    crs = st_crs(FullTable)
  )
}

generate_empty_parcel_data(FullTable)

# Function to generate parcel data
generate_parcel_data <- function() {
  if (is.null(empty_parcel_data)) {
    stop("Empty parcel data has not been initialized.")
  }
  
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
  
  return(parcel_data)
}

#* @apiTitle Parcel Data API
#* @apiDescription Returns parcel data in GeoJSON format.

#* @get /health
#* @serializer json
function() {
  list(status = "OK", message = "Plumber API is running")
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

#* Generate empty parcels
#* @get /empty_parcels
#* @serializer json
function() {
  geojson <- geojsonsf::sf_geojson(empty_parcel_data)
  return(geojson)
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
  parcel_data <- generate_parcel_data()
  
  # Update blocked parcels in the backend logic
  for (i in 1:nrow(blocked_parcels)) {
    parcel <- as.data.frame(blocked_parcels[i, , drop = FALSE])  # Convert to data frame row
    parcel_data$is_blocked[parcel_data$parcel_id == parcel$parcel_id] <- TRUE
    parcel_data$blocked_until_year[parcel_data$parcel_id == parcel$parcel_id] <- parcel$blocked_until_year
  }
  
  # Create dummy payload
  payload <- list(
    carbon = as.numeric(body$carbon) * runif(1, min = 0.9, max = 1.1),
    species = as.numeric(body$species) * runif(1, min = 0.9, max = 1.1),
    species_goat_moth = as.numeric(body$species_goat_moth) * runif(1, min = 0.9, max = 1.1),
    species_stag_beetle = as.numeric(body$species_stag_beetle) * runif(1, min = 0.9, max = 1.1),
    species_lichens = as.numeric(body$species_lichens) * runif(1, min = 0.9, max = 1.1),
    area = as.numeric(body$area) * runif(1, min = 0.9, max = 1.1),
    recreation = as.numeric(body$recreation) * runif(1, min = 0.9, max = 1.1)
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
  parcel_data <- generate_parcel_data() # 
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

# use parse/serializer
# plumber::serializer_geojson()

#* Generate parcel data for Alternative Approaches tab
#* @get /four_random_strategies
#* @serializer json
function() {
  generate_random_strategy <- function() {
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
    
    return(list(values = values, geojson = geojson))
  }
  
  # Generate four random strategies
  strategies <- replicate(4, generate_random_strategy(), simplify = FALSE)
  
  # Return the strategies
  return(list(
    strategy_1 = strategies[[1]],
    strategy_2 = strategies[[2]],
    strategy_3 = strategies[[3]],
    strategy_4 = strategies[[4]]
  ))
}


#* Generate two random strategies after receiving a user choice (1 or 2)
#* @post /preference_choice
#* @param choice Integer value (1 or 2)
function(choice) {
  generate_random_strategy <- function() {
    # Generate parcel data
    parcel_data <- generate_parcel_data()
    geojson <- geojsonsf::sf_geojson(parcel_data)
    
    # Create the values for the strategy
    values <- list(
      carbon = as.numeric(500),
      species = as.numeric(5), 
      species_goat_moth = as.numeric(4),
      species_stag_beetle = as.numeric(9),
      species_lichens = as.numeric(10),
      area = as.numeric(15),
      recreation = as.numeric(10)
    )
    
    # Add a small random variation between -0.5 and 0.5 to each value
    values <- lapply(values, function(x) x + runif(1, -0.5, 0.5))
    
    return(list(values = values, geojson = geojson))
  }
  
  # Validate the choice input (1 or 2)
  if (!(choice %in% c(1, 2))) {
    return(list(error = "Invalid input: 'choice' must be 1 or 2"))
  }
  
  # Generate two random strategies
  strategies <- replicate(2, generate_random_strategy(), simplify = FALSE)
  
  # Return the generated strategies
  return(list(
    strategy_1 = strategies[[1]],
    strategy_2 = strategies[[2]]
  ))
}

# Run this file with plumber: `plumber::plumb("ShinyForestry/backend/mock_strategy.R")$run(port=8010)`




# See 228


#* Submit targets to return a strategy
#* @POST /submit_targets
#* @serializer json
function() {

  # Takes in:
  # {
  #   "carbon": 852,
  #   "species": 18,
  #   "species_goat_moth": 91,
  #   "species_stag_beetle": 22,
  #   "species_lichens": 4,
  #   "area": 5,
  #   "recreation": 16,
  # } 
  
  # runs: submit_button()
  
  # returns:
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
}

#* Initialise Preferences tab
#* @GET /preferences_initialise
#* @serializer json
function() {
  
  # Takes in:
  # {
  #   "carbon": 852,
  #   "species": 18,
  #   "species_goat_moth": 91,
  #   "species_stag_beetle": 22,
  #   "species_lichens": 4,
  #   "area": 5,
  #   "recreation": 16,
  # } 
  
  # runs: preferences_tab_first_click()
  
  # returns: (see submit_strategy)
  # list(
  #     list(values, geojson),
  #     list(values, geojson),
  # )
}

#* Provide a preference (1 or 2)
#* @POST /preferences
#* @serializer json
function() {
  
  # Takes in: either 1 or 2

  # runs: choose_button()
  
  # returns: (see preferences_initialise)
  # list(
  #     list(values, geojson),
  #     list(values, geojson),
  # )
}

#* Obtain four alternative approaches
#* @GET /alternative_approaches
#* @serializer json
function() {
  # returns:
  # list(
  #     list(values, geojson),
  #     list(values, geojson),
  #     list(values, geojson),
  #     list(values, geojson),
  # )
}

#* Obtain four alternative approaches
#* @GET /exploration_initialise
#* @serializer json
function() {
  # input: a number from 1 - 4 for the cluster picked on the alternative_approaches tab
  
  # returns:
  # list(values, geojson) #see submit_strategy
}

#* Obtain four alternative approaches
#* @GET /exploration_plus
#* @serializer json
function() {
  # input: 
  #  slider_name
  
  # runs plus_button()
  
  # returns:
  # list(values, geojson) #see submit_strategy
}

#* Obtain four alternative approaches
#* @GET /exploration_minus
#* @serializer json
function() {
  # input: 
  #  slider_name
  
  # runs minus_button() if "-"
  
  # returns:
  # list(values, geojson) #see submit_strategy
}




