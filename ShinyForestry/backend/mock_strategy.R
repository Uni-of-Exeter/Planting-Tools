#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(sf)
library(uuid)
library(geojsonsf)

# Define file paths
setwd("/Users/paulwright/Documents/work/ADD-TREES/Planting-Tools/ShinyForestry/")
FolderSource <- normalizePath(getwd())
ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
CalculatedFilesFolder <- normalizePath(file.path(FolderSource, "CalculatedFiles"))

print("Loading data...")
FullTable <- st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
load(normalizePath(file.path(CalculatedFilesFolder, "simul636YearType.RData")))
load(normalizePath(file.path(CalculatedFilesFolder, "SubsetMeetTargets.RData")))
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
  is_blocked <- FALSE
  
  parcel_data <- st_sf(
    parcel_id = empty_parcel_data$parcel_id,
    geometry = empty_parcel_data$geometry,
    parcel_area = parcel_areas,
    planting_year = planting_years,
    planting_type = planting_types,
    is_blocked = is_blocked,
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
    planting_year = list(min = 2025, max = 2049),
    parcel_area = list(min = min(FullTable$area, na.rm = TRUE), max = max(FullTable$area, na.rm = TRUE))
  )
  return(slider_info)
}

# Example Outpuit
# 
# {
#   "planting_year": {
#     "min": [
#       2025
#     ],
#     "max": [
#       2049
#     ]
#   },
#   "parcel_area": {
#     "min": [
#       0.005
#     ],
#     "max": [
#       0.452
#     ]
#   }
# }


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


#* Generate parcel data
#* @post /generate_parcels
#* @param req The request body must contain `parcel_id` (string) and `blocked_until_year` (integer)
#* @serializer json
function(req) {
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) return(list(error = "Invalid JSON format."))
  )
  
  # Validate required fields
  if (!is.list(body) || !"blocked_until_year" %in% names(body) || !"parcel_id" %in% names(body)) {
    return(list(error = "Missing required fields: parcel_id and blocked_until_year."))
  }
  
  # Generate parcel data
  parcel_data <- generate_parcel_data()
  geojson <- geojsonsf::sf_geojson(parcel_data)
  
  return(geojson)
}


# Example Output
#                               parcel_id parcel_area planting_year planting_type is_blocked                       geometry
# 1  f4b4e4d0-7f09-4d26-a7b7-bab5b3151a57 0.022698955            NA          <NA>      FALSE POLYGON ((-1.756976 50.8314...
# 2  26841c10-5fdb-4988-8795-50ef9038ed5d 0.036774571          2026       Conifer      FALSE POLYGON ((-1.766385 50.8160...
# 3  cb4fdd41-11ac-426e-b4d6-c04ea47ea14d 0.034369548            NA          <NA>      FALSE POLYGON ((-1.765671 50.8316...
# 4  cae4aa35-d2a9-415c-86ef-873d281855d3 0.027595724            NA          <NA>      FALSE POLYGON ((-1.759141 50.8113...
# 5  bd20ad23-6046-46b8-8efa-ddb018d6d865 0.009152795          2035     Deciduous      FALSE POLYGON ((-1.759423 50.8109...
# 6  26864357-0046-448e-8b08-ebf00c61fa0a 0.021871169          2048       Conifer      FALSE POLYGON ((-1.761 50.83261, ...
# 7  447118a9-ede7-48cb-af07-c5fa822193e5 0.015572843            NA          <NA>      FALSE POLYGON ((-1.763823 50.825,...
# 8  2a110f23-eb3c-4608-a2e6-28c4e2fd673e 0.017445100            NA          <NA>      FALSE POLYGON ((-1.762628 50.8344...
# 9  3451c88f-3298-4159-89d9-7bd79b4ff513 0.015956941            NA          <NA>      FALSE POLYGON ((-1.762547 50.8200...
# 10 fa5558f3-a884-44cd-a9db-6b1876ff68cc 0.035769157          2030     Deciduous      FALSE POLYGON ((-1.760264 50.8301...


# Run this file with plumber: `plumber::plumb("ShinyForestry/backend/mock_strategy.R")$run(port=8010)`