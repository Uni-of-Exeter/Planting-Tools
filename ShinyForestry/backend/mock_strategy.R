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

# Function to generate parcel data
generate_parcel_data <- function(FullTable) {
  if (!inherits(FullTable, "sf")) {
    stop("FullTable must be an sf object.")
  }

  n <- nrow(FullTable)
  print(colnames(FullTable))
  parcel_ids <- sapply(1:n, function(x) UUIDgenerate())
  geometries <- st_geometry(FullTable)
  parcel_areas <- FullTable$area
  planting_years <- sample(2025:2049, n, replace = TRUE)
  planting_types <- sample(c("Deciduous", "Conifer", NA), n, replace = TRUE)
  planting_years[is.na(planting_types)] <- NA
  is_blocked <- FALSE
  
  parcel_data <- st_sf(
    parcel_id = parcel_ids,
    geometry = geometries,
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

#* Generate parcel data
#* @get /generate_parcels
#* @serializer json
function() {
  parcel_data <- generate_parcel_data(FullTable)
  
  # Use sf_geojson to return a proper GeoJSON response
  geojson <- geojsonsf::sf_geojson(parcel_data)
  
  # Return the proper GeoJSON
  return(geojson)
}

# Run this file with plumber: `plumber::plumb("ShinyForestry/backend/strategy.R")$run(port=8001)`
