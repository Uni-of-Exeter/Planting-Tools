library(httr)
library(jsonlite)
library(glue)
library(sf)

API_PORT <- Sys.getenv("API_PORT")
if (API_PORT == "") {
  API_PORT <- 40000
}
API_HOST <- Sys.getenv("API_HOST")
if (API_HOST == "") {
  API_HOST <- "144.173.60.164"
}

API_URL <- paste0("http://", API_HOST, ":", API_PORT)

put_initialization <- function() {
  
  url <- paste0(API_URL, "/initialize")
  
  # Convert 'info' to JSON format
  body <- toJSON(
    list(MAX_LIMIT_LOG_LEVEL = "info")    
    )
  
  # Make the PUT request
  response <- httr::PUT(
    url, 
    body = body, 
    encode = "json",
    content_type_json()
  )
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    # Read raw RDS content
    rds_raw <- httr::content(response, as = "raw")
    
    # Save to a temporary file
    tmp_file <- tempfile(fileext = ".rds")
    writeBin(rds_raw, tmp_file)
    
    # Load the RDS object
    api_response <- readRDS(tmp_file)
    
    if (is.environment(api_response)) {
      api_response <- mget(ls(api_response), envir = api_response)
    }
    
    return(api_response)
  } 
  else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

frontend_initialisation <- funtion() {
  initialization <- put_initialization()
  values <- list(
    targets = convert_targets_to_english(initialization$TARGETS, initialization$NAME_CONVERSION),
    year <- list(
      min = initialization$STARTYEAR,
      max = initialization$STARTYEAR + initialization$MAXYEAR,
      default = initialization$STARTYEAR
    ),
    # slider_info <- list(
    #   min_max_default = data.table(
    #     carbon = c(0, 10, 4),
    #     species = c(0, 10, 4),
    #     species_goat_moth = c(0, 10, 4),
    #     species_stag_beetle = c(0, 10, 4),
    #     species_lichens = c(0, 10, 4),
    #     area = c(0, 10, 4),
    #     recreation = c(0, 10, 4)),
    #   units = data.table(
    #     carbon = "tCO₂",
    #     species = "%",
    #     species_goat_moth = "%",
    #     species_stag_beetle = "%",
    #     species_lichens = "%",
    #     area = "km²",
    #     recreation = "10³Kcal")
    # )
  )
  return(targets)
}

# Perform name conversion
convert_targets_to_english <- function(targets, name_conversion) {
  # Create a lookup table from Specie to English_specie
  lookup_table <- setNames(name_conversion$English_specie, name_conversion$Specie)
  # Replace targets with their English names where applicable
  converted_targets <- ifelse(targets %in% names(lookup_table), lookup_table[targets], targets)
  return(converted_targets)
}
targets <- convert_targets_to_english(initialization$TARGETS, initialization$NAME_CONVERSION)

}