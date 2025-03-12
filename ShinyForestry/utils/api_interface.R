library(httr)
library(jsonlite)
library(glue)
library(sf)

put_initialization <- function(info) {
  
  url <- glue("http://144.173.60.164:40000/initialize")
  
  # Convert 'info' to JSON format
  body <- toJSON(
    list(MAX_LIMIT_LOG_LEVEL = "info"), 
    auto_unbox = TRUE
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

initialization <- put_initialization()


frontend_initialisation <- funtion(){
  
  initialization <- put_initialization()
  
  values <- list(
    targets = convert_targets_to_english(initialization$TARGETS, initialization$NAME_CONVERSION),
    year <- list(
      min = initialization$STARTYEAR,
      max = initialization$STARTYEAR + initialization$MAXYEAR,
      default = initialization$STARTYEAR
    ),
    # slider_info <- list(
    #   carbon = list(min = 500, max = 1000, default = 800),
    #   species = list(min = 0, max = 25, default = 10),
    #   species_goat_moth = list(min = 0, max = 100, default = 25),
    #   species_stag_beetle = list(min = 0, max = 100, default = 30),
    #   species_lichens = list(min = 0, max = 5, default = 2),
    #   area = list(min = 0, max = 15, default = 10),
    #   recreation = list(min = 0, max = 20, default = 15)
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