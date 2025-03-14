library(data.table)
library(jsonlite)

# Function to process initialization$first_strategy
process_first_strategy <- function(strategy) {
  # Extract values and geojson from the first_strategy
  values <- strategy$values
  geojson <- strategy$geojson
  geojson_parsed <- st_read(geojson, quiet=TRUE)
  
  # Debug: Check if values is a data.table and if it's not empty
  if (length(values) == 0 || !inherits(values, "data.table")) {
    stop("The 'values' data is either missing or not a data.table.")
  }
  
  # Convert the data.table row into a named list (assuming only one row)
  values_list <- as.list(values[1, ])  # Extract the first row and convert to list
  
  # Set the names of the list to match the column names of the data.table
  names(values_list) <- colnames(values)
  
  # Construct the final output as a list containing values and geojson
  output <- list(
    geojson = geojson_parsed,  # Geojson object passed as is
    values = values_list
  )
  
  return(output)
}

# POST to /generate_parcels
post_submit_targets <- function(json_payload) {
  
  url <- paste0(API_URL, "/submit_targets")
  
  # Make the API POST request with JSON payload
  response <- httr::POST(
    url,
    body = json_payload, 
    encode = "json",
    httr::content_type_json()
  )
  
  print(">>>>>>>>>>>>")
  print("json_payload")
  print(json_payload)
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    geojson <- api_response$geojson
    geojson_parsed <- st_read(geojson, quiet=TRUE)
    
    values <- api_response$values
    
    print("json_response")
    print("geojson")
    print(geojson_parsed)
    print("values")
    print(values) 
    print("<<<<<<<<<<<<<")
    
    return(list(geojson_parsed, values))
    
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

get_exploration_initialise <- function(cluster) {
  
  cluster <- as.integer(cluster)
  print(paste("The cluster choice was made:", cluster))
  
  # Define the base URL (without query parameters), append which_button query parameter
  url <- paste0(API_URL, "/preferences?which_cluster=", cluster)  # Add choice as query parameter
  
  # Make the API POST request with no JSON body
  response <- httr::GET(
    url,
    httr::content_type_json()  # Set the content-type header to application/json (if needed)
  )
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    print("api_response$values")
    print(api_response$values)
    
    # Return the list containing both sets of values and geojson data
    list(values = api_response$values, geojson = sf::st_read(api_response$geojson, quiet = TRUE))
    
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

get_alternative_approaches <- function() {
  
  url <- paste0(API_URL, "/alternative_approaches")
  
  # Make the API request
  response <- httr::GET(url)
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    # Parse the response content
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    # Return a list of lists containing values and geojson data for both options
    list(
      list(values = api_response$values[[1]], geojson = sf::st_read(api_response$geojson[[1]], quiet = TRUE)),
      list(values = api_response$values[[2]], geojson = sf::st_read(api_response$geojson[[2]], quiet = TRUE)),
      list(values = api_response$values[[3]], geojson = sf::st_read(api_response$geojson[[3]], quiet = TRUE)),
      list(values = api_response$values[[4]], geojson = sf::st_read(api_response$geojson[[4]], quiet = TRUE))
    )
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

get_preferences_initialise <- function(){
  
  url <- paste0(API_URL, "/preferences_initialise")
  
  # Make the API request
  response <- httr::GET(url)
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    # Parse the response content
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    # Return a list of lists containing values and geojson data for both options
    list(
      list(values = api_response$values[[1]], geojson = sf::st_read(api_response$geojson[[1]], quiet = TRUE)),
      list(values = api_response$values[[2]], geojson = sf::st_read(api_response$geojson[[2]], quiet = TRUE))
    )
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

post_preference_choice <- function(choice) {
  
  choice <- as.integer(choice)
  print(paste("The choice was made:", choice))
  
  # Define the base URL (without query parameters), append which_button query parameter
  url <- paste0(API_URL, "/preferences?which_button=", choice)  # Add choice as query parameter
  
  # Make the API POST request with no JSON body
  response <- httr::GET(
    url,
    httr::content_type_json()  # Set the content-type header to application/json (if needed)
  )
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    print("api_response$values")
    print(api_response$values)
    
    # Return the list containing both sets of values and geojson data
    list(
      list(values = api_response$values[[1]], geojson = sf::st_read(api_response$geojson[[1]], quiet = TRUE)),
      list(values = api_response$values[[2]], geojson = sf::st_read(api_response$geojson[[2]], quiet = TRUE))
    )
    
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}


# GET to /slider_values
get_slider_values <- function() {
  url <- paste0(API_URL, "/slider_values")
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    print(content_raw)
    return(api_response)
  } 
  else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}