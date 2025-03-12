# POST to /generate_parcels
post_generate_parcels <- function(json_payload) {
  
  url <- glue("http://{API_HOST}:{API_PORT}/generate_parcels")
  
  # Make the API POST request with JSON payload
  response <- httr::POST(
    url,
    body = json_payload, 
    encode = "json",
    httr::content_type_json()
  )
  
  print(json_payload)
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

get_random_strategy <- function() {
  
  url <- glue("http://{API_HOST}:{API_PORT}/random_strategy")
  # Make the API request
  response <- httr::GET(url)
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    geojson <- api_response$geojson
    geojson_parsed <- st_read(geojson, quiet=TRUE)
    
    values <- api_response$values
    return(list(geojson_parsed, values))
  } 
  else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}

get_four_random_strategies <- function() {
  
  url <- glue("http://{API_HOST}:{API_PORT}/four_random_strategies")
  
  # Make the API request
  response <- httr::GET(url)
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    # Extract and parse the geojson and values for each strategy
    strategies <- lapply(1:4, function(i) {
      geojson <- api_response[[i]]$geojson
      geojson_parsed <- tryCatch({
        sf::st_read(geojson, quiet = TRUE)
      }, error = function(e) {
        message(paste("Error parsing GeoJSON for strategy", i, ":", e$message))
        NULL  # Return NULL if parsing fails
      })
      values <- api_response[[i]]$values
      return(list(values = values, geojson = geojson_parsed))
    })
    
    # Return the strategies
    return(strategies)
    
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}



post_preference_choice <- function(choice) {
  
  choice <- as.integer(choice)
  print(paste("The choice was made:", choice))
  
  # Define the base URL (without query parameters)
  url <- glue("http://127.0.0.1:8042/preference_choice")
  
  # Create the JSON payload with the 'choice' as part of the body
  json_payload <- list(choice = choice)
  
  # Make the API POST request with the choice in the body
  response <- httr::POST(
    url,
    body = jsonlite::toJSON(json_payload, auto_unbox = TRUE),  # Convert choice to JSON
    encode = "json",  # Specify JSON encoding
    httr::content_type_json()  # Set the content-type header to application/json
  )
  
  # Check if the response is successful
  if (httr::status_code(response) == 200) {
    
    content_raw <- httr::content(response, "text", encoding = "UTF-8")
    api_response <- jsonlite::fromJSON(content_raw)
    
    # Parse the GeoJSON and values from the response
    geojson_1 <- api_response[[1]]$geojson
    geojson_2 <- api_response[[2]]$geojson
    geojson_parsed_1 <- sf::st_read(geojson_1, quiet = TRUE)
    geojson_parsed_2 <- sf::st_read(geojson_2, quiet = TRUE)
    
    values_1 <- api_response[[1]]$values
    values_2 <- api_response[[2]]$values
    
    Sys.sleep(1)
    
    # Return the list containing both sets of values and geojson data
    return(list(
      list(values = values_1, geojson = geojson_parsed_1),
      list(values = values_2, geojson = geojson_parsed_2)
    ))
    
  } else {
    stop(paste("Request failed with status:", httr::status_code(response)))
  }
}




# GET to /slider_values
get_slider_values <- function() {
  url <- glue("http://{API_HOST}:{API_PORT}/slider_values")
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