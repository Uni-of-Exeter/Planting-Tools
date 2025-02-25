library(plumber)

# plumb("plumber.R") |>
#   pr_run(port = 5762) # Specify API port

normalizePath <- function(path, winslash = "\\") {
  base::normalizePath(path, winslash = winslash, mustWork = FALSE)
}

#* Receive user shapefile zip
#* curl -X POST "localhost/upload_shapefile_zip" -H "accept: */*" -H "Content-Type: multipart/form-data" -F "upload=@land_parcels.shp.zip;type=application/x-zip-compressed"
#* @post /upload_shapefile_zip
#* @param file Zip shapefile
#* @param userid User identifier to store data in folder
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
#* @response 401 Unauthorized: User id is not an integer
function(res, file, userid) {
  
  if (isFALSE(is.integer(userid))) {
    res$status <- 401
    return("userid is not an integer")
  }
  
  user_folder <- normalizePath(file.path("..", paste0("userid_", userid)))
  elicitor_outout_folder <- normalizePath(file.path(user_folder, "ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    res$status <- 400
    res$body <- jsonlite::toJSON(list(
      status = 403, # Repeated per the blog post
      message = "An error message"
    ))
    return(paste("Bad request: The file name is incorrect, it should be land_parcels.shp.zip and you sent", filename))
  }
  file_path <- normalizePath(file.path(user_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  res$status <- 200
  return("Success")
}

#* Receive user outcomes.json
#* curl -X POST "localhost/upload_outcomes" -H "accept: */*" -H "Content-Type: application/json" -F "upload=@outcomes.json"
#* @post /upload_outcomes
#* @param file outcomes.json
#* @param userid User identifier to store data in folder
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
#* @response 401 Unauthorized: User id is not an integer
function(res, file, userid) {
  
  if (isFALSE(is.integer(userid))) {
    res$status <- 401
    return("userid is not an integer")
  }
  
  user_folder <- normalizePath(file.path("..", paste0("userid_", userid)))
  elicitor_outout_folder <- normalizePath(file.path(user_folder, "ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    res$status <- 400
    return(paste("Bad request: The file name is incorrect, it should be outcomes.json and you sent", filename))
  }
  file_path <- normalizePath(file.path(user_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  res$status <- 200
  return("Success")
}

#* Receive user decision_units.json
#* curl -X POST "localhost/upload_decision_units" -H "accept: */*" -H "Content-Type: application/json" -F "upload=@decision_units.json"
#* @post /upload_decision_units
#* @param file decision_units.json
#* @param userid User identifier to store data in folder
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
#* @response 401 Unauthorized: User id is not an integer
function(res, file, userid) {
  
  if (isFALSE(is.integer(userid))) {
    res$status <- 401
    return("userid is not an integer")
  }
  
  user_folder <- normalizePath(file.path("..", paste0("userid_", userid)))
  elicitor_outout_folder <- normalizePath(file.path(user_folder, "ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    res$status <- 400
    return(paste("Bad request: The file name is incorrect, it should be decision_units.json and you sent", filename))
  }
  file_path <- normalizePath(file.path(user_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  res$status <- 200
  return("Success")
}

#* @filter manage_environment
function(req, res, userid) {
  # Ensure userid is provided
  if (missing(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing required parameter: userid"))
  }
  
  # Define the folder path for the user's environment
  user_folder <- file.path("user_data", as.character(userid))
  dir.create(user_folder, showWarnings = FALSE, recursive = TRUE)
  
  # Define the environment file path
  env_file <- file.path(user_folder, "environment.RData")
  
  # Unload all objects and packages
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  suppressWarnings(
    lapply(setdiff(loadedNamespaces(), c("base", "stats", "utils", "graphics", "grDevices", "methods")), unloadNamespace)
  )
  
  # Load user-specific environment if it exists
  if (file.exists(env_file)) {
    load(env_file, envir = .GlobalEnv)
  }
  
  # Attach the file path to the request for saving later
  req$env_file <- env_file
  
  forward()
}

#* @postroute /save_environment
function(req) {
  # Save the current environment to the user-specific file
  if (!is.null(req$env_file)) {
    save(list = ls(envir = .GlobalEnv), envir = .GlobalEnv, file = req$env_file)
  }
}

#* Generate all files needed for frontend (in CalculatedFiles and ElicitorOutput)
#* curl -X GET "localhost/calculate_pre_server_block"
#* @post /pre_calculation_before_server_block
#* @param userid User identifier to store data in folder
#* @response 200 Success: The calculations are done
#* @response 400 Bad request
#* @response 401 Unauthorized: User id is not an integer
function(res, file, userid) {
  
  if (isFALSE(is.integer(userid))) {
    res$status <- 401
    return("userid is not an integer")
  }
  
  user_folder <- normalizePath(file.path("..", paste0("userid_", userid)))
  elicitor_outout_folder <- normalizePath(file.path(user_folder, "ElicitorOutput"))
  
  # Are any required files missing
  land_parcels_missing <- isFALSE(file.exists(file.path(elicitor_outout_folder, "land_parcels.shp.zip")))
  outcomes_missing <- isFALSE(file.exists(file.path(elicitor_outout_folder, "outcomes.json")))
  decisions_units_missing <- isFALSE(file.exists(file.path(elicitor_outout_folder, "decision_units.json")))
  if (land_parcels_missing || outcomes_missing || decisions_units_missing) {
    res$status <- 400
    missing_files <- ""
    if (land_parcels_missing) missing_files <- paste(missing_files, "land_parcels.shp.zip")
    if (outcomes_missing) missing_files <- paste(missing_files, "outcomes.json")
    if (decisions_units_missing) missing_files <- paste(missing_files, "decision_units.json")
    return(paste("Bad request: Please upload", missing_files))
  }

}

