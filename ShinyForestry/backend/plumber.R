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
function(res, file, userid) {
  
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
function(res, file, userid) {
  
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
function(res, file, userid) {
  
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

#* Generate all files needed for frontend (in CalculatedFiles and ElicitorOutput)
#* curl -X GET "localhost/calculate_pre_server_block"
#* @post /upload_decision_units
#* @param file decision_units.json
#* @param userid User identifier to store data in folder
#* @response 200 Success: The calculations are done
#* @response 400 Bad request: Some files are missing
function(res, file, userid) {
  
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

