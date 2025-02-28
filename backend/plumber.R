library(plumber)
library(jsonlite)
library(future)

# plumb(normalizePath(file.path("backend", "plumber.R"))) |>
#   pr_run(port = 5762) |> # Specify API port
#   pr_set_debug(debug = TRUE)

normalizePath <- function(path, winslash = "\\") {
  base::normalizePath(path, winslash = winslash, mustWork = FALSE)
}

#* Receive user shapefile zip
#* curl -X PUT "localhost/upload_shapefile_zip" -H "accept: */*" -H "Content-Type: multipart/form-data" -F "upload=@land_parcels.shp.zip;type=application/x-zip-compressed"
#* @put /upload_shapefile_zip
#* @param file Zip shapefile
#* @param userid User identifier to store data in folder
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
function(res, file) {
  
  elicitor_outout_folder <- normalizePath(file.path("ElicitorOutput"))
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
  file_path <- normalizePath(file.path(elicitor_outout_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  res$status <- 200
  return("Success")
}

#* Receive user outcomes.json
#* curl -X PUT "localhost/upload_outcomes" -H "accept: */*" -H "Content-Type: application/json" -F "upload=@outcomes.json"
#* @put /upload_outcomes
#* @param file outcomes.json
#* @param userid User identifier to store data in folder
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
function(res, file) {
  
  elicitor_outout_folder <- normalizePath(file.path("ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    res$status <- 400
    return(paste("Bad request: The file name is incorrect, it should be outcomes.json and you sent", filename))
  }
  file_path <- normalizePath(file.path(elicitor_outout_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  res$status <- 200
  return("Success")
}

#* Receive user decision_units.json
#* curl -X PUT "localhost/upload_decision_units" -H "accept: */*" -H "Content-Type: application/json" -F "upload=@decision_units.json"
#* @put /upload_decision_units
#* @param file decision_units.json
#* @param userid User identifier to store data in folder
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
function(res, file) {
  
  elicitor_outout_folder <- normalizePath(file.path("ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    res$status <- 400
    return(paste("Bad request: The file name is incorrect, it should be decision_units.json and you sent", filename))
  }
  file_path <- normalizePath(file.path(elicitor_outout_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  res$status <- 200
  return("Success")
}

#* Code before server block
#* curl -X PUT
#* @put /initialization
#* @response 200 Success: Initialized the app, did pre-processing
#* @response 403 Forbidden: Missing one or more input files from the elicitor
function() {
  new_environment <- new.env()
  with(new_environment, {
    b <- 3
  })
  return(new_environment)
}









