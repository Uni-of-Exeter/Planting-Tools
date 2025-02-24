library(plumber)

normalizePath <- function(path, winslash = "\\") {
  base::normalizePath(path, winslash = winslash, mustWork = FALSE)
}

#* Receive user shapefile zip: curl -X POST "localhost/data" -H "accept: */*" -H "Content-Type: multipart/form-data" -F "upload=@land_parcels.shp.zip;type=application/x-zip-compressed"
#* @post /upload_shapefile_zip
#* @param file Zip shapefile
#* @param userid User identifier to store data in folder
function(file, userid) {
  
  user_folder <- normalizePath(file.path("..", paste0("userid_", userid)))
  elicitor_outout_folder <- normalizePath(file.path(user_folder, "ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    return(FALSE)
  }
  file_path <- normalizePath(file.path(user_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  return(TRUE)
}

#* Receive user shapefile zip: curl -X POST "localhost/data" -H "accept: */*" -H "Content-Type: application/json" -F "upload=@outcomes.json"
#* @post /upload_outcomes
#* @param file outcomes.json
#* @param userid User identifier to store data in folder
function(file, userid) {
  
  user_folder <- normalizePath(file.path("..", paste0("userid_", userid)))
  elicitor_outout_folder <- normalizePath(file.path(user_folder, "ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    return(FALSE)
  }
  file_path <- normalizePath(file.path(user_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  return(TRUE)
}

#* Receive user shapefile zip: curl -X POST "localhost/data" -H "accept: */*" -H "Content-Type: application/json" -F "upload=@decision_units.json"
#* @post /upload_decision_units
#* @param file decision_units.json
#* @param userid User identifier to store data in folder
function(file, userid) {
  
  user_folder <- normalizePath(file.path("..", paste0("userid_", userid)))
  elicitor_outout_folder <- normalizePath(file.path(user_folder, "ElicitorOutput"))
  dir.create(elicitor_outout_folder, recursive = TRUE)
  
  # Get zip file from binary content
  filename <- names(file)
  if (filename != "land_parcels.shp.zip") {
    return(FALSE)
  }
  file_path <- normalizePath(file.path(user_folder, filename))
  file.remove(file_path)
  content <- file[[1]]
  writeBin(content, file_path)
  
  return(TRUE)
}
