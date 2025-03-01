library(plumber)
library(jsonlite)
library(future)

normalizePath <- function(path, winslash = "\\") {
  base::normalizePath(path, winslash = winslash, mustWork = FALSE)
}

#* Receive user shapefile zip
#* curl -X PUT -H "Content-Type: multipart/form-data" -F "upload=@land_parcels.shp.zip;type=application/x-zip-compressed" localhost/upload_shapefile_zip
#* @put /upload_shapefile_zip
#* @param file Zip shapefile
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
#* curl -X PUT -H "Content-Type: application/json" -F "upload=@outcomes.json" localhost/upload_outcomes
#* @put /upload_outcomes
#* @param file outcomes.json
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
#* curl -X PUT -H "Content-Type: application/json" -F "upload=@decision_units.json" localhost/upload_decision_units
#* @put /upload_decision_units
#* @param file decision_units.json
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

#* Check if user input file exists and matches the user's file
#* curl -X GET localhost/exists?filename=land_parcels.shp.zip&md5sum=ekglnswl
#* @get /exists
#* @param filename Name of user input file
#* @param md5sum md5 value of the file
#* @response 200 Success: The file exists and is correct
#* @response 400 Bad request: The file hash is incorrect
#* @response 404 Not found: The file was not found
function(res, filename, md5sum) {
  
  file <- file.path("ElicitorOutput", filename)
  if (file.exists(file)) {
    
    hash <- tools::md5sum(file)
    if (hash != md5sum) {
      res$status <- 400
      res$setHeader("Expected-MD5-Hash", hash)
      return("File hash mismatch. Expected hash is in header Expected-MD5-Hash")
    } else {
      res$status <- 200
      return(paste(filename, "exists")) 
    }
    
  } else {
    
    res$status <- 404
    return(paste(filename, "does not exist"))
    
  }
}

#* Code before server block. Returns an environment
#* curl -X PUT -H "Accept: text/plain" localhost/initialization
#* @put /initialization
#* @response 200 Success: Initialized the app, did pre-processing
#* @response 403 Forbidden: Missing one or more input files from the elicitor
#* @response 500 Internal Server Error: One of the elicitor files is not readable
function(res) {
  new_environment <- new.env()
  with(new_environment, {
    ANALYSISMODE<-FALSE
    SHOW_TITLES_ON_CLUSTERING_PAGE<-F
    
    RUN_BO<-FALSE
    RNGversion("4.0.0")
    set.seed(1)
    #fixed strategies list contains strategies pre-selected to be shown in the preference elicitation
    FIXED_STRATEGIES_LIST<-list(YEAR=matrix(0,0,1),TYPE=matrix(0,0,1),OUTPUTS=matrix(0,0,1))
    POLYGON_OPACITY<-0.6
    GREY_BACKGROUND_OPACITY<-0.3
    NOTAVAIL_OPACITY<-0.7
    
    BACKEND_HOST_OR_IP <- "localhost"
    
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    os <- tolower(os)
    if (os == "windows" || isTRUE(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())) {
      futureplan <- future::multisession
      
    } else {
      futureplan <- future::multicore
    }
    # future:::ClusterRegistry("stop")
    
    # more --> less: debug / info / warning / error / none
    LOG_LEVEL <- "error"
    
    # FolderSource <- "ShinyForestry/"
    FolderSource <- normalizePath(file.path(getwd(), "..", "ShinyForestry"))
    # if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
    #   FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
    # }
    
    STARTYEAR<-2025
    MAXYEAR<-2050-STARTYEAR-1
    SCENARIO <- 26
    
    # Delete log and lock files
    unlink(base::normalizePath(file.path(FolderSource, "log*"), mustWork = FALSE))
    unlink(base::normalizePath(file.path(FolderSource, "*lockfile*"), mustWork = FALSE))
    unlink(base::normalizePath(file.path(FolderSource, "task_id*"), mustWork = FALSE))
    
    # Overridden in server() block, necessary for source(...)
    SESSION_FILE_SUFFIX <- ""
    source(normalizePath(file.path(FolderSource, "functions.R")), local = TRUE)
    source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
    source(normalizePath(file.path(FolderSource, "preferTrees.R")), local = FALSE)
    
    
    # Retrieve packages from DESCRIPTION (in plantingtools_folder)
    plantingtools_folder <- normalizePath(file.path(FolderSource, ".."))
    packages <- read.dcf(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))[, "Imports"]
    packages <- unlist(strsplit(packages, ",\\s*"))  # Split and flatten
    packages <- gsub("\\s*\\(.*\\)", "", packages)  # Remove version constraints
    packages <- na.omit(packages)  # Remove any NAs
    
    # # Install and load packages in DESCRIPTION
    # if (Sys.getenv("USERNAME")=="bn267" || Sys.getenv("USERNAME")=="dw356") {
    #   library("dgpsi")
    #   library("RRembo")
    #   for(ll in 1:length(packages)) {
    #     library(packages[ll], character.only = TRUE)
    #   }
    # } else {
    #   if (isFALSE(require("remotes"))) {
    #     install.packages('remotes', repos = 'https://cran.rstudio.com')
    #     library(remotes)
    #   }
    #   
    #   msg <- "Installing all packages ..." 
    #   notif(msg)
    #   remotes::install_deps(pkgdir = plantingtools_folder, repos = 'https://cran.rstudio.com')
    #   
    #   msg <- paste(msg, "done")
    #   notif(msg)
    #   
    #   msg <- "Loading all packages ..." 
    #   notif(msg)
    #   
    #   sapply(packages, library, character.only = TRUE)
    #   
    #   msg <- paste(msg, "done")
    #   notif(msg)
    # }
    for(ll in 1:length(packages)) {
      library(packages[ll], character.only = TRUE)
    }
    
    if (RUN_BO) {
      dgpsi::init_py(verb = FALSE)
    }
    
    # handlers(global = TRUE)
    # Progress report with progressr
    progress_handlers <- list(
      handler_progress(
        format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
      )
    ) 
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      progress_handlers <- c(progress_handlers,
                             handler_rstudio())
    } else {
      progress_handlers <- c(progress_handlers, handler_txtprogressbar())
    }
    if (os == "windows") {
      progress_handlers <- c(progress_handlers, handler_winprogressbar())
    }
    handlers(
      progress_handlers,
      on_missing = "warning"
    )
    
    NAME_CONVERSION <- get_name_conversion()
    
    # Swap rows 83 and 84
    row83 <- NAME_CONVERSION[83, ]
    row84 <- NAME_CONVERSION[84, ]
    NAME_CONVERSION[83, ] <- row84
    NAME_CONVERSION[84, ] <- row83
    
    
    GreyPolygonWidth <- 1
    UnitPolygonColours <- 1
    # Sys.setenv(PROJ_LIB = "/usr/share/proj")
    # Sys.setenv(GDAL_DATA = "/usr/share/proj/")
    
    USER_PATH <- user_path()
    
    # ElicitorAppFolder <- normalizePath(file.path(USER_PATH, "Downloads"))
    ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
    DataFilesFolder <- normalizePath(file.path(FolderSource, "JulesOP"))
    DownscalingImagesFolder<-normalizePath(file.path(FolderSource, "DownScalingImages"))
    CalculatedFilesFolder<-normalizePath(file.path(FolderSource, "CalculatedFiles"))
    # If the folder does not exist, create it
    if (isFALSE(dir.exists(CalculatedFilesFolder))) {
      dir.create(CalculatedFilesFolder)
    }
    
    # Loading big files takes up a lot of RAM that cannot be emptied.
    # So instead, loading happens in a new R process we can then shutdown
    plan(futureplan, workers = 2)
    
    # Load files if any are missing
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson"))) ||
        !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson"))) ||
        !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))) {
      # 337th month
      # mean1 month corresponds to (maybe) January 2022
      # Jules Files loaded later as they take too much memory
      
      SquaresLoad <- value(future(sf::st_read(normalizePath(file.path(DataFilesFolder, "SEER", "Fishnet_1km_to_SEER_net2km.shp")))))
      Sqconv <- st_transform(SquaresLoad, crs = 4326)
      CorrespondenceJules <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "CorrespondanceSqToJules.csv")))[, -1]))
      seer2km <- value(future(st_read(normalizePath(file.path(DataFilesFolder, "SEER_net2km.shp")))))
      jncc100 <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "beta_JNCC100_interact_quad.csv")))))
      baseline_species_prob_40_unmanaged_conifers <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "baseline_species_prob_40_unmanaged_conifers.csv")), header = FALSE)))
      baseline_species_prob_40_unmanaged_deciduous <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "baseline_species_prob_40_unmanaged_deciduous.csv")), header = FALSE)))
      scenario_species_prob_40_unmanaged_conifers <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "scenario_species_prob_40_unmanaged_conifers.csv")), header = FALSE)))
      scenario_species_prob_40_unmanaged_deciduous <- value(future(read.csv(normalizePath(file.path(DataFilesFolder, "scenario_species_prob_40_unmanaged_deciduous.csv")), header = FALSE)))
      speciesprob_list <- list(list(planting = FALSE, # baseline
                                    tree_specie = "Conifers",
                                    table = baseline_species_prob_40_unmanaged_conifers),
                               
                               list(planting = FALSE, # baseline
                                    tree_specie = "Deciduous",
                                    table = baseline_species_prob_40_unmanaged_deciduous),
                               
                               list(planting = TRUE, # scenario
                                    tree_specie = "Conifers",
                                    table = scenario_species_prob_40_unmanaged_conifers),
                               
                               list(planting = TRUE, # scenario
                                    tree_specie = "Deciduous",
                                    table = scenario_species_prob_40_unmanaged_deciduous))
      # Frees RAM
      rm(baseline_species_prob_40_unmanaged_conifers, baseline_species_prob_40_unmanaged_deciduous,
         scenario_species_prob_40_unmanaged_conifers, scenario_species_prob_40_unmanaged_deciduous)
      
      climatecells <- read.csv(normalizePath(file.path(DataFilesFolder, "climate_cells.csv")))
    }
    
    notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip"))))
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")))) {
      res$status <- 403
      return("Please upload land_parcels.shp.zip")
    }
    notif(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "found. Trying to unzip and load files..."))
    
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))) {
      UnZipDirName <- normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp"))
      dir.create(UnZipDirName)
      tries <- 0
      while (inherits(suppressWarnings(try(unzip(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), exdir = UnZipDirName),
                                           silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read land_parcels.shp.zip")
        }
      }
      
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "unzipped. Loading files..." ))
      shconv <- sf::st_read(normalizePath(file.path(UnZipDirName, "land_parcels.shp")))
      if (is.null(shconv$extent)) {
        shconv$extent <- "NoExtent"
      }
      st_write(shconv, normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))
      shconv <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson"))  )
    } else {
      shconv <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))
    }
    
    notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "decision_units.json"))))
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))) {
      res$status <- 403
      return("Please upload decision_units.json")
    }
    notif(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "found. Trying to load file if FullTableMerged.geojson does not exist..."))
    
    if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))) {
      sf_use_s2(FALSE)
      lsh <- dim(shconv)[1]
      
      tries <- 0
      while (inherits(suppressWarnings(try(AllUnits <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))$decision_unit_ids,
                                           silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read decision_units.json")
        }
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "loaded, processing..." ))
      
      # Turn decision units to individual parcels, by making all valid ones (i.e. that are != -1) get a unique id instead
      indices <- which(AllUnits != -1)
      AllUnits[indices] <- seq_along(indices)
      
      Uni <- unique(AllUnits)
      # units is the list of decision units
      FullTab <- data.frame(extent = "NoExtent", x = rep(0, length(Uni)), y = rep(0, length(Uni)), area = rep(1, length(Uni)),
                            Carbon_Mean_Scenario26_TreeSpecieConifers = rep(15, length(Uni)),
                            Carbon_SD_Scenario26_TreeSpecieConifers = rep(1, length(Uni)), 
                            Carbon_Mean_Scenario26_TreeSpecieDeciduous = rep(15, length(Uni)),
                            Carbon_SD_Scenario26_TreeSpecieDeciduous = rep(1, length(Uni)), 
                            VisitsMean = rep(30, length(Uni)),
                            VisitsSD = rep(2, length(Uni)), BioMean_Sciurus_vulgaris = rep(0.5, length(Uni)),
                            BioSD_Sciurus_vulgaris = rep(0.02, length(Uni)), units = Uni)
      #We are looking at CO2 retained in 2050. Then JulesMeanY0 means that if we plant in year 0,
      #we will obtain JulesMeanYears$mean337, year 1: mean325, year 2: mean313,....
      #Note that JulesMeanY29 is considered "No Planting" so the values of 0 will not be changed
      for(ii in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",(ii-1))]<-rep(0, length(Uni))
      }
      for(ii in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",(ii-1))]<-rep(0, length(Uni))
      }
      
      for(ii in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",(ii-1))]<-rep(0, length(Uni))
      }
      for(ii in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",(ii-1))]<-rep(0, length(Uni))
      }
      
      
      MER <- list()
      for (ii in 1:length(Uni)) {
        SELLL <- shconv$geometry[AllUnits == Uni[ii]]
        MER[[ii]] <- suppressMessages(st_union(st_make_valid(SELLL)))
      }
      
      
      FullTable <- st_sf(FullTab, geometry = do.call(c, MER), crs = 4326)
      #
      keptLines <- sort(which(as.numeric(summary(sf::st_intersects(Sqconv, shconv))[, 1]) != 0))
      
      SELECTEDSquaresconv <- Sqconv$geometry[keptLines]
      LinesJules <- CorrespondenceJules[keptLines]
      # Find lines where Jules is not available, it means there are no trees, so replace by 0
      LinesJulesNoMinus1 <- which(LinesJules == (-1))
      LinesJules[LinesJulesNoMinus1] <- 1
      
      # Jules results for all years from 0 to MAXYEAR
      # Loading feather files takes a lot of RAM permanently. This makes it happen in another process we shut down,
      JulesMeanYears <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-mean-monthly.feather")))[,c("x","y",paste0("mean",seq(1,337,by=12)))]))
      JulesSDYears <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-sd-monthly.feather")))[,c("x","y",paste0("sd",seq(1,337,by=12)))]))
      JulesMean <- JulesMeanYears[, c("x", "y", "mean337")]
      JulesSD <- JulesSDYears[, c("x", "y", "sd337")]
      
      SelectedJulesMeanSq <- JulesMean[LinesJules, ]
      SelectedJulesMeanSq[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDSq <- JulesSD[LinesJules, ]
      SelectedJulesSDSq[LinesJulesNoMinus1, ] <- 0
      rm(JulesMean, JulesSD)
      
      
      
      SelectedJulesMeanYears<-JulesMeanYears[LinesJules,paste0("mean",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesMeanYears[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDYears<-JulesSDYears[LinesJules,paste0("sd",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesSDYears[LinesJulesNoMinus1, ] <- 0
      rm(JulesMeanYears, JulesSDYears)
      
      
      
      
      
      JulesMeanYears85 <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-mean-monthly.feather")))[,c("x","y",paste0("mean",seq(1,337,by=12)))]))
      JulesSDYears85 <- value(future(arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-sd-monthly.feather")))[,c("x","y",paste0("sd",seq(1,337,by=12)))]))
      JulesMean85 <- JulesMeanYears85[, c("x", "y", "mean337")]
      JulesSD85 <- JulesSDYears85[, c("x", "y", "sd337")]
      SelectedJulesMeanSq85 <- JulesMean85[LinesJules, ]
      SelectedJulesMeanSq85[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDSq85 <- JulesSD85[LinesJules, ]
      SelectedJulesSDSq85[LinesJulesNoMinus1, ] <- 0
      rm(JulesMean85, JulesSD85)
      
      
      SelectedJulesMeanYears85<-JulesMeanYears85[LinesJules,paste0("mean",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesMeanYears85[LinesJulesNoMinus1, ] <- 0
      SelectedJulesSDYears85<-JulesSDYears85[LinesJules,paste0("sd",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
      SelectedJulesSDYears85[LinesJulesNoMinus1, ] <- 0
      rm(JulesMeanYears85, JulesSDYears85)
      gc()
      
      
      
      
      
      SELECTEDSquaresconvTab <- data.frame(idSq = seq_along(SELECTEDSquaresconv))
      SELECTEDSquaresconvTab <- st_sf(SELECTEDSquaresconvTab, geometry = SELECTEDSquaresconv, crs = 4326)
      
      
      FullTableCopy <- FullTable
      FullTableCopy$idPoly <- seq_along(FullTableCopy$geometry)
      
      #st_as_sf(data.frame(geometry = SELECTEDSquaresconv))
      #st_as_sf(data.frame(FullTable))
      
      
      INTT <- st_intersection(st_make_valid(SELECTEDSquaresconvTab), st_make_valid(FullTableCopy))
      INTT$area <- st_area(INTT) / 1e6
      
      
      
      # Bootstrap means and standard deviations (to avoid assumptions of independence)
      # As we have sum of Gaussians, we 
      # NBSIMS <- 500
      for (ii in 1:length(FullTableCopy$geometry)) {
        SELLLines <- INTT$idPoly == ii
        SELLSqs <- INTT$idSq[SELLLines]
        SELLWeights <- INTT$area[SELLLines]
        #    SellWeightsArr <- t(matrix(SELLWeights, length(SELLWeights), NBSIMS))
        SellWeightsArr <- (matrix(SELLWeights, length(SELLWeights), (MAXYEAR+1)))
        
        SelJulesMeans <- SelectedJulesMeanSq$mean337[SELLSqs]
        SelJulesSDs <- SelectedJulesSDSq$sd337[SELLSqs]
        
        SelJulesMeansYears<-SelectedJulesMeanYears[SELLSqs,]
        SelJulesSDsYears <- SelectedJulesSDYears[SELLSqs,]
        
        SelJulesMeans85 <- SelectedJulesMeanSq85$mean337[SELLSqs]
        SelJulesSDs85 <- SelectedJulesSDSq85$sd337[SELLSqs]
        
        SelJulesMeansYears85<-SelectedJulesMeanYears85[SELLSqs,]
        SelJulesSDsYears85 <- SelectedJulesSDYears85[SELLSqs,]
        
        
        if (length(SelJulesMeans) >= 1) {
          # SimuArr <- rmvnorm(NBSIMS, mean = SelJulesMeans, sigma = diag(SelJulesSDs^2))
          FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[ii] <-    sum(SelJulesMeans*SELLWeights)#sum(colMeans(SimuArr * SellWeightsArr))
          FullTable$Carbon_SD_Scenario26_TreeSpecieConifers[ii] <- sqrt(sum((SelJulesSDs*SELLWeights)^2))#sd(rowSums(SimuArr * SellWeightsArr))
          
          #JulesMeanY29 is not replaced here as it is used to men that there is no planting.
          
          FullTable[ii,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-colSums(SelJulesMeansYears*SellWeightsArr)
          FullTable[ii,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-sqrt(colSums((SelJulesSDsYears*SellWeightsArr)^2))
          
          FullTable$Carbon_Mean_Scenario26_TreeSpecieDeciduous<-sum(SelJulesMeans85*SELLWeights)
          FullTable$Carbon_SD_Scenario26_TreeSpecieDeciduous<-sqrt(sum((SelJulesSDs85*SELLWeights)^2))
          
          
          FullTable[ii,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-colSums(SelJulesMeansYears85*SellWeightsArr)
          FullTable[ii,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-sqrt(colSums((SelJulesSDsYears85*SellWeightsArr)^2))
          
          
          FullTable$area[ii] <- sum(SELLWeights)
          
          # } else if (length(SelJulesMeans) == 1) {
          #  SimuArr <- rnorm(NBSIMS, mean = SelJulesMeans, sd = SelJulesSDs)
          # FullTable$JulesMean[ii] <- sum(colMeans(SimuArr * SellWeightsArr))
          #  FullTable$JulesSD[ii] <- sd(rowSums(SimuArr * SellWeightsArr))
          #  FullTable$area[ii] <- sum(SELLWeights)
        } else {
          FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[ii] <- 0
          FullTable$Carbon_SD_Scenario26_TreeSpecieConifers[ii] <- 0
          FullTable$Carbon_Mean_Scenario26_TreeSpecieDeciduous[ii]<-0
          FullTable$Carbon_SD_Scenario26_TreeSpecieDeciduous[ii]<-0
          
          
          
          FullTable[ii,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          FullTable[ii,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          
          FullTable[ii,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          FullTable[ii,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          
          
          FullTable$area[ii] <- sum(SELLWeights)
        }
      }
      
      # Replace Biodiversity columns with correct ones
      msg <- "Converting biodiversity from square grid cells to our shapefile polygons ..."
      notif(msg)
      FullTable <- convert_bio_to_polygons_from_elicitor_and_merge_into_FullTable(Elicitor_table = FullTable,
                                                                                  speciesprob_list = speciesprob_list,
                                                                                  seer2km = seer2km,
                                                                                  jncc100 = jncc100,
                                                                                  climatecells = climatecells,
                                                                                  MAXYEAR = MAXYEAR,
                                                                                  limit_log_level = LOG_LEVEL)
      msg <- paste(msg, "done")
      notif(msg)
      # Free a lot of RAM
      rm(speciesprob_list)
      
      # Outcomes
      notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))))
      while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
        res$status <- 403
        return("Please upload outcomes.json")
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file..."))
      
      # Read the outcomes from the Elicitor app
      tries <- 0
      while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))),
                                           silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read outcomes.json")
        }
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing..."))
      
      outsomes_biodiversity_indices <- sapply(outcomes, function (x) x$category == "Biodiversity")
      SPECIES_ENGLISH <- unique(sapply(outcomes[outsomes_biodiversity_indices], function(x) x$`sub-category`))
      
      # SPECIES_ENGLISH <- c("Pollinators", "Reed Bunting", "Lapwing", "Invertebrate - snails")
      # c("Pollinators", "Herptiles")
      # SPECIES_ENGLISH <- unique(NAME_CONVERSION$Group_pretty)[c(2, 7)]
      
      # Default specie and group
      if (length(SPECIES_ENGLISH) == 0) {
        SPECIES_ENGLISH <- "All"
      }
      GROUPS <- c()
      # Separate the groups from SPECIES_ENGLISH, then merge them to SPECIES
      SPECIES <- SPECIES_ENGLISH
      for (i in 1:length(SPECIES_ENGLISH)) {
        ugly_english_specie <- get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
        # If it is a group
        if (ugly_english_specie %in% c(unique(NAME_CONVERSION$Group), unique(NAME_CONVERSION$Group_pretty), "All")) {
          SPECIES[i] <- get_ugly_group(ugly_english_specie, NAME_CONVERSION)
          GROUPS <- c(GROUPS, get_ugly_group(ugly_english_specie, NAME_CONVERSION))
        } else {
          # If it is a specie
          SPECIES[i] <- get_specie_from_english_specie(ugly_english_specie, NAME_CONVERSION)
        }
      }
      
      # Add the richness
      msg <- "Adding richness columns ..."
      notif(msg, log_level = "debug")
      FullTable <- add_richness_columns(FullTable, groups = GROUPS, maxyear = MAXYEAR,
                                        NAME_CONVERSION = NAME_CONVERSION, SCENARIO = SCENARIO)
      msg <- paste(msg, "done")
      notif(msg)
      
      # FOR BACKWARD COMPATIBILITY
      # until the code works with the new biodiversity columns, keep the old ones:
      # BioMean_specie, BioSD_specie
      # BioMean_Birds, BioSD_Birds
      for (specie_or_group in SPECIES) {
        if (specie_or_group %in% NAME_CONVERSION$Specie) {
          
          old_col_mean_values <- FullTable %>%
            sf::st_drop_geometry() %>%
            dplyr::select(starts_with("Bio_Mean") &
                            contains(paste0("BioSpecie", specie_or_group, "_")) &
                            contains("Scenario26") &
                            contains("TreeSpecieConifers") &
                            contains("_Planting")) %>%
            pull()
          old_col_sd_values <- FullTable %>%
            sf::st_drop_geometry() %>%
            dplyr::select(starts_with("Bio_SD") &
                            contains(paste0("BioSpecie", specie_or_group, "_")) &
                            contains("Scenario26") &
                            contains("TreeSpecieConifers") &
                            contains("_Planting")) %>%
            pull()
          
          new_col_mean_name <- paste0("BioMean_", specie_or_group)
          new_col_sd_name <- paste0("BioSD_", specie_or_group)
          FullTable[[new_col_mean_name]] <- old_col_mean_values
          FullTable[[new_col_sd_name]] <- old_col_sd_values
          
        } else if (specie_or_group %in% c(NAME_CONVERSION$Group, "All")) {
          
          old_col_values <- FullTable %>%
            sf::st_drop_geometry() %>%
            dplyr::select(starts_with("Richness") &
                            contains(paste0("Group", specie_or_group, "_")) &
                            contains("TreeSpecieConifers") &
                            contains("Scenario26") &
                            contains("PlantingYear0")) %>%
            pull()
          
          new_col_name <- paste0("BioMean_", specie_or_group)
          FullTable[[new_col_name]] <- old_col_values
          
          old_col_values <- rep(0, length(old_col_values))
          new_col_name <- paste0("BioSD_", specie_or_group)
          FullTable[[new_col_name]] <- old_col_values
          
        }
      }
      
      # Only keep all possible biodiversity species (unions of species selected)
      species_to_keep <- intersect(SPECIES, NAME_CONVERSION$Specie)
      species_to_remove <- setdiff(NAME_CONVERSION$Specie, species_to_keep)
      species_to_remove_pattern <- paste0("Bio.*?(", paste0(species_to_remove, collapse = "|"), ").*")
      
      # Remove biodiversity columns with the species we don't need
      FullTable <- FullTable %>%
        dplyr::select(!matches(species_to_remove_pattern))
      
      # Move decision units with id -1 (Maintain current land use) from FullTable to FullTableNotAvail if we want to handle them in a special way
      # OR
      # Only delete lines with "units"=-1 from FullTable
      FullTableNotAvail <- FullTable %>%
        dplyr::filter(units == -1)
      FullTable <- FullTable %>%
        dplyr::filter(units != -1)
      
      st_write(FullTable, normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
      # FullTableNotAvail <- data.frame(extent = NULL)
      st_write(FullTableNotAvail, normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
    }
    
    notif(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson ..."))))
    FullTable <- value(future(sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))))
    FullTableNotAvail <- value(future(sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))))
    notif(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))
    
    future:::ClusterRegistry("stop")
    
    STDMEAN <- 0.05
    STDSTD <- 0.01
    
    # Random sampling
    NSamp <- 2000
    
    #Now the random sample contains the year of planting/
    msg <- paste0("Sampling ", NSamp, " random strategies ...")
    notif(msg)
    
    Uniqunits <- unique(FullTable$units)
    
    plan(futureplan, workers = min(5, round(future::availableCores() / 2)))
    simul636YearType <- local({
      pb <- progressor(steps = NSamp, message = paste("Sampling", NSamp, "strategies ..."))
      simul636YearType <- foreach(
        aaa = 1:NSamp,
        .combine = combine_foreach_rbind,
        .multicombine = TRUE,
        .inorder = TRUE,
        .options.future = list(
          seed = TRUE
        )
      ) %dofuture% {
        
        # Part 1: simul636
        pp <- runif(1)
        RandSamp <- rmultinom(length(Uniqunits), 1, c(pp, 1 - pp))[1, ]
        
        result <- matrix(0, nrow = 1, ncol = dim(FullTable)[1])
        for (bbb in 1:length(Uniqunits)) {
          result[1, FullTable$units == Uniqunits[bbb]] <- RandSamp[bbb]
        }
        result_simul636 <- result
        
        
        
        # Part 2: simul636Year
        
        # Simul636Year is populated with the year of planting
        # once simul636Year works it will replace simul636
        # (MAXYEAR+1) is the code for no planting
        # Otherwise 0 to MAXYEAR is the year of planting if planted.
        result <- result_simul636
        
        result[1, result_simul636[1,]==0]<-(MAXYEAR+1)
        probb<-runif(1,0.2,0.6)
        size<-15*runif(1)
        result[1, result_simul636[1,]!=0]<-pmin(rnbinom(sum(result_simul636[1,]),size=size,prob=probb),MAXYEAR)
        
        result_simul636Year <- result
        
        
        
        # Part 3: simul636YearType
        
        #### Simul636YearType is a similar table but it stops at year MAXYEAR and
        #### instead, we pick a tree type (or no planting)
        result <- list(YEAR=result_simul636-1,TYPE=t(apply(result_simul636,2,as.character)))
        result[["TYPE"]][1, result_simul636[1,]==0]<-"NoPlanting"
        probbType<-runif(1,0.5)
        Planted<-(result_simul636[1,]==1)
        if(sum(Planted)>0){
          result[["TYPE"]][1, result_simul636[1,]==1]<-sample(c("Conifers","Deciduous"),sum(Planted),replace=TRUE,prob=c(probbType,1-probbType))}
        
        probb<-runif(1,0.2,0.6)
        size<-15*runif(1)
        DRAW<-pmin(rnbinom(sum(result_simul636[1,]),size=size,prob=probb),MAXYEAR)
        result$YEAR[1, result$TYPE[1,]!="NoPlanting"]<-DRAW
        
        result_simul636YearType <- result
        
        # End
        if (aaa %% (ceiling(NSamp / 100)) == 0) {pb(amount = ceiling(NSamp / 100))}
        return(result_simul636YearType)
      }
      # Avoid warning message from progressor function
      pb(amount = 0)
      return(simul636YearType)
    })
    if (isFALSE(RUN_BO)) {
      plan(sequential)
      future:::ClusterRegistry("stop")
    }
    handlers(
      c(handler_shiny(),
        progress_handlers),
      on_missing = "ignore"
    )
    
    RREMBO_CONTROL <- list(
      # method to generate low dimensional data in RRembo::designZ ("LHS", "maximin", "unif"). default unif
      designtype = "LHS",
      # if TRUE, use the new mapping from the zonotope, otherwise the original mapping with convex projection. default TRUE
      reverse = FALSE)
    RREMBO_HYPER_PARAMETERS <- RRembo_defaults(d = 6,
                                               D = 3 * nrow(FullTable), # area + planting_year + tree_specie per parcel
                                               init = list(n = 100), budget = 100,
                                               control = RREMBO_CONTROL,
                                               limit_log_level = LOG_LEVEL)
    
    msg <- paste(msg, "done")
    notif(msg)
    
    
    Simul636YearOverrideReactive<-reactiveVal(vector("list",dim(simul636YearType$YEAR)[2]))
    Simul636YearTypeOverrideReactive<-reactiveVal(vector("list",dim(simul636YearType$YEAR)[2]))
    
    
    
    alphaLVL <- 0.9
    ILevel<- -(sqrt(alphaLVL/(1-alphaLVL)))
    
    MaxRounds <- 5
    ConvertSample <- sample(1:NSamp, 200)
    
    # Outcomes
    if (isFALSE(exists("outcomes"))) {
      notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))))
      while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
        Sys.sleep(5)
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file..."))
      
      tries <- 0
      # Read the outcomes from the Elicitor app
      while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
                                           , silent = TRUE)),
                      "try-error")) {
        Sys.sleep(1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read outcomes.json")
        }
      }
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing..."))
      
      outsomes_biodiversity_indices <- sapply(outcomes, function (x) x$category == "Biodiversity")
      SPECIES_ENGLISH <- unique(sapply(outcomes[outsomes_biodiversity_indices], function(x) x$`sub-category`))
      
      # SPECIES_ENGLISH <- c("Pollinators", "Reed Bunting", "Lapwing", "Invertebrate - snails")
      # c("Pollinators", "Herptiles")
      # SPECIES_ENGLISH <- unique(NAME_CONVERSION$Group_pretty)[c(2, 7)]
      
      # Default specie and group
      if (length(SPECIES_ENGLISH) == 0) {
        SPECIES_ENGLISH <- "All"
      }
      # Separate the groups from SPECIES_ENGLISH, then merge them to SPECIES
      SPECIES <- SPECIES_ENGLISH
      for (i in 1:length(SPECIES_ENGLISH)) {
        ugly_english_specie <- get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
        # If it is a group
        if (ugly_english_specie %in% c(unique(NAME_CONVERSION$Group), unique(NAME_CONVERSION$Group_pretty), "All")) {
          SPECIES[i] <- get_ugly_group(ugly_english_specie, NAME_CONVERSION)
        }
      }
      for (i in 1:length(SPECIES_ENGLISH)) {
        ugly_english_specie <- get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
        # If it is a specie
        if (isFALSE(ugly_english_specie %in% c(unique(NAME_CONVERSION$Group), unique(NAME_CONVERSION$Group_pretty), "All"))) {
          SPECIES[i] <- get_specie_from_english_specie(ugly_english_specie, NAME_CONVERSION)
        }
      }
    }
    
    # SPECIES <- c(NAME_CONVERSION[1:2, "Specie"], "Pollinators", "All")
    # SPECIES_ENGLISH <- c(NAME_CONVERSION[1:2, "English_specie"], "Pollinators", "All")
    N_SPECIES <- length(SPECIES)
    TARGETS <- c("Carbon", SPECIES, "Area", "Visits")
    N_TARGETS <- length(TARGETS)
    
    #Indicates if the quantity must be above (TRUE) or below the target (FALSE)
    AboveTargets<-rep(TRUE,N_TARGETS)
    AboveTargets[N_TARGETS-1]<-FALSE
    
    # slider_list <- list(
    #   sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25)
    # )
    # Add sliderInput("BioSliderSPECIE", "Average SPECIE % increase:", min = 0, max = 36, value = 25) for each specie
    
    verticalLayout_params <- c(list(sliderInput("SliderMain", "Tree carbon stored (tonnes of CO2):", min = -1, max = 870, value = -1)),
                               lapply(SPECIES, function(x, fulltable, NAME_CONVERSION_ARG) {
                                 NAME_CONVERSION <- NAME_CONVERSION_ARG
                                 # max_specie <- round(max(fulltable[, paste0("BioMean_", x)]))
                                 # value <- round(max_specie / 2)
                                 max_specie <- 36
                                 value <- 1
                                 
                                 # If it is a group
                                 if (x %in% c(NAME_CONVERSION$Group, NAME_CONVERSION$Group_pretty, "All")) {
                                   text <- paste0("Species richness (", get_pretty_group(x, NAME_CONVERSION), ")")
                                 } else {
                                   # If it is a specie
                                   text <- get_english_specie_from_specie(x, NAME_CONVERSION)
                                   text <- get_pretty_english_specie(text, NAME_CONVERSION)
                                   text <- paste(text, " (Presence, %):")
                                 }
                                 
                                 return(bquote(sliderInput(paste0("BioSlider", .(x)),
                                                           .(text),
                                                           min = 0,
                                                           max = .(max_specie),
                                                           value = .(value),
                                                           step = 0.5)))
                               }, fulltable = FullTable, NAME_CONVERSION_ARG = NAME_CONVERSION),
                               list(sliderInput("AreaSlider", HTML("Area planted (km<sup>2</sup>)"), min = 0, max = 25, value = 15,step=1)),
                               list(sliderInput("VisitsSlider", "Recreation (visits per month):", min = 0, max = 750, value = 400)))
    #SPECIES<-c("All","Acanthis_cabaret","Birds","Alauda_arvensis")
    SliderNames<- c("SliderMain",
                    paste0("BioSlider", SPECIES),
                    "AreaSlider","VisitsSlider")
    
    
    #### Precalculate simul636 table with years
    AllExtents<-sort(unique(FullTable$extent))
    
    
    #PrecalcCarbonAllExtents<-list()
    #PrecalcCarbonAllExtentsSD<-list()
    
    PrecalcCarbonAllExtentsType<-list()
    PrecalcCarbonAllExtentsSDType<-list()
    
    PrecalcCarbonAllExtentsType2Lines<-list()
    PrecalcCarbonAllExtentsSDType2Lines<-list()
    
    
    if(#file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))&
      #file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))&
      file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))
    ){
      #load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
      #load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))
      load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))
    }else{
      #if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))){
      #  file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
      #}
      #if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))){
      #  file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
      #}
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
      }
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
      }
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))
      }
      if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))){
        file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))
      }
      
      
      
      for (ext in AllExtents)
      {
        CarbonSelectedYear<-FullTable[FullTable$extent == ext,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedYear$geometry<-NULL
        CarbonSelectedSDYear<-FullTable[FullTable$extent == ext,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedSDYear$geometry<-NULL
        
        CarbonSelectedYear85<-FullTable[FullTable$extent == ext,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedYear85$geometry<-NULL
        CarbonSelectedSDYear85<-FullTable[FullTable$extent == ext,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))]
        CarbonSelectedSDYear85$geometry<-NULL
        
        
        # PrecalcCarbonAllExtents[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        # PrecalcCarbonAllExtentsSD[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        
        PrecalcCarbonAllExtentsType[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        PrecalcCarbonAllExtentsSDType[[ext]]<-matrix(0,dim(simul636YearType$YEAR)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
        
        PrecalcCarbonAllExtentsType2Lines[[ext]]<-matrix(0,2,dim(FullTable[FullTable$extent=="NoExtent",])[1])
        PrecalcCarbonAllExtentsSDType2Lines[[ext]]<-matrix(0,2,dim(FullTable[FullTable$extent=="NoExtent",])[1])
        
        # This precalculated table is used once when the map if first displayed. We use 2 identical lines to avoid issues with the
        # function call that expects a matrix in input.
        
        PrecalcCarbonAllExtentsType2Lines[[ext]][1,]<-CarbonSelectedYear85[,"Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear0"]
        PrecalcCarbonAllExtentsSDType2Lines[[ext]][1,]<-CarbonSelectedSDYear85[,"Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear0"]
        PrecalcCarbonAllExtentsType2Lines[[ext]][2,]<-PrecalcCarbonAllExtentsType2Lines[[ext]][1,]
        PrecalcCarbonAllExtentsSDType2Lines[[ext]][2,]<- PrecalcCarbonAllExtentsSDType2Lines[[ext]][1,]
        
        for(abb in 1:dim(PrecalcCarbonAllExtentsType[[ext]])[1])
        {
          for(bcc in 1:dim(PrecalcCarbonAllExtentsType[[ext]])[2])
          {
            
            # PrecalcCarbonAllExtents[[ext]][abb,bcc]<-CarbonSelectedYear[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",simul636YearType$YEAR[abb,bcc])]
            #  PrecalcCarbonAllExtentsSD[[ext]][abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",simul636YearType$YEAR[abb,bcc])]
            
            
            if(simul636YearType[["TYPE"]][abb,bcc]=="NoPlanting"){
              PrecalcCarbonAllExtentsType[[ext]][abb,bcc]<-0
              PrecalcCarbonAllExtentsSDType[[ext]][abb,bcc]<-0
            }else{
              
              if(simul636YearType[["TYPE"]][abb,bcc]=="Conifers"){
                PrecalcCarbonAllExtentsType[[ext]][abb,bcc]<-CarbonSelectedYear[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",
                                                                                           simul636YearType$YEAR[abb,bcc])]
                PrecalcCarbonAllExtentsSDType[[ext]][abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",
                                                                                               simul636YearType$YEAR[abb,bcc])]
              }else{
                PrecalcCarbonAllExtentsType[[ext]][abb,bcc]<-CarbonSelectedYear85[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",simul636YearType$YEAR[abb,bcc])]
                PrecalcCarbonAllExtentsSDType[[ext]][abb,bcc]<-CarbonSelectedSDYear85[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",simul636YearType$YEAR[abb,bcc])]
                
              }
              
            }
            
            #cat(paste0(abb,"  ",bcc,"\n"))
          }
          
        }
        
        
      }
      
      #save(PrecalcCarbonAllExtents,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
      #save(PrecalcCarbonAllExtentsSD,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
      save(PrecalcCarbonAllExtentsType,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
      save(PrecalcCarbonAllExtentsSDType,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
      save(PrecalcCarbonAllExtentsType2Lines,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType2Lines.RData")))
      save(PrecalcCarbonAllExtentsSDType2Lines,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType2Lines.RData")))
      
    }
    
    
    
    JulesMean <- 0;JulesSD <- 0;SquaresLoad <- 0;Sqconv <- 0;CorrespondenceJules <- 0;seer2km <- 0;jncc100 <- 0;speciesprob40 <- 0;climatecells <- 0;
    
    ui <- fluidPage(useShinyjs(), chooseSliderSkin("Flat",color =rgb(0.25, 0.6, 1.0)),
                    tags$style(".running .status-icon { animation: spin 1s linear infinite; }
               .running { background-color: blue; display: inline-block; padding: 10px; border-radius: 50%; }
               .finished { background-color: green; display: inline-block; padding: 10px; border-radius: 50%; }
               .status-icon { display: inline-block; }
               @keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }"),
                    div(id = "task_status", class = "finished", "🔄"), # Status indicator for the task
                    tabsetPanel(id = "tabs",
                                tabPanel("Maps", fluidPage(
                                  tags$head(
                                    tags$style(HTML("#PrefText {background-color: white;padding: 0px;border: 2px solid white;font-size: 1em; font-weight: bold; margin-bottom: 0;}"))
                                  ),
                                  fluidRow(
                                    column(9,
                                           selectInput("inSelect", "area", sort(unique(c(FullTable$extent, FullTableNotAvail$extent))), 
                                                       FullTable$extent[1]),
                                           jqui_resizable(div(
                                             style = "width: 80%; height: 400px;",
                                             leafletOutput("map", width = "100%", height = "100%"),
                                             sliderInput("YearSelect","Planting year",0+STARTYEAR,MAXYEAR+STARTYEAR,
                                                         0+STARTYEAR,step=1,width = "100%",sep = "")
                                             
                                           )
                                           )
                                    ),
                                    column(3,
                                           # verticalLayout(sliderInput("SliderMain", "Tree Carbon Stored (tonnes of CO2):", min = 0, max = 870, value = 800),
                                           #                sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25, step = 0.01),
                                           #                sliderInput("AreaSlider", "Total Area Planted (km^2):", min = 0, max = 25, value = 15),
                                           #                sliderInput("VisitsSlider", "Average Number of Visitors per cell:", min = 0, max = 750, value = 400))
                                           do.call("verticalLayout",
                                                   verticalLayout_params)
                                    ))
                                )
                                ),
                                tabPanel("Preferences", id = "Preferences",
                                         fluidPage(
                                           shinyjs::hidden(
                                             fluidRow(12, checkboxInput("Trigger", "", value = FALSE, width = NULL))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Trigger == true",
                                             verticalLayout(
                                               verbatimTextOutput("PrefText"),
                                               sliderInput("YearPref","Planting year",0+STARTYEAR,MAXYEAR+STARTYEAR,0+STARTYEAR,step=1,width = "100%",sep = ""),
                                               fluidRow(
                                                 column(6, verticalLayout(jqui_resizable(leafletOutput("ClusterPage")), 
                                                                          verbatimTextOutput("PrefTextChoiceA"),
                                                                          actionButton("choose1", "Choose"))
                                                 ),
                                                 column(6, verticalLayout(jqui_resizable(leafletOutput("ClusterPage2")), 
                                                                          verbatimTextOutput("PrefTextChoiceB"),
                                                                          actionButton("choose2", "Choose"))
                                                 )
                                               ))),
                                           conditionalPanel(
                                             condition = "input.Trigger == false", fluidRow(column(12, jqui_resizable(plotOutput("plotOP1"))))
                                           )
                                         ))
                                ,
                                tabPanel("Alternative approaches", id = "Alt",
                                         
                                         
                                         
                                         jqui_resizable(
                                           div(
                                             id = "AltContainer",
                                             style = "display: grid; grid-template-columns: 3fr 1fr; grid-template-rows: auto auto 1fr; height: 100%; 
                                                width: 100%; overflow: hidden;",
                                             div(
                                               id = "SliderYearAlt",
                                               style = "width: 100%; padding: 10px 10px; grid-column: 1 / span 2;",  # Make the slider span both columns
                                               sliderInput("YearAlt","Planting year",0+STARTYEAR,MAXYEAR+STARTYEAR,0+STARTYEAR,step=1,width = "100%",sep = "")
                                             ),
                                             div(
                                               id = "SliderTextConditional",
                                               style = "width: 100%; padding: 0px; grid-column: 1 / span 2;",  # Make the text output span both columns
                                               if (SHOW_TITLES_ON_CLUSTERING_PAGE) {column(10,verbatimTextOutput("ZeroText"),column(2,))}
                                             ),
                                             div(
                                               style = "display: grid; grid-template-columns: 1fr 1fr; grid-template-rows: auto 1fr  auto 1fr; gap: 5px; 
                                                  grid-column: 1; grid-row: 3; height: 100%;",
                                               div(
                                                 style = "grid-row: 1; grid-column: 1; width: 100%;",
                                                 if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("FirstMapTxt")}
                                               ),
                                               div(
                                                 style = "grid-row: 2; grid-column: 1; width: 100%; height: 100%;",
                                                 leafletOutput("map2", height = 250, width = "100%")
                                               ),
                                               div(
                                                 style = "grid-row: 1; grid-column: 2; width: 100%;",
                                                 if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("SecondMapTxt")}
                                               ),
                                               div(
                                                 style = "grid-row: 2; grid-column: 2; width: 100%; height: 100%;",
                                                 leafletOutput("map3", height = 250, width = "100%")
                                               ),
                                               div(
                                                 style = "grid-row: 3; grid-column: 1; width: 100%;",
                                                 if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("ThirdMapTxt")}
                                               ),
                                               div(
                                                 style = "grid-row: 4; grid-column: 1; width: 100%; height: 100%;",
                                                 leafletOutput("map4", height = 250, width = "100%")
                                               ),
                                               div(
                                                 style = "grid-row: 3; grid-column: 2; width: 100%;",
                                                 if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("FourthMapTxt")}
                                               ),
                                               div(
                                                 style = "grid-row: 4; grid-column: 2; width: 100%; height: 100%;",
                                                 leafletOutput("map5", height = 250, width = "100%")
                                               )
                                             ),
                                             div(
                                               id = "RightCol",
                                               style = "display: flex; flex-direction: column; padding: 0px 10px 0px 10px; background: white; 
                                                  grid-column: 2; grid-row: 3; height: 100%;",
                                               div(
                                                 style = "margin-bottom: 10px; width: 100%;margin-top: 2px",
                                                 verbatimTextOutput("TargetText")
                                               ),
                                               div(
                                                 style = "margin-bottom: 10px; text-align: center; width: 100%;",
                                                 actionButton("random", "Randomize!")
                                               ),
                                               div(
                                                 id = "UniqueLegend",
                                                 style = "padding: 20px; border: 1px solid grey; border-radius: 2px; margin-top: 0; width: 100%;background-color: rgba(210,210,210,0.2);",
                                                 tags$div(style = "display: flex; flex-direction: column; gap: 0px;",
                                                          tags$div(
                                                            style = "font-weight: bold; margin-bottom: 0px;",
                                                            "Planting type"
                                                          ),
                                                          tags$div(style = "display: flex; align-items: center; gap: 10px;",
                                                                   tags$div(style = paste0("width: 20px; height: 20px; background-color: 
                                                                               rgba(17, 119, 51," ,trunc(255*POLYGON_OPACITY),")
                                                                               ;")),
                                                                   "Conifer"
                                                          ),
                                                          tags$div(style = "display: flex; align-items: center; gap: 10px;",
                                                                   tags$div(style = paste0("width: 20px; height: 20px; background-color: 
                                                                               rgba(68,170,152," ,trunc(255*POLYGON_OPACITY),")
                                                                               ;")),
                                                                   "Deciduous"
                                                          ),
                                                          tags$div(style = "display: flex; align-items: center; gap: 10px;",
                                                                   tags$div(style = paste0("width: 20px; height: 20px; background-color: 
                                                                               rgba(128,128,128," ,min(trunc(1.5*255*NOTAVAIL_OPACITY),255),")
                                                                               ;")),
                                                                   "Not available"
                                                          ),
                                                          tags$div(style = "display: flex; align-items: center; gap: 10px;",
                                                                   tags$div(style = paste0("width: 20px; height: 20px; background-color: 
                                                                               rgba(255,0,0," ,trunc(255*POLYGON_OPACITY),")
                                                                               ;")),
                                                                   "Blocked"
                                                          )
                                                          
                                                 )
                                               )
                                             )
                                           )
                                         )
                                         
                                ),
                                
                                if (ANALYSISMODE){tabPanel("Clustering analysis", jqui_resizable(plotOutput("Analysis")),jqui_resizable(plotOutput("Analysis2")))},
                                tabPanel("Exploration",
                                         fluidPage(
                                           
                                           jqui_resizable(
                                             div(id = "Full-elements-container",
                                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%; 
                                                    overflow: hidden;",
                                                 div(
                                                   id = "sliderYearExplorationClusterTop",
                                                   style = "flex: 1; display: flex;align-items: center; 
                                                                justify-content: center;",
                                                   sliderInput("YearSelectClusterExplorationSlider","Planting year",0+STARTYEAR,MAXYEAR+STARTYEAR,
                                                               0+STARTYEAR,step=1,width = "100%",sep = "")
                                                 ),
                                                 div(
                                                   id = "MultipleElements",
                                                   style = "flex: 3;display: flex;flex-direction: row;height: 100%;",
                                                   div(
                                                     id = "Map6_container",
                                                     style = "flex: 3;height: 100%;padding-right: 10px",
                                                     leafletOutput("map6", width = "100%",height = "100%")
                                                   ),
                                                   div(id = "column_container",
                                                       style = "flex: 1;height: 100%;display: flex;flex-direction: column; 
                                                                      justify-content: flex-start;",
                                                       div(
                                                         style = "display: flex; align-items: center; margin-bottom: 20px;
                                                                    width: 100%;",
                                                         actionButton("Carbon_plus", "+", style = "width: 40px;"),
                                                         actionButton("Carbon_minus", "-", style = "width: 40px;"),
                                                         tags$div("Carbon", style = "margin-right: 20px;")
                                                       ),
                                                       tagList(
                                                         lapply(SPECIES, function(nm) {
                                                           div(
                                                             style = "display: flex; align-items: center; margin-bottom: 20px; 
                                                                      width: 100%;",
                                                             actionButton(paste0(nm, "_plus"), "+", style = "width: 40px;"),
                                                             actionButton(paste0(nm, "_minus"), "-", style = "width: 40px;"),
                                                             tags$div(nm, style = "margin-right: 20px;")
                                                           )
                                                         })
                                                       ),
                                                       div(
                                                         style = "display: flex; align-items: center; margin-bottom: 20px;
                                                                  width: 100%;",
                                                         actionButton("Area_plus", "+", style = "width: 40px;"),
                                                         actionButton("Area_minus", "-", style = "width: 40px;"),
                                                         tags$div("Area")
                                                       ),
                                                       
                                                       div(
                                                         style = "display: flex; align-items: center; margin-bottom: 20px;
                                                                  width: 100%;",
                                                         actionButton("Visits_plus", "+", style = "width: 40px;"),
                                                         actionButton("Visits_minus", "-", style = "width: 40px;"),
                                                         tags$div("Visits")
                                                       ),
                                                       div(
                                                         #style = "margin-bottom: 0px;",
                                                         sliderInput("Direction_x",inputId="slider_x",min=0,max=100,step=0.01,value=10,width = "100%")
                                                       ),
                                                       div(
                                                         #style = "margin-bottom: 20px;",
                                                         sliderInput("Direction_y",inputId="slider_y",min=0,max=100,step=0.01,value=10,width = "100%")
                                                       )
                                                   )
                                                 )
                                             ),
                                             options = list(minWidth = 600, minHeight = 400)
                                           )
                                         ),
                                         plotOutput("Chart1"),
                                ),
                                
                                tabPanel("Downscaling",id="DownScale",fluidPage(fluidRow(
                                  column(6,imageOutput("DownScalingImage")),
                                  column(6,imageOutput("DownScalingImage2")),
                                  
                                ))
                                )
                                #plotOutput("DownscalingPlots")),
                                
                    ))
    
    rm(SquaresLoad)
    rm(Sqconv)
    gc()
  })
  
  res$status <- 200
  return(new_environment)
}









