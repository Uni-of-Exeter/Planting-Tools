# Created and maintained by Bertrand Nortier and Timothée Bacri

# options(warn=2, error=recover)
# options(warn=2)
# options(warn=0) # default
options(shiny.error = browser)
options(shiny.reactlog = TRUE)
options(future.globals.maxSize = 3 * 1024^3) # 3 GiB RAM

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
if (os == "windows") {
  futureplan <- future::multisession
  
} else {
  futureplan <- future::multicore
}
future:::ClusterRegistry("stop")


if(Sys.getenv("USERNAME")=="bn267"){
  FolderSource <- "C://Users//bn267//OneDrive - University of Exeter//Documents//GitHub//Planting-Tools//ShinyForestry//"
}


# more --> less: debug / info / warning / error / none
LOG_LEVEL <- "debug"

# FolderSource <- "ShinyForestry/"
FolderSource <- normalizePath(getwd())
if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
  FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
}

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


packages <- c(
  # https://github.com/tidyverse/vroom/issues/538
  "progress", "shinyjs", "shiny", "shinyjqui", "shiny.fluent", "reactlog", "leaflet", "sf", "ggplot2",
  "geosphere", "feather", "readr", "dplyr", "tidyverse", "gsubfn",
  "ggpubr", "htmltools","comprehenr", "Rtsne", "mclust", "seriation", "jsonlite",
  "viridis", "ggmap", "MASS", "mgcv", "shinyWidgets", "truncnorm",
  "GGally", "purrr", "sp", "colorspace", "rjson", "arrow", "lwgeom","dgpsi", "RRembo",
  "mvtnorm", "magrittr",
  "rstudioapi",
  "lhs", "sensitivity",
  "progressr", "doFuture", "promises",
  # # Active subspace method
  "concordance", "BASS", "zipfR",
  # To plot the best map, and save it to a file
  "mapview", "webshot",
  # File-locking, for multi-process
  "flock",
  "adaptMCMC", "data.table"
)

# Retrieve packages from DESCRIPTION (in plantingtools_folder)
# plantingtools_folder <- normalizePath(file.path(FolderSource, ".."))
# if (file.exists(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))) {
#   packages <- read.dcf(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))[, "Imports"]
# } else {
#   packages <- read.dcf(normalizePath(file.path(FolderSource, "DESCRIPTION")))[, "Imports"]
# }
# packages <- unlist(strsplit(packages, ",\\s*"))  # Split and flatten
# packages <- gsub("\\s*\\(.*\\)", "", packages)  # Remove version constraints
# packages <- na.omit(packages)  # Remove any NAs

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

lapply(packages, library, character.only = TRUE)

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


if(Sys.getenv("USERNAME")=="bn267"){
  DataFilesFolder <- "d:\\JulesOP\\"
  ElicitorAppFolder<-"d:\\ElicitatorOutput\\"
  PROJdir<-system.file("proj/proj.db", package = "sf")
  PROJdir<-substring(PROJdir,1,nchar(PROJdir)-8)
  sf_proj_search_paths(PROJdir)
}



# Load files if any are missing
if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson"))) ||
    !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson"))) ||
    !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))) {
  # 337th month
  # mean1 month corresponds to (maybe) January 2022
  # Jules Files loaded later as they take too much memory
  
  
  
  SquaresLoad <- sf::st_read(normalizePath(file.path(DataFilesFolder, "SEER", "Fishnet_1km_to_SEER_net2km.shp")))
  Sqconv <- st_transform(SquaresLoad, crs = 4326)
  CorrespondenceJules <- read.csv(normalizePath(file.path(DataFilesFolder, "CorrespondanceSqToJules.csv")))[, -1]
  seer2km <- st_read(normalizePath(file.path(DataFilesFolder, "SEER_net2km.shp")))
  jncc100 <- read.csv(normalizePath(file.path(DataFilesFolder, "beta_JNCC100_interact_quad.csv")))
  baseline_species_prob_40_unmanaged_conifers <- read.csv(normalizePath(file.path(DataFilesFolder, "baseline_species_prob_40_unmanaged_conifers.csv")), header = FALSE)
  baseline_species_prob_40_unmanaged_deciduous <- read.csv(normalizePath(file.path(DataFilesFolder, "baseline_species_prob_40_unmanaged_deciduous.csv")), header = FALSE)
  scenario_species_prob_40_unmanaged_conifers <- read.csv(normalizePath(file.path(DataFilesFolder, "scenario_species_prob_40_unmanaged_conifers.csv")), header = FALSE)
  scenario_species_prob_40_unmanaged_deciduous <- read.csv(normalizePath(file.path(DataFilesFolder, "scenario_species_prob_40_unmanaged_deciduous.csv")), header = FALSE)
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
  # Free lots of RAM
  rm(baseline_species_prob_40_unmanaged_conifers, baseline_species_prob_40_unmanaged_deciduous,
     scenario_species_prob_40_unmanaged_conifers, scenario_species_prob_40_unmanaged_deciduous)
  
  climatecells <- read.csv(normalizePath(file.path(DataFilesFolder, "climate_cells.csv")))
}

notif(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip"))))
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")))) {
  Sys.sleep(5)
}
notif(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "found. Trying to unzip and load files..."))

if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))) {
  UnZipDirName <- normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp"))
  dir.create(UnZipDirName)
  while (inherits(suppressWarnings(try(unzip(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), exdir = UnZipDirName),
                                       silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
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
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))) {
  Sys.sleep(5)
}
notif(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "found. Trying to load file if FullTableMerged.geojson does not exist..."))

if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))) {
  sf_use_s2(FALSE)
  lsh <- dim(shconv)[1]
  
  while (inherits(suppressWarnings(try(AllUnits <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))$decision_unit_ids,
                                       silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
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
  
  JulesMean <- arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-mean-monthly.feather")))[, c("x", "y", "mean337")]
  JulesSD <- arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-sd-monthly.feather")))[, c("x", "y", "sd337")]
  
  SelectedJulesMeanSq <- JulesMean[LinesJules, ]
  SelectedJulesMeanSq[LinesJulesNoMinus1, ] <- 0
  SelectedJulesSDSq <- JulesSD[LinesJules, ]
  SelectedJulesSDSq[LinesJulesNoMinus1, ] <- 0
  JulesMean<-NULL;JulesSD<-NULL;
  gc()
  
  # Jules results for all years from 0 to MAXYEAR
  JulesMeanYears<-arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-mean-monthly.feather")))[,c("x","y",paste0("mean",seq(1,337,by=12)))]
  JulesSDYears<-arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-sd-monthly.feather")))[,c("x","y",paste0("sd",seq(1,337,by=12)))]
  
  
  SelectedJulesMeanYears<-JulesMeanYears[LinesJules,paste0("mean",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
  SelectedJulesMeanYears[LinesJulesNoMinus1, ] <- 0
  SelectedJulesSDYears<-JulesSDYears[LinesJules,paste0("sd",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
  SelectedJulesSDYears[LinesJulesNoMinus1, ] <- 0
  JulesMeanYears<-0;JulesSDYears<-0;
  
  
  
  
  
  JulesMean85 <- arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-mean-monthly.feather")))[, c("x", "y", "mean337")]
  JulesSD85 <- arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-sd-monthly.feather")))[, c("x", "y", "sd337")]
  SelectedJulesMeanSq85 <- JulesMean85[LinesJules, ]
  SelectedJulesMeanSq85[LinesJulesNoMinus1, ] <- 0
  SelectedJulesSDSq85 <- JulesSD85[LinesJules, ]
  SelectedJulesSDSq85[LinesJulesNoMinus1, ] <- 0
  JulesMean85<-NULL;JulesSD85<-NULL;
  gc()
  
  
  JulesMeanYears85<-arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-mean-monthly.feather")))[,c("x","y",paste0("mean",seq(1,337,by=12)))]
  JulesSDYears85<-arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp85-04-sd-monthly.feather")))[,c("x","y",paste0("sd",seq(1,337,by=12)))]
  SelectedJulesMeanYears85<-JulesMeanYears85[LinesJules,paste0("mean",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
  SelectedJulesMeanYears85[LinesJulesNoMinus1, ] <- 0
  SelectedJulesSDYears85<-JulesSDYears85[LinesJules,paste0("sd",12*((MAXYEAR+1)-seq(1,(MAXYEAR+1),1))+1)]
  SelectedJulesSDYears85[LinesJulesNoMinus1, ] <- 0
  JulesMeanYears85<-NULL;JulesSDYears85<-NULL;
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
    Sys.sleep(5)
  }
  notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file..."))
  
  # Read the outcomes from the Elicitor app
  while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))),
                                       silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
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
FullTable <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
notif(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))

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
  
  # Read the outcomes from the Elicitor app
  while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
                                       , silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
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
                            )
                ))

rm(SquaresLoad)
rm(Sqconv)
gc()

server <- function(input, output, session,
                   FullTable_arg = FullTable,
                   SPECIES_ARG1 = SPECIES,
                   SPECIES_ENGLISH_ARG1 = SPECIES_ENGLISH,
                   N_TARGETS_ARG1 = N_TARGETS,
                   NAME_CONVERSION_ARG1 = NAME_CONVERSION,
                   TARGETS_ARG1 = TARGETS,
                   LOG_LEVEL_ARG = LOG_LEVEL,
                   SCENARIO_ARG = SCENARIO) {
  set.seed(1)
  
  SPECIES <- SPECIES_ARG1
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG1
  N_SPECIES <- length(SPECIES)
  N_TARGETS <- N_TARGETS_ARG1
  TARGETS <- TARGETS_ARG1
  NAME_CONVERSION <- NAME_CONVERSION_ARG1
  LOG_LEVEL <- LOG_LEVEL_ARG
  SCENARIO <- SCENARIO_ARG
  FullTable <- FullTable_arg
  
  SESSION_FILE_SUFFIX <- paste0("_", session$token)
  # Use the local (session) variables instead of global ones
  source(normalizePath(file.path(FolderSource, "functions.R")), local = TRUE)
  source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
  source(normalizePath(file.path(FolderSource, "preferTrees.R")), local = TRUE)
  
  # Value to control the long-running task (Bayesian optimization in Tab 1)
  # We track the task ID. If it changes, the previous long-running task gets cancelled.
  set_latest_task_id(0)
  
  # Delete log file
  log_filename <- base::normalizePath(file.path(FolderSource, paste0("log", SESSION_FILE_SUFFIX, ".txt")), mustWork = FALSE)
  if (file.exists(log_filename)) {
    file.remove(log_filename)
  }
  
  bayesian_optimization_finished <- reactiveVal(TRUE)
  ClusteringDone<-reactiveVal({FALSE})
  Clustering_Category_VectorReactive<-reactiveVal(NULL)
  Clustering_Results_Object_Reactive<-reactiveVal(NULL)
  SetToClusterReactive<-reactiveVal(NULL)
  Selected_Cluster_To_Display_Reactive<-reactiveVal(NULL)
  Projected_TSNE_Data_Clusters_Reactive<-reactiveVal(NULL)
  Basis_Clustering_Reactive<-reactiveVal(NULL)
  Mean_Clusters_Reactive<-reactiveVal(NULL)
  DataCluster_Reactive<-reactiveVal(NULL)
  Limits_Direction_Clusters_Reactive<-reactiveVal(NULL)
  Selected_Point_In_Cluster_To_Display_Reactive<-reactiveVal(1)
  
  
  PrefWeightsAlreadyCalculatedNbRows<-reactiveVal(0)
  FirstTimeClickOnPreferencesReactive<-reactiveVal(TRUE)
  
  infpref_reactive <- reactiveVal()
  pref_reactive <- reactiveVal()
  
  CarbonSliderVal <- reactive({input$SliderMain})
  
  # A for loop over the reactive values causes an issue: only the last reactive value
  # takes effect and therefore overwrites other reactive values, i.e. all bioSliderValSPECIE take
  # the same value. I have to work with a list for it to work.
  reactive_list <- lapply(SPECIES, function(x) {
    var_name <- paste0("BioSliderVal", x)
    value <- reactive({
      input[[paste0("BioSlider", x)]]
    })
    assign(var_name, value, envir = .GlobalEnv)
    return(value)
  })
  AreaSliderVal <- reactive({input$AreaSlider})
  VisitsSliderVal <- reactive({input$VisitsSlider})
  
  YearSelectReactive<-reactiveVal(0)
  YearSelectClusterExplorationReactive<-reactiveVal(0)
  
  
  Text0 <- reactiveVal("")
  Text1 <- reactiveVal("")
  
  output$TargetText <- renderText({
    SPECIES <- SPECIES_ARG1
    SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG1
    N_SPECIES <- length(SPECIES)
    N_TARGETS <- N_TARGETS_ARG1
    NAME_CONVERSION <- NAME_CONVERSION_ARG1
    
    text <- paste0("Targets:\n",
                   "Tree carbon: ", as.numeric(CarbonSliderVal()))
    # A for loop over the reactive values causes an issue: only the last reactive value
    # takes effect and therefore overwrites other reactive values, i.e. all bioSliderValSPECIE take
    # the same value. I have to work with a list for it to work.
    # for (x in SPECIES) {
    #   BioSliderValSpecie <- get(paste0("BioSliderVal", x))
    #   text <- paste0(text, "\n", x, ": ", as.numeric(BioSliderValSpecie()))
    # }
    for (i in 1:length(SPECIES)) {
      specie_english <- if (SPECIES[i] == "All") "All species richness" else SPECIES_ENGLISH[i]
      BioSliderValSpecie <- reactive_list[[i]]
      text <- paste0(text, "\n", get_pretty_english_specie(specie_english, NAME_CONVERSION), ": ", as.numeric(BioSliderValSpecie()))
    }
    
    text <- paste0(text,
                   # "\nRed Squirrel: ", as.numeric(bioSliderVal()),
                   "\nArea planted: ", as.numeric(AreaSliderVal()),
                   "\nVisits: ", as.numeric(VisitsSliderVal()))
  })
  
  ColorLighteningFactor <- reactiveVal(0.5)
  ColorDarkeningFactor <- reactiveVal(0.5)
  
  ColourScheme <- reactiveVal("blue/red")
  
  output$ZeroText <- renderText({Text0()})
  output$FirstMapTxt <- renderText({Text1()})
  
  
  # Clicked Vector indicates the units that have been clicked
  # PreviousClickedVector records the previous version if there has been a change
  # SelectedVector is the vector of cells that are on
  # PreviousSelectedVector are the previous values before any change
  ClickedVector <- reactiveVal(NULL)
  PreviousClickedVector<- reactiveVal(NULL)
  
  SelectedVector<- reactiveVal(list(YEAR=NULL,TYPE=NULL))
  PreviousSelectedVector<-reactiveVal(list(YEAR=NULL,TYPE=NULL))
  
  SelectedFullTableRow<-reactiveVal(NULL)
  
  
  ClickedVectorYear <- reactiveVal(NULL)
  PreviousClickedVectorYear<- reactiveVal(NULL)
  
  ClickedVectorYearType <- reactiveVal(NULL)
  PreviousClickedVectorYearType<- reactiveVal(NULL)
  
  PreviousYearSelectReactive<-reactiveVal(0)
  
  PreviousConsolidatedReactive<-reactiveVal(NULL)
  
  
  #  MaxValsReactive<-reactiveVal(0)
  MaxMinValsReactiveVector<-reactiveVal(0)
  SlidersHaveBeenInitialized<-reactiveVal(rep(0,length(SliderNames)))
  MapReactive<-reactiveVal(NULL)
  MapReactiveNoLegend<-reactiveVal(NULL)
  
  ClickedMatrixTab2Reactive<-reactiveVal(NULL)
  PreviousClickedMatrixTab2Reactive<-reactiveVal(NULL)
  
  tolvecReactive<-reactiveVal(NULL)
  
  SubsetMeetTargetsReactive<-reactiveVal(NULL)
  SubsetMeetTargetsReactiveUnique<-reactiveVal(list(YEAR=NULL,TYPE=NULL,OUTPUTS=NULL))
  PreviousSubsetMeetTargetsReactive<-reactiveVal(NULL)
  PreviousSubsetMeetTargetsReactiveUnique<-reactiveVal(NULL)
  FourUniqueRowsReactive<-reactiveVal(NULL)
  PreviousFourUniqueRowsReactive<-reactiveVal(NULL)
  
  FourUniqueRowsClusteringReactive<-reactiveVal(rep(1,4))
  PreviousFourUniqueRowsClusteringReactive<-reactiveVal(rep(0,4))
  
  
  
  AreaSelected0 <- reactiveVal(NULL)
  CarbonSelected0 <- reactiveVal(NULL)
  CarbonSelectedYear0 <- reactiveVal(NULL)
  CarbonSelectedYear850 <- reactiveVal(NULL)
  
  
  CreatedBaseMap<-reactiveVal(0)
  UpdatedExtent<-reactiveVal(0)
  
  for (x in SPECIES) {
    var_name <- paste0(x, "Selected0")
    assign(var_name, reactiveVal(NULL))
  }
  VisitsSelected0 <- reactiveVal(NULL)
  CarbonSelectedSD0 <- reactiveVal(NULL)
  CarbonSelectedSDYear0 <- reactiveVal(NULL)
  CarbonSelectedSDYear850 <- reactiveVal(NULL)  
  
  for (x in SPECIES) {
    var_name <- paste0(x, "SelectedSD0")
    assign(var_name, reactiveVal(NULL))
  }
  VisitsSelectedSD0 <- reactiveVal(NULL)
  
  DatBinaryCode0 <- reactiveVal(NULL)
  NbRoundsMax <- reactiveVal(0)
  CurrentRound <- reactiveVal(0)
  LinesToCompareReactive <- reactiveVal(0)
  
  VecNbMet0 <- reactiveVal(NULL)
  
  output$Trigger <- reactiveVal(TRUE)
  
  observe({
    Uni <- unique(FullTable$extent)
    if (length(Uni) > 1) {
      shinyjs::show("inSelect")
    } else {
      if (unique(FullTable$extent)[1] == "NoExtent") {
        shinyjs::hide("inSelect")
      } else {
        shinyjs::show("inSelect")
      }
    }
  })
  
  # First we need to change this observeEvent inSelect
  #DONE INITIALIZATION
  observeEvent(input$inSelect, {
    
    UpdatedExtent(0)
    SelectedDropdown <- input$inSelect
    PreviousClickedVector(NULL)
    ClickedVector(NULL)
    PreviousSelectedVector(list(YEAR=NULL,TYPE=NULL))
    SelectedVector(list(YEAR=NULL,TYPE=NULL))
    ClickedMatrixTab2Reactive(NULL)
    PreviousClickedMatrixTab2Reactive(NULL)
    
    PreviousClickedVectorYear(NULL)
    ClickedVectorYear(NULL)
    
    PreviousClickedVectorYearType(NULL)
    ClickedVectorYearType(NULL)
    
    
    SelectedFullTableRow(NULL)
    
    AreaSelected0(NULL)
    CarbonSelected0(NULL)
    CarbonSelectedYear0(NULL)
    CarbonSelectedYear850(NULL)  
    
    # RedSquirrelSelected0(NULL)
    for (x in SPECIES) {
      fun <- get(paste0(x, "Selected0"))
      fun(NULL)
    }
    VisitsSelected0(NULL)
    
    CarbonSelectedSD0(NULL)
    CarbonSelectedSDYear0(NULL)
    CarbonSelectedSDYear850(NULL)
    
    # RedSquirrelSelectedSD0(NULL)
    for (x in SPECIES) {
      fun <- get(paste0(x, "SelectedSD0"))
      fun(NULL)
    }
    VisitsSelectedSD0(NULL)
    
    SelectedSquares <- cbind(extent = FullTable$extent[FullTable$extent == SelectedDropdown])#, FullTable$lgn.1[FullTable$extent == SelectedDropdown],
    #   FullTable$lat.1[FullTable$extent == SelectedDropdown])
    
    if (!(dim(SelectedSquares)[1] == 0)) {
      
      AreaSelected <- FullTable$area[FullTable$extent == SelectedDropdown]
      CarbonSelected <- (FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[FullTable$extent == SelectedDropdown])
      # we take everything up to year (MAXYEAR+1) (no planting)
      CarbonSelectedYear<-FullTable[FullTable$extent == SelectedDropdown,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))]
      CarbonSelectedYear$geometry<-NULL
      
      
      CarbonSelectedYear85<-FullTable[FullTable$extent == SelectedDropdown,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))]
      CarbonSelectedYear85$geometry<-NULL
      
      
      # RedSquirrelSelected <- FullTable$BioMean_Sciurus_vulgaris[FullTable$extent == SelectedDropdown]
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        biomean_var <- paste0("BioMean_", x)
        var_name <- paste0(x, "Selected")
        # value <- FullTable[FullTable$extent == SelectedDropdown, biomean_var]
        value <- FullTable[[biomean_var]][FullTable$extent == SelectedDropdown]
        assign(var_name, value)
        SpeciesListSelected[x] <- list(value)
      }
      VisitsSelected <- FullTable$VisitsMean[FullTable$extent == SelectedDropdown]
      
      CarbonSelectedSD <- (FullTable$Carbon_SD_Scenario26_TreeSpecieConifers[FullTable$extent == SelectedDropdown])
      CarbonSelectedSDYear<-FullTable[FullTable$extent == SelectedDropdown,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))]
      CarbonSelectedSDYear$geometry<-NULL
      
      CarbonSelectedSDYear85<-FullTable[FullTable$extent == SelectedDropdown,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))]
      CarbonSelectedSDYear85$geometry<-NULL
      
      # RedSquirrelSelectedSD <- FullTable$BioSD_Sciurus_vulgaris[FullTable$extent == SelectedDropdown]
      
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        biosd_var <- paste0("BioSD_", x)
        var_name <- paste0(x, "SelectedSD")
        # value <- FullTable[FullTable$extent == SelectedDropdown, biosd_var]
        value <- FullTable[[biosd_var]][FullTable$extent == SelectedDropdown]
        assign(var_name, value)
        SpeciesListSelectedSD[var_name] <- list(value)
      }
      VisitsSelectedSD <- FullTable$VisitsSD[FullTable$extent == SelectedDropdown]
      
      
      PreviousSelectedVector(list(YEAR=rep(-2,dim(SelectedSquares)[1]),TYPE=rep("NoPlanting",dim(SelectedSquares)[1])))
      SelectedVector(list(YEAR=rep(-1,dim(SelectedSquares)[1]),TYPE=rep("NoPlanting",dim(SelectedSquares)[1])))
      
      ClickedVector(rep(0, dim(SelectedSquares)[1]))
      PreviousClickedVector(rep(-1, dim(SelectedSquares)[1]))
      
      
      ClickedVectorYear(rep((MAXYEAR+1), dim(SelectedSquares)[1]))
      PreviousClickedVectorYear(rep(-1, dim(SelectedSquares)[1]))
      
      ClickedVectorYearType(rep(-1, dim(SelectedSquares)[1]))
      PreviousClickedVectorYearType(rep(-2, dim(SelectedSquares)[1]))
      
      
      ClickedMatrixTab2Reactive(matrix(0, 4,dim(SelectedSquares)[1]))
      PreviousClickedMatrixTab2Reactive(matrix(-1, 4,dim(SelectedSquares)[1]))
      
      AreaSelected0(AreaSelected)
      CarbonSelected0(CarbonSelected)
      CarbonSelectedYear0(CarbonSelectedYear)
      CarbonSelectedYear850(CarbonSelectedYear85)
      
      # RedSquirrelSelected0(RedSquirrelSelected)
      for (x in SPECIES) {
        fun <- get(paste0(x, "Selected0"))
        arg <- get(paste0(x, "Selected"))
        fun(arg)
      }
      VisitsSelected0(VisitsSelected)
      
      CarbonSelectedSD0(CarbonSelectedSD)
      CarbonSelectedSDYear0(CarbonSelectedSDYear)
      CarbonSelectedSDYear850(CarbonSelectedSDYear85)
      # RedSquirrelSelectedSD0(RedSquirrelSelectedSD)
      for (x in SPECIES) {
        fun <- get(paste0(x, "SelectedSD0"))
        arg <- get(paste0(x, "SelectedSD"))
        fun(arg)
      }
      VisitsSelectedSD0(VisitsSelectedSD)
      # Here, the max of the slider is based on the Max possible level for each 
      # MaxVals<-InitFindMaxSliderValues(SelectedVector(),
      #                                 AreaSelected,
      #                                CarbonSelected,
      #                               SpeciesListSelected, 
      #                              VisitsSelected,
      #                             CarbonSelectedSD,
      #                            SpeciesListSelectedSD, 
      #                           VisitsSelectedSD,
      #                          input_areaSlider_multiplicative_coefficient = TRUE,
      #                         alpha=alphaLVL)
      
      MaxVals<-InitFindMaxSliderValuesYearType_NegativeVals(SelectedVector(),
                                                            AreaSelected,
                                                            CarbonSelected,
                                                            CarbonSelectedYear,
                                                            CarbonSelectedYear85,
                                                            SpeciesListSelected, 
                                                            VisitsSelected,
                                                            CarbonSelectedSD,
                                                            CarbonSelectedSDYear,
                                                            CarbonSelectedSDYear85,
                                                            SpeciesListSelectedSD, 
                                                            VisitsSelectedSD,
                                                            input_areaSlider_multiplicative_coefficient = TRUE,
                                                            alpha=alphaLVL,
                                                            ClickedVectorYear(),
                                                            MAXYEAR=MAXYEAR,
                                                            PrecalcCarbonAllExtentsType=PrecalcCarbonAllExtentsType,
                                                            PrecalcCarbonAllExtentsSDType=PrecalcCarbonAllExtentsSDType)
      
      #MaxValsReactive(MaxVals)
      MaxMinValsReactiveVector(c(MaxVals$CarbonMax,unlist(MaxVals$bioMaxList),MaxVals$AreaMax,MaxVals$VisistMax))
      tolvecReactive(MaxVals$tolvec)
      # updateSliderInput(session, "SliderMain", max = trunc(sum(CarbonSelected)), value = trunc(sum(CarbonSelected)))
      # updateSliderInput(session, "SliderMain", max = MaxVals$CarbonMax, value = MaxVals$CarbonMax)
      session$sendInputMessage("SliderMain", list(min=0, max = MaxVals$CarbonMax, value = MaxVals$CarbonMax))
      
      # updateSliderInput(session, "BioSlider", max = trunc(100*mean(RedSquirrelSelected))/100, value = trunc(100*mean(RedSquirrelSelected))/100, step = 0.01)
      for (ijj in 1:length(SPECIES)) {
        x<-SPECIES[ijj]
        bioslider <- paste0("BioSlider", x)
        specie_names <- paste0(x, "Selected")
        specie_selected <- get(specie_names)
        # max_bioslider <- trunc(mean(specie_selected))
        max_bioslider <- MaxVals$bioMaxList[[ijj]]
        if (is.nan(max_bioslider) || is.na(max_bioslider)) {
          max_bioslider <- 0
        }
        updateSliderInput(session, bioslider, max = max_bioslider, value = max_bioslider, step = 1)
      }
      # max_areaslider <- trunc(100*sum(AreaSelected))/100
      max_areaslider <- MaxVals$AreaMax
      if (is.nan(max_areaslider) || is.na(max_areaslider)) {
        max_areaslider <- 0
      }
      # max_visitsslider <- trunc(mean(VisitsSelected))
      max_visitsslider <- MaxVals$VisistMax
      if (is.nan(max_visitsslider) || is.na(max_visitsslider)) {
        max_visitsslider <- 0
      }
      updateSliderInput(session, "AreaSlider", min=MaxVals$AreaMin,max = max_areaslider, value = MaxVals$AreaMax, step = 0.5)
      updateSliderInput(session, "VisitsSlider", max = max_visitsslider, value = max_visitsslider)
      # We now need to obtain the list of strategies from simul636 that meet the targets with the right confidence.
      # tmp <- outputmap_calculateMats(input = input,
      #                                 SavedVecLoc = ClickedVector(),
      #                                 simul636Loc = simul636,
      #                                 AreaSelected = AreaSelected,
      #                                 CarbonSelected = CarbonSelected,
      #                                 # RedSquirrelSelected = RedSquirrelSelected,
      #                                 SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
      #                                 VisitsSelected = VisitsSelected,
      #                                 CarbonSelectedSD = CarbonSelectedSD,
      #                                 # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
      #                                 SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
      #                                 VisitsSelectedSD = VisitsSelectedSD,
      #                                 alphaLVL=alphaLVL,
      #                                 ManualTargets=list(MaxVals$CarbonMax,MaxVals$bioMaxList,max_areaslider,max_visitsslider),
      #                                 tolvec=tolvecReactive(),
      #                                 MAXYEAR=MAXYEAR)
      #  
      ########## same function with Year
      #    tmpYear <- outputmap_calculateMatsYear(input = input,
      #                                           SavedVecLoc = ClickedVector(),
      #                                          simul636YearLoc = simul636Year,
      #                                         AreaSelected = AreaSelected,
      #                                         CarbonSelected = CarbonSelected,
      #                                         CarbonSelectedYear =CarbonSelectedYear,
      #                                         SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
      #                                         VisitsSelected = VisitsSelected,
      #                                         CarbonSelectedSD = CarbonSelectedSD,
      #                                         CarbonSelectedSDYear = CarbonSelectedSDYear,
      #                                         SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
      #                                         VisitsSelectedSD = VisitsSelectedSD,
      #                                         alphaLVL=alphaLVL,
      #                                         ManualTargets=list(MaxVals$CarbonMax,MaxVals$bioMaxList,max_areaslider,max_visitsslider),
      #                                         tolvec=tolvecReactive(),
      #                                         PrecalculatedCarbonSelectedTableMean=PrecalcCarbonAllExtents[[SelectedDropdown]],
      #                                         PrecalculatedCarbonSelectedTableSD=PrecalcCarbonAllExtentsSD[[SelectedDropdown]],
      #                                         SavedVecYearLoc=ClickedVectorYear(),
      #                                         PreviousSavedVecYearLoc=PreviousClickedVector(),
      #                                         SAMPLELIST=Simul636YearOverrideReactive(),
      #                                         MAXYEAR=MAXYEAR
      #  )
      
      ########## same function with YearType
      tmpYearType <- outputmap_calculateMatsYearType(input = input,
                                                     SavedVecLoc = ClickedVector(),
                                                     simul636YearTypeLoc = simul636YearType,
                                                     AreaSelected = AreaSelected,
                                                     CarbonSelected = CarbonSelected,
                                                     CarbonSelectedYear =CarbonSelectedYear,
                                                     CarbonSelectedYear85 =CarbonSelectedYear85,
                                                     # RedSquirrelSelected = RedSquirrelSelected,
                                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                                     VisitsSelected = VisitsSelected,
                                                     CarbonSelectedSD = CarbonSelectedSD,
                                                     CarbonSelectedSDYear = CarbonSelectedSDYear,
                                                     CarbonSelectedSDYear85 = CarbonSelectedSDYear85,
                                                     # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                                     SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                                     VisitsSelectedSD = VisitsSelectedSD,
                                                     alphaLVL=alphaLVL,
                                                     ManualTargets=list(MaxVals$CarbonMax,MaxVals$bioMaxList,max_areaslider,max_visitsslider),
                                                     tolvec=tolvecReactive(),
                                                     #YearSelect=input$YearSelect,
                                                     PrecalculatedCarbonSelectedTableTypeMean=PrecalcCarbonAllExtentsType[[SelectedDropdown]],
                                                     PrecalculatedCarbonSelectedTableTypeSD=PrecalcCarbonAllExtentsSDType[[SelectedDropdown]],
                                                     SavedVecYearTypeLoc=ClickedVectorYearType(),
                                                     # PreviousSavedVecYearTypeLoc=PreviousClickedVectorType(),
                                                     SAMPLELIST=Simul636YearTypeOverrideReactive(),
                                                     MAXYEAR=MAXYEAR
      )
      
      ######## With Year
      SelectedSimMat2<-list()
      SelectedSimMat2[["YEAR"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.YEAR.",1:dim(simul636YearType[["YEAR"]])[2])]
      SelectedSimMat2[["TYPE"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.TYPE.",1:dim(simul636YearType[["TYPE"]])[2])]
      SelectedSimMat2[["OUTPUTS"]]<-tmpYearType$SelectedSimMat2[,-(1:(2*(dim(simul636YearType[["YEAR"]])[2])))]
      names(SelectedSimMat2[["OUTPUTS"]])<-names(tmpYearType$SelectedSimMat2[,-(1:(2*(dim(simul636YearType[["YEAR"]])[2])))])
      Icalc <- tmpYearType$Icalc
      LimitsMat <- tmpYearType$LimitsMat
      SelecTargetCarbon <- MaxVals$CarbonMax
      SelecTargetArea <- max_areaslider
      SelecTargetVisits <- max_visitsslider
      PROBAMAT<-CalcProbaMat(Icalc$IVEC,LimitsMat,Above=AboveTargets)
      
      #  condition <- TRUE
      #for (iii in 1:length(SPECIES)) {
      #  x<-SPECIES[iii]
      #  var_name <- paste0("SelecTargetBio", x)
      #  value <- tmpYearType[[var_name]]
      #  assign(var_name, value)
      
      #  condition <- condition & (PROBAMAT[,iii+1] >= alphaLVL)
      #}
      # Carbon
      # CONDITION_SEL<-(PROBAMAT[,1] >= alphaLVL) &
      #  condition &
      #  (PROBAMAT[,dim(PROBAMAT)[2]-1] >= alphaLVL) &
      # (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL)
      
      CONDITION_SEL<-ifelse(apply((Icalc$IVEC*t(matrix(2*((AboveTargets-0.5)),dim(Icalc$IVEC)[2],dim(Icalc$IVEC)[1])))<=ILevel,1,prod),TRUE,FALSE)
      
      
      FullSubsetMeetTargets<-tmpYearType$SelectedSimMat2[CONDITION_SEL,]
      
      
      SubsetMeetTargets <-list()
      SubsetMeetTargets[["YEAR"]]<- SelectedSimMat2$YEAR[CONDITION_SEL,]
      SubsetMeetTargets[["TYPE"]]<- SelectedSimMat2$TYPE[CONDITION_SEL,]
      SubsetMeetTargets[["OUTPUTS"]]<- SelectedSimMat2$OUTPUTS[CONDITION_SEL,]
      
      SubsetMeetTargetsReactive(SubsetMeetTargets)
      
      DF<-data.frame(SubsetMeetTargets$YEAR,SubsetMeetTargets$TYPE)
      uniqueRows<-which(!duplicated(DF))
      
      SubsetMeetTargetsReactiveUnique(list(YEAR=SubsetMeetTargets$YEAR[uniqueRows,],TYPE=SubsetMeetTargets$TYPE[uniqueRows,],OUTPUTS=SubsetMeetTargets$OUTPUTS[uniqueRows,]))
      
      PreviousSub<-SubsetMeetTargets
      PreviousSub$YEAR<-SubsetMeetTargets$YEAR-1
      PreviousSubsetMeetTargetsReactive(PreviousSub)
      
      DFPrev<-data.frame(PreviousSub$YEAR,PreviousSub$TYPE)
      uniqueRowsPrev<-which(!duplicated(DFPrev))
      PreviousSubsetMeetTargetsReactiveUnique(list(YEAR=PreviousSub$YEAR[uniqueRowsPrev,],TYPE=PreviousSub$TYPE[uniqueRowsPrev,],
                                                   OUTPUTS=PreviousSub$OUTPUTS[uniqueRowsPrev,]
      ))
      
      if(dim(unique(SubsetMeetTargets$YEAR))[1]>0){
        LengthVec<-min(4,dim(unique(SubsetMeetTargets$YEAR)[1]))
        FourUniqueRowsReactive(seq(1,LengthVec))
        PreviousFourUniqueRowsReactive(seq(1,LengthVec))
      }else{FourUniqueRowsReactive(NULL)
        PreviousFourUniqueRowsReactive(NULL)}
    }
    
    CreatedBaseMap(0)
    UpdatedExtent(1)
    SlidersHaveBeenInitialized(rep(0,length(SliderNames)))
    
  })
  
  
  # Trigger if anything changes
  observe({
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SlidersHaveBeenInitialized())==1)&(!is.null(SelectedFullTableRow()))) {
      SavedVec<-ClickedVector()
      PreviousSavedVec<-PreviousClickedVector()
      
      SavedVecYear<-ClickedVectorYear()
      PreviousSavedVecYear<-PreviousClickedVectorYear()
      
      
      SavedVecYearType<-ClickedVectorYearType()
      PreviousSavedVecYearType<-PreviousClickedVectorYearType()
      
      SelectedVec<-SelectedVector()
      PreviousSelectedVec<-PreviousSelectedVector()
      
      SelectedRow<-SelectedFullTableRow()
      YearSelect<-input$YearSelect-STARTYEAR
      YearSelectReactive(YearSelect)
      PrevYearSelectedLoc<-PreviousYearSelectReactive()
      
      MeanCarbonVec<- FullTable[,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))]
      MeanCarbonVec$geometry<-NULL
      
      MeanCarbonVec85<- FullTable[,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))]
      MeanCarbonVec85$geometry<-NULL
      
      VARCarbonVec<- (FullTable[,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:(MAXYEAR+1))])
      VARCarbonVec$geometry<-NULL
      VARCarbonVec<-VARCarbonVec^2
      
      VARCarbonVec85<- (FullTable[,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:(MAXYEAR+1))])
      VARCarbonVec85$geometry<-NULL
      VARCarbonVec85<-VARCarbonVec85^2
      
      
      #YearsSelectedRow<-SelectedRow[1,paste0("SelectedSimMat.YEAR.",1:length(SavedVecYear))]
      #YearsSelectedRow[YearsSelectedRow>YearSelect]<-(-1)
      
      SelectedRowType<-as.character(SelectedRow[,paste0("SelectedSimMat.TYPE.",1:length(SavedVecYear))])
      SelectedRowYear<-as.numeric(SelectedRow[,paste0("SelectedSimMat.YEAR.",1:length(SavedVecYear))])
      
      
      PreviousSelectedVec$YEAR[PreviousSelectedVec$YEAR>PrevYearSelectedLoc]<-(-1)
      
      
      
      # Display that we can plant from SavedVecYear 
      #SavedVecYear[SavedVecYear>=YearSelect]<-29
      
      ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units,
                             ColorLighteningFactor(), ColorDarkeningFactor())
      
      CarbonMeanCalc<-rep(0.001,length(SavedVecYear))
      
      CarbonVarCalc<-rep(0.001,length(SavedVecYear))
      for(aa in 1:length(SavedVecYear))
      { 
        if(SelectedRowType[aa]=="Conifers"){
          CarbonMeanCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),MeanCarbonVec[aa,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",SelectedRowYear[aa])],0)
          CarbonVarCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),VARCarbonVec[aa,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",SelectedRowYear[aa])],0)
        }else{
          if(SelectedRowType[aa]=="Deciduous"){
            CarbonMeanCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),MeanCarbonVec85[aa,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",SelectedRowYear[aa])],0)
            CarbonVarCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),VARCarbonVec85[aa,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",SelectedRowYear[aa])],0)}else{
              CarbonMeanCalc[aa]<-0; CarbonVarCalc[aa]<-0}
        }
      }
      
      FullColVec <- ColObtained$FullColVec
      ClickedCols <- ColObtained$ClickedCols
      
      # Code: 1: RED: no planting, 2: Tree Type A 3: Tree Type B
      
      TypeA<-(SelectedRowType=="Conifers")&(SelectedRowYear<=YearSelect)&(SavedVecYearType<YearSelect)
      TypeB<-(SelectedRowType=="Deciduous")&(SelectedRowYear<=YearSelect)&(SavedVecYearType<YearSelect)
      BlockedCells<-(SavedVecYearType>=YearSelect)
      
      # Consolidated<-2*((SavedVecYear>=YearSelect)&(SavedVecYear!=29))+1*((YearsSelectedRow<29)&((SavedVecYear==29)|(SavedVecYear<YearSelect)))
      Consolidated<-1*TypeA+2*TypeB+3*BlockedCells
      #cat(as.numeric(Consolidated))
      # PreviousConsolidated<-2*((PreviousSavedVecYear>=PrevYearSelectedLoc)&(PreviousSavedVecYear!=29))+1*((PreviousSelectedVec<29)&((PreviousSavedVecYear==29)|(PreviousSavedVecYear<PrevYearSelectedLoc)))
      if(is.null(PreviousConsolidatedReactive())){PreviousConsolidated<-Consolidated+1}else{PreviousConsolidated<-PreviousConsolidatedReactive()}
      if((CreatedBaseMap()==1)&(length(SavedVecYear)>0)){
        
        # mapp<-leafletProxy("map")
        #  for(ijj in 1:length(SelectedVec)){
        if(prod(PreviousConsolidated==Consolidated)==0)
        {
          mapp<-leafletProxy("map")
          removeShape(mapp,layerId=paste0("Square",1:length(SelectedVec$YEAR)))
          removeShape(mapp,layerId=paste0("Square",1:length(Consolidated)))
          COLOURS<-rep("transparent",length(Consolidated))
          #COLOURS[Consolidated==1]<-FullColVec[Consolidated==1]
          #COLOURS[Consolidated==2]<-ClickedCols[Consolidated==2]
          
          COLOURS[TypeA]<-"#117733"#"purple"
          COLOURS[TypeB]<-"#44AA99"#green"
          COLOURS[BlockedCells]<-"red"
          
          
          mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),
                            color=COLOURS,fillColor=COLOURS,weight=1,fillOpacity = POLYGON_OPACITY)
          
          removeControl(mapp,layerId="legend")
          
          # If the bayesian optimization is not finished, SelectedFullTableRow() is NULL, so try again 5 seconds later
          # if (is.null(SelectedFullTableRow())) {
          #   invalidateLater(5000)
          # }
          
          SFTR<-SelectedFullTableRow()
          
          
          mapp<-
            addControl(mapp,html =   FormattedControl(SFTR$Carbon,SFTR$CarbonSD,
                                                      SPECIES,
                                                      SPECIES_ENGLISH,SFTR[SPECIES],SFTR[paste0(SPECIES,"SD")], 
                                                      SFTR$Area, SFTR$Visits, SFTR$VisitsSD), position = "topright",layerId="legend")
          
          
          
          
        }
        
      }
      
      
      PreviousClickedVector(SavedVec)  
      
      PreviousClickedVectorYear(SavedVecYear)  
      PreviousClickedVectorYearType(SavedVecYearType)  
      
      PreviousSelectedVector(SelectedVec)
      # replace the text
      PreviousYearSelectReactive(YearSelect)
      PreviousConsolidatedReactive(Consolidated)
      
    }
  })
  
  
  # Check if the slider values have been updated after the initialization
  observeEvent(input$SliderMain,{
    
    SHBICurrent<-SlidersHaveBeenInitialized()
    print(SHBICurrent)
    print(SliderNames)
    print(input)
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SHBICurrent)==0)) {
      for (sl in SliderNames){
        print(sl)
        print(input[[sl]])
        
        SliderNumber<-which(SliderNames==sl)        
        if(input[[sl]]==MaxMinValsReactiveVector()[SliderNumber]){SHBICurrent[SliderNumber]<-1;SlidersHaveBeenInitialized(SHBICurrent)}
      }}
  })
  
  
  # Check for changes in all the sliders, except on app launch
  observeEvent({input$map_shape_click
    lapply(SliderNames, function(sl) {input[[sl]]})
    input$YearSelect
  },{
    # disable map_click_shape while it is running
    # disable sliders while it is running
    # disable Year select
    # disable tabs click
    
    #debug()
    cat("starting updating values based on sliders\n")
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SlidersHaveBeenInitialized())==1)) {
      #  p<-profvis({
      # Increment the task ID every time. To allow the bayesian optimization to stop if this code is triggered again
      
      current_task_id <- get_latest_task_id() + 1
      set_latest_task_id(current_task_id)
      
      SavedVec <- ClickedVector()
      
      SelectedVec<- SelectedVector()
      SelectedDropdown <- input$inSelect
      
      SavedVecYear <- ClickedVectorYear()
      PreviousSavedVecYear<-PreviousClickedVectorYear()
      
      SavedVecYearType <- ClickedVectorYearType()
      PreviousSavedVecYearType<-PreviousClickedVectorYearType()
      
      if (!is.null(SavedVecYearType)) {
        
        AreaSelected <- AreaSelected0()
        CarbonSelected <- CarbonSelected0()
        CarbonSelectedYear<-CarbonSelectedYear0()
        CarbonSelectedYear85<-CarbonSelectedYear850()
        
        SpeciesListSelected <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "Selected0"))
          SpeciesListSelected[x] <- list(value())
        }
        VisitsSelected <- VisitsSelected0()
        
        CarbonSelectedSD <- CarbonSelectedSD0()
        CarbonSelectedSDYear <- CarbonSelectedSDYear0()
        CarbonSelectedSDYear85 <- CarbonSelectedSDYear850()
        
        SpeciesListSelectedSD <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "SelectedSD0"))
          var_name <- paste0(x, "SD")
          SpeciesListSelectedSD[var_name] <- list(value())
        }
        VisitsSelectedSD <- VisitsSelectedSD0()
        
        if (isTRUE(current_task_id != get_latest_task_id())) {
          notif(paste("task", current_task_id, "cancelled."))
          return()
        }
        
        #   tmp <- outputmap_calculateMats(input = input,
        #                                 SavedVecLoc = SavedVec,
        #                                 simul636Loc = simul636,
        #                                 AreaSelected = AreaSelected,
        #                                 CarbonSelected = CarbonSelected,
        #                                 # RedSquirrelSelected = RedSquirrelSelected,
        #                                 SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
        #                                 VisitsSelected = VisitsSelected,
        #                                 CarbonSelectedSD = CarbonSelectedSD,
        #                                 # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
        #                                 SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
        #                                 VisitsSelectedSD = VisitsSelectedSD,
        #                                 alphaLVL=alphaLVL,
        #                                 tolvec=tolvecReactive(),
        #                                 MAXYEAR=MAXYEAR)
        
        
        ########## same function with Year
        #  tmpYear <- outputmap_calculateMatsYear(input = input,
        #                                        SavedVecLoc = SavedVec,
        #                                         simul636YearLoc = simul636Year,
        #                                        AreaSelected = AreaSelected,
        #                                       CarbonSelected = CarbonSelected,
        #                                      CarbonSelectedYear =CarbonSelectedYear,
        #                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
        #                                    VisitsSelected = VisitsSelected,
        #                                   CarbonSelectedSD = CarbonSelectedSD,
        #                                  CarbonSelectedSDYear = CarbonSelectedSDYear,
        #                                 SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
        #                                VisitsSelectedSD = VisitsSelectedSD,
        #                               alphaLVL=alphaLVL,
        #                              tolvec=tolvecReactive(),
        #                             PrecalculatedCarbonSelectedTableMean=PrecalcCarbonAllExtents[[SelectedDropdown]],
        #                            PrecalculatedCarbonSelectedTableSD=PrecalcCarbonAllExtentsSD[[SelectedDropdown]],
        #                           SavedVecYearLoc = ClickedVectorYear(),
        #                          PreviousSavedVecYearLoc=PreviousClickedVector(),
        #                         SAMPLELIST=Simul636YearOverrideReactive(),
        #                        MAXYEAR=MAXYEAR
        #)
        #SelectedSimMat2 <- tmp$SelectedSimMat2
        #Icalc <- tmp$Icalc
        #LimitsMat <- tmp$LimitsMat
        #SelecTargetCarbon <- tmp$SelecTargetCarbon
        #SelecTargetArea <- tmp$SelecTargetArea
        #SelecTargetVisits <- tmp$SelecTargetVisits
        #PROBAMAT<-CalcProbaMat(Icalc$IVEC,LimitsMat,Above=AboveTargets)
        
        
        tmpYearType <- outputmap_calculateMatsYearType(input = input,
                                                       SavedVecLoc = SavedVecYearType,
                                                       simul636YearTypeLoc = simul636YearType,
                                                       AreaSelected = AreaSelected,
                                                       CarbonSelected = CarbonSelected,
                                                       CarbonSelectedYear =CarbonSelectedYear,
                                                       CarbonSelectedYear85 =CarbonSelectedYear85,
                                                       # RedSquirrelSelected = RedSquirrelSelected,
                                                       SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                                       VisitsSelected = VisitsSelected,
                                                       CarbonSelectedSD = CarbonSelectedSD,
                                                       CarbonSelectedSDYear = CarbonSelectedSDYear,
                                                       CarbonSelectedSDYear85 = CarbonSelectedSDYear85,
                                                       # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                                       SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                                       VisitsSelectedSD = VisitsSelectedSD,
                                                       alphaLVL=alphaLVL,
                                                       tolvec=tolvecReactive(),
                                                       #YearSelect=input$YearSelect,
                                                       PrecalculatedCarbonSelectedTableTypeMean=PrecalcCarbonAllExtentsType[[SelectedDropdown]],
                                                       PrecalculatedCarbonSelectedTableTypeSD=PrecalcCarbonAllExtentsSDType[[SelectedDropdown]],
                                                       SavedVecYearTypeLoc=ClickedVectorYearType(),
                                                       #  PreviousSavedVecYearTypeLoc=PreviousClickedVectorType(),
                                                       SAMPLELIST=Simul636YearTypeOverrideReactive(),
                                                       MAXYEAR=MAXYEAR
        )
        
        
        tt<-proc.time()
        
        ######## With Year
        #SelectedSimMat2 <- tmpYear$SelectedSimMat2[,-(1:(2*dim(simul636Year)[2]))]#tmpYear$SelectedSimMat2
        #Icalc <- tmpYear$Icalc
        #LimitsMat <- tmpYear$LimitsMat
        #SelecTargetCarbon <- tmpYear$SelecTargetCarbon
        #SelecTargetArea <- tmpYear$SelecTargetArea
        #SelecTargetVisits <- tmpYear$SelecTargetVisits
        
        
        SelectedSimMat2<-list()
        SelectedSimMat2[["YEAR"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.YEAR.",1:dim(simul636YearType[["YEAR"]])[2])]
        SelectedSimMat2[["TYPE"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.TYPE.",1:dim(simul636YearType[["TYPE"]])[2])]
        SelectedSimMat2[["OUTPUTS"]]<-tmpYearType$SelectedSimMat2[,-(1:(2*(dim(simul636YearType[["YEAR"]])[2])))]
        names(SelectedSimMat2[["OUTPUTS"]])<-names(tmpYearType$SelectedSimMat2[,-(1:(2*(dim(simul636YearType[["YEAR"]])[2])))])
        
        
        Icalc <- tmpYearType$Icalc
        LimitsMat <- tmpYearType$LimitsMat
        SelecTargetCarbon <-tmpYearType$SelecTargetCarbon
        SelecTargetArea <- tmpYearType$SelecTargetArea
        SelecTargetVisits <-tmpYearType$SelecTargetVisits
        
        
        PROBAMAT<-CalcProbaMat(Icalc$IVEC,LimitsMat,Above=AboveTargets)
        
        condition <- TRUE
        for (iii in 1:length(SPECIES)) {
          x<-SPECIES[iii]
          var_name <- paste0("SelecTargetBio", x)
          value <- tmpYearType[[var_name]]
          assign(var_name, value)
          
          condition <- condition & (PROBAMAT[,iii+1] >= alphaLVL)
        }
        
        #        SubsetMeetTargets <- SelectedSimMat2[(PROBAMAT[,1] >= alphaLVL) &
        #                                              condition &
        #                                             (PROBAMAT[,dim(PROBAMAT)[2]-1] >= alphaLVL) &
        #                                            (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL), ]
        #        SubsetMeetTargetsReactive(SubsetMeetTargets)
        #       SubsetMeetTargetsReactiveUnique(unique(SubsetMeetTargets))
        # Carbon
        #CONDITION_SEL<-(PROBAMAT[,1] >= alphaLVL) &
        #  condition &
        # (PROBAMAT[,dim(PROBAMAT)[2]-1] >= alphaLVL) &
        #  (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL)
        
        
        
        
        CONDITION_SEL<-ifelse(apply((Icalc$IVEC*t(matrix(2*((AboveTargets-0.5)),dim(Icalc$IVEC)[2],dim(Icalc$IVEC)[1])))<=ILevel,1,prod),TRUE,FALSE)
        
        SubsetMeetTargets <-list()
        SubsetMeetTargets[["YEAR"]]<- SelectedSimMat2$YEAR[CONDITION_SEL,]
        SubsetMeetTargets[["TYPE"]]<- SelectedSimMat2$TYPE[CONDITION_SEL,]
        SubsetMeetTargets[["OUTPUTS"]]<- SelectedSimMat2$OUTPUTS[CONDITION_SEL,]
        
        #  SubsetMeetTargets[["CarbonMean"]]<-tmpYearType$SelectedSimMat2$Carbon[CONDITION_SEL]
        #  SubsetMeetTargets[["CarbonSD"]]<-tmpYearType$SelectedSimMat2$CarbonSD[CONDITION_SEL]
        
        #  for (iii in 1:length(SPECIES)) {
        #    x<-SPECIES[iii]
        #    SubsetMeetTargets[[x]]<-tmpYearType$SelectedSimMat2[[x]][CONDITION_SEL]
        #    SubsetMeetTargets[[paste0(x,"SD")]]<-tmpYearType$SelectedSimMat2[[paste0(x,"SD")]][CONDITION_SEL]
        #  }
        #  SubsetMeetTargets[["Visits"]]<-tmpYearType$SelectedSimMat2[["Visits"]][CONDITION_SEL]
        #  SubsetMeetTargets[["VisitsSD"]]<-tmpYearType$SelectedSimMat2[["VisitsSD"]][CONDITION_SEL]
        #  SubsetMeetTargets[["Area"]]<-tmpYearType$SelectedSimMat2[["Area"]][CONDITION_SEL]
        
        if (isTRUE(current_task_id != get_latest_task_id())) {
          notif(paste("Task", current_task_id, "cancelled."))
          return()
        }
        
        SubsetMeetTargetsReactive(SubsetMeetTargets)
        
        DF<-data.frame(SubsetMeetTargets$YEAR,SubsetMeetTargets$TYPE)
        uniqueRows<-which(!duplicated(DF))
        #bowser()
        SubsetMeetTargetsReactiveUnique(list(YEAR=SubsetMeetTargets$YEAR[uniqueRows,],TYPE=SubsetMeetTargets$TYPE[uniqueRows,],
                                             OUTPUTS=SubsetMeetTargets$OUTPUTS[uniqueRows,]))
        PreviousSub<-SubsetMeetTargets
        PreviousSub$YEAR<-SubsetMeetTargets$YEAR-1
        PreviousSubsetMeetTargetsReactive(PreviousSub)
        
        DFPrev<-data.frame(PreviousSub$YEAR,PreviousSub$TYPE)
        uniqueRowsPrev<-which(!duplicated(DFPrev))
        PreviousSubsetMeetTargetsReactiveUnique(list(YEAR=PreviousSub$YEAR[uniqueRowsPrev,],
                                                     TYPE=PreviousSub$TYPE[uniqueRowsPrev,],
                                                     OUTPUTS=PreviousSub$OUTPUTS[uniqueRowsPrev,]))
        
        
        if(dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1]>0){
          LengthVec<-min(4,dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1])
          FourUniqueRowsReactive(seq(1,LengthVec))
          PreviousFourUniqueRowsReactive(seq(1,LengthVec))
        }else{FourUniqueRowsReactive(NULL)
          PreviousFourUniqueRowsReactive(NULL)}
        if (dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1] > 0) {
          if (max(tmpYearType$SelectedSimMat2$Carbon) != min(tmpYearType$SelectedSimMat2$Carbon)) {
            DistSliderCarbon <- (SubsetMeetTargets$OUTPUTS$Carbon - SelecTargetCarbon) / (max(tmpYearType$SelectedSimMat2$Carbon) - min(tmpYearType$SelectedSimMat2$Carbon))
          } else {
            DistSliderCarbon <- (SubsetMeetTargets$OUTPUTS$Carbon - SelecTargetCarbon) / (max(tmpYearType$SelectedSimMat2$Carbon))
          }
          # if (max(SelectedSimMat2$redsquirrel) != min(SelectedSimMat2$redsquirrel)) {
          #   DistSliderBio <- (SubsetMeetTargets$redsquirrel - SelecTargetBio) / (max(SelectedSimMat2$redsquirrel) - min(SelectedSimMat2$redsquirrel))
          # } else {
          #   DistSliderBio <- (SubsetMeetTargets$redsquirrel - SelecTargetBio) / (max(SelectedSimMat2$redsquirrel))
          # }
          DistSliderBioListDataframes <- list()
          for (x in SPECIES) {
            SelecTargetBiospecie <- get(paste0("SelecTargetBio", x))[[1]]
            var_name <- paste0("DistSliderBio", x)
            if (max(tmpYearType$SelectedSimMat2[x]) != min(tmpYearType$SelectedSimMat2[x])) {
              value <- (SubsetMeetTargets[["OUTPUTS"]][[x]] - SelecTargetBiospecie) / (max(tmpYearType$SelectedSimMat2[[x]]) - min(tmpYearType$SelectedSimMat2[[x]]))
            } else {
              if (max(tmpYearType$SelectedSimMat2[x]) != 0) {
                value <- (SubsetMeetTargets[["OUTPUTS"]][[x]] - SelecTargetBiospecie) / (max(tmpYearType$SelectedSimMat2[[x]]))
              } else {
                value <- (SubsetMeetTargets[["OUTPUTS"]][[x]] - SelecTargetBiospecie)
              }
            }
            assign(var_name, value)
            DistSliderBioListDataframes[x] <- data.frame(x = value)
          }
          if (max(tmpYearType$SelectedSimMat2$Area) != min(tmpYearType$SelectedSimMat2$Area)) {
            DistSliderArea <- (SelecTargetArea-SubsetMeetTargets[["OUTPUTS"]][["Area"]] ) / (max(tmpYearType$SelectedSimMat2$Area) - min(tmpYearType$SelectedSimMat2$Area))
          } else {
            DistSliderArea <- (SelecTargetArea-SubsetMeetTargets[["OUTPUTS"]][["Area"]] ) / (max(tmpYearType$SelectedSimMat2$Area))
          }
          if (max(tmpYearType$SelectedSimMat2$Visits) != min(tmpYearType$SelectedSimMat2$Visits)) {
            DistSliderVisits <- (SubsetMeetTargets[["OUTPUTS"]][["Visits"]] - SelecTargetVisits) / (max(tmpYearType$SelectedSimMat2$Visits) - min(tmpYearType$SelectedSimMat2$Visits))
          } else {
            DistSliderVisits <- (SubsetMeetTargets[["OUTPUTS"]][["Visits"]] - SelecTargetVisits) / (max(tmpYearType$SelectedSimMat2$Visits))
          }
          
          if (isTRUE(current_task_id != get_latest_task_id())) {
            notif(paste("Task", current_task_id, "cancelled."))
            return()
          }
          
          DistSliderBioDataframe <- do.call(cbind, DistSliderBioListDataframes)
          # SelecdMinRows <- which((DistSliderCarbon + DistSliderBio + DistSliderArea + DistSliderVisits) == min(DistSliderCarbon + DistSliderBio + DistSliderArea + DistSliderVisits))
          # SelecdMinRows <- which((DistSliderCarbon + DistSliderBio1 + DistSliderBio2 + DistSliderArea + DistSliderVisits) == min(DistSliderCarbon + DistSliderBio1 + DistSliderBio2 + DistSliderArea + DistSliderVisits))
          #SelecdMinRows <- which.min(DistSliderCarbon + rowSums(DistSliderBioDataframe) + DistSliderArea + DistSliderVisits)
          #SelectedMins <- SubsetMeetTargets[SelecdMinRows, ]
          #SelecRow <- which.min(rowSums(SelectedMins[1:length(SavedVec), ]))
          # We consider that all biodiversity are as important and carbon and visits. The area is not taken into account in the minimization
          # as it is a below target
          SUMM <- DistSliderCarbon + rowSums(DistSliderBioDataframe)+  DistSliderVisits
          SelecdMinRows <- which(SUMM == min(SUMM))
          SelectedMins <- list(YEAR=SubsetMeetTargets$YEAR[SelecdMinRows, ],TYPE=SubsetMeetTargets$TYPE[SelecdMinRows,],
                               OUTPUTS=SubsetMeetTargets$OUTPUTS[SelecdMinRows,])
          
          
          # If it is a vector, i.e. only 1 unit is available
          if (length(SavedVec) == 1) {
            result <- SelectedMins$YEAR
            #[, 1]
          } else {
            # If it is a data frame
            result <- rowSums(SelectedMins$YEAR)
          }
          SelecRow <- which.min(result)
          
          if (isTRUE(current_task_id != get_latest_task_id())) {
            notif(paste("Task", current_task_id, "cancelled."))
            return()
          }
          
          SelecTargetBioVector <- c()
          for (x in names(SpeciesListSelected)) {
            var_name <- paste0("SelecTargetBio", x)
            # input[paste0("BioSlider", x)] bugs because it is a reactivevalue
            value <- input[[paste0("BioSlider", x)]]
            assign(var_name, value)
            SelecTargetBioVector <- c(SelecTargetBioVector, setNames(value, x))
          }
          
          names(SelecTargetCarbon) <- "Carbon"
          names(SelecTargetVisits) <- "Visits"
          
          SelectedFullTableRow(
            tmpYearType$SelectedSimMat2[CONDITION_SEL,][SelecdMinRows[SelecRow],]
          )
          SelectedVector(SelectedMins)
          notif("Updated SelectedMins and SelectedFullTableRow. If case")
        } else {
          ZeroSelected<-tmpYearType$SelectedSimMat2[1,]
          ZeroSelected<-replace(ZeroSelected,1:(length(SavedVecYearType)),-1)
          ZeroSelected<-replace(ZeroSelected,(length(SavedVecYearType)+1):(2*length(SavedVecYearType)),"NoPlanting")
          
          ZeroSelected[1,(2*length(SavedVecYearType)+1):dim(ZeroSelected)[2]]<-0
          names(ZeroSelected)<-names(tmpYearType$SelectedSimMat2)
          SelectedFullTableRow(ZeroSelected)
          TOSAVE<-list(YEAR=as.numeric(ZeroSelected[1, 1:(length(SavedVecYearType))]),
                       TYPE=as.character(ZeroSelected[1, (length(SavedVecYearType)+1):(2*length(SavedVecYearType))])
          )
          SelectedVector(TOSAVE)
          notif("Updated SelectedMins and SelectedFullTableRow. Else case")
        }
        #cat(proc.time()-tt)
        #cat("\n")
        
        if(RUN_BO){
          
          msg <- paste0("task ", current_task_id, " BO start")
          notif(msg)
          showNotification(msg)
          bayesian_optimization_finished(FALSE)
          
          if (isFALSE(is.null(infpref_reactive()))) {
            # Order is c("Carbon", SPECIES, "Area", "Visits")
            len <- length(infpref_reactive())
            preference_weight_area <- infpref_reactive()[len - 1]
            mypref <- infpref_reactive()[c(1:(len - 2), len)]
          } else {
            preference_weight_area <- - 1
            mypref <- rep(1, N_TARGETS - 1)
            names(mypref) <- c("Carbon", SPECIES, "Visits")
          }
          tolvec <- tolvecReactive()
          # Slider values are AreaSliderVal(), CarbonSliderVal(), BioSliderVal...(), VisitsSliderVal()
          BioSliderVals <- setNames(sapply(paste0("BioSliderVal", SPECIES), function(x){get(x)()}), SPECIES)
          
          # We can plant at this year + 1, in each parcel
          # ClickedVector contains -1 for no planting, and 0...MAXYEAR for year after (strictly) which we can plant
          year_of_max_no_planting_threshold_vector <- ClickedVector()
          
          # https://shiny.posit.co/r/articles/improve/nonblocking/index.html
          bayesian_optimization_extendedtask <- ExtendedTask$new(function(...) {
            future_promise(expr = {
              
              notif(paste0("task ", current_task_id, ", pid ", Sys.getpid(), ", New process started"))
              
              if (isTRUE(current_task_id != get_latest_task_id())) {
                notif(paste("task", current_task_id, "cancelled."))
                return(FALSE)
              }
              
              bo_results <- bayesian_optimization(...)
              return(bo_results)
            }, seed = NULL) %...>% {
              bo_results <- .
              
              # Check if this result is invalid (i.e. a newer task has started)
              if (isFALSE(bo_results) || isTRUE(current_task_id != get_latest_task_id())) {
                msg <- paste0("task ", current_task_id, " The previous Bayesian optimization task was cancelled.")
                notif(msg)
                showNotification(msg)
                return(FALSE)
              } else { # If the result is valid (i.e. there are no new tasks started)
                
                # If no results, i.e. no feasible solution
                if (any(is.na(bo_results))) {
                  showNotification("No feasible solution found")
                  return(NA)
                }
                
                outcome <- get_outcomes_from_strategy(parameter_vector = bo_results$strategy_vector,
                                                      FullTable_long_arg = FullTable_long)
                
                # Otherwise, a feasible solution is found
                area_sum <- outcome$sum_area
                year_planting <- as.numeric(bo_results$strategy_vector[grep("plantingyear", names(bo_results$strategy_vector))])
                treespecie <- bo_results$strategy_vector[grep("treespecie", names(bo_results$strategy_vector))]
                indices_no_planting <- which(as.numeric(bo_results$strategy_vector[grep("area", names(bo_results$strategy_vector))]) == 0)
                treespecie[indices_no_planting] <- "NoPlanting"
                
                # ORDER OF SPECIES/GROUPS IS SET BY THE USER (outcomes.json), GROUPS ALWAYS COME BEFORE THE SPECIES
                # year_planting (-1:24); treespecie (NoPlanting,Conifers,Deciduous), Carbon, All, Birds, Area, Visits, CarbonSD, AllSD, BirdsSD, VisitsSD
                selectedfulltablerowvalue <- as.data.frame(matrix(c(year_planting,
                                                                    treespecie,
                                                                    # CarbonMean, BioMeans, Area, VisitsMean
                                                                    unlist(outcome[c("sum_carbon", "sum_richness", "sum_biodiversity", "sum_area", "sum_visits")]),
                                                                    # CarbonSD, BioSD, VisitsSD
                                                                    unlist(outcome[c("sum_carbon_sd", "sum_richness_sd", "sum_biodiversity_sd", "sum_visits_sd")])),
                                                                  nrow = 1))
                colnames(selectedfulltablerowvalue) <- names(tmpYearType$SelectedSimMat2)
                
                # Carbon, All, Birds, Area, Visits, CarbonSD, AllSD, BirdsSD, VisitsSD
                richness_data_frame <- do.call("data.frame",
                                               setNames(lapply(SPECIES, function(x) bquote(outcome$sum_richness[.(x)])),
                                                        SPECIES))
                biodiversity_data_frame <- do.call("data.frame",
                                                   setNames(lapply(SPECIES, function(x) bquote(outcome$sum_biodiversity[.(x)])),
                                                            SPECIES))
                richness_sd_data_frame <- do.call("data.frame",
                                                  setNames(lapply(SPECIES, function(x) bquote(outcome$sum_richness_sd[.(x)])),
                                                           paste0(SPECIES, "SD")))
                biodiversity_sd_data_frame <- do.call("data.frame",
                                                      setNames(lapply(SPECIES, function(x) bquote(outcome$sum_biodiversity_sd[.(x)])),
                                                               paste0(SPECIES, "SD")))
                selectedvectorvalue_output <- data.frame("Carbon" = outcome$sum_carbon)
                if (nrow(richness_data_frame) != 0) {
                  selectedvectorvalue_output <- cbind(selectedvectorvalue_output, richness_data_frame)
                }
                if (nrow(biodiversity_data_frame) != 0) {
                  selectedvectorvalue_output <- cbind(selectedvectorvalue_output, biodiversity_data_frame)
                }
                selectedvectorvalue_output <- cbind(selectedvectorvalue_output, data.frame("Area" = outcome$sum_area,
                                                                                           "Visits" = outcome$sum_visits,
                                                                                           "CarbonSD" = outcome$sum_carbon))
                if (nrow(richness_sd_data_frame) != 0) {
                  selectedvectorvalue_output <- cbind(selectedvectorvalue_output, richness_sd_data_frame)
                }
                if (nrow(biodiversity_sd_data_frame) != 0) {
                  selectedvectorvalue_output <- cbind(selectedvectorvalue_output, biodiversity_sd_data_frame)
                }
                selectedvectorvalue_output <- cbind(selectedvectorvalue_output, data.frame("VisitsSD" = outcome$sum_visits_sd))
                
                selectedvectorvalue <- list(YEAR = as.data.frame(matrix(year_planting, nrow = 1)),
                                            TYPE = as.data.frame(matrix(treespecie, nrow = 1)),
                                            OUTPUTS = as.data.frame(matrix(selectedvectorvalue_output, nrow = 1)))
                
                # SelectedFullTableRow(SelectedMins[SelecRow, ])
                # SelectedVector(SelectedMins[SelecRow, 1:length(SavedVec)])
                SelectedFullTableRow(selectedfulltablerowvalue)
                SelectedVector(selectedvectorvalue)
                
                notif(paste(current_task_id, "Bayesian optimization finished successfully"))
                showNotification(paste(current_task_id, "[INFO] Bayesian optimization finished successfully"))
                notif(paste(current_task_id, "sum_carbon=", outcome$sum_carbon))
                
              }
              return(TRUE)
            } %...!% {
              error <- .
              if (is.null(error)) {
                msg <- paste0("task ", current_task_id, " future_promise resulted in NULL")
              } else {
                msg <- paste0("task ", current_task_id, " future_promise resulted in the error: ", toString(error))
              }
              
              showNotification(paste("[ERROR]", msg))
              notif(msg, log_level = "error", limit_log_level = LOG_LEVEL)
              return(FALSE)
            } %>%
              finally(function() {
                bayesian_optimization_finished(TRUE)
                return(.)
              })
          })
          
          # # shiny::withProgress(
          # progressr::withProgressShiny(
          #   # progressr::with_progress(
          #   message = "Finding strategy",
          #   value = 0,
          #   # session = session,
          #   # min = 0,
          #   # max = BAYESIAN_OPTIMIZATION_ITERATIONS * 3,
          #   expr = {
          # my_progressr_object <- progressor(steps = 5 * 3, message = "Bayesian optimization")
          FullTable_long <- transform_FullTable_wide_to_long(FullTable_arg = FullTable,
                                                             SCENARIO_arg = SCENARIO,
                                                             MAXYEAR_arg = MAXYEAR,
                                                             verbose = FALSE)
          
          bayesian_optimization_extendedtask$invoke(seed = 1,
                                                    FullTable_arg = FullTable,
                                                    FullTable_long_arg = FullTable_long,
                                                    MAXYEAR = MAXYEAR,
                                                    SCENARIO = SCENARIO,
                                                    year_of_max_no_planting_threshold_vector = year_of_max_no_planting_threshold_vector,
                                                    area_sum_threshold = AreaSliderVal(),
                                                    outcomes_to_maximize_sum_threshold_vector = c("Carbon" = CarbonSliderVal(), BioSliderVals, "Visits" = VisitsSliderVal()),
                                                    outcomes_to_minimize_sum_threshold_vector = NULL,
                                                    limit_log_level = LOG_LEVEL,
                                                    PLOT = FALSE,
                                                    
                                                    BAYESIAN_OPTIMIZATION_ITERATIONS = 10,
                                                    # progressr_object = function(amount = 0, message = "") {},
                                                    # progressr_object_arg = my_progressr_object,
                                                    BAYESIAN_OPTIMIZATION_BATCH_SIZE = 1,
                                                    PENALTY_COEFFICIENT = 2000,
                                                    # PENALTY_COEFFICIENT = 10 * max(FullTable %>% select(contains("Mean"))),
                                                    EXPLORATION = FALSE, # FALSE for tab 1, TRUE for tab 2
                                                    EXPLORATION_COEFFICIENT = 0,
                                                    
                                                    preference_weight_area = preference_weight_area,
                                                    preference_weights_maximize = mypref,
                                                    # preference_weights_minimize = rep(1, length(c())),
                                                    
                                                    current_task_id = current_task_id,
                                                    
                                                    CUSTOM_DESIGN_POINTS_STRATEGIES = c("expected improvement", "probability of improvement"),
                                                    DESIGN_POINTS_STRATEGY = "expected improvement",
                                                    
                                                    CONSTRAINED_INPUTS = TRUE,
                                                    
                                                    RREMBO_CONTROL = RREMBO_CONTROL,
                                                    RREMBO_HYPER_PARAMETERS_arg = RREMBO_HYPER_PARAMETERS,
                                                    RREMBO_SMART = FALSE,
                                                    
                                                    # GP
                                                    KERNEL = "matern2.5", # matern2.5 or sexp
                                                    NUMBER_OF_VECCHIA_NEIGHBOURS = 20,
                                                    
                                                    tolvec = tolvec,
                                                    alpha = alphaLVL)
          # Return result with
          # while (bayesian_optimization_extendedtask$status() %in% c("initial", "running")) {
          #   later::run_now(timeout = 1)
          # }
          # bayesian_optimization_extendedtask$result()
        }
        
      }
      #end of profvis
      #  })
      #   saveWidget(p, "d:\\profvis_output.html")
      
      
    }
    
    
    #cat("ended updating values based on sliders\n")
  }, ignoreInit = TRUE)
  observe({
    status <- bayesian_optimization_finished()
    notif(paste("bayesian_optimization_finished() update:", status))
    shinyjs::addClass(id = "task_status", class = if (isTRUE(status)) "finished" else "running")
    shinyjs::removeClass(id = "task_status", class = if (isTRUE(status)) "running" else "finished")
  })
  
  
  ### If we are not on the Preference tab and some new data has been added, then re-update the weights
  ### Note that there could be more data than is actually used in the preferences. Then we take the max
  ### value in pref_reactive()$prefs as the last row that is used.
  observeEvent(input$tabs,{
    
    if(!is.null(pref_reactive()$prefs)){
      if((PrefWeightsAlreadyCalculatedNbRows()<max(pref_reactive()$prefs))&(input$tabs != "Preferences"))
      {
        pref_reactive()$update()
        PrefWeightsAlreadyCalculatedNbRows(max(pref_reactive()$prefs))
        infpref_reactive( pref_reactive()$posterior_mean)
      }}
  })
  
  observeEvent({input$map_shape_click}, {
    click <- input$map_shape_click
    SelectedDropdown <- input$inSelect
    SelectedRowsUnits <- FullTable$units[FullTable$extent == SelectedDropdown]
    
    
    #GEOVEC <- st_geometry_type(FullTable$geometry)
    
    if (!is.null(click$id)) {
      ChangeDone <- FALSE
      SavedVec <- ClickedVector()
      SavedVecYear <- ClickedVectorYear()
      SavedVecYearType <- ClickedVectorYearType()
      SavedVecYearPriorToChange<-SavedVecYear
      SavedVecYearTypePriorToChange<-SavedVecYearType
      #PreviousSavedVecYear<-PreviousClickedVectorYear()
      SAMPLELIST<-Simul636YearOverrideReactive()
      SAMPLELIST_TYPE<-Simul636YearTypeOverrideReactive()
      
      iii <- 1
      
      while ((!ChangeDone) && (iii <= length(SavedVec))) {
        if ((click$id == paste0("Square", iii))) {
          
          SavedVec[iii] <- ifelse(SavedVec[iii] == 1, 0, 1);
          
          if(SavedVecYear[iii]==(MAXYEAR+1)){SavedVecYear[iii] <- (input$YearSelect-STARTYEAR);}else{
            SavedVecYear[iii]<-ifelse(SavedVecYear[iii]>= (input$YearSelect-STARTYEAR), (MAXYEAR+1), (input$YearSelect-STARTYEAR))}
          #If there has been a changed in SavedVec, then updated the list containing the override of simul636
          
          
          if(SavedVecYearPriorToChange[iii]!=SavedVecYear[iii]){
            if(SavedVecYear[iii]==(MAXYEAR+1)){SAMPLELIST[iii]<-list(NULL)}else{
              if(SavedVecYear[iii]==MAXYEAR){
                SAMPLELIST[[iii]]<-rep(29,dim(simul636YearType$YEAR)[1])}else{
                  SAMPLELIST[[ iii]]<- sample((SavedVecYear[ iii]+1):(MAXYEAR+1),dim(simul636YearType$YEAR)[1], replace=T)
                  
                }
            }
            
          }
          
          if(SavedVecYearType[iii]==(-1)){SavedVecYearType[iii] <- (input$YearSelect-STARTYEAR);}else{
            SavedVecYearType[iii]<-ifelse(SavedVecYearType[iii]>= (input$YearSelect-STARTYEAR), (-1), (input$YearSelect-STARTYEAR))}
          
          if(SavedVecYearTypePriorToChange[iii]!=SavedVecYearType[iii]){
            if(SavedVecYearType[iii]==(-1)){SAMPLELIST_TYPE[iii]<-list(NULL)}else{
              if(SavedVecYearType[iii]==MAXYEAR){
                
                SAMPLELIST_TYPE[[iii]]<-list()
                SAMPLELIST_TYPE[[iii]][["YEAR"]]<-rep(-1,dim(simul636YearType$YEAR)[1])
                SAMPLELIST_TYPE[[iii]][["TYPE"]]<-rep("NoPlanting",dim(simul636YearType$YEAR)[1])
              }else{
                
                SAMPLELIST_TYPE[[iii]]<-list()
                SAMPLELIST_TYPE[[iii]][["YEAR"]]<-simul636YearType$YEAR[,iii]
                SAMPLELIST_TYPE[[iii]][["TYPE"]]<-simul636YearType$TYPE[,iii]
                
                ALREADYC<-(SAMPLELIST_TYPE[[iii]]$TYPE!="NoPlanting")
                
                SAMPLELIST_TYPE[[iii]]$YEAR[!ALREADYC]<-sample((SavedVecYearType[ iii]+1):MAXYEAR,sum(!ALREADYC), replace=T)
                
              }
            }
            
          }
          #### TO CHANGE  
        }
        iii <- iii + 1
      }
      
      ClickedVector(SavedVec)
      ClickedVectorYear(SavedVecYear)
      ClickedVectorYearType(SavedVecYearType)
      
      Simul636YearOverrideReactive(SAMPLELIST)
      Simul636YearTypeOverrideReactive(SAMPLELIST_TYPE)
      
      #PreviousClickedVectorYear(SavedVecYear)
      
      ClickedMatrixTab2Reactive(t(matrix(SavedVec,length(SavedVec),4)))
      ChangeDone <- TRUE
      
      
    }
    
  })
  
  
  # In this part, we plot the first version of the map
  # DONE TO CHANGE LATER
  output$map <- renderLeaflet({
    #  shinyjs::hide("tabs")
    
    
    if((CreatedBaseMap()==0)&(UpdatedExtent()==1)){
      SavedVec <- ClickedVector()
      SavedVecYear <- ClickedVectorYear()
      
      SelectedVec <- SelectedVector()
      SelectedDropdown <- input$inSelect
      calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main", shconv, GreyPolygonWidth = GreyPolygonWidth)
      calcBaseMapNoLegend <- BaseMap2(SelectedDropdown, layerId = "main", shconv, GreyPolygonWidth = GreyPolygonWidth,PrintLegend=FALSE)
      
      map <- calcBaseMap$map
      mapNoLegend<-calcBaseMapNoLegend$map
      
      if (!is.null(SavedVec)) {
        
        AreaSelected <- AreaSelected0()
        CarbonSelected <- CarbonSelected0()
        CarbonSelectedYear<-CarbonSelectedYear0()
        CarbonSelectedYear85<-CarbonSelectedYear850()
        
        # RedSquirrelSelected <- RedSquirrelSelected0()
        SpeciesListSelected <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "Selected0"))
          SpeciesListSelected[x] <- list(value())
        }
        VisitsSelected <- VisitsSelected0()
        
        CarbonSelectedSD <- CarbonSelectedSD0()
        CarbonSelectedSDYear <- CarbonSelectedSDYear0()
        CarbonSelectedSDYear85<-CarbonSelectedSDYear850()
        
        # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
        SpeciesListSelectedSD <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "SelectedSD0"))
          var_name <- paste0(x, "SD")
          SpeciesListSelectedSD[var_name] <- list(value())
        }
        VisitsSelectedSD <- VisitsSelectedSD0()
        
        
        TwoRows<-matrix(1,nrow=2,ncol=dim(FullTable)[1])
        TwoRowsYearType<-list()
        TwoRowsYearType[["YEAR"]]<-matrix(0,2,dim(simul636YearType$YEAR)[2])
        TwoRowsYearType[["TYPE"]]<-matrix("Deciduous",2,dim(simul636YearType$YEAR)[2])
        #  tmp <- outputmap_calculateMats(input = input,
        #                                 SavedVecLoc = TwoRows[1,],
        #                                 simul636Loc = TwoRows,
        #                                 AreaSelected = AreaSelected,
        #                                 CarbonSelected = CarbonSelected,
        #                                 # RedSquirrelSelected = RedSquirrelSelected,
        #                                 SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
        #                                 VisitsSelected = VisitsSelected,
        #                                 CarbonSelectedSD = CarbonSelectedSD,
        #                                 # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
        #                                 SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
        #                                 VisitsSelectedSD = VisitsSelectedSD,
        #                                 alphaLVL = 0,tolvec=tolvecReactive()) # At the beginning we want to switch on all the sliders
        #  browser()
        
        tmpYearType <- outputmap_calculateMatsYearType(input = input,
                                                       SavedVecLoc = TwoRows[1,],
                                                       simul636YearTypeLoc = TwoRowsYearType,
                                                       AreaSelected = AreaSelected,
                                                       CarbonSelected = CarbonSelected,
                                                       CarbonSelectedYear =CarbonSelectedYear,
                                                       CarbonSelectedYear85 =CarbonSelectedYear85,
                                                       # RedSquirrelSelected = RedSquirrelSelected,
                                                       SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                                       VisitsSelected = VisitsSelected,
                                                       CarbonSelectedSD = CarbonSelectedSD,
                                                       CarbonSelectedSDYear = CarbonSelectedSDYear,
                                                       CarbonSelectedSDYear85 = CarbonSelectedSDYear85,
                                                       # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                                       SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                                       VisitsSelectedSD = VisitsSelectedSD,
                                                       alphaLVL=alphaLVL,
                                                       tolvec=tolvecReactive(),
                                                       #YearSelect=input$YearSelect,
                                                       PrecalculatedCarbonSelectedTableTypeMean=PrecalcCarbonAllExtentsType2Lines[[SelectedDropdown]],
                                                       PrecalculatedCarbonSelectedTableTypeSD=PrecalcCarbonAllExtentsSDType2Lines[[SelectedDropdown]],
                                                       SavedVecYearTypeLoc=ClickedVectorYearType(),
                                                       # PreviousSavedVecYearTypeLoc=PreviousClickedVectorType(),
                                                       SAMPLELIST=Simul636YearTypeOverrideReactive(),
                                                       MAXYEAR=MAXYEAR
        )
        
        #################
        
        SelecRow<-1
        #        SelectedMins <- tmp$SelectedSimMat2
        SelectedMins <- tmpYearType$SelectedSimMat2
        #SwitchedOnCells <- SelectedMins[SelecRow, 1:length(SavedVec)]
        SwitchedOnCells <-rep(1,length(TwoRows[1,]))
        #ToRemove<-which(names(tmpYearType$SelectedSimMat2)%in%
        #                  c(paste0("SelectedSimMat.YEAR.",1:length(TwoRows[1,])),
        #                           paste0("SelectedSimMat.TYPE.",1:length(TwoRows[1,])
        #                                  )
        #                  ))
        #SwitchedOnCells<-data.frame(rep(1,length(TwoRows[1,])),SwitchedOnCells[,-ToRemove])      
        
        
        SELL <- (FullTable$extent == SelectedDropdown)
        if (!is.null(SELL)) {
          SelectedTreeCarbon <- SelectedMins[SelecRow, ]$Carbon
          # SelectedBio <- SelectedMins[SelecRow, ]$redsquirrel
          for (x in SPECIES) {
            var_name <- paste0("SelectedBio", x)
            value <- SelectedMins[SelecRow, x]
            assign(var_name, value)
          }
          SelectedArea <- SelectedMins[SelecRow, ]$Area
          SelectedVisits <- SelectedMins[SelecRow, ]$Visits
          
          SelectedTreeCarbonSD <- SelectedMins[SelecRow, ]$CarbonSD
          # SelectedBioSD <- SelectedMins[SelecRow, ]$redsquirrelSD
          for (x in SPECIES) {
            var_name <- paste0("SelectedBioSD", x)
            value <- SelectedMins[SelecRow, paste0(x, "SD")]
            assign(var_name, value)
          }
          SelectedVisitsSD <- SelectedMins[SelecRow, ]$VisitsSD
          
          
          SELGEOFull <- FullTable[SELL, ]
          SELGEOFull$layerId <- paste0("Square", 1:dim(SELGEOFull)[1])
          SELGEO <- FullTable$geometry[SELL]
          
          ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units[SELL],
                                 ColorLighteningFactor(), ColorDarkeningFactor())
          
          FullColVec <- ColObtained$FullColVec
          ClickedCols <- ColObtained$ClickedCols
          SELGEOFull$color <- ColObtained$FullColVec
          SELGEOFull$color[SavedVec == 1] <- ColObtained$ClickedCols[SavedVec == 1]
          
          
          
          SELGEOSavedVec <- SELGEOFull[, c("geometry", "layerId")]
          SELGEOSwitched <- SELGEOFull[ c("geometry", "layerId")]
          
          SELGEOSavedVec <- SELGEOSavedVec[SavedVec == 1, ]#;gpNamesSavedVec <- gpNamesSavedVec[SavedVec]
          SELGEOSwitched <- SELGEOSwitched[(SwitchedOnCells == 1) & (SavedVec != 1), ]#;gpNamesSwitched <- gpNamesSwitched[SwitchedOnCells & (!SavedVec)]
          SELGEORemaining <- SELGEOFull[(SavedVec == 1) | (SwitchedOnCells == 1), c("geometry", "layerId", "color")]
          
          
          
          if (dim(SELGEORemaining)[1] > 0) {
            map <- addPolygons(map, data = SELGEORemaining, color = SELGEORemaining$color, 
                               layerId = ~SELGEORemaining$layerId, weight = UnitPolygonColours,fillOpacity = POLYGON_OPACITY)
            mapNoLegend <- addPolygons(mapNoLegend, data = SELGEORemaining, color = SELGEORemaining$color, 
                                       layerId = ~SELGEORemaining$layerId, weight = UnitPolygonColours,fillOpacity = POLYGON_OPACITY)
            
          }
          
          BioMean <- data.frame(matrix(ncol = length(SPECIES), nrow = 1))
          colnames(BioMean) <- SPECIES
          BioSD <- data.frame(matrix(ncol = length(SPECIES), nrow = 1))
          colnames(BioSD) <- paste0(SPECIES,"SD")
          
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
            selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
            BioMean[specie_latin]<-selectedBiospecie
            BioSD[paste0(specie_latin,"SD")]<-selectedBioSDspecie
          }
          
          map<-
            addControl(map,html =   FormattedControl(SelectedTreeCarbon,
                                                     SelectedTreeCarbonSD,
                                                     SPECIES,
                                                     SPECIES_ENGLISH,
                                                     BioMean,
                                                     BioSD, 
                                                     SelectedArea, 
                                                     SelectedVisits, SelectedVisitsSD), position = "topright",layerId="legend")
          mapNoLegend<-
            addControl(mapNoLegend,html =   FormattedControl(SelectedTreeCarbon,
                                                             SelectedTreeCarbonSD,
                                                             SPECIES,
                                                             SPECIES_ENGLISH,
                                                             BioMean,
                                                             BioSD, 
                                                             SelectedArea, 
                                                             SelectedVisits, SelectedVisitsSD), position = "topright",layerId="legend")
          
          
          
          
          
          
          
        }
        #} else { map <- map %>%
        #  addControl(html = paste0("<p> Targets Cannot be met</p>"), position = "topright")
        #}
      }
      map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
      mapNoLegend <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = mapNoLegend)
      MapReactiveNoLegend(mapNoLegend)
      
      MapReactive(map)
      CreatedBaseMap(1)
      MapReactive()
      
      
    }else{    MapReactive()
    }
    # ChangeSliders(FALSE)
    # shinyjs::show("tabs")
    
  })
  
  
  # When user changes tab, the leaflet map is not correct
  # User needs to resize maps to re-render them
  # This fixes it (TODO: 2 tabs left to do)
  observeEvent(input$tabs, {
    runjs('$(document).trigger("shown.htmlwidgets");')
    if (input$tabs == "Maps") {
      runjs('
            var widget = HTMLWidgets.find("#map");
            widget.resize($("#map")[0], $("#map").width(), $("#map").height());
          
            ') 
    }
    if (input$tabs == "Preferences") {
      runjs('
  var widget = HTMLWidgets.find("#ClusterPage");
      widget.resize($("#ClusterPage")[0], $("#ClusterPage").width(), $("#ClusterPage").height());
   
            ')
      
      runjs('
    var widget2 = HTMLWidgets.find("#ClusterPage2");
      widget2.resize($("#ClusterPage2")[0], $("#ClusterPage2").width(), $("#ClusterPage2").height());
           ')
      
      
    }
  })
  
  # On session close, delete temporary files
  onSessionEnded(function() {
    if (SESSION_FILE_SUFFIX != "") {
      session_files <- paste0(normalizePath(file.path(FolderSource)), "/*", SESSION_FILE_SUFFIX, "*")
      unlink(session_files)
    }
  })
  
  onStop(function() {
    if (SESSION_FILE_SUFFIX != "") {
      session_files <- paste0(normalizePath(file.path(FolderSource)), "/*", SESSION_FILE_SUFFIX, "*")
      unlink(session_files)
    }
  })
  # On error, kill the background processes
  onUnhandledError(function(err) {
    # The background processes check the task id, and end if they are not the latest task
    set_latest_task_id(-1)
    
    if (SESSION_FILE_SUFFIX != "") {
      session_files <- paste0(normalizePath(file.path(FolderSource)), "/*", SESSION_FILE_SUFFIX, "*")
      unlink(session_files)
    }
    
    # log the unhandled error
    # level <- if (inherits(err, "shiny.error.fatal")) "FATAL" else "ERROR"
    notif(paste0("onUnhandledError triggered with error: ", conditionMessage(err)), log_level = "error")
  })
  observeEvent(input$crash, function(){
    # The background processes check the task id, and end if they are not the latest task
    set_latest_task_id(-1)
    
    if (SESSION_FILE_SUFFIX != "") {
      session_files <- paste0(normalizePath(file.path(FolderSource)), "/*", SESSION_FILE_SUFFIX, "*")
      unlink(session_files)
    }
    notif("Oops, input$crash triggered: an unhandled error happened!", log_level = "error")
    stop("Oops, input$crash triggered: an unhandled error happened!")
  })
  
}

shinyApp(ui, server)