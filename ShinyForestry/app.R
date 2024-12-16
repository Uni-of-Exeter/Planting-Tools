# Created and maintained by Bertrand Nortier and Timothée Bacri

#library(profvis)
#library(htmlwidgets)

# Ubuntu packages needed
# sudo apt-get -y --no-install-recommends install libcurl4-openssl-dev
# sudo apt-get -y --no-install-recommends install libfontconfig1-dev
# sudo apt-get -y --no-install-recommends install libxml2-dev
# sudo apt-get -y --no-install-recommends install libudunits2-dev
# sudo apt-get -y --no-install-recommends install libssl-dev
# sudo apt-get -y --no-install-recommends install libfontconfig1-dev
# sudo apt-get -y --no-install-recommends install libproj-dev
# sudo apt-get -y --no-install-recommends install cmake
# sudo apt-get -y --no-install-recommends install libgdal-dev
# sudo apt-get -y --no-install-recommends install libharfbuzz-dev
# sudo apt-get -y --no-install-recommends install libfribidi-dev

# options(warn=2, error=recover)
# options(warn=2)
# options(warn=0) # default
options(shiny.error = browser)
options(shiny.reactlog = TRUE)
options(future.globals.maxSize = 3 * 1024^3) # 3 GiB RAM

ANALYSISMODE<-FALSE
SHOW_TITLES_ON_CLUSTERING_PAGE<-F

RUN_BO<-FALSE
set.seed(1)
#fixed strategies list contains strategies pre-selected to be shown in the preference elicitation
FIXED_STRATEGIES_LIST<-list(YEAR=matrix(0,0,1),TYPE=matrix(0,0,1),OUTPUTS=matrix(0,0,1))
POLYGON_OPACITY<-0.6
GREY_BACKGROUND_OPACITY<-0.3
NOTAVAIL_OPACITY<-0.7


if(Sys.getenv("USERNAME")=="bn267"){
  FolderSource <- "C://Users//bn267//OneDrive - University of Exeter//Documents//GitHub//Planting-Tools//ShinyForestry//"
}


# more --> less: debug / info / warning / error / none
LOG_LEVEL <- "error"

# FolderSource <- "ShinyForestry/"
FolderSource <- normalizePath(getwd())
if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
  FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
}

set.seed(1)

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
  "progress",
  "car", "shinyjs", "shiny", "shinyjqui", "shiny.fluent", "reactlog","leaflet", "sf", "ggplot2",
  "geosphere", "feather", "readr", "dplyr", "tidyverse", "gsubfn",
  "ggpubr", "htmltools","comprehenr", "Rtsne", "mclust", "seriation", "jsonlite",
  "viridis", "ggmap", "shinyjqui", "MASS", "mgcv", "shinyWidgets", "truncnorm",
  "GGally", "purrr", "sp", "colorspace", "rjson", "arrow", "lwgeom",
  "mvtnorm", "dplyr", "magrittr",
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
# Bertrand's computer has issues loading and installing packages
if (Sys.getenv("USERNAME")=="bn267") {
  library("dgpsi")
  library("RRembo")
  for(ll in 1:length(packages)) {
    library(packages[ll], character.only = TRUE)
  }
} else {
  
  if (!require(dgpsi)) {
    # devtools on Linux requires testthat and pkgload (https://stackoverflow.com/questions/61643552/r-devtools-unable-to-install-ubuntu-20-04-package-or-namespace-load-failed-f)
    install_and_load_packages("testthat", verbose = FALSE)
    install_and_load_packages("pkgload", verbose = FALSE)
    install_and_load_packages("devtools", verbose = FALSE)
    devtools::install_github('mingdeyu/dgpsi-R', upgrade = "always", quiet = TRUE)
    library("dgpsi")
  }
  if (!require(RRembo)) {
    # devtools on Linux requires testthat and pkgload (https://stackoverflow.com/questions/61643552/r-devtools-unable-to-install-ubuntu-20-04-package-or-namespace-load-failed-f)
    install_and_load_packages("testthat", verbose = FALSE)
    install_and_load_packages("pkgload", verbose = FALSE)
    install_and_load_packages("devtools", verbose = FALSE)
    # RRembo needs mvtnorm loaded
    install_and_load_packages("mvtnorm", verbose = FALSE)
    devtools::install_github('mbinois/RRembo', upgrade = "always", quiet = TRUE)
    library("RRembo")
  }
  install_and_load_packages(packages = packages, update = FALSE)
}
if (RUN_BO) {
  dgpsi::init_py()
}

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


if(Sys.getenv("USERNAME")=="bn267"){
DataFilesFolder <- "d:\\JulesOP\\"
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
    MER[[ii]] <- st_union(st_make_valid(SELLL))
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
  FullTable <- add_richness_columns(FullTable, groups = GROUPS, maxyear = MAXYEAR,
                                    NAME_CONVERSION = NAME_CONVERSION, SCENARIO = 26)
  
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

notif(paste("Loading ", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson ..."))))
FullTable <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
notif(paste("Loading ", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))

STDMEAN <- 0.05
STDSTD <- 0.01

# Random sampling
NSamp <- 2000

#Now the random sample contains the year of planting/
msg <- paste0("Sampling ", NSamp, " random strategies ...")
notif(msg)

simul636 <- matrix(0, NSamp, dim(FullTable)[1])
Uniqunits <- unique(FullTable$units)

handlers(
  list(
    handler_shiny(),
    handler_progress(
      format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
    ),
    handler_txtprogressbar()
  ),
  on_missing = "warning"
)
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
if (tolower(os) == "windows") {
  futureplan <- future::multisession
} else {
  futureplan <- future::multicore
}
plan(futureplan, workers = min(5, round(future::availableCores() / 2)))
with_progress({
  pb <- progressor(steps = NSamp, message = paste("Sampling", NSamp, "strategies ..."))
  simul636 <- foreach(
    aaa = 1:NSamp,
    .combine = rbind,
    .inorder = TRUE,
    .options.future = list(
      chunk.size = round(NSamp / (3 * (future::availableCores() - 1))),
      scheduling = 2,
      seed = TRUE
    )
  ) %dofuture% {
    
    pp <- runif(1)
    RandSamp <- rmultinom(length(Uniqunits), 1, c(pp, 1 - pp))[1, ]
    
    result <- matrix(NA, nrow = 1, ncol = dim(FullTable)[1])
    for (bbb in 1:length(Uniqunits)) {
      result[1, FullTable$units == Uniqunits[bbb]] <- RandSamp[bbb]
    }
    
    if (aaa %% 10 == 0) {pb(amount = 10)}
    return(result)
  }
    # Avoid warning message from progressor function
  pb(amount = 0)
})
if (isFALSE(RUN_BO)) {
  plan(sequential)
}

RREMBO_CONTROL <- list(
  # method to generate low dimensional data in RRembo::designZ ("LHS", "maximin", "unif"). default unif
  designtype = "LHS",
  # if TRUE, use the new mapping from the zonotope, otherwise the original mapping with convex projection. default TRUE
  reverse = FALSE)
RREMBO_HYPER_PARAMETERS <- RRembo_defaults(d = 6,
                                           # per parcel, area + year planting + tree specie
                                           D = 3 * nrow(FullTable),
                                           init = list(n = 1000),
                                           budget = 100,
                                           control = list(
                                             # method to generate low dimensional data in RRembo::designZ ("LHS", "maximin", "unif"). default unif
                                             designtype = "LHS",
                                             # if TRUE, use the new mapping from the zonotope, otherwise the original mapping with convex projection. default TRUE
                                             reverse = FALSE),
                                           limit_log_level = LOG_LEVEL)

# for (aaa in 1:NSamp) {
#   pp <- runif(1)
#   RandSamp <- rmultinom(length(Uniqunits), 1, c(pp, 1 - pp))[1, ]
#   for (bbb in 1:length(Uniqunits)) {
#     simul636[aaa, FullTable$units == Uniqunits[bbb]] <- RandSamp[bbb]
#   }
# }

# Simul636Year is populated with the year of planting
# once simul636Year works it will replace simul636
# (MAXYEAR+1) is the code for no planting
# Otherwise 0 to MAXYEAR is the year of planting if planted.
simul636Year<-simul636
for (aaa in 1:NSamp) {
  
  simul636Year[aaa,simul636[aaa,]==0]<-(MAXYEAR+1)
  probb<-runif(1,0.2,0.6)
  size<-15*runif(1)
  simul636Year[aaa,simul636[aaa,]!=0]<-pmin(rnbinom(sum(simul636[aaa,]),size=size,prob=probb),MAXYEAR)
}
#### Simul636YearType is a similar table but it stops at year MAXYEAR and
#### instead, we pick a tree type (or no planting)
simul636YearType<-list(YEAR=simul636-1,TYPE=apply(simul636,2,as.character))
for (aaa in 1:NSamp) {
  
  simul636YearType[["TYPE"]][aaa,simul636[aaa,]==0]<-"NoPlanting"
  probbType<-runif(1,0.5)
  Planted<-(simul636[aaa,]==1)
  if(sum(Planted)>0){
    simul636YearType[["TYPE"]][aaa,simul636[aaa,]==1]<-sample(c("Conifers","Deciduous"),sum(Planted),replace=T,prob=c(probbType,1-probbType))}
  
  probb<-runif(1,0.2,0.6)
  size<-15*runif(1)
  DRAW<-pmin(rnbinom(sum(simul636[aaa,]),size=size,prob=probb),MAXYEAR)
  simul636YearType$YEAR[aaa,simul636YearType$TYPE[aaa,]!="NoPlanting"]<-DRAW
}

msg <- paste(msg, "done")
notif(msg)

Simul636YearOverrideReactive<-reactiveVal(vector("list",dim(simul636Year)[2]))
Simul636YearTypeOverrideReactive<-reactiveVal(vector("list",dim(simul636Year)[2]))




#hist(simul636YearType,100)

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
    } else {
      # If it is a specie
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


PrecalcCarbonAllExtents<-list()
PrecalcCarbonAllExtentsSD<-list()

PrecalcCarbonAllExtentsType<-list()
PrecalcCarbonAllExtentsSDType<-list()

if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))&
   file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))&
   file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))&
   file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
){
load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
load(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
}else{
  if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))){
    file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
  }
  if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))){
    file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
  }
  if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))){
    file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
  }
  if(file.exists(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))){
    file.remove(normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
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
  
  
  PrecalcCarbonAllExtents[[ext]]<-matrix(0,dim(simul636)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
  PrecalcCarbonAllExtentsSD[[ext]]<-matrix(0,dim(simul636)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])

  PrecalcCarbonAllExtentsType[[ext]]<-matrix(0,dim(simul636)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
  PrecalcCarbonAllExtentsSDType[[ext]]<-matrix(0,dim(simul636)[1],dim(FullTable[FullTable$extent=="NoExtent",])[1])
  
  for(abb in 1:dim(PrecalcCarbonAllExtents[[ext]])[1])
  {
    for(bcc in 1:dim(PrecalcCarbonAllExtents[[ext]])[2])
    {
      
      PrecalcCarbonAllExtents[[ext]][abb,bcc]<-CarbonSelectedYear[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",simul636Year[abb,bcc])]
      PrecalcCarbonAllExtentsSD[[ext]][abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",simul636Year[abb,bcc])]

      
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
  
  save(PrecalcCarbonAllExtents,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtents.RData")))
  save(PrecalcCarbonAllExtentsSD,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSD.RData")))
  save(PrecalcCarbonAllExtentsType,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsType.RData")))
  save(PrecalcCarbonAllExtentsSDType,file=normalizePath(file.path(CalculatedFilesFolder, "PrecalcCarbonAllExtentsSDType.RData")))
  
}


JulesMean <- 0;JulesSD <- 0;SquaresLoad <- 0;Sqconv <- 0;CorrespondenceJules <- 0;seer2km <- 0;jncc100 <- 0;speciesprob40 <- 0;climatecells <- 0;

ui <- fluidPage(useShinyjs(), chooseSliderSkin("Flat",color =rgb(0.25, 0.6, 1.0)),
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
                                            

#verticalLayout(
#  fluidPage(fluidRow(verticalLayout(sliderInput("YearAlt","Planting year",0+STARTYEAR,MAXYEAR+STARTYEAR,0+STARTYEAR,step=1,width = "100%",sep = ""),
 #                                   if (SHOW_TITLES_ON_CLUSTERING_PAGE) {
#                                      column(10,verbatimTextOutput("ZeroText"),column(2,))}
#  ))),
#  fluidPage(fluidRow(                                  
                                            
      #                                        column(5,
     #                                                verticalLayout(
       #                                                if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("FirstMapTxt")}, jqui_resizable(leafletOutput("map2", height = 400, width = "100%")))
      #                                        ),
        #                                      column(5,
         #                                            verticalLayout(if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("SecondMapTxt")}, jqui_resizable(leafletOutput("map3", height = 400, width = "100%")))
       #                                       ),
        #                                      column(2, verticalLayout(verbatimTextOutput("TargetText"),
         #                                                              #  selectInput("chooseGrouping", "Grouping Type:", c("Carbon level"), "Carbon level"),
          #                                                             actionButton("random", "Randomize!"))
           #                                   )
              #                              ),
              #                              fluidRow(
               #                               column(5,
                #                                     verticalLayout(if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("ThirdMapTxt")}, jqui_resizable(leafletOutput("map4", height = 400, width = "100%")))
                 #                             ),
                  #                            column(5,
                   #                                  verticalLayout(if (SHOW_TITLES_ON_CLUSTERING_PAGE) {verbatimTextOutput("FourthMapTxt")}, jqui_resizable(leafletOutput("map5", height = 400, width = "100%")))
                    #                          ),
                     #                         column(2, "")
                      #                      )
                #                            )
                                     #     )
                                    #    ),
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

server <- function(input, output, session,
                   SPECIES_ARG1 = SPECIES,
                   SPECIES_ENGLISH_ARG1 = SPECIES_ENGLISH,
                   N_TARGETS_ARG1 = N_TARGETS,
                   NAME_CONVERSION_ARG1 = NAME_CONVERSION,
                   TARGETS_ARG1 = TARGETS,
                   LOG_LEVEL_ARG = LOG_LEVEL,
                   SCENARIO_ARG = SCENARIO) {
  set.seed(1)
  
  # hideTab(inputId = "tabs", target = "Exploration")
  # hideTab(inputId = "tabs", target = "Preferences")
  SPECIES <- SPECIES_ARG1
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG1
  N_SPECIES <- length(SPECIES)
  N_TARGETS <- N_TARGETS_ARG1
  TARGETS <- TARGETS_ARG1
  NAME_CONVERSION <- NAME_CONVERSION_ARG1
  LOG_LEVEL <- LOG_LEVEL_ARG
  SCENARIO <- SCENARIO_ARG
  
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
  
  # bioSliderVal <- reactive({input$BioSlider})
  # Add BioSliderValSPECIE <- reactive({input$BioSliderSPECIE}) for each specie
  # for (x in SPECIES) {
  #   var_name <- paste0("BioSliderVal", x)
  #   value <- reactive({ input[[paste0("BioSlider", x)]] })
  #   assign(var_name, value, envir = .GlobalEnv)
  # }
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
  Text2 <- reactiveVal("")
  Text3 <- reactiveVal("")
  Text4 <- reactiveVal("")
  PrefTextA <- reactiveVal("")
  PrefTextB <- reactiveVal("")

  # # Add TextN <- reactiveVal("") for each specie
  # for (i in 1:N_SPECIES) {
  #   var_name <- paste0("Text", i + 3)
  #   value <- reactiveVal("")
  #   assign(var_name, value)
  # }
  
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
  output$SecondMapTxt <- renderText({Text2()})
  output$ThirdMapTxt <- renderText({Text3()})
  output$FourthMapTxt <- renderText({Text4()})
  output$PrefTextChoiceA <- renderText({PrefTextA()})
  output$PrefTextChoiceB <- renderText({PrefTextB()})
  output$PrefText<-renderText({"Tell us more about your preferences.  Please look at the two planting strategies below and indicate which you would prefer if these were the only two options by selecting
the 'Choose' button below that option:"})
  
  output$Analysis<-renderPlot({
   
    if(ClusteringDone()){
    if(!is.null(Clustering_Results_Object_Reactive())){
      plot(Clustering_Results_Object_Reactive(),what = "classification")
      
    }else{
      plot.new()
      text(0.5, 0.5, "There is not a sufficient number of target compatible strategies to obtain clusters",cex = 1.5, col = "red", font = 2)}
    }else{
      plot.new()
      text(0.5, 0.5, "Clustering has not been done yet",cex = 1.5, col = "red", font = 2)
    }
  })
  
  
  
  
  
  output$Analysis2<-renderPlot({
   
    if(ClusteringDone()){
      if(!is.null(SetToClusterReactive())&!is.null(Clustering_Results_Object_Reactive())){
       
        pairs(SetToClusterReactive(), col=Clustering_Results_Object_Reactive()$classification)
        
      }else{
        plot.new()
        text(0.5, 0.5, "There is not a sufficient number of target compatible strategies to obtain clusters",cex = 1.5, col = "red", font = 2)}
    }else{
      plot.new()
      text(0.5, 0.5, "Clustering has not been done yet",cex = 1.5, col = "red", font = 2)
      }
  })
  
  
  output$Chart1<-renderPlot({
   
    if(ANALYSISMODE){
    if(ClusteringDone()){
      if(!is.null(SetToClusterReactive())&!is.null(Clustering_Results_Object_Reactive())&length(unique(Clustering_Category_VectorReactive()))==4){
    
        Selected_Cluster_To_Display<-Selected_Cluster_To_Display_Reactive()        
       # Categories<-Clustering_Category_VectorReactive()
        DataClust<-DataCluster_Reactive()[[Selected_Cluster_To_Display]]
        Basis_Loc<-Basis_Clustering_Reactive()[[Selected_Cluster_To_Display]]
        MEANS_Loc<-Mean_Clusters_Reactive()[[Selected_Cluster_To_Display]]
        Limits_Direction_Clusters_Loc<-Limits_Direction_Clusters_Reactive()[[Selected_Cluster_To_Display]]
        min_x_t<-Limits_Direction_Clusters_Loc$min_dir1
        min_y_t<-Limits_Direction_Clusters_Loc$min_dir2
        max_x_t<-Limits_Direction_Clusters_Loc$max_dir1
        max_y_t<-Limits_Direction_Clusters_Loc$max_dir2
        
        #Rotated_Coord<-(DataClust-t(matrix(MEANS_Loc,dim(DataClust)[2],dim(DataClust)[1])))%*%Basis_Loc
        #min_x_t<-min(Rotated_Coord[,1])
        #min_y_t<-min(Rotated_Coord[,2])
        #max_x_t<-max(Rotated_Coord[,1])
        #max_y_t<-max(Rotated_Coord[,2])
        
        #On the left we plot the data in the tsne-output format
        CoordPoly<-data.frame(x=c(min_x_t,min_x_t,max_x_t,max_x_t),y=c(min_y_t,max_y_t,max_y_t,min_y_t))
        CoordPolyOrig<-as.matrix(CoordPoly)%*%t(as.matrix(Basis_Loc))+t(matrix(MEANS_Loc,2,4))
       
       
        pointCoordinatexy<-(c(input$slider_x/100*(max_x_t-min_x_t)+min_x_t,
                             input$slider_y/100*(max_y_t-min_y_t)+min_y_t))%*%t(as.matrix(Basis_Loc))+t(matrix(MEANS_Loc,2,1))
        To_Add_Or_Subtract_To_Scale_x<-max(abs(min(CoordPolyOrig[,1])),abs(max(CoordPolyOrig[,1])))/10
        To_Add_Or_Subtract_To_Scale_y<-max(abs(min(CoordPolyOrig[,2])),abs(max(CoordPolyOrig[,2])))/10
        
        plot(DataClust,col= rgb(0.5, 0.5, 0.5, alpha = 0.5) ,
             xlim=c(min(CoordPolyOrig[,1])-To_Add_Or_Subtract_To_Scale_x,max(CoordPolyOrig[,1])+To_Add_Or_Subtract_To_Scale_x),
             ylim=c(min(CoordPolyOrig[,2])-To_Add_Or_Subtract_To_Scale_y,max(CoordPolyOrig[,2])+To_Add_Or_Subtract_To_Scale_y))
        points(pointCoordinatexy,col="red",pch=20)
        polygon(CoordPolyOrig[,1],CoordPolyOrig[,2])
        
        if(!is.null(dim(DataClust))){
        Dist_Between_Selected_Points_And_Data_Points_After_tsne<-sqrt((DataClust[,1]-pointCoordinatexy[1])^2+(DataClust[,2]-pointCoordinatexy[2])^2)
        Selected_Point_In_Cluster_To_Display<-which.min(Dist_Between_Selected_Points_And_Data_Points_After_tsne)
        Selected_Point_In_Cluster_To_Display_Reactive(Selected_Point_In_Cluster_To_Display)
        points(DataClust[Selected_Point_In_Cluster_To_Display,1],DataClust[Selected_Point_In_Cluster_To_Display,2],col="blue",pch=20)
        }else{Selected_Point_In_Cluster_To_Display_Reactive(1)
          points(DataClust,col="blue",pch=20)}
      }else{
        plot.new()
        text(0.5, 0.5, "There is not a sufficient number of target compatible strategies to obtain clusters",cex = 1.5, col = "red", font = 2)}
    }else{
      plot.new()
      text(0.5, 0.5, "Clustering has not been done yet",cex = 1.5, col = "red", font = 2)
    }  
      
    }else{}
      
      })
  
  
 # output$Chart2<-renderPlot({
  #  plot(1)
  #})
  
  output$DownScalingImage<-renderImage({
    list(src = paste0(DownscalingImagesFolder,"\\9do9xt.gif"), 
         contentType = 'image/gif', width = 800, height = 600)
  }, deleteFile = FALSE)
  
  output$DownScalingImage2<-renderImage({
    list(src = paste0(DownscalingImagesFolder,"\\9do1ky.gif"), 
         contentType = 'image/gif', width = 800, height = 600)
  }, deleteFile = FALSE)
  
  
 
# first_time_open_exploration_reactive <- reactiveVal(TRUE)
  
  # If we click random or open the Exploration tab then we pick 4 different scenarios
 # observeEvent({
#    input$tabs
#  },{
#    if (input$tabs == "Exploration"){
#    
#      if (first_time_open_exploration_reactive() == TRUE) {
#        SelectedSample <- sample(1:dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1],
#                                 min(4, dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1]), replace = FALSE)
#        FourUniqueRowsReactive(SelectedSample)
 #       first_time_open_exploration_reactive(FALSE)
  #    }
 #   }
#  })
  
  
  observeEvent({
    input$random
  },{
    SelectedSample <- sample(1:dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1],
                             min(4, dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1]), replace = FALSE)
    FourUniqueRowsReactive(SelectedSample)
    if(ClusteringDone()){

        PreviousFourUniqueRowsClusteringReactive(FourUniqueRowsClusteringReactive())
     

      UniqueCategories<-unique(Clustering_Category_VectorReactive())
      if(!is.null(UniqueCategories)){
        NewSamplesSelected<-rep(1,4)
        for (ii in 1:4){
          if(length(UniqueCategories)>=ii){NewSamplesSelected[ii]<-sample(1:sum(Clustering_Category_VectorReactive()==ii),1)}
          }
        FourUniqueRowsClusteringReactive(NewSamplesSelected) 
      }
      
      
        
    }
      
  
    
    
  })
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
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVecLoc = ClickedVector(),
                                     simul636Loc = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     # RedSquirrelSelected = RedSquirrelSelected,
                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                     SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                     VisitsSelectedSD = VisitsSelectedSD,
                                     alphaLVL=alphaLVL,
                                     ManualTargets=list(MaxVals$CarbonMax,MaxVals$bioMaxList,max_areaslider,max_visitsslider),
                                     tolvec=tolvecReactive(),
                                     MAXYEAR=MAXYEAR)
      
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
      
      condition <- TRUE
      for (iii in 1:length(SPECIES)) {
        x<-SPECIES[iii]
        var_name <- paste0("SelecTargetBio", x)
        value <- tmp[[var_name]]
        assign(var_name, value)
        
        condition <- condition & (PROBAMAT[,iii+1] >= alphaLVL)
      }
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
  
  # Trigger on changes on the Year slider
  observe({
    if((input$YearSelectClusterExplorationSlider-STARTYEAR)!=YearSelectClusterExplorationReactive()){
      YearSelectClusterExplorationReactive(input$YearSelectClusterExplorationSlider-STARTYEAR)}})
  
  # Trigger on changes on the Year slider for cluster exploration
  observe({
    if((input$YearSelect-STARTYEAR)!=YearSelectReactive()){
      YearSelectReactive(input$YearSelect-STARTYEAR)}})
  
  
  # Trigger if anything changes
  observe({
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SlidersHaveBeenInitialized())==1)) {
     
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
          if (is.null(SelectedFullTableRow())) {
            invalidateLater(5000)
          }
          
          SFTR<-SelectedFullTableRow()
 #         addControlText <- ""
#          for (i in 1:length(SPECIES)) {
#            specie_latin <- SPECIES[i]
#             if (specie_latin == "All") 
#             {specie_english <-paste0("<span style='display: inline-block; width: 140px;'>All Species Richness</span>")}else 
#                  {specie_english <-paste0("<span style='display: inline-block; width: 140px;'>",SPECIES_ENGLISH[i],"</span>")}
#            selectedBiospecie <- SFTR[[specie_latin]]
#            selectedBioSDspecie <- SFTR[[paste0( specie_latin,"SD")]]
#            if(!is.null(selectedBiospecie)){
#              addControlText <- paste0(addControlText, specie_english, ": ", 
#                                       round(selectedBiospecie, 2), "\u00B1",sprintf("%.2f", 2 * selectedBioSDspecie), "<br>")}
#          }
#
#          mapp<-
#            addControl(mapp,html = paste0("<p>
#                                          <span style='display: inline-block; width: 140px;'>
#Carbon</span>: ", round(SFTR$Carbon,2)#round(sum(CarbonMeanCalc), 2)
#                                          , "\u00B1",sprintf("%.2f",2*SFTR$CarbonSD)#round(2*sqrt(sum(CarbonVarCalc)), 2)
#                                          , "<br>",
#                                          # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
#                                          addControlText,
#                                          "<span style='display: inline-block; width: 140px;'>Area Planted</span>: ", round(SFTR$Area, 2), "<br>",
#                                          "<span style='display: inline-block; width: 140px;'>Visitors</span>: ", round(SFTR$Visits, 2), "\u00B1",sprintf("%.2f",2*SFTR$VisitsSD),
#                                          "</p>"), position = "topright",layerId="legend")
      # browser()   
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
  
  # Run clustering if we click on "Exploration" and Clustering has not been done yet
  observe({
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Alternative approaches")&&(!ClusteringDone())) {
 
    #Define local variables in advance

    
    if(dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1]>0)
    {
      Notif<-showNotification("Running Clustering Algorithm..",duration=10,type="message")
      
      if(dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1]<5){
        Clustering_Category_VectorReactive(1:dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1])
        ClusteringDone(TRUE)
        Clustering_Results_Object_Reactive(NULL)
        
        
        
        
      }else{
      
        #cat("starting clustering\n")
        
        NamesOUTPUTS<-names(SubsetMeetTargetsReactiveUnique()$OUTPUTS)
        NamesOUTPUTS<-NamesOUTPUTS[!(sapply(NamesOUTPUTS,function(x) {substr(x,nchar(x)-1,nchar(x))})=="SD")]
        Set_To_Cluster<-SubsetMeetTargetsReactiveUnique()$OUTPUTS[NamesOUTPUTS]
        #save(Set_To_Cluster,file="d:\\Set_To_Clust.RData")
        #cat(Set_To_Cluster$Carbon)

        if(is.null(infpref_reactive())){Weights_To_Use<-rep(1,length(NamesOUTPUTS))}else{
          Weights_To_Use<-sqrt(abs(infpref_reactive()))
          
        }
        Weights_To_Use_MAT<-t(matrix(Weights_To_Use,dim(Set_To_Cluster)[2],dim(Set_To_Cluster)[1]))
        TSNE_RESULTS<-Rtsne::Rtsne(Weights_To_Use_MAT*scale(Set_To_Cluster),perplexity=min(30,(dim(Set_To_Cluster)[1]-1.01)/3))
        #mgcvObjectTsneToData<-vector("list",length(NamesOUTPUTS))
        #mgcvObjectDataTotsne<-vector("list",2)
        #As we cannot directly invert tsne, we fit a GAM to each output individually (we would need to have an inversion model with correlation).
        #for(ii in 1:length(NamesOUTPUTS)){
        #  dat<-data.frame(y=SubsetMeetTargetsReactiveUnique()$OUTPUTS[,NamesOUTPUTS[ii]],tsne1=TSNE_RESULTS$Y[,1],tsne2=TSNE_RESULTS$Y[,2])
        #  mgcvObjectTsneToData[[ii]]<-mgcv::gam(y~s(tsne1,tsne2),data=dat)
        #  
        #}
       # for(ii in 1:2){
      #    StringToEval<-"dat<-data.frame(y=TSNE_RESULTS[,ii],"
      #    for(jj in 1:length(NamesOUTPUTS)){
       #     StringToEval<-paste0(StringToEval,)}
      #    mgcvObjectDataTotsne[[ii]]<-mgcv(y~s(tsne1,tsne2),data=dat)
          
        #}
        
        #cat(TSNE_RESULTS$Y)
      
        MClust_RESULTS<-NULL
        MClust_RESULTS<-mclust::Mclust(TSNE_RESULTS$Y,G=4)#,modelNames=c("VVV"))
        #cat(MClust_RESULTS$classification)
        #cat("\n")
        #cat("\n")
        #cat(MClust_RESULTS$parameters$variance)
        Clustering_Results_Object_Reactive(MClust_RESULTS)
        Clustering_Category_VectorReactive(MClust_RESULTS$classification)
        SetToClusterReactive(scale(Set_To_Cluster))
        
        # In this part, we extract the basis for each cluster found in the 2d projected data with tsne.
        # We then project the tsne transformed data on each cluster.
        # We can then find the min and max value for each direction
        Basis_Clustering<-vector("list",length(unique(Clustering_Category_VectorReactive())))
        Mean_Clusters<-vector("list",length(unique(Clustering_Category_VectorReactive())))
        Projected_TSNE_Data_Clusters<-vector("list",length(unique(Clustering_Category_VectorReactive())))
        Limits_Direction_Clusters<-vector("list",length(unique(Clustering_Category_VectorReactive())))
        DataClustersClassified<-vector("list",length(unique(Clustering_Category_VectorReactive())))
      
        for(ii in 1:length(unique(Clustering_Category_VectorReactive()))){
          #cat(Basis_Clustering[[ii]])

          DataCluster<-TSNE_RESULTS$Y[Clustering_Category_VectorReactive()==ii,]
          DataClustersClassified[[ii]]<-DataCluster
          
          if(!is.null(dim(DataCluster))){
            if(dim(DataCluster)[2]>=2){
          CovarianceDataCluster<-var(DataCluster)
          svd_res<-svd(CovarianceDataCluster)
          Basis_Clustering[[ii]]<-svd_res$u}else{Basis_Clustering[[ii]]<-diag(2)}
          }else{Basis_Clustering[[ii]]<-diag(2)}
          
        #  if(is.null(MClust_RESULTS$parameters$variance$orientation)){Basis_Clustering[[ii]]<-diag(2)}else{
         #   cat(MClust_RESULTS$parameters$variance$orientation)
           
            #it is possible for orientation to have a single matrix if the orientation is the 
            # same for all clusters.This test checks if orientation is a single matrix and not a 3d array.
            # if it's a 3d array then we allocate each matrix to its correct cluster.
        #    if(length(dim(MClust_RESULTS$parameters$variance$orientation))==2){
        #  Basis_Clustering[[ii]]<-MClust_RESULTS$parameters$variance$orientation
         #   }else{Basis_Clustering[[ii]]<-MClust_RESULTS$parameters$variance$orientation[, , ii]}
        #    
         #   }
          #cat(Basis_Clustering[[ii]])
          Mean_Clusters[[ii]]<-MClust_RESULTS$parameters$mean[,ii]
         
          if(!is.null(dim(DataCluster))){
          Projected_TSNE_Data_Clusters[[ii]]<-(DataCluster- t(matrix(Mean_Clusters[[ii]],dim(DataCluster)[2],dim(DataCluster)[1])))%*% Basis_Clustering[[ii]]}else{
            Projected_TSNE_Data_Clusters[[ii]]<-(DataCluster- Mean_Clusters[[ii]])%*% Basis_Clustering[[ii]]
            
          }
          
          
          Limits_Direction_Clusters[[ii]]<-data.frame(min_dir1=min( Projected_TSNE_Data_Clusters[[ii]][,1]),
                                                      min_dir2=min( Projected_TSNE_Data_Clusters[[ii]][,2]),
                                                      max_dir1=max( Projected_TSNE_Data_Clusters[[ii]][,1]),
                                                      max_dir2=max( Projected_TSNE_Data_Clusters[[ii]][,2])
                                                      )
        }
      
        Projected_TSNE_Data_Clusters_Reactive(Projected_TSNE_Data_Clusters)
        Basis_Clustering_Reactive(Basis_Clustering)
        Mean_Clusters_Reactive(Mean_Clusters)
        DataCluster_Reactive(DataClustersClassified)
        Limits_Direction_Clusters_Reactive(Limits_Direction_Clusters)
        
        
        ClusteringDone(TRUE)
        
        
        #cat("ending clustering\n")
        
        
      }
      
      PreviousFourUniqueRowsClusteringReactive(FourUniqueRowsClusteringReactive())
      
      
      UniqueCategories<-unique(Clustering_Category_VectorReactive())
      if(!is.null(UniqueCategories)){
        NewSamplesSelected<-rep(1,4)
        for (ii in 1:4){
          if(length(UniqueCategories)>=ii){NewSamplesSelected[ii]<-sample(1:sum(Clustering_Category_VectorReactive()==ii),1)}
        }
        FourUniqueRowsClusteringReactive(NewSamplesSelected) 
      }
      
      removeNotification(Notif) 
      Selected_Cluster_To_Display_Reactive(1)
    }
    
  }
    
  })
  
  
  # If we are not on Tab Exploration, clustering Resets.
  observe({
    if((input$tabs=="Maps")|(input$tabs=="Preferences")){
 
  ClusteringDone(FALSE)  
  Clustering_Category_VectorReactive(NULL)
  Selected_Cluster_To_Display_Reactive(NULL)
  }
  })
  
### Update the map rendering on Alternative Approaches
  observeEvent({input$YearAlt
                input$random
                input$tabs=="Alternative approaches"
                Selected_Cluster_To_Display_Reactive()},
               {
    
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Alternative approaches") && (ClusteringDone())) {
    #browser()
        YearSelect<-input$YearAlt-STARTYEAR
      PrevYearSelect<-PreviousYearSelectReactive()
      SavedVecYearType<-ClickedVectorYearType()
      PrevSavedVecYearType<-PreviousClickedVectorYearType()
      
      SubsetMeetTargets<-SubsetMeetTargetsReactive()
      PreviousSubsetMeetTargets<-PreviousSubsetMeetTargetsReactive()
      SubsetMeetTargetsUnique<-SubsetMeetTargetsReactiveUnique()
      PreviousSubsetMeetTargetsUnique<-PreviousSubsetMeetTargetsReactiveUnique()
      
      SavedMat<-ClickedMatrixTab2Reactive()
      PreviousSavedMat<-PreviousClickedMatrixTab2Reactive()
      FourUniqueRowsLoc<-FourUniqueRowsReactive()
      PreviousFourUniqueRowsLoc<-PreviousFourUniqueRowsReactive()
      
      FourUniqueRowsClusteringLoc<-FourUniqueRowsClusteringReactive()
      Clustering_Category_VectorLoc<-Clustering_Category_VectorReactive()
      
      
      
      #if(length(FourUniqueRowsLoc)>0){
        if(length(unique(Clustering_Category_VectorLoc))>0){
          
          SelectedRows<-list(YEAR=NULL,TYPE=NULL,OUTPUTS=NULL)
          LISTSeparatedClusters<-vector("list",length(unique(Clustering_Category_VectorLoc)))
        for (ii in 1:length(unique(Clustering_Category_VectorLoc))){
          #cat(ii)
          LISTSeparatedClusters[[ii]]<-list(YEAR=SubsetMeetTargetsUnique$YEAR[Clustering_Category_VectorLoc==ii,],TYPE=SubsetMeetTargetsUnique$TYPE[Clustering_Category_VectorLoc==ii,],
                                            OUTPUTS=SubsetMeetTargetsUnique$OUTPUTS[Clustering_Category_VectorLoc==ii,])
          SelectedRows$YEAR<-rbind(SelectedRows$YEAR,LISTSeparatedClusters[[ii]]$YEAR[FourUniqueRowsClusteringLoc[ii],])
          SelectedRows$TYPE<-rbind(SelectedRows$TYPE,LISTSeparatedClusters[[ii]]$TYPE[FourUniqueRowsClusteringLoc[ii],])
          SelectedRows$OUTPUTS<-rbind(SelectedRows$OUTPUTS,LISTSeparatedClusters[[ii]]$OUTPUTS[FourUniqueRowsClusteringLoc[ii],])
          
        }
        
        #)
        #PrevSelectedRows<-list(YEAR=PreviousSubsetMeetTargetsUnique$YEAR[PreviousFourUniqueRowsLoc,],
        #                       TYPE=PreviousSubsetMeetTargetsUnique$TYPE[PreviousFourUniqueRowsLoc,],
        #                       OUTPUTS=PreviousSubsetMeetTargetsUnique$OUTPUTS[PreviousFourUniqueRowsLoc,])
        
        
        ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units,
                               ColorLighteningFactor(), ColorDarkeningFactor())
        
        FullColVec <- ColObtained$FullColVec
        ClickedCols <- ColObtained$ClickedCols
       # if (length(PreviousFourUniqueRowsLoc) < length(FourUniqueRowsLoc)) { 
        PrevSelectedRows<-SelectedRows
          PrevSelectedRows$YEAR=SelectedRows$YEAR+1# }
        

      #  for (ii in seq(1,min(4,length(FourUniqueRowsLoc)))) {
          for (ii in  1:length(unique(Clustering_Category_VectorLoc))) {          
         
        
          
          TypeA<-(SelectedRows$TYPE[ii,]=="Conifers")&(SelectedRows$YEAR[ii,]<=YearSelect)&(SavedVecYearType<YearSelect)
          TypeB<-(SelectedRows$TYPE[ii,]=="Deciduous")&(SelectedRows$YEAR[ii,]<=YearSelect)&(SavedVecYearType<YearSelect)
          BlockedCells<-(SavedVecYearType>=YearSelect)
          
          
          Consolidated<-1*TypeA+2*TypeB+3*BlockedCells
          
          PrevTypeA<-(PrevSelectedRows$TYPE[ii,]=="Conifers")&(PrevSelectedRows$YEAR[ii,]<=PrevYearSelect)&(PrevSavedVecYearType<PrevYearSelect)
          PrevTypeB<-(PrevSelectedRows$TYPE[ii,]=="Deciduous")&(PrevSelectedRows$YEAR[ii,]<=PrevYearSelect)&(PrevSavedVecYearType<PrevYearSelect)
          PrevBlockedCells<-(PrevSavedVecYearType>=PrevYearSelect)
         
          
          PreviousConsolidated<-1*PrevTypeA+2*PrevTypeB+3*PrevBlockedCells
          
          
          
           if (length(PreviousConsolidated)==0){PreviousConsolidated<-Consolidated+1}
          if ((CreatedBaseMap()==1)&(dim(SavedMat)[2]>0)){
            
            mapp<-leafletProxy(paste0("map",ii+1))
            removeShape(mapp,layerId=paste0("Square",1:length(Consolidated)))
            COLOURS<-rep("transparent",length(Consolidated))
            COLOURS[TypeA]<-"#117733"#"purple"
            COLOURS[TypeB]<-"#44AA99"#green"
            COLOURS[BlockedCells]<-"red"
            mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),color=COLOURS,
                              fillColor=COLOURS,weight=1,fillOpacity = POLYGON_OPACITY)
            
          }
          removeControl(mapp,layerId="legend")

          SFTR<-list()
          SFTR$YEAR<-SelectedRows$YEAR[ii,]
          SFTR$TYPE<-SelectedRows$TYPE[ii,]
          SFTR$OUTPUTS<-SelectedRows$OUTPUTS[ii,]
          #addControlText <- ""
          #for (i in 1:length(SPECIES)) {
          #  specie_latin <- SPECIES[i]
          #  specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
          #  selectedBiospecie <- SFTR$OUTPUTS[[specie_latin]]
          #  selectedBioSDspecie <- SFTR$OUTPUTS[[paste0( specie_latin,"SD")]]
          #  addControlText <- paste0(addControlText, specie_english, ": ", 
          #                           round(selectedBiospecie, 2), "\u00B1", sprintf("%.2f", 2 * selectedBioSDspecie), "<br>")
          #}
          
          #mapp<-
          #  addControl(mapp,html = paste0("<p>Carbon: ", round(SFTR$OUTPUTS$Carbon, 2), "\u00B1", sprintf("%.2f",2*SFTR$OUTPUTS$CarbonSD), "<br>",
          #                                addControlText,
          #                                "Area Planted: ", round(SFTR$OUTPUTS$Area, 2), "<br>",
          #                                "Visitors: ", round(SFTR$OUTPUTS$Visits, 2), "\u00B1", sprintf("%.2f",2*SFTR$OUTPUTS$VisitsSD),
          #                                "</p>"), position = "topright",layerId="legend")
          mapp<-
            addControl(mapp,html =   FormattedControl(SFTR$OUTPUTS$Carbon,SFTR$OUTPUTS$CarbonSD,
                                                      SPECIES,
                                                      SPECIES_ENGLISH,SFTR$OUTPUTS[SPECIES],SFTR$OUTPUTS[paste0(SPECIES,"SD")], 
                                                      SFTR$OUTPUTS$Area, SFTR$OUTPUTS$Visits, SFTR$OUTPUTS$VisitsSD), position = "topright",layerId="legend")
          
          
          
          
          
          
          
          
          ifelse(Selected_Cluster_To_Display_Reactive()==ii,
          mapp<-
            addControl(mapp,html =
                         tags$div(
                           style = "padding: 10px; 
                           background-color: #ff9999; 
                             color: red; 
                           font-weight: bold; 
                           border: 0; 
                           box-shadow:none;
                           font-size: 18px; 
                           border-radius: 5px; 
                           outline: none; 
                           margin: 0; 
                           display: block;",paste0( "Cluster ",ii," Selected")
                         ),
           position = "bottomleft",layerId="legend2",
        className = "fieldset {border: 0;}"),
          removeControl(mapp,layerId="legend2")
          )
          
          
          
        }
        
        #if(length(FourUniqueRowsLoc)<4){
          if( length(unique(Clustering_Category_VectorLoc))<4){
         
          
          #add here text to say that there are no more unique examples.
          #for(ii in seq(length(FourUniqueRowsLoc)+1,4))
            for(ii in seq( length(unique(Clustering_Category_VectorLoc))+1,4))
          {
            
            mapp<-leafletProxy(paste0("map",ii+1))
            removeShape(mapp,layerId=paste0("Square",1:length(Consolidated)))
            mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),
                              color="transparent",fillColor="transparent",fillOpacity = POLYGON_OPACITY)  
            removeControl(mapp,layerId="legend")
            removeControl(mapp,layerId="legend2")
            #mapp<-
             # addControl(mapp,html = "", position = "topright",layerId="legend")
           # mapp<-
            #  addControl(mapp,html = NULL, position = "bottomleft",layerId="legend2")
            
            
          }
        #  PreviousSubsetMeetTargetsReactive(SubsetMeetTargetsReactive())
        #  PreviousFourUniqueRowsReactive(FourUniqueRowsReactive())
        #  PreviousSubsetMeetTargetsReactiveUnique(SubsetMeetTargetsReactiveUnique())
        #  
        #  
        #  UpdatedRows<-ClickedMatrixTab2Reactive()
        #  if(length(FourUniqueRowsLoc)<4){
        #    UpdatedRows[length(FourUniqueRowsLoc):4,]<-PreviousClickedMatrixTab2Reactive()[length(FourUniqueRowsLoc):4,]
        #    
        #  }
        #  PreviousClickedMatrixTab2Reactive(UpdatedRows)
        }
        
      }else{}
      
    }
  })
  
  
  #DONE
  observeEvent({
    input$random
    input$tabs
  }, {
    
    FourUniqueRowsClusteringLoc<-FourUniqueRowsClusteringReactive()
    Clustering_Category_VectorLoc<-Clustering_Category_VectorReactive()
    
    if(length(unique(Clustering_Category_VectorLoc))>0){
      Text1(
        paste0("Strategy Displayed: ",FourUniqueRowsClusteringLoc[1]," out of ",sum(Clustering_Category_VectorLoc==1))
      )}else{
        Text1("No Strategy that meet all the targets")
      }
    if(length(unique(Clustering_Category_VectorLoc))>1){
      Text2(
        paste0("Strategy Displayed: ",FourUniqueRowsClusteringLoc[2]," out of ",sum(Clustering_Category_VectorLoc==2))
      )}else{
        Text2("No Second Strategy that meet all the targets")
      }
    if(length(unique(Clustering_Category_VectorLoc))>2){
      Text3(
        paste0("Strategy Displayed: ",FourUniqueRowsClusteringLoc[3]," out of ",sum(Clustering_Category_VectorLoc==3))
      )}else{
        Text3("No Third Strategy that meet all the targets")
      }
    if(length(unique(Clustering_Category_VectorLoc))>3){
      Text4(
        paste0("Strategy Displayed: ",FourUniqueRowsClusteringLoc[4]," out of ",sum(Clustering_Category_VectorLoc==4))
      )}else{
        Text4("No Fourth Strategy that meet all the targets")
      }
    Text0(paste0("Estimated percentage of strategies that meet all ", N_TARGETS," targets: ",
                 round(dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1] / dim(unique(data.frame(simul636YearType$YEAR,simul636YearType$TYPE)))[1] * 100, 2),"%
displayed : trees planted from 2025 to year:",YearSelectReactive()+STARTYEAR))
  })
  
  #DONE
  # Check if the slider values have been updated after the initialization
  observeEvent(input$SliderMain,{
    SHBICurrent<-SlidersHaveBeenInitialized()
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SHBICurrent)==0)) {
      for (sl in SliderNames){
        SliderNumber<-which(SliderNames==sl)        
        if(input[[sl]]==MaxMinValsReactiveVector()[SliderNumber]){SHBICurrent[SliderNumber]<-1;SlidersHaveBeenInitialized(SHBICurrent)}
      }}
  }
  )
  
  
  
  
  # Check for changes in all the sliders, except on app launch
  # TODO: Maybe add &bayesian_optimization_finished() as a condition, or check that values have been populated
  
  #lapply(SliderNames, function(sl) {observeEvent(input[[sl]],{
  # if (input[[sl]]) {
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
          notif(paste("Task", current_task_id, "cancelled."))
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
        }
        #cat(proc.time()-tt)
        #cat("\n")
      
        if(RUN_BO){
          
          if (isTRUE(current_task_id != get_latest_task_id())) {
            notif(paste("Task", current_task_id, "cancelled."))
            return()
          }
          
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
              bo_results <- bayesian_optimization(...)
              return(bo_results)
            }, seed = NULL) %...>% {
              bo_results <- .
              
              # Check if this result is invalid (i.e. a newer task has started)
              if (isFALSE(bo_results) || isTRUE(current_task_id != get_latest_task_id())) {
                msg <- paste0("task ", current_task_id, " The previous Bayesian optimization has been cancelled.")
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
                                                      FullTable_arg = FullTable)
                
                # Otherwise, a feasible solution is found
                area_sum <- outcome$sum_area
                parcels_activation <- as.numeric(bo_results$strategy_vector[grep("area", names(bo_results$strategy_vector))])
                parcels_activation[parcels_activation != 0] <- 1
                
                last_col <- ncol(bo_results$outcomes_to_maximize)
                number_of_rows <- nrow(bo_results$outcomes_to_maximize)
                col_sums <- c(colSums(bo_results$outcomes_to_maximize %>% dplyr::select("Carbon_Mean_Scenario26_TreeSpecieConifers")),
                              colMeans(bo_results$outcomes_to_maximize %>% dplyr::select(-"Carbon_Mean_Scenario26_TreeSpecieConifers")))
                col_sums_SD <- c(sqrt(colSums((bo_results$outcomes_to_maximize_SD %>% dplyr::select("Carbon_SD_Scenario26_TreeSpecieConifers"))^2)) / number_of_rows,
                                 sqrt(colSums((bo_results$outcomes_to_maximize_SD %>% dplyr::select(-"Carbon_SD_Scenario26_TreeSpecieConifers"))^2)) / number_of_rows)
                
                selectedfulltablerowvalue <- as.data.frame(matrix(c(parcels_activation,
                                                                    # CarbonMean, BioMeans, Area, VisitsMean
                                                                    unlist(outcome[c("sum_carbon", "sum_richness", "sum_biodiversity", "sum_area", "sum_visits")]),
                                                                    # CarbonSD, BioSD, VisitsSD
                                                                    unlist(outcome[c("sum_carbon_sd", "sum_biodiversity_sd", "sum_visits_sd")])),
                                                                  nrow = 1))
                
                colnames(selectedfulltablerowvalue) <- names(SelectedMins[SelecRow, ])
                selectedvectorvalue <- selectedfulltablerowvalue[, 1:length(parcels_activation)]
                
                # SelectedFullTableRow(SelectedMins[SelecRow, ])
                # SelectedVector(SelectedMins[SelecRow, 1:length(SavedVec)])
                SelectedFullTableRow(selectedfulltablerowvalue)
                SelectedVector(selectedvectorvalue)
                
                notif(paste(current_task_id, "Bayesian optimization finished successfully"))
                showNotification(paste(current_task_id, "[INFO] Bayesian optimization finished successfully"))
                notif(paste(current_task_id, "sum_carbon=", outcome$sum_carbon))
                
              }
              bayesian_optimization_finished(TRUE)
              return(TRUE)
            } %...!% {
              error <- .
              if (is.null(error)) {
                msg <- paste0("task ", current_task_id, " future_promise resulted in NULL")
              } else {
                msg <- paste0("task ", current_task_id, " future_promise resulted in the error: ", toString(error))
              }
              
              showNotification(paste("[ERROR]", msg))
              notif(msg, log_level = "error", limit_log_level = limit_log_level)
              bayesian_optimization_finished(TRUE)
              return(FALSE)
            }
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
          
          bayesian_optimization_extendedtask$invoke(seed = 1,
                                                    FullTable_arg = FullTable,
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
  
  
  observeEvent(input$tabs == "Preferences", {

    if(FirstTimeClickOnPreferencesReactive()){
 

    SavedVec <- ClickedVector()
    SavedVecYear <- ClickedVectorYear()
    SavedVecYearType <- ClickedVectorYearType()  
    # TO CHANGE LATER
    YearSelect<-input$YearPref-STARTYEAR
    
    SelectedDropdown <- input$inSelect
    
    updateCheckboxInput(session, "Trigger", label = "", value = TRUE)
    #input$Trigger <- TRUE
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main20", shconv, GreyPolygonWidth = GreyPolygonWidth)
    
    shinyjs::disable("choose1")
    shinyjs::disable("choose2")
    CurrentRound(0)
    listMaps <- list()
    listMaps[[1]] <- calcBaseMap$map
    listMaps[[2]] <- calcBaseMap$map
    
    if (!is.null(SavedVecYearType)) {
      
      AreaSelected <- AreaSelected0()
      CarbonSelected <- CarbonSelected0()
      CarbonSelectedYear<-CarbonSelectedYear0()
      CarbonSelectedYear85<-CarbonSelectedYear850()
      # RedSquirrelSelected <- RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        var_name <- paste0(x, "Selected")
        value <- get(paste0(x, "Selected0"))
        assign(var_name, value())
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      CarbonSelectedSDYear <- CarbonSelectedSDYear0()
      CarbonSelectedSDYear85 <- CarbonSelectedSDYear850()
      
      # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        var_name <- paste0(x, "Selected")
        value <- get(paste0(x, "SelectedSD0"))
        assign(var_name, value())
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      # tmp <- outputmap_calculateMats(input = input,
      #                               SavedVecLoc = SavedVec,
      #                               simul636Loc = simul636,
      #                               AreaSelected = AreaSelected,
      #                               CarbonSelected = CarbonSelected,
      #                               # RedSquirrelSelected = RedSquirrelSelected,
      #                               SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
      #                               VisitsSelected = VisitsSelected,
      #                               CarbonSelectedSD = CarbonSelectedSD,
      #                               # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
      #                               SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
      #                               VisitsSelectedSD = VisitsSelectedSD,
      #                               alphaLVL = alphaLVL,
      #                                input_areaSlider_multiplicative_coefficient = FALSE,
      #                                 tolvec=tolvecReactive())
      
      
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
                                                     #PreviousSavedVecYearTypeLoc=PreviousClickedVectorType(),
                                                     SAMPLELIST=Simul636YearTypeOverrideReactive(),
                                                     MAXYEAR=MAXYEAR,
                                                     CONCATENATION_SIM_MAT=TRUE
      )
      
      
      
      
      #SelectedSimMat2 <- tmp$SelectedSimMat2
      #Icalc <- tmp$Icalc
      #LimitsMat <- tmp$LimitsMat
      #rm(tmp)
      
      SelectedSimMat2<-list()
      SelectedSimMat2[["YEAR"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.YEAR.",1:dim(simul636YearType[["YEAR"]])[2])]
      SelectedSimMat2[["TYPE"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.TYPE.",1:dim(simul636YearType[["TYPE"]])[2])]
      SelectedSimMat2[["OUTPUTS"]]<-tmpYearType$SelectedSimMat2[,-(1:(2*(dim(simul636YearType[["YEAR"]])[2])))]
      names(SelectedSimMat2[["OUTPUTS"]])<-names(tmpYearType$SelectedSimMat2[,-(1:(2*(dim(simul636YearType[["YEAR"]])[2])))])
      
      
      SelectedSimMatGlobal <<- SelectedSimMat2
      SelectedSimMatGlobal_YearType_Concatenation<<-tmpYearType$SelectedSimMat2_YearType_Concatenation
      Icalc <- tmpYearType$Icalc
      
      #  PROBAMAT<-CalcProbaMat(Icalc$IVEC,LimitsMat,Above=AboveTargets)
      # CONDPROB_AtLeast1 <- FALSE
      #  for (i in 1:ncol(PROBAMAT)) {
      #    CONDPROB_AtLeast1 <- CONDPROB_AtLeast1 | (PROBAMAT[, 1] >= alphaLVL)
      #  }
      
      # AtleastOneDat <- unique(SelectedSimMat2[CONDPROB_AtLeast1, ])
      
      #species_data_frame <- do.call("data.frame",
      #                             setNames(lapply(SPECIES, function(x) bquote(SelectedSimMat2[.(x)])),
      #                                     SPECIES))
      #datAll = as.matrix(data.frame(Carbon = SelectedSimMat2$Carbon,
      #                           species_data_frame,
      #                            Area = SelectedSimMat2$Area,
      #                            Visits = SelectedSimMat2$Visits))
      #datAll2 <- datAll[ConvertSample, ]
      
      #DatBinaryCode <- ""
      #for (i in 1:(ncol(PROBAMAT) - 1)) {
      #  DatBinaryCode <- paste0(DatBinaryCode, 1 * (PROBAMAT[, 1] >= alphaLVL))
      #}
      #DatBinaryCode <- paste0(DatBinaryCode, Visits = 1 * (PROBAMAT[, ncol(PROBAMAT)] >= alphaLVL))
      
      
      #DatBinaryCode0(DatBinaryCode)
      #VecNbMet <- rep(0, length(CONDPROB_AtLeast1))
      #  for (i in 1:ncol(PROBAMAT)) {
      #   indices_list <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = i)
      #    indices <- FALSE
      #   for (j in 1:length(indices_list)) {
      #    indices <- indices | indices_list[[j]]
      #  }
      # VecNbMet[indices] <- i
      #}
      
      #  VecNbMet0(VecNbMet)
      
      CONDITION_SEL<-ifelse(apply((Icalc$IVEC*t(matrix(2*((AboveTargets-0.5)),dim(Icalc$IVEC)[2],dim(Icalc$IVEC)[1])))<=ILevel,1,prod),TRUE,FALSE)
      
      
      SubsetMeetTargets <-list()
      SubsetMeetTargets[["YEAR"]]<- SelectedSimMat2$YEAR[CONDITION_SEL,]
      SubsetMeetTargets[["TYPE"]]<- SelectedSimMat2$TYPE[CONDITION_SEL,]
      SubsetMeetTargets[["OUTPUTS"]]<- SelectedSimMat2$OUTPUTS[CONDITION_SEL,]
      
      DF<-data.frame(SubsetMeetTargets$YEAR,SubsetMeetTargets$TYPE)
      uniqueRows<-which(!duplicated(DF))
      
      SubsetMeetTargetsUnique<-(list(YEAR=SubsetMeetTargets$YEAR[uniqueRows,],TYPE=SubsetMeetTargets$TYPE[uniqueRows,],
                                     OUTPUTS=SubsetMeetTargets$OUTPUTS[uniqueRows,]))
      
      SubsetMeetTargetsReactive(SubsetMeetTargets)
      SubsetMeetTargetsReactiveUnique(SubsetMeetTargetsUnique)
      # if (dim(SubsetMeetTargetsUnique$YEAR)[1] >= 250)
      #{
      
      prior_list_temp <- list()
      # Carbon prior
      # mean = 1 / half(midpoint)
      # 2 * sd = 1 / half(midpoint)
      if (isFALSE("Carbon" %in% colnames(SelectedSimMat2$OUTPUTS))) {
        stop("Defining a prior over an outcome that doesn't exist")
      }
      prior_list_temp$Carbon <- gamma_prior(2 / max(SelectedSimMat2$OUTPUTS$Carbon),
                                            1 / max(SelectedSimMat2$OUTPUTS$Carbon))
      
      # Species priors, similarly-derived values
      for (i in 1:N_SPECIES) {
        specie <- SPECIES[i]
        if (isFALSE(specie %in% colnames(SelectedSimMat2$OUTPUTS))) {
          stop("Defining a prior over an outcome that doesn't exist")
        }
        add_to_list <- setNames(list(Normal(2 / max(SelectedSimMat2$OUTPUTS[[specie]]),
                                            1 / max(SelectedSimMat2$OUTPUTS[[specie]]))),
                                specie)
        prior_list_temp <- append(prior_list_temp, add_to_list)
      }
      
      # Area prior
      if (isFALSE("Area" %in% colnames(SelectedSimMat2$OUTPUTS))) {
        stop("Defining a prior over an outcome that doesn't exist")
      }
      prior_list_temp$Area <- gamma_prior(- 2 / max(SelectedSimMat2$OUTPUTS$Area),
                                          1 / max(SelectedSimMat2$OUTPUTS$Area))
      
      # Visits prior
      if (isFALSE("Visits" %in% colnames(SelectedSimMat2$OUTPUTS))) {
        stop("Defining a prior over an outcome that doesn't exist")
      }
      prior_list_temp$Visits <- Normal(2 / max(SelectedSimMat2$OUTPUTS$Visits),
                                       1 / max(SelectedSimMat2$OUTPUTS$Visits))
      
      # Re-order the list in accordance to TARGETS vector
      prior_list <- list()
      for (target in TARGETS) {
        prior_list[[target]] <- prior_list_temp[[target]]
      }
      
      # pref_reactive(prefObject(data = datAll2,
      #                          priors = prior_list))
      
      #      UniqueBinCodes <- unique(DatBinaryCode)
      
      #first, we look at the pre-recorded set
      if(dim(FIXED_STRATEGIES_LIST$YEAR)[1]!=0){
        LinesToCompare<-list(YEAR=FIXED_STRATEGIES_LIST$YEAR[1:2,],
                             TYPE=FIXED_STRATEGIES_LIST$TYPE[1:2,],
                             OUTPUTS=FIXED_STRATEGIES_LIST$OUTPUTS[1:2,])
      }
      #THEN, we look in the target compatible samples because I want to 
      else{if(dim(SubsetMeetTargetsUnique$YEAR)[1]>=2){
        
        RandomSubsetIndices<-sample(1:dim(SubsetMeetTargetsUnique$YEAR)[1],2,replace=F)
        #browser()
        LinesToCompare<-list(YEAR=SubsetMeetTargetsUnique$YEAR[RandomSubsetIndices,],
                             TYPE=SubsetMeetTargetsUnique$TYPE[RandomSubsetIndices,],
                             OUTPUTS=SubsetMeetTargetsUnique$OUTPUTS[RandomSubsetIndices,])
      }else{
        ##browser()
        RandomSubsetIndices<-sample(1:dim(SelectedSimMatGlobal$YEAR)[1],2,replace=F)
        LinesToCompare<-list(YEAR=SelectedSimMatGlobal$YEAR[RandomSubsetIndices,],
                             TYPE=SelectedSimMatGlobal$TYPE[RandomSubsetIndices,],
                             OUTPUTS=SelectedSimMatGlobal$OUTPUTS[RandomSubsetIndices,])
      }
        
      }
      #  NbRoundsMax(MaxRounds)
      
      #        LinesToCompare <- matrix(0, 0, 2)
      #LinesToCompare[1, ] <- sample(1:dim(datAll2$YEAR)[1], 2, replace = F)
      #CurrentRound(1)
      
      LinesToCompareReactive(LinesToCompare)
      SelectedLine <- list()
      pref_reactive(prefObject(data = LinesToCompare$OUTPUTS[TARGETS],
                               priors = prior_list))
      # SelectedLine[[1]] <- SelectedSimMat2[ConvertSample[LinesToCompare[1, 1]], ]
      # SelectedLine[[2]] <- SelectedSimMat2[ConvertSample[LinesToCompare[1, 2]], ]
      
      # Pick 2 random strategies that meet all targets and update pref_reactive
      #two_strategies_that_meet_all_targets <- pick_two_strategies_that_meet_targets_update_pref_reactive(VecNbMet0 = VecNbMet0,
      #                                                                                                  SelectedSimMat2 = SelectedSimMat2,
      #                                                                                                 pref_reactive = pref_reactive,
      #                                                                                                N_TARGETS_ARG3 = N_TARGETS,
      #                                                                                               TARGETS_ARG2 = TARGETS,
      #                                                                                              prior_list = prior_list,
      #              #                                                                                             limit_log_level = LOG_LEVEL)
      #        SelectedLine[[1]] <- list(YEAR=datAll2$YEAR[ LinesToCompare[1, 1],],
      #                                  TYPE=datAll2$TYPE[ LinesToCompare[1, 1],],
      #                                  OUTPUTS=datAll2$OUTPUTS[ LinesToCompare[1, 1],])#SelectedSimMat2[two_strategies_that_meet_all_targets[1], ]
      #        SelectedLine[[2]] <- list(YEAR=datAll2$YEAR[ LinesToCompare[1,2],],
      #                                  TYPE=datAll2$TYPE[ LinesToCompare[1, 2],],
      #                                  OUTPUTS=datAll2$OUTPUTS[ LinesToCompare[1, 2],])#SelectedSimMat2[two_strategies_that_meet_all_targets[2], ]
      #                                                                                             global_log_level = LOG_LEVEL)
      SelectedLine[[1]] <- list(YEAR=LinesToCompare$YEAR[1,],
                                TYPE=LinesToCompare$TYPE[1,],
                                OUTPUTS=LinesToCompare$OUTPUTS[1,])#SelectedSimMat2[two_strategies_that_meet_all_targets[1], ]
      SelectedLine[[2]] <- list(YEAR=LinesToCompare$YEAR[2,],
                                TYPE=LinesToCompare$TYPE[2,],
                                OUTPUTS=LinesToCompare$OUTPUTS[2,])#SelectedSimMat2[two_strategies_that_meet_all_targets[2], ]
      
      for (aai in 1:2) {
        
        TypeA<-(SelectedLine[[aai]]$TYPE=="Conifers")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
        TypeB<-(SelectedLine[[aai]]$TYPE=="Deciduous")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
        BlockedCells<-(SavedVecYearType>=YearSelect)
        mapp<-listMaps[[aai]]
        removeShape(mapp,layerId=paste0("Square",1:length(TypeA)))
        COLOURS<-rep("transparent",length(TypeA))
        
        COLOURS[TypeA]<-"#117733"#"purple"
        COLOURS[TypeB]<-"#44AA99"#green"
        COLOURS[BlockedCells]<-"red"
        mapp<-addPolygons(mapp,data=FullTable$geometry,
                          layerId=paste0("Square",1:length(TypeA)),color=COLOURS,fillColor=COLOURS,weight=1,fillOpacity = POLYGON_OPACITY)
        removeControl(mapp,layerId="legend")
        #### TO CHANGE PREF ELICITATION           
        
        
        # addControlText <- ""
        #  for (i in 1:length(SPECIES)) {
        ##    specie_latin <- SPECIES[i]
        #    specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
        #    selectedBiospecie <- SelectedLine[[aai]]$OUTPUTS[[specie_latin]]
        #    selectedBioSDspecie <- SelectedLine[[aai]]$OUTPUTS[[paste0( specie_latin,"SD")]]
        #    addControlText <- paste0(addControlText, specie_english, ": ", 
        #                             round(selectedBiospecie,2), "\u00B1", sprintf("%.2f", 2 * selectedBioSDspecie), "\n")
        #  }
        
        
        #mapp<-
        #  addControl(mapp,html = paste0("<p>Carbon: ", round(SelectedLine[[aai]]$OUTPUTS$Carbon, 2), "\u00B1", 
        #                                round(2*SelectedLine[[aai]]$OUTPUTS$CarbonSD, 2), "<br>",
        #                                addControlText,
        #                                "Area Planted: ", round(SelectedLine[[aai]]$OUTPUTS$Area, 4), "<br>",
        #                                "Visitors: ", round(SelectedLine[[aai]]$OUTPUTS$Visits, 2), 
        #                                "\u00B1", round(2*SelectedLine[[aai]]$OUTPUTS$VisitsSD, 2),
        #                                "</p>"), position = "topright",layerId="legend")
        if(aai==1){
          #### 
          
          PrefTextA(
            FormattedText(SelectedLine[[aai]]$OUTPUTS$Carbon,
                          SelectedLine[[aai]]$OUTPUTS$CarbonSD,
                          SPECIES,SPECIES_ENGLISH,
                          SelectedLine[[aai]]$OUTPUTS[SPECIES],
                          SelectedLine[[aai]]$OUTPUTS[paste0( SPECIES,"SD")],
                          SelectedLine[[aai]]$OUTPUTS$Area,
                          SelectedLine[[aai]]$OUTPUTS$Visits,
                          SelectedLine[[aai]]$OUTPUTS$VisitsSD
            )
            
          )}else{
            PrefTextB( FormattedText(SelectedLine[[aai]]$OUTPUTS$Carbon,
                                     SelectedLine[[aai]]$OUTPUTS$CarbonSD,
                                     SPECIES,SPECIES_ENGLISH,
                                     SelectedLine[[aai]]$OUTPUTS[SPECIES],
                                     SelectedLine[[aai]]$OUTPUTS[paste0( SPECIES,"SD")],
                                     SelectedLine[[aai]]$OUTPUTS$Area,
                                     SelectedLine[[aai]]$OUTPUTS$Visits,
                                     SelectedLine[[aai]]$OUTPUTS$VisitsSD
            ))
            
          }
        
        listMaps[[aai]]<-mapp
        
        #    SwitchedOnCells <- SelectedLine[[aai]][1:length(SavedVec)]
        #    SelectedTreeCarbon <- SelectedLine[[aai]]$Carbon
        #   for (x in SPECIES) {
        #    var_name <- paste0("SelectedBio", x)
        #   value <- SelectedLine[[aai]][[x]]
        #   assign(var_name, value)
        # }
        #  SelectedArea <- SelectedLine[[aai]]$Area
        #  SelectedVisits <- SelectedLine[[aai]]$Visits
        
        #     SelectedTreeCarbonSD <- SelectedLine[[aai]]$CarbonSD
        #    for (x in SPECIES) {
        #     var_name <- paste0("SelectedBioSD", x)
        #    value <- SelectedLine[[aai]][[paste0(x, "SD")]]
        #   assign(var_name, value)
        #}
        #  SelectedVisitsSD <- SelectedLine[[aai]]$VisitsSD
        
        # SELL <- (FullTable$extent == SelectedDropdown)
        #  if (!is.null(SELL)) {
        #    
        #    SELGEO <- FullTable$geometry[SELL]
        #    SELGEOFull <- FullTable[SELL, ]
        #    SELGEOFull$layerId <- paste0("Square", 1:dim(SELGEOFull)[1])
        #    
        #    
        #    
        #    ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units[SELL],
        #                           ColorLighteningFactor(), ColorDarkeningFactor())
        #    
        #    FullColVec <- ColObtained$FullColVec
        #    ClickedCols <- ColObtained$ClickedCols
        #    SELGEOFull$color <- ColObtained$FullColVec
        #    SELGEOFull$color[SavedVec == 1] <- ColObtained$ClickedCols[SavedVec == 1]
        #    
        #    
        #    SELGEOSavedVec <- SELGEOFull[, c("geometry", "layerId")]
        #    SELGEOSwitched <- SELGEOFull[, c("geometry", "layerId")]
        #    
        #    SELGEORemaining <- SELGEOFull[(SavedVec == 1) | (SwitchedOnCells == 1), c("geometry", "layerId", "color")]
        #    
        #    
        #    SELGEOSavedVec <- SELGEOSavedVec[SavedVec == 1, ]
        #    SELGEOSwitched <- SELGEOSwitched[(SwitchedOnCells == 1) & (SavedVec != 1), ]
        #    
        #    if (dim(SELGEORemaining)[1] > 0) {
        #      listMaps[[aai]] <- addPolygons(listMaps[[aai]], data = SELGEORemaining, color = SELGEORemaining$color, layerId = ~SELGEORemaining$layerId, weight = UnitPolygonColours)
        #    }
        #    
        #    
        #  }
        
        # addControlText <- ""
        #  for (i in 1:length(SPECIES)) {
        #    specie_latin <- get_ugly_specie(SPECIES[i], NAME_CONVERSION)
        #    specie_english <- if (specie_latin == "All") "All Species Richness" else get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
        #    selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
        #    selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
        #    addControlText <- paste0(addControlText, get_pretty_english_specie(specie_english, NAME_CONVERSION), ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        #  }
        
        #  listMaps[[aai]] <- listMaps[[aai]] %>%
        #    addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2*SelectedTreeCarbonSD, 2), "<br>",
        #                             # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
        #                             addControlText,
        #                            "Area Planted: ", round(SelectedArea, 2), "<br>",
        #                             "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2*SelectedVisitsSD, 2),
        #                             "</p>"), position = "topright")
        #  
      }
      
      shinyjs::enable("choose1")
      shinyjs::enable("choose2")
      FirstTimeClickOnPreferencesReactive(FALSE)
      
      #} #else {
      #listMaps[[1]] <- listMaps[[1]] %>%
      #  addControl(html = paste0("<p> Elicitation Not Possible as there are not enough samples that meet some of the targets
        #                                      </p>"), position = "topright")
        #listMaps[[2]] <- listMaps[[2]] %>%
        #  addControl(html = paste0("<p> Elicitation Not Possible as there are not enough samples that meet some of the targets
        #                                      </p>"), position = "topright")
        #shinyjs::disable("choose1")
       # shinyjs::disable("choose2")
      #}
    
    }
    
    listMaps <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, listMaps = listMaps)
    
    output$ClusterPage <- renderLeaflet({listMaps[[1]]})
    output$ClusterPage2 <- renderLeaflet({listMaps[[2]]})
    
    
  }}, ignoreInit = TRUE)

  observeEvent(input$choose1, {
 
    observe_event_function_YearType(choose = 1, # 1 for input$choose1, 2 for input$choose2
                           input = input,
                           output = output,
                           session = session,
                           infpref_reactive = infpref_reactive,
                           ConvertSample = ConvertSample,
                           LinesToCompareReactive = LinesToCompareReactive,
                           ClickedVector = ClickedVector,
                           NbRoundsMax = NbRoundsMax,
                           CurrentRound = CurrentRound,
                           FullTable = FullTable,
                           FullTableNotAvail = FullTableNotAvail,
                           VecNbMet0 = VecNbMet0,
                           shconv = shconv,
                           SelectedSimMatGlobal = SelectedSimMatGlobal,
                           pref_reactive = pref_reactive,
                           ColourScheme = ColourScheme(),
                           ColorLighteningFactor = ColorLighteningFactor(),
                           ColorDarkeningFactor = ColorDarkeningFactor(),
                           SPECIES_ARG3 = SPECIES,
                           SPECIES_ENGLISH_ARG3 = SPECIES_ENGLISH,
                           N_TARGETS_ARG2 = N_TARGETS,
                           TARGETS_ARG1 = TARGETS,
                           GreyPolygonWidth = GreyPolygonWidth,
                           UnitPolygonColours = UnitPolygonColours,
                           ClickedVectorYear=ClickedVectorYear,
                           ClickedVectorYearType=ClickedVectorYearType)
  })

  observeEvent(input$choose2, {
    observe_event_function_YearType(choose = 2, # 1 for input$choose1, 2 for input$choose2
                           input = input,
                           output = output,
                           session = session,
                           infpref_reactive = infpref_reactive,
                           ConvertSample = ConvertSample,
                           LinesToCompareReactive = LinesToCompareReactive,
                           ClickedVector = ClickedVector,
                           NbRoundsMax = NbRoundsMax,
                           CurrentRound = CurrentRound,
                           FullTable = FullTable,
                           FullTableNotAvail = FullTableNotAvail,
                           VecNbMet0 = VecNbMet0,
                           shconv = shconv,
                           SelectedSimMatGlobal = SelectedSimMatGlobal,
                           pref_reactive = pref_reactive,
                           ColourScheme = ColourScheme(),
                           ColorLighteningFactor = ColorLighteningFactor(),
                           ColorDarkeningFactor = ColorDarkeningFactor(),
                           SPECIES_ARG3 = SPECIES,
                           SPECIES_ENGLISH_ARG3 = SPECIES_ENGLISH,
                           N_TARGETS_ARG2 = N_TARGETS,
                           TARGETS_ARG1 = TARGETS,
                           GreyPolygonWidth = GreyPolygonWidth,
                           UnitPolygonColours = UnitPolygonColours,
                           ClickedVectorYear=ClickedVectorYear,
                           ClickedVectorYearType=ClickedVectorYearType)
  })
  
### the year slider on the preference tab is changed, then change the land parcels displayed.  
  observeEvent(input$YearPref,
               {# if the first two maps have been displayed on the pref tabs
                 if(!FirstTimeClickOnPreferencesReactive()){
                 SavedVecYearType <- ClickedVectorYearType()  
                YearSelect<-input$YearPref-STARTYEAR   
                listMaps <- list()
                
                SelectedDropdown <- input$inSelect
                
                listMaps[[1]]<-leafletProxy("ClusterPage")
                listMaps[[2]]<-leafletProxy("ClusterPage2")
                 LinesToCompare<-LinesToCompareReactive()
                 SelectedLine<-list()
                # browser()
                 
                 CurrentLengthLinesToCompare<-dim(LinesToCompare$YEAR)[1]
                 
                 TwoLinesToCompareTemp<-list(YEAR=LinesToCompare$YEAR[,paste0("SelectedSimMat.YEAR.",1:length(SavedVecYearType))],
                                      TYPE=LinesToCompare$TYPE[,paste0("SelectedSimMat.TYPE.",1:length(SavedVecYearType))],
                                      OUTPUTS=LinesToCompare$OUTPUTS)
                 
                 SelectedLine[[1]] <- list(YEAR=TwoLinesToCompareTemp$YEAR[CurrentLengthLinesToCompare-1,],
                                           TYPE=TwoLinesToCompareTemp$TYPE[CurrentLengthLinesToCompare-1,],
                                           OUTPUTS=TwoLinesToCompareTemp$OUTPUTS[CurrentLengthLinesToCompare-1,])
                 SelectedLine[[2]] <- list(YEAR=TwoLinesToCompareTemp$YEAR[CurrentLengthLinesToCompare,],
                                           TYPE=TwoLinesToCompareTemp$TYPE[CurrentLengthLinesToCompare,],
                                           OUTPUTS=TwoLinesToCompareTemp$OUTPUTS[CurrentLengthLinesToCompare,])
                 
                 

                 
                 for (aai in 1:2) {
                   
                   TypeA<-(SelectedLine[[aai]]$TYPE=="Conifers")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
                   TypeB<-(SelectedLine[[aai]]$TYPE=="Deciduous")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
                   BlockedCells<-(SavedVecYearType>=YearSelect)
                   mapp<-listMaps[[aai]]
                   removeShape(mapp,layerId=paste0("Square",1:length(TypeA)))
                   COLOURS<-rep("transparent",length(TypeA))
                   
                   COLOURS[TypeA]<-"#117733"#"purple"
                   COLOURS[TypeB]<-"#44AA99"#green"
                   COLOURS[BlockedCells]<-"red"
                   mapp<-addPolygons(mapp,data=FullTable$geometry,
                                     layerId=paste0("Square",1:length(TypeA)),color=COLOURS,fillColor=COLOURS,weight=1
                                     ,fillOpacity = POLYGON_OPACITY)
                   
                 }
                 
               
                 listMaps <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, listMaps = listMaps)
                 }
                 
})
                 
              
  
  ### the year slider on the Alternative approaches tab is changed, then change the land parcels displayed.  
  #observeEvent(input$YearAlt,
  #             {# if the first two maps have been displayed on the pref tabs
  #               if(!FirstTimeClickOnPreferencesReactive()){
  #                 SavedVecYearType <- ClickedVectorYearType()  
  #                 YearSelect<-input$YearPref-STARTYEAR   
  #                 listMaps <- list()
  #                 
  #                 SelectedDropdown <- input$inSelect
  #                 
  #                 listMaps[[1]]<-leafletProxy("ClusterPage")
  #                 listMaps[[2]]<-leafletProxy("ClusterPage2")
  #                 LinesToCompare<-LinesToCompareReactive()
  #                 SelectedLine<-list()
  #                 # browser()
  #                 
  #               #  CurrentLengthLinesToCompare<-dim(LinesToCompare$YEAR)[1]
  #              #   
  #              #   TwoLinesToCompareTemp<-list(YEAR=LinesToCompare$YEAR[,paste0("SelectedSimMat.YEAR.",1:length(SavedVecYearType))],
  #              #                               TYPE=LinesToCompare$TYPE[,paste0("SelectedSimMat.TYPE.",1:length(SavedVecYearType))],
  #              #                               OUTPUTS=LinesToCompare$OUTPUTS)
  #              #   
  #              #   SelectedLine[[1]] <- list(YEAR=TwoLinesToCompareTemp$YEAR[CurrentLengthLinesToCompare-1,],
  #              #                             TYPE=TwoLinesToCompareTemp$TYPE[CurrentLengthLinesToCompare-1,],
  #              #                             OUTPUTS=TwoLinesToCompareTemp$OUTPUTS[CurrentLengthLinesToCompare-1,])
  #              #   SelectedLine[[2]] <- list(YEAR=TwoLinesToCompareTemp$YEAR[CurrentLengthLinesToCompare,],
  #              #                             TYPE=TwoLinesToCompareTemp$TYPE[CurrentLengthLinesToCompare,],
                #                             OUTPUTS=TwoLinesToCompareTemp$OUTPUTS[CurrentLengthLinesToCompare,])
                #   
                #   
                #   
                #   
                ##   for (aai in 1:2) {
                ##     
                ##     TypeA<-(SelectedLine[[aai]]$TYPE=="Conifers")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
                #     TypeB<-(SelectedLine[[aai]]$TYPE=="Deciduous")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
                #     BlockedCells<-(SavedVecYearType>=YearSelect)
                #     mapp<-listMaps[[aai]]
                #     removeShape(mapp,layerId=paste0("Square",1:length(TypeA)))
                #     COLOURS<-rep("transparent",length(TypeA))
                #     
                #     COLOURS[TypeA]<-"purple"
                #     COLOURS[TypeB]<-"green"
                #     COLOURS[BlockedCells]<-"red"
                #     mapp<-addPolygons(mapp,data=FullTable$geometry,
                #                       layerId=paste0("Square",1:length(TypeA)),color=COLOURS,fillColor=COLOURS,weight=1)
                #     
                #   }
                #   
                #   #

  #               }
                 
  #             })
  
  
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
                SAMPLELIST[[iii]]<-rep(29,dim(simul636Year)[1])}else{
                  SAMPLELIST[[ iii]]<- sample((SavedVecYear[ iii]+1):(MAXYEAR+1),dim(simul636Year)[1], replace=T)
                  
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
  
  # This part updates the map on the cluster exploration page.  
  observe({
    

      if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
        Selected_Cluster_To_Display_Loc<-Selected_Cluster_To_Display_Reactive()
        
        Selected_Point_In_Cluster_To_Display_Loc<-Selected_Point_In_Cluster_To_Display_Reactive()
        
        YearSelect<-YearSelectClusterExplorationReactive()
        Clustering_Category_VectorLoc<-Clustering_Category_VectorReactive()
        SavedVecYearType<-ClickedVectorYearType()
        SubsetMeetTargetsUnique<-SubsetMeetTargetsReactiveUnique()
 
        
        
        if(length(unique(Clustering_Category_VectorLoc))>0){
          Rows_of_Selected_Cluster_To_Display_in_SubsetMeetTargetsUnique<-which(Clustering_Category_VectorLoc==Selected_Cluster_To_Display_Loc)
          Row_in_SubsetMeetTargetsUnique_To_Display<-Rows_of_Selected_Cluster_To_Display_in_SubsetMeetTargetsUnique[Selected_Point_In_Cluster_To_Display_Loc]
          
          SelectedRowToDisplay<-list(YEAR=SubsetMeetTargetsUnique$YEAR[Row_in_SubsetMeetTargetsUnique_To_Display,],
                                     TYPE=SubsetMeetTargetsUnique$TYPE[Row_in_SubsetMeetTargetsUnique_To_Display,],
                                     OUTPUTS=SubsetMeetTargetsUnique$OUTPUTS[Row_in_SubsetMeetTargetsUnique_To_Display,])
        
            TypeA<-(SelectedRowToDisplay$TYPE=="Conifers")&(SelectedRowToDisplay$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
            TypeB<-(SelectedRowToDisplay$TYPE=="Deciduous")&(SelectedRowToDisplay$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
            BlockedCells<-(SavedVecYearType>=YearSelect)
              mapp<-leafletProxy(paste0("map6"))
              removeShape(mapp,layerId=paste0("Square",1:length(TypeA)))
              COLOURS<-rep("transparent",length(TypeA))
              COLOURS[TypeA]<-"#117733"#"purple"
              COLOURS[TypeB]<-"#44AA99"#green"
              COLOURS[BlockedCells]<-"red"
              mapp<-addPolygons(mapp,data=FullTable$geometry,
                                layerId=paste0("Square",1:length(TypeA)),color=COLOURS,fillColor=COLOURS,weight=1,fillOpacity = POLYGON_OPACITY)
            removeControl(mapp,layerId="legend")
          
            
           # addControlText <- ""
          #  for (i in 1:length(SPECIES)) {
          #    specie_latin <- SPECIES[i]
          #    specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
          #    selectedBiospecie <- SelectedRowToDisplay$OUTPUTS[[specie_latin]]
          #    selectedBioSDspecie <- SelectedRowToDisplay$OUTPUTS[[paste0( specie_latin,"SD")]]
          #    addControlText <- paste0(addControlText, specie_english, ": ", 
          #                            round(selectedBiospecie, 2), "\u00B1",sprintf("%.2f", 2 * selectedBioSDspecie), "<br>")
          #  }
            
            if(length(unique(Clustering_Category_VectorReactive()))>=4){
            #mapp<-
            #  addControl(mapp,html = paste0("<p>Carbon: ", round(SelectedRowToDisplay$OUTPUTS$Carbon, 2), "\u00B1", 
            #                                sprintf("%.2f",2*SelectedRowToDisplay$OUTPUTS$CarbonSD), "<br>",
            #                                addControlText,
            #                                "Area Planted: ", round(SelectedRowToDisplay$OUTPUTS$Area, 4), "<br>",
            #                                "Visitors: ", round(SelectedRowToDisplay$OUTPUTS$Visits, 2), 
            #                                "\u00B1", sprintf("%.2f",2*SelectedRowToDisplay$OUTPUTS$VisitsSD),
            #                                "</p>"), position = "topright",layerId="legend")
            
              mapp<-
                addControl(mapp,html =   FormattedControl(SelectedRowToDisplay$OUTPUTS$Carbon,
                                                          SelectedRowToDisplay$OUTPUTS$CarbonSD,
                                                          SPECIES,
                                                          SPECIES_ENGLISH,
                                                          SelectedRowToDisplay$OUTPUTS[SPECIES],
                                                          SelectedRowToDisplay$OUTPUTS[paste0(SPECIES,"SD")], 
                                                          SelectedRowToDisplay$OUTPUTS$Area, 
                                                          SelectedRowToDisplay$OUTPUTS$Visits, SelectedRowToDisplay$OUTPUTS$VisitsSD), position = "topright",layerId="legend")
              
              
              
              
            
            }
            
          }
          
        }else{}
        
      #}
      
      
      
      
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
        
        # RedSquirrelSelected <- RedSquirrelSelected0()
        SpeciesListSelected <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "Selected0"))
          SpeciesListSelected[x] <- list(value())
        }
        VisitsSelected <- VisitsSelected0()
        
        CarbonSelectedSD <- CarbonSelectedSD0()
        CarbonSelectedSDYear <- CarbonSelectedSDYear0()
        
        # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
        SpeciesListSelectedSD <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "SelectedSD0"))
          var_name <- paste0(x, "SD")
          SpeciesListSelectedSD[var_name] <- list(value())
        }
        VisitsSelectedSD <- VisitsSelectedSD0()
        
        
        TwoRows<-matrix(1,nrow=2,ncol=dim(FullTable)[1])
        tmp <- outputmap_calculateMats(input = input,
                                       SavedVecLoc = TwoRows[1,],
                                       simul636Loc = TwoRows,
                                       AreaSelected = AreaSelected,
                                       CarbonSelected = CarbonSelected,
                                       # RedSquirrelSelected = RedSquirrelSelected,
                                       SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                       VisitsSelected = VisitsSelected,
                                       CarbonSelectedSD = CarbonSelectedSD,
                                       # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                       SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                       VisitsSelectedSD = VisitsSelectedSD,
                                       alphaLVL = 0,tolvec=tolvecReactive()) # At the beginning we want to switch on all the sliders
        
        SelecRow<-1
        SelectedMins <- tmp$SelectedSimMat2
        SwitchedOnCells <- SelectedMins[SelecRow, 1:length(SavedVec)]
        
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
          
         # addControlText <- ""
          #for (i in 1:length(SPECIES)) {
          #  specie_latin <- SPECIES[i]
          #  specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
          #  selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
          #  selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
          #  if (SPECIES[i] == "All") {
          #    specie_english <- "All species"
          #  }
          #  addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1",sprintf("%.2f", 2 * selectedBioSDspecie), "<br>")
          #}
          
         # map <- map %>%
          #  addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", sprintf("%.2f",2*SelectedTreeCarbonSD), "<br>",
           #                          # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
            #                         addControlText,
             #                        "Area Planted: ", round(SelectedArea, 2), "<br>",
              #                       "Visitors: ", round(SelectedVisits, 2), "\u00B1", sprintf("%.2f",2*SelectedVisitsSD),
               #                      "</p>"), position = "topright",layerId="legend")
          
          
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
  
  # DONE TO CHANGE LATER
  output$map2 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactiveNoLegend()
      
    }
  })
  # DONE TO CHANGE LATER
  
  output$map3 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactiveNoLegend()
       }
  })
  # DONE TO CHANGE LATER
  
  output$map4 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactiveNoLegend()
      }
    
  })
  # DONE TO CHANGE LATER
  
  output$map5 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactiveNoLegend()
          }
  })
  
  
  output$map6 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactiveNoLegend()
    }
  })
  

  
  
  observeEvent(input$map2_click, {Selected_Cluster_To_Display_Reactive(1)  })
  observeEvent(input$map3_click, {Selected_Cluster_To_Display_Reactive(2) })
  observeEvent(input$map4_click, {Selected_Cluster_To_Display_Reactive(3)  })
  observeEvent(input$map5_click, {Selected_Cluster_To_Display_Reactive(4) })
  
  observeEvent(input$Carbon_plus,{
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && 
        (input$tabs=="Exploration") && (ClusteringDone())) {
    if(length(unique(Clustering_Category_VectorReactive()))>=4){
    PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                           Selected_Point_In_Cluster_To_Display_Reactive(),
                           Clustering_Category_VectorReactive(),
                           SubsetMeetTargetsReactiveUnique(),
                           Projected_TSNE_Data_Clusters_Reactive(),
                           Basis_Clustering_Reactive(),
                           Mean_Clusters_Reactive(),
                           Limits_Direction_Clusters_Reactive(),
                           Output_Name="Carbon",
                           Sign_Button="+"
                                )
    if(PM_res$UpdateSliders){
      updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
        updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
        
      }
    }
  })
  
  observeEvent(input$Carbon_minus,{
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
      PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                                   Selected_Point_In_Cluster_To_Display_Reactive(),
                                   Clustering_Category_VectorReactive(),
                                   SubsetMeetTargetsReactiveUnique(),
                                   Projected_TSNE_Data_Clusters_Reactive(),
                                   Basis_Clustering_Reactive(),
                                   Mean_Clusters_Reactive(),
                                   Limits_Direction_Clusters_Reactive(),
                                   Output_Name="Carbon",
                                   Sign_Button="-"
      )
      if(PM_res$UpdateSliders){
        updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
        updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
      
    }
    
  })

  for (spe in SPECIES) {
    local({
      sp <- spe        
      
      observeEvent(input[[paste0(sp, "_plus")]], {
        if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
          PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                                       Selected_Point_In_Cluster_To_Display_Reactive(),
                                       Clustering_Category_VectorReactive(),
                                       SubsetMeetTargetsReactiveUnique(),
                                       Projected_TSNE_Data_Clusters_Reactive(),
                                       Basis_Clustering_Reactive(),
                                       Mean_Clusters_Reactive(),
                                       Limits_Direction_Clusters_Reactive(),
                                       Output_Name=sp,
                                       Sign_Button="+"
          )
          if(PM_res$UpdateSliders){
            updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
            updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
          
        }
      })
      
      observeEvent(input[[paste0(sp, "_minus")]], {
        if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
          PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                                       Selected_Point_In_Cluster_To_Display_Reactive(),
                                       Clustering_Category_VectorReactive(),
                                       SubsetMeetTargetsReactiveUnique(),
                                       Projected_TSNE_Data_Clusters_Reactive(),
                                       Basis_Clustering_Reactive(),
                                       Mean_Clusters_Reactive(),
                                       Limits_Direction_Clusters_Reactive(),
                                       Output_Name=sp,
                                       Sign_Button="-"
          )
          if(PM_res$UpdateSliders){
            updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
            updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
          
        }
      })
      
    })
  }
  
  observeEvent(input$Area_plus,{
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
      PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                                   Selected_Point_In_Cluster_To_Display_Reactive(),
                                   Clustering_Category_VectorReactive(),
                                   SubsetMeetTargetsReactiveUnique(),
                                   Projected_TSNE_Data_Clusters_Reactive(),
                                   Basis_Clustering_Reactive(),
                                   Mean_Clusters_Reactive(),
                                   Limits_Direction_Clusters_Reactive(),
                                   Output_Name="Area",
                                   Sign_Button="+"
      )
      if(PM_res$UpdateSliders){
        updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
        updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
      
    }
    
  })
  
  
  observeEvent(input$Area_minus,{
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
      PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                                   Selected_Point_In_Cluster_To_Display_Reactive(),
                                   Clustering_Category_VectorReactive(),
                                   SubsetMeetTargetsReactiveUnique(),
                                   Projected_TSNE_Data_Clusters_Reactive(),
                                   Basis_Clustering_Reactive(),
                                   Mean_Clusters_Reactive(),
                                   Limits_Direction_Clusters_Reactive(),
                                   Output_Name="Area",
                                   Sign_Button="-"
      )
      if(PM_res$UpdateSliders){
        updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
        updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
      
    }
    
  })
  
  observeEvent(input$Visits_plus,{
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
      PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                                   Selected_Point_In_Cluster_To_Display_Reactive(),
                                   Clustering_Category_VectorReactive(),
                                   SubsetMeetTargetsReactiveUnique(),
                                   Projected_TSNE_Data_Clusters_Reactive(),
                                   Basis_Clustering_Reactive(),
                                   Mean_Clusters_Reactive(),
                                   Limits_Direction_Clusters_Reactive(),
                                   Output_Name="Visits",
                                   Sign_Button="+"
      )
      if(PM_res$UpdateSliders){
        updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
        updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
      
    }
    
  })
  
  
  observeEvent(input$Visits_minus,{
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration") && (ClusteringDone())) {
      PM_res<-Plus_Or_Minus_Button(Selected_Cluster_To_Display_Reactive(),
                                   Selected_Point_In_Cluster_To_Display_Reactive(),
                                   Clustering_Category_VectorReactive(),
                                   SubsetMeetTargetsReactiveUnique(),
                                   Projected_TSNE_Data_Clusters_Reactive(),
                                   Basis_Clustering_Reactive(),
                                   Mean_Clusters_Reactive(),
                                   Limits_Direction_Clusters_Reactive(),
                                   Output_Name="Visits",
                                   Sign_Button="-"
      )
      if(PM_res$UpdateSliders){
        updateSliderInput(inputId="slider_x", value = PM_res$NewValueOnSlider_x)
        updateSliderInput(inputId="slider_y", value = PM_res$NewValueOnSlider_y)}
      
    }
    
  })
  
  # On session close, delete temporary files and delete the extra processes
  session$onSessionEnded(function() {
    unlink(paste0("*", SESSION_FILE_SUFFIX, "*"))
    if (RUN_BO) {
      plan(sequential)
    }
  })
}

shinyApp(ui, server)
