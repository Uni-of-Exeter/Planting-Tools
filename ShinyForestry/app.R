# Created and maintained by Bertrand Nortier and Timothée Bacri

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

set.seed(1)


if(Sys.getenv("USERNAME")=="bn267"){
  FolderSource <- "C://Users//bn267//OneDrive - University of Exeter//Documents//GitHub//Planting-Tools//ShinyForestry//"
}


# more --> less: debug / info / warning / error / none
LOG_LEVEL <- "warning"

# FolderSource <- "ShinyForestry/"
FolderSource <- normalizePath(getwd())
if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
  FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
}

set.seed(1)


MAXYEAR<-28

# Delete log file
log_filename <- base::normalizePath(file.path(FolderSource, "log.txt"), mustWork = FALSE)
if (file.exists(log_filename)) {
  file.remove(log_filename)
}

source(normalizePath(file.path(FolderSource, "functions.R")))
source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")))
source(normalizePath(file.path(FolderSource, "preferTrees.R")))

install_and_load_packages("devtools", quiet = TRUE)
if (!require(dgpsi)) {
  devtools::install_github('mingdeyu/dgpsi-R', upgrade = "always", quiet = TRUE)
  library("dgpsi")
  dgpsi::init_py()
}
if (!require(RRembo)) {
  devtools::install_github('mbinois/RRembo', upgrade = "always", quiet = TRUE)
  library("RRembo")
}
install_and_load_packages(packages = c("car", "shinyjs", "shiny", "shinyjqui", "leaflet", "sf", "ggplot2",
                                       "geosphere", "feather", "readr", "dplyr", "tidyverse", "gsubfn",
                                       "ggpubr", "comprehenr", "Rtsne", "mclust", "seriation", "jsonlite",
                                       "viridis", "ggmap", "shinyjqui", "MASS", "shinyWidgets", "truncnorm",
                                       "GGally", "purrr", "sp", "colorspace", "rjson", "arrow", "lwgeom",
                                       "mvtnorm", "dplyr", "magrittr",
                                       "lhs", "sensitivity",
                                       "progressr", "doFuture", "promises",
                                       # # Active subspace method
                                       "concordance", "BASS", "zipfR",
                                       # To plot the best map, and save it to a file
                                       "mapview", "webshot",
                                       # File-locking, for multi-process
                                       "flock",
                                       "adaptMCMC"))

# Value to control the long-running task (Bayesian optimization in Tab 1)
# We track the task ID. If it changes, the previous long-running task gets cancelled.
set_latest_task_id(0)

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

if(Sys.getenv("USERNAME")=="bn267"){
### CHANGED!!!
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
  speciesprob40 <-  read.csv(normalizePath(file.path(DataFilesFolder, "scenario_species_prob_40.csv")), header = FALSE)
  climatecells <- read.csv(normalizePath(file.path(DataFilesFolder, "climate_cells.csv")))
}

message(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip"))))
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")))) {
  Sys.sleep(5)
}
message(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "found. Trying to unzip and load files..."))

if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))) {
  UnZipDirName <- normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp"))
  dir.create(UnZipDirName)
  while (inherits(suppressWarnings(try(unzip(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), exdir = UnZipDirName),
                                       silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
  }
  
  message(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "unzipped. Loading files..." ))
  shconv <- sf::st_read(normalizePath(file.path(UnZipDirName, "land_parcels.shp")))
  if (is.null(shconv$extent)) {
    shconv$extent <- "NoExtent"
  }
  st_write(shconv, normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))
  shconv <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson"))  )
} else {
  shconv <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))
}

message(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "decision_units.json"))))
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))) {
  Sys.sleep(5)
}
message(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "found. Trying to load file if FullTableMerged.geojson does not exist..."))

if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))) {
  sf_use_s2(FALSE)
  lsh <- dim(shconv)[1]
  
  while (inherits(suppressWarnings(try(AllUnits <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))$decision_unit_ids,
                                       silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
  }
  message(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "loaded, processing..." ))
  
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
  for(ii in 1:30)
  {
    FullTab[paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",(ii-1))]<-rep(0, length(Uni))
  }
  for(ii in 1:30)
  {
    FullTab[paste0("JulesSDY",(ii-1))]<-rep(0, length(Uni))
  }
  
  for(ii in 1:30)
  {
    FullTab[paste0("JulesMeanY85",(ii-1))]<-rep(0, length(Uni))
  }
  for(ii in 1:30)
  {
    FullTab[paste0("JulesSDY85",(ii-1))]<-rep(0, length(Uni))
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
  
  # Jules results for all years from 0 to 28
  JulesMeanYears<-arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-mean-monthly.feather")))[,c("x","y",paste0("mean",seq(1,337,by=12)))]
  JulesSDYears<-arrow::read_feather(normalizePath(file.path(DataFilesFolder, "JulesApp-rcp26-06-sd-monthly.feather")))[,c("x","y",paste0("sd",seq(1,337,by=12)))]
  
  
  SelectedJulesMeanYears<-JulesMeanYears[LinesJules,paste0("mean",12*(29-seq(1,29,1))+1)]
  SelectedJulesMeanYears[LinesJulesNoMinus1, ] <- 0
  SelectedJulesSDYears<-JulesSDYears[LinesJules,paste0("sd",12*(29-seq(1,29,1))+1)]
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
  SelectedJulesMeanYears85<-JulesMeanYears85[LinesJules,paste0("mean",12*(29-seq(1,29,1))+1)]
  SelectedJulesMeanYears85[LinesJulesNoMinus1, ] <- 0
  SelectedJulesSDYears85<-JulesSDYears85[LinesJules,paste0("sd",12*(29-seq(1,29,1))+1)]
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
    SellWeightsArr <- (matrix(SELLWeights, length(SELLWeights), 29))
    
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
      
      FullTable[ii,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:28)]<-colSums(SelJulesMeansYears*SellWeightsArr)
      FullTable[ii,paste0("JulesSDY",0:28)]<-sqrt(colSums((SelJulesSDsYears*SellWeightsArr)^2))
      
      FullTable$Carbon_Mean_Scenario26_TreeSpecieDeciduous<-sum(SelJulesMeans85*SELLWeights)
      FullTable$Carbon_SD_Scenario26_TreeSpecieDeciduous<-sqrt(sum((SelJulesSDs85*SELLWeights)^2))
      
      
      FullTable[ii,paste0("JulesMeanY85",0:28)]<-colSums(SelJulesMeansYears85*SellWeightsArr)
      FullTable[ii,paste0("JulesSDY85",0:28)]<-sqrt(colSums((SelJulesSDsYears85*SellWeightsArr)^2))
      
      
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
      
      
      
      FullTable[ii,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:28)]<-29
      FullTable[ii,paste0("JulesSDY",0:28)]<-29
      
      FullTable[ii,paste0("JulesMeanY85",0:28)]<-29
      FullTable[ii,paste0("JulesSDY85",0:28)]<-29
      
      
      FullTable$area[ii] <- sum(SELLWeights)
    }
  }
  
  # Replace Biodiversity columns with correct ones
  FullTable <- convert_bio_to_polygons_from_elicitor_and_merge_into_FullTable(Elicitor_table = FullTable,
                                                                              speciesprob40 = speciesprob40,
                                                                              seer2km = seer2km,
                                                                              jncc100 = jncc100,
                                                                              climatecells = climatecells,
                                                                              global_log_level = LOG_LEVEL)
  # Add richness columns
  FullTable <- add_richness_columns(FullTable = FullTable, NAME_CONVERSION = NAME_CONVERSION) %>% st_as_sf()
  
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

message("Loading ", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson ...")))
FullTable <- st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
message("Loading ", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson done")))

handlers(global = TRUE)

STDMEAN <- 0.05
STDSTD <- 0.01

# Random sampling
NSamp <- 5000

#Now the random sample contains the year of planting/
notif(paste0("Sampling ", NSamp, " random strategies ..."), global_log_level = LOG_LEVEL)

simul636 <- matrix(0, NSamp, dim(FullTable)[1])
Uniqunits <- unique(FullTable$units)
handlers(
  list(
    handler_progress(
      format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
    )
  ),
  on_missing = "ignore"
)
plan(multisession, workers = future::availableCores() - 2)
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
# plan(multisession, workers = 2)
plan(sequential)

handlers(
  list(
    handler_shiny(),
    handler_progress(
      format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
    )
  ),
  on_missing = "ignore"
)

RREMBO_CONTROL <- list(
  # method to generate low dimensional data in RRembo::designZ ("LHS", "maximin", "unif"). default unif
  designtype = "LHS",
  # if TRUE, use the new mapping from the zonotope, otherwise the original mapping with convex projection. default TRUE
  reverse = FALSE)
RREMBO_HYPER_PARAMETERS = RRembo_defaults(d = 6, D = nrow(FullTable),
                                          init = list(n = NSamp), budget = 100,
                                          control = RREMBO_CONTROL,
                                          global_log_level = LOG_LEVEL)

# for (aaa in 1:NSamp) {
#   pp <- runif(1)
#   RandSamp <- rmultinom(length(Uniqunits), 1, c(pp, 1 - pp))[1, ]
#   for (bbb in 1:length(Uniqunits)) {
#     simul636[aaa, FullTable$units == Uniqunits[bbb]] <- RandSamp[bbb]
#   }
# }

# Simul636Year is populated with the year of planting
# once simul636Year works it will replace simul636
# 29 is the code for no planting
# Otherwise 0 to 28 is the year of planting if planted.
simul636Year<-simul636
for (aaa in 1:NSamp) {
  
  simul636Year[aaa,simul636[aaa,]==0]<-(29)
  probb<-runif(1,0.2,0.6)
  size<-15*runif(1)
  simul636Year[aaa,simul636[aaa,]!=0]<-pmin(rnbinom(sum(simul636[aaa,]),size=size,prob=probb),28)
}
#### Simul636YearType is a similar table but it stops at year 28 and
#### instead, we pick a tree type (or no planting)
simul636YearType<-list(YEAR=simul636-1,TYPE=apply(simul636,2,as.character))
for (aaa in 1:NSamp) {
  
  simul636YearType[["TYPE"]][aaa,simul636[aaa,]==0]<-"C"
  probbType<-runif(1,0.5)
  Planted<-(simul636[aaa,]==1)
  if(sum(Planted)>0){
    simul636YearType[["TYPE"]][aaa,simul636[aaa,]==1]<-sample(c("A","B"),sum(Planted),replace=T,prob=c(probbType,1-probbType))}
  
  probb<-runif(1,0.2,0.6)
  size<-15*runif(1)
  DRAW<-pmin(rnbinom(sum(simul636[aaa,]),size=size,prob=probb),MAXYEAR)
  simul636YearType$YEAR[aaa,simul636YearType$TYPE[aaa,]!="C"]<-DRAW
}

notif(paste0("Sampling ", NSamp, " random strategies ... done"), global_log_level = LOG_LEVEL)

Simul636YearOverrideReactive<-reactiveVal(vector("list",dim(simul636Year)[2]))
Simul636YearTypeOverrideReactive<-reactiveVal(vector("list",dim(simul636Year)[2]))

#browser()
#hist(simul636YearType,100)

message(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))))
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
  Sys.sleep(5)
}
message(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file..."))

alphaLVL <- 0.9
MaxRounds <- 5
ConvertSample <- sample(1:NSamp, 200)

# Read the outcomes from the Elicitor app
while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
                                     , silent = TRUE)),
                "try-error")) {
  Sys.sleep(1)
}
message(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing..."))

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

verticalLayout_params <- c(list(sliderInput("SliderMain", "Tree Carbon Stored (tonnes of CO2):", min = -1, max = 870, value = -1)),
                           lapply(SPECIES, function(x, fulltable, NAME_CONVERSION_ARG) {
                             NAME_CONVERSION <- NAME_CONVERSION_ARG
                             # max_specie <- round(max(fulltable[, paste0("BioMean_", x)]))
                             # value <- round(max_specie / 2)
                             max_specie <- 36
                             value <- 1
                             
                             # If it is a group
                             if (x %in% c(NAME_CONVERSION$Group, NAME_CONVERSION$Group_pretty, "All")) {
                               text <- paste0("Change in Species Richness (", get_pretty_group(x, NAME_CONVERSION), ")")
                             } else {
                               # If it is a specie
                               text <- get_english_specie_from_specie(x, NAME_CONVERSION)
                               text <- get_pretty_english_specie(text, NAME_CONVERSION)
                               text <- paste(text, " (Change in Presence, %):")
                             }
                             
                             return(bquote(sliderInput(paste0("BioSlider", .(x)),
                                                       .(text),
                                                       min = 0,
                                                       max = .(max_specie),
                                                       value = .(value),
                                                       step = 0.5)))
                           }, fulltable = FullTable, NAME_CONVERSION_ARG = NAME_CONVERSION),
                           list(sliderInput("AreaSlider", HTML("Area Planted (km<sup>2</sup>)"), min = 0, max = 25, value = 15)),
                           list(sliderInput("VisitsSlider", "Recreation (average visits per month):", min = 0, max = 750, value = 400)))
#SPECIES<-c("All","Acanthis_cabaret","Birds","Alauda_arvensis")
SliderNames<- c("SliderMain",
                paste0("BioSlider", SPECIES),
                "AreaSlider","VisitsSlider")


##################### Precalculate simul636 table with years
AllExtents<-sort(unique(FullTable$extent))


PrecalcCarbonAllExtents<-list()
PrecalcCarbonAllExtentsSD<-list()

PrecalcCarbonAllExtentsType<-list()
PrecalcCarbonAllExtentsSDType<-list()


for (ext in AllExtents)
{
  CarbonSelectedYear<-FullTable[FullTable$extent == ext,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:29)]
  CarbonSelectedYear$geometry<-NULL
  CarbonSelectedSDYear<-FullTable[FullTable$extent == ext,paste0("JulesSDY",0:29)]
  CarbonSelectedSDYear$geometry<-NULL
  
  CarbonSelectedYear85<-FullTable[FullTable$extent == ext,paste0("JulesMeanY85",0:MAXYEAR)]
  CarbonSelectedYear85$geometry<-NULL
  CarbonSelectedSDYear85<-FullTable[FullTable$extent == ext,paste0("JulesSDY85",0:MAXYEAR)]
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
      PrecalcCarbonAllExtentsSD[[ext]][abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("JulesSDY",simul636Year[abb,bcc])]

      
      if(simul636YearType[["TYPE"]][abb,bcc]=="C"){
      PrecalcCarbonAllExtentsType[[ext]][abb,bcc]<-0
      PrecalcCarbonAllExtentsSDType[[ext]][abb,bcc]<-0
      }else{
        
        if(simul636YearType[["TYPE"]][abb,bcc]=="A"){
          PrecalcCarbonAllExtentsType[[ext]][abb,bcc]<-CarbonSelectedYear[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",
                                                                                         simul636YearType$YEAR[abb,bcc])]
          PrecalcCarbonAllExtentsSDType[[ext]][abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("JulesSDY",
                                                                                             simul636YearType$YEAR[abb,bcc])]
        }else{
          PrecalcCarbonAllExtentsType[[ext]][abb,bcc]<-CarbonSelectedYear85[bcc,paste0("JulesMeanY85",simul636YearType$YEAR[abb,bcc])]
          PrecalcCarbonAllExtentsSDType[[ext]][abb,bcc]<-CarbonSelectedSDYear85[bcc,paste0("JulesSDY85",simul636YearType$YEAR[abb,bcc])]
          
        }
          
          }
      
    }
    
  }
  
}



JulesMean <- 0;JulesSD <- 0;SquaresLoad <- 0;Sqconv <- 0;CorrespondenceJules <- 0;seer2km <- 0;jncc100 <- 0;speciesprob40 <- 0;climatecells <- 0;

ui <- fluidPage(useShinyjs(), tabsetPanel(id = "tabs",
                                          tabPanel("Maps", fluidPage(fluidRow(
                                            column(9,
                                                   selectInput("inSelect", "area", sort(unique(c(FullTable$extent, FullTableNotAvail$extent))), 
                                                               FullTable$extent[1]),
                                                   jqui_resizable(div(
                                                     leafletOutput("map", width = "100%", height = "100%"),
                                                     sliderInput("YearSelect","year",0,28,0,step=1,width = "100%")
                                                     
                                                   )
                                                   ),
                                                   
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
                                          tabPanel("Exploration", id = "Exploration",verticalLayout(
                                            fluidPage(fluidRow(
                                              column(10,verbatimTextOutput("ZeroText"),column(2,)))),
                                            fluidPage(fluidRow(
                                              column(5,
                                                     verticalLayout(verbatimTextOutput("FirstMapTxt"), jqui_resizable(leafletOutput("map2", height = 400, width = "100%")))
                                              ),
                                              column(5,
                                                     verticalLayout(verbatimTextOutput("SecondMapTxt"), jqui_resizable(leafletOutput("map3", height = 400, width = "100%")))
                                              ),
                                              column(2, verticalLayout(verbatimTextOutput("TargetText"),
                                                                       #  selectInput("chooseGrouping", "Grouping Type:", c("Carbon level"), "Carbon level"),
                                                                       actionButton("random", "Randomize!"))
                                              )
                                            ),
                                            fluidRow(
                                              column(5,
                                                     verticalLayout(verbatimTextOutput("ThirdMapTxt"), jqui_resizable(leafletOutput("map4", height = 400, width = "100%")))
                                              ),
                                              column(5,
                                                     verticalLayout(verbatimTextOutput("FourthMapTxt"), jqui_resizable(leafletOutput("map5", height = 400, width = "100%")))
                                              ),
                                              column(2, "")
                                            )
                                            )
                                          )
                                          ),
                                          tabPanel("Preferences", id = "Preferences",
                                                   fluidPage(
                                                     shinyjs::hidden(
                                                       fluidRow(12, checkboxInput("Trigger", "", value = FALSE, width = NULL))
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.Trigger == true",
                                                       fluidRow(
                                                         column(6, verticalLayout(jqui_resizable(leafletOutput("ClusterPage")), actionButton("choose1", "choose"))
                                                         ),
                                                         column(6, verticalLayout(jqui_resizable(leafletOutput("ClusterPage2")), actionButton("choose2", "choose"))
                                                         )
                                                       )),
                                                     conditionalPanel(
                                                       condition = "input.Trigger == false", fluidRow(column(12, jqui_resizable(plotOutput("plotOP1"))))
                                                     )
                                                   ))
))

server <- function(input, output, session,
                   SPECIES_ARG1 = SPECIES,
                   SPECIES_ENGLISH_ARG1 = SPECIES_ENGLISH,
                   N_TARGETS_ARG1 = N_TARGETS,
                   NAME_CONVERSION_ARG1 = NAME_CONVERSION,
                   TARGETS_ARG1 = TARGETS) {
  
  # hideTab(inputId = "tabs", target = "Exploration")
  # hideTab(inputId = "tabs", target = "Preferences")
  SPECIES <- SPECIES_ARG1
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG1
  N_SPECIES <- length(SPECIES)
  N_TARGETS <- N_TARGETS_ARG1
  TARGETS <- TARGETS_ARG1
  NAME_CONVERSION <- NAME_CONVERSION_ARG1
  
  bayesian_optimization_finished <- reactiveVal(TRUE)
  
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
  
  
  
  Text0 <- reactiveVal("")
  Text1 <- reactiveVal("")
  Text2 <- reactiveVal("")
  Text3 <- reactiveVal("")
  Text4 <- reactiveVal("")
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
                   "Tree Carbon: ", as.numeric(CarbonSliderVal()))
    # A for loop over the reactive values causes an issue: only the last reactive value
    # takes effect and therefore overwrites other reactive values, i.e. all bioSliderValSPECIE take
    # the same value. I have to work with a list for it to work.
    # for (x in SPECIES) {
    #   BioSliderValSpecie <- get(paste0("BioSliderVal", x))
    #   text <- paste0(text, "\n", x, ": ", as.numeric(BioSliderValSpecie()))
    # }
    for (i in 1:length(SPECIES)) {
      specie_english <- if (SPECIES[i] == "All") "All Species Richness" else SPECIES_ENGLISH[i]
      BioSliderValSpecie <- reactive_list[[i]]
      text <- paste0(text, "\n", get_pretty_english_specie(specie_english, NAME_CONVERSION), ": ", as.numeric(BioSliderValSpecie()))
    }
    
    text <- paste0(text,
                   # "\nRed Squirrel: ", as.numeric(bioSliderVal()),
                   "\nArea Planted: ", as.numeric(AreaSliderVal()),
                   "\nVisits/km^2: ", as.numeric(VisitsSliderVal()))
  })
  
  ColorLighteningFactor <- reactiveVal(0.5)
  ColorDarkeningFactor <- reactiveVal(0.5)
  
  ColourScheme <- reactiveVal("blue/red")
  
  output$ZeroText <- renderText({Text0()})
  output$FirstMapTxt <- renderText({Text1()})
  output$SecondMapTxt <- renderText({Text2()})
  output$ThirdMapTxt <- renderText({Text3()})
  output$FourthMapTxt <- renderText({Text4()})
  
  first_time_open_exploration_reactive <- reactiveVal(TRUE)
  
  # If we click random or open the Exploration tab then we pick 4 different scenarios
  observeEvent({
    input$tabs
  },{
    if (input$tabs == "Exploration"){
      if (first_time_open_exploration_reactive() == TRUE) {
        SelectedSample <- sample(1:dim(SubsetMeetTargetsReactiveUnique())[1],
                                 min(4, dim(SubsetMeetTargetsReactiveUnique())[1]), replace = FALSE)
        FourUniqueRowsReactive(SelectedSample)
        first_time_open_exploration_reactive(FALSE)
      }
    }
  })
  observeEvent({
    input$random
  },{
    SelectedSample <- sample(1:dim(SubsetMeetTargetsReactiveUnique())[1],
                             min(4, dim(SubsetMeetTargetsReactiveUnique())[1]), replace = FALSE)
    FourUniqueRowsReactive(SelectedSample)
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
  
  PreviousYearSelect<-reactiveVal(0)
  
  PreviousConsolidatedReactive<-reactiveVal(NULL)

  
  #  MaxValsReactive<-reactiveVal(0)
  MaxMinValsReactiveVector<-reactiveVal(0)
  SlidersHaveBeenInitialized<-reactiveVal(rep(0,length(SliderNames)))
  MapReactive<-reactiveVal(NULL)
  
  ClickedMatrixTab2Reactive<-reactiveVal(NULL)
  PreviousClickedMatrixTab2Reactive<-reactiveVal(NULL)
  
  tolvecReactive<-reactiveVal(NULL)
  
  SubsetMeetTargetsReactive<-reactiveVal(NULL)
  SubsetMeetTargetsReactiveUnique<-reactiveVal(NULL)
  PreviousSubsetMeetTargetsReactive<-reactiveVal(NULL)
  PreviousSubsetMeetTargetsReactiveUnique<-reactiveVal(NULL)
  FourUniqueRowsReactive<-reactiveVal(NULL)
  PreviousFourUniqueRowsReactive<-reactiveVal(NULL)
  
  
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
      
      #browser()
      AreaSelected <- FullTable$area[FullTable$extent == SelectedDropdown]
      CarbonSelected <- (FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[FullTable$extent == SelectedDropdown])
      # we take everything up to year 29 (no planting)
      CarbonSelectedYear<-FullTable[FullTable$extent == SelectedDropdown,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:29)]
      CarbonSelectedYear$geometry<-NULL
  

      CarbonSelectedYear85<-FullTable[FullTable$extent == SelectedDropdown,paste0("JulesMeanY85",0:29)]
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
      CarbonSelectedSDYear<-FullTable[FullTable$extent == SelectedDropdown,paste0("JulesSDY",0:29)]
      CarbonSelectedSDYear$geometry<-NULL

      CarbonSelectedSDYear85<-FullTable[FullTable$extent == SelectedDropdown,paste0("JulesSDY85",0:29)]
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
      
      
      PreviousSelectedVector(list(YEAR=rep(-2,dim(SelectedSquares)[1]),TYPE=rep("C",dim(SelectedSquares)[1])))
      SelectedVector(list(YEAR=rep(-1,dim(SelectedSquares)[1]),TYPE=rep("C",dim(SelectedSquares)[1])))
      
      ClickedVector(rep(0, dim(SelectedSquares)[1]))
      PreviousClickedVector(rep(-1, dim(SelectedSquares)[1]))
      
      
      ClickedVectorYear(rep(29, dim(SelectedSquares)[1]))
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
      #browser()   
      #TODO
      #browser()
      MaxVals<-InitFindMaxSliderValuesYear(SelectedVector(),
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
                                           ClickedVectorYear())
      
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
        updateSliderInput(session, bioslider, max = max_bioslider, value = max_bioslider, step = 0.5)
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
      #browser()
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
                                     tolvec=tolvecReactive())
      
      ########## same function with Year
      tmpYear <- outputmap_calculateMatsYear(input = input,
                                             SavedVecLoc = ClickedVector(),
                                             simul636YearLoc = simul636Year,
                                             AreaSelected = AreaSelected,
                                             CarbonSelected = CarbonSelected,
                                             CarbonSelectedYear =CarbonSelectedYear,
                                             # RedSquirrelSelected = RedSquirrelSelected,
                                             SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                             VisitsSelected = VisitsSelected,
                                             CarbonSelectedSD = CarbonSelectedSD,
                                             CarbonSelectedSDYear = CarbonSelectedSDYear,
                                             # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                             SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                             VisitsSelectedSD = VisitsSelectedSD,
                                             alphaLVL=alphaLVL,
                                             ManualTargets=list(MaxVals$CarbonMax,MaxVals$bioMaxList,max_areaslider,max_visitsslider),
                                             tolvec=tolvecReactive(),
                                             #YearSelect=input$YearSelect,
                                             PrecalculatedCarbonSelectedTableMean=PrecalcCarbonAllExtents[[SelectedDropdown]],
                                             PrecalculatedCarbonSelectedTableSD=PrecalcCarbonAllExtentsSD[[SelectedDropdown]],
                                             SavedVecYearLoc=ClickedVectorYear(),
                                             PreviousSavedVecYearLoc=PreviousClickedVector(),
                                             SAMPLELIST=Simul636YearOverrideReactive()
      )
      
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
                                             PreviousSavedVecYearTypeLoc=PreviousClickedVectorType(),
                                             SAMPLELIST=Simul636YearTypeOverrideReactive()
      )
      
    
      ######## With Year
      SelectedSimMat2<-list()
      SelectedSimMat2[["YEAR"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.YEAR.",1:dim(simul636YearType[["YEAR"]])[2])]
      SelectedSimMat2[["TYPE"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.TYPE.",1:dim(simul636YearType[["TYPE"]])[2])]
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
      CONDITION_SEL<-(PROBAMAT[,1] >= alphaLVL) &
        # Biodiversity
        # (SelectedSimMat2$redsquirrel >= SelecTargetBio) &
        condition &
        # Area
        (PROBAMAT[,dim(PROBAMAT)[2]-1] >= alphaLVL) &
        # Visits
        (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL)
    
      SubsetMeetTargets <-list()
      SubsetMeetTargets[["YEAR"]]<- SelectedSimMat2$YEAR[CONDITION_SEL,]
      SubsetMeetTargets[["TYPE"]]<- SelectedSimMat2$TYPE[CONDITION_SEL,]
      
      SubsetMeetTargetsReactive(SubsetMeetTargets)
      
      DF<-data.frame(SubsetMeetTargets$YEAR,SubsetMeetTargets$TYPE)
      uniqueRows<-which(!duplicated(DF))
      
      SubsetMeetTargetsReactiveUnique(list(YEAR=SubsetMeetTargets$YEAR[uniqueRows,],TYPE=SubsetMeetTargets$TYPE[uniqueRows,]))
      PreviousSub<-SubsetMeetTargets
      PreviousSub$YEAR<-SubsetMeetTargets$YEAR-1
      PreviousSubsetMeetTargetsReactive(PreviousSub)
      
      DFPrev<-data.frame(PreviousSub$YEAR,PreviousSub$TYPE)
      uniqueRowsPrev<-which(!duplicated(DFPrev))
      PreviousSubsetMeetTargetsReactiveUnique(list(YEAR=PreviousSub$YEAR[uniqueRowsPrev,],TYPE=PreviousSub$TYPE[uniqueRowsPrev,]))
      
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
    if(input$YearSelect!=YearSelectReactive()){
      YearSelectReactive(input$YearSelect)}})
  
  # Trigger if anything changes
  # TODO: Maybe add &bayesian_optimization_finished() as a condition, or check that values have been populated
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
      YearSelect<-YearSelectReactive()
      PrevYearSelectedLoc<-PreviousYearSelect()
      
      MeanCarbonVec<- FullTable[,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:29)]
      MeanCarbonVec$geometry<-NULL
      
      MeanCarbonVec85<- FullTable[,paste0("JulesMeanY85",0:29)]
      MeanCarbonVec85$geometry<-NULL
      
      VARCarbonVec<- (FullTable[,paste0("JulesSDY",0:29)])
      VARCarbonVec$geometry<-NULL
      VARCarbonVec<-VARCarbonVec^2
      
      VARCarbonVec85<- (FullTable[,paste0("JulesSDY85",0:29)])
      VARCarbonVec85$geometry<-NULL
      VARCarbonVec85<-VARCarbonVec85^2
      
      #browser()
      #YearsSelectedRow<-SelectedRow[1,paste0("SelectedSimMat.YEAR.",1:length(SavedVecYear))]
      #YearsSelectedRow[YearsSelectedRow>YearSelect]<-(-1)
      
      SelectedRowType<-as.character(SelectedRow[,paste0("SelectedSimMat.TYPE.",1:length(SavedVecYear))])
      SelectedRowYear<-as.numeric(SelectedRow[,paste0("SelectedSimMat.YEAR.",1:length(SavedVecYear))])
      
      
      PreviousSelectedVec$YEAR[PreviousSelectedVec$YEAR>PrevYearSelectedLoc]<-(-1)
      #browser()
      
      
      # Display that we can plant from SavedVecYear 
      #SavedVecYear[SavedVecYear>=YearSelect]<-29
      
      ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units,
                             ColorLighteningFactor(), ColorDarkeningFactor())
      
      CarbonMeanCalc<-rep(0.001,length(SavedVecYear))

      CarbonVarCalc<-rep(0.001,length(SavedVecYear))

      for(aa in 1:length(SavedVecYear))
      { 
        if(SelectedRowType[aa]=="A"){
        CarbonMeanCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),MeanCarbonVec[aa,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",SelectedRowYear[aa])],0)
        CarbonVarCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),VARCarbonVec[aa,paste0("JulesSDY",SelectedRowYear[aa])],0)
        }else{
          if(SelectedRowType[aa]=="B"){
        CarbonMeanCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),MeanCarbonVec85[aa,paste0("JulesMeanY85",SelectedRowYear[aa])],0)
        CarbonVarCalc[aa]<-ifelse((SelectedRowYear[aa]>(-1))&(SelectedRowYear[aa]<=YearSelect),VARCarbonVec85[aa,paste0("JulesSDY85",SelectedRowYear[aa])],0)}else{
          CarbonMeanCalc[aa]<-0; CarbonVarCalc[aa]<-0}
        }
      }

      FullColVec <- ColObtained$FullColVec
      ClickedCols <- ColObtained$ClickedCols
      
      # Code: 1: RED: no planting, 2: Tree Type A 3: Tree Type B
      
      TypeA<-(SelectedRowType=="A")&(SelectedRowYear<=YearSelect)&(SavedVecYearType<YearSelect)
      TypeB<-(SelectedRowType=="B")&(SelectedRowYear<=YearSelect)&(SavedVecYearType<YearSelect)
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
         # browser()
          COLOURS[TypeA]<-"purple"
          COLOURS[TypeB]<-"green"
          COLOURS[BlockedCells]<-"red"
          
          
          mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),color=COLOURS,fillColor=COLOURS,weight=1)
          
          removeControl(mapp,layerId="legend")
          
          # If the bayesian optimization is not finished, SelectedFullTableRow() is NULL, so try again 5 seconds later
          if (is.null(SelectedFullTableRow())) {
            invalidateLater(5000)
          }
          
          SFTR<-SelectedFullTableRow()
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
            selectedBiospecie <- SFTR[[specie_latin]]
            selectedBioSDspecie <- SFTR[[paste0( specie_latin,"SD")]]
            if(!is.null(selectedBiospecie)){
              addControlText <- paste0(addControlText, specie_english, ": ", 
                                       round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")}
          }
          #browser()
          mapp<-
            addControl(mapp,html = paste0("<p>Carbon: ", round(sum(CarbonMeanCalc), 2), "\u00B1", round(2*sqrt(sum(CarbonVarCalc)), 2), "<br>",
                                          # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
                                          addControlText,
                                          "Area Planted: ", round(SFTR$Area, 2), "<br>",
                                          "Visitors: ", round(SFTR$Visits, 2), "\u00B1", round(2*SFTR$VisitsSD, 2),
                                          "</p>"), position = "topright",layerId="legend")
          
          
        }
        
      }
      
      
      PreviousClickedVector(SavedVec)  
      
      PreviousClickedVectorYear(SavedVecYear)  
      PreviousClickedVectorYearType(SavedVecYearType)  
            
      PreviousSelectedVector(SelectedVec)
      # replace the text
      PreviousYearSelect(YearSelect)
      PreviousConsolidatedReactive(Consolidated)
      
    }
  })
  
  # LOOP TO render the land parcel colours
  
  # TO CHANGE LATER!!
  observe({
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration")) {
      #browser()
      SubsetMeetTargets<-SubsetMeetTargetsReactive()
      PreviousSubsetMeetTargets<-PreviousSubsetMeetTargetsReactive()
      SubsetMeetTargetsUnique<-SubsetMeetTargetsReactiveUnique()
      PreviousSubsetMeetTargetsUnique<-PreviousSubsetMeetTargetsReactiveUnique()
      
      SavedMat<-ClickedMatrixTab2Reactive()
      PreviousSavedMat<-PreviousClickedMatrixTab2Reactive()
      FourUniqueRowsLoc<-FourUniqueRowsReactive()
      PreviousFourUniqueRowsLoc<-PreviousFourUniqueRowsReactive()
      if(length(FourUniqueRowsLoc)>0){
        SelectedRows<-SubsetMeetTargetsUnique[FourUniqueRowsLoc,]
        PrevSelectedRows<-PreviousSubsetMeetTargetsUnique[PreviousFourUniqueRowsLoc,]
        
        ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units,
                               ColorLighteningFactor(), ColorDarkeningFactor())
        
        FullColVec <- ColObtained$FullColVec
        ClickedCols <- ColObtained$ClickedCols
        if (dim(PrevSelectedRows)[1] < dim(SelectedRows)[1]) { PrevSelectedRows=SelectedRows+1 }
        
        
        for (ii in seq(1,min(4,length(FourUniqueRowsLoc)))) {
          
          Consolidated<-2*SavedMat[ii,]+1*((SelectedRows[ii,1:dim(SavedMat)[2]]==1)&(SavedMat[ii,]==0))
          
          PreviousConsolidated<-2*PreviousSavedMat[ii,]+1*((PrevSelectedRows[ii,1:dim(SavedMat)[2]]==1)&(PreviousSavedMat[ii,]==0))
          if (length(PreviousConsolidated)==0){PreviousConsolidated<-Consolidated+1}
          if ((CreatedBaseMap()==1)&(dim(SavedMat)[2]>0)){
            
            mapp<-leafletProxy(paste0("map",ii+1))
            removeShape(mapp,layerId=paste0("Square",1:length(Consolidated)))
            COLOURS<-rep("transparent",length(Consolidated))
            COLOURS[Consolidated==1]<-FullColVec[Consolidated==1]
            COLOURS[Consolidated==2]<-ClickedCols[Consolidated==2]
            mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),color=COLOURS,fillColor=COLOURS,weight=1)
            
          }
          removeControl(mapp,layerId="legend")
          #browser()
          SFTR<-SelectedRows[ii,]
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
            selectedBiospecie <- SFTR[[specie_latin]]
            selectedBioSDspecie <- SFTR[[paste0( specie_latin,"SD")]]
            addControlText <- paste0(addControlText, specie_english, ": ", 
                                     round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
          }
          
          mapp<-
            addControl(mapp,html = paste0("<p>Carbon: ", round(SFTR$Carbon, 2), "\u00B1", round(2*SFTR$CarbonSD, 2), "<br>",
                                          # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
                                          addControlText,
                                          "Area Planted: ", round(SFTR$Area, 2), "<br>",
                                          "Visitors: ", round(SFTR$Visits, 2), "\u00B1", round(2*SFTR$VisitsSD, 2),
                                          "</p>"), position = "topright",layerId="legend")
          
          
          
        }
        
        #}
        if(length(FourUniqueRowsLoc)<4){
          #add here text to say that there are no more unique examples.
          for(ii in seq(length(FourUniqueRowsLoc)+1,4))
          {
            
            mapp<-leafletProxy(paste0("map",ii+1))
            removeShape(mapp,layerId=paste0("Square",1:length(Consolidated)))
            mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),
                              color="transparent",fillColor="transparent")  
            removeControl(mapp,layerId="legend")
            mapp<-
              addControl(mapp,html = "", position = "topright",layerId="legend")
            
            
          }
          PreviousSubsetMeetTargetsReactive(SubsetMeetTargetsReactive())
          PreviousFourUniqueRowsReactive(FourUniqueRowsReactive())
          PreviousSubsetMeetTargetsReactiveUnique(SubsetMeetTargetsReactiveUnique())
          
          
          UpdatedRows<-ClickedMatrixTab2Reactive()
          if(length(FourUniqueRowsLoc)<4){
            UpdatedRows[length(FourUniqueRowsLoc):4,]<-PreviousClickedMatrixTab2Reactive()[length(FourUniqueRowsLoc):4,]
            
          }
          PreviousClickedMatrixTab2Reactive(UpdatedRows)
        }
        
      }else{}
      
    }
  })
  
  
  #DONE
  observeEvent({
    input$random
    input$tabs
  }, {
    FourUniqueRows<-FourUniqueRowsReactive()
    if(length(FourUniqueRows)>0){
      Text1(
        paste0("Strategy Displayed: ",FourUniqueRowsReactive()[1]," out of ",dim(SubsetMeetTargetsReactiveUnique())[1])
      )}else{
        Text1("No Strategy that meet all the targets")
      }
    if(length(FourUniqueRows)>1){
      Text2(
        paste0("Strategy Displayed: ",FourUniqueRowsReactive()[2]," out of ",dim(SubsetMeetTargetsReactiveUnique())[1])
      )}else{
        Text2("No Second Strategy that meet all the targets")
      }
    if(length(FourUniqueRows)>2){
      Text3(
        paste0("Strategy Displayed: ",FourUniqueRowsReactive()[3]," out of ",dim(SubsetMeetTargetsReactiveUnique())[1])
      )}else{
        Text3("No Third Strategy that meet all the targets")
      }
    if(length(FourUniqueRows)>3){
      Text4(
        paste0("Strategy Displayed: ",FourUniqueRowsReactive()[4]," out of ",dim(SubsetMeetTargetsReactiveUnique())[1])
      )}else{
        Text4("No Fourth Strategy that meet all the targets")
      }
    
    Text0(paste0("Estimated percentage of strategies that meet all ", N_TARGETS," targets: ",
                 round(dim(SubsetMeetTargetsReactiveUnique())[1] / dim(unique(simul636))[1] * 100, 2),"%"))
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
  
  #lapply(SliderNames, function(sl) {observeEvent(input[[sl]],{
  # if (input[[sl]]) {
  observeEvent({input$map_shape_click
    lapply(SliderNames, function(sl) {input[[sl]]})
    input$YearSelect
  },{
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SlidersHaveBeenInitialized())==1)) {
      
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
        
        if (current_task_id != get_latest_task_id()) {
          notif(paste("Task", current_task_id, "cancelled."), global_log_level = LOG_LEVEL)
          return()
        }
        
        tmp <- outputmap_calculateMats(input = input,
                                       SavedVecLoc = SavedVec,
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
                                       tolvec=tolvecReactive())
        
        
        ########## same function with Year
        # browser()
        tmpYear <- outputmap_calculateMatsYear(input = input,
                                               SavedVecLoc = SavedVec,
                                               simul636YearLoc = simul636Year,
                                               AreaSelected = AreaSelected,
                                               CarbonSelected = CarbonSelected,
                                               CarbonSelectedYear =CarbonSelectedYear,
                                               SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                               VisitsSelected = VisitsSelected,
                                               CarbonSelectedSD = CarbonSelectedSD,
                                               CarbonSelectedSDYear = CarbonSelectedSDYear,
                                               SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                               VisitsSelectedSD = VisitsSelectedSD,
                                               alphaLVL=alphaLVL,
                                               tolvec=tolvecReactive(),
                                               #YearSelect=input$YearSelect,
                                               PrecalculatedCarbonSelectedTableMean=PrecalcCarbonAllExtents[[SelectedDropdown]],
                                               PrecalculatedCarbonSelectedTableSD=PrecalcCarbonAllExtentsSD[[SelectedDropdown]],
                                               SavedVecYearLoc = ClickedVectorYear(),
                                               PreviousSavedVecYearLoc=PreviousClickedVector(),
                                               SAMPLELIST=Simul636YearOverrideReactive()
        )
        #browser()
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
                                                       PreviousSavedVecYearTypeLoc=PreviousClickedVectorType(),
                                                       SAMPLELIST=Simul636YearTypeOverrideReactive()
        )
        
        
        
        ######## With Year
        #browser()
        
        # TO CHANGE HERE
        
        #SelectedSimMat2 <- tmpYear$SelectedSimMat2[,-(1:(2*dim(simul636Year)[2]))]#tmpYear$SelectedSimMat2
        #Icalc <- tmpYear$Icalc
        #LimitsMat <- tmpYear$LimitsMat
        #SelecTargetCarbon <- tmpYear$SelecTargetCarbon
        #SelecTargetArea <- tmpYear$SelecTargetArea
        #SelecTargetVisits <- tmpYear$SelecTargetVisits
        
        
        SelectedSimMat2<-list()
        SelectedSimMat2[["YEAR"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.YEAR.",1:dim(simul636YearType[["YEAR"]])[2])]
        SelectedSimMat2[["TYPE"]]<- tmpYearType$SelectedSimMat2[,paste0("SelectedSimMat.TYPE.",1:dim(simul636YearType[["TYPE"]])[2])]

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
          value <- tmp[[var_name]]
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
        CONDITION_SEL<-(PROBAMAT[,1] >= alphaLVL) &
          # Biodiversity species
          condition &
          # Area
          (PROBAMAT[,dim(PROBAMAT)[2]-1] >= alphaLVL) &
          # Visits
          (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL)
        
        SubsetMeetTargets <-list()
        SubsetMeetTargets[["YEAR"]]<- SelectedSimMat2$YEAR[CONDITION_SEL,]
        SubsetMeetTargets[["TYPE"]]<- SelectedSimMat2$TYPE[CONDITION_SEL,]
        
        SubsetMeetTargets[["CarbonMean"]]<-tmpYearType$SelectedSimMat2$Carbon[CONDITION_SEL]
        SubsetMeetTargets[["CarbonSD"]]<-tmpYearType$SelectedSimMat2$CarbonSD[CONDITION_SEL]
        
        for (iii in 1:length(SPECIES)) {
          x<-SPECIES[iii]
          SubsetMeetTargets[[x]]<-tmpYearType$SelectedSimMat2[[x]][CONDITION_SEL]
          SubsetMeetTargets[[paste0(x,"SD")]]<-tmpYearType$SelectedSimMat2[[paste0(x,"SD")]][CONDITION_SEL]
        }
        SubsetMeetTargets[["Visits"]]<-tmpYearType$SelectedSimMat2[["Visits"]][CONDITION_SEL]
        SubsetMeetTargets[["VisitsSD"]]<-tmpYearType$SelectedSimMat2[["VisitsSD"]][CONDITION_SEL]
        SubsetMeetTargets[["Area"]]<-tmpYearType$SelectedSimMat2[["Area"]][CONDITION_SEL]
        
        if (current_task_id != get_latest_task_id()) {
          notif(paste("Task", current_task_id, "cancelled."), global_log_level = LOG_LEVEL)
          return()
        }
        
        SubsetMeetTargetsReactive(SubsetMeetTargets)
        
        DF<-data.frame(SubsetMeetTargets$YEAR,SubsetMeetTargets$TYPE)
        uniqueRows<-which(!duplicated(DF))
        
        SubsetMeetTargetsReactiveUnique(list(YEAR=SubsetMeetTargets$YEAR[uniqueRows,],TYPE=SubsetMeetTargets$TYPE[uniqueRows,]))
        PreviousSub<-SubsetMeetTargets
        PreviousSub$YEAR<-SubsetMeetTargets$YEAR-1
        PreviousSubsetMeetTargetsReactive(PreviousSub)
        
        DFPrev<-data.frame(PreviousSub$YEAR,PreviousSub$TYPE)
        uniqueRowsPrev<-which(!duplicated(DFPrev))
        PreviousSubsetMeetTargetsReactiveUnique(list(YEAR=PreviousSub$YEAR[uniqueRowsPrev,],TYPE=PreviousSub$TYPE[uniqueRowsPrev,]))
                 

        if(dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1]>0){
          LengthVec<-min(4,dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1])
          FourUniqueRowsReactive(seq(1,LengthVec))
          PreviousFourUniqueRowsReactive(seq(1,LengthVec))
        }else{FourUniqueRowsReactive(NULL)
          PreviousFourUniqueRowsReactive(NULL)}
        
        if (dim(SubsetMeetTargetsReactiveUnique()$YEAR)[1] > 0) {
          if (max(tmpYearType$SelectedSimMat2$Carbon) != min(tmpYearType$SelectedSimMat2$Carbon)) {
            DistSliderCarbon <- (SubsetMeetTargets[["CarbonMean"]] - SelecTargetCarbon) / (max(tmpYearType$SelectedSimMat2$Carbon) - min(tmpYearType$SelectedSimMat2$Carbon))
          } else {
            DistSliderCarbon <- (SubsetMeetTargets[["CarbonMean"]] - SelecTargetCarbon) / (max(tmpYearType$SelectedSimMat2$Carbon))
          }
          # if (max(SelectedSimMat2$redsquirrel) != min(SelectedSimMat2$redsquirrel)) {
          #   DistSliderBio <- (SubsetMeetTargets$redsquirrel - SelecTargetBio) / (max(SelectedSimMat2$redsquirrel) - min(SelectedSimMat2$redsquirrel))
          # } else {
          #   DistSliderBio <- (SubsetMeetTargets$redsquirrel - SelecTargetBio) / (max(SelectedSimMat2$redsquirrel))
          # }
          #browser()
          DistSliderBioListDataframes <- list()
          for (x in SPECIES) {
            SelecTargetBiospecie <- get(paste0("SelecTargetBio", x))[[1]]
            var_name <- paste0("DistSliderBio", x)
            if (max(tmpYearType$SelectedSimMat2[x]) != min(tmpYearType$SelectedSimMat2[x])) {
              value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie) / (max(tmpYearType$SelectedSimMat2[[x]]) - min(tmpYearType$SelectedSimMat2[[x]]))
            } else {
              if (max(tmpYearType$SelectedSimMat2[x]) != 0) {
                value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie) / (max(tmpYearType$SelectedSimMat2[[x]]))
              } else {
                value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie)
              }
            }
            assign(var_name, value)
            DistSliderBioListDataframes[x] <- data.frame(x = value)
          }
          if (max(tmpYearType$SelectedSimMat2$Area) != min(tmpYearType$SelectedSimMat2$Area)) {
            DistSliderArea <- (SubsetMeetTargets[["Area"]] - SelecTargetArea) / (max(tmpYearType$SelectedSimMat2$Area) - min(tmpYearType$SelectedSimMat2$Area))
          } else {
            DistSliderArea <- (SubsetMeetTargets[["Area"]] - SelecTargetArea) / (max(tmpYearType$SelectedSimMat2$Area))
          }
          if (max(tmpYearType$SelectedSimMat2$Visits) != min(tmpYearType$SelectedSimMat2$Visits)) {
            DistSliderVisits <- (SubsetMeetTargets[["Visits"]] - SelecTargetVisits) / (max(tmpYearType$SelectedSimMat2$Visits) - min(tmpYearType$SelectedSimMat2$Visits))
          } else {
            DistSliderVisits <- (SubsetMeetTargets[["Visits"]] - SelecTargetVisits) / (max(tmpYearType$SelectedSimMat2$Visits))
          }
          
          if (current_task_id != get_latest_task_id()) {
            notif(paste("Task", current_task_id, "cancelled."), global_log_level = LOG_LEVEL)
            return()
          }
          
          # REMINDER TO SCALE VALUES
          DistSliderBioDataframe <- do.call(cbind, DistSliderBioListDataframes)
          # SelecdMinRows <- which((DistSliderCarbon + DistSliderBio + DistSliderArea + DistSliderVisits) == min(DistSliderCarbon + DistSliderBio + DistSliderArea + DistSliderVisits))
          # SelecdMinRows <- which((DistSliderCarbon + DistSliderBio1 + DistSliderBio2 + DistSliderArea + DistSliderVisits) == min(DistSliderCarbon + DistSliderBio1 + DistSliderBio2 + DistSliderArea + DistSliderVisits))
          #SelecdMinRows <- which.min(DistSliderCarbon + rowSums(DistSliderBioDataframe) + DistSliderArea + DistSliderVisits)
          #SelectedMins <- SubsetMeetTargets[SelecdMinRows, ]
          #SelecRow <- which.min(rowSums(SelectedMins[1:length(SavedVec), ]))
          SUMM <- DistSliderCarbon + rowSums(DistSliderBioDataframe) + DistSliderArea + DistSliderVisits
          SelecdMinRows <- which(SUMM == min(SUMM))
          SelectedMins <- list(YEAR=SubsetMeetTargets$YEAR[SelecdMinRows, ],TYPE=SubsetMeetTargets$TYPE[SelecdMinRows,])
         
          
          # If it is a vector, i.e. only 1 unit is available
          if (length(SavedVec) == 1) {
            result <- SelectedMins$YEAR
            #[, 1]
          } else {
            # If it is a data frame
            result <- rowSums(SelectedMins$YEAR)
          }
          SelecRow <- which.min(result)
          
          if (current_task_id != get_latest_task_id()) {
            notif(paste("Task", current_task_id, "cancelled."), global_log_level = LOG_LEVEL)
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
          
          msg <- "Updating SelectedFullTableRow before BO ..."
          notif(msg, log_level = "debug", global_log_level = LOG_LEVEL)
          
          SelectedFullTableRow(
            tmpYearType$SelectedSimMat2[CONDITION_SEL,][SelecRow,]
          )
          SelectedVector(SelectedMins)
          
          msg <- paste(msg, "done")
          notif(msg, log_level = "debug", global_log_level = LOG_LEVEL)
          
          if (current_task_id != get_latest_task_id()) {
            notif(paste("Task", current_task_id, "cancelled."), global_log_level = LOG_LEVEL)
            return()
          }
          
          msg <- paste0("task ", current_task_id, " BO future start")
          notif(msg, global_log_level = LOG_LEVEL)
          bayesian_optimization_finished(FALSE)
          
          if (isFALSE(is.null(infpref_reactive()))) {
            # Order is c("Carbon", SPECIES, "Area", "Visits")
            len <- length(infpref_reactive())
            preference_weight_area <- infpref_reactive()[len - 1]
            mypref <- infpref_reactive()[c(1:(len - 2), len)]
          } else {
            preference_weight_area <- - 1
            mypref <- rep(1, N_TARGETS - 1)
          }
          tolvec <- tolvecReactive()
          # https://shiny.posit.co/r/articles/improve/nonblocking/index.html
          bayesian_optimization_extendedtask <- ExtendedTask$new(function(
            seed,
            FullTable,
            area_sum_threshold,
            outcomes_to_maximize_sum_threshold_vector,
            # outcomes_to_minimize_sum_threshold_vector = NULL,
            global_log_level,
            PLOT,
            
            BAYESIAN_OPTIMIZATION_ITERATIONS,
            # progressr_object_arg,
            # progressr_object = progressor(steps = max_loop_progress_bar, message = "Bayesian optimization"),
            BAYESIAN_OPTIMIZATION_BATCH_SIZE,
            PENALTY_COEFFICIENT,
            EXPLORATION,
            EXPLORATION_COEFFICIENT,
            
            SCALE,
            
            preference_weight_area,
            preference_weights_maximize,
            # preference_weights_minimize = rep(1, length(c())),
            
            current_task_id,
            
            CUSTOM_DESIGN_POINTS_STRATEGIES,
            DESIGN_POINTS_STRATEGY,
            
            CONSTRAINED_INPUTS,
            
            RREMBO_CONTROL,
            RREMBO_HYPER_PARAMETERS,
            RREMBO_SMART,
            
            # GP
            KERNEL, # matern2.5 or sexp
            NUMBER_OF_VECCHIA_NEIGHBOURS,
            
            tolvec,
            alpha
          ) {
            future_promise(expr = {
              bo_results <- bayesian_optimization(
                # session = session,
                seed = seed,
                FullTable = FullTable,
                area_sum_threshold = area_sum_threshold,
                outcomes_to_maximize_sum_threshold_vector = outcomes_to_maximize_sum_threshold_vector,
                # outcomes_to_minimize_sum_threshold_vector = NULL,
                global_log_level = global_log_level,
                PLOT = PLOT,
                
                BAYESIAN_OPTIMIZATION_ITERATIONS = BAYESIAN_OPTIMIZATION_ITERATIONS,
                # progressr_object = progressr_object_arg,
                # progressr_object = progressor(steps = max_loop_progress_bar, message = "Bayesian optimization"),
                BAYESIAN_OPTIMIZATION_BATCH_SIZE = BAYESIAN_OPTIMIZATION_BATCH_SIZE,
                PENALTY_COEFFICIENT = PENALTY_COEFFICIENT,
                # PENALTY_COEFFICIENT = 10 * max(FullTable %>% select(contains("Mean"))),
                EXPLORATION = EXPLORATION, # FALSE for tab 1, TRUE for tab 2
                EXPLORATION_COEFFICIENT = EXPLORATION_COEFFICIENT,
                
                SCALE = SCALE,
                
                preference_weight_area = preference_weight_area,
                preference_weights_maximize = preference_weights_maximize,
                # preference_weights_minimize = rep(1, length(c())),
                
                current_task_id = current_task_id,
                
                CUSTOM_DESIGN_POINTS_STRATEGIES = CUSTOM_DESIGN_POINTS_STRATEGIES,
                DESIGN_POINTS_STRATEGY = DESIGN_POINTS_STRATEGY,
                
                CONSTRAINED_INPUTS = CONSTRAINED_INPUTS,
                
                RREMBO_CONTROL = RREMBO_CONTROL,
                RREMBO_HYPER_PARAMETERS = RREMBO_HYPER_PARAMETERS,
                RREMBO_SMART = RREMBO_SMART,
                
                # GP
                KERNEL = KERNEL, # matern2.5 or sexp
                NUMBER_OF_VECCHIA_NEIGHBOURS = NUMBER_OF_VECCHIA_NEIGHBOURS,
                
                tolvec = tolvec,
                alpha = alpha
              )
              return(bo_results)
            }, seed = NULL) %...>% {
              
              bo_results <- .
              
              # Check if this result is invalid (i.e. a newer task has started)
              if (isFALSE(bo_results) || current_task_id != get_latest_task_id()) {
                msg <- paste0("task ", current_task_id, " The previous Bayesian optimization has been cancelled.")
                notif(msg, global_log_level = global_log_level)
                showNotification(msg)
                return()
              } else { # If the result is valid (i.e. there are no new tasks started)
                
                # If no results, i.e. no feasible solution
                if (is.null(bo_results)) {
                  showNotification("No feasible solution found")
                  return()
                }
                
                # Otherwise, a feasible solution is found
                area_sum <- sum(bo_results$area_vector)
                parcels_activation <- bo_results$area_vector
                parcels_activation[parcels_activation != 0] <- 1
                names(parcels_activation) <- NULL
                
                last_col <- ncol(bo_results$outcomes_to_maximize)
                number_of_rows <- nrow(bo_results$outcomes_to_maximize)
                col_sums <- c(colSums(bo_results$outcomes_to_maximize %>% dplyr::select("Carbon_Mean_Scenario26_TreeSpecieConifers")),
                              colMeans(bo_results$outcomes_to_maximize %>% dplyr::select(-"Carbon_Mean_Scenario26_TreeSpecieConifers")))
                col_sums_SD <- c(sqrt(colSums((bo_results$outcomes_to_maximize_SD %>% dplyr::select("Carbon_SD_Scenario26_TreeSpecieConifers"))^2)) / number_of_rows,
                                 sqrt(colSums((bo_results$outcomes_to_maximize_SD %>% dplyr::select(-"Carbon_SD_Scenario26_TreeSpecieConifers"))^2)) / number_of_rows)
                
                selectedfulltablerowvalue <- as.data.frame(matrix(c(parcels_activation,
                                                                    # CarbonMean, BioMeans, Area, VisitsMean
                                                                    col_sums[-last_col], "Area" = area_sum, col_sums[last_col],
                                                                    # CarbonSD, BioSD, VisitsSD
                                                                    col_sums_SD), nrow = 1))
                
                colnames(selectedfulltablerowvalue) <- names(SelectedMins[SelecRow, ])
                selectedvectorvalue <- selectedfulltablerowvalue[, 1:length(parcels_activation)]
                
                # SelectedFullTableRow(SelectedMins[SelecRow, ])
                # SelectedVector(SelectedMins[SelecRow, 1:length(SavedVec)])
                SelectedFullTableRow(selectedfulltablerowvalue)
                SelectedVector(selectedvectorvalue)
                
                message(current_task_id, " [INFO] Bayesian optimization finished successfully.")
                showNotification(current_task_id, " [INFO] Bayesian optimization finished successfully.")
                message(current_task_id, " [INFO] sum_carbon=", col_sums[1])
                
              }
              bayesian_optimization_finished(TRUE)
            } %...!% {
              error <- .
              msg <- paste0("task ", current_task_id, " future_promise resulted in the error: ", error)
              showNotification(paste("[ERROR]", msg))
              notif(msg, log_level = "error", global_log_level = global_log_level)
              bayesian_optimization_finished(TRUE)
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
          bayesian_optimization_extendedtask$invoke(
                seed = 1,
                FullTable = FullTable,
                area_sum_threshold = SelecTargetArea,
                outcomes_to_maximize_sum_threshold_vector = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetVisits),
                # outcomes_to_minimize_sum_threshold_vector = NULL,
                global_log_level = LOG_LEVEL,
                PLOT = FALSE,
                
                BAYESIAN_OPTIMIZATION_ITERATIONS = 5,
                # progressr_object = function(amount = 0, message = "") {},
                # progressr_object_arg = my_progressr_object,
                BAYESIAN_OPTIMIZATION_BATCH_SIZE = 1,
                PENALTY_COEFFICIENT = 1000,
                # PENALTY_COEFFICIENT = 10 * max(FullTable %>% select(contains("Mean"))),
                EXPLORATION = FALSE, # FALSE for tab 1, TRUE for tab 2
                EXPLORATION_COEFFICIENT = 0,
                
                SCALE = FALSE,
                
                preference_weight_area = preference_weight_area,
                preference_weights_maximize = mypref,
                # preference_weights_minimize = rep(1, length(c())),
                
                current_task_id = current_task_id,
                
                CUSTOM_DESIGN_POINTS_STRATEGIES = c("expected improvement", "probability of improvement"),
                DESIGN_POINTS_STRATEGY = "expected improvement",
                
                CONSTRAINED_INPUTS = TRUE,
                
                RREMBO_CONTROL = RREMBO_CONTROL,
                RREMBO_HYPER_PARAMETERS = RREMBO_HYPER_PARAMETERS,
                RREMBO_SMART = FALSE,
                
                # GP
                KERNEL = "matern2.5", # matern2.5 or sexp
                NUMBER_OF_VECCHIA_NEIGHBOURS = 20,
                
                tolvec = tolvec,
                alpha = alphaLVL
              )
            # })
        } else {
          ZeroSelected<-tmpYearType$SelectedSimMat2[1,]
          ZeroSelected<-replace(ZeroSelected,1:(length(SavedVecYearType)),-1)
          ZeroSelected<-replace(ZeroSelected,(length(SavedVecYearType)+1):(2*length(SavedVecYearType)),"C")

          ZeroSelected[1,(2*length(SavedVecYearType)+1):dim(ZeroSelected)[2]]<-0
          names(ZeroSelected)<-names(tmpYearType$SelectedSimMat2)
          SelectedFullTableRow(ZeroSelected)
          TOSAVE<-list(YEAR=as.numeric(ZeroSelected[1, 1:(length(SavedVecYearType))]),
                       TYPE=as.character(ZeroSelected[1, (length(SavedVecYearType)+1):(2*length(SavedVecYearType))])
          )
          SelectedVector(TOSAVE)
        }
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$tabs == "Preferences", {
    
    SavedVec <- ClickedVector()
    SavedVecYear <- ClickedVectorYear()
    
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
    
    if (!is.null(SavedVec)) {
      
      AreaSelected <- AreaSelected0()
      CarbonSelected <- CarbonSelected0()
      CarbonSelectedYear<-CarbonSelectedYear0()
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
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVecLoc = SavedVec,
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
                                     alphaLVL = alphaLVL,
                                     input_areaSlider_multiplicative_coefficient = FALSE,
                                     tolvec=tolvecReactive())
      
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      SelectedSimMatGlobal <<- SelectedSimMat2
      
      #PROBAMAT <- Icalc$IVEC
      #for (abc in 1:dim(Icalc$IVEC)[2]) {
      #  PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      #}
      PROBAMAT<-CalcProbaMat(Icalc$IVEC,LimitsMat,Above=AboveTargets)
      
      # CONDPROB_AtLeast1 <- (PROBAMAT[, 1] >= alphaLVL) | (PROBAMAT[, 2] >= alphaLVL) | (PROBAMAT[, 3] >= alphaLVL) | (PROBAMAT[, 4] >= alphaLVL)
      CONDPROB_AtLeast1 <- FALSE
      for (i in 1:ncol(PROBAMAT)) {
        CONDPROB_AtLeast1 <- CONDPROB_AtLeast1 | (PROBAMAT[, 1] >= alphaLVL)
      }
      
      #  datAll = data.frame(CarbonMet = 1*(PROBAMAT[, 1] >= alphaLVL),
      #                      redSquirrel = 1*(PROBAMAT[, 2] >= alphaLVL),
      #                   Area = 1*(PROBAMAT[, 3] >= alphaLVL),
      #                  Visits = 1*(PROBAMAT[, 4] >= alphaLVL))
      AtleastOneDat <- unique(SelectedSimMat2[CONDPROB_AtLeast1, ])
      
      species_data_frame <- do.call("data.frame",
                                    setNames(lapply(SPECIES, function(x) bquote(SelectedSimMat2[.(x)])),
                                             SPECIES))
      datAll = as.matrix(data.frame(Carbon = SelectedSimMat2$Carbon,
                                    # redsquirrel = SelectedSimMat2$redsquirrel,
                                    species_data_frame,
                                    Area = SelectedSimMat2$Area,
                                    Visits = SelectedSimMat2$Visits))
      datAll2 <- datAll[ConvertSample, ]
      
      # DatBinaryCode <- paste0(1*(PROBAMAT[, 1] >= alphaLVL), 1*(PROBAMAT[, 2] >= alphaLVL), 1*(PROBAMAT[, 3] >= alphaLVL), Visits = 1*(PROBAMAT[, 4] >= alphaLVL))
      DatBinaryCode <- ""
      for (i in 1:(ncol(PROBAMAT) - 1)) {
        DatBinaryCode <- paste0(DatBinaryCode, 1 * (PROBAMAT[, 1] >= alphaLVL))
      }
      DatBinaryCode <- paste0(DatBinaryCode, Visits = 1 * (PROBAMAT[, ncol(PROBAMAT)] >= alphaLVL))
      
      
      DatBinaryCode0(DatBinaryCode)
      VecNbMet <- rep(0, length(CONDPROB_AtLeast1))
      # VecNbMet[(((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)) |
      #             ((PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)) |
      #             ((PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)) |
      #             ((PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)))] <- 1
      # VecNbMet[(((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)) |
      #             ((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)) |
      #             ((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)) |
      #             ((PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)) |
      #             ((PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)) |
      #             ((PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL))
      # )] <- 2
      # VecNbMet[(((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)) |
      #             ((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)) |
      #             ((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)) |
      #             ((PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL))
      # )] <- 3
      # VecNbMet[(((PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL))
      # )] <- 4
      for (i in 1:ncol(PROBAMAT)) {
        indices_list <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = i)
        indices <- FALSE
        for (j in 1:length(indices_list)) {
          indices <- indices | indices_list[[j]]
        }
        VecNbMet[indices] <- i
      }
      
      VecNbMet0(VecNbMet)
      prior_list_temp <- list()
      # Carbon prior
      # mean = 1 / half(midpoint)
      # 2 * sd = 1 / half(midpoint)
      if (isFALSE("Carbon" %in% colnames(SelectedSimMat2))) {
        stop("Defining a prior over an outcome that doesn't exist")
      }
      prior_list_temp$Carbon <- gamma_prior(2 / max(SelectedSimMat2$Carbon),
                                            1 / max(SelectedSimMat2$Carbon))
      
      # Species priors, similarly-derived values
      for (i in 1:N_SPECIES) {
        specie <- SPECIES[i]
        if (isFALSE(specie %in% colnames(SelectedSimMat2))) {
          stop("Defining a prior over an outcome that doesn't exist")
        }
        add_to_list <- setNames(list(Normal(2 / max(SelectedSimMat2[[specie]]),
                                            1 / max(SelectedSimMat2[[specie]]))),
                                specie)
        prior_list_temp <- append(prior_list_temp, add_to_list)
      }
      
      # Area prior
      if (isFALSE("Area" %in% colnames(SelectedSimMat2))) {
        stop("Defining a prior over an outcome that doesn't exist")
      }
      prior_list_temp$Area <- gamma_prior(- 2 / max(SelectedSimMat2$Area),
                                          1 / max(SelectedSimMat2$Area))
      
      # Visits prior
      if (isFALSE("Visits" %in% colnames(SelectedSimMat2))) {
        stop("Defining a prior over an outcome that doesn't exist")
      }
      prior_list_temp$Visits <- Normal(2 / max(SelectedSimMat2$Visits),
                                       1 / max(SelectedSimMat2$Visits))
      
      # Re-order the list in accordance to TARGETS vector
      prior_list <- list()
      for (target in TARGETS) {
        prior_list[[target]] <- prior_list_temp[[target]]
      }
      
      # pref_reactive(prefObject(data = datAll2,
      #                          priors = prior_list))
      
      UniqueBinCodes <- unique(DatBinaryCode)
      if (dim(AtleastOneDat)[1] >= 250)
      {
        NbRoundsMax(MaxRounds)
        
        LinesToCompare <- matrix(1, MaxRounds, 2)
        LinesToCompare[1, ] <- sample(1:dim(datAll2)[1], 2, replace = F)
        CurrentRound(1)
        
        LinesToCompareReactive(LinesToCompare)
        SelectedLine <- list()
        # SelectedLine[[1]] <- SelectedSimMat2[ConvertSample[LinesToCompare[1, 1]], ]
        # SelectedLine[[2]] <- SelectedSimMat2[ConvertSample[LinesToCompare[1, 2]], ]
        
        # Pick 2 random strategies that meet all targets and update pref_reactive
        two_strategies_that_meet_all_targets <- pick_two_strategies_that_meet_targets_update_pref_reactive(VecNbMet0 = VecNbMet0,
                                                                                                           SelectedSimMat2 = SelectedSimMat2,
                                                                                                           pref_reactive = pref_reactive,
                                                                                                           N_TARGETS_ARG3 = N_TARGETS,
                                                                                                           TARGETS_ARG2 = TARGETS,
                                                                                                           prior_list = prior_list)
        SelectedLine[[1]] <- SelectedSimMat2[two_strategies_that_meet_all_targets[1], ]
        SelectedLine[[2]] <- SelectedSimMat2[two_strategies_that_meet_all_targets[2], ]
        
        for (aai in 1:2) {
          SwitchedOnCells <- SelectedLine[[aai]][1:length(SavedVec)]
          SelectedTreeCarbon <- SelectedLine[[aai]]$Carbon
          # SelectedBio <- SelectedLine[[aai]]$redsquirrel
          for (x in SPECIES) {
            var_name <- paste0("SelectedBio", x)
            value <- SelectedLine[[aai]][[x]]
            assign(var_name, value)
          }
          SelectedArea <- SelectedLine[[aai]]$Area
          SelectedVisits <- SelectedLine[[aai]]$Visits
          
          SelectedTreeCarbonSD <- SelectedLine[[aai]]$CarbonSD
          # SelectedBioSD <- SelectedLine[[aai]]$redsquirrelSD
          for (x in SPECIES) {
            var_name <- paste0("SelectedBioSD", x)
            value <- SelectedLine[[aai]][[paste0(x, "SD")]]
            assign(var_name, value)
          }
          SelectedVisitsSD <- SelectedLine[[aai]]$VisitsSD
          
          SELL <- (FullTable$extent == SelectedDropdown)
          if (!is.null(SELL)) {
            
            SELGEO <- FullTable$geometry[SELL]
            SELGEOFull <- FullTable[SELL, ]
            SELGEOFull$layerId <- paste0("Square", 1:dim(SELGEOFull)[1])
            
            
            
            ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units[SELL],
                                   ColorLighteningFactor(), ColorDarkeningFactor())
            
            FullColVec <- ColObtained$FullColVec
            ClickedCols <- ColObtained$ClickedCols
            SELGEOFull$color <- ColObtained$FullColVec
            SELGEOFull$color[SavedVec == 1] <- ColObtained$ClickedCols[SavedVec == 1]
            
            
            SELGEOSavedVec <- SELGEOFull[, c("geometry", "layerId")]
            SELGEOSwitched <- SELGEOFull[, c("geometry", "layerId")]
            
            SELGEORemaining <- SELGEOFull[(SavedVec == 1) | (SwitchedOnCells == 1), c("geometry", "layerId", "color")]
            
            
            SELGEOSavedVec <- SELGEOSavedVec[SavedVec == 1, ]
            SELGEOSwitched <- SELGEOSwitched[(SwitchedOnCells == 1) & (SavedVec != 1), ]
            
            if (dim(SELGEORemaining)[1] > 0) {
              listMaps[[aai]] <- addPolygons(listMaps[[aai]], data = SELGEORemaining, color = SELGEORemaining$color, layerId = ~SELGEORemaining$layerId, weight = UnitPolygonColours)
            }
            
            
          }
          
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- get_ugly_specie(SPECIES[i], NAME_CONVERSION)
            specie_english <- if (specie_latin == "All") "All Species Richness" else get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
            selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
            selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
            addControlText <- paste0(addControlText, get_pretty_english_specie(specie_english, NAME_CONVERSION), ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
          }
          
          listMaps[[aai]] <- listMaps[[aai]] %>%
            addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2*SelectedTreeCarbonSD, 2), "<br>",
                                     # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
                                     addControlText,
                                     "Area Planted: ", round(SelectedArea, 2), "<br>",
                                     "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2*SelectedVisitsSD, 2),
                                     "</p>"), position = "topright")
          
        }
        
        shinyjs::enable("choose1")
        shinyjs::enable("choose2")
        
      } else {
        listMaps[[1]] <- listMaps[[1]] %>%
          addControl(html = paste0("<p> Elicitation Not Possible as there are not enough samples that meet some of the targets
                                              </p>"), position = "topright")
        listMaps[[2]] <- listMaps[[2]] %>%
          addControl(html = paste0("<p> Elicitation Not Possible as there are not enough samples that meet some of the targets
                                              </p>"), position = "topright")
        shinyjs::disable("choose1")
        shinyjs::disable("choose2")
      }
      
    }
    
    listMaps <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, listMaps = listMaps)
    
    output$ClusterPage <- renderLeaflet({listMaps[[1]]})
    output$ClusterPage2 <- renderLeaflet({listMaps[[2]]})
    
    
  })
  # DONE TO CHANGE LATER
  observeEvent(input$choose1, {
    observe_event_function(choose = 1, # 1 for input$choose1, 2 for input$choose2
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
                           ClickedVectorYear=ClickedVectorYear)
  })
  # DONE TO CHANGE LATER
  observeEvent(input$choose2, {
    observe_event_function(choose = 2, # 1 for input$choose1, 2 for input$choose2
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
                           ClickedVectorYear=ClickedVectorYear)
  })
  
  
  
  # TO CHECK 1
  observeEvent({input$map_shape_click}, {
    click <- input$map_shape_click
    SelectedDropdown <- input$inSelect
    SelectedRowsUnits <- FullTable$units[FullTable$extent == SelectedDropdown]
    
    
    #GEOVEC <- st_geometry_type(FullTable$geometry)
    #browser()
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
          
          if(SavedVecYear[iii]==29){SavedVecYear[iii] <- input$YearSelect;}else{
            SavedVecYear[iii]<-ifelse(SavedVecYear[iii]>= input$YearSelect, 29, input$YearSelect)}
          #If there has been a changed in SavedVec, then updated the list containing the override of simul636
          
          
          if(SavedVecYearPriorToChange[iii]!=SavedVecYear[iii]){
            if(SavedVecYear[iii]==29){SAMPLELIST[iii]<-list(NULL)}else{
              if(SavedVecYear[iii]==28){#browser()
                SAMPLELIST[[iii]]<-rep(29,dim(simul636Year)[1])}else{
                  SAMPLELIST[[ iii]]<- sample((SavedVecYear[ iii]+1):29,dim(simul636Year)[1], replace=T)
                  
                }
            }
            
          }
      #### TO CHANGE: there is a problem here.
        #  browser()
          if(SavedVecYearType[iii]==(-1)){SavedVecYearType[iii] <- input$YearSelect;}else{
            SavedVecYearType[iii]<-ifelse(SavedVecYearType[iii]>= input$YearSelect, (-1), input$YearSelect)}

          if(SavedVecYearTypePriorToChange[iii]!=SavedVecYearType[iii]){
            if(SavedVecYearType[iii]==(-1)){SAMPLELIST_TYPE[iii]<-list(NULL)}else{
              if(SavedVecYearType[iii]==MAXYEAR){#browser()
                  
                SAMPLELIST_TYPE[[iii]]<-list()
                SAMPLELIST_TYPE[[iii]][["YEAR"]]<-rep(-1,dim(simul636YearType$YEAR)[1])
                SAMPLELIST_TYPE[[iii]][["TYPE"]]<-rep("C",dim(simul636YearType$YEAR)[1])
                }else{
              #    browser()
                  SAMPLELIST_TYPE[[iii]]<-list()
                  SAMPLELIST_TYPE[[iii]][["YEAR"]]<-simul636YearType$YEAR[,iii]
                  SAMPLELIST_TYPE[[iii]][["TYPE"]]<-simul636YearType$TYPE[,iii]
                  
                  ALREADYC<-(SAMPLELIST_TYPE[[iii]]$TYPE!="C")
                  
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
      #browser()
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
      map <- calcBaseMap$map
      
      
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
            map <- addPolygons(map, data = SELGEORemaining, color = SELGEORemaining$color, layerId = ~SELGEORemaining$layerId, weight = UnitPolygonColours)
          }
          
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
            selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
            selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
            if (SPECIES[i] == "All") {
              specie_english <- "All species"
            }
            addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
          }
          
          map <- map %>%
            addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2*SelectedTreeCarbonSD, 2), "<br>",
                                     # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
                                     addControlText,
                                     "Area Planted: ", round(SelectedArea, 2), "<br>",
                                     "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2*SelectedVisitsSD, 2),
                                     "</p>"), position = "topright",layerId="legend")
        }
        #} else { map <- map %>%
        #  addControl(html = paste0("<p> Targets Cannot be met</p>"), position = "topright")
        #}
      }
      map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
      CreatedBaseMap(1)
      MapReactive(map)
      MapReactive()}else{    MapReactive()
      }
    # ChangeSliders(FALSE)
    # shinyjs::show("tabs")
    
  })
  
  # DONE TO CHANGE LATER
  output$map2 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
  })
  # DONE TO CHANGE LATER
  
  output$map3 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
  })
  # DONE TO CHANGE LATER
  
  output$map4 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
    
  })
  # DONE TO CHANGE LATER
  
  output$map5 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
  })
}

shinyApp(ui, server)
