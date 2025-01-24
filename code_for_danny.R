# ALREADY DONE IN THE APP (begin)

# Load function files
LOG_LEVEL <- "error"
FolderSource <- normalizePath(getwd())
if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
  FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
}
set.seed(1)

STARTYEAR<-2025
MAXYEAR<-2050-STARTYEAR-1

# Delete log and lock files
unlink(base::normalizePath(file.path(FolderSource, "log*"), mustWork = FALSE))
unlink(base::normalizePath(file.path(FolderSource, "*lockfile*"), mustWork = FALSE))
unlink(base::normalizePath(file.path(FolderSource, "task_id*"), mustWork = FALSE))

# Overridden in server() block, necessary for source(...)
SESSION_FILE_SUFFIX <- ""
source(normalizePath(file.path(FolderSource, "functions.R")))
source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")))
source(normalizePath(file.path(FolderSource, "preferTrees.R")))

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
    devtools::install_github('mingdeyu/dgpsi-R', upgrade = "ask", verbose = FALSE)
    library("dgpsi")
    dgpsi::init_py()
  }
  if (!require(RRembo)) {
    # devtools on Linux requires testthat and pkgload (https://stackoverflow.com/questions/61643552/r-devtools-unable-to-install-ubuntu-20-04-package-or-namespace-load-failed-f)
    install_and_load_packages("testthat", verbose = FALSE)
    install_and_load_packages("pkgload", verbose = FALSE)
    install_and_load_packages("devtools", verbose = FALSE)
    devtools::install_github('mbinois/RRembo', upgrade = "ask", verbose = FALSE)
    library("RRembo")
  }
  
  install_and_load_packages(packages = packages, update = FALSE)
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

# Load FullTable from file
FullTable <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
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
# ALREADY DONE IN THE APP (end)

DoE_low_dimension <- lhs::randomLHS(10, RREMBO_HYPER_PARAMETERS$d)

A <- RREMBO_HYPER_PARAMETERS$A
DoE_high_dimension <- RRembo_project_low_dimension_to_high_dimension_basic(DoE_low_dimension = DoE_low_dimension, A = A)

# ALREADY DONE IN THE APP (begin)

# Decide outcomes
if (isFALSE(exists("outcomes"))) {
  message(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))))
  while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
    Sys.sleep(5)
  }
  message(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file..."))
  
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
}
# Decide tree specie and scenario
SCENARIO <- 26

# Slider thresholds

## input$AreaSlider
area_sum_threshold <- 15

## input$SliderMain
## lapply(paste0("input$BioSlider", SPECIES), get)
## input$VisitsSlider
outcomes_to_maximize_sum_threshold_vector <- c("Carbon" = 20,
                                               do.call(c, setNames(lapply(SPECIES, function(x) {rnorm(1, 100, 100)}), SPECIES)),
                                               "Visits" = 10)

# The threshold from tab 1 (with values in -1:MAXYEAR) corresponds to the end of the year until which planting is forbidden
# i.e. -1 means we can plant from year 0, ..., MAXYEAR (24) means we cannot plant i.e. find column with year MAXYEAR+1 (25)
# In order to sample effectively, we don't sample the years we cannot plant during
## ClickedVector()
year_of_max_no_planting_threshold_vector <- sample(x = -1:MAXYEAR,
                                                   size = nrow(FullTable),
                                                   replace = TRUE)

DoE_high_dimension_categorical <- transform_DoE_high_dimension_continuous_to_strategy_rowwise_matrix(DoE_high_dimension_rowwise_matrix = DoE_high_dimension,
                                                                                                     RREMBO_HYPER_PARAMETERS = RREMBO_HYPER_PARAMETERS,
                                                                                                     FullTable_arg = FullTable,
                                                                                                     MAXYEAR_arg = MAXYEAR,
                                                                                                     SPECIES_arg = SPECIES,
                                                                                                     # typically ClickedVector()
                                                                                                     year_of_max_no_planting_threshold_vector = year_of_max_no_planting_threshold_vector)

FullTable_long <- transform_FullTable_wide_to_long(FullTable_arg = FullTable,
                                                   SCENARIO_arg = SCENARIO,
                                                   MAXYEAR_arg = MAXYEAR,
                                                   verbose = FALSE)
# ALREADY DONE IN THE APP (end)

# Look at a single strategy (pick any, 1 is for the example)
parameter_vector <- inputs <- DoE_high_dimension_categorical[1, ]


# tictoc::tic()
# a <- get_outcomes_from_strategy(DoE_high_dimension_categorical[1, ],
#                                 FullTable_long_arg = FullTable_long)
# tictoc::toc()
# 
# tictoc::tic()
# a <- apply(DoE_high_dimension_categorical[1:10, ],
#            1,
#            get_outcomes_from_strategy,
#            FullTable_long_arg = FullTable_long)
# tictoc::toc()

alpha <- 0.99
group_size <- nrow(A) / 3
area_names <- paste0("area_parcel_", 1:group_size)
plantingyear_names <- paste0("plantingyear_parcel_", 1:group_size)
treespecie_names <- paste0("treespecie_parcel_", 1:group_size)

Implausibility <- function(x, targetLevel = -sqrt(alpha/(1-alpha)),
                           MAXYEAR_arg = MAXYEAR,
                           SPECIES_arg = SPECIES,
                           SCENARIO_arg = SCENARIO,
                           FullTable_long_arg = FullTable_long,
                           area_sum_threshold_numeric = area_sum_threshold,
                           year_of_max_no_planting_threshold_vector_arg = year_of_max_no_planting_threshold_vector,
                           RREMBO_HYPER_PARAMETERS_arg = RREMBO_HYPER_PARAMETERS) {
  
  FullTable_long <- FullTable_long_arg
  MAXYEAR <- MAXYEAR_arg
  SPECIES <- SPECIES_arg
  SCENARIO <- SCENARIO_arg
  area_sum_threshold <- area_sum_threshold_numeric
  year_of_max_no_planting_threshold_vector <- year_of_max_no_planting_threshold_vector_arg
  RREMBO_HYPER_PARAMETERS <- RREMBO_HYPER_PARAMETERS_arg
  A <- RREMBO_HYPER_PARAMETERS$A
  
  year_of_planting_min_threshold_vector <- year_of_max_no_planting_threshold_vector + 1
  
  #strategy in high-dim continuous space
  x <- matrix(x, ncol=length(x))
  strategy_cont <- RRembo_project_low_dimension_to_high_dimension_basic(DoE_low_dimension = x, A = A)
  # group_size <- ncol(strategy_cont) / 3
  # # Area
  # strategy_area <- continuous_to_multi_categorical(values = strategy_cont[, 1:group_size, drop=F],
  #                                                  legal_values_ordered = area_possible_values_dataframe)
  # # Year of planting
  # strategy_year <- continuous_to_multi_categorical(values = strategy_cont[, (group_size+1):(2*group_size), drop=F],
  #                                                  legal_values_ordered = years_possible_values_dataframe)
  # # Tree specie
  # strategy_treespecie <- continuous_to_multi_categorical(values = strategy_cont[, (2*group_size+1):(3*group_size),drop=F],
  #                                                        legal_values_ordered = tree_specie_possible_values_dataframe)
  # colnames(strategy_area) <- area_names
  # colnames(strategy_year) <- plantingyear_names
  # colnames(strategy_treespecie) <- treespecie_names
  #
  # strategy <- cbind(strategy_area, strategy_year, strategy_treespecie)

  strategy <- transform_DoE_high_dimension_continuous_to_strategy_rowwise_matrix(DoE_high_dimension_rowwise_matrix = strategy_cont,
                                                                                 RREMBO_HYPER_PARAMETERS_arg = RREMBO_HYPER_PARAMETERS,
                                                                                 FullTable_arg = FullTable,
                                                                                 MAXYEAR_arg = MAXYEAR,
                                                                                 SPECIES_arg = SPECIES,
                                                                                 year_of_max_no_planting_threshold_vector = year_of_max_no_planting_threshold_vector)

  #Assumption that year_of_planting_min_threshold_vector has meaning of 0 = no restriction, 1 = cant plant in year 0 can in year 1, 2 = cant plant in years 0 or 1, can in 2 etc
  strategy_year <- as.numeric(strategy[, grep("plantingyear", colnames(strategy))])
  bad_years <- strategy_year < year_of_planting_min_threshold_vector
  if(any(bad_years)){
    #planting in prohibited year
    diff <- year_of_planting_min_threshold_vector[bad_years]-strategy_year[bad_years]
    return(sum(diff))
  }
  outcomes <- get_outcomes_from_strategy(parameter_vector=strategy[1,],
                                         FullTable_long_arg = FullTable_long)
  if(outcomes$sum_area > area_sum_threshold){
    return((outcomes$sum_area-area_sum_threshold)/0.001)
  }
  # Pre-fetch thresholds and SDs for carbon and visits
  thresholds <- outcomes_to_maximize_sum_threshold_vector[c("Carbon", "Visits")]
  sds_carbon_visits <- c(outcomes$sum_carbon_sd, outcomes$sum_visits_sd)
  val_carbon_visits <- c(outcomes$sum_carbon, outcomes$sum_visits)
  implausibilities_carbon_visits <- (thresholds - val_carbon_visits) / sds_carbon_visits
  
  #richness has no sd, so divide by very small number for now (it will have sd)
  imp_richness <- (outcomes_to_maximize_sum_threshold_vector[names(outcomes_to_maximize_sum_threshold_vector) %in% c(NAME_CONVERSION$Group, "All")] - outcomes$sum_richness)/(.01)
  implausibilities <- c(implausibilities_carbon_visits, imp_richness)
  #biodiversity
  if(!is.null(outcomes$sum_biodiversity)){
    #handle species names
    imp_bio <- (outcomes_to_maximize_sum_threshold_vector[names(outcomes_to_maximize_sum_threshold_vector) %in% NAME_CONVERSION$Specie] - outcomes$sum_biodiversity)/(outcomes$sum_biodiversity_sd)
    implausibilities <- c(implausibilities, imp_bio)
  }
  return(max(implausibilities))
}

#debug(Implausibility)

profvis({Implausibility(x=rep(1,6))})

tictoc::tic()
Implausibility(x=rep(1,6))
tictoc::toc()

# microbenchmark::microbenchmark(Implausibility(x=rep(1,6)))

LogDens <- function(x, ThisLevels, FinalLevels, BoxLimits){
  timp <- Implausibility(x, FinalLevels)
  ifelse(all(timp<=ThisLevels), 1, -Inf)
}

print("Full PTMCMC-Slice Started")
tictoc::tic()
print("Find target compatible space")
#profvis({
EmbeddingSamples <- ImplausibilitySampler(Implausibility=Implausibility, dims=6, method="slice", targetLevels=-sqrt(alpha/(1-alpha)),
                                          control.list=list(num.mutations=10, num.iterations=10, BoxLimits=cbind(rep(0,6),rep(1,6)),
                                                            debug.mode=FALSE))
#})
print("Generate 100 uniform samples")
tSams <- SampleSpace(MySampler2tiny,control.list=list(debug.mode=TRUE), niter = 100)
tictoc::toc()

