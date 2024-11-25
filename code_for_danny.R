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
    install_and_load_packages("testthat", quiet = TRUE)
    install_and_load_packages("pkgload", quiet = TRUE)
    install_and_load_packages("devtools", quiet = TRUE)
    devtools::install_github('mingdeyu/dgpsi-R', upgrade = "ask", quiet = TRUE)
    library("dgpsi")
    dgpsi::init_py()
  }
  if (!require(RRembo)) {
    # devtools on Linux requires testthat and pkgload (https://stackoverflow.com/questions/61643552/r-devtools-unable-to-install-ubuntu-20-04-package-or-namespace-load-failed-f)
    install_and_load_packages("testthat", quiet = TRUE)
    install_and_load_packages("pkgload", quiet = TRUE)
    install_and_load_packages("devtools", quiet = TRUE)
    devtools::install_github('mbinois/RRembo', upgrade = "ask", quiet = TRUE)
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


handlers(global = TRUE)

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
                                           global_log_level = LOG_LEVEL)
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
TREE_SPECIE <- "Conifers"

# Slider thresholds
area_sum_threshold <- 15
outcomes_to_maximize_sum_threshold_vector <- c("Carbon" = 20,
                                               do.call(c, setNames(lapply(SPECIES, function(x) {runif(1)}), SPECIES)),
                                               "Visits" = 10)
# The threshold from tab 1 (with values in -1:MAXYEAR) corresponds to the end of the year until which planting is forbidden
# i.e. -1 means we can plant from year 0, ..., MAXYEAR (24) means we cannot plant i.e. find column with year MAXYEAR+1 (25)
# In order to sample effectively, we don't sample the years we cannot plant during
year_of_max_no_planting_threshold_vector <- sample(x = -1:MAXYEAR,
                                                   size = nrow(FullTable),
                                                   replace = TRUE)
year_of_planting_min_threshold_vector <- 

# Possible values for area, and tree specie (years later)
area_possible_non_zero_values <- FullTable %>%
  sf::st_drop_geometry() %>%
  dplyr::select(area) %>%
  unlist(use.names = FALSE)
area_possible_values_dataframe <- rbind(0, area_possible_non_zero_values)
rownames(area_possible_values_dataframe) <- NULL

# years_possible_values_dataframe <- 0:MAXYEAR
# years_possible_values_dataframe <- matrix(years_possible_values_dataframe,
#                                           nrow = length(years_possible_values_dataframe),
#                                           ncol = ncol(area_possible_values_dataframe))

tree_specie_possible_values_dataframe <- FullTable %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  grep(pattern = "TreeSpecie", x = ., value = TRUE) %>%
  gsub(pattern = ".*TreeSpecie(.*?)_.*", x = ., replacement = "\\1", perl = TRUE) %>%
  gsub(pattern = ".*TreeSpecie(.*)", x = ., replacement = "\\1", perl = TRUE) %>%
  unique()
tree_specie_possible_values_dataframe <- matrix(tree_specie_possible_values_dataframe,
                                                nrow = length(tree_specie_possible_values_dataframe),
                                                ncol = ncol(area_possible_values_dataframe))

# Turn area to categorical values
group_size <- ncol(DoE_high_dimension) / 3
indices <- 1:group_size
DoE_high_dimension_categorical_area <- continuous_to_multi_categorical(values = DoE_high_dimension[, indices],
                                                                       legal_values_ordered = area_possible_values_dataframe)
colnames(DoE_high_dimension_categorical_area) <- paste0("area_parcel_id", 1:ncol(DoE_high_dimension_categorical_area))

# Possible values for year, and turn to categorical values
DoE_high_dimension_categorical_year <- matrix(NA,
                                              nrow = length(tree_specie_possible_values_dataframe),
                                              ncol = ncol(area_possible_values_dataframe))
indices <- group_size + indices
## Per parcel, uniformly sample over the allowed planting years (from minimum_specified_in_strategy to MAXYEAR)
for (i in indices) {
  # Loop over parcels
  parcel_idx <- i - min(indices) + 1
  can_we_plant_vector <- (as.numeric(DoE_high_dimension_categorical_area[, parcel_idx]) == 0)
  
  years_possible_values_dataframe <- sapply(1:nrow(DoE_high_dimension_categorical_area), function(j) {
    if (isTRUE(can_we_plant_vector[j])) {
      return(sample(x = year_of_planting_min_threshold_vector[j]:MAXYEAR, size = 1))
    } else {
      return(invisible())
    }
  }) |>
    unlist() |>
    cbind()
  
  DoE_high_dimension_categorical_year[, parcel_idx] <- continuous_to_multi_categorical(values = DoE_high_dimension[, i, drop = FALSE],
                                                                                       legal_values_ordered = years_possible_values_dataframe)
  
}

DoE_high_dimension_categorical_year <- continuous_to_multi_categorical(values = DoE_high_dimension[, indices],
                                                                       legal_values_ordered = years_possible_values_dataframe)
colnames(DoE_high_dimension_categorical_year) <- paste0("plantingyear_parcel_id", 1:ncol(DoE_high_dimension_categorical_year))

# Turn tree specie to categorical values
indices <- group_size + indices
DoE_high_dimension_categorical_treespecie <- continuous_to_multi_categorical(values = DoE_high_dimension[, indices],
                                                                             legal_values_ordered = tree_specie_possible_values_dataframe)
colnames(DoE_high_dimension_categorical_treespecie) <- paste0("treespecie_parcel_id", 1:ncol(DoE_high_dimension_categorical_treespecie))

DoE_high_dimension_categorical <- cbind(DoE_high_dimension_categorical_area,
                                        DoE_high_dimension_categorical_year,
                                        DoE_high_dimension_categorical_treespecie)
# ALREADY DONE IN THE APP (end)

# Look at a single strategy (pick any, 1 is for the example)
parameter_vector <- DoE_high_dimension_categorical[1, ]

get_outcomes_from_strategy <- function(parameter_vector, FullTable_arg = FullTable, SCENARIO_ARG = SCENARIO, MAXYEAR_ARG = MAXYEAR) {
  SCENARIO <- SCENARIO_ARG
  FullTable <- FullTable_arg
  MAXYEAR <- MAXYEAR_ARG
  
  
  # Prepare data once to avoid re-doing it everywhere ----
  FullTable <- FullTable %>%
    sf::st_drop_geometry() %>%
    dplyr::select(!contains("Bio_SD"), -extent, -x, -y)
  
  
  
  
  # Area ----
  group_size <- length(parameter_vector) / 3
  indices <- 1:group_size
  area_vector <- as.numeric(parameter_vector[indices])
  indices <- indices + group_size
  year_vector <- as.numeric(parameter_vector[indices])
  indices <- indices + group_size
  treespecie_vector <- parameter_vector[indices]
  
  FullTable <- FullTable %>%
    mutate(area = area_vector,
           year = year_vector,
           treespecie = treespecie_vector)
  
  # Calculate outcomes (sumCarbon, sumArea, sumBiodiversity, sumVisits)
  
  
  
  # Carbon ----
  small_fulltable <- FullTable %>%
    dplyr::select(area, year, treespecie,
                    (
                      contains("Carbon") &
                        contains(c("Mean", "SD")) &
                        contains(paste0("Scenario", SCENARIO)) &
                        contains("PlantingYear")
                    ))
  
  # sum_carbon <- small_fulltable %>%
  #   dplyr::select(contains(c("area", "year", "treespecie", "Mean"), ignore.case = FALSE)) %>%
  #   rowwise() %>%
  #   mutate(
  #     # Check if area is 0 and ignore the row by setting sum to 0 if true
  #     # Also set the value to 0 if the year of planting is less than the minimum specified in year_vector
  #     # or if the tree specie is the wrong one
  #     final_value = ifelse(area == 0,
  #                          0,
  #                          across(contains("PlantingYear"),
  #                                 ~ ifelse(
  #                                   # Correct planting year
  #                                   (as.numeric(sub(".*PlantingYear([0-9]+).*",
  #                                                   "\\1",
  #                                                   cur_column())) == year) &
  #                                     # Correct tree specie
  #                                     (sub(".*TreeSpecie(.*?)_PlantingYear.*",
  #                                          "\\1",
  #                                          cur_column()) == treespecie)
  #                                   ,
  #                                   .,
  #                                   0)
  #                          ) %>%
  #                            sum())
  #   ) %>%
  #   ungroup() %>%
  #   dplyr::select(final_value) %>%
  #   sum()
  
  small_fulltable_dt <- small_fulltable %>%
    dplyr::select(area, year, treespecie, contains("Mean")) |>
    setDT()
  
  melted_dt <- data.table::melt(small_fulltable_dt,
                                id.vars = c("area", "year", "treespecie"),
                                measure.vars = patterns("PlantingYear"),
                                variable.name = "carbon_column",
                                value.name = "carbon_value")
  
  # Extract planting year and tree specie from the column names
  melted_dt[, col_planting_year := as.numeric(sub(".*PlantingYear([0-9]+).*", "\\1", carbon_column))]
  melted_dt[, col_treespecie := sub(".*TreeSpecie(.*?)_PlantingYear.*", "\\1", carbon_column)]
  
  # Apply the conditions and calculate the sum
  sum_carbon <- melted_dt[
    # Filter rows where area is non-zero and conditions on planting year and species match
    area != 0 & col_planting_year == year & col_treespecie == treespecie,
    sum(carbon_value, na.rm = TRUE),  # Summing the matching carbon values
    by = .(area, year, treespecie)    # Summing for each combination of area, year, and treespecie
  ]$V1 |> sum()  # Extract the result directly
  
  
  
  
  
  # Carbon SD (similar to above) ----
  small_fulltable_dt <- small_fulltable %>%
    dplyr::select(area, year, treespecie, contains("SD")) |>
    setDT()
  
  melted_dt <- data.table::melt(small_fulltable_dt,
                                id.vars = c("area", "year", "treespecie"),
                                measure.vars = patterns("PlantingYear"),
                                variable.name = "carbon_column",
                                value.name = "carbon_value")
  
  # Extract planting year and tree specie from the column names
  melted_dt[, col_planting_year := as.numeric(sub(".*PlantingYear([0-9]+).*", "\\1", carbon_column))]
  melted_dt[, col_treespecie := sub(".*TreeSpecie(.*?)_PlantingYear.*", "\\1", carbon_column)]
  
  # Apply the conditions and calculate the sum
  sum_carbon_sd <- melted_dt[
    # Filter rows where area is non-zero and conditions on planting year and species match
    area != 0 & col_planting_year == year & col_treespecie == treespecie,
    sum(carbon_value, na.rm = TRUE),  # Summing the matching carbon values
    by = .(area, year, treespecie)    # Summing for each combination of area, year, and treespecie
  ]$V1 # Extract the result directly
  sum_carbon_sd <- sqrt(sum(sum_carbon_sd^2))
  
  
  
  
  
  # Visits ----
  sum_visits <- sum(FullTable[area_vector != 0, "VisitsMean"])
  sum_visits_sd <- sqrt(sum(FullTable[area_vector != 0, "VisitsMean"]^2))
  
  
  
  
  # Biodiversity ----
  species_indices <- which(SPECIES %in% NAME_CONVERSION$Specie)
  if (length(species_indices) == 0) {
    sum_biodiversity <- sum_biodiversity_sd <- c()
  } else {
    species_pattern <- paste0(".*Specie(", paste0(SPECIES[species_indices], collapse = "|"), ").*")
    
    small_fulltable <- FullTable %>%
      dplyr::select(area, year, treespecie,
                    (
                      matches(species_pattern) &
                        contains("Bio_Mean") &
                        contains(paste0("Scenario", SCENARIO))
                    ))
    
    # biodiversity_planting <- small_fulltable %>%
    #   dplyr::select(area, year, treespecie, contains("_Planting")) %>%
    #   rowwise() %>%
    #   mutate(
    #     # Check if area is 0 and ignore the row by setting sum to 0 if true
    #     # Also set the value to 0 if the tree specie is the wrong one
    #     final_value = ifelse(area == 0,
    #                          0,
    #                          across(contains("Bio_Mean"),
    #                                 ~ ifelse(
    #                                   # Correct tree specie
    #                                   sub(".*TreeSpecie(.*?)_.*",
    #                                       "\\1",
    #                                       cur_column()) == treespecie,
    #                                   .,
    #                                   0)
    #                          ) %>%
    #                            sum())
    #   ) %>%
    #   ungroup() %>%
    #   dplyr::select(final_value)
    
    
    
    
    
    ## Biodiversity per specie per parcel, when planting ----
    small_fulltable_dt <- small_fulltable %>%
      dplyr::select(area, year, treespecie, contains("_Planting")) %>%
      mutate(parcel_id = 1:nrow(FullTable)) |>
      setDT()
    
    # Melt the data table to convert from wide to long format
    melted_dt <- melt(small_fulltable_dt, 
                      id.vars = c("area", "year", "treespecie", "parcel_id"), 
                      measure.vars = grep("_Planting", colnames(small_fulltable_dt), value = TRUE), 
                      variable.name = "column_name", 
                      value.name = "biodiversity")
    
    # Extract the species name from the column names (e.g., 'Alauda_arvensis', 'Carex_magellanica')
    melted_dt <- melted_dt[, colname_specie := gsub(".*BioSpecie(.*?)_Scenario.*", "\\1", column_name)]
    # Extract the column name's tree specie
    melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*?)_.*", "\\1", column_name)]
    
    # Remove rows where the column_name's tree specie is not the strategy treespecie
    melted_dt <- melted_dt[colname_treespecie == treespecie, ]
    
    biodiversity_planting <- dcast(melted_dt, parcel_id ~ colname_specie, value.var = "biodiversity")
    
    # Re-order by parcel_id
    biodiversity_planting <- setorder(biodiversity_planting, parcel_id)
    # Remove parcel_id
    biodiversity_planting[, parcel_id := NULL]
    
    
    
    
    ## Biodiversity per specie per parcel, when NOT planting ----
    small_fulltable_dt <- small_fulltable %>%
      dplyr::select(area, year, treespecie, contains("_NoPlanting")) %>%
      mutate(parcel_id = 1:nrow(FullTable)) |>
      setDT()
    
    # Melt the data table to convert from wide to long format
    melted_dt <- melt(small_fulltable_dt, 
                      id.vars = c("area", "year", "treespecie", "parcel_id"), 
                      measure.vars = grep("_NoPlanting", colnames(small_fulltable_dt), value = TRUE), 
                      variable.name = "column_name", 
                      value.name = "biodiversity")
    
    # Extract the species name from the column names (e.g., 'Alauda_arvensis', 'Carex_magellanica')
    melted_dt <- melted_dt[, colname_specie := gsub(".*BioSpecie(.*?)_Scenario.*", "\\1", column_name)]
    # Extract the column name's tree specie
    melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*?)_.*", "\\1", column_name)]
    
    # Remove rows where the column_name's tree specie is not the strategy treespecie
    melted_dt <- melted_dt[colname_treespecie == treespecie, ]
    
    # Re-order by parcel_id
    melted_dt <- setorder(melted_dt, parcel_id)
    
    biodiversity_no_planting <- dcast(melted_dt, parcel_id ~ colname_specie, value.var = "biodiversity")
    
    # Re-order by parcel_id
    biodiversity_no_planting <- setorder(biodiversity_no_planting, parcel_id)
    # Remove parcel_id
    biodiversity_no_planting[, parcel_id := NULL]
    
    
    
    
    # Biodiversity total ----
    sum_biodiversity <- get_regressed_biodiversity_change(biodiversity_planting = biodiversity_planting,
                                                          biodiversity_no_planting = biodiversity_no_planting,
                                                          MAXYEAR = MAXYEAR,
                                                          year_of_planting_from_0 = year_vector) |>
      colSums()
    
    # Biodiversity SD
    sum_biodiversity_sd <- 0
    sum_biodiversity_sd <- sqrt(sum(sum_biodiversity_sd^2))
  }
  
  
  
  
  # Richness ----
  # For now, the code below only works for the 2 tree species Conifers and Deciduous. We will have a new biodiversity model later on, so it's not important
  richness <- c()
  
  # # apply_biodiversity_logic <- function(specie, SCENARIO, all_biodiversity_planting, all_biodiversity_no_planting) {
  # #   
  # #   # Create column names for both the 'Planting' and 'NoPlanting' cases
  # #   conifer_col_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_Planting")
  # #   deciduous_col_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_Planting")
  # #   new_col_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_Planting")
  # #   
  # #   conifer_col_no_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_NoPlanting")
  # #   deciduous_col_no_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_NoPlanting")
  # #   new_col_no_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_NoPlanting")
  # #   
  # #   # Apply the logic for 'Planting'
  # #   all_biodiversity_planting[, (new_col_planting) := fifelse(treespecie == "Conifers",
  # #                                                             get(conifer_col_planting),
  # #                                                             get(deciduous_col_planting))]
  # #   
  # #   # Apply the logic for 'NoPlanting'
  # #   all_biodiversity_no_planting[, (new_col_no_planting) := fifelse(treespecie == "Conifers",
  # #                                                                   get(conifer_col_no_planting),
  # #                                                                   get(deciduous_col_no_planting))]
  # #   
  # #   # Remove the original columns after applying the logic
  # #   all_biodiversity_planting[, c(conifer_col_planting, deciduous_col_planting) := NULL]
  # #   all_biodiversity_no_planting[, c(conifer_col_no_planting, deciduous_col_no_planting) := NULL]
  # #   
  # #   # Return the updated data.tables
  # #   return(list(all_biodiversity_planting = all_biodiversity_planting,
  # #               all_biodiversity_no_planting = all_biodiversity_no_planting))
  # # }
  # 
  # small_fulltable <- FullTable %>%
  #   dplyr::select(area, year, treespecie,
  #                 contains("Bio_Mean") &
  #                   contains(paste0("Scenario", SCENARIO))
  #   ) %>%
  #   mutate(parcel_id = 1:nrow(FullTable))
  # for (group in SPECIES) {
  #   if (group == "All" || group %in% NAME_CONVERSION$Group) {
  #     
  #     if (group == "All") {
  #       species_in_group <- NAME_CONVERSION$Specie
  #     } else {
  #       species_in_group <- NAME_CONVERSION$Specie[NAME_CONVERSION$Group == group]
  #     }
  #     
  #     species_pattern <- paste0(".*BioSpecie(", paste0(species_in_group, collapse = "|"), ").*")
  #     
  #     # Table with biodiversity values per parcel where we plant the correct tree specie
  #     all_biodiversity_planting <- small_fulltable %>%
  #       dplyr::select(parcel_id, treespecie,
  #                     contains("_Planting") &
  #                       matches(species_pattern)) %>%
  #       setDT()
  #     
  #     # Table with biodiversity values per parcels where we don't plant the correct tree specie
  #     all_biodiversity_no_planting <- small_fulltable %>%
  #       dplyr::select(parcel_id, treespecie, 
  #                     contains("_NoPlanting") &
  #                       matches(species_pattern)) %>%
  #       setDT()
  #     
  #     # Create a new column with value equal to the correct tree specie
  #     for (specie in species_in_group) {
  #       # When planting
  #       conifer_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_Planting")
  #       deciduous_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_Planting")
  #       new_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_Planting")
  #       
  #       all_biodiversity_planting[, (new_col) := fifelse(treespecie == "Conifers",
  #                                                        get(conifer_col),
  #                                                        get(deciduous_col))]
  #       all_biodiversity_planting[, c(conifer_col, deciduous_col) := NULL]
  #       
  #       # When not planting
  #       conifer_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_NoPlanting")
  #       deciduous_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_NoPlanting")
  #       new_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_NoPlanting")
  #       
  #       all_biodiversity_no_planting[, (new_col) := fifelse(treespecie == "Conifers",
  #                                                           get(conifer_col),
  #                                                           get(deciduous_col))]
  #       all_biodiversity_no_planting[, c(conifer_col, deciduous_col) := NULL]
  #     }
  #     
  #     # Re-order by parcel_id
  #     all_biodiversity_planting <- setorder(all_biodiversity_planting, parcel_id)
  #     all_biodiversity_no_planting <- setorder(all_biodiversity_no_planting, parcel_id)
  #     
  #     # Remove useless columns
  #     all_biodiversity_planting <- all_biodiversity_planting[, c("parcel_id", "treespecie") := NULL]
  #     all_biodiversity_no_planting <- all_biodiversity_no_planting[, c("parcel_id", "treespecie"):= NULL]
  # 
  #     # # Apply the function over all species in the group and get the updated data.tables
  #     # result <- lapply(species_in_group, apply_biodiversity_logic, SCENARIO = SCENARIO,
  #     #                  all_biodiversity_planting = all_biodiversity_planting,
  #     #                  all_biodiversity_no_planting = all_biodiversity_no_planting)
  # 
  #     # Extract updated results from the list
  #     # all_biodiversity_planting <- result[[length(result)]]$all_biodiversity_planting
  #     # all_biodiversity_no_planting <- result[[length(result)]]$all_biodiversity_no_planting
  
  # tmp <- get_richness(group = group,
  #                     all_biodiversity_planting = all_biodiversity_planting,
  #                     all_biodiversity_no_planting = all_biodiversity_no_planting,
  #                     MAXYEAR = MAXYEAR,
  #                     year_of_planting_from_0 = year_vector,
  #                     NAME_CONVERSION = NAME_CONVERSION)
  
  
  
  small_fulltable_dt <- FullTable %>%
    dplyr::select(area, year, treespecie, starts_with("Richness")) %>%
    mutate(parcel_id = 1:nrow(FullTable)) |>
    setDT()
  
  # Melt the data table to convert from wide to long format
  melted_dt <- melt(small_fulltable_dt, 
                    id.vars = c("area", "year", "treespecie", "parcel_id"), 
                    measure.vars = grep("Richness", colnames(small_fulltable_dt), value = TRUE),
                    variable.name = "column_name", 
                    value.name = "richness")
  
  # Extract the column name's tree specie
  melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*?)_.*", "\\1", column_name)]
  # Extract the column name's planting year
  melted_dt <- melted_dt[, colname_plantingyear := gsub(".*PlantingYear(.*?)", "\\1", column_name)]
  # Extract the column name's richness group
  melted_dt <- melted_dt[, colname_group := gsub(".*Group(.*?)_.*", "\\1", column_name)]
  
  # Remove rows where the column_name's tree specie is not the strategy treespecie
  # and where the planting year is not the strategy plantingyear
  # and where we don't plant (area is 0)
  # and where plantingyear is 25
  melted_dt <- melted_dt[colname_treespecie == treespecie &
                           colname_plantingyear == year &
                           area != 0 &
                           colname_plantingyear != MAXYEAR + 1, ]
  
  # Sum richness by group
  tmp <- melted_dt[, .(total_richness = sum(richness)), by = colname_group]
  richness <- tmp$total_richness
  names(richness) <- tmp$colname_group
  
  # End ----
  
  return(list("sum_carbon" = sum_carbon,
              "sum_carbon_sd" = sum_carbon_sd,
              "richness" = richness,
              "sum_biodiversity" = sum_biodiversity,
              "sum_biodiversity_sd" = sum_biodiversity_sd,
              "sum_visits" = sum_visits,
              "sum_visits_sd" = sum_visits_sd,
              "sum_area" = sum(area_vector)
  ))
}

# tic()
# a <- get_outcomes_from_strategy(DoE_high_dimension_categorical[1, ])
# toc()
# 
# tic()
# a <- apply(DoE_high_dimension_categorical[1:10, ],
#            1,
#            get_outcomes_from_strategy)
# toc()
# 
# microbenchmark::microbenchmark(a <- get_outcomes_from_strategy(DoE_high_dimension_categorical[1, ]),
#                                times = 10)
# 
# microbenchmark::microbenchmark(a <- apply(DoE_high_dimension_categorical[1:10, ],
#                                           1,
#                                           get_outcomes_from_strategy),
#                                times = 5)




alpha <- 0.99
Implausibility <- function(x, targetLevel = -sqrt(alpha/(1-alpha))){
  #strategy in high-dim continuous space
  strategy_cont <- RRembo_project_low_dimension_to_high_dimension_basic(DoE_low_dimension = x, A = A)
  group_size <- ncol(strategy_cont) / 3
  # Area
  strategy_area <- continuous_to_multi_categorical(values = strategy_cont[, 1:group_size],
                                                   legal_values_ordered = area_possible_values_dataframe)
  colnames(strategy_area) <- paste0("area_parcel_", 1:group_size)
  # Year of planting
  strategy_year <- continuous_to_multi_categorical(values = strategy_cont[, (group_size+1):(2*group_size)],
                                                   legal_values_ordered = years_possible_values_dataframe)
  colnames(strategy_year) <- paste0("plantingyear_parcel_", 1:group_size)
  # Tree specie
  strategy_treespecie <- continuous_to_multi_categorical(values = strategy_cont[, (2*group_size+1):(3*group_size)],
                                                         legal_values_ordered = tree_specie_possible_values_dataframe)
  colnames(strategy_treespecie) <- paste0("treespecie_parcel_", 1:group_size)
  
  strategy <- cbind(strategy_area, strategy_year, strategy_treespecie)
  outcomes <- get_outcomes_from_strategy(parameter_vector=strategy[1,])
  if(outcomes$sum_area > area_sum_threshold){
    return((outcomes$sum_area-area_sum_threshold)/0.001)
  }
  if(any(strategy_year < year_of_planting_min_threshold_vector)){
    #planting in prohibited year
    return(max(year_of_planting_min_threshold_vector-strategy_year))
  }
  #carbon
  imp_carbon <- (outcomes_to_maximize_sum_threshold_vector["Carbon"] - outcomes$sum_carbon)/(outcomes$sum_carbon_sd)
  #visits
  imp_visits <- (outcomes_to_maximize_sum_threshold_vector["Visits"] - outcomes$sum_visits)/(outcomes$sum_visits_sd)
  #richness has no sd
  imp_richness <- outcomes_to_maximize_sum_threshold_vector[SPECIES %in% NAME_CONVERSION$Group] - outcomes$sum_richness
  outcomes_to_maximize_sum_threshold_vector #targets for species but where does richness come in?
  #biodiversity
}