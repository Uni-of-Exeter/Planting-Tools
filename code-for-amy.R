# TASKS
# about preferences tab:
# - make a table where each line denotes a pair of strategies
# - a session ID gets randomly assigned to a sample of 20 of the pairs (add the session ID to the table)
# 
# table:
#   map 1; map 2; session_id
# 1       2       1
# 1       3       1
# ...
# 3       20      1
# 2       21      2
# ...
# 9
# 
# there will be 9 session IDs
# 
# These maps are saved somewhere
# 
# Each map is the result of a bayesian optimization / bertrand method with slider values given from a LHS
# 
# Slider min/max are MaxMinValsReactiveVector()$AreaMin
# 
# 
# 1. lhs::randomLHS() scale it to slider min/max
# 2. remove pairs where one map has all outcomes are better than with the other map
# 3. sample 100 000 from bertrand's method
# 4. remove target-incompatible ones
# 5. for each session id, sample 10 000 pairs
# 6. sample 20 from the remaining ones, per session ID
# 7. save maps and the table everything to file

rm(list = ls())
dgpsi::init_py()
library(doFuture)
library(progressr)
library(dplyr)
library(data.table)
library(flock)
library(RRembo)
library(sf)

RNGversion("4.0.0")
set.seed(2)

# Load files ----

# Load function files
LOG_LEVEL <- "debug"
FolderSource <- normalizePath(getwd())
if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
  FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
}

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

# Retrieve packages from DESCRIPTION (in plantingtools_folder)
plantingtools_folder <- normalizePath(file.path(FolderSource, ".."))
if (file.exists(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))) {
  packages <- read.dcf(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))[, "Imports"]
} else {
  packages <- read.dcf(normalizePath(file.path(FolderSource, "DESCRIPTION")))[, "Imports"]
}
packages <- unlist(strsplit(packages, ",\\s*"))  # Split and flatten
packages <- gsub("\\s*\\(.*\\)", "", packages)  # Remove version constraints
packages <- na.omit(packages)  # Remove any NAs

# Install and load packages in DESCRIPTION
if (Sys.getenv("USERNAME")=="bn267" || Sys.getenv("USERNAME")=="dw356") {
  library("dgpsi")
  library("RRembo")
  for(ll in 1:length(packages)) {
    library(packages[ll], character.only = TRUE)
  }
} else {
  if (isFALSE(require("remotes"))) {
    install.packages('remotes', repos = 'https://cran.rstudio.com')
    library(remotes)
  }
  
  msg <- "Installing all packages ..."
  notif(msg)
  # remotes::install_deps(pkgdir = plantingtools_folder, repos = 'https://cran.rstudio.com', upgrade = TRUE)
  
  msg <- paste(msg, "done")
  notif(msg)
  
  msg <- "Loading all packages ..."
  notif(msg)
  
  sapply(packages, library, character.only = TRUE)
  
  msg <- paste(msg, "done")
  notif(msg)
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

alphaLVL <- 0.9
ILevel<- -(sqrt(alphaLVL/(1-alphaLVL)))
NSamp <- 2000

MaxRounds <- 5
ConvertSample <- sample(1:NSamp, 200)

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
if (os == "windows" || rstudioapi::isAvailable()) {
  futureplan <- future::multisession
  
} else {
  futureplan <- future::multicore
}



# Code for Amy ----


session_ids <- 1:9
nb_strategies_per_session_id <- 20

total_number_of_strategies <- (nb_strategies_per_session_id + 400) * length(session_ids)

# Max slider values
max_slider_values <- c("Carbon" = 257, "All" = 25, "Cossus_cossus" = 13, "Lucanus_cervus" = 98, "Lichens" = 27, "Area" = 15, "Visits" = 29)
max_slider_values["Area"] <- max_slider_values["Area"] * -1

# Generate thresholds
slider_values_matrix <- sapply(max_slider_values, rep, total_number_of_strategies)
slider_values_sample_map_1 <- lhs::randomLHS(n = total_number_of_strategies, k = length(max_slider_values)) * slider_values_matrix
slider_values_sample_map_2 <- lhs::randomLHS(n = total_number_of_strategies, k = length(max_slider_values)) * slider_values_matrix

# Remove rows where all sliders are above/below the other map's sliders
comparisons <- slider_values_sample_map_1 > slider_values_sample_map_2
rows_to_keep <- !(rowSums(comparisons) %in% c(0, 5))
slider_values_sample_map_1 <- slider_values_sample_map_1[rows_to_keep, ]
slider_values_sample_map_2 <- slider_values_sample_map_2[rows_to_keep, ]

# Undo the Area * -1
slider_values_sample_map_1[, "Area"] <- slider_values_sample_map_1[, "Area"] * -1
slider_values_sample_map_2[, "Area"] <- slider_values_sample_map_2[, "Area"] * -1

# Add id columns
slider_values_sample_map_1 <- cbind(slider_values_sample_map_1, "ID" = 1:nrow(slider_values_sample_map_1))
slider_values_sample_map_2 <- cbind(slider_values_sample_map_2, "ID" = nrow(slider_values_sample_map_1) + 1:nrow(slider_values_sample_map_2))

# Take map_1 and find a close but not dominant or dominated strategy for comparison from map_2
# Function to check if two rows match based on ±20% criterion
within_x_percent <- function(row1, row2,x) {
  abs(row1 - row2) / row2 <= x
}

# Add a column to store the matched ID from slider_values_sample_map_2
slider_values_sample_map_1 <- cbind(slider_values_sample_map_1, Matched_ID = NA)

# Iterate through each row in slider_values_sample_map_1

handlers(global = TRUE)
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
handlers(progress_handlers)

if (nrow(slider_values_sample_map_1) > 300) {
  plan(futureplan, workers = 5)
} else {
  plan(sequential)
}
slider_values_sample_map_1 <- local({
  pb <- progressor(steps = nrow(slider_values_sample_map_1), message = paste("Filtering ..."))
  slider_values_sample_map_1 <- foreach(i = 1:nrow(slider_values_sample_map_1),
                                        .inorder = TRUE,
                                        .combine = rbind) %dofuture% {
                                          
                                          full_row1 <- slider_values_sample_map_1[i, , drop = FALSE]
                                          
                                          row1 <- full_row1[1, names(max_slider_values)]  # Exclude the ID column
                                          
                                          # Check all rows in slider_values_sample_map_2 for ±30% match (excluding the ID column)
                                          matches <- apply(slider_values_sample_map_2[, names(max_slider_values)], 1, function(row2) {
                                            all(within_x_percent(row1, row2, 0.3))
                                          })
                                          
                                          # Filter matches based on the !(rowSums condition)
                                          valid_matches <- which(matches & !(rowSums(row1 > slider_values_sample_map_2[, names(max_slider_values)]) %in% c(0, 4)))
                                          
                                          # If valid matches exist, select the first match and note its ID
                                          if (length(valid_matches) > 0) {
                                            matched_id <- slider_values_sample_map_2[valid_matches[1], "ID"]
                                            full_row1[1, "Matched_ID"] <- matched_id
                                          }
                                          pb()
                                          return(full_row1)
                                        }
  pb(amount = 0)
  return(slider_values_sample_map_1)
})


pairs <- na.omit(slider_values_sample_map_1)

# Keep only nb_strategies_per_session_id per session ID
if (nrow(pairs) < length(session_ids) * nb_strategies_per_session_id) {
  stop("Too few pairs, you should increase total_number_of_strategies")
}
pairs <- pairs[1:(length(session_ids) * nb_strategies_per_session_id + 200), c("ID", "Matched_ID")]
notif(paste("There are", nrow(pairs), "pairs"))

# Keep only useful rows
ids_to_keep_map1 <- pairs[, "ID"]
ids_to_keep_map2 <- pairs[, "Matched_ID"]

rows_to_keep_map1 <- which(slider_values_sample_map_1[, "ID"] %in% ids_to_keep_map1)
rows_to_keep_map2 <- which(slider_values_sample_map_2[, "ID"] %in% ids_to_keep_map2)
slider_values_sample_map_1 <- slider_values_sample_map_1[rows_to_keep_map1, ]
slider_values_sample_map_2 <- slider_values_sample_map_2[rows_to_keep_map2, ]


FullTable_long <- transform_FullTable_wide_to_long(FullTable_arg = FullTable,
                                                   SCENARIO_arg = SCENARIO,
                                                   MAXYEAR_arg = MAXYEAR,
                                                   verbose = FALSE)

tolvec <- rep(0.1, 2 + length(SPECIES))
names(tolvec) <- c("Carbon", SPECIES, "Visits")
year_of_max_no_planting_threshold_vector <- rep(0, nrow(FullTable))

# Override get_latest_task_id
get_latest_task_id <- function() {return(1)}

notif("Starting the Bayesian optimizations")
# plan(futureplan, workers = max(6, round(availableCores() - 10)))
plan(futureplan, workers = 7)
# plan(sequential)
bo_results <- local({
  
  unlink(base::normalizePath(file.path("BO_results*"), mustWork = FALSE))
  
  pair_rows <- 1:nrow(pairs)
  
  # pair_rows <- 1:5
  # pair_row <- pair_rows[1]
  # pb <- function(...) {return()}
  
  # 2 bayesian optimizations * 3 calls to pb() * iterations
  pb <- progressor(steps = length(pair_rows) * (1 + 10 * 3 * 2), message = paste("Bayesian optimizations ..."))
  
  bo_results <- foreach(pair_row = pair_rows,
                        .inorder = TRUE,
                        .combine = combine_foreach_rbind,
                        .multicombine = TRUE,
                        .options.future = list(seed = TRUE)) %dofuture% {
                          
                          pb(amount = 0, message = "Starting BO map 1")
                          
                          id_map_1 <- pairs[pair_row, 1]
                          id_map_2 <- pairs[pair_row, 2]
                          row_map_1 <- which(slider_values_sample_map_1[, "ID"] == id_map_1)
                          row_map_2 <- which(slider_values_sample_map_2[, "ID"] == id_map_2)
                          slider_threshold_map_1 <- slider_values_sample_map_1[row_map_1, !(colnames(slider_values_sample_map_1) %in% c("ID", "Matched_ID"))]
                          slider_threshold_map_2 <- slider_values_sample_map_2[row_map_2, !(colnames(slider_values_sample_map_2) %in% c("ID"))]
                          
                          RREMBO_HYPER_PARAMETERS <- RRembo_defaults(d = 6,
                                                                     D = 3 * nrow(FullTable), # area + planting_year + tree_specie per parcel
                                                                     init = list(n = 100), budget = 100,
                                                                     control = RREMBO_CONTROL,
                                                                     limit_log_level = LOG_LEVEL)
                          
                          bo_result_map_1 <- bayesian_optimization(seed = 1,
                                                                   FullTable_arg = FullTable,
                                                                   FullTable_long_arg = FullTable_long,
                                                                   MAXYEAR = MAXYEAR,
                                                                   SCENARIO = SCENARIO,
                                                                   year_of_max_no_planting_threshold_vector = year_of_max_no_planting_threshold_vector,
                                                                   area_sum_threshold = slider_threshold_map_1["Area"],
                                                                   outcomes_to_maximize_sum_threshold_vector = slider_threshold_map_1[names(slider_threshold_map_1) != "Area"],
                                                                   outcomes_to_minimize_sum_threshold_vector = NULL,
                                                                   limit_log_level = LOG_LEVEL,
                                                                   PLOT = FALSE,
                                                                   
                                                                   BAYESIAN_OPTIMIZATION_ITERATIONS = 15,
                                                                   
                                                                   BAYESIAN_OPTIMIZATION_BATCH_SIZE = 1,
                                                                   PENALTY_COEFFICIENT = 2000,
                                                                   
                                                                   current_task_id = 1,
                                                                   
                                                                   EXPLORATION = FALSE,
                                                                   
                                                                   preference_weight_area = -1,
                                                                   preference_weights_maximize = rep(1, length(max_slider_values) - 1),
                                                                   # preference_weights_minimize = rep(1, length(c())),
                                                                   
                                                                   progressr_object = pb,
                                                                   
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
                          
                          if ("strategy_vector" %in% names(bo_result_map_1)) {
                            bo_result_map_1 <- c("ID" = id_map_1, bo_result_map_1$strategy_vector)
                          } else {
                            bo_result_map_1 <- c("ID" = id_map_1, rep(NA, 3 * nrow(FullTable)))
                          }
                          
                          filename <- "BO_results_Amy_map_1.RData"
                          filename_lock <- paste0(filename, ".lock")
                          
                          mylock <- flock::lock(filename_lock)
                          
                          if (file.exists(filename)) {
                            bo_result_map_1_on_file <- get(load(filename))
                            bo_result_map_1_on_file <- rbind(bo_result_map_1_on_file, bo_result_map_1)
                          } else {
                            bo_result_map_1_on_file <- bo_result_map_1
                          }
                          save(bo_result_map_1_on_file, file = filename, compress = FALSE)
                          
                          flock::unlock(mylock)
                          
                          
                          
                          pb(amount = 0, message = "Starting BO map 2")
                          
                          
                          RREMBO_HYPER_PARAMETERS <- RRembo_defaults(d = 6,
                                                                     D = 3 * nrow(FullTable), # area + planting_year + tree_specie per parcel
                                                                     init = list(n = 100), budget = 100,
                                                                     control = RREMBO_CONTROL,
                                                                     limit_log_level = LOG_LEVEL)
                          bo_result_map_2 <- bayesian_optimization(seed = 1,
                                                                   FullTable_arg = FullTable,
                                                                   FullTable_long_arg = FullTable_long,
                                                                   MAXYEAR = MAXYEAR,
                                                                   SCENARIO = SCENARIO,
                                                                   year_of_max_no_planting_threshold_vector = year_of_max_no_planting_threshold_vector,
                                                                   area_sum_threshold = slider_threshold_map_2["Area"],
                                                                   outcomes_to_maximize_sum_threshold_vector = slider_threshold_map_2[names(slider_threshold_map_2) != "Area"],
                                                                   outcomes_to_minimize_sum_threshold_vector = NULL,
                                                                   limit_log_level = LOG_LEVEL,
                                                                   PLOT = FALSE,
                                                                   
                                                                   BAYESIAN_OPTIMIZATION_ITERATIONS = 10,
                                                                   
                                                                   BAYESIAN_OPTIMIZATION_BATCH_SIZE = 1,
                                                                   PENALTY_COEFFICIENT = 2000,
                                                                   
                                                                   current_task_id = 1,
                                                                   
                                                                   EXPLORATION = FALSE,
                                                                   
                                                                   preference_weight_area = -1,
                                                                   preference_weights_maximize = rep(1, length(max_slider_values) - 1),
                                                                   # preference_weights_minimize = rep(1, length(c())),
                                                                   
                                                                   progressr_object = pb,
                                                                   
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
                          
                          if ("strategy_vector" %in% names(bo_result_map_2)) {
                            bo_result_map_2 <- c("ID" = id_map_2, bo_result_map_2$strategy_vector)
                          } else {
                            bo_result_map_2 <- c("ID" = id_map_2, rep(NA, 3 * nrow(FullTable)))
                          }
                          
                          filename <- "BO_results_Amy_map_2.RData"
                          filename_lock <- paste0(filename, ".lock")
                          
                          mylock <- flock::lock(filename_lock)
                          
                          if (file.exists(filename)) {
                            bo_result_map_2_on_file <- get(load(filename))
                            bo_result_map_2_on_file <- rbind(bo_result_map_2_on_file, bo_result_map_2)
                          } else {
                            bo_result_map_2_on_file <- bo_result_map_2
                          }
                          save(bo_result_map_2_on_file, file = filename, compress = FALSE)
                          
                          flock::unlock(mylock)
                          
                          bo_result <- list(map_1 = bo_result_map_1, map_2 = bo_result_map_2)
                          notif(paste(Sys.time(), "Pair number", pair_row, "finished for BO"))
                          
                          pb()
                          
                          return(bo_result)
                        }
  pb(amount = 0)
  return(bo_results)
})
plan(sequential)

notif("code-for-amy.R is finished", ntfy = TRUE)
# stop("END")


bo_result_map_1_on_file <- get(load("BO_results_Amy_map_1.RData")) |>
  data.table::as.data.table()
bo_result_map_1_on_file[, ID.ID := as.numeric(ID.ID)]
bo_result_map_2_on_file <- get(load("BO_results_Amy_map_2.RData")) |>
  data.table::as.data.table()
bo_result_map_2_on_file[, ID.Matched_ID := as.numeric(ID.Matched_ID)]

pairs <- data.table::as.data.table(pairs)
save(pairs, file = "pairs.RData")

# bo_result_map_1_on_file <- data.frame(bo_result_map_1_on_file)

# all_strategies <- data.table::merge.data.table(x = data.table::as.data.table(pairs), y = bo_result_map_1_on_file,
#                                                by.x = "ID", by.y = "ID.ID")
# colnames(all_strategies) <- c(colnames(all_strategies)[c(1,2)],
#                               paste0("map_1_", colnames(all_strategies)[-c(1,2)]))
# all_strategies <- data.table::merge.data.table(x = all_strategies, y = bo_result_map_2_on_file,
#                                                by.x = "Matched_ID", by.y = "ID.Matched_ID")
# colnames(all_strategies) <- c(colnames(all_strategies)[c(1,2)],
#                               paste0("map_1_", colnames(all_strategies)[-c(1,2)]))

# Lines 1-40 are for user 1
# Lines 41-80 are for user 2, etc
# For user 1, lines 1-2 are the pair shown on the first choice
# For user 1, lines 2-3 are the pair shown on the second choice, etc
strategies <- local({
  
  pb <- progressor(steps = nrow(pairs), message = paste("Strategy table ..."))
  
  # strategies <- foreach(
  #   # i = 1:(nb_strategies_per_session_id * length(session_ids)),
  #   i = 1:nrow(pairs),
  #   .combine = combine_foreach_rbind,
  #   .multicombine = TRUE,
  #   .inorder = TRUE
  # ) %do% {
  
  result <- list(YEAR = data.frame(), TYPE = data.frame(), OUTPUTS = data.frame())
  
  for (i in 1:nrow(pairs)) {
    id_map_1 <- as.numeric(pairs[i, 1])
    id_map_2 <- as.numeric(pairs[i, 2])
    
    bo_result_map_1_idx <- which(bo_result_map_1_on_file$ID.ID == id_map_1)
    bo_result_map_2_idx <- which(bo_result_map_2_on_file$ID.Matched_ID == id_map_2)
    
    # result <- list(YEAR = data.frame(), TYPE = data.frame(), OUTPUTS = data.frame())
    if (length(bo_result_map_1_idx) == 0 || length(bo_result_map_2_idx) == 0) {
      pb()
      # return(result)
      next
    }
    
    for (j in 1:2) {
      bo_result <- if (j == 1) bo_result_map_1_on_file[bo_result_map_1_idx[1], -1] else bo_result_map_2_on_file[bo_result_map_2_idx[1], -1]
      bo_result <- as.vector(bo_result)
      
      outcome <- get_outcomes_from_strategy(parameter_vector = bo_result,
                                            FullTable_long_arg = FullTable_long)
      
      area_sum <- outcome$sum_area
      year_planting <- as.numeric(bo_result[grep("plantingyear", names(bo_result))])
      treespecie <- bo_result[grep("treespecie", names(bo_result))]
      indices_no_planting <- which(as.numeric(bo_result[grep("area", names(bo_result))]) == 0)
      treespecie[indices_no_planting] <- "NoPlanting"
      year_planting[indices_no_planting] <- -1
      
      # ORDER OF SPECIES/GROUPS IS SET BY THE USER (outcomes.json), GROUPS ALWAYS COME BEFORE THE SPECIES
      # year_planting (-1:24); treespecie (NoPlanting,Conifers,Deciduous), Carbon, All, Birds, Area, Visits, CarbonSD, AllSD, BirdsSD, VisitsSD
      selectedfulltablerowvalue <- as.data.frame(matrix(c(year_planting,
                                                          treespecie,
                                                          # CarbonMean, BioMeans, Area, VisitsMean
                                                          unlist(outcome[c("sum_carbon", "sum_richness", "sum_biodiversity", "sum_area", "sum_visits")]),
                                                          # CarbonSD, BioSD, VisitsSD
                                                          unlist(outcome[c("sum_carbon_sd", "sum_richness_sd", "sum_biodiversity_sd", "sum_visits_sd")])),
                                                        nrow = 1))
      
      RICHNESS_GROUPS <- names(outcome$sum_richness)
      BIO_SPECIES <- names(outcome$sum_biodiversity)
      # Carbon, All, Birds, Area, Visits, CarbonSD, AllSD, BirdsSD, VisitsSD
      richness_data_frame <- do.call("data.frame",
                                     setNames(lapply(RICHNESS_GROUPS, function(x) bquote(outcome$sum_richness[.(x)])),
                                              RICHNESS_GROUPS))
      biodiversity_data_frame <- do.call("data.frame",
                                         setNames(lapply(BIO_SPECIES, function(x) bquote(outcome$sum_biodiversity[.(x)])),
                                                  BIO_SPECIES))
      richness_sd_data_frame <- do.call("data.frame",
                                        setNames(lapply(RICHNESS_GROUPS, function(x) bquote(outcome$sum_richness_sd[.(x)])),
                                                 paste0(RICHNESS_GROUPS, "SD")))
      biodiversity_sd_data_frame <- do.call("data.frame",
                                            setNames(lapply(BIO_SPECIES, function(x) bquote(outcome$sum_biodiversity_sd[.(x)])),
                                                     paste0(BIO_SPECIES, "SD")))
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
                                  OUTPUTS = selectedvectorvalue_output)
      
      # Change the column names for Bertrand
      colnames(selectedvectorvalue$YEAR) <- paste0("SelectedSimMat.YEAR.", 1:nrow(FullTable))
      colnames(selectedvectorvalue$TYPE) <- paste0("SelectedSimMat.TYPE.", 1:nrow(FullTable))
      
      result$YEAR <- rbind(result$YEAR, selectedvectorvalue$YEAR)
      result$TYPE <- rbind(result$TYPE, selectedvectorvalue$TYPE)
      result$OUTPUTS <- rbind(result$OUTPUTS, selectedvectorvalue$OUTPUTS)
      
    }
    
    pb()
    
  #   return(result)
  }
  
  pb(amount = 0)
  strategies <- result
  return(strategies)
})

save(strategies, file = "strategies_for_amy.RData")

# Remove duplicate strategies
rows_to_keep <- which(!duplicated(strategies$OUTPUTS))

FIXED_STRATEGIES_LIST <- list(YEAR = strategies$YEAR[rows_to_keep, ][1:360, ],
                              TYPE = strategies$TYPE[rows_to_keep, ][1:360, ],
                              OUTPUTS = strategies$OUTPUTS[rows_to_keep, ][1:360, ])
save(FIXED_STRATEGIES_LIST, file = "ShinyForestry/FixedStrats/FIXED_STRATEGIES_LIST.RData")
