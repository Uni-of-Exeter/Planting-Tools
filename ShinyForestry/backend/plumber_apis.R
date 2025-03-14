library(plumber)
library(jsonlite)
library(future)


RNGversion("4.0.0")
set.seed(1)

normalizePath <- function(path, winslash = "/", mustWork = FALSE) {
  base::normalizePath(path, winslash = winslash, mustWork = mustWork)
}

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
MAX_LIMIT_LOG_LEVEL <- "debug"

# FolderSource <- normalizePath("/Planting-Tools/ShinyForestry")
FolderSource <- normalizePath(file.path(".."))
# Overridden in server() block, necessary for source(...)
SESSION_FILE_SUFFIX <- ""

source(normalizePath(file.path(FolderSource, "functions.R")), local = TRUE)
source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
source(normalizePath(file.path(FolderSource, "preferTrees.R")), local = FALSE)
source(normalizePath(file.path(FolderSource, "..", "backend", "DannyFunctions.R")), local = FALSE)


# Retrieve packages from DESCRIPTION (in plantingtools_folder)
plantingtools_folder <- normalizePath(file.path(FolderSource, ".."))
packages <- read.dcf(normalizePath(file.path(plantingtools_folder, "DESCRIPTION")))[, "Imports"]
packages <- unlist(strsplit(packages, ",\\s*"))  # Split and flatten
packages <- gsub("\\s*\\(.*\\)", "", packages)  # Remove version constraints
packages <- na.omit(packages)  # Remove any NAs

# Load packages in DESCRIPTION
for(ll in 1:length(packages)) {
  library(packages[ll], character.only = TRUE)
}

# Define file paths
ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
CalculatedFilesFolder <- normalizePath(file.path(FolderSource, "CalculatedFiles"))


plan(futureplan, workers = min(3, future::availableCores()))

#* Log full request information
#* @filter log_request
function(req) {
  # # Print full URI and other request details
  # cat("Full URI:", req$rook.url_scheme, "://", req$HTTP_HOST, req$PATH_INFO, "\n")
  # cat("Method:", req$REQUEST_METHOD, "\n")
  # cat("Query String:", req$QUERY_STRING, "\n")
  # cat("Headers:", paste(names(req$HEADERS), req$HEADERS, sep = ": ", collapse = "; "), "\n")
  # try(cat("POST Body (if any):", rawToChar(req$postBody), "\n"), silent = FALSE)
  
  notif(paste0(req$REQUEST_METHOD, " -- ",
               req$rook.url_scheme, "://", req$HTTP_HOST, req$PATH_INFO, req$QUERY_STRING, " -- ",
               "Headers = { ", paste(names(req$HEADERS), req$HEADERS, sep = ": ", collapse = "; "), " }"),
        ntfy = FALSE)
  
  # Forward the request to the next filter or endpoint
  forward()
}


# #* @get /health
# #* @serializer json
# function() {
#   list(status = "OK", message = "Plumber API is running")
#   res$status <- 200
# }

#* Health
#* @get /health
#* @response 200 Success: Service is healthy
function(res) {
  res$status <- 200
  return("OK")
}

# Example Output
#                               parcel_id                       geometry
# 1  f4b4e4d0-7f09-4d26-a7b7-bab5b3151a57 POLYGON ((-1.756976 50.8314...
# 2  26841c10-5fdb-4988-8795-50ef9038ed5d POLYGON ((-1.766385 50.8160...
# 3  cb4fdd41-11ac-426e-b4d6-c04ea47ea14d POLYGON ((-1.765671 50.8316...
# 4  cae4aa35-d2a9-415c-86ef-873d281855d3 POLYGON ((-1.759141 50.8113...
# 5  bd20ad23-6046-46b8-8efa-ddb018d6d865 POLYGON ((-1.759423 50.8109...
# 6  26864357-0046-448e-8b08-ebf00c61fa0a POLYGON ((-1.761 50.83261, ...
# 7  447118a9-ede7-48cb-af07-c5fa822193e5 POLYGON ((-1.763823 50.825,...
# 8  2a110f23-eb3c-4608-a2e6-28c4e2fd673e POLYGON ((-1.762628 50.8344...
# 9  3451c88f-3298-4159-89d9-7bd79b4ff513 POLYGON ((-1.762547 50.8200...
# 10 fa5558f3-a884-44cd-a9db-6b1876ff68cc POLYGON ((-1.760264 50.8301...







# ---- GENERATE_PARCELS
# -- Expected Input
# {
#   "carbon": 800,
#   "species": 12,
#   "species_goat_moth": 90,
#   "species_stag_beetle": 20,
#   "species_lichens": 2,
#   "area": 7,
#   "recreation": 10,
#   "blocked_parcels": [
#     {
#       "parcel_id": "4fe067d1-d80d-4d33-8323-4a4403d2b4a5",
#       "blocked_until_year": 2025
#     },
#     {
#       "parcel_id": "9af563df-2be3-4520-bdd0-dbd26638a063",
#       "blocked_until_year": 2025
#     }
#   ]
# } 

# -- Expected Output list(values, geojson)
# {
#   "carbon": 852,
#   "species": 18,
#   "species_goat_moth": 91,
#   "species_stag_beetle": 22,
#   "species_lichens": 4,
#   "area": 5,
#   "recreation": 16,
# } 
#                               parcel_id parcel_area planting_year planting_type is_available blocked_until_year is_blocked                       geometry
# 1  bdd124e7-a162-4602-bff3-eb5e438d1440 0.022698955            NA          <NA>         TRUE                  0         NA POLYGON ((-1.756976 50.8314...
# 2  162f46c9-dd15-42eb-aa6d-fbbafe002bb6 0.036774571          2043       Conifer         TRUE                  0         NA POLYGON ((-1.766385 50.8160...
# 3  cc38292e-c59c-46b5-84b5-b4a015622d61 0.034369548          2038       Conifer         TRUE                  0         NA POLYGON ((-1.765671 50.8316...
# 4  558f048b-c156-4bf8-9f8f-5dbfce356210 0.027595724            NA          <NA>         TRUE                  0         NA POLYGON ((-1.759141 50.8113...
# 5  48fe3001-8443-4f08-b403-d2304a6c80a9 0.009152795            NA          <NA>         TRUE                  0         NA POLYGON ((-1.759423 50.8109...
# 6  7b66b5a2-8f68-4bba-adf8-6285fc96940a 0.021871169          2044     Deciduous         TRUE                  0         NA POLYGON ((-1.761 50.83261, ...
# 7  a41dfe2f-1856-4806-bf8d-7955d10565bc 0.015572843            NA          <NA>         TRUE                  0         NA POLYGON ((-1.763823 50.825,...
# 8  e52438b7-acf3-4428-b148-bd5b5ea7313e 0.017445100            NA          <NA>         TRUE                  0         NA POLYGON ((-1.762628 50.8344...
# 9  c033b36f-e7dd-4bdb-9deb-a008e442c413 0.015956941          2045       Conifer         TRUE                  0         NA POLYGON ((-1.762547 50.8200...
# 10 3d8adce4-14a0-4b35-8595-ef4645aed0db 0.035769157            NA          <NA>         TRUE                  0         NA POLYGON ((-1.760264 50.8301...


#* Initialise Preferences tab
#* @get /preferences_initialise
#* @serializer json
#* @response 200 Success: Preferences were initialized
function(res) {
  
  # runs: preferences_tab_first_click()
  
  #What happens on first preference tab launch
  
  #Make 2 strategies to compare
  #strategy i
  
  res$status <- 200
  return(lapply(1:2, function(kk) make_strategy_forfront_preftab(kk)))
  
  # returns: (see submit_strategy)
  # list(
  #     list(values, geojson),
  #     list(values, geojson),
  # )
}

#* Provide a preference (1 or 2)
#* @get /preferences
#* @serializer json
#* @param which_button Choice of button, 1 or 2
#* @response 200 Success: Preferences are updates
#* @response 403 Forbidden: the choice should be 1 for the left button or 2 for the right button
function(res, which_button) {
  
  # Takes in: either 1 or 2
  if(!which_button %in% c(1,2)) {
    res$status <- 403
    notif("the choice should be 1 for the left button or 2 for the right button", log_level = "error")
    return("the choice should be 1 for the left button or 2 for the right button")
  }
  if(which_button == 1)
    pref_elicitation_object$addPref(c(comparison_index,comparison_index+1))
  else
    pref_elicitation_object$addPref(c(comparison_index+1,comparison_index))
  MCMCflag <- FALSE
  if(comparison_index %% 5 < 1){
    #We will do MCMC after returning strategies
    MCMCflag <- TRUE
  }
  comparison_index <<- comparison_index + 2
  strategies_compared <<- valid_strategies[c(comparison_index,comparison_index+1)] #Note we need some error handling if no strategies left (only for aggressive blocking and someone who wants to do 1000 comparisons)
  pref_elicitation_object$data <- rbind(pref_elicitation_object$data, strategy_outcomes[strategies_compared,..TARGETS])
  
  # MCMC
  
  #This next part is important and I think needs wrapping in future because we want to pass the data back before this is done, as it's after the return it wont happen until done properly, but I've tested and it works.
  
  
  
  if(MCMCflag){
    msg <- "MCMC starting"
    notif(msg, log_level = "debug")
    
    future_promise(expr = {
      
      msg <- "MCMC in a new process starting ..."
      notif(msg, log_level = "debug")
      
      # Changes the global environment:
      # target_compatible_strategies
      
      pref_elicitation_object$update(method="adapt")
      preference_weights <- pref_elicitation_object$posterior_mean
      
      notif(paste(msg, "done"))
      
      
      msg <- "Clustering in the existing new process starting ..."
      notif(msg, log_level = "debug")
      
      target_compatible_strategies <- cluster_samples()
      
      notif(paste(msg, "done"))
      
      return(list(pref_elicitation_object = pref_elicitation_object,
                  preference_weights = preference_weights,
                  target_compatible_strategies = target_compatible_strategies))
      
    }, seed = NULL) %...>% {
      
      # On success
      mcmc_results <- .
      
      
      msg <- "MCMC success, merging pref_elicitation_object and preference_weights (and target_compatible_strategies if we clustered) to .GlobalEnv"
      notif(msg, log_level = "debug")
      pref_elicitation_object <<- mcmc_results$pref_elicitation_object
      preference_weights <<- mcmc_results$preference_weights
      target_compatible_strategies <<- mcmc_results$target_compatible_strategies
      return(TRUE)
      
    } %...!% {
      
      # On failure
      msg <- "MCMC failure"
      notif(msg, log_level = "error")
      return(FALSE)
      
    }
  }
  

  
  
  res$status <- 200
  
  return(lapply(1:2, function(jj) make_strategy_forfront_preftab(jj)))
  # runs: choose_button()
  
  # returns: (see preferences_initialise)
  # list(
  #     list(values, geojson),
  #     list(values, geojson),
  # )
}


#* Submit targets to return a strategy
#* Triggers a clustering in a new process in the background
#* @post /submit_targets
#* @serializer json
#* @param from_submit_button JSON data that contains targets
#* @response 200 Success: Returned strategy
function(req, res, from_submit_button) {
  
  # Takes in:
  # {
  #   "carbon": 852,
  #   "species": 18,
  #   "species_goat_moth": 91,
  #   "species_stag_beetle": 22,
  #   "species_lichens": 4,
  #   "area": 5,
  #   "recreation": 16,
  # } 
  
  # runs: submit_button()
  
  # returns:
  # -- Expected Output list(values, geojson)
  # {
  #   "carbon": 852,
  #   "species": 18,
  #   "species_goat_moth": 91,
  #   "species_stag_beetle": 22,
  #   "species_lichens": 4,
  #   "area": 5,
  #   "recreation": 16,
  # } 
  #                               parcel_id parcel_area planting_year planting_type is_available blocked_until_year is_blocked                       geometry
  # 1  bdd124e7-a162-4602-bff3-eb5e438d1440 0.022698955            NA          <NA>         TRUE                  0         NA POLYGON ((-1.756976 50.8314...
  # 2  162f46c9-dd15-42eb-aa6d-fbbafe002bb6 0.036774571          2043       Conifer         TRUE                  0         NA POLYGON ((-1.766385 50.8160...
  # 3  cc38292e-c59c-46b5-84b5-b4a015622d61 0.034369548          2038       Conifer         TRUE                  0         NA POLYGON ((-1.765671 50.8316...
  # 4  558f048b-c156-4bf8-9f8f-5dbfce356210 0.027595724            NA          <NA>         TRUE                  0         NA POLYGON ((-1.759141 50.8113...
  # 5  48fe3001-8443-4f08-b403-d2304a6c80a9 0.009152795            NA          <NA>         TRUE                  0         NA POLYGON ((-1.759423 50.8109...
  # 6  7b66b5a2-8f68-4bba-adf8-6285fc96940a 0.021871169          2044     Deciduous         TRUE                  0         NA POLYGON ((-1.761 50.83261, ...
  # 7  a41dfe2f-1856-4806-bf8d-7955d10565bc 0.015572843            NA          <NA>         TRUE                  0         NA POLYGON ((-1.763823 50.825,...
  # 8  e52438b7-acf3-4428-b148-bd5b5ea7313e 0.017445100            NA          <NA>         TRUE                  0         NA POLYGON ((-1.762628 50.8344...
  # 9  c033b36f-e7dd-4bdb-9deb-a008e442c413 0.015956941          2045       Conifer         TRUE                  0         NA POLYGON ((-1.762547 50.8200...
  # 10 3d8adce4-14a0-4b35-8595-ef4645aed0db 0.035769157            NA          <NA>         TRUE                  0         NA POLYGON ((-1.760264 50.8301...
  
  
  # from_submit_button <- plumber::parser_json()(value = from_submit_button)
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) return(list(error = "Invalid JSON format."))
  )
  from_submit_button <- body
  
  #Amend global blocked_parcels
  blocked_parcels <<- from_submit_button$blocked_parcels
  #Extract targets
  target_carbon <- from_submit_button$carbon
  target_visits <- from_submit_button$recreation
  target_area <- from_submit_button$area
  bio_names <- names(from_submit_button)[ ! names(from_submit_button)%in% c("carbon", "area", "recreation", "blocked_parcels")]
  targets_bio <- from_submit_button[bio_names]
  names(targets_bio)[which(names(targets_bio)=="biodiversity")] <- "All"
  #Convert English to Latin names for FullTable Compatibility
  for(species in 1:length(targets_bio)){
    t_name <- names(targets_bio)[species]
    if(t_name != "All"){
      specie_num <- which(NAME_CONVERSION$English_specie == t_name)
      if(length(specie_num)>0){
        #A species
        names(targets_bio)[species] <- NAME_CONVERSION$Specie[specie_num]
        #No need to change if its a group
      }
    }
  }
  #Filter strategies for blocked parcels and amend global compatible strategies
  if(length(from_submit_button$blocked_parcels)>0){
    t_ids <- paste("plantingyear","parcel",from_submit_button$blocked_parcels$parcel_id,sep="_")
    sampled_parcels <- Strategies[,t_ids,drop=F]
    block_compatible <- matrix(0, nrow=nrow(sampled_parcels),ncol=ncol(sampled_parcels))
    for(kk in 1:length(t_ids)){
      block_compatible[,kk] <- sampled_parcels[,kk] > from_submit_button$blocked_parcels$blocked_until_year[kk]
    }
    valid_strategies <<- which(rowSums(block_compatible)==ncol(block_compatible))
  }else{
    valid_strategies <<- strategy_outcomes$strategy_id
  }
  #Find the target compatible strategies and assign global variable for use in other algorithms
  target_compatible_strategies <<- strategy_outcomes[ strategy_id %in% valid_strategies &
                                                        (target_carbon - carbon)/carbon_sd < (-sqrt(alpha/(1-alpha))) &
                                                        area < target_area & 
                                                        (target_visits - visits)/visits_sd < (-sqrt(alpha/(1-alpha))) &
                                                        Reduce(`&`, lapply(SPECIES, function(col) 100 * (targets_bio[[col]] - get(col)) < (-sqrt(alpha/(1-alpha))) )) ]
  if(nrow(target_compatible_strategies)>0){
    optimal_strategy_forfrontend <- target_compatible_strategies[which.max(objective)]
    #Now wrap the optimal strategy into the right format
    tyears <- as.numeric(as.vector(Strategies[optimal_strategy_forfrontend$strategy_id,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
    tspecies <- as.vector(Strategies[optimal_strategy_forfrontend$strategy_id,startsWith(colnames(Strategies),"treespecie")])
  }else{#return the null strategy
    optimal_strategy_forfrontend <- null_outcomes
    tyears <- as.numeric(as.vector(null_strategy[1,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
    tspecies <- as.vector(null_strategy[1,startsWith(colnames(Strategies),"treespecie")])
  }
  blocked_until_year <- rep(0, length(parcel_ids))
  blocked_until_year[which(parcel_ids %in% from_submit_button$blocked_parcels$parcel_id)] <- from_submit_button$blocked_parcels$blocked_until_year
  for_frontend <- st_sf(
    parcel_id = parcel_ids,
    geometry = FullTable$geometry,
    parcel_area = FullTable$area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- optimal_strategy_forfrontend[, ..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="visits")] <- "recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("carbon", "area", "recreation", "biodiversity")]
  bio_names_latin
  for(species in bio_names_latin){
    specie_num <- which(NAME_CONVERSION$Specie == species)
    if(length(specie_num)>0){
      #A species
      names(payload)[which(names(payload)==species)] <- NAME_CONVERSION$English_specie[specie_num]
      #No need to change if its a group
    }
  }
  
  # Clustering as a side-effect, in a future, then push it to global environment
  msg <- "Clustering starting as a new process"
  notif(msg, log_level = "debug")
  future_promise(expr = {
    
    msg <- "Clustering in a new process starting"
    notif(msg, log_level = "debug")
    
    # Changes the global environment:
    # target_compatible_strategies
    # 
    target_compatible_strategies <- cluster_samples()
    return(target_compatible_strategies)
    
  }, seed = NULL) %...>% {
    
    # On success
    target_compatible_strategies <- .
    
    msg <- "Clustering success, merging target_compatible_strategies to .GlobalEnv"
    notif(msg, log_level = "debug")
    target_compatible_strategies <<- target_compatible_strategies
    return(TRUE)
    
  } %...!% {
    
    # On failure
    msg <- "Clustering failure"
    notif(msg, log_level = "error")
    return(FALSE)
    
  }
  res$status <- 200
  return(list(
    values = payload,
    geojson = geojson
  ))
}

# /alternative_approaches ----
#* Obtain four alternative approaches
#* @get /alternative_approaches
#* @serializer json
#* @response 200 Success: Returned strategy
function(res) {
  # returns:
  # list(
  #     list(values, geojson),
  #     list(values, geojson),
  #     list(values, geojson),
  #     list(values, geojson),
  # )
  
  res$status <- 200
  if(all(target_compatible_strategies$cluster==1)) {
    return(lapply(1:4, function(kk) make_strategy_forfront_altapproach(1)))
  } else {
    return(lapply(1:4, function(kk) make_strategy_forfront_altapproach(kk)))
  }
}

# /exploration_initialise ----
#* Obtain four alternative approaches
#* @get /exploration_initialise
#* @serializer json
#* @param which_cluster Which cluster is selected
#* @response 200 Success: Returned strategy
#* @response 403 Forbidden: the choice must be between 1 and 4
function(res, which_cluster = 1) {
  # input: a number from 1 - 4 for the cluster picked on the alternative_approaches tab
  
  # returns:
  # list(values, geojson) #see submit_strategy
  
  if(!which_cluster %in% c(1,2,3,4)) {
    res$status <- 403
    notif("thethe choice must be between 1 and 4", log_level = "error")
    return("the choice must be between 1 and 4")
  }
  
  if (is.null(target_compatible_strategies$cluster)) {
    target_compatible_strategies$cluster <- 1
  }
  
  #First we need a global variable containing the target compatible samples for a cluster. This will be amended on entering the exploration tab
  tc_samples_cluster <<- target_compatible_strategies[cluster==1]
  #We also need a strategy_id to represent the current strategy shown. This will also be amended on entry
  cluster_strat_id <<- tc_samples_cluster[sample(1:nrow(tc_samples_cluster),1)]$strategy_id
  #which_cluster must come from the front end (a map is selected on Alternative strategies) When we know this works, we just need to add a catch 
  #that defaults to cluster 1 (easy, but dont want to add it until the click into this page works)
  
  
  
  if(all(target_compatible_strategies$cluster==1)) {
    which_cluster <- 1
  }
  #Reassign global variable to the strategies that will populate the page
  tc_samples_cluster <<- target_compatible_strategies[cluster==which_cluster]
  #Send back a random strategy so the front end can plot it
  random_strategy <- tc_samples_cluster[sample(1:nrow(tc_samples_cluster),1)]
  #update global variable to point to strategy shown for button functions
  cluster_strat_id <<- random_strategy$strategy_id
  tyears <- as.numeric(as.vector(Strategies[random_strategy$strategy_id,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
  tspecies <- as.vector(Strategies[random_strategy$strategy_id,startsWith(colnames(Strategies),"treespecie")])
  blocked_until_year <- rep(0, length(parcel_ids))
  blocked_until_year[which(parcel_ids %in% blocked_parcels$parcel_id)] <- blocked_parcels$blocked_until_year
  for_frontend <- st_sf(
    parcel_id = parcel_ids,
    geometry = FullTable$geometry,
    parcel_area = FullTable$area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- random_strategy[,..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="visits")] <- "recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("carbon", "area", "recreation", "biodiversity")]
  bio_names_latin
  for(species in bio_names_latin){
    specie_num <- which(NAME_CONVERSION$Specie == species)
    if(length(specie_num)>0){
      #A species
      names(payload)[which(names(payload)==species)] <- NAME_CONVERSION$English_specie[specie_num]
      #No need to change if its a group
    }
  }
  res$status <- 200
  return(list(
    values = payload,
    geojson = geojson
  ))
  
}

# /exploration_plus ----
#* Obtain four alternative approaches, increase one slider
#* @get /exploration_plus
#* @serializer json
#* @param slider_name Name of slider being pushed up by 1 unit
#* @response 200 Success: Returned strategy
#* @response 403 Forbidden: Parameter must be a slider name
function(res, slider_name) {
  # input: 
  #  slider_name
  
  # runs plus_button()
  
  # returns:
  # list(values, geojson) #see submit_strategy
  slider_names <- c(TARGETS, "pc_1", "pc_2")
  if (isFALSE(slider_name %in% slider_names)) {
    res$status <- 403
    notif(paste("Parameter in /exploration_plus must be a slider name:", slider_names), log_level = "error")
    return(paste("Parameter in /exploration_plus must be a slider name:", slider_names))
  }
  
  setorderv(tc_samples_cluster, slider_name, order = -1) 
  current_row <- which(tc_samples_cluster$strategy_id == cluster_strat_id)
  if(current_row > 1){#If we're in row one, we can't increase and will just return the same strategy we had before
    cluster_strat_id <<- tc_samples_cluster$strategy_id[current_row - 1]
    current_row <- current_row-1
  }
  tyears <- as.numeric(as.vector(Strategies[cluster_strat_id,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
  tspecies <- as.vector(Strategies[cluster_strat_id,startsWith(colnames(Strategies),"treespecie")])
  blocked_until_year <- rep(0, length(parcel_ids))
  blocked_until_year[which(parcel_ids %in% blocked_parcels$parcel_id)] <- blocked_parcels$blocked_until_year
  for_frontend <- st_sf(
    parcel_id = parcel_ids,
    geometry = FullTable$geometry,
    parcel_area = FullTable$area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- tc_samples_cluster[current_row,..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="visits")] <- "recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("carbon", "area", "recreation", "biodiversity")]
  bio_names_latin
  for(species in bio_names_latin){
    specie_num <- which(NAME_CONVERSION$Specie == species)
    if(length(specie_num)>0){
      #A species
      names(payload)[which(names(payload)==species)] <- NAME_CONVERSION$English_specie[specie_num]
      #No need to change if its a group
    }
  }
  res$status <- 200
  return(list(
    values = payload,
    geojson = geojson
  ))
  
}

# /exploration_minus ----
#* Obtain four alternative approaches, decrease one slider
#* @get /exploration_minus
#* @serializer json
#* @param slider_name Name of slider being pushed up by 1 unit
#* @response 200 Success: Returned strategy
#* @response 403 Forbidden: Parameter must be a slider name
function(res, slider_name) {
  # input: 
  #  slider_name
  
  # runs minus_button() if "-"
  
  # returns:
  # list(values, geojson) #see submit_strategy
  
  
  slider_names <- c(TARGETS, "pc_1", "pc_2")
  if (isFALSE(slider_name %in% slider_names)) {
    res$status <- 403
    notif(paste("Parameter in /exploration_minus must be a slider name:", slider_names), log_level = "error")
    return(paste("Parameter in /exploration_minus must be a slider name:", slider_names))
  }
  
  setorderv(tc_samples_cluster, slider_name, order = 1) 
  current_row <- which(tc_samples_cluster$strategy_id == cluster_strat_id)
  if(current_row > 1){#If we're in row one, we can't increase and will just return the same strategy we had before
    cluster_strat_id <<- tc_samples_cluster$strategy_id[current_row - 1]
    current_row <- current_row-1
  }
  tyears <- as.numeric(as.vector(Strategies[cluster_strat_id,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
  tspecies <- as.vector(Strategies[cluster_strat_id,startsWith(colnames(Strategies),"treespecie")])
  blocked_until_year <- rep(0, length(parcel_ids))
  blocked_until_year[which(parcel_ids %in% blocked_parcels$parcel_id)] <- blocked_parcels$blocked_until_year
  for_frontend <- st_sf(
    parcel_id = parcel_ids,
    geometry = FullTable$geometry,
    parcel_area = FullTable$area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- tc_samples_cluster[current_row,..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="visits")] <- "recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("carbon", "area", "recreation", "biodiversity")]
  bio_names_latin
  for(species in bio_names_latin){
    specie_num <- which(NAME_CONVERSION$Specie == species)
    if(length(specie_num)>0){
      #A species
      names(payload)[which(names(payload)==species)] <- NAME_CONVERSION$English_specie[specie_num]
      #No need to change if its a group
    }
  }
  
  res$status <- 200
  return(list(
    values = payload,
    geojson = geojson
  ))
}

# https://github.com/rstudio/plumber/issues/579#issuecomment-702432276
# https://stackoverflow.com/questions/76968662/plumber-accepting-a-file-and-a-string-file-name
#* Upload user Elicitor files
#* curl -X PUT -H "Content-Type: multipart/form-data" -F "file_to_upload=@land_parcels.shp.zip;type=application/x-zip-compressed" localhost:<port>/upload
#* curl -X PUT -H "Content-Type: multipart/form-data" -F "file_to_upload=@decision_units.json" localhost:<port>/upload
#* curl -X PUT -H "Content-Type: multipart/form-data" -F "file_to_upload=@outcomes.json" localhost:<port>/upload
#* @put /upload
#* @param file_to_upload:[file] File to upload
#* @response 200 Success: The file was saved to disk
#* @response 400 Bad request: The file name is incorrect
function(req, res, file_to_upload) {
  
  library(mime)
  suppressWarnings({
    multi <- mime::parse_multipart(req)
  })
  
  acceptable_file_names <- c("land_parcels.shp.zip", "decision_units.json", "outcomes.json")
  
  save_folder <- get_backend_folder_save_data()
  elicitor_output_folder <- normalizePath(file.path(save_folder, "ElicitorOutput"))
  
  if (isFALSE(dir.exists(elicitor_output_folder))) {
    dir.create(elicitor_output_folder, recursive = TRUE)
  }
  
  
  # Get file from binary content
  filename <- multi$file_to_upload$name
  
  
  if (isFALSE(filename %in% acceptable_file_names)) {
    res$status <- 400
    return(paste("Bad request: The file name is incorrect, it should be one of", acceptable_file_names, "and you sent", filename))
  }
  file_path <- normalizePath(file.path(elicitor_output_folder, filename))
  
  file.copy(from = multi$file_to_upload$datapath, to = file_path,
            overwrite = TRUE)
  
  # An Elicitor file was changed, Calculated files must be re-computed
  calculated_files_folder <- normalizePath(file.path("CalculatedFiles"))
  files_to_remove <- list.files(path = calculated_files_folder)
  file.remove(file.path(calculated_files_folder, files_to_remove))
  
  res$status <- 200
  return("Success")
}

# /exists ----
#* Check if user input file exists and matches the user's file
#* curl -X GET localhost:<port>/exists?filename=<filename>&md5sum=<md5sum>
#* @get /exists
#* @param filename Name of user input file
#* @param md5sum md5 value of the file
#* @response 200 Success: The file exists and is correct
#* @response 400 Bad request: The file hash is incorrect
#* @response 404 Not found: The file was not found
function(res, filename, md5sum) {
  
  save_folder <- get_backend_folder_save_data()
  elicitor_output_folder <- normalizePath(file.path(save_folder, "ElicitorOutput"))
  file <- file.path(elicitor_output_folder, filename)
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

# /initialize ----
#* Code before server block. Returns an environment
#* curl -X PUT -H "Accept: text/plain" localhost:<port>/initialization
#* @put /initialize
#* @serializer rds
#* @param MAX_LIMIT_LOG_LEVEL more --> less: debug / info / warning / error / none
#* @response 200 Success: Initialized the app, did pre-processing
#* @response 403 Forbidden: Missing one or more input files from the elicitor
#* @response 500 Internal Server Error: One of the elicitor files is not readable
function(res, MAX_LIMIT_LOG_LEVEL = "debug") {
  
  library(future)
  
  formals(notif)$max_limit_log_level <- MAX_LIMIT_LOG_LEVEL
  # plan(sequential)
  notif("Backend initialization ...")
  
  new_environment <- new.env()
  with(new_environment, {
    
    options(future.rng.onMisuse = "ignore")
    
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
    } else { # mystery machine
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
    # futureplan <- future::sequential
    # future:::ClusterRegistry("stop")
    
    FolderSource <- normalizePath(file.path(".."))
    # FolderSource <- normalizePath(file.path(getwd(), "..", "ShinyForestry"))
    # if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
    #   FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
    # }
    
    STARTYEAR<-2025
    MAXYEAR<-2050-STARTYEAR-1
    SCENARIO <- 26
    
    # # Delete log and lock files
    # unlink(base::normalizePath(file.path(FolderSource, "log*"), mustWork = FALSE))
    # unlink(base::normalizePath(file.path(FolderSource, "*lockfile*"), mustWork = FALSE))
    # unlink(base::normalizePath(file.path(FolderSource, "task_id*"), mustWork = FALSE))
    
    # Overridden in server() block, necessary for source(...)
    SESSION_FILE_SUFFIX <- ""
    source(normalizePath(file.path(FolderSource, "functions.R")), local = TRUE)
    source(normalizePath(file.path(FolderSource, "bayesian-optimization-functions.R")), local = TRUE)
    source(normalizePath(file.path(FolderSource, "preferTrees.R")), local = FALSE)
    source(normalizePath(file.path(FolderSource, "..", "backend", "DannyFunctions.R")), local = FALSE)
    
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
    
    save_folder <- get_backend_folder_save_data()
    save_folder_elicitoroutput <- normalizePath(file.path(save_folder, "ElicitorOutput"))
    dir.create(save_folder_elicitoroutput, recursive = TRUE, showWarnings = FALSE)
    
    # ElicitorAppFolder <- normalizePath(file.path(USER_PATH, "Downloads"))
    # ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
    ElicitorAppFolder <- save_folder_elicitoroutput
    DataFilesFolder <- normalizePath(file.path(FolderSource, "JulesOP"))
    DownscalingImagesFolder<-normalizePath(file.path(FolderSource, "DownScalingImages"))
    
    # CalculatedFilesFolder <- normalizePath(file.path(FolderSource, "CalculatedFiles"))
    CalculatedFilesFolder <- normalizePath(file.path(save_folder, "CalculatedFiles"))
    
    # If the folder does not exist, create it
    if (isFALSE(dir.exists(CalculatedFilesFolder))) {
      dir.create(CalculatedFilesFolder)
    }
    
    # Loading big files takes up a lot of RAM that cannot be emptied.
    # So instead, loading happens in a new R process we can then shutdown
    # plan(futureplan, workers = 2)
    
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
      # UnZipDirName <- normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp"))
      UnZipDirName <- normalizePath(file.path("/tmp", "land_parcels.shp"))
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
      st_write(shconv, normalizePath(file.path(save_folder_elicitoroutput, "Parcels.geojson")))
    } else {
      shconv <- sf::st_read(normalizePath(file.path(save_folder_elicitoroutput, "Parcels.geojson")))
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
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "loaded, processing ..." ))
      
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
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      for(i in 1:(MAXYEAR+2))
      {
        FullTab[paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",(i-1))]<-rep(0, length(Uni))
      }
      
      
      MER <- list()
      for (i in 1:length(Uni)) {
        SELLL <- shconv$geometry[AllUnits == Uni[i]]
        MER[[i]] <- suppressMessages(st_union(st_make_valid(SELLL)))
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
      # gc()
      
      
      
      
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
      for (i in 1:length(FullTableCopy$geometry)) {
        SELLLines <- INTT$idPoly == i
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
          FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[i] <-    sum(SelJulesMeans*SELLWeights)#sum(colMeans(SimuArr * SellWeightsArr))
          FullTable$Carbon_SD_Scenario26_TreeSpecieConifers[i] <- sqrt(sum((SelJulesSDs*SELLWeights)^2))#sd(rowSums(SimuArr * SellWeightsArr))
          
          #JulesMeanY29 is not replaced here as it is used to men that there is no planting.
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-colSums(SelJulesMeansYears*SellWeightsArr)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-sqrt(colSums((SelJulesSDsYears*SellWeightsArr)^2))
          
          FullTable$Carbon_Mean_Scenario26_TreeSpecieDeciduous<-sum(SelJulesMeans85*SELLWeights)
          FullTable$Carbon_SD_Scenario26_TreeSpecieDeciduous<-sqrt(sum((SelJulesSDs85*SELLWeights)^2))
          
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-colSums(SelJulesMeansYears85*SellWeightsArr)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-sqrt(colSums((SelJulesSDsYears85*SellWeightsArr)^2))
          
          
          FullTable$area[i] <- sum(SELLWeights)
          
          # } else if (length(SelJulesMeans) == 1) {
          #  SimuArr <- rnorm(NBSIMS, mean = SelJulesMeans, sd = SelJulesSDs)
          # FullTable$JulesMean[i] <- sum(colMeans(SimuArr * SellWeightsArr))
          #  FullTable$JulesSD[i] <- sd(rowSums(SimuArr * SellWeightsArr))
          #  FullTable$area[i] <- sum(SELLWeights)
        } else {
          FullTable$Carbon_Mean_Scenario26_TreeSpecieConifers[i] <- 0
          FullTable$Carbon_SD_Scenario26_TreeSpecieConifers[i] <- 0
          FullTable$Carbon_Mean_Scenario26_TreeSpecieDeciduous[i]<-0
          FullTable$Carbon_SD_Scenario26_TreeSpecieDeciduous[i]<-0
          
          
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          
          FullTable[i,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          FullTable[i,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",0:MAXYEAR)]<-(MAXYEAR+1)
          
          
          FullTable$area[i] <- sum(SELLWeights)
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
                                                                                  max_limit_log_level = MAX_LIMIT_LOG_LEVEL)
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
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file ..."))
      
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
      notif(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing ..."))
      
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
      msg <- paste(msg, "done")
      notif(msg)
      
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
      
      # st_write(FullTable, normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
      st_write(FullTable, normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson")))
      # FullTableNotAvail <- data.frame(extent = NULL)
      # st_write(FullTableNotAvail, normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))
      st_write(FullTableNotAvail, normalizePath(file.path(save_folder_elicitoroutput, "FullTableNotAvail.geojson")))
    }
    
    # notif(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson ..."))))
    # FullTable <- value(future(sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))))
    # FullTableNotAvail <- value(future(sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))))
    # notif(paste("Loading", normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))
    notif(paste("Loading", normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson and FullTableNotAvail.geojson ..."))))
    FullTable <- value(future(sf::st_read(normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson")))))
    FullTableNotAvail <- value(future(sf::st_read(normalizePath(file.path(save_folder_elicitoroutput, "FullTableNotAvail.geojson")))))
    notif(paste("Loading", normalizePath(file.path(save_folder_elicitoroutput, "FullTableMerged.geojson and FullTableNotAvail.geojson done"))))
    
    # future:::ClusterRegistry("stop")
    
    # Outcomes
    if (isFALSE(exists("outcomes", inherits = FALSE))) {
      msg <- paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
      notif(msg)
      while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
        Sys.sleep(5)
      }
      msg <- paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file...")
      notif(msg)
      
      # Read the outcomes from the Elicitor app
      tries <- 0
      while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
                                           , silent = TRUE)),
                      "try-error")) {
        Sys.sleep(0.1)
        tries <- tries + 1
        if (tries > 10) {
          res$status <- 500
          return("Unable to read outcomes.json")
        }
      }
      msg <- paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing ...")
      notif(msg)
      
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
      
      msg <- paste(msg, "done")
      notif(msg)
    }
    
    
    FullTable_long <- transform_FullTable_wide_to_long(FullTable_arg = FullTable,
                                                       SCENARIO_arg = SCENARIO,
                                                       MAXYEAR_arg = MAXYEAR,
                                                       verbose = FALSE)
    FullTable_long <- precompute_biodiversity(FullTable_long, MAXYEAR)
    parcel_id <- unique(FullTable_long$parcel_id)
    group_size <- length(parcel_id)
    n_parcels <- group_size
    
    precomputed_vals <- precompute_static_values(FullTable, MAXYEAR)
    
    
    # Random sampling
    NSamp <- 2000
    
    #Now the random sample contains the year of planting/
    msg <- paste0("Sampling ", NSamp, " random strategies ...")
    notif(msg)
    
    Strategies <- local({
      pb <- progressor(steps = NSamp, message = paste("Sampling", NSamp, "strategies ..."))
      Strategies <- foreach(
        i = 1:NSamp,
        .combine = rbind,
        .inorder = TRUE,
        .options.future = list(
          seed = TRUE
        )
      ) %dofuture% {
        
        #Area
        pp <- runif(1)
        rand_samp <- as.integer(runif(n_parcels)< pp)
        Uniqunits <- unique(FullTable$units)
        
        #This part simply ensures that if we are doing decision units as clusters of parcels
        #the same strategy is assigned to all parcels per unit
        result <- matrix(0, nrow = 1, ncol = n_parcels)
        
        for (j in 1:n_parcels) {
          result[1, FullTable$units == Uniqunits[j]] <- rand_samp[j]
        }
        strategy_area <- t(precomputed_vals$area_possible_values_dataframe[cbind(1+result[1,],1:n_parcels)])
        colnames(strategy_area) <- precomputed_vals$area_colnames
        
        #Year
        strategy_year <- result
        strategy_year[1, result[1,]==0] <- (MAXYEAR+1)#Coding no planting as planting in 2051?!
        #This code adds a year layer, ensuring planting year is biased low. It replaces plant no plant with a year and year = MAXYEAR+1 is now "no plant" and 0 is plant immediately
        probb<-runif(1,0.2,0.6)
        size<-15*runif(1)
        strategy_year[1, result[1,]!=0]<-pmin(rnbinom(sum(result[1,]),size=size,prob=probb),MAXYEAR)
        colnames(strategy_year) <- precomputed_vals$plantingyear_colnames
        
        #Tree species
        probbType<-runif(1,0.5)
        strategy_species <- t(sample(c("Conifers", "Deciduous"),n_parcels,replace=TRUE,prob=c(probbType,1-probbType)))
        colnames(strategy_species) <- precomputed_vals$treespecie_colnames
        
        #Combine into a strategy
        return(cbind(strategy_area, strategy_year, strategy_species))
      }
      # Avoid warning message from progressor function
      pb(amount = 0)
      return(Strategies)
    })
    
    msg <- paste(msg, "done")
    notif(msg)
    
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
                                               max_limit_log_level = MAX_LIMIT_LOG_LEVEL)
    
    # SPECIES <- c(NAME_CONVERSION[1:2, "Specie"], "Pollinators", "All")
    # SPECIES_ENGLISH <- c(NAME_CONVERSION[1:2, "English_specie"], "Pollinators", "All")
    N_SPECIES <- length(SPECIES)
    TARGETS <- c("carbon", SPECIES, "area", "visits")
    N_TARGETS <- length(TARGETS)
    
    if (exists("SquaresLoad", inherits = FALSE)) {
      rm(SquaresLoad)
    }
    if (exists("Sqconv", inherits = FALSE)) {
      rm(Sqconv)
    }
    
    
    #Needed for passing strategies to and from front
    parcel_ids = paste0("id",parcel_id)
    
    #FullTable_working corrects the issue with visits only being available for year 0
    FullTable_working <- copy(FullTable_long)
    FullTable_working[, scenario := NULL]
    
    # Create lookup for Visits values (both Mean and SD for planting_year == 0)
    visits_mean_sd_lookup <- FullTable_working[
      planting_year == 0 & outcome_type == "Visits", 
      .(parcel_id, treespecie, statistic_name, visits_value = outcome_value)
    ]
    # Step 1: Create all combinations of planting_year (1:25) for each unique row in visits_mean_sd_lookup
    all_visits_years <- visits_mean_sd_lookup[
      , .(planting_year = 1:25), by = .(parcel_id, treespecie, statistic_name, visits_value)
    ]
    # Step 2: Remove `visits_value` and assign it to `outcome_value`
    all_visits_years[, `:=`(outcome_value = visits_value, visits_value = NULL)]
    # Step 3: Ensure outcome_type is set to "Visits"
    all_visits_years[, outcome_type := "Visits"]
    # Step 4: Bind these new rows to FullTable_working
    FullTable_working <- rbind(
      FullTable_working,
      all_visits_years,
      use.names = TRUE,
      fill = TRUE
    )
    FullTable_working <- FullTable_working[
      is.na(outcome_sub_type) | outcome_sub_type %in% SPECIES
    ]
    
    #Find the outcomes from strategies
    msg <- "Finding the outcomes from strategies ..."
    notif(msg, log_level = "debug")
    strategy_outcomes <- rbindlist(lapply(1:NSamp, function(i) get_outcome_dt(Strategies[i,], FullTable_working,
                                                                              SPECIES_arg = SPECIES,
                                                                              SCENARIO_arg = SCENARIO,
                                                                              MAXYEAR_arg = MAXYEAR,
                                                                              NAME_CONVERSION_arg = NAME_CONVERSION)))
    strategy_outcomes[, strategy_id := 1:NSamp]
    notif(paste(msg, "done"), log_level = "debug")
    
    #establish preference weights
    msg <- "Establish preference weights ..."
    notif(msg, log_level = "debug")
    preference_weights <- c()
    for(target in TARGETS){
      preference_weights[target] <- 1/max(strategy_outcomes[,..target])
      if(target=="area"){preference_weights[target] <- -preference_weights[target]}
    }
    notif(paste(msg, "done"), log_level = "debug")
    
    #Store objective function as weighted combination of outcomes
    strategy_outcomes[, objective := rowSums(.SD * unlist(preference_weights)[TARGETS]), .SDcols = TARGETS]
    
    defaults <- NULL
    get_slider_info <- function(){
      max_values <- strategy_outcomes[, lapply(.SD, max, na.rm=TRUE), .SDcols = TARGETS]
      names(max_values)[which(names(max_values)=="All")] <- "biodiversity"
      names(max_values)[which(names(max_values)=="visits")] <- "recreation"
      bio_names_latin <- names(max_values)[ ! names(max_values)%in% c("carbon", "area", "recreation", "biodiversity")]
      bio_names_latin
      for(species in bio_names_latin){
        specie_num <- which(NAME_CONVERSION$Specie == species)
        if(length(specie_num)>0){
          #A species
          names(max_values)[which(names(max_values)==species)] <- NAME_CONVERSION$English_specie[specie_num]
          #No need to change if its a group
        }
      }
      max_values[, (names(max_values)) := lapply(.SD, signif, digits=3)]
      min_max <- rbind(as.list(setNames(rep(0,length(TARGETS)),names(max_values))), max_values)
      defaults_quantile <- runif(1, 0.5,0.75)
      defaults <- max_values[1, .SD*defaults_quantile]
      defaults$area <- max_values$area
      defaults[, (names(defaults)) := lapply(.SD, signif,digits=3)]
      min_max_default <- rbind(min_max, defaults)
      return(list(defaults = defaults,
                  min_max_default = min_max_default))
    }
    # Assign defaults to the global environment
    slider_info_value <- get_slider_info()
    slider_info <- slider_info_value$min_max_default
    defaults <- slider_info_value$defaults
    rm(slider_info_value)
    
    
    #MAKE THE NULL STRATEGY. USE IT TO RETURN STRATEGIES WHEN CLUSTERING CANT BE DONE, WHEN NOTHING IS TARGET COMPATIBLE ETC. 
    msg <- "Making the null strategy ..."
    notif(msg, log_level = "debug")
    null_strategy <- matrix(0, nrow = 1, ncol = n_parcels*3)
    colnames(null_strategy) <- colnames(Strategies)
    null_strategy[1,(2*n_parcels+1):(3*n_parcels)] <- "Conifers"
    null_outcomes <- get_outcome_dt(null_strategy, FullTable_working,
                                    SPECIES_arg = SPECIES,
                                    SCENARIO_arg = SCENARIO,
                                    MAXYEAR_arg = MAXYEAR,
                                    NAME_CONVERSION_arg = NAME_CONVERSION)
    null_strategy[1,(n_parcels+1):(2*n_parcels)] <- 2050 - STARTYEAR
    notif(paste(msg, "done"), log_level = "debug")
    
    #Function to return optimal strategy from submit button
    #Function must also keep the target and target compatible strategies for use elsewhere
    # Will need to have the 0 strategy in the sample set and use it if target_compatible empty
    #First global variable is the list of valid_strategies which here is assigned to all. This will be amended by the submit_button
    valid_strategies <- strategy_outcomes$strategy_id
    #Second global variable amended is target_compatible_strategies, first assigned to all strategies here
    target_compatible_strategies <- strategy_outcomes
    #Blocked parcels is something we need to store once amended via a submit (it is used throughout the app)
    blocked_parcels <- list()
    
    #alpha for implausibility 
    alpha <- 0.9
    
    msg <- "Making the first strategy ..."
    notif(msg, log_level = "debug")
    get_first_strategy <- function(){
      target_carbon <- defaults$carbon
      target_visits <- defaults$recreation
      target_area <- defaults$area
      bio_names <- names(defaults)[ ! names(defaults)%in% c("carbon", "area", "recreation", "blocked_parcels")]
      targets_bio <- defaults[,  ..bio_names]
      names(targets_bio)[which(names(targets_bio)=="biodiversity")] <- "All"
      #Convert English to Latin names for FullTable Compatibility
      for(species in 1:length(targets_bio)){
        t_name <- names(targets_bio)[species]
        if(t_name != "All"){
          specie_num <- which(NAME_CONVERSION$English_specie == t_name)
          if(length(specie_num)>0){
            #A species
            names(targets_bio)[species] <- NAME_CONVERSION$Specie[specie_num]
            #No need to change if its a group
          }
        }
      }
      
      
      target_compatible_strategies <- strategy_outcomes[ (target_carbon - carbon)/carbon_sd < (-sqrt(alpha/(1-alpha))) &
                                                            area < target_area & 
                                                            (target_visits - visits)/visits_sd < (-sqrt(alpha/(1-alpha))) &
                                                            Reduce(`&`, lapply(SPECIES, function(col) 100 * (targets_bio[[col]] - get(col)) < (-sqrt(alpha/(1-alpha))) )) ]
      if(nrow(target_compatible_strategies)>0){
        optimal_strategy_forfrontend <- target_compatible_strategies[which.max(objective)]
        #Now wrap the optimal strategy into the right format
        tyears <- as.numeric(as.vector(Strategies[optimal_strategy_forfrontend$strategy_id,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
        tspecies <- as.vector(Strategies[optimal_strategy_forfrontend$strategy_id,startsWith(colnames(Strategies),"treespecie")])
      }else{#return the null strategy
        optimal_strategy_forfrontend <- null_outcomes
        tyears <- as.numeric(as.vector(null_strategy[1,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
        tspecies <- as.vector(null_strategy[1,startsWith(colnames(Strategies),"treespecie")])
      }
      blocked_until_year <- rep(0, length(parcel_ids))
      for_frontend <- st_sf(
        parcel_id = parcel_ids,
        geometry = FullTable$geometry,
        parcel_area = FullTable$area,
        planting_year = ifelse(tyears<2050,tyears,NA),
        planting_types = ifelse(tyears<2050, tspecies, NA),
        blocked_until_year = blocked_until_year,
        crs = st_crs(FullTable)
      )
      #convert to geojson
      geojson <- geojsonsf::sf_geojson(for_frontend)
      
      payload <- optimal_strategy_forfrontend[, ..TARGETS]
      names(payload)[which(names(payload)=="All")] <- "biodiversity"
      names(payload)[which(names(payload)=="visits")] <- "recreation"
      bio_names_latin <- names(payload)[ ! names(payload)%in% c("carbon", "area", "recreation", "biodiversity")]
      bio_names_latin
      for(species in bio_names_latin){
        specie_num <- which(NAME_CONVERSION$Specie == species)
        if(length(specie_num)>0){
          #A species
          names(payload)[which(names(payload)==species)] <- NAME_CONVERSION$English_specie[specie_num]
          #No need to change if its a group
        }
      }
      return(list(
        values = payload,
        geojson = geojson,
        target_compatible_strategies = target_compatible_strategies
      ))
    }
    notif(paste(msg, "done"), log_level = "debug")
    
    # target_compatible_strategies is needed in the environment, but first_strategy needs only the other variables
    msg <- "Making the target_compatible_strategies ..."
    notif(msg, log_level = "debug")
    first_strategy <- get_first_strategy()
    target_compatible_strategies <- first_strategy$target_compatible_strategies
    first_strategy$target_compatible_strategies <- NULL
    notif(paste(msg, "done"), log_level = "debug")
    
    
    ## Preference initialisation ----
    msg <- "Starting preference tab initialisation ..."
    notif(msg, log_level = "debug")
    #Preferences tab. Here we should already know the strategies that are currently compared and the ones that are to be compared next. We should already have a set of weights
    
    #Set priors
    prior_list_temp <- list()
    # Carbon prior
    # mean = 1 / half(midpoint)
    # 2 * sd = 1 / half(midpoint)
    
    prior_list_temp$carbon <- gamma_prior(2 / max(strategy_outcomes[,carbon]),
                                          1 / max(strategy_outcomes[,carbon]))
    
    # Species priors, similarly-derived values
    for (i in 1:length(SPECIES)) {
      specie <- SPECIES[i]
      add_to_list <- setNames(list(Normal(2 / max(strategy_outcomes[,get(specie)]),
                                          1 / max(strategy_outcomes[,get(specie)]))),
                              specie)
      prior_list_temp <- append(prior_list_temp, add_to_list)
    }
    
    # Area prior
    prior_list_temp$area <- gamma_prior(- 2 / max(strategy_outcomes[,area]),
                                        1 / max(strategy_outcomes[,area]))
    
    # Visits prior
    prior_list_temp$visits <- Normal(2 / max(strategy_outcomes[,visits]),
                                     1 / max(strategy_outcomes[,visits]))
    
    # Re-order the list in accordance to TARGETS vector
    prior_list <- list()
    for (target in TARGETS) {
      prior_list[[target]] <- prior_list_temp[[target]]
    }
    rm(prior_list_temp)
    prior_list
    
    ## We need to compare valid_strategies as users will have parcels blocked
    #valid_strategies is a dynamic global variable pointing to the samples we are allowed to compare
    #Maybe one issue is what happens if blocking changes and we end up comparing two we've had before? Perhaps it is not an issue, but flagged.
    
    #Indexes which comparison we are sending to the front end and expecting back following "choose"
    comparison_index <- 1
    strategies_compared <- valid_strategies[c(comparison_index,comparison_index+1)]
    
    #Establish Preference elicitation object
    pref_elicitation_object <- prefObject(data = strategy_outcomes[strategies_compared,..TARGETS], priors = prior_list)
    
    
    
    notif(paste(msg, "done"), log_level = "debug")
    
  })
  
  notif("Backend initialization ... done", log_level = "debug")
  plan(sequential)
  
  # Merge the environment into the global environment
  list2env(as.list(new_environment), envir = .GlobalEnv)
  
  rm(list = c("ElicitorAppFolder",
              "DataFilesFolder",
              "DownscalingImagesFolder",
              "CalculatedFilesFolder",
              "save_folder",
              "save_folder_elicitoroutput",
              # "plantingtools_folder",
              "FolderSource"),
     envir = new_environment)
  
  # We only need a few variables
  env <- new.env()
  env$FullTableNotAvail <- FullTableNotAvail
  env$slider_info <- slider_info
  env$first_strategy <- first_strategy
  env$NAME_CONVERSION <- NAME_CONVERSION
  new_environment <- env
  
  notif("Backend initialization ... done")
  
  res$status <- 200
  return(new_environment)
}

# /check_initialized ----
#* If FullTable is available in the global environment, the /initialize function was called
#* @get /check_initialized
#* @response 200 Success: Was initialized
#* @response 404 Not found: Was not initialized
function(res) {
  if (exists("FullTable", envir = .GlobalEnv)) {
    res$status <- 200
    return(TRUE)
  } else {
    res$status <- 404
    return(FALSE)
  }
}

# Run this file with plumber: `plumber::plumb("ShinyForestry/backend/mock_strategy.R")$run(port=8010)`