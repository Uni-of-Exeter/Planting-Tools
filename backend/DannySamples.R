##Assuming some preamble already happened (package loading etc) I load here just what I need for these functions.
#This code should be part of the pre-amble. The functions later need to be APIs
FullTable <- sf::st_read("ShinyForestry/ElicitorOutput/FullTableMerged.geojson")
FullTable
FullNot <- sf::st_read("ShinyForestry/ElicitorOutput/FullTableNotAvail.geojson")
STARTYEAR<-2025
MAXYEAR<-2050-STARTYEAR-1



FullTable_long <- transform_FullTable_wide_to_long(FullTable_arg = FullTable,
                                                   SCENARIO_arg = SCENARIO,
                                                   MAXYEAR_arg = MAXYEAR,
                                                   verbose = FALSE)
FullTable_long <- precompute_biodiversity(FullTable_long, MAXYEAR)
parcel_id = unique(FullTable_long$parcel_id)
group_size <- length(parcel_id)

precomputed_vals <- precompute_static_values(FullTable, MAXYEAR)

NSamp <- 2000
Strategies <- foreach(
  i = 1:NSamp,
  .combine = rbind,
  .multicombine = TRUE,
  .inorder = TRUE,
  .options.future = list(
    seed = TRUE
  )
) %do% {
  
  #Area
  pp <- runif(1)
  rand_samp <- as.integer(runif(n_parcels)< pp)

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



#Needed for passing strategies to and from front
parcel_ids = paste0("id",parcel_id)

#FullTable_working corrects the issue with Visits only being available for year 0
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

load("ShinyForestry/Arable_livestock_data/merged_calories.RData")

for (year in 0:25) {
  
  # Identify the correct column name
  calorie_col <- paste0("Calories_Arable_Livestock_", 2025 + year)
  
  # Check if the column exists in merged_calories
  if (calorie_col %in% names(merged_calories)) {
    
    # Create a lookup table with row index as parcel_id
    calorie_mapping <- merged_calories[, .(parcel_id = .I, outcome_value = get(calorie_col))]
    
    
    # Perform the merge using a data.table join
    FullTable_working[outcome_type == "Visits" & 
                        statistic_name == "Mean" & 
                        planting_year == year, 
                      outcome_value := calorie_mapping[.SD, on = "parcel_id", x.outcome_value]]
  }
}


# Convert to "kilo calories"
FullTable_working[outcome_type == "Visits" & statistic_name == "Mean", 
                  `:=`(outcome_value = outcome_value / 1e3)]

#Find the outcomes from strategies
strategy_outcomes <- rbindlist(lapply(1:NSamp, function(jj) get_outcome_dt(Strategies[jj,], FullTable_working)))
strategy_outcomes[, strategy_id := 1:NSamp]

TARGETS <- c("Carbon", SPECIES, "Area", "Visits")

#establish preference weights
preference_weights <- c()
for(target in TARGETS){
  preference_weights[target] <- 1/max(strategy_outcomes[,..target])
  if(target=="Area"){preference_weights[target] <- -preference_weights[target]}
}

#Store objective function as weighted combination of outcomes
strategy_outcomes[, objective := rowSums(.SD * unlist(preference_weights)[TARGETS]), .SDcols = TARGETS]

defaults <- NULL
get_slider_info <- function(){
  max_values <- strategy_outcomes[, lapply(.SD, max, na.rm=TRUE), .SDcols = TARGETS]
  names(max_values)[which(names(max_values)=="All")] <- "biodiversity"
  names(max_values)[which(names(max_values)=="Visits")] <- "Recreation"
  bio_names_latin <- names(max_values)[ ! names(max_values)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
  defaults <<- max_values[1, .SD*defaults_quantile]
  defaults[, (names(defaults)) := lapply(.SD, signif,digits=3)]
  min_max_default <- rbind(min_max, defaults)
  return(min_max_default)
}
get_slider_info()


#function() {
#  slider_info <- list(
#    Carbon = list(min = 500, max = 1000, default = 800),
#    species = list(min = 0, max = 25, default = 10),
#    species_goat_moth = list(min = 0, max = 100, default = 25),
#    species_stag_beetle = list(min = 0, max = 100, default = 30),
#    species_lichens = list(min = 0, max = 5, default = 2),
#    Area = list(min = 0, max = 15, default = 10),
#    Recreation = list(min = 0, max = 20, default = 15)
#  )
#  return(slider_info)
#}

#####NOT NEEDED BUT USED FOR TESTING
#First create a list of the values the app will generate before conversion to JSON
payload <- list(
  Carbon = 100, #This is the value on the slider, i.e. the user target
  biodiversity = 24, #All species richness
  Goat_Moth = 17,
  Stag_Beetle = 90,
  Lichens = 5,
  Area = 10,
  Recreation = 20,
  blocked_parcels = data.frame(parcel_id = c("id12", "id13"), blocked_until_year = c("2030", "2027"))
)

json_payload <- jsonlite::toJSON(payload <- payload, auto_unbox = TRUE, pretty=TRUE)

paul_test <- list(
  Carbon = 3,
  biodiversity = 0.5,
  Goat_Moth = 0.5,
  Stag_Beetle = 3,
  Lichens = 0.1,
  Area = 19.4,
  Recreation = 0.6,
  blocked_parcels = list()
)
json_paul <- jsonlite::toJSON(payload <- paul_test, auto_unbox = TRUE, pretty=TRUE)

#######END NOT NEEDED USED FOR TESTING

#Info from front end comes in this JSON format. 

#MAKE THE NULL STRATEGY. USE IT TO RETURN STRATEGIES WHEN CLUSTERING CANT BE DONE, WHEN NOTHING IS TARGET COMPATIBLE ETC. 
null_strategy <- matrix(0, nrow = 1, ncol = n_parcels*3)
colnames(null_strategy) <- colnames(Strategies)
null_strategy[1,(2*n_parcels+1):(3*n_parcels)] <- "Conifers"
null_outcomes <- get_outcome_dt(null_strategy, FullTable_working)
null_strategy[1,(n_parcels+1):(2*n_parcels)] <- 2050 - STARTYEAR

#Function to return optimal strategy from submit button
#Function must also keep the target and target compatible strategies for use elsewhere
# Will need to have the 0 strategy in the sample set and use it if target_compatible empty
#First global variable is the list of valid_strategies which here is assigned to all. This will be amended by the submit_button
valid_strategies <- strategy_outcomes$strategy_id
#Second global variable amended is target_compatible_strategies, first assigned to all strategies here
target_compatible_strategies <- strategy_outcomes
#Blocked parcels is something we need to store once amended via a submit (it is used throughout the app)
blocked_parcels <- list()

get_first_strategy <- function(){
  target_carbon <- defaults$Carbon
  target_visits <- defaults$Visits
  target_area <- defaults$Area
  bio_names <- names(defaults)[ ! names(defaults)%in% c("Carbon", "Area", "Recreation", "blocked_parcels")]
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
  target_compatible_strategies <<- strategy_outcomes[ (target_carbon - Carbon)/Carbon_sd < (-sqrt(alpha/(1-alpha))) &
                                                        Area < target_area & 
                                                        (target_visits - Visits)/visits_sd < (-sqrt(alpha/(1-alpha))) &
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
    parcel_area = FullTable$Area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- optimal_strategy_forfrontend[, ..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="Visits")] <- "Recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
    geojson = geojson
  ))
}

get_first_strategy()

submit_button <- function(from_front_end){
  #Read in json
  from_submit_button <- tryCatch(
    jsonlite::fromJSON(from_front_end, simplifyVector=TRUE),
    error = function(e) return(list(error= "Invalid JSON format."))
  )
  #plumber::parser_json()(value = from_front_end)
  #Amend global blocked_parcels
  blocked_parcels <<- from_submit_button$blocked_parcels
  #Extract targets
  target_carbon = from_submit_button$Carbon
  target_visits = from_submit_button$Recreation
  target_area = from_submit_button$Area
  bio_names <- names(from_submit_button)[ ! names(from_submit_button)%in% c("Carbon", "Area", "Recreation", "blocked_parcels")]
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
                                                       (target_carbon - Carbon)/carbon_sd < (-sqrt(alpha/(1-alpha))) &
                                                       Area < target_area & 
                                                       (target_visits - Visits)/visits_sd < (-sqrt(alpha/(1-alpha))) &
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
    parcel_area = FullTable$Area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- optimal_strategy_forfrontend[, ..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="Visits")] <- "Recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
    geojson = geojson
  ))
}

#test
submit_button(from_front_end = json_payload)



#######
#Preferences tab. Here we should already know the strategies that are currently compared and the ones that are to be compared next. We should already have a set of weights

#Set priors
prior_list_temp <- list()
# Carbon prior
# mean = 1 / half(midpoint)
# 2 * sd = 1 / half(midpoint)

prior_list_temp$Carbon <- gamma_prior(2 / max(strategy_outcomes[,Carbon]),
                                      1 / max(strategy_outcomes[,Carbon]))

# Species priors, similarly-derived values
for (i in 1:length(SPECIES)) {
  specie <- SPECIES[i]
  add_to_list <- setNames(list(Normal(2 / max(strategy_outcomes[,get(specie)]),
                                      1 / max(strategy_outcomes[,get(specie)]))),
                          specie)
  prior_list_temp <- append(prior_list_temp, add_to_list)
}

# Area prior
prior_list_temp$Area <- gamma_prior(- 2 / max(strategy_outcomes[,Area]),
                                    1 / max(strategy_outcomes[,Area]))

# Visits prior
prior_list_temp$Visits <- Normal(2 / max(strategy_outcomes[,Visits]),
                                 1 / max(strategy_outcomes[,Visits]))

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

#What happens on first preference tab launch
preferences_tab_first_click <- function(){
  #Make 2 strategies to compare
  #strategy i
  make_strategy_forfront_preftab <- function(index){
    if(!index %in% c(1,2))
      stop("Index should be 1 or 2 for the preference tab")
    tyears <- as.numeric(as.vector(Strategies[strategies_compared[index],startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
    tspecies <- as.vector(Strategies[strategies_compared[index],startsWith(colnames(Strategies),"treespecie")])
    blocked_until_year <- rep(0, length(parcel_ids))
    blocked_until_year[which(parcel_ids %in% blocked_parcels$parcel_id)] <- blocked_parcels$blocked_until_year
    for_frontend <- st_sf(
      parcel_id = parcel_ids,
      geometry = FullTable$geometry,
      parcel_area = FullTable$Area,
      planting_year = ifelse(tyears<2050,tyears,NA),
      planting_types = ifelse(tyears<2050, tspecies, NA),
      blocked_until_year = blocked_until_year,
      crs = st_crs(FullTable)
    )
    #convert to geojson
    geojson <- geojsonsf::sf_geojson(for_frontend)
    
    payload <- pref_elicitation_object$data[comparison_index + (index-1)]
    names(payload)[which(names(payload)=="All")] <- "biodiversity"
    names(payload)[which(names(payload)=="Visits")] <- "Recreation"
    bio_names_latin <- names(payload)[ ! names(payload)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
      geojson = geojson
    ))
  }
  
  return(lapply(1:2, function(kk) make_strategy_forfront_preftab(kk)))
}

#Now when a choose is selected we need the number of the choice (left is 1, right is 2)
#We need to update the pref object, then return 2 new strategies. 
#We also sometimes need to spawn another action with future to do the MCMC (if comparison_index%%5<1)

choose_button <- function(which_button){
  if(!which_button %in% c(1,2))
    stop("the choice should be 1 for the left button or 2 for the right button")
  if(which_button == 1)
    pref_elicitation_object$addPref(c(comparison_index,comparison_index+1))
  else
    pref_elicitation_object$addPref(c(comparison_index+1,comparison_index))
  MCMCflag <- FALSE
  if(comparison_index%%5<1){
    #We will do MCMC after returning strategies
    MCMCflag <- TRUE
  }
  comparison_index <<- comparison_index + 2
  strategies_compared <<- valid_strategies[c(comparison_index,comparison_index+1)] #Note we need some error handling if no strategies left (only for aggressive blocking and someone who wants to do 1000 comparisons)
  pref_elicitation_object$data <- rbind(pref_elicitation_object$data, strategy_outcomes[strategies_compared,..TARGETS])
  return(lapply(1:2, function(jj) make_strategy_forfront_preftab(jj)))
  #This next part is important and I think needs wrapping in future because we want to pass the data back before this is done, as it's after the return it wont happen until done properly, but I've tested and it works.
  if(MCMCflag){
    pref_elicitation_object$update(method="adapt")
    preference_weights <<- pref_elicitation_object$posterior_mean
  }
}


#####
#Clustering for target compatible samples. 

#This should happen if preference_weights gets updated
#Or after submit_button when a new set of target compatible strategies is generated

cluster_samples <- function(){
  #CANT CLUSTER IF NOT ENOUGH SAMPLES. 
  if(nrow(target_compatible_strategies)<6){
    notif("Too few target compatible samples to cluster")
    target_compatible_strategies <- target_compatible_strategies[,cluster := 1]
    return(invisible())
  }
    tsne_projected_target_compatible_data <- Rtsne::Rtsne(scale(target_compatible_strategies[,..TARGETS], center=FALSE, scale = sqrt(abs(preference_weights))), perplexity = min(30, (nrow(target_compatible_strategies)-1.01)/3))$Y
    clustered_samples <- mclust::Mclust(tsne_projected_target_compatible_data,G=4)
    target_compatible_strategies <- target_compatible_strategies[,cluster := clustered_samples$classification]
    unique_clusters <- 1:4
    cluster_projections <- lapply(unique_clusters, function(ZZ) {
      projected <- tsne_projected_target_compatible_data %*% eigen(clustered_samples$parameters$variance$sigma[,,ZZ])$vectors
      return(projected)  
    })
    projection_lookup <- setNames(cluster_projections, unique_clusters)
    target_compatible_strategies[, c("pc_1", "pc_2") := {
      proj <- projection_lookup[[as.character(.BY$cluster)]]  # Fetch correct projection for cluster
      row_index <- .I  # Row index for correct alignment
      list(proj[row_index, 1], proj[row_index, 2])  # Assign correct rows
    }, by=cluster]
    
    return(target_compatible_strategies)
}
cluster_samples()



#Now target_compatible_strategies has a cluster column. Now we need a function to pass the 4 strategies to the front

#What happens on first preference tab launch
#Make 2 strategies to compare
#strategy i
make_strategy_forfront_altapproach <- function(index){
  if(!index %in% c(1,2,3,4))
    stop("Index should be 1, 2, 3 or 4 for alternative approaches")
  samples_in_cluster <- target_compatible_strategies[cluster == index]
  random_strategy <- samples_in_cluster[sample(1:nrow(samples_in_cluster),1)]
  tyears <- as.numeric(as.vector(Strategies[random_strategy$strategy_id,startsWith(colnames(Strategies),"plantingyear")]))+STARTYEAR
  tspecies <- as.vector(Strategies[random_strategy$strategy_id,startsWith(colnames(Strategies),"treespecie")])
  blocked_until_year <- rep(0, length(parcel_ids))
  blocked_until_year[which(parcel_ids %in% blocked_parcels$parcel_id)] <- blocked_parcels$blocked_until_year
  for_frontend <- st_sf(
    parcel_id = parcel_ids,
    geometry = FullTable$geometry,
    parcel_area = FullTable$Area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- random_strategy[,..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="Visits")] <- "Recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
    geojson = geojson
  ))
}

#This will work both for tab select and the "Sample" button click
alternative_approaches <- function(){
  if(all(target_compatible_strategies$cluster==1))
    return(lapply(1:4, function(kk) make_strategy_forfront_altapproach(1)))
  else
    return(lapply(1:4, function(kk) make_strategy_forfront_altapproach(kk)))
}
#tictoc::tic()
#check <- alternative_approaches()
#tictoc::toc()

#Exploration tab
#First we need a global variable containing the target compatible samples for a cluster. This will be amended on entering the exploration tab
tc_samples_cluster <- target_compatible_strategies[cluster==1]
#We also need a strategy_id to represent the current strategy shown. This will also be amended on entry
cluster_strat_id <- tc_samples_cluster[sample(1:nrow(tc_samples_cluster),1)]$strategy_id
#which_cluster must come from the front end (a map is selected on Alternative strategies) When we know this works, we just need to add a catch 
#that defaults to cluster 1 (easy, but dont want to add it until the click into this page works)
enter_exploration_tab <- function(which_cluster){
  if(all(target_compatible_strategies$cluster==1))
    which_cluster <- 1
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
    parcel_area = FullTable$Area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- random_strategy[,..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="Visits")] <- "Recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
    geojson = geojson
  ))
}

#enter_exploration_tab(2)

#variable must be named as something in our table. May need to add a handle to change species from English to latin
plus_button <- function(variable){
  setorderv(tc_samples_cluster, variable, order = -1) 
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
    parcel_area = FullTable$Area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- tc_samples_cluster[current_row,..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="Visits")] <- "Recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
    geojson = geojson
  ))
}

#plus_button("Carbon")
#plus_button("Cossus_cossus")
#plus_button("pc_1")

#variable must be named as something in our table. May need to add a handle to change species from English to latin
minus_button <- function(variable){
  setorderv(tc_samples_cluster, variable, order = 1) 
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
    parcel_area = FullTable$Area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  payload <- tc_samples_cluster[current_row,..TARGETS]
  names(payload)[which(names(payload)=="All")] <- "biodiversity"
  names(payload)[which(names(payload)=="Visits")] <- "Recreation"
  bio_names_latin <- names(payload)[ ! names(payload)%in% c("Carbon", "Area", "Recreation", "biodiversity")]
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
    geojson = geojson
  ))
}

#minus_button("Lichens")
