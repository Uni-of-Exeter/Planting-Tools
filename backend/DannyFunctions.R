#Danny's functions for manipulating FullTable, precomputing biodiversity and getting outcomes from strategies
#MUST BE SOURCED AFTER TIM'S SOURCE FILES AS THERE IS SOME OVERWRITING OF FUNCTIONS TO IMPROVE SPEED

#Take long-form FullTable and precompute the infilling of biodiversity for annual values (via Tim's regression method)
precompute_biodiversity <- function(FullTable_long, MAXYEAR) {
  # Filter for Bio and Mean rows
  bio_rows <- FullTable_long[
    outcome_type == "Bio" & 
      statistic_name == "Mean"
  ]
  
  # Compute slope and intercept for each parcel_id and outcome_sub_type
  bio_rows_slope <- bio_rows[
    planting_year %in% c(0, MAXYEAR + 1),
    .(slope = diff(outcome_value) / (MAXYEAR + 1), # (value at MAXYEAR+1 - value at 0) / range
      intercept = outcome_value[planting_year == 0]), # value at planting_year == 0
    by = .(parcel_id, outcome_sub_type, treespecie)
  ]
  
  # Generate biodiversity scores for intermediate years (1 to MAXYEAR)
  all_years <- 1:MAXYEAR
  precomputed_bio <- bio_rows_slope[, .(
    planting_year = all_years,
    outcome_value = intercept + slope * all_years # Linear interpolation
  ), by = .(parcel_id, outcome_sub_type, treespecie)]
  
  # Combine the original rows with the new precomputed rows
  FullTable_long <- rbind(
    FullTable_long, # Keep all original rows
    precomputed_bio[, .(
      parcel_id, outcome_type = "Bio", statistic_name = "Mean", planting_year,
      treespecie, scenario = NA, outcome_value, outcome_sub_type
    )]
  )
  
  # Ensure the result is sorted by parcel_id and planting_year
  setorder(FullTable_long, parcel_id, planting_year)
  
  return(FullTable_long)
}


#Version to remove check that will always be true inside of implausibility (or check can be made outside to save time)
RRembo_project_low_dimension_to_high_dimension_basic_dw <- function(DoE_low_dimension, A, limit_log_level = LOG_LEVEL) {
  # Skip matrix size check if dimensions are always correct
  # if (ncol(A) != ncol(DoE_low_dimension)) { ... }
  
  # Skip unnecessary as.matrix() conversion if DoE_low_dimension is already a matrix
  DoE_high_dimension <- tcrossprod(DoE_low_dimension, A)[, , drop = FALSE]
  
  return(pmax(pmin(DoE_high_dimension, 1), -1))
}

#This one for species
continuous_to_multi_categorical_new <- function(values_matrix, legal_values_ordered) {
  # Get the number of categories based on the number of rows in legal_values_ordered
  num_categories <- nrow(legal_values_ordered)
  
  # Define **one** set of quantile breakpoints for all parcels
  cutoffs <- seq(-1, 1, length.out = num_categories + 1)
  
  # Apply findInterval in a **single vectorized call** for all parcels
  indices <- findInterval(values_matrix, cutoffs, rightmost.closed = TRUE)
  
  # Assign categorical values from legal_values_ordered
  col_indices <- rep(1:ncol(values_matrix), each = nrow(values_matrix))
  result <- matrix(legal_values_ordered[cbind(indices, col_indices)], 
                   nrow = nrow(values_matrix), 
                   ncol = ncol(values_matrix))
  
  return(result)
}

#This one for years
continuous_to_multi_categorical_batch_fast <- function(values_matrix, legal_values_mapping, 
                                                       unique_legal_values, cutoffs_list) {
  # Preallocate result matrix
  result <- matrix(nrow = nrow(values_matrix), ncol = ncol(values_matrix))
  
  # Process each unique set **only once**
  for (unique_idx in seq_along(unique_legal_values)) {
    # Get parcels that use this unique legal set
    parcels <- which(legal_values_mapping == unique_idx)
    
    # Get the precomputed `cutoffs`
    cutoffs <- cutoffs_list[[unique_idx]]
    
    # Apply `findInterval()` in **one batch** for all parcels using this legal set
    indices <- findInterval(values_matrix[, parcels, drop = FALSE], cutoffs, rightmost.closed = TRUE)
    
    # Assign the correct categorical values
    result[, parcels] <- unique_legal_values[[unique_idx]][indices]
  }
  
  return(result)
}

transform_DoE_high_dimension_continuous_to_strategy_rowwise_matrix_dw <- function(
    DoE_high_dimension_rowwise_matrix,
    RREMBO_HYPER_PARAMETERS,
    year_of_max_no_planting_threshold_vector,
    unique_legal_values,
    legal_values_mapping,
    cutoffs_list,
    precomputed_values
) {
  
  # Extract precomputed values
  area_possible_values_dataframe <- precomputed_values$area_possible_values_dataframe
  tree_specie_possible_values_dataframe <- precomputed_values$tree_specie_possible_values_dataframe
  static_group_size <- precomputed_values$static_group_size
  area_colnames <- precomputed_values$area_colnames
  treespecie_colnames <- precomputed_values$treespecie_colnames
  plantingyear_colnames <- precomputed_values$plantingyear_colnames
  
  # Compute group size ONCE
  group_size <- static_group_size(ncol(DoE_high_dimension_rowwise_matrix))
  indices <- 1:group_size
  
  # Area
  DoE_high_dimension_rowwise_matrix_area_normalized <- DoE_high_dimension_rowwise_matrix[, indices, drop = FALSE]
  
  # Convert Area to categorical values, uses old continuous_to_multi_categorical as unique area vals on each parcel. Could be faster
  DoE_high_dimension_categorical_area <- continuous_to_multi_categorical_new(
    values_matrix =  DoE_high_dimension_rowwise_matrix_area_normalized,
    legal_values_ordered = area_possible_values_dataframe
  )
  colnames(DoE_high_dimension_categorical_area) <- area_colnames
  
  # Year
  indices <- group_size + indices
  DoE_high_dimension_rowwise_matrix_year_normalized <- DoE_high_dimension_rowwise_matrix[, indices, drop = FALSE]
  
  # Convert Year to categorical values using batch processing
  DoE_high_dimension_categorical_year <- continuous_to_multi_categorical_batch_fast(
    DoE_high_dimension_rowwise_matrix_year_normalized,
    legal_values_mapping,
    unique_legal_values,
    cutoffs_list
  )
  colnames(DoE_high_dimension_categorical_year) <- plantingyear_colnames
  
  # Tree Specie
  indices <- group_size + indices
  DoE_high_dimension_rowwise_matrix_tree_normalized <- DoE_high_dimension_rowwise_matrix[, indices, drop = FALSE]
  
  # Convert Tree Specie to categorical values
  DoE_high_dimension_categorical_treespecie <- continuous_to_multi_categorical_new(
    values = DoE_high_dimension_rowwise_matrix_tree_normalized,
    legal_values_ordered = tree_specie_possible_values_dataframe
  )
  colnames(DoE_high_dimension_categorical_treespecie) <- treespecie_colnames
  
  # Combine
  DoE_high_dimension_categorical <- cbind(
    DoE_high_dimension_categorical_area,
    DoE_high_dimension_categorical_year,
    DoE_high_dimension_categorical_treespecie
  )
  
  return(DoE_high_dimension_categorical)
}

#First try - Imp here is 15.96ms (fastest)

get_outcomes_from_strategy <- function(parameter_vector,
                                       FullTable_long_arg,
                                       SPECIES_arg = SPECIES,
                                       SCENARIO_arg = SCENARIO,
                                       MAXYEAR_arg = MAXYEAR,
                                       NAME_CONVERSION_arg = NAME_CONVERSION) {
  
  NAME_CONVERSION <- NAME_CONVERSION_arg
  
  # Extract unique parcel_id
  outcomes <- data.table(parcel_id = unique(FullTable_long_arg$parcel_id))
  
  # Pre-slice parameter vector
  group_size <- length(parameter_vector) / 3
  outcomes[, `:=`(
    strategy_area = as.numeric(parameter_vector[1:group_size]),
    strategy_year = as.numeric(parameter_vector[(group_size + 1):(2 * group_size)]),
    strategy_treespecie = parameter_vector[(2 * group_size + 1):(3 * group_size)]
  )]
  
  # Efficiently update strategy_year to 25 where strategy_area is 0
  outcomes[strategy_area == 0, strategy_year := 25]
  
  #Merge outcomes into the table
  FullTable_long_arg[
    outcomes, 
    on = .(parcel_id), 
    `:=`(
      strategy_area = i.strategy_area,
      strategy_year = i.strategy_year,
      strategy_treespecie = i.strategy_treespecie
    )
  ]
  
  # Filter once and store the result
  filtered_table <- FullTable_long_arg[
    treespecie == strategy_treespecie & planting_year == strategy_year
  ]
  
  #filtered_table <- FullTable_long_arg[strategy_area != 0]
  #filtered_table <- filtered_table[treespecie == strategy_treespecie]
  
  # Use filtered_table in sum_visits calculation
  result <- filtered_table[
    outcome_type == "Visits",
    .(
      sum_visits = mean(outcome_value[statistic_name == "Mean"])#,
      #sum_visits_sd = sqrt(sum(outcome_value[statistic_name == "SD"]^2)) / .N (removed given not ORVAL)
    )
  ]
  
  #result <- FullTable_long_arg[
  #  strategy_area != 0 & treespecie == strategy_treespecie & outcome_type == "Visits",
  #  .(
  #    sum_visits = mean(outcome_value[statistic_name == "Mean"]),
  #    sum_visits_sd = sqrt(sum(outcome_value[statistic_name == "SD"]^2)) / .N
  #  )
  #]
  
  sum_visits <- result[, sum_visits]
  
  #Set to 2 whilst ORVAL worked on
  sum_visits_sd <- 2 #result[, sum_visits_sd]
  # Main filtering (reusable for several calculations)
  #filtered_table <- FullTable_long_arg[
  #  strategy_area != 0 & 
  #    treespecie == strategy_treespecie
  #]
  
  # Area calculation
  sum_area <- sum(outcomes$strategy_area)
  
  #sum_area <- filtered_table[
  #  planting_year == strategy_year, 
  #  unique(strategy_area), by = parcel_id
  #][, sum(V1)]
  
  filtered_table_carbon <- filtered_table[strategy_area != 0 & outcome_type == "Carbon"]
  
  # Carbon mean
  sum_carbon <- filtered_table_carbon[
    statistic_name == "Mean",
    sum(outcome_value)
  ]
  
  # Carbon SD
  sum_carbon_sd <- filtered_table_carbon[
    statistic_name == "SD",
    sqrt(sum(outcome_value^2))
  ]
  
  # Biodiversity Section
  if (!any(SPECIES_arg %in% NAME_CONVERSION$Specie)) {
    sum_biodiversity <- sum_biodiversity_sd <- c()
  } else {
    # Filter biodiversity rows
    biodiversity_rows <- filtered_table[
      outcome_type == "Bio" & 
        statistic_name == "Mean"
    ]
    
    # Aggregate biodiversity scores (use colMeans if needed)
    sum_biodiversity <- biodiversity_rows[
      , .(sum_biodiversity = mean(outcome_value)), 
      by = .(outcome_sub_type)
    ]
    
    # Set SDs to zero (can be adjusted if needed)
    # Not really needed and will be replaced by model.
    #sum_biodiversity_sd <- rep(0, length(sum_biodiversity))
    sum_biodiversity <- setNames(sum_biodiversity$sum_biodiversity, sum_biodiversity$outcome_sub_type)
  }
  # Richness calculation
  richness_results <- filtered_table[
    outcome_type == "Richness",
    .(sum_richness = mean(outcome_value)), # Aggregate directly
    by = outcome_sub_type
  ]
  
  # Convert to named vector
  sum_richness <- setNames(richness_results$sum_richness, richness_results$outcome_sub_type)
  
  #sum_richness <- FullTable_long[
  #  outcome_type == "Richness" & planting_year %in% outcomes[, strategy_year], 
  #  .(sum_richness = sum(outcome_value)), 
  #  by = outcome_sub_type
  #][, setNames(sum_richness, outcome_sub_type)]
  
  # Set SDs to zero
  #Again remove as SD not needed and new model will estimate
  #sum_richness_sd <- numeric(length(sum_richness))
  return(list("sum_carbon" = sum_carbon,
              "sum_carbon_sd" = sum_carbon_sd,
              "sum_richness" = sum_richness,
              #"sum_richness_sd" = sum_richness_sd,
              "sum_biodiversity" = sum_biodiversity,
              #"sum_biodiversity_sd" = sum_biodiversity_sd,
              "sum_visits" = sum_visits,
              "sum_visits_sd" = sum_visits_sd,
              "sum_area" = sum_area
  ))
}

#Some precomputing for discretising years during REMBO
compute_legal_values <- function(year_of_planting_min_threshold_vector, 
                                 year_of_max_no_planting_threshold_vector, 
                                 MAXYEAR) {
  # Step 1: Compute legal values per parcel
  legal_values_list <- lapply(seq_along(year_of_planting_min_threshold_vector), function(parcel_idx) {
    if (year_of_max_no_planting_threshold_vector[parcel_idx] < MAXYEAR) {
      seq(year_of_planting_min_threshold_vector[parcel_idx], MAXYEAR)
    } else {
      MAXYEAR + 1
    }
  })
  
  # Step 2: Find unique sets of legal values
  unique_legal_values <- unique(legal_values_list)
  
  # Step 3: Create a mapping from parcels to unique legal sets
  legal_values_mapping <- match(sapply(legal_values_list, toString), sapply(unique_legal_values, toString))
  
  return(list(
    unique_legal_values = unique_legal_values,
    legal_values_mapping = legal_values_mapping
  ))
}

#Pre-computing elements needed for efficient get_outcomes_from_strategy
precompute_static_values <- function(FullTable, MAXYEAR) {
  
  # Precompute Area values
  area_possible_non_zero_values <- FullTable$area
  area_possible_values_dataframe <- rbind(0, area_possible_non_zero_values)
  max_ncol <- ncol(area_possible_values_dataframe)
  # Precompute Tree Specie Values
  tree_specie_possible_values_dataframe <- FullTable %>%
    colnames() %>%
    grep(pattern = "TreeSpecie", x = ., value = TRUE) %>%
    gsub(pattern = ".*TreeSpecie(.*?)_.*", x = ., replacement = "\\1", perl = TRUE) %>%
    gsub(pattern = ".*TreeSpecie(.*)", x = ., replacement = "\\1", perl = TRUE) %>%
    unique() %>%
    matrix(nrow = length(.), ncol = max_ncol)
  
  # Compute group size (based on matrix dimensions)
  static_group_size <- function(ncol_matrix) ncol_matrix / 3
  
  # Precompute column names
  area_colnames <- paste0("area_parcel_id", 1:max_ncol)
  treespecie_colnames <- paste0("treespecie_parcel_id", 1:max_ncol)
  plantingyear_colnames <- paste0("plantingyear_parcel_id", 1:max_ncol)
  
  #Precompute Ax transformation bounds. (No longer needed)
  # global_min <- rowSums(pmin(A, 0))
  # global_max <- rowSums(pmax(A, 0))
  
  return(list(
    area_possible_values_dataframe = area_possible_values_dataframe,
    tree_specie_possible_values_dataframe = tree_specie_possible_values_dataframe,
    static_group_size = static_group_size,
    area_colnames = area_colnames,
    treespecie_colnames = treespecie_colnames,
    plantingyear_colnames = plantingyear_colnames
  ))
}

#Make outcomes format into data.table
convert_outcome_to_dt <- function(outcome) {
  dt <- data.table::data.table(row_id = 1)  # Create a row to initialize the data.table
  
  for (name in names(outcome)) {
    clean_name <- gsub("^sum_", "", name)
    if (is.numeric(outcome[[name]]) && length(outcome[[name]]) == 1) {
      dt[[clean_name]] <- outcome[[name]]
    } else if (is.numeric(outcome[[name]]) && length(outcome[[name]]) > 1) {
      for (subname in names(outcome[[name]])) {
        dt[[subname]] <- outcome[[name]][subname]
      }
    }
  }
  dt[, row_id := NULL]  # Remove the temporary row_id column
  return(dt)
}

#combine get_outcomes_from_strategy and return data.table
get_outcome_dt <- function(one_strategy, FullTable_long_arg,
                           SPECIES_arg = SPECIES,
                           SCENARIO_arg = SCENARIO,
                           MAXYEAR_arg = MAXYEAR,
                           NAME_CONVERSION_arg = NAME_CONVERSION){
  SPECIES <- SPECIES_arg
  SCENARIO <- SCENARIO_arg
  MAXYEAR <- MAXYEAR_arg
  NAME_CONVERSION <- NAME_CONVERSION_arg
  outcomes <- get_outcomes_from_strategy(parameter_vector = one_strategy,
                                         FullTable_long_arg = FullTable_long_arg,
                                         SPECIES_arg = SPECIES,
                                         SCENARIO_arg = SCENARIO,
                                         MAXYEAR_arg = MAXYEAR,
                                         NAME_CONVERSION_arg = NAME_CONVERSION)
  return(convert_outcome_to_dt(outcomes))
}

# Clustering
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
    parcel_area = FullTable$area,
    planting_year = ifelse(tyears<2050,tyears,NA),
    planting_types = ifelse(tyears<2050, tspecies, NA),
    blocked_until_year = blocked_until_year,
    crs = st_crs(FullTable)
  )
  #convert to geojson
  geojson <- geojsonsf::sf_geojson(for_frontend)
  
  
  
  
  # In the update() function, it is converted to a matrix, so we need the comma to select the entire row
  payload <- pref_elicitation_object$data[comparison_index + (index-1), ]
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
  if (isFALSE(data.table::is.data.table(payload))) {
    payload <- data.table::as.data.table(t(payload))
  }
  return(list(
    values = payload,
    geojson = geojson
  ))
}

#Now target_compatible_strategies has a cluster column. Now we need a function to pass the 4 strategies to the front

#What happens on first preference tab launch
#Make 2 strategies to compare
#strategy i
make_strategy_forfront_altapproach <- function(index){
  if(!index %in% c(1,2,3,4)) {
    stop("make_strategy_forfront_altapproach(): Index should be 1, 2, 3 or 4 for alternative approaches")
  }
  if (is.null(target_compatible_strategies$cluster)) {
    target_compatible_strategies$cluster <- index
  }
  samples_in_cluster <- target_compatible_strategies[cluster == index]
  random_strategy <- samples_in_cluster[sample(1:nrow(samples_in_cluster),1)]
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
  return(list(
    values = payload,
    geojson = geojson
  ))
}