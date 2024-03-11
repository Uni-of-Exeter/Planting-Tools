# library(R.matlab)
# biodiversity_baseline <- readMat("Temp_data/es_biodiversity_baseline.mat")
# biodiversity_scenario <- readMat("Temp_data/es_biodiversity_scenario.mat")

# col_names <- attributes(biodiversity_baseline[["es.biodiversity.baseline"]])[["dimnames"]][[1]]
# names_to_keep <- c("species.prob.20", "species.prob.30", "species.prob.40", "species.prob.50")
# positions_of_names_to_keep <- unlist(lapply(names_to_keep, function(x) which(col_names == x)))


# Load all species prob results
# In MATLAB, code is
# baseline_species_prob_20 = es_biodiversity_baseline.species_prob_20;
# writematrix(baseline_species_prob_20, 'baseline_species_prob_20.csv');
# baseline_species_prob_30 = es_biodiversity_baseline.species_prob_30;
# writematrix(baseline_species_prob_30, 'baseline_species_prob_30.csv');
# baseline_species_prob_40 = es_biodiversity_baseline.species_prob_40;
# writematrix(baseline_species_prob_40, 'baseline_species_prob_40.csv');
# baseline_species_prob_50 = es_biodiversity_baseline.species_prob_50;
# writematrix(baseline_species_prob_50, 'baseline_species_prob_50.csv');
# 
# scenario_species_prob_20 = es_biodiversity_scenario.species_prob_20;
# writematrix(scenario_species_prob_20, 'scenario_species_prob_20.csv');
# scenario_species_prob_30 = es_biodiversity_scenario.species_prob_30;
# writematrix(scenario_species_prob_30, 'scenario_species_prob_30.csv');
# scenario_species_prob_40 = es_biodiversity_scenario.species_prob_40;
# writematrix(scenario_species_prob_40, 'scenario_species_prob_40.csv');
# scenario_species_prob_50 = es_biodiversity_scenario.species_prob_50;
# writematrix(scenario_species_prob_50, 'scenario_species_prob_50.csv');
# 
# climate_cells = es_biodiversity_scenario.climate_cells;
# writetable(climate_cells, 'climate_cells.csv');

# csv_species_files <- list.files("ShinyForestry/Matlab results", pattern = "^(baseline|scenario)", full.names = TRUE)
# for (x in csv_species_files) {
#   variable_name <- substring(x, 1, nchar(x) - 4)
#   variable_name <- substring(variable_name, nchar("ShinyForestry/Matlab results/") + 1)
#   assign(variable_name, read.csv(x, header = FALSE))
# }
# rm(x, variable_name)
scenario_species_prob_40 <- read.csv("ShinyForestry/Matlab results/scenario_species_prob_40.csv", header = FALSE)

# Load new2kid
new2kid <- read.csv("ShinyForestry/Matlab results/climate_cells.csv")$new2kid

# Load the 2-to-1 km^2 mapping (OLD)
# library(readxl)
# library(dplyr)
# one_to_two_km_mapping <- read_excel("ShinyForestry/Documentation/one_to_two_km_mapping.xlsx") %>%
#   mutate(xmin = `__xmin`) %>%
#   select(-`__xmin`)
# # Somes rows are completely identical, remove duplicates
# one_to_two_km_mapping <- unique(one_to_two_km_mapping)

# Load the 2-to-1 km^2 mapping (NEW)
library(dplyr)
one_to_two_km_mapping_full <- read.csv("ShinyForestry/Documentation/England_1km_to_2km.csv")
one_to_two_km_mapping <- one_to_two_km_mapping_full %>%
  mutate(xmin = X__xmin,
         share = proportion,
         x = left,
         y = bottom) %>%
  select(id, x, y, new2kid, xmin, ymin, area, share)

# Load species names
all_species_names <- colnames(read.csv("ShinyForestry/Model Data/Biodiversity/JNCC/beta_JNCC100_interact_quad.csv"))[-1]
# colnames(baseline_species_prob_20) <- all_species_names
# colnames(baseline_species_prob_30) <- all_species_names
# colnames(baseline_species_prob_40) <- all_species_names
# colnames(baseline_species_prob_50) <- all_species_names
# colnames(scenario_species_prob_20) <- all_species_names
# colnames(scenario_species_prob_30) <- all_species_names
colnames(scenario_species_prob_40) <- all_species_names
# colnames(scenario_species_prob_50) <- all_species_names

# Add new2kid to each data.frame, and change into percentages
# baseline_species_prob_20 <- data.frame(new2kid, 100 * baseline_species_prob_20)
# baseline_species_prob_30 <- data.frame(new2kid, 100 * baseline_species_prob_30)
# baseline_species_prob_40 <- data.frame(new2kid, 100 * baseline_species_prob_40)
# baseline_species_prob_50 <- data.frame(new2kid, 100 * baseline_species_prob_50)
# scenario_species_prob_20 <- data.frame(new2kid, 100 * scenario_species_prob_20)
# scenario_species_prob_30 <- data.frame(new2kid, 100 * scenario_species_prob_30)
scenario_species_prob_40 <- data.frame(new2kid, 100 * scenario_species_prob_40)
# scenario_species_prob_50 <- data.frame(new2kid, 100 * scenario_species_prob_50)

# The mapping only uses England, not Scotland -> we filter based on one_to_two_km_mapping$new2kid
library(dplyr)
new2kid_to_keep <- one_to_two_km_mapping$new2kid
# baseline_species_prob_20_filtered <- baseline_species_prob_20 %>% filter(new2kid %in% new2kid_to_keep)
# baseline_species_prob_30_filtered <- baseline_species_prob_30 %>% filter(new2kid %in% new2kid_to_keep)
# baseline_species_prob_40_filtered <- baseline_species_prob_40 %>% filter(new2kid %in% new2kid_to_keep)
# baseline_species_prob_50_filtered <- baseline_species_prob_50 %>% filter(new2kid %in% new2kid_to_keep)
# scenario_species_prob_20_filtered <- scenario_species_prob_20 %>% filter(new2kid %in% new2kid_to_keep)
# scenario_species_prob_30_filtered <- scenario_species_prob_30 %>% filter(new2kid %in% new2kid_to_keep)
scenario_species_prob_40_filtered <- scenario_species_prob_40 %>% filter(new2kid %in% new2kid_to_keep)
# scenario_species_prob_50_filtered <- scenario_species_prob_50 %>% filter(new2kid %in% new2kid_to_keep)

# species_names <- c("Sciurus_vulgaris", "Acanthis_cabaret")
species_names <- all_species_names
colnames_mean <- paste0("BioMean_", species_names)
colnames_sd <- paste0("BioSD_", species_names)

# DO FOR 2050, -> "_40", see fcn_create_model_matrix_jncc.m
# '_20' (2020-2029 land use)
# '_30' (2030-2039 land use)
# '_40' (2040-2049 land use)
# '_50' (2050-2059 land use)

# new2kid_to_keep has 14732 ids, but
# baseline_species_prob_40 %>% filter(new2kid %in% new2kid_to_keep) only has 3700 rows
# So a lot of new2kids from one_to_two_km_mapping are not present in the MATLAB results
probs_species <- merge(scenario_species_prob_40_filtered, one_to_two_km_mapping, by = "new2kid", all = TRUE) %>%
  # Reorder columns and remove useless ones
  dplyr::select(new2kid, id, share, x, y, xmin, ymin, any_of(species_names)) %>%
  # Assume uniformity, multiply probability by share
  mutate(across(all_of(species_names), ~ . * share))

library(readr)
FullTable <- FullTable_old <- read_csv("ShinyForestry/FullTableNew.csv")
# Load and modify the FullTable
FullTable[, colnames_mean] <- NA
FullTable[, colnames_sd] <- 0
FullTable <- FullTable %>%
  # Adjust the x,y values from FullTable (JULES uses centroids, JNCC uses bottom-left corners)
  # remove 500 from x,y in JULES to match JNCC
  # Column used for merging with probs_species
  # mutate(xybgn = paste0(xbgn - 500, ybgn - 500))
  mutate(xybgn = paste0(xbgn, ybgn))

probs_species <- probs_species %>%
  mutate(xybgn = paste0(x, y))

length(which(FullTable$xybgn %in% probs_species$xybgn))

for (i in 1:length(species_names)) {
  specie_name <- species_names[i]
  colname_mean <- colnames_mean[i]
  
  # Replace NA values with global mean
  mean_of_probs <- mean(probs_species[, specie_name], na.rm = TRUE)
  na_indices <- which(is.na(probs_species[, specie_name]))
  probs_species[na_indices, specie_name] <- mean_of_probs
  
  for (xybgn in unique(FullTable$xybgn)) {
    rows_fulltable <- which(FullTable$xybgn == xybgn)
    rows_probs_species <- which(probs_species$xybgn == xybgn)
    
    if (length(rows_probs_species) == 0) {
      # If we don't have a probability, fill in with global mean for that specie
      FullTable[rows_fulltable, colname_mean] <- mean_of_probs
    } else {
      # Fill all matching cells of FullTable with the mean of all matching cells of probs_species
      FullTable[rows_fulltable, colname_mean] <- mean(probs_species[rows_probs_species, specie_name])
    }
  }
}

FullTable <- FullTable %>%
  dplyr::select(-c("...1", "BioMean", "BioSD"))
write.csv(FullTable, file = "ShinyForestry/FullTableNewResult.csv")
