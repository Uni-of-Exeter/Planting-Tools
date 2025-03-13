# Take the Biodiversity probabilities from Matlab results/scenario_species_prob_40.csv
# and merge them with BristolFullTableMerged.geojson

# Load required libraries
library(sf)
library(sp)
library(dplyr)
library(purrr)

# Intersect with GeoJSON shapes

# Load the shapefile mapping new2kid with Polygons
id_polygons <- st_read("ShinyForestry/Documentation/SEER_net2km/SEER_net2km.shp") %>%
  select(c(new2kid, geometry))

# Load the biodiversity results from Matlab
biodiversity <- read.csv("ShinyForestry/Matlab results/scenario_species_prob_40.csv", header = FALSE)

# Replace NAs with column mean
biodiversity <- biodiversity %>%
  mutate(across(everything(), ~ replace(.x,
                                        is.na(.x),
                                        mean(.x, na.rm = TRUE))))

# Load new2kid
new2kid <- read.csv("ShinyForestry/Matlab results/climate_cells.csv")$new2kid

# Load species names
all_species_names <- colnames(read.csv("ShinyForestry/Model Data/Biodiversity/JNCC/beta_JNCC100_interact_quad.csv"))[-1]
colnames(biodiversity) <- all_species_names

# Read the GeoJSON polygon layer
polygons_jules <- st_read("ShinyForestry/BristolFullTableMerged.geojson") %>%
  # Duplicate the geometry column
  mutate(geometry_jules = geometry) %>%
  # # Only keep useful columns
  select(-starts_with("Bio"))
  # select(c(JulesMean, JulesSD,
  #          VisitsMean, VisitsSD,
  #          geometry, geometry_jules))

# Add new2kid to each data.frame, and change into percentages
polygons_bio <- data.frame(new2kid, 100 * biodiversity) %>%
  # Add polygons
  left_join(id_polygons, by = "new2kid") %>%
  # Transform to sf object
  st_as_sf() %>%
  # Convert the CRS
  st_transform(crs = st_crs(polygons_jules)) %>%
  # Duplicate the geometry column
  mutate(geometry_bio = geometry)

rm(biodiversity, new2kid)

# Validity of geometries
if (!all(st_is_valid(polygons_bio))) {
  polygons_bio <- st_make_valid(polygons_bio)
}
if (!all(st_is_valid(polygons_jules))) {
  polygons_jules <- st_make_valid(polygons_jules)
}

# Perform the intersection (cartesian sense, all possible intersections)
polygons_bio$polygon_id_bio <- seq_along(polygons_bio$geometry)
polygons_jules$polygon_id_jules <- seq_along(polygons_jules$geometry)
# TODO: parallelize? Filter -> check how Bertrand does it
intersection <- st_intersection(polygons_bio, polygons_jules)

# data.frame with SD = 0 for species
df0 <- as.data.frame(matrix(0, ncol = length(all_species_names)))
colnames(df0) <- paste0("BioSD_", all_species_names)

# Look at the area difference rowwise for mutate
compute_difference_area <- function(geom1, geom2) {
  diff_geom <- st_difference(geom1, geom2)
  if (!is_empty(diff_geom)) {
    return(st_area(diff_geom))
  } else {
    return(0)  # Return 0 if there is no difference
  }
}

# Calculate the proportion of areas (intersection / bio)
FullTable <- intersection %>%
  
  # Calculate the areas
  mutate(area_bio = st_area(geometry_bio),
         area_jules = st_area(geometry_jules),
         area_intersection = st_area(geometry)) %>%
  
  # Calculate the ratio of areas
  mutate(proportion_intersection_in_bio = area_intersection / area_bio,
         proportion_intersection_in_jules = area_intersection / area_jules) %>%
  
  # Assume uniformity, multiply probability by proportion
  mutate(across(all_of(all_species_names),
                ~ .x * proportion_intersection_in_bio)) %>%

  # Rename columns, add BioMean_ and BioSD_ to species
  rename_with(.fn = ~ paste0("BioMean_", .x), .cols = all_of(all_species_names)) %>%
  
  # Add empty SD columns for species
  bind_cols(df0) %>%
  
  as_tibble() %>%
  
  # Group by polygon_id_jules
  group_by(polygon_id_jules) %>%
  summarize(
    across(paste0("BioMean_", all_species_names),
           ~ mean(.x * area_jules / area_intersection)),
    
    across(paste0("BioSD_", all_species_names),
           ~ 0),
    
    geometry_union = st_union(geometry),
    geometry_jules = st_union(geometry_jules),
    
    polygon_id_jules = mean(polygon_id_jules)
  ) %>%

  mutate(area_diff = map2_dbl(geometry_union, geometry_jules, compute_difference_area)) %>%
  
  # Remove useless columns
  as_tibble() %>%
  select(c(starts_with("Bio"), polygon_id_jules, area_diff)) %>%
  
  # Merge back to original table
  left_join(polygons_jules, by = "polygon_id_jules") %>%
  
  select(-c(polygon_id_jules, starts_with("geometry_")))
  
rm(polygons_bio, polygons_jules)

if (any(FullTable$area_diff >= 0.1)) {
  stop("The merged geometries from the intersections do not sum the ones of the elicitor (jules)")
}
rm(df0)

FullTable <- FullTable %>% select(-area_diff)

write.csv(FullTable, file = "ShinyForestry/FullTableWithBio.csv")
