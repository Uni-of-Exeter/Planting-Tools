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

# Load packages
packages <- c("car", "shinyjs", "shiny", "shinyjqui", "leaflet", "sf", "ggplot2",
              "geosphere", "feather", "readr", "dplyr", "tidyverse", "gsubfn",
              "ggpubr", "comprehenr", "Rtsne", "mclust", "seriation", "jsonlite",
              "viridis", "ggmap", "shinyjqui", "MASS", "shinyWidgets", "truncnorm",
              "GGally", "purrr", "sp", "colorspace", "rjson", "arrow", "lwgeom",
              "mvtnorm", "dplyr")
lib <- normalizePath(Sys.getenv("R_LIBS_USER"))
repo <- "https://cran.rstudio.com/"
# update.packages(lib.loc = lib, repos = repo)
if (!require("prefeR")) {
  install.packages("prefeR", lib = lib, repos = repo)
}
loadNamespace("prefeR")
# Load the packages already installed
packages_status <- sapply(packages, require, character.only = TRUE, quietly = TRUE)
packages_to_install <- packages[packages_status == FALSE]
# Remove packages that failed to load if they are already there
try(remove.packages(packages_to_install, lib = lib))
# Install packages
install.packages(packages_to_install, lib = lib, repos = repo)
# Load packages
sapply(packages, library, character.only = TRUE, quietly = TRUE)


#  SavedVec <- rep(0, 47)
#SelecTargetCarbon <- 240;      SelecTargetBio <- 19;SelecTargetArea <- 13890596;SelecTargetVisits <- 17
#SelecTargetCarbon <- 1000;      SelecTargetBio <- 1100;SelecTargetArea <- 1000000000;SelecTargetVisits <- 1000000
#SelectedDropdown <- "Ennerdale"

# FolderSource <- "ShinyForestry/"
FolderSource <- normalizePath(getwd())
if (!grepl("ShinyForestry", FolderSource)) {
  FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
}

source(normalizePath(file.path(FolderSource, "functions.R")))
# species_names <- colnames(read.csv(paste0(FolderSource, "Model Data/Biodiversity/JNCC/beta_JNCC100_interact_quad.csv")))[-1]
# english_species_names <- c("Lesser Redpoll", "Skylark", "Tree Pipit", "Bittern", "Nightjar", "Yellowhammer", "Reed Bunting", "Grasshopper Warbler", "Woodlark", "Yellow Wagtail", "Spotted Flycatcher", "Curlew", "Grey Partridge", "Wood Warbler", "Turtle Dove", "Ring Ouzel", "Lapwing", "Adder", "Mountain Bumblebee", "Water Beetle sp.", "Noble Chafer", "Water Beetle sp.", "Stag Beetle", "Small Pearl-Bordered Fritillary", "Small Heath", "Large Heath", "Small Blue", "Mountain Ringlet", "Dingy Skipper", "Grayling", "Wall", "White Admiral", "White-Letter Hairstreak", "Speckled Bush Cricket", "Bog Bush Cricket", "Goat Moth", "Grey Dagger", "Green-brindled Crescent", "Brindled Ochre", "Red Carpet", "Plaited Door Snail", "Kentish Snail", "Hollowed Glass Snail", "Lichen subsp.", "Lichen sp.", "Lichen sp.", "Lichen sp.", "String-Of-Sausage Lichen", "Barbastelle bat", "Wildcat", "European hare", "Mountain Hare", "Pine Marten", "Harvest Mouse", "Hazel Dormouse", "Polecat", "Bechstein's bat", "Noctule Bat", "Brown Long-eared Bat", "Greater Horseshoe Bat", "Lesser Horseshoe Bat", "Eurasian red squirrel", "Field bugloss", "Bog Rosemary", "Mountain bearberry", "Green spleenwort", "Frosted Orache", "Saltmarsh Flat-Sedge", "Sea Rocket", "Clustered Bellflower", "Long-Bracted Sedge", "Tall Bog-Sedge", "Lesser Centaury", "Field Mouse-Ear", "Woolly Thistle", "Spurge-Laurel", "Broad-Leaved Cottongrass", "Common Ramping-Fumitory", "Petty Whin", "Dyer's Greenweed", "Dwarf Cudweed", "Creeping Lady's-Tresses", "Marsh St John's-Wort", "Cut-Leaved Dead-Nettle", "Lyme Grass", "Stag's-Horn Clubmoss", "Neottia nidus-avis Bird's-Nest Orchid", "Bird's-Foot", "Serrated Wintergreen", "Mountain Sorrel", "Intermediate Wintergreen", "Allseed", "Round-Leaved Crowfoot", "Rue-Leaved Saxifrage", "Pepper-Saxifrage", "Large Thyme", "Small-Leaved Lime", "Strawberry Clover", "Knotted Clover", "Small Cranberry")
# english_species_names <- add_suffix_to_duplicates(english_species_names)

NAME_CONVERSION <- matrix(data = c("Birds", "Acanthis cabaret", "Lesser Redpoll",
                                   "Birds", "Alauda arvensis", "Skylark",
                                   "Birds", "Anthus trivialis", "Tree Pipit",
                                   "Birds", "Botaurus stellaris", "Bittern",
                                   "Birds", "Caprimulgus europaeus", "Nightjar",
                                   "Birds", "Emberiza citrinella", "Yellowhammer",
                                   "Birds", "Emberiza schoeniclus", "Reed Bunting",
                                   "Birds", "Locustella naevia", "Grasshopper Warbler",
                                   "Birds", "Lullula arborea", "Woodlark",
                                   "Birds", "Motacilla flava subsp flavissima", "Yellow Wagtail",
                                   "Birds", "Muscicapa striata", "Spotted Flycatcher",
                                   "Birds", "Numenius arquata", "Curlew",
                                   "Birds", "Perdix perdix", "Grey Partridge",
                                   "Birds", "Phylloscopus sibilatrix", "Wood Warbler",
                                   "Birds", "Streptopelia turtur", "Turtle Dove",
                                   "Birds", "Turdus torquatus", "Ring Ouzel",
                                   "Birds", "Vanellus vanellus", "Lapwing",
                                   "Herptiles", "Vipera berus", "Adder",
                                   "Invertebrate - bees", "Bombus monticola", "Mountain Bumblebee",
                                   "Invertebrate - beetles", "Cercyon convexiusculus", "Water Beetle sp.",
                                   "Invertebrate - beetles", "Gnorimus nobilis", "Noble Chafer",
                                   "Invertebrate - beetles", "Liopterus haemorrhoidalis", "Water Beetle sp.",
                                   "Invertebrate - beetles", "Lucanus cervus", "Stag Beetle",
                                   "Invertebrate - butterflies", "Boloria selene", "Small Pearl-Bordered Fritillary",
                                   "Invertebrate - butterflies", "Coenonympha pamphilus", "Small Heath",
                                   "Invertebrate - butterflies", "Coenonympha tullia", "Large Heath",
                                   "Invertebrate - butterflies", "Cupido minimus", "Small Blue",
                                   "Invertebrate - butterflies", "Erebia epiphron", "Mountain Ringlet",
                                   "Invertebrate - butterflies", "Erynnis tages", "Dingy Skipper",
                                   "Invertebrate - butterflies", "Hipparchia semele", "Grayling",
                                   "Invertebrate - butterflies", "Lasiommata megera", "Wall",
                                   "Invertebrate - butterflies", "Limenitis camilla", "White Admiral",
                                   "Invertebrate - butterflies", "Satyrium w-album", "White-Letter Hairstreak",
                                   "Invertebrate - crickets", "Leptophyes punctatissima", "Speckled Bush Cricket",
                                   "Invertebrate - crickets", "Metrioptera brachyptera", "Bog Bush Cricket",
                                   "Invertebrate - moths", "Cossus cossus", "Goat Moth",
                                   "Invertebrate - moths", "Acronicta psi", "Grey Dagger",
                                   "Invertebrate - moths", "Allophyes oxyacanthae", "Green-brindled Crescent",
                                   "Invertebrate - moths", "Dasypolia templi", "Brindled Ochre",
                                   "Invertebrate - moths", "Xanthorhoe decoloraria", "Red Carpet",
                                   "Invertebrate - snails", "Cochlodina laminata", "Plaited Door Snail",
                                   "Invertebrate - snails", "Monacha cantiana", "Kentish Snail",
                                   "Invertebrate - snails", "Zonitoides excavatus", "Hollowed Glass Snail",
                                   "Lichens", "Anaptychia ciliaris subsp ciliaris", "Lichen subsp.",
                                   "Lichens", "Leptogium brebissonii", "Lichen sp.",
                                   "Lichens", "Parmeliella testacea", "Lichen sp.",
                                   "Lichens", "Pseudocyphellaria intricata", "Lichen sp.",
                                   "Lichens", "Usnea articulata", "String-Of-Sausage Lichen",
                                   "Mammals", "Barbastella barbastellus", "Barbastelle bat",
                                   "Mammals", "Felis silvestris", "Wildcat",
                                   "Mammals", "Lepus europaeus", "European hare",
                                   "Mammals", "Lepus timidus", "Mountain Hare",
                                   "Mammals", "Martes martes", "Pine Marten",
                                   "Mammals", "Micromys minutus", "Harvest Mouse",
                                   "Mammals", "Muscardinus avellanarius", "Hazel Dormouse",
                                   "Mammals", "Mustela putorius", "Polecat",
                                   "Mammals", "Myotis bechsteinii", "Bechstein's bat",
                                   "Mammals", "Nyctalus noctula", "Noctule Bat",
                                   "Mammals", "Plecotus auritus", "Brown Long-eared Bat",
                                   "Mammals", "Rhinolophus ferrumequinum", "Greater Horseshoe Bat",
                                   "Mammals", "Rhinolophus hipposideros", "Lesser Horseshoe Bat",
                                   "Mammals", "Sciurus vulgaris", "Eurasian red squirrel",
                                   "Vascular plants", "Anchusa arvensis", "Field bugloss",
                                   "Vascular plants", "Andromeda polifolia", "Bog Rosemary",
                                   "Vascular plants", "Arctostaphylos alpinus", "Mountain bearberry",
                                   "Vascular plants", "Asplenium viride", "Green spleenwort",
                                   "Vascular plants", "Atriplex laciniata", "Frosted Orache",
                                   "Vascular plants", "Blysmus rufus", "Saltmarsh Flat-Sedge",
                                   "Vascular plants", "Cakile maritima", "Sea Rocket",
                                   "Vascular plants", "Campanula glomerata", "Clustered Bellflower",
                                   "Vascular plants", "Carex extensa", "Long-Bracted Sedge",
                                   "Vascular plants", "Carex magellanica", "Tall Bog-Sedge",
                                   "Vascular plants", "Centaurium pulchellum", "Lesser Centaury",
                                   "Vascular plants", "Cerastium arvense", "Field Mouse-Ear",
                                   "Vascular plants", "Cirsium eriophorum", "Woolly Thistle",
                                   "Vascular plants", "Daphne laureola", "Spurge-Laurel",
                                   "Vascular plants", "Eriophorum latifolium", "Broad-Leaved Cottongrass",
                                   "Vascular plants", "Fumaria muralis", "Common Ramping-Fumitory",
                                   "Vascular plants", "Genista anglica", "Petty Whin",
                                   "Vascular plants", "Genista tinctoria", "Dyer's Greenweed",
                                   "Vascular plants", "Gnaphalium supinum", "Dwarf Cudweed",
                                   "Vascular plants", "Goodyera repens", "Creeping Lady's-Tresses",
                                   "Vascular plants", "Hypericum elodes", "Marsh St John's-Wort",
                                   "Vascular plants", "Lamium hybridum", "Cut-Leaved Dead-Nettle",
                                   "Vascular plants", "Leymus arenarius", "Lyme Grass",
                                   "Vascular plants", "Lycopodium clavatum", "Stag's-Horn Clubmoss",
                                   "Vascular plants", "Neottia nidus-avis", "Bird's-Nest Orchid",
                                   "Vascular plants", "Ornithopus perpusillus", "Bird's-Foot",
                                   "Vascular plants", "Orthilia secunda", "Serrated Wintergreen",
                                   "Vascular plants", "Oxyria digyna", "Mountain Sorrel",
                                   "Vascular plants", "Pyrola media", "Intermediate Wintergreen",
                                   "Vascular plants", "Radiola linoides", "Allseed",
                                   "Vascular plants", "Ranunculus omiophyllus", "Round-Leaved Crowfoot",
                                   "Vascular plants", "Saxifraga tridactylites", "Rue-Leaved Saxifrage",
                                   "Vascular plants", "Silaum silaus", "Pepper-Saxifrage",
                                   "Vascular plants", "Thymus pulegioides", "Large Thyme",
                                   "Vascular plants", "Tilia cordata", "Small-Leaved Lime",
                                   "Vascular plants", "Trifolium fragiferum", "Strawberry Clover",
                                   "Vascular plants", "Trifolium striatum", "Knotted Clover",
                                   "Vascular plants", "Vaccinium microcarpum", "Small Cranberry"),
                          ncol = 3, byrow = TRUE)
NAME_CONVERSION <- data.frame(Specie = NAME_CONVERSION[, 2],
                              English_specie = add_suffix_to_duplicates(NAME_CONVERSION[, 3]),
                              Group = NAME_CONVERSION[, 1])
# Replace Invertebrate - bees/beetles/butterflies/crickets/moths by Pollinators
# Crashes on the server for some reason, so we use data.frames instead
# dplyr::mutate(Group = dplyr::case_when(grepl("bee|beetle|butterfly|cricket|moth", Group) ~ "Pollinators",
# .default = Group)) %>%
indices <- grep("bees|beetles|butterflys|crickets|moths", NAME_CONVERSION$Group)
NAME_CONVERSION[indices, "Group"] <- "Pollinators"
# dplyr::mutate(Group = dplyr::case_when(Group == "Invertebrate - bees" ~ "Pollinators",
#                                        Group == "Invertebrate - beetles" ~ "Pollinators",
#                                        Group == "Invertebrate - butterflys" ~ "Pollinators",
#                                        Group == "Invertebrate - crickets" ~ "Pollinators",
#                                        Group == "Invertebrate - moths" ~ "Pollinators",
#                                        .default = Group)) %>%
# Acanthis cabaret -> Acanthis_cabaret, and Neottia nidus-avis -> Neottia_nidus_avis
NAME_CONVERSION <- NAME_CONVERSION %>%
  dplyr::mutate(Specie_pretty = Specie,
                Group_pretty = Group,
                English_specie_pretty = English_specie,
                Specie = gsub(" |-", "_", Specie),
                English_specie = gsub(" |-", "_", English_specie),
                Group = gsub(" - ", "_", Group)) %>%
  dplyr::mutate(Group = gsub(" ", "_", Group)) %>%
  # Sort by Specie
  dplyr::arrange(Specie)
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

ElicitorAppFolder <- normalizePath(file.path(USER_PATH, "Downloads"))
# ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
JulesAppFolder <- normalizePath(file.path(FolderSource, "JulesOP"))

# Load Files
JulesMean <- arrow::read_feather(normalizePath(file.path(JulesAppFolder, "JulesApp-rcp26-06-mean-monthly.feather")))[, c("x", "y", "mean337")]
JulesSD <- arrow::read_feather(normalizePath(file.path(JulesAppFolder, "JulesApp-rcp26-06-sd-monthly.feather")))[, c("x", "y", "sd337")]
SquaresLoad <- sf::st_read(normalizePath(file.path(JulesAppFolder, "SEER", "Fishnet_1km_to_SEER_net2km.shp")))
Sqconv <- st_transform(SquaresLoad, crs = 4326)
CorrespondenceJules <- read.csv(normalizePath(file.path(JulesAppFolder, "CorrespondanceSqToJules.csv")))[, -1]
seer2km <- st_read(normalizePath(file.path(JulesAppFolder, "SEER_net2km.shp")))
jncc100 <- read.csv(normalizePath(file.path(JulesAppFolder, "beta_JNCC100_interact_quad.csv")))
speciesprob40 <-  read.csv(normalizePath(file.path(JulesAppFolder, "scenario_species_prob_40.csv")), header = FALSE)
climatecells <- read.csv(normalizePath(file.path(JulesAppFolder, "climate_cells.csv")))

cat(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "\n" ))

while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")))) {
  Sys.sleep(5)
}

if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))) {
  cat(paste(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), "found. Trying to load file \n"))
  UnZipDirName <- paste0(substr(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")),
                                1,
                                nchar(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip"))) - 4)
  )
  dir.create(UnZipDirName)
  
  while (inherits(suppressWarnings(try(unzip(normalizePath(file.path(ElicitorAppFolder, "land_parcels.shp.zip")), exdir = UnZipDirName),
                                       silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
  }
  cat(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "unzipped, processing... \n" ))
  
  shconv <- sf::st_read(normalizePath(file.path(UnZipDirName, "land_parcels.shp")))
  if (is.null(shconv$extent)) {
    shconv$extent <- "NoExtent"
  }
  st_write(shconv, normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))
  shconv <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson"))  )
} else {
  shconv <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson")))
}

cat(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "\n" ))
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))) {
  Sys.sleep(5)
}

if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))) {
  sf_use_s2(FALSE)
  lsh <- dim(shconv)[1]
  cat(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "found. Trying to load file \n"))
  
  while (inherits(suppressWarnings(try(AllUnits <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "decision_units.json")))$decision_unit_ids,
                                       silent = TRUE)),
                  "try-error")) {
    Sys.sleep(1)
  }
  cat(paste(normalizePath(file.path(ElicitorAppFolder, "decision_units.json")), "loaded, processing... \n" ))
  
  Uni <- unique(AllUnits)
  # units is the list of decision units
  FullTab <- data.frame(extent = "NoExtent", x = rep(0, length(Uni)), y = rep(0, length(Uni)), area = rep(1, length(Uni)),
                        JulesMean = rep(15, length(Uni)),
                        JulesSD = rep(1, length(Uni)), VisitsMean = rep(30, length(Uni)),
                        VisitsSD = rep(2, length(Uni)), BioMean_Sciurus_vulgaris = rep(0.5, length(Uni)),
                        BioSD_Sciurus_vulgaris = rep(0.02, length(Uni)), units = Uni)
  
  
  MER <- list()
  for (ii in 1:length(Uni)) {
    SELLL <- shconv$geometry[AllUnits == Uni[ii]]
    MER[[ii]] <- st_union(st_make_valid(SELLL))
  }
  
  
  FullTable <- st_sf(FullTab, geometry = do.call(c, MER), crs = 4326)
  # TODO: Replace the Jules Mean here
  
  keptLines <- sort(which(as.numeric(summary(sf::st_intersects(Sqconv, shconv))[, 1]) != 0))
  
  SELECTEDSquaresconv <- Sqconv$geometry[keptLines]
  LinesJules <- CorrespondenceJules[keptLines]
  # Find lines where Jules is not available
  LinesJulesNoMinus1 <- which(LinesJules == (-1))
  LinesJules[LinesJulesNoMinus1] <- 1
  SelectedJulesMeanSq <- JulesMean[CorrespondenceJules[keptLines], ]
  SelectedJulesMeanSq[LinesJulesNoMinus1] <- 0
  SelectedJulesSDSq <- JulesSD[CorrespondenceJules[keptLines], ]
  SelectedJulesSDSq[LinesJulesNoMinus1] <- 0
  
  SELECTEDSquaresconvTab <- data.frame(idSq = seq_along(SELECTEDSquaresconv))
  SELECTEDSquaresconvTab <- st_sf(SELECTEDSquaresconvTab, geometry = SELECTEDSquaresconv, crs = 4326)
  
  
  FullTableCopy <- FullTable
  FullTableCopy$idPoly <- seq_along(FullTableCopy$geometry)
  
  #st_as_sf(data.frame(geometry = SELECTEDSquaresconv))
  #st_as_sf(data.frame(FullTable))
  
  INTT <- st_intersection(st_make_valid(SELECTEDSquaresconvTab), st_make_valid(FullTableCopy))
  INTT$area <- st_area(INTT) / 1e6
  
  NBSIMS <- 500
  for (ii in 1:length(FullTableCopy$geometry)) {
    SELLLines <- INTT$idPoly == ii
    SELLSqs <- INTT$idSq[SELLLines]
    SELLWeights <- INTT$area[SELLLines]
    SellWeightsArr <- t(matrix(SELLWeights, length(SELLWeights), NBSIMS))
    
    SelJulesMeans <- SelectedJulesMeanSq$mean337[SELLSqs]
    SelJulesSDs <- SelectedJulesSDSq$sd337[SELLSqs]
    
    if (length(SelJulesMeans) > 1) {
      SimuArr <- rmvnorm(NBSIMS, mean = SelJulesMeans, sigma = diag(SelJulesSDs^2))
      FullTable$JulesMean[ii] <- sum(colMeans(SimuArr * SellWeightsArr))
      FullTable$JulesSD[ii] <- sd(rowSums(SimuArr * SellWeightsArr))
      FullTable$area[ii] <- sum(SELLWeights)
    } else if (length(SelJulesMeans) == 1) {
      SimuArr <- rnorm(NBSIMS, mean = SelJulesMeans, sd = SelJulesSDs)
      FullTable$JulesMean[ii] <- sum(colMeans(SimuArr * SellWeightsArr))
      FullTable$JulesSD[ii] <- sd(rowSums(SimuArr * SellWeightsArr))
      FullTable$area[ii] <- sum(SELLWeights)
    } else {
      FullTable$JulesMean[ii] <- 0
      FullTable$JulesSD[ii] <- 0
      FullTable$area[ii] <- sum(SELLWeights)
    }
  }
  
  # Replace Biodiversity columns with correct ones
  FullTable <- convert_bio_to_polygons_from_elicitor_and_merge_into_FullTable(Elicitor_table = FullTable,
                                                                              speciesprob40 = speciesprob40,
                                                                              seer2km = seer2km,
                                                                              jncc100 = jncc100,
                                                                              climatecells = climatecells)
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

FullTable <- st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson")))
FullTableNotAvail <- sf::st_read(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))


#shconv <- sf::st_read("d://BristolParcels.geojson")
#FullTable <- st_read("d://BristolFullTableMerged.geojson")
#FullTableNotAvail <- sf::st_read("d://BristolFullTableNotAvail.geojson")
#shconv <- sf::st_read("d://ForestryParcels.geojson")
#FullTable <- st_read("d://ForestryFullTable.geojson")
#FullTableNotAvail <- sf::st_read("d://ForestryFullTableNotAvail.geojson")

#shconv <- sf::st_read("d://PoundsgateParcels.geojson")
#FullTable <- st_read("d://PoundsgateFullTable.geojson")
#FullTableNotAvail <- sf::st_read("d://PoundsgateFullTableNotAvail.geojson")


STDMEAN <- 0.05
STDSTD <- 0.01

# Random sampling
NSamp <- 5000
simul636 <- matrix(0, NSamp, dim(FullTable)[1])
for (aaa in 1:NSamp) {
  Uniqunits <- unique(FullTable$units)
  pp <- runif(1)
  RandSamp <- rmultinom(length(Uniqunits), 1, c(pp, 1 - pp))[1, ]
  for (bbb in 1:length(Uniqunits)) {
    simul636[aaa, FullTable$units == Uniqunits[bbb]] <- RandSamp[bbb]
  }
}

cat(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "\n" ))
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
  Sys.sleep(5)
}
cat(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file \n"))


# # Move rows from FullTableNotAvail to FullTable with blank data
# FullTableAdd <- with(FullTableNotAvail, data.frame("extent" = extent,
# "id" = id,
# "area" = 1,
# x = lgn.1,
# y = lat.1,
# xbgn = 0,
# ybgn = 0,
# lgn.1 = lgn.1, lgn.2 = lgn.2, lgn.3 = lgn.3, lgn.4 = lgn.4, lgn.5 = lgn.5, lat.1 = lat.1, lat.2 = lat.2, lat.3 = lat.3, lat.4 = lat.4, lat.5 = lat.5,
# JulesMean = 0, JulesSD = 0,
# VisitsMean = 0, VisitsSD = 0)) %>%
# mutate(across(x:lat.5, as.numeric)) %>%
# mutate(across(c(id, VisitsMean), as.integer)) %>% as_tibble()

# cols <- colnames(FullTable)
# FullTableAdd <- merge(FullTable, FullTableAdd, all = TRUE)
# FullTableAdd <- FullTableAdd[, cols]
# FullTableAdd$xybgn <- 1
# rows_na <- which(is.na(FullTableAdd[, paste0("BioMean_", NAME_CONVERSION[1, "Specie"])]))
# # Check which columns contain NA values
# columns_with_na <- colSums(is.na(FullTableAdd))
# colnames_with_na <- colnames(FullTableAdd)[columns_with_na > 0]
# # Replace NA with 0
# FullTableAdd[rows_na, colnames_with_na] <- 0
# FullTable <- FullTableAdd

# old_cols <- colnames(FullTableNotAvail)
# FullTableNotAvail <- data.frame(0)
# FullTableNotAvail[, old_cols] <- 0
# FullTableNotAvail <- FullTableNotAvail[, -1]

alphaLVL <- 0.9
MaxRounds <- 5
ConvertSample <- sample(1:5000, 200)

# Read the outcomes from the Elicitor app
while (inherits(suppressWarnings(try(outcomes <- rjson::fromJSON(file = normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))
                                     , silent = TRUE)),
                "try-error")) {
  Sys.sleep(1)
}
cat(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "loaded, processing... \n" ))

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
#groups <- base::intersect(SPECIES_ENGLISH, c(unique(NAME_CONVERSION$Group), "All"))
#indices_species_english_in_NAME_CONVERSION <- which(NAME_CONVERSION$English_specie %in% SPECIES_ENGLISH)
#SPECIES <- c(NAME_CONVERSION[indices_species_english_in_NAME_CONVERSION, "Specie"],
#            groups)

# indices_groups_english_in_name_conversion <- which(SPECIES_ENGLISH %in%  c(unique(name_conversion$Group), "All"))
# indices_species_in_vec <- which(SPECIES_ENGLISH %in% name_conversion$English_specie )
# indices_species_english_in_name_conversion <- which(name_conversion$English_specie %in% SPECIES_ENGLISH)
# SPECIES <- SPECIES_ENGLISH
# SPECIES[indices_species_in_vec] <- name_conversion[indices_species_english_in_name_conversion, "Specie"]

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

# indices_groups_english_in_NAME_CONVERSION <- which(SPECIES_ENGLISH %in% c(unique(NAME_CONVERSION$Group), "All"))
# indices_species_in_NAME_CONVERSION <- which(SPECIES_ENGLISH %in% NAME_CONVERSION$English_specie)
# indices_NAME_CONVERSION_in_species_english <- which(NAME_CONVERSION$English_specie %in% SPECIES_ENGLISH)
# SPECIES <- SPECIES_ENGLISH
# SPECIES[indices_species_in_NAME_CONVERSION] <- NAME_CONVERSION[indices_NAME_CONVERSION_in_species_english, "Specie"]


# SPECIES <- c(NAME_CONVERSION[1:2, "Specie"], "Pollinators", "All")
# SPECIES_ENGLISH <- c(NAME_CONVERSION[1:2, "English_specie"], "Pollinators", "All")
N_SPECIES <- length(SPECIES)
TARGETS <- c("Carbon", SPECIES, "Area", "NbVisits")
N_TARGETS <- length(TARGETS)

# slider_list <- list(
#   sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25)
# )
# Add sliderInput("BioSliderSPECIE", "Average SPECIE % increase:", min = 0, max = 36, value = 25) for each specie

verticalLayout_params <- c(list(sliderInput("SliderMain", "Tree Carbon Stored (tonnes of CO2):", min = 0, max = 870, value = 800)),
                           lapply(SPECIES, function(x, fulltable, NAME_CONVERSION_ARG) {
                             NAME_CONVERSION <- NAME_CONVERSION_ARG
                             # max_specie <- round(max(fulltable[, paste0("BioMean_", x)]))
                             # value <- round(max_specie / 2)
                             max_specie <- 36
                             value <- 1
                             
                             # If it is a group
                             if (x %in% c(NAME_CONVERSION$Group, NAME_CONVERSION$Group_pretty, "All")) {
                               text <- paste(get_pretty_group(x, NAME_CONVERSION), "(Change in Species Richness)")
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

JulesMean <- 0;JulesSD <- 0;SquaresLoad <- 0;Sqconv <- 0;CorrespondenceJules <- 0;seer2km <- 0;jncc100 <- 0;speciesprob40 <- 0;climatecells <- 0;
gc()

ui <- fluidPage(useShinyjs(), tabsetPanel(id = "tabs",
                                          tabPanel("Maps", fluidPage(fluidRow(
                                            column(9,
                                                   #  tags$head(tags$style(HTML("#map {pointer-events: none;}"))),
                                                   #tags$style("#inSelect {color: white; background-color: transparent; border: white;}"),
                                                   selectInput("inSelect", "area", sort(unique(c(FullTable$extent, FullTableNotAvail$extent))), FullTable$extent[1]),
                                                   #fluidRow(column(4, selectInput("ColourScheme", "Colour Scheme", c("blue/red", "rainbow dark/light",
                                                   #                                                              "rainbow dark/red",
                                                   #                                                             "Terrain darkened/lightened",
                                                   #                                                            "Terrain darkened/red",
                                                   #                                                           "Viridis darkened/red"))),
                                                   #column(4, sliderInput("Darken", "Darkening Factor:", min = -100, max = 100, value = 70)),
                                                   #column(4, sliderInput("Lighten", "Lightening Factor:", min = -100, max = 100, value = 50))),
                                                   jqui_resizable(leafletOutput("map", height = 800, width = "100%"))
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
                                          tabPanel("Exploration", id = "Exploration",
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
                                          ),
                                          tabPanel("Clustering", id = "Clustering",
                                                   fluidPage(
                                                     shinyjs::hidden(
                                                       fluidRow(12, checkboxInput("Trigger", "", value = FALSE, width = NULL))),
                                                     conditionalPanel(
                                                       condition = "input.Trigger == true",
                                                       fluidRow(
                                                         column(6, verticalLayout(jqui_resizable(leafletOutput("ClusterPage")), actionButton("choose1", "choose"))
                                                         ),
                                                         column(6, verticalLayout(jqui_resizable(leafletOutput("ClusterPage2")), actionButton("choose2", "choose"))
                                                         )
                                                       )),
                                                     conditionalPanel(
                                                       condition = "input.Trigger == false", fluidRow(
                                                         column(12, jqui_resizable(plotOutput("plotOP1"))))
                                                     )
                                                   ))
))

server <- function(input, output, session, SPECIES_ARG1 = SPECIES, SPECIES_ENGLISH_ARG1 = SPECIES_ENGLISH, N_TARGETS_ARG1 = N_TARGETS,
                   NAME_CONVERSION_ARG1 = NAME_CONVERSION) {
  # hideTab(inputId = "tabs", target = "Exploration")
  hideTab(inputId = "tabs", target = "Clustering")
  
  SPECIES <- SPECIES_ARG1
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG1
  N_SPECIES <- length(SPECIES)
  N_TARGETS <- N_TARGETS_ARG1
  NAME_CONVERSION <- NAME_CONVERSION_ARG1
  
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
      specie_english <- SPECIES_ENGLISH[i]
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
  #  ColourScheme <- reactiveVal("Viridis darkened/red")
  # observeEvent(input$Darken, {
  #    ColorDarkeningFactor(input$Darken/100)
  #  })
  # observeEvent(input$Lighten, {
  #    ColorLighteningFactor(input$Lighten/100)
  #  })
  
  output$FirstMapTxt <- renderText({Text1()})
  output$SecondMapTxt <- renderText({Text2()})
  output$ThirdMapTxt <- renderText({Text3()})
  output$FourthMapTxt <- renderText({Text4()})
  
  randomValue <- eventReactive({
    input$random
    input$tabsetPanel == "Exploration"
  }, {
    runif(1)
  })
  ClickedVector <- reactiveVal(NULL)
  AreaSelected0 <- reactiveVal(NULL)
  CarbonSelected0 <- reactiveVal(NULL)
  # RedSquirrelSelected0 <- reactiveVal(NULL)
  for (x in SPECIES) {
    var_name <- paste0(x, "Selected0")
    assign(var_name, reactiveVal(NULL))
  }
  VisitsSelected0 <- reactiveVal(NULL)
  CarbonSelectedSD0 <- reactiveVal(NULL)
  # RedSquirrelSelectedSD0 <- reactiveVal(NULL)
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
  #inputOptions(intput, 'Trigger', suspendWhenHidden = FALSE)
  
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
  
  
  
  observeEvent(input$inSelect, {
    SelectedDropdown <- input$inSelect
    ClickedVector(NULL)
    AreaSelected0(NULL)
    CarbonSelected0(NULL)
    # RedSquirrelSelected0(NULL)
    for (x in SPECIES) {
      fun <- get(paste0(x, "Selected0"))
      fun(NULL)
    }
    VisitsSelected0(NULL)
    
    CarbonSelectedSD0(NULL)
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
      CarbonSelected <- (FullTable$JulesMean[FullTable$extent == SelectedDropdown])
      # RedSquirrelSelected <- FullTable$BioMean_Sciurus_vulgaris[FullTable$extent == SelectedDropdown]
      for (x in SPECIES) {
        biomean_var <- paste0("BioMean_", x)
        var_name <- paste0(x, "Selected")
        # value <- FullTable[FullTable$extent == SelectedDropdown, biomean_var]
        value <- FullTable[[biomean_var]][FullTable$extent == SelectedDropdown]
        assign(var_name, value)
      }
      VisitsSelected <- FullTable$VisitsMean[FullTable$extent == SelectedDropdown]
      
      CarbonSelectedSD <- (FullTable$JulesSD[FullTable$extent == SelectedDropdown])
      # RedSquirrelSelectedSD <- FullTable$BioSD_Sciurus_vulgaris[FullTable$extent == SelectedDropdown]
      for (x in SPECIES) {
        biosd_var <- paste0("BioSD_", x)
        var_name <- paste0(x, "SelectedSD")
        # value <- FullTable[FullTable$extent == SelectedDropdown, biosd_var]
        value <- FullTable[[biosd_var]][FullTable$extent == SelectedDropdown]
        assign(var_name, value)
      }
      VisitsSelectedSD <- FullTable$VisitsSD[FullTable$extent == SelectedDropdown]
      
      
      
      ClickedVector(rep(0, dim(SelectedSquares)[1]))
      AreaSelected0(AreaSelected)
      CarbonSelected0(CarbonSelected)
      # RedSquirrelSelected0(RedSquirrelSelected)
      for (x in SPECIES) {
        fun <- get(paste0(x, "Selected0"))
        arg <- get(paste0(x, "Selected"))
        fun(arg)
      }
      VisitsSelected0(VisitsSelected)
      
      CarbonSelectedSD0(CarbonSelectedSD)
      # RedSquirrelSelectedSD0(RedSquirrelSelectedSD)
      for (x in SPECIES) {
        fun <- get(paste0(x, "SelectedSD0"))
        arg <- get(paste0(x, "SelectedSD"))
        fun(arg)
      }
      VisitsSelectedSD0(VisitsSelectedSD)
      
      updateSliderInput(session, "SliderMain", max = trunc(sum(CarbonSelected)), value = trunc(sum(CarbonSelected)))
      # updateSliderInput(session, "BioSlider", max = trunc(100*mean(RedSquirrelSelected))/100, value = trunc(100*mean(RedSquirrelSelected))/100, step = 0.01)
      for (x in SPECIES) {
        bioslider <- paste0("BioSlider", x)
        specie_names <- paste0(x, "Selected")
        specie_selected <- get(specie_names)
        max_bioslider <- trunc(mean(specie_selected))
        if (is.nan(max_bioslider)) {
          max_bioslider <- 0
        }
        max_areaslider <- trunc(100*sum(AreaSelected))/100
        if (is.nan(max_areaslider)) {
          max_areaslider <- 0
        }
        max_visitsslider <- trunc(mean(VisitsSelected))
        if (is.nan(max_visitsslider)) {
          max_visitsslider <- 0
        }
        updateSliderInput(session, bioslider, max = max_bioslider, value = max_bioslider, step = 0.5)
      }
      updateSliderInput(session, "AreaSlider", max = max_areaslider, value = max_areaslider, step = 0.5)
      updateSliderInput(session, "VisitsSlider", max = max_visitsslider, value = max_visitsslider)
    }
    
    
    
    
  })
  
  
  observeEvent(input$tabs == "Clustering", {
    
    SavedVec <- ClickedVector()
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
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
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
                                     input_areaSlider_multiplicative_coefficient = FALSE)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      SelectedSimMatGlobal <<- SelectedSimMat2
      
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
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
      
      priors <- c(prefeR::Normal(125, 60),
                  prefeR::Normal(5, 3))
      for (i in 1:N_SPECIES) {
        priors <- c(priors, prefeR::Normal(5, 5))
      }
      priors <- c(priors, prefeR::Normal(8, 5))
      pref <<- prefeR::prefEl(data = datAll2,
                              priors = priors)
      
      UniqueBinCodes <- unique(DatBinaryCode)
      
      if (dim(AtleastOneDat)[1] >= 250)
      {
        NbRoundsMax(MaxRounds)
        
        LinesToCompare <- matrix(1, MaxRounds, 2)
        #  indd <- 1
        
        #  for (iws in 1:(as.integer(trunc(length(UniqueBinCodes)/2)))) {
        #  LinesToCompare[iws, 1] <- which(DatBinaryCode == UniqueBinCodes[indd])[1]
        # indd <- indd + 1
        # LinesToCompare[iws, 2] <- which(DatBinaryCode == UniqueBinCodes[indd])[1]
        # indd <- indd + 1
        #}
        LinesToCompare[1, ] <- sample(1:dim(datAll2)[1], 2, replace = F)
        CurrentRound(1)
        
        LinesToCompareReactive(LinesToCompare)
        SelectedLine <- list()
        SelectedLine[[1]] <- SelectedSimMat2[ConvertSample[LinesToCompare[1, 1]], ]
        SelectedLine[[2]] <- SelectedSimMat2[ConvertSample[LinesToCompare[1, 2]], ]
        
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
            
            
            SELGEOSavedVec <- SELGEOSavedVec[SavedVec == 1, ]#;gpNamesSavedVec <- gpNamesSavedVec[SavedVec]
            SELGEOSwitched <- SELGEOSwitched[(SwitchedOnCells == 1) & (SavedVec != 1), ]#;gpNamesSwitched <- gpNamesSwitched[SwitchedOnCells & (!SavedVec)]
            
            
            if (dim(SELGEORemaining)[1] > 0) {
              listMaps[[aai]] <- addPolygons(listMaps[[aai]], data = SELGEORemaining, color = SELGEORemaining$color, layerId = SELGEORemaining$layerId, weight = UnitPolygonColours)
            }
            
            
          }
          
          
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- get_ugly_specie(SPECIES[i], NAME_CONVERSION)
            specie_english <- get_ugly_english_specie(SPECIES_ENGLISH[i], NAME_CONVERSION)
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
  
  observeEvent(input$choose1, {
    observe_event_function(choose = 1, # 1 for input$choose1, 2 for input$choose2
                           input = input,
                           output = output,
                           session = session,
                           ConvertSample = ConvertSample,
                           LinesToCompareReactive = LinesToCompareReactive,
                           ClickedVector = ClickedVector,
                           # AreaSelected0 = AreaSelected0,
                           # CarbonSelected0 = CarbonSelected0,
                           # RedSquirrelSelected0 = RedSquirrelSelected0,
                           # SpeciesListSelected0 = SpeciesListSelected, # list(Acanthis_cabaret = Acanthis_cabaretSelected0, ...)
                           # VisitsSelected0 = VisitsSelected0,
                           # CarbonSelectedSD0 = CarbonSelectedSD0,
                           # RedSquirrelSelectedSD0 = RedSquirrelSelectedSD0,
                           # SpeciesListSelectedSD0 = SpeciesListSelectedSD, # list(Acanthis_cabaretSD0 = Acanthis_cabaretSelectedSD0, ...)
                           # VisitsSelectedSD0 = VisitsSelectedSD0,
                           # DatBinaryCode0 = DatBinaryCode0,
                           NbRoundsMax = NbRoundsMax,
                           CurrentRound = CurrentRound,
                           FullTable = FullTable,
                           FullTableNotAvail = FullTableNotAvail,
                           VecNbMet0 = VecNbMet0,
                           shconv = shconv,
                           SelectedSimMatGlobal = SelectedSimMatGlobal,
                           pref = pref,
                           ColourScheme = ColourScheme(),
                           ColorLighteningFactor = ColorLighteningFactor(),
                           ColorDarkeningFactor = ColorDarkeningFactor(),
                           SPECIES_ARG3 = SPECIES,
                           SPECIES_ENGLISH_ARG3 = SPECIES_ENGLISH,
                           N_TARGETS_ARG2 = N_TARGETS,
                           GreyPolygonWidth = GreyPolygonWidth,
                           UnitPolygonColours = UnitPolygonColours)
  })
  
  observeEvent(input$choose2, {
    observe_event_function(choose = 2, # 1 for input$choose1, 2 for input$choose2
                           input = input,
                           output = output,
                           session = session,
                           ConvertSample = ConvertSample,
                           LinesToCompareReactive = LinesToCompareReactive,
                           ClickedVector = ClickedVector,
                           # AreaSelected0 = AreaSelected0,
                           # CarbonSelected0 = CarbonSelected0,
                           # RedSquirrelSelected0 = RedSquirrelSelected0,
                           # SpeciesListSelected0 = SpeciesListSelected, # list(Acanthis_cabaret = Acanthis_cabaretSelected0, ...)
                           # VisitsSelected0 = VisitsSelected0,
                           # CarbonSelectedSD0 = CarbonSelectedSD0,
                           # RedSquirrelSelectedSD0 = RedSquirrelSelectedSD0,
                           # SpeciesListSelectedSD0 = SpeciesListSelectedSD, # list(Acanthis_cabaretSD0 = Acanthis_cabaretSelectedSD0, ...)
                           # VisitsSelectedSD0 = VisitsSelectedSD0,
                           # DatBinaryCode0 = DatBinaryCode0,
                           NbRoundsMax = NbRoundsMax,
                           CurrentRound = CurrentRound,
                           FullTable = FullTable,
                           FullTableNotAvail = FullTableNotAvail,
                           VecNbMet0 = VecNbMet0,
                           shconv = shconv,
                           SelectedSimMatGlobal = SelectedSimMatGlobal,
                           pref = pref,
                           ColourScheme = ColourScheme(),
                           ColorLighteningFactor = ColorLighteningFactor(),
                           ColorDarkeningFactor = ColorDarkeningFactor(),
                           SPECIES_ARG3 = SPECIES,
                           SPECIES_ENGLISH_ARG3 = SPECIES_ENGLISH,
                           N_TARGETS_ARG2 = N_TARGETS,
                           GreyPolygonWidth = GreyPolygonWidth,
                           UnitPolygonColours = UnitPolygonColours)
  })
  
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    SelectedDropdown <- input$inSelect
    SelectedRowsUnits <- FullTable$units[FullTable$extent == SelectedDropdown]
    
    
    #GEOVEC <- st_geometry_type(FullTable$geometry)
    
    if (!is.null(click$id)) {
      ChangeDone <- FALSE
      SavedVec <- ClickedVector()
      iii <- 1
      
      while ((!ChangeDone) && (iii <= length(SavedVec))) {
        if ((click$id == paste0("Square", iii))) {
          SavedVec[SelectedRowsUnits == SelectedRowsUnits[iii]] <- ifelse(SavedVec[iii] == 1, 0, 1);
          ClickedVector(SavedVec)
          ChangeDone <- TRUE
        }
        iii <- iii + 1
      }
    }
  })
  
  
  output$map <- renderLeaflet({
    #  shinyjs::hide("tabs")
    
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect#"Ennerdale"#input$inSelect#"Abbeyford"#"Ennerdale"#
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main", shconv, GreyPolygonWidth = GreyPolygonWidth)
    map <- calcBaseMap$map
    
    if (!is.null(SavedVec)) {
      
      AreaSelected <- AreaSelected0()
      CarbonSelected <- CarbonSelected0()
      # RedSquirrelSelected <- RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "Selected0"))
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "SelectedSD0"))
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     # RedSquirrelSelected = RedSquirrelSelected,
                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                     SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                     VisitsSelectedSD = VisitsSelectedSD,
                                     alphaLVL = alphaLVL)
      
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      SelecTargetCarbon <- tmp$SelecTargetCarbon
      # SelecTargetBio <- tmp$SelecTargetBio
      condition <- TRUE
      for (x in SPECIES) {
        var_name <- paste0("SelecTargetBio", x)
        value <- tmp[[var_name]]
        assign(var_name, value)
        
        condition <- condition & (SelectedSimMat2[[x]] >= value)
      }
      SelecTargetArea <- tmp$SelecTargetArea
      SelecTargetVisits <- tmp$SelecTargetVisits
      rm(tmp)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      
      SubsetMeetTargets <- SelectedSimMat2[(SelectedSimMat2$Carbon >= SelecTargetCarbon) &
                                             # (SelectedSimMat2$redsquirrel >= SelecTargetBio) &
                                             condition &
                                             (SelectedSimMat2$Area >= SelecTargetArea) &
                                             (SelectedSimMat2$Visits >= SelecTargetVisits), ]
      
      #SubsetMeetTargets <- SelectedSimMat2[Icalc$NROYTotal, ]
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        if (max(SelectedSimMat2$Carbon) != min(SelectedSimMat2$Carbon)) {
          DistSliderCarbon <- (SubsetMeetTargets$Carbon - SelecTargetCarbon) / (max(SelectedSimMat2$Carbon) - min(SelectedSimMat2$Carbon))
        } else {
          DistSliderCarbon <- (SubsetMeetTargets$Carbon - SelecTargetCarbon) / (max(SelectedSimMat2$Carbon))
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
          if (max(SelectedSimMat2[x]) != min(SelectedSimMat2[x])) {
            value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie) / (max(SelectedSimMat2[[x]]) - min(SelectedSimMat2[[x]]))
          } else {
            if (max(SelectedSimMat2[x]) != 0) {
              value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie) / (max(SelectedSimMat2[[x]]))
            } else {
              value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie)
            }
          }
          assign(var_name, value)
          DistSliderBioListDataframes[x] <- data.frame(x = value)
        }
        if (max(SelectedSimMat2$Area) != min(SelectedSimMat2$Area)) {
          DistSliderArea <- (SubsetMeetTargets$Area - SelecTargetArea) / (max(SelectedSimMat2$Area) - min(SelectedSimMat2$Area))
        } else {
          DistSliderArea <- (SubsetMeetTargets$Area - SelecTargetArea) / (max(SelectedSimMat2$Area))
        }
        if (max(SelectedSimMat2$Visits) != min(SelectedSimMat2$Visits)) {
          DistSliderVisits <- (SubsetMeetTargets$Visits - SelecTargetVisits) / (max(SelectedSimMat2$Visits) - min(SelectedSimMat2$Visits))
        } else {
          DistSliderVisits <- (SubsetMeetTargets$Visits - SelecTargetVisits) / (max(SelectedSimMat2$Visits))
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
        SelectedMins <- SubsetMeetTargets[SelecdMinRows, ]
        
        # If it is a vector, i.e. only 1 unit is available
        if (length(SavedVec) == 1) {
          result <- SelectedMins[, 1]
        } else {
          # If it is a data frame
          result <- rowSums(SelectedMins[, 1:length(SavedVec)])
        }
        SelecRow <- which.min(result)
        
        
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
          SELGEOSwitched <- SELGEOFull[, c("geometry", "layerId")]
          
          SELGEOSavedVec <- SELGEOSavedVec[SavedVec == 1, ]#;gpNamesSavedVec <- gpNamesSavedVec[SavedVec]
          SELGEOSwitched <- SELGEOSwitched[(SwitchedOnCells == 1) & (SavedVec != 1), ]#;gpNamesSwitched <- gpNamesSwitched[SwitchedOnCells & (!SavedVec)]
          SELGEORemaining <- SELGEOFull[(SavedVec == 1) | (SwitchedOnCells == 1), c("geometry", "layerId", "color")]
          
          
          
          if (dim(SELGEORemaining)[1] > 0) {
            map <- addPolygons(map, data = SELGEORemaining, color = SELGEORemaining$color, layerId = SELGEORemaining$layerId, weight = UnitPolygonColours)
          }
          
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- SPECIES_ENGLISH[i]
            selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
            selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
            addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
          }
          
          map <- map %>%
            addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2*SelectedTreeCarbonSD, 2), "<br>",
                                     # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2*SelectedBioSD, 2), "<br>",
                                     addControlText,
                                     "Area Planted: ", round(SelectedArea, 2), "<br>",
                                     "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2*SelectedVisitsSD, 2),
                                     "</p>"), position = "topright")
          
        }
      } else { map <- map %>%
        addControl(html = paste0("<p> Targets Cannot be met</p>"), position = "topright")
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
    # ChangeSliders(FALSE)
    # shinyjs::show("tabs")
    
  })
  
  output$map2 <- renderLeaflet({
    
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main2", shconv, GreyPolygonWidth = GreyPolygonWidth)
    map <- calcBaseMap$map
    
    if (!is.null(SavedVec)) {
      
      AreaSelected <- AreaSelected0()
      CarbonSelected <- CarbonSelected0()
      # RedSquirrelSelected <- RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "Selected0"))
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "SelectedSD0"))
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     # RedSquirrelSelected = RedSquirrelSelected,
                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                     SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                     VisitsSelectedSD = VisitsSelectedSD,
                                     alphaLVL = alphaLVL)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT <- 1 - pnorm(Icalc$IVEC)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      # CONDPROBA <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS)
      
      # SubsetMeetTargets <- subset_meet_targets(PROBAMAT = PROBAMAT, SelectedSimMat2 = SelectedSimMat2, CONDPROBAPositiveLIST = CONDPROBA, TARGETS = TARGETS, nb_targets_met = N_TARGETS)
      SubsetMeetTargets <- SelectedSimMat2[CONDPROBA, ]
      # SelIMAT <- Icalc$IVEC[CONDPROBA, ]
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              ColourScheme = ColourScheme(),
                                              ColorLighteningFactor = ColorLighteningFactor(),
                                              ColorDarkeningFactor = ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES,
                                              SPECIES_ENGLISH_ARG2 = SPECIES_ENGLISH,
                                              UnitPolygonColours = UnitPolygonColours)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (i in 1:length(SPECIES)) {
          specie_latin <- SPECIES[i]
          specie_english <- SPECIES_ENGLISH[i]
          selectedBiospecie <- mapresults[[paste0("SelectedBio", specie_latin)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", specie_latin)]]
          addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        Text1(paste0("Strategies that meet all ", N_TARGETS, " targets:", round(dim(SubsetMeetTargets)[1] / 5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1)))
      } else {
        Text1(paste("No strategy where all", N_TARGETS, "targets are met found"))
      }
    }
    
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
  output$map3 <- renderLeaflet({
    
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main3", shconv, GreyPolygonWidth = GreyPolygonWidth)
    map <- calcBaseMap$map
    
    if (!is.null(SavedVec)) {
      
      AreaSelected <- AreaSelected0()
      CarbonSelected <- CarbonSelected0()
      # RedSquirrelSelected <- RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "Selected0"))
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "SelectedSD0"))
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     # RedSquirrelSelected = RedSquirrelSelected,
                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                     SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                     VisitsSelectedSD = VisitsSelectedSD,
                                     alphaLVL = alphaLVL)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT <- 1 - pnorm(Icalc$IVEC)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      # CONDPROBA3PositiveLIST <- list()
      # CONDPROBA3PositiveLIST[[1]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA3PositiveLIST[[2]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA3PositiveLIST[[3]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA3PositiveLIST[[4]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA3PositiveLIST <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS)
      
      # SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[1]], ],
      #                                 NotMet = rep("Carbon", sum(CONDPROBA3PositiveLIST[[1]])))
      # # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[2]], ], NotMet = rep("redSquirrel", sum(CONDPROBA3PositiveLIST[[2]]))))
      # for (i in 1:N_SPECIES) {
      #   specie <- SPECIES[i]
      #   SubsetMeetTargets <- rbind(SubsetMeetTargets,
      #                              data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[i + 1]], ],
      #                                         NotMet = rep(specie, sum(CONDPROBA3PositiveLIST[[i + 1]]))))
      # }
      # SubsetMeetTargets <- rbind(SubsetMeetTargets,
      #                            data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[N_SPECIES + 2]], ],
      #                                       NotMet = rep("Area", sum(CONDPROBA3PositiveLIST[[N_SPECIES + 2]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets,
      #                            data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[N_SPECIES + 3]], ],
      #                                       NotMet = rep("NbVisits", sum(CONDPROBA3PositiveLIST[[N_SPECIES + 3]]))))
      SubsetMeetTargets <- subset_meet_targets(PROBAMAT = PROBAMAT, SelectedSimMat2 = SelectedSimMat2, CONDPROBAPositiveLIST = CONDPROBA3PositiveLIST, TARGETS = TARGETS, nb_targets_met = N_TARGETS)
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              ColourScheme = ColourScheme(),
                                              ColorLighteningFactor = ColorLighteningFactor(),
                                              ColorDarkeningFactor = ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES,
                                              SPECIES_ENGLISH_ARG2 = SPECIES_ENGLISH,
                                              UnitPolygonColours = UnitPolygonColours)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (i in 1:length(SPECIES)) {
          specie_latin <- SPECIES[i]
          specie_english <- SPECIES_ENGLISH[i]
          selectedBiospecie <- mapresults[[paste0("SelectedBio", specie_latin)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", specie_latin)]]
          addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        
        # Replace species Latin names with English names, and keep everything else
        targets_not_met <- str_split_1(mapresults$SelectedLine$NotMet, ", ")
        for (i in seq_along(targets_not_met)) {
          target <- targets_not_met[i]
          if (target %in% NAME_CONVERSION$Specie) {
            idx <- NAME_CONVERSION$Specie == target
            matching_english_specie <- NAME_CONVERSION[idx, "English_specie"]
            targets_not_met[i] <- matching_english_specie
          }
        }
        targets_not_met <- paste(targets_not_met, collapse = ", ")
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        
        # Text2(paste0("Strategies that meet exactly ", N_TARGETS - 1, " targets:", round(dim(SubsetMeetTargets)[1] / 5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Target Not Met:", targets_not_met))
        Text2(paste0("Strategies that meet all ", N_TARGETS, " targets:", round(dim(SubsetMeetTargets)[1] / 5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1)))
      } else {
        # Text2(paste("No strategy where exactly", N_TARGETS - 1, "targets are met found"))
        Text2(paste("No strategy where all", N_TARGETS, "targets are met found"))
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
  output$map4 <- renderLeaflet({
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main4", shconv, GreyPolygonWidth = GreyPolygonWidth)
    map <- calcBaseMap$map
    
    if (!is.null(SavedVec)) {
      
      AreaSelected <- AreaSelected0()
      CarbonSelected <- CarbonSelected0()
      # RedSquirrelSelected <- RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "Selected0"))
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "SelectedSD0"))
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     # RedSquirrelSelected = RedSquirrelSelected,
                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                     SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                     VisitsSelectedSD = VisitsSelectedSD,
                                     alphaLVL = alphaLVL)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT <- 1 - pnorm(Icalc$IVEC)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      # CONDPROBA2PositiveLIST <- list()
      # CONDPROBA2PositiveLIST[[1]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA2PositiveLIST[[2]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA2PositiveLIST[[3]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA2PositiveLIST[[4]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA2PositiveLIST[[5]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA2PositiveLIST[[6]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA2PositiveLIST <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS)
      
      # SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[1]], ], NotMet = rep("Carbon, redSquirrel", sum(CONDPROBA2PositiveLIST[[1]])))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[2]], ], NotMet = rep("Carbon, Area", sum(CONDPROBA2PositiveLIST[[2]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[3]], ], NotMet = rep("Carbon, NbVisits", sum(CONDPROBA2PositiveLIST[[3]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[4]], ], NotMet = rep("redSquirrel, Area", sum(CONDPROBA2PositiveLIST[[4]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[5]], ], NotMet = rep("redSquirrel, NbVisits", sum(CONDPROBA2PositiveLIST[[5]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[6]], ], NotMet = rep("Area, NbVisits", sum(CONDPROBA2PositiveLIST[[6]]))))
      SubsetMeetTargets <- subset_meet_targets(PROBAMAT = PROBAMAT, SelectedSimMat2 = SelectedSimMat2, CONDPROBAPositiveLIST = CONDPROBA2PositiveLIST, TARGETS = TARGETS, nb_targets_met = N_TARGETS)
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              ColourScheme = ColourScheme(),
                                              ColorLighteningFactor = ColorLighteningFactor(),
                                              ColorDarkeningFactor = ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES,
                                              SPECIES_ENGLISH_ARG2 = SPECIES_ENGLISH,
                                              UnitPolygonColours = UnitPolygonColours)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (i in 1:length(SPECIES)) {
          specie_latin <- SPECIES[i]
          specie_english <- SPECIES_ENGLISH[i]
          selectedBiospecie <- mapresults[[paste0("SelectedBio", specie_latin)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", specie_latin)]]
          addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        
        # Replace species Latin names with English names, and keep everything else
        targets_not_met <- str_split_1(mapresults$SelectedLine$NotMet, ", ")
        for (i in seq_along(targets_not_met)) {
          target <- targets_not_met[i]
          if (target %in% NAME_CONVERSION$Specie) {
            idx <- NAME_CONVERSION$Specie == target
            matching_english_specie <- NAME_CONVERSION[idx, "English_specie"]
            targets_not_met[i] <- matching_english_specie
          }
        }
        targets_not_met <- paste(targets_not_met, collapse = ", ")
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        
        # Text3(paste0("Strategies that meet exactly ", N_TARGETS - 2, " targets:", round(dim(SubsetMeetTargets)[1] / 5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Targets Not Met:", targets_not_met))
        Text3(paste0("Strategies that meet all ", N_TARGETS, " targets:", round(dim(SubsetMeetTargets)[1] / 5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1)))
      } else {
        # Text3(paste("No strategy where exactly", N_TARGETS - 2, "targets are met found"))
        Text3(paste("No strategy where all", N_TARGETS, "targets are met found"))
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
  output$map5 <- renderLeaflet({
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main5", shconv, GreyPolygonWidth = GreyPolygonWidth)
    map <- calcBaseMap$map
    
    if (!is.null(SavedVec)) {
      
      AreaSelected <- AreaSelected0()
      CarbonSelected <- CarbonSelected0()
      # RedSquirrelSelected <- RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "Selected0"))
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      # RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "SelectedSD0"))
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     # RedSquirrelSelected = RedSquirrelSelected,
                                     SpeciesListSelected = SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     # RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                     SpeciesListSelectedSD = SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                     VisitsSelectedSD = VisitsSelectedSD,
                                     alphaLVL = alphaLVL)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT <- 1 - pnorm(Icalc$IVEC)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      # CONDPROBA1PositiveLIST <- list()
      # CONDPROBA1PositiveLIST[[1]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA1PositiveLIST[[2]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA1PositiveLIST[[3]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA1PositiveLIST[[4]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA1PositiveLIST <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS)
      
      # SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[1]], ], Met = rep("Carbon", sum(CONDPROBA1PositiveLIST[[1]])))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[2]], ], Met = rep("redSquirrel", sum(CONDPROBA1PositiveLIST[[2]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[3]], ], Met = rep("Area", sum(CONDPROBA1PositiveLIST[[3]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[4]], ], Met = rep("NbVisits", sum(CONDPROBA1PositiveLIST[[4]]))))
      SubsetMeetTargets <- subset_meet_targets(PROBAMAT = PROBAMAT, SelectedSimMat2 = SelectedSimMat2, CONDPROBAPositiveLIST = CONDPROBA1PositiveLIST, TARGETS = TARGETS, nb_targets_met = N_TARGETS)
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              ColourScheme = ColourScheme(),
                                              ColorLighteningFactor = ColorLighteningFactor(),
                                              ColorDarkeningFactor = ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES,
                                              SPECIES_ENGLISH_ARG2 = SPECIES_ENGLISH,
                                              UnitPolygonColours = UnitPolygonColours)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (i in 1:length(SPECIES)) {
          specie_latin <- SPECIES[i]
          specie_english <- SPECIES_ENGLISH[i]
          selectedBiospecie <- mapresults[[paste0("SelectedBio", specie_latin)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", specie_latin)]]
          addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        
        # Replace species Latin names with English names, and keep everything else
        targets_met <- str_split_1(mapresults$SelectedLine$Met, ", ")
        for (i in seq_along(targets_met)) {
          target <- targets_met[i]
          if (target %in% NAME_CONVERSION$Specie) {
            idx <- NAME_CONVERSION$Specie == target
            matching_english_specie <- NAME_CONVERSION[idx, "English_specie"]
            targets_met[i] <- matching_english_specie
          }
        }
        targets_met <- paste(targets_met, collapse = ", ")
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        
        # Text4(paste0("Strategies that meet only ", N_TARGETS - 3, " target:", round(dim(SubsetMeetTargets)[1] / 5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Target Met:", targets_met))
        Text4(paste0("Strategies that meet all ", N_TARGETS, " targets:", round(dim(SubsetMeetTargets)[1] / 5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1)))
      } else {
        # Text4(paste("No strategy where only", N_TARGETS - 3, "target is met found"))
        Text4(paste("No strategy where all", N_TARGETS, "targets are met found"))
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
}

shinyApp(ui, server)
