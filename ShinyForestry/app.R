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

# FolderSource <- "ShinyForestry/"
FolderSource <- normalizePath(getwd())
if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
  FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
}

source(normalizePath(file.path(FolderSource, "functions.R")))

# Load packages
libs <- unique(c(normalizePath(.libPaths()),
                 normalizePath(Sys.getenv("R_LIBS_USER")),
                 normalizePath(file.path(getwd(), "myRlibrary"))))
packages <- c("car", "shinyjs", "shiny", "shinyjqui", "leaflet", "sf", "ggplot2",
              "geosphere", "feather", "readr", "dplyr", "tidyverse", "gsubfn",
              "ggpubr", "comprehenr", "Rtsne", "mclust", "seriation", "jsonlite",
              "viridis", "ggmap", "shinyjqui", "MASS", "shinyWidgets", "truncnorm",
              "GGally", "purrr", "sp", "colorspace", "rjson", "arrow", "lwgeom",
              "mvtnorm", "dplyr")
repo <- "https://cran.rstudio.com/"

# Loop through libraries until one is writable
error <- TRUE
i <- 1
while (error == TRUE && i <= length(libs)) {
  lib <- libs[i]
  tryCatch({
    # update.packages(lib.loc = lib, repos = repo)
    if (!require("prefeR")) {
      install.packages("prefeR", lib = lib, repos = repo)
    }
    loadNamespace("prefeR")
    # Load the packages already installed
    packages_status <- sapply(packages, require, character.only = TRUE, quietly = TRUE)
    packages_to_install <- packages[packages_status == FALSE]
    # Remove packages that failed to load if they are already there
    tryCatch(remove.packages(packages_to_install, lib = lib), error = function(e) {})
    # Install packages
    install.packages(packages_to_install, lib = lib, repos = repo)
    # Load packages
    sapply(packages, library, character.only = TRUE, quietly = TRUE)
    # Stop the loop
    error <- FALSE
  },
  error = function(e) {},
  finally = {
    i <- i + 1
  }
  )
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

ElicitorAppFolder <- normalizePath(file.path(USER_PATH, "Downloads"))
# ElicitorAppFolder <- normalizePath(file.path(FolderSource, "ElicitorOutput"))
JulesAppFolder <- normalizePath(file.path(FolderSource, "JulesOP"))


# Load files if any are missing
if (!file.exists(normalizePath(file.path(ElicitorAppFolder, "Parcels.geojson"))) ||
    !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableMerged.geojson"))) ||
    !file.exists(normalizePath(file.path(ElicitorAppFolder, "FullTableNotAvail.geojson")))) {
  # 337th month
  # mean1 month corresponds to (maybe) January 2022
  JulesMean <- arrow::read_feather(normalizePath(file.path(JulesAppFolder, "JulesApp-rcp26-06-mean-monthly.feather")))[, c("x", "y", "mean337")]
  JulesSD <- arrow::read_feather(normalizePath(file.path(JulesAppFolder, "JulesApp-rcp26-06-sd-monthly.feather")))[, c("x", "y", "sd337")]
  SquaresLoad <- sf::st_read(normalizePath(file.path(JulesAppFolder, "SEER", "Fishnet_1km_to_SEER_net2km.shp")))
  Sqconv <- st_transform(SquaresLoad, crs = 4326)
  CorrespondenceJules <- read.csv(normalizePath(file.path(JulesAppFolder, "CorrespondanceSqToJules.csv")))[, -1]
  seer2km <- st_read(normalizePath(file.path(JulesAppFolder, "SEER_net2km.shp")))
  jncc100 <- read.csv(normalizePath(file.path(JulesAppFolder, "beta_JNCC100_interact_quad.csv")))
  speciesprob40 <-  read.csv(normalizePath(file.path(JulesAppFolder, "scenario_species_prob_40.csv")), header = FALSE)
  climatecells <- read.csv(normalizePath(file.path(JulesAppFolder, "climate_cells.csv")))
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
  # Find lines where Jules is not available, it means there are no trees, so replace by 0
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
  
  # Bootstrap means and standard deviations (to avoid assumptions of independence)
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

message(paste("Waiting for", normalizePath(file.path(ElicitorAppFolder, "outcomes.json"))))
while (!file.exists(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")))) {
  Sys.sleep(5)
}
message(paste(normalizePath(file.path(ElicitorAppFolder, "outcomes.json")), "found. Trying to load file..."))

alphaLVL <- 0.9
MaxRounds <- 5
ConvertSample <- sample(1:5000, 200)

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

# SPECIES <- c(NAME_CONVERSION[1:2, "Specie"], "Pollinators", "All")
# SPECIES_ENGLISH <- c(NAME_CONVERSION[1:2, "English_specie"], "Pollinators", "All")
N_SPECIES <- length(SPECIES)
TARGETS <- c("Carbon", SPECIES, "Area", "NbVisits")
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


JulesMean <- 0;JulesSD <- 0;SquaresLoad <- 0;Sqconv <- 0;CorrespondenceJules <- 0;seer2km <- 0;jncc100 <- 0;speciesprob40 <- 0;climatecells <- 0;
gc()

ui <- fluidPage(useShinyjs(), tabsetPanel(id = "tabs",
                                          tabPanel("Maps", fluidPage(fluidRow(
                                            column(9,
                                                   selectInput("inSelect", "area", sort(unique(c(FullTable$extent, FullTableNotAvail$extent))), FullTable$extent[1]),
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
                                          tabPanel("Clustering", id = "Clustering",
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

server <- function(input, output, session, SPECIES_ARG1 = SPECIES, SPECIES_ENGLISH_ARG1 = SPECIES_ENGLISH, N_TARGETS_ARG1 = N_TARGETS,
                   NAME_CONVERSION_ARG1 = NAME_CONVERSION) {
  
  # hideTab(inputId = "tabs", target = "Exploration")
  # hideTab(inputId = "tabs", target = "Clustering")
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
  SelectedVector<- reactiveVal(NULL)
  PreviousSelectedVector<-reactiveVal(NULL)
  SelectedFullTableRow<-reactiveVal(NULL)
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
  
  CreatedBaseMap<-reactiveVal(0)
  UpdatedExtent<-reactiveVal(0)
  
  for (x in SPECIES) {
    var_name <- paste0(x, "Selected0")
    assign(var_name, reactiveVal(NULL))
  }
  VisitsSelected0 <- reactiveVal(NULL)
  CarbonSelectedSD0 <- reactiveVal(NULL)
  
  
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
  observeEvent(input$inSelect, {
    UpdatedExtent(0)
    SelectedDropdown <- input$inSelect
    PreviousClickedVector(NULL)
    ClickedVector(NULL)
    PreviousSelectedVector(NULL)
    SelectedVector(NULL)
    ClickedMatrixTab2Reactive(NULL)
    PreviousClickedMatrixTab2Reactive(NULL)
    
    
    SelectedFullTableRow(NULL)
    
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
      
      CarbonSelectedSD <- (FullTable$JulesSD[FullTable$extent == SelectedDropdown])
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
      
      
      PreviousSelectedVector(rep(0, dim(SelectedSquares)[1]))
      SelectedVector(rep(1, dim(SelectedSquares)[1]))
      ClickedVector(rep(0, dim(SelectedSquares)[1]))
      PreviousClickedVector(rep(-1, dim(SelectedSquares)[1]))
      
      ClickedMatrixTab2Reactive(matrix(0, 4,dim(SelectedSquares)[1]))
      PreviousClickedMatrixTab2Reactive(matrix(-1, 4,dim(SelectedSquares)[1]))
      
      # PreviousSelectedMatrixTab2(matrix(0, 4,dim(SelectedSquares)[1]))
      #  SelectedMatrixTab2(matrix(1, 4,dim(SelectedSquares)[1]))
      
      
      
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
      # Here, the max of the slider is based on the Max possible level for each 
      MaxVals<-InitFindMaxSliderValues(SelectedVector(),
                                       AreaSelected,
                                       CarbonSelected,
                                       SpeciesListSelected, 
                                       VisitsSelected,
                                       CarbonSelectedSD,
                                       SpeciesListSelectedSD, 
                                       VisitsSelectedSD,
                                       input_areaSlider_multiplicative_coefficient = TRUE,
                                       alpha=alphaLVL)
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
        if (is.nan(max_bioslider)) {
          max_bioslider <- 0
        }
        updateSliderInput(session, bioslider, max = max_bioslider, value = max_bioslider, step = 0.5)
      }
      # max_areaslider <- trunc(100*sum(AreaSelected))/100
      max_areaslider <- MaxVals$AreaMax
      if (is.nan(max_areaslider)) {
        max_areaslider <- 0
      }
      # max_visitsslider <- trunc(mean(VisitsSelected))
      max_visitsslider <- MaxVals$VisistMax
      if (is.nan(max_visitsslider)) {
        max_visitsslider <- 0
      }
      updateSliderInput(session, "AreaSlider", min=MaxVals$AreaMin,max = max_areaslider, value = MaxVals$AreaMax, step = 0.5)
      updateSliderInput(session, "VisitsSlider", max = max_visitsslider, value = max_visitsslider)
      
      # We now need to obtain the list of strategies from simul636 that meet the tragets with the right confidence.
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
      
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      SelecTargetCarbon <- MaxVals$CarbonMax
      
      condition<-TRUE
      for (ijj in 1:length(SPECIES)) {
        x<-SPECIES[ijj]
        max_bioslider <- MaxVals$bioMaxList[[ijj]]
        if (is.nan(max_bioslider)) {
          max_bioslider <- 0
        }
        
        condition <- condition & (SelectedSimMat2[[x]] >=max_bioslider)
      }
      
      SelecTargetArea <- max_areaslider
      SelecTargetVisits <- max_visitsslider
      #PROBAMAT <- Icalc$IVEC
      #for (abc in 1:dim(Icalc$IVEC)[2]) {
      #  PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      #}
      PROBAMAT<-CalcProbaMat(Icalc$IVEC,LimitsMat,Above=AboveTargets)
      
      condition <- TRUE
      for (iii in 1:length(SPECIES)) {
        x<-SPECIES[iii]
        var_name <- paste0("SelecTargetBio", x)
        value <- tmp[[var_name]]
        assign(var_name, value)
        
        condition <- condition & (PROBAMAT[,iii+1] >= alphaLVL)
      }
      rm(tmp)
      
      SubsetMeetTargets <- SelectedSimMat2[(PROBAMAT[,1] >= alphaLVL) &
                                             # (SelectedSimMat2$redsquirrel >= SelecTargetBio) &
                                             condition &
                                             (PROBAMAT[,dim(PROBAMAT)[2]-1] >= alphaLVL) &
                                             (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL), ]
      
      
      SubsetMeetTargetsReactive(SubsetMeetTargets)
      SubsetMeetTargetsReactiveUnique(unique(SubsetMeetTargets))
      PreviousSubsetMeetTargetsReactive(SubsetMeetTargetsReactive()-1)
      PreviousSubsetMeetTargetsReactiveUnique(SubsetMeetTargetsReactiveUnique()-1)
      
      if(dim(unique(SubsetMeetTargets))[1]>0){
        LengthVec<-min(4,dim(unique(SubsetMeetTargets)[1]))
        FourUniqueRowsReactive(seq(1,LengthVec))
        PreviousFourUniqueRowsReactive(seq(1,LengthVec))
      }else{FourUniqueRowsReactive(NULL)
        PreviousFourUniqueRowsReactive(NULL)}
      
      
      
      
    }
    
    
    CreatedBaseMap(0)
    UpdatedExtent(1)
    SlidersHaveBeenInitialized(rep(0,length(SliderNames)))
    
  })
  
  observe({
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SlidersHaveBeenInitialized())==1)) {
      SavedVec<-ClickedVector()
      PreviousSavedVec<-PreviousClickedVector()
      SelectedVec<-SelectedVector()
      PreviousSelectedVec<-PreviousSelectedVector()
      
      
      ColObtained <- getCols(ColourScheme = ColourScheme(), UnitsVec = FullTable$units,
                             ColorLighteningFactor(), ColorDarkeningFactor())
      
      FullColVec <- ColObtained$FullColVec
      ClickedCols <- ColObtained$ClickedCols
      
      
      Consolidated<-2*SavedVec+1*((SelectedVec==1)&(SavedVec==0))
      
      PreviousConsolidated<-2*PreviousSavedVec+1*((PreviousSelectedVec==1)&(PreviousSavedVec==0))
      if(length(PreviousConsolidated)==0){PreviousConsolidated<-Consolidated+1}
      if((CreatedBaseMap()==1)&(length(SavedVec)>0)){
        
        # mapp<-leafletProxy("map")
        #  for(ijj in 1:length(SelectedVec)){
        if(prod(PreviousConsolidated==Consolidated)==0)
        {
          mapp<-leafletProxy("map")
          removeShape(mapp,layerId=paste0("Square",1:length(SelectedVec)))
          #  if(Consolitated[ijj]==0){
          #    
          #      mapp<-addPolygons(mapp,data=FullTable$geometry[ijj],layerId=paste0("Square",ijj),color="transparent",fillColor="transparent")
          #  }
          #  if(Consolitated[ijj]==1){
          #      mapp<-addPolygons(mapp,data=FullTable$geometry[ijj],layerId=paste0("Square",ijj),color=FullColVec[ijj],weight=1)#color=FullColVec[ijj],fillColor
          #  }
          #  if(Consolitated[ijj]==2){
          #      mapp<-addPolygons(mapp,data=FullTable$geometry[ijj],layerId=paste0("Square",ijj),color=ClickedCols[ijj],weight=1)#,color=ClickedCols[ijj]
          #  }
          removeShape(mapp,layerId=paste0("Square",1:length(Consolidated)))
          #if(Consolitated[ijj]==0){
          COLOURS<-rep("transparent",length(Consolidated))
          COLOURS[Consolidated==1]<-FullColVec[Consolidated==1]
          COLOURS[Consolidated==2]<-ClickedCols[Consolidated==2]
          mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),color=COLOURS,fillColor=COLOURS,weight=1)
          
          removeControl(mapp,layerId="legend")
          
          SFTR<-SelectedFullTableRow()
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- SPECIES_ENGLISH[i]
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
        
      }
      
      
      PreviousClickedVector(SavedVec)  
      PreviousSelectedVector(SelectedVec)
      # replace the text
      
      
      
    }
  })
  
  observe({
    if ((CreatedBaseMap()==1) && (UpdatedExtent()==1) && (prod(SlidersHaveBeenInitialized())==1) && (input$tabs=="Exploration")) {
      SubsetMeetTargets<-SubsetMeetTargetsReactive()
      PreviousSubsetMeetTargets<-PreviousSubsetMeetTargetsReactive()
      SubsetMeetTargetsUnique<-SubsetMeetTargetsReactiveUnique()
      PreviousSubsetMeetTargetsUnique<-PreviousSubsetMeetTargetsReactiveUnique()
      
      SavedMat<-ClickedMatrixTab2Reactive()
      PreviousSavedMat<-PreviousClickedMatrixTab2Reactive()
      FourUniqueRowsLoc<-FourUniqueRowsReactive()
      PreviousFourUniqueRowsLoc<-PreviousFourUniqueRowsReactive()
       #if(is.null(dim(PreviousFourUniqueRowsLoc))){
      #  PreviousFourUniqueRowsLoc<-matrix(PreviousFourUniqueRowsLoc,1,length(PreviousFourUniqueRowsLoc))}
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
          
          # mapp<-leafletProxy(paste0("map",ii+1))
          # for(ijj in 1:length(Consolitated)){
          # if(PreviousConsolitated[ijj]!=Consolitated[ijj])
          #{
          mapp<-leafletProxy(paste0("map",ii+1))
          removeShape(mapp,layerId=paste0("Square",1:length(Consolidated)))
          #if(Consolitated[ijj]==0){
          COLOURS<-rep("transparent",length(Consolidated))
          COLOURS[Consolidated==1]<-FullColVec[Consolidated==1]
          COLOURS[Consolidated==2]<-ClickedCols[Consolidated==2]
          mapp<-addPolygons(mapp,data=FullTable$geometry,layerId=paste0("Square",1:length(Consolidated)),color=COLOURS,fillColor=COLOURS,weight=1)
          #}
          #              if(Consolitated[ijj]==1){
          #               mapp<-addPolygons(mapp,data=FullTable$geometry[ijj],layerId=paste0("Square",ijj),color=FullColVec[ijj],weight=1)#,color=FullColVec[ijj]
          #            }
          #           if(Consolitated[ijj]==2){
          #            mapp<-addPolygons(mapp,data=FullTable$geometry[ijj],layerId=paste0("Square",ijj),color=ClickedCols[ijj],weight=1)#,color=ClickedCols[ijj]
          #         }
          
          
          #  }
          
        }
        removeControl(mapp,layerId="legend")
        
        SFTR<-SelectedRows[ii,]
        addControlText <- ""
        for (i in 1:length(SPECIES)) {
          specie_latin <- SPECIES[i]
          specie_english <- SPECIES_ENGLISH[i]
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
      #form0<-paste0('Text', ii,'("")')
      #eval(parse(text=form0))
      #message(form0)
      #form<-paste0('Text', ii ,
      #             '(paste0("Strategy Displayed: ",FourUniqueRowsLoc[ii]," out of ",dim(SubsetMeetTargetsUnique)
      #             
      #             ))')
      #message(form)
      
      
      #eval(parse(text=form))
      
      #}
      if(length(FourUniqueRowsLoc)<4){
        #add here text to say that there are no more unique examples.
        for(ii in seq(length(FourUniqueRowsLoc)+1,4))
        {  #form2<-paste0('Text', ii ,
          #            '(paste0("Strategies that meet all ", N_TARGETS))')
          
          #eval(parse(text=form2))
          
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
        
        #Text1("")
        
        
        #  PreviousSubsetMeetTargetsReactive4Unique(SubsetMeetTargetsReactive4Unique())  
        #PreviousSelectedVector(SelectedVec)
        # replace the text
      }
      
    }else{}
      
    }
  })
  
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
  
  
  # Check if the slider values have been updated after the initialization
  #lapply(SliderNames, function(sl) {
  observeEvent(input$SliderMain,{
    SHBICurrent<-SlidersHaveBeenInitialized()
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SHBICurrent)==0)) {
      for (sl in SliderNames){
        SliderNumber<-which(SliderNames==sl)        
        if(input[[sl]]==MaxMinValsReactiveVector()[SliderNumber]){SHBICurrent[SliderNumber]<-1;SlidersHaveBeenInitialized(SHBICurrent)}
      }}
    #})
  }
  )
  
  
  
  
  
  # Check for changes in all the sliders
  #lapply(SliderNames, function(sl) {observeEvent(input[[sl]],{
  # if (input[[sl]]) {
  observeEvent({input$map_shape_click
    lapply(SliderNames, function(sl) {input[[sl]]})
  },{
    if((CreatedBaseMap()==1)&(UpdatedExtent()==1)&(prod(SlidersHaveBeenInitialized())==1)) {
      
      
      SavedVec <- ClickedVector()
      SelectedVec<- SelectedVector()
      SelectedDropdown <- input$inSelect
      
      if (!is.null(SavedVec)) {
        
        AreaSelected <- AreaSelected0()
        CarbonSelected <- CarbonSelected0()
        SpeciesListSelected <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "Selected0"))
          SpeciesListSelected[x] <- list(value())
        }
        VisitsSelected <- VisitsSelected0()
        
        CarbonSelectedSD <- CarbonSelectedSD0()
        SpeciesListSelectedSD <- list()
        for (x in SPECIES) {
          value <- get(paste0(x, "SelectedSD0"))
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
                                       alphaLVL=alphaLVL,
                                       tolvec=tolvecReactive())
        
        SelectedSimMat2 <- tmp$SelectedSimMat2
        Icalc <- tmp$Icalc
        LimitsMat <- tmp$LimitsMat
        SelecTargetCarbon <- tmp$SelecTargetCarbon
        # SelecTargetBio <- tmp$SelecTargetBio
        # condition <- TRUE
        #for (x in SPECIES) {
        #  var_name <- paste0("SelecTargetBio", x)
        #  value <- tmp[[var_name]]
        #  assign(var_name, value)
        
        # condition <- condition & (SelectedSimMat2[[x]] >= value)
        #}
        SelecTargetArea <- tmp$SelecTargetArea
        SelecTargetVisits <- tmp$SelecTargetVisits
        #PROBAMAT <- Icalc$IVEC
        #for (abc in 1:dim(Icalc$IVEC)[2]) {
        #  PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
        #}
        PROBAMAT<-CalcProbaMat(Icalc$IVEC,LimitsMat,Above=AboveTargets)

        
        
        condition <- TRUE
        for (iii in 1:length(SPECIES)) {
          x<-SPECIES[iii]
          var_name <- paste0("SelecTargetBio", x)
          value <- tmp[[var_name]]
          assign(var_name, value)
          
          condition <- condition & (PROBAMAT[,iii+1] >= alphaLVL)
        }
        rm(tmp)
        ############# Change the min of Area bar 
        #SubsetMeetTargetsWithoutArea <- SelectedSimMat2[(PROBAMAT[,1] >= alphaLVL) &
                                               # (SelectedSimMat2$redsquirrel >= SelecTargetBio) &
        #                                       condition &
        #                                       (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL), ]
        #LimMatMeetTargetsWithoutArea<-LimitsMat[(PROBAMAT[,1] >= alphaLVL) &
          # (SelectedSimMat2$redsquirrel >= SelecTargetBio) &
         # condition &
        #  (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL),]
        
        #MinAreaLocIndex<-which.min(SubsetMeetTargetsWithoutArea$Area)
        #MinAreaLoc<-SubsetMeetTargetsWithoutArea$Area[MinAreaLocIndex]
        #LimAreaLoc<-LimMatMeetTargetsWithoutArea$SelectedSimMat2.Area[MinAreaLocIndex]
        #CalcNewMin<-max(0,trunc(1+MinAreaLoc+sqrt( tolvecReactive()[dim(LimitsMat)[2]-1])*qtruncnorm(p=alphaLVL,a=LimAreaLoc,b=Inf,mean=0,sd=1)))
        #if(CalcNewMin)
        
        #################
                
        SubsetMeetTargets <- SelectedSimMat2[(PROBAMAT[,1] >= alphaLVL) &
                                               # (SelectedSimMat2$redsquirrel >= SelecTargetBio) &
                                               condition &
                                               (PROBAMAT[,dim(PROBAMAT)[2]-1] >= alphaLVL) &
                                               (PROBAMAT[,dim(PROBAMAT)[2]] >= alphaLVL), ]
        
        SubsetMeetTargetsReactive(SubsetMeetTargets)
        SubsetMeetTargetsReactiveUnique(unique(SubsetMeetTargets))
        
        if(dim(unique(SubsetMeetTargets))[1]>0){
          LengthVec<-min(4,dim(unique(SubsetMeetTargets)[1]))
          FourUniqueRowsReactive(seq(1,LengthVec))
          PreviousFourUniqueRowsReactive(seq(1,LengthVec))
        }else{FourUniqueRowsReactive(NULL)
          PreviousFourUniqueRowsReactive(NULL)}
        
        #SubsetMeetTargets <- SelectedSimMat2[(SelectedSimMat2$Carbon >= SelecTargetCarbon) &
        #                                      # (SelectedSimMat2$redsquirrel >= SelecTargetBio) &
        #                                     condition &
        #                                    (SelectedSimMat2$Area >= SelecTargetArea) &
        #                                   (SelectedSimMat2$Visits >= SelecTargetVisits), ]
        
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
          
          SelectedFullTableRow(SelectedMins[SelecRow,])
          SelectedVector(SelectedMins[SelecRow, 1:length(SavedVec)])
        } else {
          ZeroSelected<-SelectedSimMat2[1,]
          ZeroSelected<-replace(ZeroSelected,1:length(ZeroSelected),0)
          SelectedFullTableRow(ZeroSelected)
          SelectedVector(ZeroSelected[ 1:length(SavedVec)])
        }
      }      
      
      
      
      
    }
    #  }
  })
  # }
  #  )
  
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
            
            
            SELGEOSavedVec <- SELGEOSavedVec[SavedVec == 1, ]
            SELGEOSwitched <- SELGEOSwitched[(SwitchedOnCells == 1) & (SavedVec != 1), ]
            
            if (dim(SELGEORemaining)[1] > 0) {
              listMaps[[aai]] <- addPolygons(listMaps[[aai]], data = SELGEORemaining, color = SELGEORemaining$color, layerId = ~SELGEORemaining$layerId, weight = UnitPolygonColours)
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
          ClickedMatrixTab2Reactive(t(matrix(SavedVec,length(SavedVec),4)))
          ChangeDone <- TRUE
        }
        iii <- iii + 1
      }
    }
  })
  
  output$map <- renderLeaflet({
    #  shinyjs::hide("tabs")
    
    if((CreatedBaseMap()==0)&(UpdatedExtent()==1)){
      SavedVec <- ClickedVector()
      SelectedVec <- SelectedVector()
      SelectedDropdown <- input$inSelect
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
          SELGEOSwitched <- SELGEOFull[, c("geometry", "layerId")]
          
          SELGEOSavedVec <- SELGEOSavedVec[SavedVec == 1, ]#;gpNamesSavedVec <- gpNamesSavedVec[SavedVec]
          SELGEOSwitched <- SELGEOSwitched[(SwitchedOnCells == 1) & (SavedVec != 1), ]#;gpNamesSwitched <- gpNamesSwitched[SwitchedOnCells & (!SavedVec)]
          SELGEORemaining <- SELGEOFull[(SavedVec == 1) | (SwitchedOnCells == 1), c("geometry", "layerId", "color")]
          
          
          
          if (dim(SELGEORemaining)[1] > 0) {
            map <- addPolygons(map, data = SELGEORemaining, color = SELGEORemaining$color, layerId = ~SELGEORemaining$layerId, weight = UnitPolygonColours)
          }
          
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- SPECIES_ENGLISH[i]
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
        # if(CreatedBaseMap()==1){
        #  mapp<-leafletProxy("map")
        #mapp}
      }
    # ChangeSliders(FALSE)
    # shinyjs::show("tabs")
    
  })
  
  output$map2 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
  })
  
  output$map3 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
  })
  
  output$map4 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
    
  })
  
  output$map5 <- renderLeaflet({
    
    if ((CreatedBaseMap() == 1) & (UpdatedExtent() == 1)) {
      MapReactive()
    }
  })
}

shinyApp(ui, server)
