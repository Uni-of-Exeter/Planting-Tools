library(shiny)
library(shinyjs)
library(shinyjqui)
library(leaflet)
library(sf)
library(ggplot2)
library(geosphere)
library(feather)
library(readr)
library(dplyr)
library(tidyverse)
library(gsubfn)
library(ggpubr)
library(comprehenr)
library(Rtsne)
library(mclust)
library(seriation)
library(jsonlite)
library(viridis)
library(ggmap)
library(shinyjqui)
library(MASS)
library(shinyWidgets)
library(truncnorm)
loadNamespace("prefeR")
library(GGally)
#  SavedVec<-rep(0,47)
#SelecTargetCarbon<-240;      SelecTargetBio<-19;SelecTargetArea<-13890596;SelecTargetVisits<-17
#SelecTargetCarbon<-1000;      SelecTargetBio<-1100;SelecTargetArea<-1000000000;SelecTargetVisits<-1000000
#SelectedDropdown<-"Ennerdale"

# FolderSource<-"ShinyForestry/"
FolderSource<-""

source(paste0(FolderSource, "functions.R"))
# species_names <- colnames(read.csv(paste0(FolderSource, "Model Data/Biodiversity/JNCC/beta_JNCC100_interact_quad.csv")))[-1]
# english_species_names <- c("Lesser Redpoll", "Skylark", "Tree Pipit", "Bittern", "Nightjar", "Yellowhammer", "Reed Bunting", "Grasshopper Warbler", "Woodlark", "Yellow Wagtail", "Spotted Flycatcher", "Curlew", "Grey Partridge", "Wood Warbler", "Turtle Dove", "Ring Ouzel", "Lapwing", "Adder", "Mountain Bumblebee", "Water Beetle sp.", "Noble Chafer", "Water Beetle sp.", "Stag Beetle", "Small Pearl-Bordered Fritillary", "Small Heath", "Large Heath", "Small Blue", "Mountain Ringlet", "Dingy Skipper", "Grayling", "Wall", "White Admiral", "White-Letter Hairstreak", "Speckled Bush Cricket", "Bog Bush Cricket", "Goat Moth", "Grey Dagger", "Green-brindled Crescent", "Brindled Ochre", "Red Carpet", "Plaited Door Snail", "Kentish Snail", "Hollowed Glass Snail", "Lichen subsp.", "Lichen sp.", "Lichen sp.", "Lichen sp.", "String-Of-Sausage Lichen", "Barbastelle bat", "Wildcat", "European hare", "Mountain Hare", "Pine Marten", "Harvest Mouse", "Hazel Dormouse", "Polecat", "Bechstein's bat", "Noctule Bat", "Brown Long-eared Bat", "Greater Horseshoe Bat", "Lesser Horseshoe Bat", "Eurasian red squirrel", "Field bugloss", "Bog Rosemary", "Mountain bearberry", "Green spleenwort", "Frosted Orache", "Saltmarsh Flat-Sedge", "Sea Rocket", "Clustered Bellflower", "Long-Bracted Sedge", "Tall Bog-Sedge", "Lesser Centaury", "Field Mouse-Ear", "Woolly Thistle", "Spurge-Laurel", "Broad-Leaved Cottongrass", "Common Ramping-Fumitory", "Petty Whin", "Dyer's Greenweed", "Dwarf Cudweed", "Creeping Lady's-Tresses", "Marsh St John's-Wort", "Cut-Leaved Dead-Nettle", "Lyme Grass", "Stag's-Horn Clubmoss", "Neottia nidus-avis Bird's-Nest Orchid", "Bird's-Foot", "Serrated Wintergreen", "Mountain Sorrel", "Intermediate Wintergreen", "Allseed", "Round-Leaved Crowfoot", "Rue-Leaved Saxifrage", "Pepper-Saxifrage", "Large Thyme", "Small-Leaved Lime", "Strawberry Clover", "Knotted Clover", "Small Cranberry")
# english_species_names <- add_suffix_to_duplicates(english_species_names)

name_conversion <- matrix(data = c("Bird", "Acanthis cabaret", "Lesser Redpoll",
                                   "Bird", "Alauda arvensis", "Skylark",
                                   "Bird", "Anthus trivialis", "Tree Pipit",
                                   "Bird", "Botaurus stellaris", "Bittern",
                                   "Bird", "Caprimulgus europaeus", "Nightjar",
                                   "Bird", "Emberiza citrinella", "Yellowhammer",
                                   "Bird", "Emberiza schoeniclus", "Reed Bunting",
                                   "Bird", "Locustella naevia", "Grasshopper Warbler",
                                   "Bird", "Lullula arborea", "Woodlark",
                                   "Bird", "Motacilla flava subsp flavissima", "Yellow Wagtail",
                                   "Bird", "Muscicapa striata", "Spotted Flycatcher",
                                   "Bird", "Numenius arquata", "Curlew",
                                   "Bird", "Perdix perdix", "Grey Partridge",
                                   "Bird", "Phylloscopus sibilatrix", "Wood Warbler",
                                   "Bird", "Streptopelia turtur", "Turtle Dove",
                                   "Bird", "Turdus torquatus", "Ring Ouzel",
                                   "Bird", "Vanellus vanellus", "Lapwing",
                                   "Herptile", "Vipera berus", "Adder",
                                   "Invertebrate - bee", "Bombus monticola", "Mountain Bumblebee",
                                   "Invertebrate - beetle", "Cercyon convexiusculus", "Water Beetle sp.",
                                   "Invertebrate - beetle", "Gnorimus nobilis", "Noble Chafer",
                                   "Invertebrate - beetle", "Liopterus haemorrhoidalis", "Water Beetle sp.",
                                   "Invertebrate - beetle", "Lucanus cervus", "Stag Beetle",
                                   "Invertebrate - butterfly", "Boloria selene", "Small Pearl-Bordered Fritillary",
                                   "Invertebrate - butterfly", "Coenonympha pamphilus", "Small Heath",
                                   "Invertebrate - butterfly", "Coenonympha tullia", "Large Heath",
                                   "Invertebrate - butterfly", "Cupido minimus", "Small Blue",
                                   "Invertebrate - butterfly", "Erebia epiphron", "Mountain Ringlet",
                                   "Invertebrate - butterfly", "Erynnis tages", "Dingy Skipper",
                                   "Invertebrate - butterfly", "Hipparchia semele", "Grayling",
                                   "Invertebrate - butterfly", "Lasiommata megera", "Wall",
                                   "Invertebrate - butterfly", "Limenitis camilla", "White Admiral",
                                   "Invertebrate - butterfly", "Satyrium w-album", "White-Letter Hairstreak",
                                   "Invertebrate - cricket", "Leptophyes punctatissima", "Speckled Bush Cricket",
                                   "Invertebrate - cricket", "Metrioptera brachyptera", "Bog Bush Cricket",
                                   "Invertebrate - moth", "Cossus cossus", "Goat Moth",
                                   "Invertebrate - moth", "Acronicta psi", "Grey Dagger",
                                   "Invertebrate - moth", "Allophyes oxyacanthae", "Green-brindled Crescent",
                                   "Invertebrate - moth", "Dasypolia templi", "Brindled Ochre",
                                   "Invertebrate - moth", "Xanthorhoe decoloraria", "Red Carpet",
                                   "Invertebrate - snail", "Cochlodina laminata", "Plaited Door Snail",
                                   "Invertebrate - snail", "Monacha cantiana", "Kentish Snail",
                                   "Invertebrate - snail", "Zonitoides excavatus", "Hollowed Glass Snail",
                                   "Lichen", "Anaptychia ciliaris subsp ciliaris", "Lichen subsp.",
                                   "Lichen", "Leptogium brebissonii", "Lichen sp.",
                                   "Lichen", "Parmeliella testacea", "Lichen sp.",
                                   "Lichen", "Pseudocyphellaria intricata", "Lichen sp.",
                                   "Lichen", "Usnea articulata", "String-Of-Sausage Lichen",
                                   "Mammal", "Barbastella barbastellus", "Barbastelle bat",
                                   "Mammal", "Felis silvestris", "Wildcat",
                                   "Mammal", "Lepus europaeus", "European hare",
                                   "Mammal", "Lepus timidus", "Mountain Hare",
                                   "Mammal", "Martes martes", "Pine Marten",
                                   "Mammal", "Micromys minutus", "Harvest Mouse",
                                   "Mammal", "Muscardinus avellanarius", "Hazel Dormouse",
                                   "Mammal", "Mustela putorius", "Polecat",
                                   "Mammal", "Myotis bechsteinii", "Bechstein's bat",
                                   "Mammal", "Nyctalus noctula", "Noctule Bat",
                                   "Mammal", "Plecotus auritus", "Brown Long-eared Bat",
                                   "Mammal", "Rhinolophus ferrumequinum", "Greater Horseshoe Bat",
                                   "Mammal", "Rhinolophus hipposideros", "Lesser Horseshoe Bat",
                                   "Mammal", "Sciurus vulgaris", "Eurasian red squirrel",
                                   "Vascular plant", "Anchusa arvensis", "Field bugloss",
                                   "Vascular plant", "Andromeda polifolia", "Bog Rosemary",
                                   "Vascular plant", "Arctostaphylos alpinus", "Mountain bearberry",
                                   "Vascular plant", "Asplenium viride", "Green spleenwort",
                                   "Vascular plant", "Atriplex laciniata", "Frosted Orache",
                                   "Vascular plant", "Blysmus rufus", "Saltmarsh Flat-Sedge",
                                   "Vascular plant", "Cakile maritima", "Sea Rocket",
                                   "Vascular plant", "Campanula glomerata", "Clustered Bellflower",
                                   "Vascular plant", "Carex extensa", "Long-Bracted Sedge",
                                   "Vascular plant", "Carex magellanica", "Tall Bog-Sedge",
                                   "Vascular plant", "Centaurium pulchellum", "Lesser Centaury",
                                   "Vascular plant", "Cerastium arvense", "Field Mouse-Ear",
                                   "Vascular plant", "Cirsium eriophorum", "Woolly Thistle",
                                   "Vascular plant", "Daphne laureola", "Spurge-Laurel",
                                   "Vascular plant", "Eriophorum latifolium", "Broad-Leaved Cottongrass",
                                   "Vascular plant", "Fumaria muralis", "Common Ramping-Fumitory",
                                   "Vascular plant", "Genista anglica", "Petty Whin",
                                   "Vascular plant", "Genista tinctoria", "Dyer's Greenweed",
                                   "Vascular plant", "Gnaphalium supinum", "Dwarf Cudweed",
                                   "Vascular plant", "Goodyera repens", "Creeping Lady's-Tresses",
                                   "Vascular plant", "Hypericum elodes", "Marsh St John's-Wort",
                                   "Vascular plant", "Lamium hybridum", "Cut-Leaved Dead-Nettle",
                                   "Vascular plant", "Leymus arenarius", "Lyme Grass",
                                   "Vascular plant", "Lycopodium clavatum", "Stag's-Horn Clubmoss",
                                   "Vascular plant", "Neottia nidus-avis", "Bird's-Nest Orchid",
                                   "Vascular plant", "Ornithopus perpusillus", "Bird's-Foot",
                                   "Vascular plant", "Orthilia secunda", "Serrated Wintergreen",
                                   "Vascular plant", "Oxyria digyna", "Mountain Sorrel",
                                   "Vascular plant", "Pyrola media", "Intermediate Wintergreen",
                                   "Vascular plant", "Radiola linoides", "Allseed",
                                   "Vascular plant", "Ranunculus omiophyllus", "Round-Leaved Crowfoot",
                                   "Vascular plant", "Saxifraga tridactylites", "Rue-Leaved Saxifrage",
                                   "Vascular plant", "Silaum silaus", "Pepper-Saxifrage",
                                   "Vascular plant", "Thymus pulegioides", "Large Thyme",
                                   "Vascular plant", "Tilia cordata", "Small-Leaved Lime",
                                   "Vascular plant", "Trifolium fragiferum", "Strawberry Clover",
                                   "Vascular plant", "Trifolium striatum", "Knotted Clover",
                                   "Vascular plant", "Vaccinium microcarpum", "Small Cranberry"),
                          ncol = 3, byrow = TRUE)
name_conversion <- data.frame(Specie = name_conversion[, 2],
                              English_specie = add_suffix_to_duplicates(name_conversion[, 3]),
                              Group = name_conversion[, 1]) %>%
  # Replace Invertebrate - bee/beetle/butterfly/cricket/moth by Pollinator
  dplyr::mutate(Group = case_when(grepl("bee|beetle|butterfly|cricket|moth", Group) ~ "Pollinator",
  .default = Group)) %>%
  # Acanthis cabaret -> Acanthis_cabaret, and Neottia nidus-avis -> Neottia_nidus_avis
  dplyr::mutate(Specie = gsub(" |-", "_", Specie)) %>%
  # Sort by Specie
  dplyr::arrange(Specie)
# Swap rows 83 and 84
row83 <- name_conversion[83, ]
row84 <- name_conversion[84, ]
name_conversion[83, ] <- row84
name_conversion[84, ] <- row83

# Sys.setenv(PROJ_LIB="/usr/share/proj")
# Sys.setenv(GDAL_DATA = "/usr/share/proj/")

# load all shapes
# PROJdir<-system.file("proj/proj.db", package = "sf")
# PROJdir<-substring(PROJdir,1,nchar(PROJdir)-8)
# sf_proj_search_paths(PROJdir)
shfile<-sf::st_read(paste0(FolderSource,"_exp0308154002.gdb.zip"),layer = "INV_BLKDATA")
shconv <- st_transform(shfile, crs = 4326)
FullTable<-data.frame(read.csv(paste0(FolderSource,"FullTableResult.csv"))[,-1])
FullTable <- add_richness_columns(FullTable = FullTable, name_conversion = name_conversion)
FullTableNotAvail<-data.frame(read.csv(paste0(FolderSource,"FullTableNotAvail.csv"))[,-1])
STDMEAN<-0.05
STDSTD<-0.01
simul636<-read.csv(file=paste0(FolderSource,"Simul636.csv"))[,-1]

alphaLVL<-0.9

MaxRounds<-5

ConvertSample<-sample(1:5000,200)



# SPECIES <- name_conversion[1:2, "Specie"]
SPECIES <- c("Pollinator", "All")
N_SPECIES <- length(SPECIES)
TARGETS <- c("Carbon", SPECIES, "Area", "NbVisits")
N_TARGETS <- length(TARGETS)

# slider_list <- list(
#   sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25)
# )
# Add sliderInput("BioSliderSPECIE","Average SPECIE % increase:",min=0,max=36,value=25) for each specie

verticalLayout_params <- c(list(bquote(sliderInput("SliderMain","Tree Carbon Stored (2050):",min=0,max=870,value=800))),
                           list(bquote(textOutput("SoilCarbonNotIncluded"))),
                           lapply(SPECIES, function(x, fulltable) {
                             # max_specie <- round(max(fulltable[, paste0("BioMean_", x)]))
                             # value <- round(max_specie / 2)
                             max_specie <- 36
                             value <- 1
                             return(bquote(sliderInput(paste0("BioSlider", .(x)), 
                                                       if (.(x) %in% name_conversion$Group || .(x) == "All") paste("Richness for", .(x)) else paste("Average species", .(x), "% chance of appearance:"), 
                                                       min = 0,
                                                       max = .(max_specie),
                                                       value = .(value))))
                           }, fulltable = FullTable),
                           list(bquote(sliderInput("AreaSlider","Total Area Planted (km^2):",min=0,max=25,value=15))),
                           list(bquote(sliderInput("VisitsSlider","Average Number of Visitors per cell:",min=0,max=750,value=400))))

ui <- fluidPage(useShinyjs(),tabsetPanel(id = "tabs",
                                         tabPanel("Maps",fluidPage(fluidRow(
                                           column(9,
                                                  #  tags$head(tags$style(HTML("#map {pointer-events: none;}"))),
                                                  selectInput("inSelect", "area",sort(unique(c(FullTable$extent,FullTableNotAvail$extent))),"Ennerdale"),
                                                  jqui_resizable(leafletOutput("map",height = 800,width="100%"))
                                           ),
                                           column(3,
                                                  # verticalLayout(sliderInput("SliderMain","Tree Carbon Stored (2050):",min=0,max=870,value=800),
                                                  #                textOutput("SoilCarbonNotIncluded"),
                                                  #                sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25),
                                                  #                sliderInput("AreaSlider","Total Area Planted (km^2):",min=0,max=25,value=15),
                                                  #                sliderInput("VisitsSlider","Average Number of Visitors per cell:",min=0,max=750,value=400))
                                                  do.call("verticalLayout",
                                                          verticalLayout_params)
                                                  ))
                                         )
                                         ),
                                         tabPanel("Exploration",
                                                  fluidPage(fluidRow(
                                                    column(5,
                                                           verticalLayout(verbatimTextOutput("FirstMapTxt"),jqui_resizable(leafletOutput("map2",height = 400,width="100%")))
                                                    ),
                                                    column(5,
                                                           verticalLayout(verbatimTextOutput("SecondMapTxt"),jqui_resizable(leafletOutput("map3",height = 400,width="100%")))
                                                    ),
                                                    column(2, verticalLayout(verbatimTextOutput("TargetText"),
                                                                             #  selectInput("chooseGrouping", "Grouping Type:",c("Carbon level"),"Carbon level"),
                                                                             actionButton("random", "Randomize!"))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    column(5,
                                                           verticalLayout(verbatimTextOutput("ThirdMapTxt"),jqui_resizable(leafletOutput("map4",height = 400,width="100%")))
                                                    ),
                                                    column(5,
                                                           verticalLayout(verbatimTextOutput("FourthMapTxt"),jqui_resizable(leafletOutput("map5",height = 400,width="100%")))
                                                    ),
                                                    column(2,"")
                                                  )
                                                  )
                                         ),
                                         tabPanel("Clustering",
                                                  fluidPage(
                                                    shinyjs::hidden(
                                                      fluidRow(12, checkboxInput("Trigger", "", value = FALSE, width = NULL))),
                                                    conditionalPanel(
                                                      condition="input.Trigger==true",
                                                      fluidRow(
                                                        column(6,verticalLayout(jqui_resizable(leafletOutput("ClusterPage")),actionButton("choose1", "choose"))
                                                        ),
                                                        column(6,verticalLayout(jqui_resizable(leafletOutput("ClusterPage2")),actionButton("choose2", "choose"))
                                                        )
                                                      )),
                                                    conditionalPanel(
                                                      condition="input.Trigger==false", fluidRow(
                                                        column(12,jqui_resizable(plotOutput("plotOP1"))))
                                                    )
                                                  ))
))

server <- function(input, output, session, SPECIES_ARG1 = SPECIES, N_TARGETS_ARG1 = N_TARGETS) {
  SPECIES <- SPECIES_ARG1
  N_SPECIES <- length(SPECIES)
  N_TARGETS <- N_TARGETS_ARG1
  
  CarbonSliderVal<- reactive({input$SliderMain})

  # bioSliderVal<- reactive({input$BioSlider})
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
  AreaSliderVal<- reactive({input$AreaSlider})
  VisitsSliderVal<- reactive({input$VisitsSlider})

  Text1<-reactiveVal("")
  Text2<-reactiveVal("")
  Text3<-reactiveVal("")
  # Add TextN<-reactiveVal("") for each specie
  for (i in 1:N_SPECIES) {
    var_name <- paste0("Text", i + 3)
    value <- reactiveVal("")
    assign(var_name, value)
  }
  
  output$TargetText<-renderText({
    SPECIES <- SPECIES_ARG1
    N_SPECIES <- length(SPECIES)
    N_TARGETS <- N_TARGETS_ARG1
    
    text <- paste0("Targets:\n",
                   "Tree Carbon: ", as.numeric(CarbonSliderVal()))
    # A for loop over the reactive values causes an issue: only the last reactive value
    # takes effect and therefore overwrites other reactive values, i.e. all bioSliderValSPECIE take
    # the same value. I have to work with a list for it to work.
    # for (x in SPECIES) {
    #   BioSliderValSpecie <- get(paste0("BioSliderVal", x))
    #   text <- paste0(text, "\n", x, ": ", as.numeric(BioSliderValSpecie()))
    # }
    for (i in seq_along(SPECIES)) {
      x <- SPECIES[i]
      BioSliderValSpecie <- reactive_list[[i]]
      text <- paste0(text, "\n", x, ": ", as.numeric(BioSliderValSpecie()))
    }
    text <- paste0(text,
                   # "\nRed Squirrel: ",as.numeric(bioSliderVal()),
                   "\nArea Planted: ", as.numeric(AreaSliderVal()),
                   "\nVisits/km^2: ", as.numeric(VisitsSliderVal()))
  })
  output$SoilCarbonNotIncluded<-renderText({paste0("Soil carbon in future versions")})
  
  output$FirstMapTxt<-renderText({Text1()})
  output$SecondMapTxt<-renderText({Text2()})
  output$ThirdMapTxt<-renderText({Text3()})
  output$FourthMapTxt<-renderText({Text4()})
  
  randomValue <- eventReactive({
    input$random
    input$tabsetPanel == "Exploration"
  }, {
    runif(1)
  })
  ClickedVector<-reactiveVal(NULL)
  AreaSelected0<-reactiveVal(NULL)
  CarbonSelected0<-reactiveVal(NULL)
  # RedSquirrelSelected0<-reactiveVal(NULL)
  for (x in SPECIES) {
    var_name <- paste0(x, "Selected0")
    assign(var_name, reactiveVal(NULL))
  }
  VisitsSelected0<-reactiveVal(NULL)
  CarbonSelectedSD0<-reactiveVal(NULL)
  # RedSquirrelSelectedSD0<-reactiveVal(NULL)
  for (x in SPECIES) {
    var_name <- paste0(x, "SelectedSD0")
    assign(var_name, reactiveVal(NULL))
  }
  VisitsSelectedSD0<-reactiveVal(NULL)
  
  DatBinaryCode0<-reactiveVal(NULL)
  NbRoundsMax<-reactiveVal(0)
  CurrentRound<-reactiveVal(0)
  LinesToCompareReactive<-reactiveVal(0)
  
  VecNbMet0<-reactiveVal(NULL)
  
  output$Trigger<-reactiveVal(TRUE)
  #inputOptions(intput, 'Trigger', suspendWhenHidden = FALSE)
  
  
  observeEvent(input$inSelect, {
    SelectedDropdown<-input$inSelect
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
    
    SelectedSquares<-cbind(extent=FullTable[FullTable$extent==SelectedDropdown,c("extent")],FullTable[FullTable$extent==SelectedDropdown,c("lgn.1","lat.1")])
    
    if(!(dim(SelectedSquares)[1]==0)){
      AreaSelected<-FullTable[FullTable$extent==SelectedDropdown,c("area")]
      CarbonSelected<-(FullTable[FullTable$extent==SelectedDropdown,c("JulesMean")]*AreaSelected)/1e6
      # RedSquirrelSelected<-FullTable[FullTable$extent==SelectedDropdown,c("BioMean_Sciurus_vulgaris")]
      for (x in SPECIES) {
        biomean_var <- paste0("BioMean_", x)
        var_name <- paste0(x, "Selected")
        value <- FullTable[FullTable$extent==SelectedDropdown, biomean_var]
        assign(var_name, value)
      }
      VisitsSelected<-FullTable[FullTable$extent==SelectedDropdown,c("VisitsMean")]
      
      
      CarbonSelectedSD<-(FullTable[FullTable$extent==SelectedDropdown,c("JulesSD")]*AreaSelected)/1e6
      # RedSquirrelSelectedSD<-FullTable[FullTable$extent==SelectedDropdown,c("BioSD_Sciurus_vulgaris")]
      for (x in SPECIES) {
        biosd_var <- paste0("BioSD_", x)
        var_name <- paste0(x, "SelectedSD")
        value <- FullTable[FullTable$extent==SelectedDropdown, biosd_var]
        assign(var_name, value)
      }
      VisitsSelectedSD<-FullTable[FullTable$extent==SelectedDropdown,c("VisitsSD")]
      
      
      
      ClickedVector(rep(0,dim(SelectedSquares)[1]))
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
      
      updateSliderInput(session, "SliderMain", max = trunc(sum(CarbonSelected)),value=trunc(sum(CarbonSelected)))
      # updateSliderInput(session, "BioSlider", max = trunc(mean(RedSquirrelSelected)),value=trunc(mean(RedSquirrelSelected)))
      for (x in SPECIES) {
        bioslider <- paste0("BioSlider", x)
        species_names <- paste0(x, "Selected")
        species_selected <- unlist(mget(species_names))
        updateSliderInput(session, bioslider, max = trunc(mean(species_selected)),value=trunc(mean(species_selected)))
      }
      updateSliderInput(session, "AreaSlider", max = trunc(sum(AreaSelected)/1e6),value=trunc(sum(AreaSelected)/1e6))
      updateSliderInput(session, "VisitsSlider", max = trunc(mean(VisitsSelected)),value=trunc(mean(VisitsSelected)))
    }
    
    
    
    
  })
  
  observeEvent(input$tabs == "Clustering", {
    
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect
    
    updateCheckboxInput(session,"Trigger", label = "", value = TRUE)
    #input$Trigger<-TRUE
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main20",shconv)
    
    shinyjs::disable("choose1")
    shinyjs::disable("choose2")
    CurrentRound(0)
    listMaps<-list()
    listMaps[[1]]<-calcBaseMap$map
    listMaps[[2]]<-calcBaseMap$map
    
    if(!is.null(SavedVec)){
      
      AreaSelected<-AreaSelected0()
      CarbonSelected<-CarbonSelected0()
      # RedSquirrelSelected<-RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        var_name <- paste0(x, "Selected")
        value <- get(paste0(x, "Selected0"))
        assign(var_name, value())
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      # RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        var_name <- paste0(x, "Selected")
        value <- get(paste0(x, "SelectedSD0"))
        assign(var_name, value())
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD<-VisitsSelectedSD0()
      
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
                                     input_areaSlider_multiplicative_coefficient = FALSE)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      SelectedSimMatGlobal <<- SelectedSimMat2
      
      PROBAMAT<-Icalc$IVEC
      for(abc in 1:dim(Icalc$IVEC)[2]){
        PROBAMAT[,abc]<-1-ptruncnorm(Icalc$IVEC[,abc],a=LimitsMat[,abc], b=Inf)}
      
      # CONDPROB_AtLeast1<-(PROBAMAT[,1]>=alphaLVL)|(PROBAMAT[,2]>=alphaLVL)|(PROBAMAT[,3]>=alphaLVL)|(PROBAMAT[,4]>=alphaLVL)
      CONDPROB_AtLeast1 <- FALSE
      for (i in 1:ncol(PROBAMAT)) {
        CONDPROB_AtLeast1 <- CONDPROB_AtLeast1 | (PROBAMAT[,1]>=alphaLVL)
      }
      
      #  datAll = data.frame(CarbonMet=1*(PROBAMAT[,1]>=alphaLVL),
      ##                     redSquirrel=1*(PROBAMAT[,2]>=alphaLVL),
      #                   Area=1*(PROBAMAT[,3]>=alphaLVL),
      #                  Visits=1*(PROBAMAT[,4]>=alphaLVL))
      AtleastOneDat<-unique(SelectedSimMat2[CONDPROB_AtLeast1,])
      
      species_data_frame <- do.call("data.frame",
                                    setNames(lapply(SPECIES, function(x) bquote(SelectedSimMat2[.(x)])),
                                             SPECIES))
      datAll=as.matrix(data.frame(Carbon=SelectedSimMat2$Carbon,
                                  # redsquirrel=SelectedSimMat2$redsquirrel,
                                  species_data_frame,
                                  Area=SelectedSimMat2$Area,
                                  Visits=SelectedSimMat2$Visits))
      datAll2<-datAll[ConvertSample,]
      
      # DatBinaryCode<-paste0(1*(PROBAMAT[,1]>=alphaLVL),1*(PROBAMAT[,2]>=alphaLVL),1*(PROBAMAT[,3]>=alphaLVL),Visits=1*(PROBAMAT[,4]>=alphaLVL))
      DatBinaryCode <- ""
      for (i in 1:(ncol(PROBAMAT) - 1)) {
        DatBinaryCode <- paste0(DatBinaryCode, 1 * (PROBAMAT[,1]>=alphaLVL))
      }
      DatBinaryCode <- paste0(DatBinaryCode, Visits = 1 * (PROBAMAT[,ncol(PROBAMAT)]>=alphaLVL))
      
      
      DatBinaryCode0(DatBinaryCode)
      VecNbMet<-rep(0,length(CONDPROB_AtLeast1))
      # VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
      #             ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
      #             ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
      #             ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL)))]<-1
      # VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
      #             ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
      #             ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
      #             ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
      #             ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
      #             ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))
      # )]<-2
      # VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
      #             ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
      #             ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
      #             ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))
      # )]<-3
      # VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))
      # )]<-4
      for (i in 1:ncol(PROBAMAT)) {
        indices_list <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = i)
        indices <- FALSE
        for (j in 1:length(indices_list)) {
          indices <- indices | indices_list[[j]]
        }
        VecNbMet[indices] <- i
      }
      
      VecNbMet0(VecNbMet)
      
      priors <- c(prefeR::Normal(125,60),
                  prefeR::Normal(5,3))
      for (i in 1:N_SPECIES) {
        priors <- c(priors, prefeR::Normal(10,20))
      }
      priors <- c(priors, prefeR::Normal(8,5))
      pref <<- prefeR::prefEl(data=datAll2,
                              priors = priors)
      
      UniqueBinCodes<-unique(DatBinaryCode)
      
      if (dim(AtleastOneDat)[1]>=250)
      {
        NbRoundsMax(MaxRounds)
        
        LinesToCompare<-matrix(1,MaxRounds,2)
        #  indd<-1
        
        #  for(iws in 1:(as.integer(trunc(length(UniqueBinCodes)/2)))){
        #  LinesToCompare[iws,1]<-which(DatBinaryCode==UniqueBinCodes[indd])[1]
        # indd<-indd+1
        # LinesToCompare[iws,2]<-which(DatBinaryCode==UniqueBinCodes[indd])[1]
        # indd<-indd+1
        #}
        LinesToCompare[1,]<-sample(1:dim(datAll2)[1],2,replace=F)
        CurrentRound(1)
        
        LinesToCompareReactive(LinesToCompare)
        SelectedLine<-list()
        SelectedLine[[1]]<-SelectedSimMat2[ConvertSample[LinesToCompare[1,1]],]
        SelectedLine[[2]]<-SelectedSimMat2[ConvertSample[LinesToCompare[1,2]],]
        
        for(aai in 1:2){
          SwitchedOnCells<-SelectedLine[[aai]][1:length(SavedVec)]
          SelectedTreeCarbon<-SelectedLine[[aai]]$Carbon
          # SelectedBio<-SelectedLine[[aai]]$redsquirrel
          for (x in SPECIES) {
            var_name <- paste0("SelectedBio", x)
            value <- SelectedLine[[aai]][[x]]
            assign(var_name, value)
          }
          SelectedArea<-SelectedLine[[aai]]$Area
          SelectedVisits<-SelectedLine[[aai]]$Visits
          
          SelectedTreeCarbonSD<-SelectedLine[[aai]]$CarbonSD
          # SelectedBioSD<-SelectedLine[[aai]]$redsquirrelSD
          for (x in SPECIES) {
            var_name <- paste0("SelectedBioSD", x)
            value <- SelectedLine[[aai]][[paste0(x, "SD")]]
            assign(var_name, value)
          }
          SelectedVisitsSD<-SelectedLine[[aai]]$VisitsSD
          
          SELL<-(FullTable$extent==SelectedDropdown)
          if(!is.null(SELL)){
            sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
            sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
            for (iii in 1:length(SwitchedOnCells)){
              if(SavedVec[iii]==1){listMaps[[aai]]<-addPolygons(listMaps[[aai]],lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
              else{
                # Sometimes, SwitchedOnCells is a data.frame with 0 rows
                if(nrow(SwitchedOnCells) != 0 && SwitchedOnCells[iii]==1){
                  listMaps[[aai]]<-addPolygons(listMaps[[aai]],lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
              }
            }
          }
          addControlText <- ""
          for (x in SPECIES) {
            selectedBiospecie <- get(paste0("SelectedBio", x))
            selectedBioSDspecie <- get(paste0("SelectedBioSD", x))
            addControlText <- paste0(addControlText, x, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
          }
          
          listMaps[[aai]]<-listMaps[[aai]]%>%
            addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>",
                                     # "Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>",
                                     addControlText,
                                     "Area Planted: ",round(SelectedArea/1e6,2),"<br>",
                                     "Visitors: ",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                     "</p>"), position = "topright")
          
        }
        
        shinyjs::enable("choose1")
        shinyjs::enable("choose2")
        
      } else {
        listMaps[[1]]<-listMaps[[1]]%>%
          addControl(html = paste0("<p> Elicitation Not Possible as there are not enough samples that meet some of the targets
                                              </p>"), position = "topright")
        listMaps[[2]]<-listMaps[[2]]%>%
          addControl(html = paste0("<p> Elicitation Not Possible as there are not enough samples that meet some of the targets
                                              </p>"), position = "topright")
        shinyjs::disable("choose1")
        shinyjs::disable("choose2")
      }
      
    }
    
    listMaps <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, listMaps = listMaps)
    
    output$ClusterPage<-renderLeaflet({listMaps[[1]]})
    output$ClusterPage2<-renderLeaflet({listMaps[[2]]})
    
    
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
                           SPECIES_ARG3 = SPECIES,
                           N_TARGETS_ARG2 = N_TARGETS)
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
                           SPECIES_ARG3 = SPECIES,
                           N_TARGETS_ARG2 = N_TARGETS)
  })
  
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if(!is.null(click)){SavedVec<-ClickedVector()
    for(iii in 1:length(SavedVec)){
      if((click$id == paste0("Square",iii))){SavedVec[iii]<-ifelse(SavedVec[iii]==1,0,1);ClickedVector(SavedVec)}
    }
    }
    
  })
  
  
  output$map <- renderLeaflet({
    
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect#"Ennerdale"#input$inSelect#"Abbeyford"#"Ennerdale"#
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main",shconv)
    map<-calcBaseMap$map
    
    if(!is.null(SavedVec)){
      
      AreaSelected<-AreaSelected0()
      CarbonSelected<-CarbonSelected0()
      # RedSquirrelSelected<-RedSquirrelSelected0()
      SpeciesListSelected <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "Selected0"))
        SpeciesListSelected[x] <- list(value())
      }
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      # RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      SpeciesListSelectedSD <- list()
      for (x in SPECIES) {
        value <- get(paste0(x, "SelectedSD0"))
        var_name <- paste0(x, "SD")
        SpeciesListSelectedSD[var_name] <- list(value())
      }
      VisitsSelectedSD<-VisitsSelectedSD0()
      
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
                                     VisitsSelectedSD = VisitsSelectedSD)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      SelecTargetCarbon <- tmp$SelecTargetCarbon
      # SelecTargetBio <- tmp$SelecTargetBio
      condition <- TRUE
      for (x in SPECIES) {
        var_name <- paste0("SelecTargetBio", x)
        value <- tmp[[var_name]]
        assign(var_name, value)
        
        condition <- condition & SelectedSimMat2[[x]] >= value
      }
      SelecTargetArea <- tmp$SelecTargetArea
      SelecTargetVisits <- tmp$SelecTargetVisits
      rm(tmp)
      
      # SubsetMeetTargets<-SelectedSimMat2[(SelectedSimMat2$Carbon>=SelecTargetCarbon)&
      #                                      # (SelectedSimMat2$redsquirrel>=SelecTargetBio)&
      #                                      condition&
      #                                      (SelectedSimMat2$Area>=SelecTargetArea)&
      #                                      (SelectedSimMat2$Visits>=SelecTargetVisits),]
      
      SubsetMeetTargets<-SelectedSimMat2[Icalc$NROYTotal,]
      
      if(dim(SubsetMeetTargets)[1]>0){
        if(max(SelectedSimMat2$Carbon)!=min(SelectedSimMat2$Carbon)){
          DistSliderCarbon<-(SubsetMeetTargets$Carbon-SelecTargetCarbon)/(max(SelectedSimMat2$Carbon)-min(SelectedSimMat2$Carbon))}else{
            DistSliderCarbon<-(SubsetMeetTargets$Carbon-SelecTargetCarbon)/(max(SelectedSimMat2$Carbon))
          }
        # if(max(SelectedSimMat2$redsquirrel)!=min(SelectedSimMat2$redsquirrel)){
        #   
        #   DistSliderBio<-(SubsetMeetTargets$redsquirrel-SelecTargetBio)/(max(SelectedSimMat2$redsquirrel)-min(SelectedSimMat2$redsquirrel))}else{
        #     DistSliderBio<-(SubsetMeetTargets$redsquirrel-SelecTargetBio)/(max(SelectedSimMat2$redsquirrel))
        #   }
        DistSliderBioListDataframes <- list()
        for (x in SPECIES) {
          SelecTargetBiospecie <- get(paste0("SelecTargetBio", x))[[1]]
          var_name <- paste0("DistSliderBio", x)
          if(max(SelectedSimMat2[x]) != min(SelectedSimMat2[x])) {
            value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie) / (max(SelectedSimMat2[[x]]) - min(SelectedSimMat2[[x]]))
          } else {
            value <- (SubsetMeetTargets[[x]] - SelecTargetBiospecie) / (max(SelectedSimMat2[[x]]))
          }
          assign(var_name, value)
          DistSliderBioListDataframes[x] <- data.frame(x = value)
        }
        if(max(SelectedSimMat2$Area)!=min(SelectedSimMat2$Area)){
          DistSliderArea<-(SubsetMeetTargets$Area-SelecTargetArea)/(max(SelectedSimMat2$Area)-min(SelectedSimMat2$Area))
        } else {
          DistSliderArea<-(SubsetMeetTargets$Area-SelecTargetArea)/(max(SelectedSimMat2$Area))
        }
        if(max(SelectedSimMat2$Visits)!=min(SelectedSimMat2$Visits)){
          DistSliderVisits<-(SubsetMeetTargets$Visits-SelecTargetVisits)/(max(SelectedSimMat2$Visits)-min(SelectedSimMat2$Visits))
        } else {
          DistSliderVisits<-(SubsetMeetTargets$Visits-SelecTargetVisits)/(max(SelectedSimMat2$Visits))
        }
        
        # REMINDER TO SCALE VALUES
        DistSliderBioDataframe <- do.call(cbind, DistSliderBioListDataframes)
        # SelecdMinRows<-which((DistSliderCarbon+DistSliderBio+DistSliderArea+DistSliderVisits)==min(DistSliderCarbon+DistSliderBio+DistSliderArea+DistSliderVisits))
        # SelecdMinRows<-which((DistSliderCarbon+DistSliderBio1+DistSliderBio2+DistSliderArea+DistSliderVisits)==min(DistSliderCarbon+DistSliderBio1+DistSliderBio2+DistSliderArea+DistSliderVisits))
        SelecdMinRows<-which.min(DistSliderCarbon + rowSums(DistSliderBioDataframe) + DistSliderArea + DistSliderVisits)
        SelectedMins<-SubsetMeetTargets[SelecdMinRows,]
        SelecRow<-which.min(rowSums(SelectedMins[1:length(SavedVec),]))
        SwitchedOnCells<-SelectedMins[SelecRow,1:length(SavedVec)]
        
        SELL<-(FullTable$extent==SelectedDropdown)
        if(!is.null(SELL)){
          SelectedTreeCarbon<-SelectedMins[SelecRow,]$Carbon
          # SelectedBio<-SelectedMins[SelecRow,]$redsquirrel
          for (x in SPECIES) {
            var_name <- paste0("SelectedBio", x)
            value <- SelectedMins[SelecRow, x]
            assign(var_name, value)
          }
          SelectedArea<-SelectedMins[SelecRow,]$Area
          SelectedVisits<-SelectedMins[SelecRow,]$Visits
          
          SelectedTreeCarbonSD<-SelectedMins[SelecRow,]$CarbonSD
          # SelectedBioSD<-SelectedMins[SelecRow,]$redsquirrelSD
          for (x in SPECIES) {
            var_name <- paste0("SelectedBioSD", x)
            value <- SelectedMins[SelecRow, paste0(x, "SD")]
            assign(var_name, value)
          }
          SelectedVisitsSD<-SelectedMins[SelecRow,]$VisitsSD
          
          
          
          sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
          sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
          for (iii in 1:length(SwitchedOnCells)){
            if(SavedVec[iii]==1){map<-addPolygons(map,lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
            else{
              # Sometimes, SwitchedOnCells is a data.frame with 0 rows
              if(nrow(SwitchedOnCells) != 0 && SwitchedOnCells[iii]==1){
                map<-addPolygons(map,lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
            }
          }
          
          addControlText <- ""
          for (x in SPECIES) {
            selectedBiospecie <- get(paste0("SelectedBio", x))
            selectedBioSDspecie <- get(paste0("SelectedBioSD", x))
            addControlText <- paste0(addControlText, x, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
          }
          map<-map%>%
            addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>",
                                     # "Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>",
                                     addControlText,
                                     "Area Planted: ",round(SelectedArea/1e6,2),"<br>",
                                     "Visitors: ",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                     "</p>"), position = "topright")
          
        }else{ map<-map%>%
          addControl(html = paste0("<p> Targets Cannot be met</p>"), position = "topright")
        }
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
  output$map2 <- renderLeaflet({
    
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect
    calcBaseMap <- BaseMap(SelectedDropdown, layerId = "main2", shconv)
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
                                     VisitsSelectedSD = VisitsSelectedSD)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT<-1-pnorm(Icalc$IVEC)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      # CONDPROBA <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS)
      
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
                                              SPECIES_ARG2 = SPECIES)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (x in SPECIES) {
          selectedBiospecie <- mapresults[[paste0("SelectedBio", x)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", x)]]
          addControlText <- paste0(addControlText, x, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea/1e6, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        Text1(paste0("Strategies that meet all ", N_TARGETS, " targets:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1)))
        
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
    calcBaseMap <- BaseMap(SelectedDropdown, layerId = "main3", shconv)
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
                                     VisitsSelectedSD = VisitsSelectedSD)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT<-1-pnorm(Icalc$IVEC)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      # CONDPROBA3PositiveLIST <- list()
      # CONDPROBA3PositiveLIST[[1]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA3PositiveLIST[[2]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA3PositiveLIST[[3]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      # CONDPROBA3PositiveLIST[[4]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA3PositiveLIST <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS - 1)
      
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
      SubsetMeetTargets <- subset_meet_targets(PROBAMAT = PROBAMAT, SelectedSimMat2 = SelectedSimMat2, CONDPROBAPositiveLIST = CONDPROBA3PositiveLIST, TARGETS = TARGETS, nb_targets_met = N_TARGETS - 1)
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              SPECIES_ARG2 = SPECIES)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (x in SPECIES) {
          selectedBiospecie <- mapresults[[paste0("SelectedBio", x)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", x)]]
          addControlText <- paste0(addControlText, x, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea/1e6, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        
        Text2(paste0("Strategies that meet exactly ", N_TARGETS - 1, " targets:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Target Not Met:", mapresults$SelectedLine$NotMet))
        
      } else {
        Text2(paste("No strategy where exactly", N_TARGETS - 1, "targets are met found"))
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
  output$map4 <- renderLeaflet({
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect
    calcBaseMap <- BaseMap(SelectedDropdown, layerId = "main4", shconv)
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
                                     VisitsSelectedSD = VisitsSelectedSD)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT<-1-pnorm(Icalc$IVEC)
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
      CONDPROBA2PositiveLIST <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS - 2)
      
      # SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[1]], ], NotMet = rep("Carbon,redSquirrel", sum(CONDPROBA2PositiveLIST[[1]])))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[2]], ], NotMet = rep("Carbon,Area", sum(CONDPROBA2PositiveLIST[[2]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[3]], ], NotMet = rep("Carbon,NbVisits", sum(CONDPROBA2PositiveLIST[[3]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[4]], ], NotMet = rep("redSquirrel,Area", sum(CONDPROBA2PositiveLIST[[4]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[5]], ], NotMet = rep("redSquirrel,NbVisits", sum(CONDPROBA2PositiveLIST[[5]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[6]], ], NotMet = rep("Area,NbVisits", sum(CONDPROBA2PositiveLIST[[6]]))))
      SubsetMeetTargets <- subset_meet_targets(PROBAMAT = PROBAMAT, SelectedSimMat2 = SelectedSimMat2, CONDPROBAPositiveLIST = CONDPROBA2PositiveLIST, TARGETS = TARGETS, nb_targets_met = N_TARGETS - 2)
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              SPECIES_ARG2 = SPECIES)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (x in SPECIES) {
          selectedBiospecie <- mapresults[[paste0("SelectedBio", x)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", x)]]
          addControlText <- paste0(addControlText, x, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea/1e6, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        
        Text3(paste0("Strategies that meet exactly ", N_TARGETS - 2, " targets:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Targets Not Met:", mapresults$SelectedLine$NotMet))
        
      } else {
        Text3(paste("No strategy where exactly", N_TARGETS - 2, "targets are met found"))
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
  output$map5 <- renderLeaflet({
    SavedVec <- ClickedVector()
    SelectedDropdown <- input$inSelect
    calcBaseMap <- BaseMap(SelectedDropdown, layerId = "main5", shconv)
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
                                     VisitsSelectedSD = VisitsSelectedSD)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      LimitsMat <- tmp$LimitsMat
      rm(tmp)
      
      # PROBAMAT<-1-pnorm(Icalc$IVEC)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      # CONDPROBA1PositiveLIST <- list()
      # CONDPROBA1PositiveLIST[[1]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA1PositiveLIST[[2]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA1PositiveLIST[[3]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      # CONDPROBA1PositiveLIST[[4]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA1PositiveLIST <- check_targets_met(PROBAMAT, target = alphaLVL, nb_targets_met = N_TARGETS - 3)
      
      # SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[1]], ], Met = rep("Carbon", sum(CONDPROBA1PositiveLIST[[1]])))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[2]], ], Met = rep("redSquirrel", sum(CONDPROBA1PositiveLIST[[2]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[3]], ], Met = rep("Area", sum(CONDPROBA1PositiveLIST[[3]]))))
      # SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[4]], ], Met = rep("NbVisits", sum(CONDPROBA1PositiveLIST[[4]]))))
      SubsetMeetTargets <- subset_meet_targets(PROBAMAT = PROBAMAT, SelectedSimMat2 = SelectedSimMat2, CONDPROBAPositiveLIST = CONDPROBA1PositiveLIST, TARGETS = TARGETS, nb_targets_met = N_TARGETS - 3)
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              SPECIES_ARG2 = SPECIES)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        addControlText <- ""
        for (x in SPECIES) {
          selectedBiospecie <- mapresults[[paste0("SelectedBio", x)]]
          selectedBioSDspecie <- mapresults[[paste0("SelectedBioSD", x)]]
          addControlText <- paste0(addControlText, x, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
        }
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>",
                                               # "Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>",
                                               addControlText,
                                               "Area Planted: ", round(SelectedArea/1e6, 2), "<br>",
                                               "Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        
        Text4(paste0("Strategies that meet only ", N_TARGETS - 3, " target:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Target Met:", mapresults$SelectedLine$Met))
        
      } else {
        Text4(paste("No strategy where only", N_TARGETS - 3, "target is met found"))
      }
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
}

shinyApp(ui, server)
