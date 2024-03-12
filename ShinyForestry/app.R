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
library(purrr)
library(sp)
library(colorspace)
library(rjson)
library(arrow)
library(lwgeom)
library(mvtnorm)
library(dplyr)
# if (!require("prefeR")) {
  # install.packages("prefeR", lib = "/RPackages")
  # detach("package:prefeR", unload = TRUE)
  # loadNamespace("prefeR")
# }
# packages <- c("car", "shinyjs", "shiny", "shinyjqui", "leaflet", "sf", "ggplot2", "geosphere", "feather", "readr", "dplyr", "tidyverse", "gsubfn", "ggpubr", "comprehenr", "Rtsne", "mclust", "seriation", "jsonlite", "viridis", "ggmap", "shinyjqui", "MASS", "shinyWidgets", "truncnorm", "GGally")
# if(!all( packages %in% (.packages()) )) {
  # for(pkg in packages) {
    # if( !require(pkg, character.only = TRUE) ) {
      # install.packages(pkg, lib = "/RPackages")
      # library(pkg, character.only = TRUE)
    # }
  # }
# }
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
                              Group = name_conversion[, 1])
# Replace Invertebrate - bee/beetle/butterfly/cricket/moth by Pollinator
# Crashes on the server for some reason, so we use data.frames instead
# dplyr::mutate(Group = dplyr::case_when(grepl("bee|beetle|butterfly|cricket|moth", Group) ~ "Pollinator",
# .default = Group)) %>%
indices <- grep("bee|beetle|butterfly|cricket|moth", name_conversion$Group)
name_conversion[indices, "Group"] <- "Pollinator"
# dplyr::mutate(Group = dplyr::case_when(Group == "Invertebrate - bee" ~ "Pollinator",
#                                        Group == "Invertebrate - beetle" ~ "Pollinator",
#                                        Group == "Invertebrate - butterfly" ~ "Pollinator",
#                                        Group == "Invertebrate - cricket" ~ "Pollinator",
#                                        Group == "Invertebrate - moth" ~ "Pollinator",
#                                        .default = Group)) %>%
# Acanthis cabaret -> Acanthis_cabaret, and Neottia nidus-avis -> Neottia_nidus_avis
name_conversion <- name_conversion %>%
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
ElicitatorAppFolder<-"./ElicitatorOutput/"
JulesAppFolder<-"./JulesOutput/"

########################## Pre-processing
#remove all directories starting with "land"
unlink(list.dirs(ElicitatorAppFolder)[substr(list.dirs(ElicitatorAppFolder),1,nchar(ElicitatorAppFolder)+4)==paste0(ElicitatorAppFolder,"land")], recursive = T)

DIRR<-list.files(ElicitatorAppFolder,full.names = TRUE,recursive=F)
CTIMES<-file.info(DIRR)$ctime
LatestFileNames<-c(LandPFileName="",UnitsFileName="",OutcomesFileName="")
LatestFileNames[1]<-DIRR[CTIMES==max(CTIMES[(substr(DIRR,1,nchar(ElicitatorAppFolder)+4)==paste0(ElicitatorAppFolder,"land"))])]
LatestFileNames[2]<-DIRR[CTIMES==max(CTIMES[(substr(DIRR,1,nchar(ElicitatorAppFolder)+4)==paste0(ElicitatorAppFolder,"deci"))])]
LatestFileNames[3]<-DIRR[CTIMES==max(CTIMES[(substr(DIRR,1,nchar(ElicitatorAppFolder)+4)==paste0(ElicitatorAppFolder,"outc"))])]
LatestFilesInfo<-data.frame(LatestFileNames,ctime=file.info(LatestFileNames)$ctime)

LatestFilesInfo$ctime <- format(LatestFilesInfo$ctime, "%Y-%m-%d %H:%M:%OS4")
#write.csv(LatestFilesInfo,"d://ElicitatorOutput//LastestFilesInfo.csv")
PrevFilesInfo<-read.csv(paste0(ElicitatorAppFolder,"LastestFilesInfo.csv"))


if(sum(PrevFilesInfo$ctime!=LatestFilesInfo$ctime)>0){
if(file.exists(paste0(ElicitatorAppFolder,"Parcels.geojson"))){file.remove(paste0(ElicitatorAppFolder,"Parcels.geojson"))}
if(file.exists(paste0(ElicitatorAppFolder,"FullTableMerged.geojson"))){file.remove(paste0(ElicitatorAppFolder,"FullTableMerged.geojson"))}
if(file.exists(paste0(ElicitatorAppFolder,"FullTableNotAvail.geojson"))){file.remove(paste0(ElicitatorAppFolder,"FullTableNotAvail.geojson"))}  
UnZipDirName<-paste0(substr(LatestFilesInfo$LatestFileNames[1],1,nchar(LatestFilesInfo$LatestFileNames[1])-4))
#if(dir.exists(UnZipDirName)){unlink(UnZipDirName, recursive = T)}
dir.create(UnZipDirName)
unzip(LatestFilesInfo$LatestFileNames[1], exdir = UnZipDirName)
################## Load necessary files
# PROJdir<-system.file("proj/proj.db", package = "sf")
# PROJdir<-substring(PROJdir,1,nchar(PROJdir)-8)
# sf_proj_search_paths(PROJdir)
shconv<-sf::st_read(paste0(UnZipDirName,"//land_parcels.shp"))
if(is.null(shconv$extent)){shconv$extent<-"NoExtent"}
st_write(shconv, paste0(ElicitatorAppFolder,"Parcels.geojson"))
JulesMean<-arrow::read_feather(paste0(JulesAppFolder,"JulesApp-rcp26-06-mean-monthly.feather"))[,c("x","y","mean337")]
JulesSD<-arrow::read_feather(paste0(JulesAppFolder,"JulesApp-rcp26-06-sd-monthly.feather"))[,c("x","y","sd337")]
SquaresLoad<-sf::st_read(paste0(JulesAppFolder,"SEER//Fishnet_1km_to_SEER_net2km.shp"))
Sqconv<-st_transform(SquaresLoad, crs = 4326)
XYMAT<-read.csv(paste0(JulesAppFolder,"XYMat_1km.csv"))[,-1]
CorrespondenceJules<-read.csv(paste0(JulesAppFolder,"/CorrespondanceSqToJules.csv"))[,-1]
seer2km<-st_read(paste0(JulesAppFolder,"/SEER_net2km.shp"))
jncc100<-read.csv(paste0(JulesAppFolder,"/beta_JNCC100_interact_quad.csv"))
speciesprob40<- read.csv(paste0(JulesAppFolder,"scenario_species_prob_40.csv"), header = FALSE)
climatecells<-read.csv(paste0(JulesAppFolder,"climate_cells.csv"))
###################
sf_use_s2(FALSE)
lsh<-dim(shconv)[1]
AllUnits<- rjson::fromJSON(file=LatestFilesInfo$LatestFileNames[2])$decision_unit_ids
Uni<-unique(AllUnits)
FullTab<-data.frame(extent="NoExtent",x=rep(0,length(Uni)),y=rep(0,length(Uni)),area=rep(1,length(Uni)),
                          JulesMean=rep(15,length(Uni)),
                          JulesSD=rep(1,length(Uni)),VisitsMean=rep(30,length(Uni)),
                          VisitsSD=rep(2,length(Uni)),BioMean_Sciurus_vulgaris=rep(0.5,length(Uni)),
                          BioSD_Sciurus_vulgaris=rep(0.02,length(Uni)),units=Uni)

tt<-proc.time()

#MER<-list()
#for(ii in 1:length(Uni))
#{
#  SELLL<-shconv$geometry[AllUnits==Uni[ii]]
#  MER[[ii]]<-st_union(SELLL[1],SELLL[2])
#  if(length(SELLL)>2){
#    for (jj in 3:length(SELLL)){
#      MER[[ii]]<-st_union(MER[[ii]],st_make_valid(SELLL[jj]))
#      }
#    
#  }
#  
#}

tt2<-proc.time()-tt

tt3<-proc.time()
MER<-list()
for(ii in 1:length(Uni))
{
  SELLL<-shconv$geometry[AllUnits==Uni[ii]]
  MER[[ii]]<-st_union(st_make_valid(SELLL))
  
}
tt4<-proc.time()-tt3

FullTable <- st_sf(FullTab,geometry=do.call(c,MER),crs=4326)
############################################ Replace the Jules Mean here



#XVEC<-sort(unique(c(XYMAT$XMIN,XYMAT$XMAX)))
#YVEC<-sort(unique(c(XYMAT$YMIN,XYMAT$YMAX)))


#keptLines<-NULL
#shconvBack<-st_transform(shconv, crs =st_crs(27700))
#for(ii in 1:length(shconv$geometry))
#{
#  MERconvback<-shconvBack$geometry[[ii]][[1]]##
#
#      xmin<-min(MERconvback[,1])
#      DIF<-xmin-XVEC
#      XMINVEC<-(XVEC[DIF>=0])[which.min(DIF[DIF>=0])]
#      
#      xmax<-max(MERconvback[,1])
#      DIF2<-XVEC-xmax
#      XMAXVEC<-(XVEC[DIF2>=0])[which.min(DIF2[DIF2>=0])]
#      
#      ymin<-min(MERconvback[,2])
#      DIF3<-ymin-YVEC
#      YMINVEC<-(YVEC[DIF3>=0])[which.min(DIF3[DIF3>=0])]
#      
#      ymax<-max(MERconvback[,2])
 #     DIF4<-YVEC-ymax
#      YMAXVEC<-(YVEC[DIF4>=0])[which.min(DIF4[DIF4>=0])]
#      
#      keptLines<-unique(c(keptLines,which((XYMAT$XMAX<=XMAXVEC)&(XYMAT$XMIN>=XMINVEC)&(XYMAT$YMIN>=YMINVEC)&(XYMAT$YMAX<=YMAXVEC))))
#   
#}
keptLines<-sort(which(as.numeric(summary(sf::st_intersects(Sqconv,shconv))[,1])!=0))

SELECTEDSquaresconv<-Sqconv$geometry[keptLines]
LinesJules<-CorrespondenceJules[keptLines]
# Find lines where Jules is not available
LinesJulesNoMinus1<-which(LinesJules==(-1))
LinesJules[LinesJulesNoMinus1]<-1
SelectedJulesMeanSq<-JulesMean[CorrespondenceJules[keptLines],]
SelectedJulesMeanSq[LinesJulesNoMinus1]<-0
SelectedJulesSDSq<-JulesSD[CorrespondenceJules[keptLines],]
SelectedJulesSDSq[LinesJulesNoMinus1]<-0

SELECTEDSquaresconvTab<-data.frame(idSq=seq_along(SELECTEDSquaresconv))
SELECTEDSquaresconvTab<-st_sf(SELECTEDSquaresconvTab,geometry=SELECTEDSquaresconv,crs=4326)


FullTableCopy<-FullTable
FullTableCopy$idPoly<-seq_along(FullTableCopy$geometry)

#st_as_sf(data.frame(geometry=SELECTEDSquaresconv))
#st_as_sf(data.frame(FullTable))

INTT<-st_intersection(st_make_valid(SELECTEDSquaresconvTab),st_make_valid(FullTableCopy))
INTT$area<-st_area(INTT)/1e6

NBSIMS<-500
for(ii in 1:length(FullTableCopy$geometry))
{
  SELLLines<-INTT$idPoly==ii
  SELLSqs<-INTT$idSq[SELLLines]
  SELLWeights<-INTT$area[SELLLines]
  SellWeightsArr<-t(matrix(SELLWeights,length(SELLWeights),NBSIMS))
  
  SelJulesMeans<-SelectedJulesMeanSq$mean337[SELLSqs]
  SelJulesSDs<-SelectedJulesSDSq$sd337[SELLSqs]
  SimuArr<-rmvnorm(NBSIMS,mean=SelJulesMeans,sigma=diag(SelJulesSDs^2))
  
  FullTable$JulesMean[ii]<-sum(colMeans(SimuArr*SellWeightsArr))
  FullTable$JulesSD[ii]<-sd(rowSums(SimuArr*SellWeightsArr))
  FullTable$area[ii]<-sum(SELLWeights)
}

# Replace Biodiversity columns with correct ones
FullTable <- convert_bio_to_polygons_from_elicitor_and_merge_into_FullTable(Elicitor_table = FullTable,
                                                                            speciesprob40=speciesprob40,
                                                                            seer2km=seer2km,
                                                                            jncc100=jncc100,
                                                                            climatecells=climatecells
)
# Add richness columns
FullTable <- add_richness_columns(FullTable = FullTable, name_conversion = name_conversion) %>% st_as_sf()

#aa<-leaflet()
#aa<-  addTiles(aa) 
#for(aaa in 1:30){
#aa<-addPolygons(aa,data=MER[[aaa]])
#}#

  #aa<-addPolygons(aa,data=Sqconv$geometry[keptLines],col="red")

#######################################
st_write(FullTable,paste0(ElicitatorAppFolder,"FullTableMerged.geojson"))


##################
FullTableNotAvail<-data.frame(extent=NULL)
st_write(FullTableNotAvail, paste0(ElicitatorAppFolder,"FullTableNotAvail.geojson"))


   write.csv(LatestFilesInfo,paste0(ElicitatorAppFolder,"LastestFilesInfo.csv"))
}else{

  shconv<-sf::st_read(paste0(ElicitatorAppFolder,"Parcels.geojson"))
  FullTable<-st_read(paste0(ElicitatorAppFolder,"FullTableMerged.geojson"))
  FullTableNotAvail<-sf::st_read( paste0(ElicitatorAppFolder,"FullTableNotAvail.geojson"))

}


#shconv<-sf::st_read("d://BristolParcels.geojson")
#FullTable<-st_read("d://BristolFullTableMerged.geojson")
#FullTableNotAvail<-sf::st_read("d://BristolFullTableNotAvail.geojson")
#shconv<-sf::st_read("d://ForestryParcels.geojson")
#FullTable<-st_read("d://ForestryFullTable.geojson")
#FullTableNotAvail<-sf::st_read("d://ForestryFullTableNotAvail.geojson")

#shconv<-sf::st_read("d://PoundsgateParcels.geojson")
#FullTable<-st_read("d://PoundsgateFullTable.geojson")
#FullTableNotAvail<-sf::st_read("d://PoundsgateFullTableNotAvail.geojson")


STDMEAN<-0.05
STDSTD<-0.01

NSamp<-5000
simul636<-matrix(0,NSamp,dim(FullTable)[1])
for (aaa in 1:NSamp)
{
  Uniqunits<-unique(FullTable$units)
  pp<-runif(1)
  RandSamp<-rmultinom(length(Uniqunits),1,c(pp,1-pp))[1,]
  for(bbb in 1:length(Uniqunits)){
    simul636[aaa,FullTable$units==Uniqunits[bbb]]<-RandSamp[bbb]  
    }
}



# # Move rows from FullTableNotAvail to FullTable with blank data
# FullTableAdd <- with(FullTableNotAvail, data.frame("extent" = extent,
                                                   # "id" = id,
                                                   # "area" = 1,
                                                   # x = lgn.1,
                                                   # y = lat.1,
                                                   # xbgn = 0,
                                                   # ybgn = 0,
                                                   # lgn.1 = lgn.1, lgn.2 = lgn.2, lgn.3=lgn.3,lgn.4=lgn.4,lgn.5=lgn.5,lat.1=lat.1,lat.2=lat.2,lat.3=lat.3,lat.4=lat.4,lat.5=lat.5,
                                                   # JulesMean=0,JulesSD=0,
                                                   # VisitsMean=0,VisitsSD=0)) %>%
  # mutate(across(x:lat.5, as.numeric)) %>% 
  # mutate(across(c(id, VisitsMean), as.integer)) %>% as_tibble()

# cols <- colnames(FullTable)
# FullTableAdd <- merge(FullTable, FullTableAdd, all = TRUE)
# FullTableAdd <- FullTableAdd[, cols]
# FullTableAdd$xybgn <- 1
# rows_na <- which(is.na(FullTableAdd[, paste0("BioMean_", name_conversion[1, "Specie"])]))
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

alphaLVL<-0.9

MaxRounds<-5

ConvertSample<-sample(1:5000,200)

SPECIES <- c(name_conversion[1:2, "Specie"], "Pollinator", "All")
SPECIES_ENGLISH <- c(name_conversion[1:2, "English_Specie"], "Pollinator", "All")
N_SPECIES <- length(SPECIES)
TARGETS <- c("Carbon", SPECIES, "Area", "NbVisits")
N_TARGETS <- length(TARGETS)

# slider_list <- list(
#   sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25)
# )
# Add sliderInput("BioSliderSPECIE","Average SPECIE % increase:",min=0,max=36,value=25) for each specie

verticalLayout_params <- c(list(sliderInput("SliderMain","Tree Carbon Stored (2050):",min=0,max=870,value=800)),
                           lapply(SPECIES, function(x, fulltable) {
                             # max_specie <- round(max(fulltable[, paste0("BioMean_", x)]))
                             # value <- round(max_specie / 2)
                             max_specie <- 36
                             value <- 1
                             return(bquote(sliderInput(paste0("BioSlider", .(x)), 
                                                       if (.(x) %in% name_conversion$Group || .(x) == "All") paste("Species Richness (", .(x), ")") else paste(name_conversion[which(name_conversion$Specie == .(x)), "English_specie"], "Presence (%):"),
                                                       min = 0,
                                                       max = .(max_specie),
                                                       value = .(value),
                                                       step = 0.5)))
                           }, fulltable = FullTable),
                           list(sliderInput("AreaSlider", HTML("Area Planted (km<sup>2</sup>)"),min=0,max=25,value=15)),
                           list(sliderInput("VisitsSlider", "Average Number of Visitors per cell:",min=0,max=750,value=400)))


ui <- fluidPage(useShinyjs(),tabsetPanel(id = "tabs",
                                         tabPanel("Maps",fluidPage(fluidRow(
                                           column(9,
                                                  #  tags$head(tags$style(HTML("#map {pointer-events: none;}"))),
                                                  #tags$style("#inSelect {color: white; background-color: transparent; border: white;}"),
                                                  selectInput("inSelect", "area",sort(unique(c(FullTable$extent,FullTableNotAvail$extent))),FullTable$extent[1]),
                                                  #fluidRow(column(4,selectInput("ColourScheme","Colour Scheme",c("blue/red","rainbow dark/light",
                                                   #                                                              "rainbow dark/red",
                                                    #                                                             "Terrain darkened/lightened",
                                                     #                                                            "Terrain darkened/red",
                                                      #                                                           "Viridis darkened/red"))),
                                                  #column(4,sliderInput("Darken","Darkening Factor:",min=-100,max=100,value=70)),
                                                  #column(4,sliderInput("Lighten","Lightening Factor:",min=-100,max=100,value=50))),
                                                  jqui_resizable(leafletOutput("map",height = 800,width="100%"))
                                           ),
                                           column(3,
                                                  # verticalLayout(sliderInput("SliderMain","Tree Carbon Stored (2050):",min=0,max=870,value=800),
                                                  #                sliderInput("BioSliderAcanthis_cabaret", "Average Acanthis_cabaret % increase:", min = 0, max = 36, value = 25, step=0.01),
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
  Text4<-reactiveVal("")
  # # Add TextN<-reactiveVal("") for each specie
  # for (i in 1:N_SPECIES) {
  #   var_name <- paste0("Text", i + 3)
  #   value <- reactiveVal("")
  #   assign(var_name, value)
  # }
  
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
  
  ColorLighteningFactor<-reactiveVal(0.5)
  ColorDarkeningFactor<-reactiveVal(0.5)
  
  ColourScheme<-reactiveVal("Viridis darkened/red")
 # observeEvent(input$Darken,{
#    ColorDarkeningFactor(input$Darken/100)
#  })
 # observeEvent(input$Lighten,{
#    ColorLighteningFactor(input$Lighten/100)
#  })
    
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
  
  observe({
    Uni<-unique(FullTable$extent)
    if(length(Uni)>1){  shinyjs::show("inSelect")}else{
      
      if (    unique(FullTable$extent)[1]=="NoExtent") {
        shinyjs::hide("inSelect")
      } else {
        shinyjs::show("inSelect")
      }
    }
   
  })
  
  
  
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
    
    SelectedSquares<-cbind(extent=FullTable$extent[FullTable$extent==SelectedDropdown])#,FullTable$lgn.1[FullTable$extent==SelectedDropdown],
                        #   FullTable$lat.1[FullTable$extent==SelectedDropdown])
    
    if(!(dim(SelectedSquares)[1]==0)){
      AreaSelected<-FullTable$area[FullTable$extent==SelectedDropdown]
      CarbonSelected<-(FullTable$JulesMean[FullTable$extent==SelectedDropdown])
      # RedSquirrelSelected<-FullTable$BioMean_Sciurus_vulgaris[FullTable$extent==SelectedDropdown]
      for (x in SPECIES) {
        biomean_var <- paste0("BioMean_", x)
        var_name <- paste0(x, "Selected")
        # value <- FullTable[FullTable$extent==SelectedDropdown, biomean_var]
        value <- FullTable[[biomean_var]][FullTable$extent==SelectedDropdown]
        assign(var_name, value)
      }
      VisitsSelected<-FullTable$VisitsMean[FullTable$extent==SelectedDropdown]
      
      CarbonSelectedSD<-(FullTable$JulesSD[FullTable$extent==SelectedDropdown])
      # RedSquirrelSelectedSD<-FullTable$BioSD_Sciurus_vulgaris[FullTable$extent==SelectedDropdown]
      for (x in SPECIES) {
        biosd_var <- paste0("BioSD_", x)
        var_name <- paste0(x, "SelectedSD")
        # value <- FullTable[FullTable$extent==SelectedDropdown, biosd_var]
        value <- FullTable[[biosd_var]][FullTable$extent==SelectedDropdown]
        assign(var_name, value)
      }
      VisitsSelectedSD<-FullTable$VisitsSD[FullTable$extent==SelectedDropdown]
      
      
      
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
      # updateSliderInput(session, "BioSlider", max = trunc(100*mean(RedSquirrelSelected))/100,value=trunc(100*mean(RedSquirrelSelected))/100,step=0.01)
      for (x in SPECIES) {
        bioslider <- paste0("BioSlider", x)
        species_names <- paste0(x, "Selected")
        species_selected <- unlist(mget(species_names))
        updateSliderInput(session, bioslider, max = trunc(mean(species_selected)),value=trunc(mean(species_selected)), step = 0.5)
      }
      updateSliderInput(session, "AreaSlider", max = trunc(100*sum(AreaSelected))/100,value=trunc(100*sum(AreaSelected))/100)
      updateSliderInput(session, "VisitsSlider", max = trunc(mean(VisitsSelected)),value=trunc(mean(VisitsSelected)))
    }
    
    
    
    
  })

  
  observeEvent(input$tabs == "Clustering", {
    
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect
    
    updateCheckboxInput(session,"Trigger", label = "", value = TRUE)
    #input$Trigger<-TRUE
    calcBaseMap<-BaseMap2(SelectedDropdown,layerId="main20",shconv)
    
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
        priors <- c(priors, prefeR::Normal(5,5))
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

            SELGEO<-FullTable$geometry[SELL]
            SELGEOFull<-FullTable[SELL,]
            SELGEOFull$layerId<-paste0("Square",1:dim(SELGEOFull)[1])
            ############
            
            
            ColObtained<-getCols(ColourScheme=ColourScheme(),UnitsVec=FullTable$units[SELL],
                              ColorLighteningFactor(),ColorDarkeningFactor())
            
            FullColVec<-ColObtained$FullColVec
            ClickedCols<-ColObtained$ClickedCols
            SELGEOFull$color<-ColObtained$FullColVec
            SELGEOFull$color[SavedVec==1]<-ColObtained$ClickedCols[SavedVec==1]
            ############  
            SELGEOSavedVec<-SELGEOFull[,c("geometry","layerId")]
            SELGEOSwitched<-SELGEOFull[,c("geometry","layerId")]

            SELGEORemaining<-SELGEOFull[(SavedVec==1)|(SwitchedOnCells==1),c("geometry","layerId","color")]
            
            
            SELGEOSavedVec<-SELGEOSavedVec[SavedVec==1,]#;gpNamesSavedVec<-gpNamesSavedVec[SavedVec]
            SELGEOSwitched<-SELGEOSwitched[(SwitchedOnCells==1)&(SavedVec!=1),]#;gpNamesSwitched<-gpNamesSwitched[SwitchedOnCells&(!SavedVec)]
            
            
            if(dim(SELGEORemaining)[1]>0){listMaps[[aai]]<-addPolygons(listMaps[[aai]],data=SELGEORemaining,color=SELGEORemaining$color,layerId=SELGEORemaining$layerId)}
            
        
            }
          
                                     
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- SPECIES_ENGLISH[i]
            selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
            selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
            addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", round(2 * selectedBioSDspecie, 2), "<br>")
          }
          
          listMaps[[aai]] <- listMaps[[aai]]%>%
            addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>",
                                     # "Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>",
                                     addControlText,
                                     "Area Planted: ",round(SelectedArea,2),"<br>",
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
                           ColorLighteningFactor = ColorLighteningFactor(),
                           ColorDarkeningFactor = ColorDarkeningFactor(),
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
                           ColorLighteningFactor=ColorLighteningFactor(),
                           ColorDarkeningFactor=ColorDarkeningFactor(),
                           SPECIES_ARG3 = SPECIES,
                           N_TARGETS_ARG2 = N_TARGETS)
  })
  
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    SelectedDropdown <- input$inSelect
    SelectedRowsUnits<-FullTable$units[FullTable$extent==SelectedDropdown]
    
    
    #GEOVEC<-st_geometry_type(FullTable$geometry)
    
    if(!is.null(click$id)){
      ChangeDone<-FALSE
      SavedVec<-ClickedVector()
        iii<-1
    
        while((!ChangeDone)&&(iii<=length(SavedVec))){
                            if((click$id == paste0("Square",iii))){
                              SavedVec[SelectedRowsUnits==SelectedRowsUnits[iii]]<-ifelse(SavedVec[iii]==1,0,1);
                              ClickedVector(SavedVec)
                              ChangeDone<-TRUE
                              }
            iii<-iii+1
    }
    }
  })
  
  
  output$map <- renderLeaflet({
  #  shinyjs::hide("tabs")
    
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect#"Ennerdale"#input$inSelect#"Abbeyford"#"Ennerdale"#
    calcBaseMap<-BaseMap2(SelectedDropdown,layerId="main",shconv)
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
      LimitsMat <- tmp$LimitsMat
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
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      
       SubsetMeetTargets<-SelectedSimMat2[(SelectedSimMat2$Carbon>=SelecTargetCarbon)&
                                            # (SelectedSimMat2$redsquirrel>=SelecTargetBio)&
                                            condition&
                                            (SelectedSimMat2$Area>=SelecTargetArea)&
                                            (SelectedSimMat2$Visits>=SelecTargetVisits),]
      
      #SubsetMeetTargets<-SelectedSimMat2[Icalc$NROYTotal,]
      
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
          
          
          SELGEOFull<-FullTable[SELL,]
          SELGEOFull$layerId<-paste0("Square",1:dim(SELGEOFull)[1])
          SELGEO<-FullTable$geometry[SELL]
          ############
          ColObtained<-getCols(ColourScheme=ColourScheme(),UnitsVec=FullTable$units[SELL],
                               ColorLighteningFactor(),ColorDarkeningFactor())
          
          FullColVec<-ColObtained$FullColVec
          ClickedCols<-ColObtained$ClickedCols
          SELGEOFull$color<-ColObtained$FullColVec
          SELGEOFull$color[SavedVec==1]<-ColObtained$ClickedCols[SavedVec==1]
          
          
          ############  
          SELGEOSavedVec<-SELGEOFull[,c("geometry","layerId")]
          SELGEOSwitched<-SELGEOFull[,c("geometry","layerId")]
          
          SELGEOSavedVec<-SELGEOSavedVec[SavedVec==1,]#;gpNamesSavedVec<-gpNamesSavedVec[SavedVec]
          SELGEOSwitched<-SELGEOSwitched[(SwitchedOnCells==1)&(SavedVec!=1),]#;gpNamesSwitched<-gpNamesSwitched[SwitchedOnCells&(!SavedVec)]
          SELGEORemaining<-SELGEOFull[(SavedVec==1)|(SwitchedOnCells==1),c("geometry","layerId","color")]
          
        
          
          if(dim(SELGEORemaining)[1]>0){map<-addPolygons(map,data=SELGEORemaining,color=SELGEORemaining$color,layerId=SELGEORemaining$layerId)}
    
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
                                     "Area Planted: ",round(SelectedArea,2),"<br>",
                                     "Visitors: ",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                     "</p>"), position = "topright")
          
        } else { map<-map %>%
          addControl(html = paste0("<p> Targets Cannot be met</p>"), position = "topright")
        }
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
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main2", shconv)
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
                                              ColourScheme=ColourScheme(),
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES)
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
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main3", shconv)
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
                                              ColourScheme=ColourScheme(),
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES)
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
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main4", shconv)
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
                                              ColourScheme=ColourScheme(),
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES)
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
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main5", shconv)
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
                                              ColourScheme=ColourScheme(),
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor(),
                                              SPECIES_ARG2 = SPECIES)
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
