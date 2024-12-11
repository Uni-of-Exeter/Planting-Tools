CalcProbaMat<-function(IVECloc, LimitsMatloc,Above=rep(TRUE,dim(IVECloc)[2]))
{
  PROBAMATloc <- IVECloc
  for (abc in 1:dim(IVECloc)[2]) {
    if(Above[abc]){
      PROBAMATloc[, abc] <- 1 - ptruncnorm(IVECloc[, abc], a = LimitsMatloc[, abc], b = Inf)}else{
        PROBAMATloc[, abc] <-  ptruncnorm(IVECloc[, abc], a = LimitsMatloc[, abc], b = Inf)
      }
  }
  return(PROBAMATloc)
}

getCols<-function(ColourScheme,UnitsVec,ColorLighteningFactor,ColorDarkeningFactor)
{
  LL<-length(UnitsVec)
  UniqC<-unique(UnitsVec)
  Cols<-rainbow(length(UniqC))
  
  if(ColourScheme=="blue/red"){
    FullColVec<-rep("blue",LL)
    ClickedCols<-rep("red",LL)
    
  }
  if(ColourScheme=="rainbow dark/light"){
    FullColVec<-rep(0,LL)
    for (iii in 1:length(Cols)){
      FullColVec[UnitsVec==UniqC[iii]]<-Cols[iii]
    }
    ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
    FullColVec<-darken(FullColVec,ColorDarkeningFactor)
  }
  
  if(ColourScheme=="rainbow dark/red"){
    
    FullColVec<-rep(0,LL)
    for (iii in 1:length(Cols)){
      FullColVec[UnitsVec==UniqC[iii]]<-Cols[iii]
    }
    ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
    FullColVec<-darken(FullColVec,ColorDarkeningFactor)
    ClickedCols<-rep("red",LL)
    
  }
  
  if(ColourScheme=="Terrain darkened/lightened"){
    Cols<-terrain.colors( length(UniqC))
    FullColVec<-rep(0,LL)
    for (iii in 1:length(Cols)){
      FullColVec[UnitsVec==UniqC[iii]]<-Cols[iii]
    }
    ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
    FullColVec<-darken(FullColVec,ColorDarkeningFactor)
    
    
  }
  if(ColourScheme=="Terrain darkened/red"){
    Cols<-terrain.colors( length(UniqC))
    FullColVec<-rep(0,LL)
    for (iii in 1:length(Cols)){
      FullColVec[UnitsVec==UniqC[iii]]<-Cols[iii]
    }
    ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
    FullColVec<-darken(FullColVec,ColorDarkeningFactor)
    ClickedCols<-rep("red",LL)
    
    
  }
  
  if(ColourScheme=="Viridis darkened/red"){
    Cols<-viridis( length(UniqC))
    FullColVec<-rep(0,LL)
    for (iii in 1:length(Cols)){
      FullColVec[UnitsVec==UniqC[iii]]<-Cols[iii]
    }
    ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
    FullColVec<-darken(FullColVec,ColorDarkeningFactor)
    ClickedCols<-rep("red",LL)
    
    
  }
  
  
  return(list(FullColVec=FullColVec,ClickedCols=ClickedCols))
  
}

normalize <- function(x) {
  return(scale(x, center=TRUE, scale=TRUE)[,1])
}

# Implausibility
Impl <- function(Target,EY,SDY,alpha,tol) {
  # Avoid division by 0
  if (0 %in% (SDY^2+tol)) {
    idx <- which(SDY^2+tol == 0)
    tol[idx] <- 0.1
  }
  
  Im<-(Target-EY)/sqrt(SDY^2+tol)
  # Not Ruled-Out space Yet
  NROY<-(Im<=sqrt((1-alpha)/alpha))
  return(list(Im=Im,NROY=NROY))
}

MultiImpl <- function(TargetsVec,EYMat,SDYMat,alpha,tolVec) {
  # Avoid tolVec values equal to 0
  if (0 %in% tolVec) {
    idx = which(tolVec == 0)
    tolVec[idx] <- 0.1
  }
  
  IVEC<-matrix(0,dim(EYMat)[1],length(TargetsVec))
  for(ii in 1:length(TargetsVec))
  {
    IVEC[,ii]<-Impl(TargetsVec[ii],EYMat[,ii],SDYMat[,ii],alpha,tolVec[ii])$Im
  }
  ItotalMax<-(apply(IVEC,1,max))
  ItotalMin<-(apply(IVEC,1,min))
  NROYTotal<-(ItotalMax<=sqrt((1-alpha)/alpha))
  return(list(ImTotMax=ItotalMax,ImTotMin=ItotalMin,NROYTotal=NROYTotal,IVEC=IVEC))
}

#BaseMap <- function(SelectedMap,layerId=NULL,shconv,GreyPolygonWidth) {
#  
#  #ListMaps<-shconv[shconv$extent==SelectedMap,]$shape[[1]]
#  ListMaps<-shconv$geometry[shconv$extent==SelectedMap]
#  
#  max_x2<-(-Inf);min_x2<-(Inf);max_y2<-(-Inf);min_y2<-Inf;
#  
#  if(st_geometry_type(shconv)[1]=="POLYGON"){
#  
#  for(ii in 1: length(ListMaps)){
#    for(jj in 1: length(ListMaps[[ii]])){
#   
#      xvec<-ListMaps[[ii]][[jj]][,1]
#      yvec<-ListMaps[[ii]][[jj]][,2]
#      xvec<-xvec[!is.na(xvec)]
#      yvec<-yvec[!is.na(yvec)]
#      max_x2<-max(max_x2,xvec)
#      min_x2<-min(min_x2,xvec)
#      
#      max_y2<-max(max_y2,yvec)
#      min_y2<-min(min_y2,yvec)
#      }
#    }
#  
#    map<-leaflet() 
#    map<-  addTiles(map) 
#    map<-fitBounds(map,lng1 = min_x2, lat1 = min_y2, 
#                   lng2 = max_x2, lat2 =max_y2) #%>%
#    for(ii in 1: length(ListMaps)){
#      for(jj in 1: length(ListMaps[[ii]])){
##          map<-addPolygons(map,lng=ListMaps[[ii]][[jj]][,1],lat=ListMaps[[ii]][[jj]][,2],color="grey",layerId==paste0(layerId,ii,"_",jj,width=GreyPolygonWidth))}}
#    
#  
#  }else{
#    for(ii in 1: length(ListMaps)){
#      for(jj in 1: length(ListMaps[[ii]])){
#        for(kk in 1: length(ListMaps[[ii]][[jj]])){
#          xvec<-ListMaps[[ii]][[jj]][[kk]][,1]
#          yvec<-ListMaps[[ii]][[jj]][[kk]][,2]
#          xvec<-xvec[!is.na(xvec)]
#          yvec<-yvec[!is.na(yvec)]
#          max_x2<-max(max_x2,xvec)
#          min_x2<-min(min_x2,xvec)
#          
#          max_y2<-max(max_y2,yvec)
#          min_y2<-min(min_y2,yvec)
#        }
#      }
#    }
#    map<-leaflet() 
#    map<-  addTiles(map) 
#    map<-fitBounds(map,lng1 = min_x2, lat1 = min_y2, 
#                   lng2 = max_x2, lat2 =max_y2) #%>%
#    for(ii in 1: length(ListMaps)){
#      for(jj in 1: length(ListMaps[[ii]])){
#        for(kk in 1: length(ListMaps[[ii]][[jj]])){
#          map<-addPolygons(map,lng=ListMaps[[ii]][[jj]][[kk]][,1],lat=ListMaps[[ii]][[jj]][[kk]][,2],color="grey",layerId==paste0(layerId,ii,"_",jj,"_",kk,width=GreyPolygonWidth))}}}
#    
#    
#  }  
#    
#  return(list(map=map,max_x2=max_x2,min_x2=min_x2,max_y2=max_y2,min_y2=min_y2))
#}

get_name_conversion <- function() {
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
  
  return(NAME_CONVERSION)
}

BaseMap2<-function(SelectedMap,layerId=NULL,shconv,GreyPolygonWidth)
{
  
  #ListMaps<-shconv[shconv$extent==SelectedMap,]$shape[[1]]
  ListMaps<-shconv$geometry[shconv$extent==SelectedMap]
  
  max_x2<-(-Inf);min_x2<-(Inf);max_y2<-(-Inf);min_y2<-Inf;
  
  if(st_geometry_type(shconv)[1]=="POLYGON"){
    
    for(ii in 1: length(ListMaps)){
      for(jj in 1: length(ListMaps[[ii]])){
        
        xvec<-ListMaps[[ii]][[jj]][,1]
        yvec<-ListMaps[[ii]][[jj]][,2]
        xvec<-xvec[!is.na(xvec)]
        yvec<-yvec[!is.na(yvec)]
        max_x2<-max(max_x2,xvec)
        min_x2<-min(min_x2,xvec)
        
        max_y2<-max(max_y2,yvec)
        min_y2<-min(min_y2,yvec)
      }
    }
    
    map<-leaflet(options = leafletOptions(attributionControl = TRUE))  %>% addTiles(attribution = NULL) %>% htmlwidgets::onRender("function(el, x) {this.attributionControl.setPosition('bottomleft');}")
    map<-  addTiles(map) 
    map<-fitBounds(map,lng1 = min_x2, lat1 = min_y2, 
                   lng2 = max_x2, lat2 =max_y2) #%>%
    
  }else{
    for(ii in 1: length(ListMaps)){
      for(jj in 1: length(ListMaps[[ii]])){
        for(kk in 1: length(ListMaps[[ii]][[jj]])){
          xvec<-ListMaps[[ii]][[jj]][[kk]][,1]
          yvec<-ListMaps[[ii]][[jj]][[kk]][,2]
          xvec<-xvec[!is.na(xvec)]
          yvec<-yvec[!is.na(yvec)]
          max_x2<-max(max_x2,xvec)
          min_x2<-min(min_x2,xvec)
          
          max_y2<-max(max_y2,yvec)
          min_y2<-min(min_y2,yvec)
        }
      }
    }
    map<-leaflet(options = leafletOptions(attributionControl = TRUE))  %>% addTiles(attribution = NULL) %>% htmlwidgets::onRender("function(el, x) {this.attributionControl.setPosition('bottomleft');}")
    map<-  addTiles(map) 
    map<-fitBounds(map,lng1 = min_x2, lat1 = min_y2, 
                   lng2 = max_x2, lat2 =max_y2) 
    
  }  
  map<-addPolygons(map,data=ListMaps,color="grey",weight=GreyPolygonWidth, fillOpacity = 0.5)
  
  return(list(map=map,max_x2=max_x2,min_x2=min_x2,max_y2=max_y2,min_y2=min_y2))
}

# Cells that cannot be planted are in green
map_sell_not_avail <- function(FullTableNotAvail,
                               SelectedDropdown,
                               listMaps = NULL,
                               map = NULL,NotAvailColour="black") {
  if (dim(FullTableNotAvail)[1]>0) { 
    SELLNOTAVAIL <- FullTableNotAvail$extent==SelectedDropdown
  } else {
    SELLNOTAVAIL <- NULL
  }
  if (sum(SELLNOTAVAIL) > 0) {
    SELGEO <- FullTableNotAvail$geometry[SELLNOTAVAIL]
    for (iii in 1:length(SELGEO)) {
      if (is.null(map) && !is.null(listMaps)) {
        listMaps[[1]]<-addPolygons(listMaps[[1]],data=SELGEO,layerId=paste0("NotAvail"),color=NotAvailColour,fillColor=NotAvailColour,weight=1)
        listMaps[[2]]<-addPolygons(listMaps[[2]],data=SELGEO,layerId=paste0("NotAvail"),color=NotAvailColour,fillColor=NotAvailColour,weight=1)
        
      } else if (!is.null(map) && is.null(listMaps)) {
        map<-addPolygons(map,data=SELGEO,layerId=paste0("NotAvail"),color=NotAvailColour,fillColor=NotAvailColour,weight=1)
      }
    }
  }
  if (is.null(map) && !is.null(listMaps)) {
    return(listMaps)
  } else if (!is.null(map) && is.null(listMaps)) {
    return(map)
  }
  
}

pick_two_strategies_that_meet_targets_update_pref_reactive <- function(VecNbMet0,
                                                                       SelectedSimMat2,
                                                                       pref_reactive,
                                                                       N_TARGETS_ARG3,
                                                                       TARGETS_ARG2,
                                                                       prior_list,
                                                                       limit_log_level = LOG_LEVEL) {
  N_TARGETS <- N_TARGETS_ARG3
  TARGETS <- TARGETS_ARG2
  indices_strategies_meet_all_targets <- which(VecNbMet0() == N_TARGETS)
  # If there are none, pick 2 random strategies
  if (length(indices_strategies_meet_all_targets) == 0) {
    indices_strategies_meet_all_targets <- sample(1:nrow(SelectedSimMat2), 2)
  }
  
  two_strategies_that_meet_all_targets <- sample(indices_strategies_meet_all_targets, 2)
  if (isFALSE(is.null(pref_reactive()))) {
    
    # If we have already added strategies
    i <- 0
    continue_loop <- TRUE
    while (i < 10 && continue_loop) {
      i <- i + 1
      
      # Convert to strings in order to compare rows easily
      current_rows_as_strings <- apply(pref_reactive()$data, 1, toString)
      
      temp <- SelectedSimMat2[two_strategies_that_meet_all_targets, TARGETS]
      rownames(temp) <- NULL
      new_rows_as_strings <- apply(temp, 1, toString)
      
      indices_duplicates_in_new_rows <- which(new_rows_as_strings %in% current_rows_as_strings)
      if (length(indices_duplicates_in_new_rows) > 0) {
        # If there are duplicates, try 2 new ones
        two_strategies_that_meet_all_targets[indices_duplicates_in_new_rows] <- sample(indices_strategies_meet_all_targets, length(indices_duplicates_in_new_rows))
      } else {
        # No duplicates, so we end the loop
        continue_loop <- FALSE
      }
      
    }
    # We couldn't find enough target-compatible strategies to avoid preference strategy duplication
    if (i == 10) {
      notif("We couldn't find enough target-compatible strategies to avoid preference strategy duplication", log_level = "warning", limit_log_level = limit_log_level)
    }
    
    # temp is SelectedSimMat2[two_strategies_that_meet_all_targets, TARGETS]
    pref_reactive()$data_augment(temp)
    rm(temp)
    
  } else {
    
    # If we are adding strategies for the first time
    # Rownames might cause issues, so we remove them
    temp <- SelectedSimMat2[two_strategies_that_meet_all_targets, TARGETS]
    rownames(temp) <- NULL
    pref_reactive(prefObject(data = temp,
                             priors = prior_list))
    rm(temp)
    
  }
  
  return(two_strategies_that_meet_all_targets)
}

observe_event_function <- function(choose = 1, # 1 for input$choose1, 2 for input$choose2
                                   input,
                                   output,
                                   session,
                                   infpref_reactive,
                                   ConvertSample,
                                   LinesToCompareReactive,
                                   ClickedVector,
                                   # AreaSelected0,
                                   # CarbonSelected0,
                                   # # RedSquirrelSelected0,
                                   # SpeciesListSelected0, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected0, ...)
                                   # VisitsSelected0,
                                   # CarbonSelectedSD0,
                                   # # RedSquirrelSelectedSD0,
                                   # SpeciesListSelectedSD0, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD0, ...)
                                   # VisitsSelectedSD0,
                                   # DatBinaryCode0,
                                   NbRoundsMax,
                                   CurrentRound,
                                   FullTable,
                                   FullTableNotAvail,
                                   VecNbMet0,
                                   shconv,
                                   SelectedSimMatGlobal,
                                   pref_reactive,
                                   ColourScheme,
                                   ColorLighteningFactor,
                                   ColorDarkeningFactor,
                                   SPECIES_ARG3,
                                   SPECIES_ENGLISH_ARG3,
                                   N_TARGETS_ARG2,
                                   TARGETS_ARG1,
                                   GreyPolygonWidth,
                                   UnitPolygonColours,
                                   ClickedVectorYear) {
  SPECIES <- SPECIES_ARG3
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG3
  N_TARGETS <- N_TARGETS_ARG2
  TARGETS <- TARGETS_ARG1
  SavedVec <- ClickedVector()
  SavedVecYear <- ClickedVectorYear()
  LinesToCompare <- as.matrix(LinesToCompareReactive())
  SelectedDropdown <- input$inSelect
  
  shinyjs::disable("choose1")
  shinyjs::disable("choose2")
  
  if((!is.null(SavedVec))&(CurrentRound()>0)){
    calcBaseMap <- BaseMap2(SelectedDropdown,layerId="main100",shconv=shconv,GreyPolygonWidth=GreyPolygonWidth)
    
    SelectedSimMat2 <- SelectedSimMatGlobal
    if (dim(LinesToCompare)[1]>CurrentRound())#NbRoundsMax()
    {
      CR <- CurrentRound()
      length_pref_reactive_data <- nrow(pref_reactive()$data)
      if (choose == 1) {
        # pref$addPref(prefeR::`%>%`(LinesToCompare[CR,1],LinesToCompare[CR,2]))
        pref_reactive()$addPref(c(length_pref_reactive_data - 1, length_pref_reactive_data))
      } else if (choose == 2) {
        # pref$addPref(prefeR::`%>%`(LinesToCompare[CR,2],LinesToCompare[CR,1]))
        pref_reactive()$addPref(c(length_pref_reactive_data, length_pref_reactive_data - 1))
      }
      # if(CR<dim(LinesToCompare)[1]){
      #   LinesToCompare[CR+1,] <- prefeR::suggest(pref,maxComparisons = 5)
      # }
      # LinesToCompareReactive(LinesToCompare)
      
      CR <- CR+1
      CurrentRound(CR)
      
      listMaps <- list()
      listMaps[[1]] <- calcBaseMap$map
      listMaps[[2]] <- calcBaseMap$map
      
      SelectedLine <- list()
      # Re-pick 2 random stragies like in app.R line 1687
      # SelectedLine[[1]] <- SelectedSimMat2[ConvertSample[LinesToCompare[CR,1]],]
      # SelectedLine[[2]] <- SelectedSimMat2[ConvertSample[LinesToCompare[CR,2]],]
      two_strategies_that_meet_all_targets <- pick_two_strategies_that_meet_targets_update_pref_reactive(VecNbMet0 = VecNbMet0,
                                                                                                         SelectedSimMat2 = SelectedSimMat2,
                                                                                                         pref_reactive = pref_reactive,
                                                                                                         N_TARGETS_ARG3 = N_TARGETS,
                                                                                                         TARGETS_ARG2 = TARGETS,
                                                                                                         prior_list = NULL)
      SelectedLine[[1]] <- SelectedSimMat2[two_strategies_that_meet_all_targets[1], ]
      SelectedLine[[2]] <- SelectedSimMat2[two_strategies_that_meet_all_targets[2], ]
      
      for(aai in 1:2){
        SwitchedOnCells <- SelectedLine[[aai]][1:length(SavedVecYear)]
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
        
        SELL <- (FullTable$extent==SelectedDropdown)
        if(!is.null(SELL)){
          SELGEO<-FullTable$geometry[SELL]
          
          UnitsSel<-unique(FullTable$units[SELL])
          Cols<-rainbow(length(UnitsSel))
          FullColVec<-rep(0,dim(FullTable[SELL,])[1])
          for (iii in 1:length(Cols)){
            FullColVec[FullTable$units[SELL]==UnitsSel[iii]]<-Cols[iii]
          }
          
          ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
          FullColVec<-darken(FullColVec,ColorDarkeningFactor)
          ClickedCols<-rep("red",length(ClickedCols))
          
          ColObtained<-getCols(ColourScheme,UnitsVec=FullTable$units[SELL],
                               ColorLighteningFactor,ColorDarkeningFactor)
          
          FullColVec<-ColObtained$FullColVec#darken(FullColVec,ColorDarkeningFactor)
          ClickedCols<-ColObtained$ClickedCols
          
          #  sellng <- FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
          #  sellat <- FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
          for (iii in 1:length(SwitchedOnCells)){
            if(SavedVec[iii]==1){
              
              if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
                listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng= as.numeric(SELGEO[[iii]][[1]][,1]),lat= as.numeric(SELGEO[[iii]][[1]][,2]),layerId =paste0("Square",iii),color =ClickedCols[iii],weight=UnitPolygonColours)
              }else{
                for(kk in 1:length(SELGEO[[iii]])) {
                  listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng= as.numeric(SELGEO[[iii]][[kk]][[1]][,1]),lat= as.numeric(SELGEO[[iii]][[kk]][[1]][,2]),layerId =paste0("Square",iii,"_",kk),color =ClickedCols[iii],weight=UnitPolygonColours)
                }
                
              }
              
            }
            else{
              if(SwitchedOnCells[iii]==1){
                if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
                  listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng=  as.numeric(SELGEO[[iii]][[1]][,1]),lat=  as.numeric(SELGEO[[iii]][[1]][,2]),layerId =paste0("Square",iii),color=FullColVec[iii],weight=UnitPolygonColours)
                }else{
                  for(kk in 1:length(SELGEO[[iii]])) {
                    listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng=  as.numeric(SELGEO[[iii]][[kk]][[1]][,1]),lat=  as.numeric(SELGEO[[iii]][[kk]][[1]][,2]),layerId =paste0("Square",iii),color=FullColVec[iii],weight=UnitPolygonColours)
                    
                  }
                  
                }
              }
            }
          }
        }
        addControlText <- ""
        for (i in 1:length(SPECIES)) {
          specie_latin <- SPECIES[i]
          specie_english <- SPECIES_ENGLISH[i]
          selectedBiospecie <- get(paste0("SelectedBio", specie_latin))
          selectedBioSDspecie <- get(paste0("SelectedBioSD", specie_latin))
          addControlText <- paste0(addControlText, specie_english, ": ", round(selectedBiospecie, 2), "\u00B1", sprintf("%.2f", 2 * selectedBioSDspecie), "<br>")
        }
        listMaps[[aai]] <- listMaps[[aai]]%>%  
          addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",sprintf("%.2f",2*SelectedTreeCarbonSD),"<br>",
                                   # "Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>",
                                   addControlText,
                                   "Area Planted: ",round(SelectedArea,2),"<br>",
                                   "Visitors: ",round(SelectedVisits,2),"\u00B1",sprintf("%.2f",2*SelectedVisitsSD),
                                   "</p>"), position = "topright")
      }
      listMaps <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail,
                                     SelectedDropdown = SelectedDropdown,
                                     listMaps = listMaps)
      
      output$ClusterPage <- renderLeaflet({listMaps[[1]]})
      output$ClusterPage2 <- renderLeaflet({listMaps[[2]]})  
      shinyjs::enable("choose1")
      shinyjs::enable("choose2")
      
    } else {
      CR <- CurrentRound()
      # pref$addPref(prefeR::`%>%`(LinesToCompare[CR,1],LinesToCompare[CR,2]))
      length_pref_reactive_data <- nrow(pref_reactive()$data)
      pref_reactive()$addPref(c(length_pref_reactive_data - 1, length_pref_reactive_data))
      
      
      shinyjs::disable("choose1")
      shinyjs::disable("choose2")
      
      # temp <- pref$infer()
      pref_reactive()$update()
      temp <- pref_reactive()$posterior_mean
      infpref_reactive(temp)
      
      # SelectedSimMat2 <- SelectedSimMatGlobal
      # VecNbMet <- VecNbMet0()
      # 
      # # columns <- c("Carbon","redsquirrel","Area","Visits")
      # SelectedSimMat2columns <- c("Carbon", SPECIES, "Area","Visits")
      # ClusteringDat <- data.frame(sqrt(infpref_reactive())*SelectedSimMat2[,SelectedSimMat2columns],NbTargetsMet=VecNbMet)
      # ClusteringDat <- ClusteringDat[ClusteringDat$NbTargetsMet>0,]
      # ClusteringDat <- unique(ClusteringDat)
      # set.seed(123)
      # 
      # FailedTsne <- TRUE
      # PerpVec <- c(10,20,30,5,2,1,0.1,40,50,60,70,80,100)
      # IndexPerp <- 1
      # 
      # while((FailedTsne)&(IndexPerp<=length(PerpVec))){
      #   Perp <- PerpVec[IndexPerp]
      #   tsRes <-try(Rtsne(ClusteringDat, perplexity = Perp))
      #   IndexPerp <- IndexPerp+1       
      #   if(class(tsRes)[1]!="try-error"){FailedTsne <- FALSE}
      # }
      # 
      # if(FailedTsne){
      #   
      #   pp <- ggplot() +theme_void() +
      #     annotate("text", x = 0.5, y = 0.5, label = "Clustering Failed",
      #              size = 10, color = "black", hjust = 0.5, vjust = 0.5)
      #   output$plotOP1 <- renderPlot({pp})
      #   updateCheckboxInput(session,"Trigger", label = "", value = FALSE)
      #   
      # }else{
      #   
      #   tsneclusters <- Mclust(tsRes$Y, 1:N_TARGETS)
      #   ClusterPlot <- mutate(ClusteringDat, cluster=as.factor(tsneclusters$classification)) %>%
      #     ggpairs(columns=1:N_TARGETS, aes(color=cluster),upper=list(continuous="points"))
      #   output$plotOP1 <- renderPlot({ClusterPlot})
      #   
      #   
      #   #pp< <- ggplot(data=data.frame(x=tsRes$Y[,1],y=tsRes$Y[,2]),aes(x,y))+
      #   #  geom_point(aes(colour =factor(ClusteringDat$NbTargetsMet)))+
      #   #  labs(x="dim1",y="dim2",color = "Number of Targets Met")+theme_minimal()
      #   #output$plotOP1 <- renderPlot({pp})
      #   updateCheckboxInput(session,"Trigger", label = "", value = FALSE)
      # }
      
    }  }
}

observe_event_function_YearType <- function(choose = 1, # 1 for input$choose1, 2 for input$choose2
                                   input,
                                   output,
                                   session,
                                   infpref_reactive,
                                   ConvertSample,
                                   LinesToCompareReactive,
                                   ClickedVector,
                                   NbRoundsMax,
                                   CurrentRound,
                                   FullTable,
                                   FullTableNotAvail,
                                   VecNbMet0,
                                   shconv,
                                   SelectedSimMatGlobal,
                                   pref_reactive,
                                   ColourScheme,
                                   ColorLighteningFactor,
                                   ColorDarkeningFactor,
                                   SPECIES_ARG3,
                                   SPECIES_ENGLISH_ARG3,
                                   N_TARGETS_ARG2,
                                   TARGETS_ARG1,
                                   GreyPolygonWidth,
                                   UnitPolygonColours,
                                   ClickedVectorYear,
                                   ClickedVectorYearType) {

  SPECIES <- SPECIES_ARG3
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG3
  N_TARGETS <- N_TARGETS_ARG2
  TARGETS <- TARGETS_ARG1
  SavedVec <- ClickedVector()
  SavedVecYear <- ClickedVectorYear()
  SavedVecYearType <- ClickedVectorYearType()
  YearSelect<-input$YearPref-STARTYEAR

  LinesToCompare <- as.matrix(LinesToCompareReactive())
  SelectedDropdown <- input$inSelect
  
  shinyjs::disable("choose1")
  shinyjs::disable("choose2")
  #browser()
  if(!is.null(SavedVec)){
    calcBaseMap <- BaseMap2(SelectedDropdown,layerId="main100",shconv=shconv,GreyPolygonWidth=GreyPolygonWidth)
    
    SelectedSimMat2 <- SelectedSimMatGlobal
   # if (dim(LinesToCompare)[1]>CurrentRound())#NbRoundsMax()
  #  {
      CR <- CurrentRound()
   
      length_pref_reactive_data <- nrow(pref_reactive()$data)
      if (choose == 1) {
        pref_reactive()$addPref(c(length_pref_reactive_data - 1, length_pref_reactive_data))
      } else if (choose == 2) {
        pref_reactive()$addPref(c(length_pref_reactive_data, length_pref_reactive_data - 1))
      }
     # browser()

      
      CR <- CR+1
      CurrentRound(CR)
      
      listMaps <- list()
      listMaps[[1]] <- calcBaseMap$map
      listMaps[[2]] <- calcBaseMap$map
      
      SelectedLine <- list()
      
      LinesToCompare<-LinesToCompareReactive()
      LinesToCompareYear_Type_Concatenation<-cbind(apply(as.matrix(LinesToCompare$YEAR),c(1,2),as.character),
                                                   as.matrix(LinesToCompare$TYPE))
      NumberNewStrategiesFound<-0
      
      
      if(dim(FIXED_STRATEGIES_LIST$YEAR)[1]!=0){
        FIXED_STRATEGIES_LIST_Year_Type_Concatenation<-data.frame(YEAR=FIXED_STRATEGIES_LIST$YEAR,
                                                                  TYPE=FIXED_STRATEGIES_LIST$TYPE)
        lenghtFixedStrategies<-dim(FIXED_STRATEGIES_LIST$YEAR)[1]
        CounterLoc<-1
        while((CounterLoc<=lenghtFixedStrategies)&(NumberNewStrategiesFound<2))
        {# Test if the line of the matrix is in the list of strategies already selected
         if(sum((apply(t(matrix(as.character(FIXED_STRATEGIES_LIST_Year_Type_Concatenation[CounterLoc,])
                                           ,dim(LinesToCompareYear_Type_Concatenation)[2],
                                           dim(LinesToCompareYear_Type_Concatenation)[1]))==LinesToCompareYear_Type_Concatenation,1,prod)==1))==0
                       )
         {LinesToCompare$YEAR<-rbind(LinesToCompare$YEAR,FIXED_STRATEGIES_LIST$YEAR[CounterLoc,])
         LinesToCompare$TYPE<-rbind(LinesToCompare$YEAR,FIXED_STRATEGIES_LIST$TYPE[CounterLoc,])
         LinesToCompare$OUTPUTS<-rbind(LinesToCompare$YEAR,FIXED_STRATEGIES_LIST$OUTPUTS[CounterLoc,])
         NumberNewStrategiesFound<-NumberNewStrategiesFound+1
         LinesToCompareYear_Type_Concatenation<-data.frame(YEAR=LinesToCompare$YEAR,TYPE=LinesToCompare$TYPE)
         
         
         } 
          CounterLoc<-CounterLoc+1
        }
      }
        #if 2 strategies were not found in the list of fixed strategies, we try to find them in the target compatible space
        SubsetMeetTargetsUnique<-SubsetMeetTargetsReactiveUnique()
     # browser()
        if((dim(SubsetMeetTargetsUnique$YEAR)[1]!=0)&(NumberNewStrategiesFound<2)){
          SubsetMeetTargetsUniqueYear_Type_Concatenation<-data.frame(YEAR=SubsetMeetTargetsUnique$YEAR,TYPE=SubsetMeetTargetsUnique$TYPE)
          
          lenghtStrategiesMeetTargets<-dim(SubsetMeetTargetsUnique$YEAR)[1]
          IndexRandomizeOrder<-sample(1:lenghtStrategiesMeetTargets,lenghtStrategiesMeetTargets,replace=F)
          CounterLoc<-1
          while((CounterLoc<=lenghtStrategiesMeetTargets)&(NumberNewStrategiesFound<2))
          {# Test if the line of the matrix is in the list of strategies that are target compatible
  
            CurrentStrategy<-as.character(SubsetMeetTargetsUniqueYear_Type_Concatenation[IndexRandomizeOrder[CounterLoc],])
            if(sum((apply(t(matrix(CurrentStrategy
                                   ,dim(LinesToCompareYear_Type_Concatenation)[2],
                                   dim(LinesToCompareYear_Type_Concatenation)[1]))==LinesToCompareYear_Type_Concatenation,1,prod)==1))==0
            )
            {LinesToCompare$YEAR<-rbind(LinesToCompare$YEAR,SubsetMeetTargetsUnique$YEAR[IndexRandomizeOrder[CounterLoc],])
            LinesToCompare$TYPE<-rbind(LinesToCompare$TYPE,SubsetMeetTargetsUnique$TYPE[IndexRandomizeOrder[CounterLoc],])
            LinesToCompare$OUTPUTS<-rbind(LinesToCompare$OUTPUTS,SubsetMeetTargetsUnique$OUTPUTS[IndexRandomizeOrder[CounterLoc],])
            
            NumberNewStrategiesFound<-NumberNewStrategiesFound+1
            LinesToCompareYear_Type_Concatenation<-cbind(apply(as.matrix(LinesToCompare$YEAR),c(1,2),as.character),
                                                         as.matrix(LinesToCompare$TYPE))
            } 
            CounterLoc<-CounterLoc+1
          }
          
        }
        
        
        #Otherwise we pick in the precalculated examples (target compatible or not)
        
        if(NumberNewStrategiesFound<2){
          SelectedSimMat2_YearType_Concatenation<-SelectedSimMatGlobal_YearType_Concatenation#data.frame(YEAR=apply(as.matrix(SelectedSimMat2$YEAR),c(1,2),as.character),
                                                             #TYPE=as.matrix(SelectedSimMat2$TYPE))
          
          lenghtSimulMat2<-dim(SelectedSimMat2_YearType_Concatenation)[1]
          IndexRandomizeOrder<-sample(1:lenghtSimulMat2,lenghtSimulMat2,replace=F)
          CounterLoc<-1
          while((CounterLoc<=lenghtSimulMat2)&(NumberNewStrategiesFound<2))
          {# Test if the line of the matrix is in the list of strategies that are target compatible
            CurrentStrategy<-as.character(SelectedSimMat2_YearType_Concatenation[IndexRandomizeOrder[CounterLoc],])
            if(sum((apply(t(matrix(as.character(CurrentStrategy)
                                   ,dim(LinesToCompareYear_Type_Concatenation)[2],
                                   dim(LinesToCompareYear_Type_Concatenation)[1]))==LinesToCompareYear_Type_Concatenation,1,prod)==1))==0
            )
            {LinesToCompare$YEAR<-rbind(LinesToCompare$YEAR,SelectedSimMat2$YEAR[IndexRandomizeOrder[CounterLoc],])
            LinesToCompare$TYPE<-rbind(LinesToCompare$TYPE,SelectedSimMat2$TYPE[IndexRandomizeOrder[CounterLoc],])
            LinesToCompare$OUTPUTS<-rbind(LinesToCompare$OUTPUTS,SelectedSimMat2$OUTPUTS[IndexRandomizeOrder[CounterLoc],])
            
            NumberNewStrategiesFound<-NumberNewStrategiesFound+1
            LinesToCompareYear_Type_Concatenation<-cbind(apply(as.matrix(LinesToCompare$YEAR),c(1,2),as.character),
                                                         as.matrix(LinesToCompare$TYPE))
            
            } 
            CounterLoc<-CounterLoc+1
          }
          
        }
        
        
      #else{if(dim(SubsetMeetTargetsUnique$YEAR)[1]>=2){
      #  
      #  RandomSubsetIndices<-sample(1:dim(SubsetMeetTargetsUnique$YEAR)[1],2,replace=F)
      #  
      #  LinesToCompare<-list(YEAR=SubsetMeetTargetsUnique$YEAR[RandomSubsetIndices,],
      #                       TYPE=SubsetMeetTargetsUnique$TYPE[RandomSubsetIndices,],
      #                       OUTPUTS=SubsetMeetTargetsUnique$OUTPUTS[RandomSubsetIndices,])
      #}else{
      #  RandomSubsetIndices<-sample(1:dim(SelectedSimMatGlobal$YEAR)[1],2,replace=F)
      #  LinesToCompare<-list(YEAR=SelectedSimMatGlobal$YEAR[RandomSubsetIndices,],
      #                       TYPE=SelectedSimMatGlobal$TYPE[RandomSubsetIndices,],
      #                       OUTPUTS=SelectedSimMatGlobal$OUTPUTS[RandomSubsetIndices,])
      #}
        
      #}
      
      
        
      
      
      #RandomSubsetIndices<-sample(1:dim(SubsetMeetTargetsUnique$YEAR)[1],length(ConvertSample),replace=F)
      #datAll2<-list(YEAR=SubsetMeetTargetsUnique$YEAR[RandomSubsetIndices,],
       #             TYPE=SubsetMeetTargetsUnique$TYPE[RandomSubsetIndices,],
        #            OUTPUTS=SubsetMeetTargetsUnique$OUTPUTS[RandomSubsetIndices,])
      
      #LinesToCompare[CR, ] <- sample(1:dim(datAll2$YEAR)[1], 2, replace = F)
     
      LinesToCompareReactive(LinesToCompare)
      
      CurrentLengthLinesToCompare<-dim(LinesToCompare$YEAR)[1]
      
      SelectedLine[[1]] <- list(YEAR=LinesToCompare$YEAR[ CurrentLengthLinesToCompare-1,],
                                TYPE=LinesToCompare$TYPE[ CurrentLengthLinesToCompare-1,],
                                OUTPUTS=LinesToCompare$OUTPUTS[ CurrentLengthLinesToCompare-1,])#SelectedSimMat2[two_strategies_that_meet_all_targets[1], ]
      SelectedLine[[2]] <- list(YEAR=LinesToCompare$YEAR[CurrentLengthLinesToCompare,],
                                TYPE=LinesToCompare$TYPE[CurrentLengthLinesToCompare,],
                                OUTPUTS=LinesToCompare$OUTPUTS[CurrentLengthLinesToCompare,])#SelectedSimMat2[two_strategies_that_meet_all_targets[2], ]
      
      pref_reactive()$data_augment(rbind(SelectedLine[[1]]$OUTPUTS[TARGETS],SelectedLine[[2]]$OUTPUTS[TARGETS]))
      #browser()
      for (aai in 1:2) {
          
          TypeA<-(SelectedLine[[aai]]$TYPE=="Conifers")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
          TypeB<-(SelectedLine[[aai]]$TYPE=="Deciduous")&(SelectedLine[[aai]]$YEAR<=YearSelect)&(SavedVecYearType<YearSelect)
          BlockedCells<-(SavedVecYearType>=YearSelect)
          mapp<-listMaps[[aai]]
          removeShape(mapp,layerId=paste0("Square",1:length(TypeA)))
          COLOURS<-rep("transparent",length(TypeA))
          
          COLOURS[TypeA]<-"purple"
          COLOURS[TypeB]<-"green"
          COLOURS[BlockedCells]<-"red"
          mapp<-addPolygons(mapp,data=FullTable$geometry,
                            layerId=paste0("Square",1:length(TypeA)),color=COLOURS,fillColor=COLOURS,weight=1)
          #removeControl(mapp,layerId="legend")
          ############################################################################# TO CHANGE PREF ELICITATION           
          
        
          addControlText <- ""
          for (i in 1:length(SPECIES)) {
            specie_latin <- SPECIES[i]
            specie_english <- if (specie_latin == "All") "All Species Richness" else SPECIES_ENGLISH[i]
            selectedBiospecie <- SelectedLine[[aai]]$OUTPUTS[[specie_latin]]
            selectedBioSDspecie <- SelectedLine[[aai]]$OUTPUTS[[paste0( specie_latin,"SD")]]
            addControlText <- paste0(addControlText, specie_english, ": ", 
                                     round(selectedBiospecie, 2), "\u00B1", sprintf("%.2f", 2 * selectedBioSDspecie), "\n")
          }
        
          
          #mapp<-
          #  addControl(mapp,html = paste0("<p>Carbon: ", round(SelectedLine[[aai]]$OUTPUTS$Carbon, 2), "\u00B1", 
          #                                round(2*SelectedLine[[aai]]$OUTPUTS$CarbonSD, 2), "<br>",
          #                                addControlText,
          #                                "Area Planted: ", round(SelectedLine[[aai]]$OUTPUTS$Area, 4), "<br>",
          #                                "Visitors: ", round(SelectedLine[[aai]]$OUTPUTS$Visits, 2), 
          #                                "\u00B1", round(2*SelectedLine[[aai]]$OUTPUTS$VisitsSD, 2),
          #                                "</p>"), position = "topright",layerId="legend")
          if(aai==1){
            PrefTextA(paste0("Carbon: ", round(SelectedLine[[aai]]$OUTPUTS$Carbon, 2), "\u00B1", 
                             sprintf("%.2f",2*SelectedLine[[aai]]$OUTPUTS$CarbonSD), "\n",
                             addControlText,
                             "Area Planted: ", round(SelectedLine[[aai]]$OUTPUTS$Area, 4), "\n",
                             "Visitors: ", round(SelectedLine[[aai]]$OUTPUTS$Visits, 2), 
                             "\u00B1",sprintf("%.2f",2*SelectedLine[[aai]]$OUTPUTS$VisitsSD) ))}else{
                               PrefTextB(paste0("Carbon: ", round(SelectedLine[[aai]]$OUTPUTS$Carbon, 2), "\u00B1", 
                                                sprintf("%.2f",2*SelectedLine[[aai]]$OUTPUTS$CarbonSD), "\n",
                                                addControlText,
                                                "Area Planted: ", round(SelectedLine[[aai]]$OUTPUTS$Area, 4), "\n",
                                                "Visitors: ", round(SelectedLine[[aai]]$OUTPUTS$Visits, 2), 
                                                "\u00B1", sprintf("%.2f",2*SelectedLine[[aai]]$OUTPUTS$VisitsSD)))
                               
                             }
          
          
          
          listMaps[[aai]]<-mapp
        
    
        }
      #browser()
        length_pref_reactive_data <- nrow(pref_reactive()$data)
        #pref_reactive()$addPref(c(length_pref_reactive_data - 1, length_pref_reactive_data))
        
      
        temp <- pref_reactive()$posterior_mean
        infpref_reactive(temp)
        
      #}
    #else {
    #  CR <- CurrentRound()
      
      listMaps <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail,
                                     SelectedDropdown = SelectedDropdown,
                                     listMaps = listMaps)
      
      output$ClusterPage <- renderLeaflet({listMaps[[1]]})
      output$ClusterPage2 <- renderLeaflet({listMaps[[2]]})  
     
    #  

  } 
  shinyjs::enable("choose1")
  shinyjs::enable("choose2")
  
}

outputmap_calculateMats <- function(input,
                                    SavedVecLoc,
                                    simul636Loc,
                                    AreaSelected,
                                    CarbonSelected,
                                    # RedSquirrelSelected,
                                    SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                    VisitsSelected,
                                    CarbonSelectedSD,
                                    # RedSquirrelSelectedSD,
                                    SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                    VisitsSelectedSD,
                                    alphaLVL = alphaLVL,
                                    input_areaSlider_multiplicative_coefficient = TRUE,
                                    ManualTargets=NULL,
                                    tolvec,
                                    MAXYEAR=MAXYEAR) {
  #browser()
  
  # If only one element in SavedVec, select corresponding column in simul636
  if (length(SavedVecLoc) == 1) {
    SelectedSimMat <- as.matrix(simul636Loc[, 1:length(SavedVecLoc)])
  } else {
    SelectedSimMat <- simul636Loc[, 1:length(SavedVecLoc)]
  }
  
  SVMAT <- t(matrix(SavedVecLoc, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  CarbonMAT <- t(matrix(CarbonSelected, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  # RedSquirrelMAT <- t(matrix(as.numeric(RedSquirrelSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelected)) {
    specie_name <- names(SpeciesListSelected)[i]
    specie_value <- SpeciesListSelected[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "Selected"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  AreaMAT <- t(matrix(AreaSelected, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected), length(SavedVecLoc), dim(SelectedSimMat)[1]))
  
  CarbonSDMAT <- t(matrix(CarbonSelectedSD, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  # RedSquirrelSDMAT <- t(matrix(as.numeric(RedSquirrelSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelectedSD)) {
    specie_name <- names(SpeciesListSelectedSD)[i]
    specie_value <- SpeciesListSelectedSD[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "SelectedSD"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVecLoc), dim(SelectedSimMat)[1]))
  
  # Create a data frame representing the selected similarity matrix
  SelectedSimMat <- data.frame(1 * (SelectedSimMat | SVMAT))
  
  if (is.null(ManualTargets)){
    SelecTargetCarbon <- input$SliderMain
    # SelecTargetBio <- input$BioSlider
    SelecTargetBioVector <- c()
    SelecTargetBioList <- list()
    for (x in names(SpeciesListSelected)) {
      var_name <- paste0("SelecTargetBio", x)
      # input[paste0("BioSlider", x)] bugs because it is a reactivevalue
      value <- input[[paste0("BioSlider", x)]]
      assign(var_name, value)
      SelecTargetBioVector <- c(SelecTargetBioVector, value)
      SelecTargetBioList[var_name] <- value
    }
    SelecTargetArea <- input$AreaSlider
    SelecTargetVisits <- input$VisitsSlider}else{SelecTargetCarbon<-ManualTargets[[1]];SelecTargetBioVector<-unlist(ManualTargets[[2]]);
    SelecTargetBioList<-ManualTargets[[2]];
    SelecTargetArea<-ManualTargets[[3]];SelecTargetVisits<-ManualTargets[[4]]}
  
  
  speciesMat <- do.call("data.frame", setNames(lapply(names(SpeciesListSelected),
                                                      function(x) bquote(rowMeans(SelectedSimMat * get(paste0(.(x), "MAT"))))),
                                               names(SpeciesListSelected)))
  speciesMatSD <- do.call("data.frame", setNames(lapply(names(SpeciesListSelectedSD),
                                                        function(x) bquote(sqrt(rowSums(SelectedSimMat * (get(paste0(.(x), "MAT"))^2) / length(SavedVecLoc))))),
                                                 names(SpeciesListSelectedSD)))
  SelectedSimMat2 <- data.frame(SelectedSimMat,
                                Carbon = rowSums(SelectedSimMat * CarbonMAT),
                                # redsquirrel = rowMeans(SelectedSimMat * RedSquirrelMAT),
                                speciesMat,
                                Area = rowSums(SelectedSimMat * AreaMAT),
                                Visits = rowMeans(SelectedSimMat * (VisitsMAT)),
                                CarbonSD = sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
                                # redsquirrelSD = sqrt(rowSums(SelectedSimMat * (RedSquirrelSDMAT^2))) / length(SavedVec),
                                speciesMatSD,
                                VisitsSD = sqrt(rowSums(SelectedSimMat * (VisitsSDMAT^2))) / length(SavedVecLoc))
  # for (specie_name in names(SpeciesListSelected)) {
  #   value <- rowMeans(SelectedSimMat * get(paste0(specie_name, "MAT")))
  #   SelectedSimMat2[specie_name] <- value
  # }
  # for (specie_name in names(SpeciesListSelectedSD)) {
  #   value <- sqrt(rowSums(SelectedSimMat * (get(paste0(specie_name, "MAT"))^2) / length(SavedVec)))
  #   SelectedSimMat2[specie_name] <- value
  # }
  
  # tolvec <- c(mean(SelectedSimMat2$Carbon) / 50,
  #            colMeans(speciesMat) / 50,
  #           mean(SelectedSimMat2$Area) / 50,
  #          mean(SelectedSimMat2$Visits) / 50)
  #  for(i in 1:length(tolvec)) {
  #   if (tolvec[i] == 0) {
  #    tolvec[i] <- 0.1
  # }
  #}
  # tolVec <- c(4, 0.05, 0.1, 2)
  Icalc <- MultiImpl(
    # TargetsVec = c(SelecTargetCarbon, SelecTargetBio, SelecTargetArea, SelecTargetVisits),
    TargetsVec = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetArea, SelecTargetVisits),
    # EYMat = data.frame(SelectedSimMat2$Carbon, SelectedSimMat2$redsquirrel, SelectedSimMat2$Area, SelectedSimMat2$Visits),
    EYMat = data.frame(SelectedSimMat2$Carbon, speciesMat, SelectedSimMat2$Area, SelectedSimMat2$Visits),
    # SDYMat = data.frame(SelectedSimMat2$CarbonSD, SelectedSimMat2$redsquirrelSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
    SDYMat = data.frame(SelectedSimMat2$CarbonSD, speciesMatSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
    alpha = alphaLVL, tolVec = tolvec
  )
  
  LimitsMat <- (-data.frame(SelectedSimMat2$Carbon,
                            # SelectedSimMat2$redsquirrel,
                            speciesMat,
                            SelectedSimMat2$Area,
                            # SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + 4^2, SelectedSimMat2$redsquirrelSD^2 + 2^2, rep(0, length(SelectedSimMat2$Area)) + 100^2, SelectedSimMat2$VisitsSD + 2^2))
                            SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + tolvec[1],
                                                                       speciesMatSD^2 + tolvec[1 + (1:ncol(speciesMat))],
                                                                       rep(0, length(SelectedSimMat2$Area)) + tolvec[length(tolvec) - 1],
                                                                       SelectedSimMat2$VisitsSD^2 + tolvec[length(tolvec)]))
  
  return(c(list(SelectedSimMat2 = SelectedSimMat2, Icalc = Icalc, LimitsMat = LimitsMat, SelecTargetCarbon = SelecTargetCarbon,
                # SelecTargetBio = SelecTargetBio, SelecTargetArea = SelecTargetArea, SelecTargetVisits = SelecTargetVisits))
                SelecTargetArea = SelecTargetArea, SelecTargetVisits = SelecTargetVisits),
           SelecTargetBioList))
}

# This is function outputmap_calculateMats adapted to the case where
# HERE CHANGE
outputmap_calculateMatsYear <- function(input,
                                        SavedVecLoc,
                                        simul636YearLoc,
                                        AreaSelected,
                                        CarbonSelected,
                                        CarbonSelectedYear,
                                        # RedSquirrelSelected,
                                        SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                        VisitsSelected,
                                        CarbonSelectedSD,
                                        CarbonSelectedSDYear,
                                        # RedSquirrelSelectedSD,
                                        SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                        VisitsSelectedSD,
                                        alphaLVL = alphaLVL,
                                        input_areaSlider_multiplicative_coefficient = TRUE,
                                        ManualTargets=NULL,
                                        tolvec,
                                        #YearSelect,
                                        PrecalculatedCarbonSelectedTableMean=NULL,
                                        PrecalculatedCarbonSelectedTableSD=NULL,
                                        SavedVecYearLoc,
                                        PreviousSavedVecYearLoc,
                                        SAMPLELIST,
                                        MAXYEAR=MAXYEAR) {
  
  # Because only CO2 has variations with time we create a "binary" SelectedSimMat that is used for 
  # all the other output variables. (MAXYEAR+1) is the code for No Planting
  # SelectedSimMat: matrix of simulations with year
  # SelectedSimMatBinary: convert SelectedSimMat without year of planting (binary)
  # SelectedSimMatYearOrSavedVec: matrix where the column where SavedVec=1 is replaced by 0 (planting at year 0)
  # SVMMAT: Matrix composed on SavedVec on every line (to do an OR later).
  # For the moment, the area does not change with year of planting.
  if (length(SavedVecYearLoc) == 1) {
    SelectedSimMat <- as.matrix(simul636YearLoc[, 1:length(SavedVecYearLoc)])
    SelectedSimMatBinary<- 1*(as.matrix(simul636YearLoc[, 1:length(SavedVecYearLoc)])!=(MAXYEAR+1))
  } else {
    SelectedSimMat <- simul636YearLoc[, 1:length(SavedVecYearLoc)]
    SelectedSimMatBinary <- 1*(simul636YearLoc[, 1:length(SavedVecYearLoc)]!=(MAXYEAR+1))
  }
  
  #Simul636YearOverrideLoc<-Simul636YearOverrideReactive()
  
  #SelectedSimMat[SelectedSimMat>YearSelect]<-(MAXYEAR+1)

  SelectedSimMatYearORSavedVec<-SelectedSimMat
  #We assume that if the land parcel has been clicked by the user, it overrides previous year of planting
  # and forces the whole column of SelectedSimMat to be equal to 0 (plant at year 0)
  # Note that SelectedSimMat is only used for CO2 as for the others, 
  #SAMPLELIST<-list()
  
  for(bcc in 1:dim(SelectedSimMatYearORSavedVec)[2])
  {#if(SavedVecYearLoc[bcc]<(MAXYEAR+1)){
    
    #  
    # if(SavedVecYearLoc[bcc]<28){
    #SAMPLELIST[[bcc]]<- sample((SavedVecYearLoc[bcc]+1):(MAXYEAR+1),dim(SelectedSimMatYearORSavedVec)[1], replace=T)
    #   SAMPLELIST[[bcc]]<- sample((MAXYEAR):(MAXYEAR+1),dim(SelectedSimMatYearORSavedVec)[1], replace=T)
    #  }else{SAMPLELIST[[bcc]]<-rep((MAXYEAR+1),dim(SelectedSimMatYearORSavedVec)[1])}
    if(!is.null(SAMPLELIST[[bcc]])){SelectedSimMatYearORSavedVec[,bcc] <-SAMPLELIST[[bcc]]}
    #}
  }
  
  SelectedSimMatBinary<-1*(SelectedSimMatYearORSavedVec!=(MAXYEAR+1))
  
  SVMAT <- t(matrix(SavedVecLoc, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  CarbonMATYearORSavedVec <- t(matrix(CarbonSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  # CarbonMATYearORSavedVec <- CarbonMAT
  # CarbonMATBinary <- CarbonMAT
  
  if(is.null(PrecalculatedCarbonSelectedTableMean)){
    
    for(abb in 1:dim(CarbonMATYearORSavedVec)[1])
    {
      for(bcc in 1:dim(CarbonMATYearORSavedVec)[2])
      {
        #CarbonMAT[abb,bcc]<-CarbonSelectedYear[bcc,paste0("JulesMeanY",SelectedSimMat[abb,bcc])]
        CarbonMATYearORSavedVec[abb,bcc]<-CarbonSelectedYear[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
        
      }
      
    }
    
  }else{CarbonMATYearORSavedVec<-PrecalculatedCarbonSelectedTableMean
  for (bb in 1:length(SavedVecYearLoc))
  {#if(SavedVecYearLoc[bb]<(MAXYEAR+1)){
    if(!is.null(SAMPLELIST[[bb]])){  
      CarbonMATYearORSavedVec[,bb]<-as.numeric(CarbonSelectedYear[bb,1+SAMPLELIST[[bb]]])}
    #}
    
    
  }}
  
  for (i in 1:length(SpeciesListSelected)) {
    specie_name <- names(SpeciesListSelected)[i]
    specie_value <- SpeciesListSelected[[i]]
    mat_name <- paste0(specie_name, "MAT")
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  AreaMAT <- t(matrix(AreaSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  CarbonSDMATYearORSavedVec <- t(matrix(CarbonSelectedSD, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  #CarbonSDMATYearORSavedVec<-CarbonSDMAT
  
  
  
  if(is.null(PrecalculatedCarbonSelectedTableSD)){
    
    for(abb in 1:dim(CarbonSDMATYearORSavedVec)[1])
    {
      for(bcc in 1:dim(CarbonSDMATYearORSavedVec)[2])
      {
        CarbonSDMATYearORSavedVec[abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
        
      }
      
    }
  }else{CarbonSDMATYearORSavedVec<-PrecalculatedCarbonSelectedTableSD
  for (bb in 1:length(SavedVecYearLoc))
  {#if(SavedVecLoc[bb]==1){
    if(!is.null(SAMPLELIST[[bb]])){  
      CarbonSDMATYearORSavedVec[,bb]<-as.numeric(CarbonSelectedSDYear[bb,1+SAMPLELIST[[bb]]])}
  }
  
  
  }
  for (i in 1:length(SpeciesListSelectedSD)) {
    specie_name <- names(SpeciesListSelectedSD)[i]
    specie_value <- SpeciesListSelectedSD[[i]]
    mat_name <- paste0(specie_name, "MAT")
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  
  # Create a data frame representing the selected similarity matrix
  # SelectedSimMatBinary <- data.frame(1 * (SelectedSimMatBinary | SVMAT))
  
  
  
  if (is.null(ManualTargets)){
    SelecTargetCarbon <- input$SliderMain
    SelecTargetBioVector <- c()
    SelecTargetBioList <- list()
    for (x in names(SpeciesListSelected)) {
      var_name <- paste0("SelecTargetBio", x)
      value <- input[[paste0("BioSlider", x)]]
      assign(var_name, value)
      SelecTargetBioVector <- c(SelecTargetBioVector, value)
      SelecTargetBioList[var_name] <- value
    }
    SelecTargetArea <- input$AreaSlider
    SelecTargetVisits <- input$VisitsSlider}else{SelecTargetCarbon<-ManualTargets[[1]];SelecTargetBioVector<-unlist(ManualTargets[[2]]);
    SelecTargetBioList<-ManualTargets[[2]];
    SelecTargetArea<-ManualTargets[[3]];SelecTargetVisits<-ManualTargets[[4]]
    }
  
  
  speciesMat <- do.call("data.frame", setNames(lapply(names(SpeciesListSelected),
                                                      function(x) bquote(rowMeans(SelectedSimMatBinary * get(paste0(.(x), "MAT"))))),
                                               names(SpeciesListSelected)))
  speciesMatSD <- do.call("data.frame", setNames(lapply(names(SpeciesListSelectedSD),
                                                        function(x) bquote(sqrt(rowSums(SelectedSimMatBinary * (get(paste0(.(x), "MAT"))^2) / length(SavedVecYearLoc))))),
                                                 names(SpeciesListSelectedSD)))
  
  SelectedSimMat2 <- data.frame(SelectedSimMat=SelectedSimMat,SelectedSimMatBinary=SelectedSimMatBinary,
                                SelectedSimMatYearORSavedVec=SelectedSimMatYearORSavedVec,
                                Carbon = rowSums(CarbonMATYearORSavedVec),#rowSums(SelectedSimMat * CarbonMAT),
                                speciesMat,
                                Area = rowSums(SelectedSimMatBinary * AreaMAT),
                                Visits = rowMeans(SelectedSimMatBinary * (VisitsMAT)),
                                CarbonSD = sqrt(rowSums(CarbonMATYearORSavedVec^2)),#sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
                                speciesMatSD,
                                VisitsSD = sqrt(rowSums(SelectedSimMatBinary * (VisitsSDMAT^2))) / length(SavedVecYearLoc))
  
  Icalc <- MultiImpl(
    TargetsVec = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetArea, SelecTargetVisits),
    EYMat = data.frame(SelectedSimMat2$Carbon, speciesMat, SelectedSimMat2$Area, SelectedSimMat2$Visits),
    SDYMat = data.frame(SelectedSimMat2$CarbonSD, speciesMatSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
    alpha = alphaLVL, tolVec = tolvec
  )
  
  LimitsMat <- (-data.frame(SelectedSimMat2$Carbon,
                            speciesMat,
                            SelectedSimMat2$Area,
                            SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + tolvec[1],
                                                                       speciesMatSD^2 + tolvec[1 + (1:ncol(speciesMat))],
                                                                       rep(0, length(SelectedSimMat2$Area)) + tolvec[length(tolvec) - 1],
                                                                       SelectedSimMat2$VisitsSD^2 + tolvec[length(tolvec)]))
  
  return(c(list(SelectedSimMat2 = SelectedSimMat2, Icalc = Icalc, LimitsMat = LimitsMat, SelecTargetCarbon = SelecTargetCarbon,
                SelecTargetArea = SelecTargetArea, SelecTargetVisits = SelecTargetVisits),
           SelecTargetBioList,SAMPLELIST=SAMPLELIST))
}
# This is function outputmap_calculateMats adapted to the case where
outputmap_calculateMatsYearType <- function(input,
                                            SavedVecLoc,
                                            simul636YearTypeLoc,
                                            AreaSelected,
                                            CarbonSelected,
                                            CarbonSelectedYear,
                                            CarbonSelectedYear85,
                                            # RedSquirrelSelected,
                                            SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                            VisitsSelected,
                                            CarbonSelectedSD,
                                            CarbonSelectedSDYear,
                                            CarbonSelectedSDYear85,
                                            # RedSquirrelSelectedSD,
                                            SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                            VisitsSelectedSD,
                                            alphaLVL = alphaLVL,
                                            input_areaSlider_multiplicative_coefficient = TRUE,
                                            ManualTargets=NULL,
                                            tolvec,
                                            #YearSelect,
                                            PrecalculatedCarbonSelectedTableTypeMean=NULL,
                                            PrecalculatedCarbonSelectedTableTypeSD=NULL,
                                            SavedVecYearTypeLoc,
                                            #  PreviousSavedVecYearTypeLoc,
                                            SAMPLELIST,
                                            MAXYEAR=MAXYEAR,
                                            CONCATENATION_SIM_MAT=FALSE) {
  
  # Because only CO2 has variations with time we create a "binary" SelectedSimMat that is used for 
  # all the other output variables. (MAXYEAR+1) is the code for No Planting
  # SelectedSimMat: matrix of simulations with year
  # SelectedSimMatBinary: convert SelectedSimMat without year of planting (binary)
  # SelectedSimMatYearOrSavedVec: matrix where the column where SavedVec=1 is replaced by 0 (planting at year 0)
  # SVMMAT: Matrix composed on SavedVec on every line (to do an OR later).
  # For the moment, the area does not change with year of planting.
  if (length(SavedVecYearTypeLoc) == 1) {
    SelectedSimMat <- as.matrix(simul636YearTypeLoc$YEAR[, 1:length(SavedVecYearTypeLoc)])
    SelectedSimMatBinary<- 1*(as.matrix(simul636YearTypeLoc$YEAR[, 1:length(SavedVecYearTypeLoc)])!=(-1))
  } else {
    SelectedSimMat <- simul636YearTypeLoc$YEAR[, 1:length(SavedVecYearTypeLoc)]
    SelectedSimMatBinary <- 1*(simul636YearTypeLoc$YEAR[, 1:length(SavedVecYearTypeLoc)]!=(-1))
  }
  
  SelectedSimMatYearTypeORSavedVec<-simul636YearTypeLoc
  
  #Need to transform matrix here.
  
  #We assume that if the land parcel has been clicked by the user, it overrides previous year of planting
  # and forces the whole column of SelectedSimMat to be equal to 0 (plant at year 0)
  # Note that SelectedSimMat is only used for CO2 as for the others, 
  
  
  for(bcc in 1:dim(SelectedSimMatYearTypeORSavedVec$TYPE)[2])
  {
    if(!is.null(SAMPLELIST[[bcc]])){
      SelectedSimMatYearTypeORSavedVec$YEAR[,bcc] <-SAMPLELIST[[bcc]]$YEAR
      SelectedSimMatYearTypeORSavedVec$TYPE[,bcc] <-SAMPLELIST[[bcc]]$TYPE
    }
    
  }
  SelectedSimMatBinary<-1*(SelectedSimMatYearTypeORSavedVec$YEAR!=(-1))
  
  SVMAT <- t(matrix(SavedVecYearTypeLoc, length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  #CarbonMATYearTypeORSavedVec <- t(matrix(as.matrix(CarbonSelectedYear), length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  #CarbonMATYearTypeORSavedVec85 <- t(matrix(CarbonSelectedYear85, length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  
  #if(is.null(PrecalculatedCarbonSelectedTableTypeMean)){
  
  # for(abb in 1:dim(CarbonMATYearTypeORSavedVec)[1])
  #  {
  #   for(bcc in 1:dim(CarbonMATYearORSavedVec)[2])
  #  {
  #  CarbonMATYearTypeORSavedVec[abb,bcc]<-CarbonSelectedYear[bcc,paste0("JulesMeanY",SelectedSimMatYearORSavedVec[abb,bcc])]
  #   CarbonMATYearTypeORSavedVec85[abb,bcc]<-CarbonSelectedYear85[bcc,paste0("JulesMeanY",SelectedSimMatYearORSavedVec[abb,bcc])]
  #    
  #  }
  
  #}
  
  #}else{
  
  CarbonMATYearTypeORSavedVec<-PrecalculatedCarbonSelectedTableTypeMean
  for (bb in 1:length(SavedVecYearTypeLoc))
  {
    if(!is.null(SAMPLELIST[[bb]])){  
  
      CarbonMATYearTypeORSavedVec[,bb]<-0
      TypeA<-(SAMPLELIST[[bb]]$TYPE=="Conifers")
      TypeB<-(SAMPLELIST[[bb]]$TYPE=="Deciduous")
      CarbonMATYearTypeORSavedVec[TypeA,bb]<-as.numeric(CarbonSelectedYear[bb,1+SAMPLELIST[[bb]]$YEAR[TypeA]])      
      CarbonMATYearTypeORSavedVec[TypeB,bb]<-as.numeric(CarbonSelectedYear85[bb,1+SAMPLELIST[[bb]]$YEAR[TypeB]])
    }
    
  }
  
  #}
  
  for (i in 1:length(SpeciesListSelected)) {
    specie_name <- names(SpeciesListSelected)[i]
    specie_value <- SpeciesListSelected[[i]]
    mat_name <- paste0(specie_name, "MAT")
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  AreaMAT <- t(matrix(AreaSelected, length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected), length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  
  #CarbonSDMATYearTypeORSavedVec <- t(matrix(CarbonSelectedSD, length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  #CarbonSDMATYearTypeORSavedVec85 <- t(matrix(CarbonSelectedSDYear85, length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  
  
  
  #if(is.null(PrecalculatedCarbonSelectedTableTypeSD)){
  
  #  for(abb in 1:dim(CarbonSDMATYearTypeORSavedVec)[1])
  # {
  #  for(bcc in 1:dim(CarbonSDMATYearTypeORSavedVec)[2])
  # {
  #  CarbonSDMATYearTypeORSavedVec[abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("JulesSDY",SelectedSimMatYearORSavedVec[abb,bcc])]
  # CarbonSDMATYearTypeORSavedVec85[abb,bcc]<-CarbonSelectedSDYear85[bcc,paste0("JulesSDY",SelectedSimMatYearORSavedVec[abb,bcc])]
  #}
  #  }
  #}else{
  CarbonSDMATYearTypeORSavedVec<-PrecalculatedCarbonSelectedTableTypeSD
  for (bb in 1:length(SavedVecYearTypeLoc))
  {  if(!is.null(SAMPLELIST[[bb]])){  
  
    CarbonSDMATYearTypeORSavedVec[,bb]<-0
    TypeA<-(SAMPLELIST[[bb]]$TYPE=="Conifers")
    TypeB<-(SAMPLELIST[[bb]]$TYPE=="Deciduous")
    CarbonSDMATYearTypeORSavedVec[TypeA,bb]<-as.numeric(CarbonSelectedSDYear[bb,1+SAMPLELIST[[bb]]$YEAR[TypeA]])      
    CarbonSDMATYearTypeORSavedVec[TypeB,bb]<-as.numeric(CarbonSelectedSDYear85[bb,1+SAMPLELIST[[bb]]$YEAR[TypeB]])
    
    #   CarbonSDMATYearTypeORSavedVec[,bb]<-as.numeric(CarbonSelectedSDYear[bb,1+SAMPLELIST[[bb]]])
    
  }
  }
  
  
  #}
  for (i in 1:length(SpeciesListSelectedSD)) {
    specie_name <- names(SpeciesListSelectedSD)[i]
    specie_value <- SpeciesListSelectedSD[[i]]
    mat_name <- paste0(specie_name, "MAT")
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVecYearTypeLoc), dim(SelectedSimMat)[1]))
  
  
  
  #browser()
  if (is.null(ManualTargets)){
    SelecTargetCarbon <- input$SliderMain
    SelecTargetBioVector <- c()
    SelecTargetBioList <- list()
    for (x in names(SpeciesListSelected)) {
      var_name <- paste0("SelecTargetBio", x)
      value <- input[[paste0("BioSlider", x)]]
      assign(var_name, value)
      SelecTargetBioVector <- c(SelecTargetBioVector, value)
      SelecTargetBioList[var_name] <- value
    }
    SelecTargetArea <- input$AreaSlider
    SelecTargetVisits <- input$VisitsSlider}else{SelecTargetCarbon<-ManualTargets[[1]];SelecTargetBioVector<-unlist(ManualTargets[[2]]);
    SelecTargetBioList<-ManualTargets[[2]];
    SelecTargetArea<-ManualTargets[[3]];SelecTargetVisits<-ManualTargets[[4]]
    }
  
  
  speciesMat <- do.call("data.frame", setNames(lapply(names(SpeciesListSelected),
                                                      function(x) bquote(rowMeans(SelectedSimMatBinary * get(paste0(.(x), "MAT"))))),
                                               names(SpeciesListSelected)))
  speciesMatSD <- do.call("data.frame", setNames(lapply(names(SpeciesListSelectedSD),
                                                        function(x) bquote(sqrt(rowSums(SelectedSimMatBinary * (get(paste0(.(x), "MAT"))^2) / length(SavedVecYearTypeLoc))))),
                                                 names(SpeciesListSelectedSD)))
  
  SelectedSimMat2 <- data.frame(SelectedSimMat=SelectedSimMatYearTypeORSavedVec,#SelectedSimMat=SelectedSimMat,SelectedSimMatBinary=SelectedSimMatBinary,
                                #SelectedSimMatYearTypeORSavedVec=SelectedSimMatYearTypeORSavedVec,
                                Carbon = rowSums(CarbonMATYearTypeORSavedVec),
                                speciesMat,
                                Area = rowSums(SelectedSimMatBinary * AreaMAT),
                                Visits = rowMeans(SelectedSimMatBinary * (VisitsMAT)),
                                CarbonSD = sqrt(rowSums(CarbonSDMATYearTypeORSavedVec^2)),
                                speciesMatSD,
                                VisitsSD = sqrt(rowSums(SelectedSimMatBinary * (VisitsSDMAT^2))) / length(SavedVecYearTypeLoc))
  #  write.csv(SelectedSimMat3,file="d:\\ValuesMat.csv")
  #  save(simul636YearTypeLoc,file="d:\\SimulYearType.Rdata")
  
  
  Icalc <- MultiImpl(
    TargetsVec = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetArea, SelecTargetVisits),
    EYMat = data.frame(SelectedSimMat2$Carbon, speciesMat, SelectedSimMat2$Area, SelectedSimMat2$Visits),
    SDYMat = data.frame(SelectedSimMat2$CarbonSD,speciesMatSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
    alpha = alphaLVL, tolVec = tolvec
  )
  
  LimitsMat <- (-data.frame(SelectedSimMat2$Carbon,
                            speciesMat,
                            SelectedSimMat2$Area,
                            SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + tolvec[1], 
                                                                       speciesMatSD^2 + tolvec[1 + (1:ncol(speciesMat))],
                                                                       rep(0, length(SelectedSimMat2$Area)) + tolvec[length(tolvec) - 1],
                                                                       SelectedSimMat2$VisitsSD^2 + tolvec[length(tolvec)]))
  
  if(!CONCATENATION_SIM_MAT){
    TORETURN<-c(list(SelectedSimMat2 = SelectedSimMat2, Icalc = Icalc, LimitsMat = LimitsMat, SelecTargetCarbon = SelecTargetCarbon,
                   SelecTargetArea = SelecTargetArea, SelecTargetVisits = SelecTargetVisits),
    SelecTargetBioList,SAMPLELIST=SAMPLELIST)
  }else{
    SelectedSimMat2_YearType_Concatenation<-data.frame(YEAR=apply(as.matrix(SelectedSimMatYearTypeORSavedVec$YEAR),c(1,2),as.character),
                                                       TYPE=as.matrix(SelectedSimMatYearTypeORSavedVec$TYPE))
    
    TORETURN<-c(list(SelectedSimMat2 = SelectedSimMat2, Icalc = Icalc, LimitsMat = LimitsMat, SelecTargetCarbon = SelecTargetCarbon,
                     SelecTargetArea = SelecTargetArea, SelecTargetVisits = SelecTargetVisits,SelectedSimMat2_YearType_Concatenation=SelectedSimMat2_YearType_Concatenation),
                SelecTargetBioList,SAMPLELIST=SAMPLELIST)
  }
  
  return(TORETURN)
}

InitFindMaxSliderValues <- function(SavedVecLoc,
                                    AreaSelected,
                                    CarbonSelected,
                                    # RedSquirrelSelected,
                                    SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                    VisitsSelected,
                                    CarbonSelectedSD,
                                    # RedSquirrelSelectedSD,
                                    SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                    VisitsSelectedSD,
                                    input_areaSlider_multiplicative_coefficient = TRUE,
                                    alpha,
                                    MAXYEAR) {
  simul636Loc <- matrix(1,2,length(SavedVecLoc))
  # If only one element in SavedVec, select corresponding column in simul636
  if (length(SavedVecLoc) == 1) {
    SelectedSimMat <- as.matrix(simul636Loc[, 1:length(SavedVecLoc)])
  } else {
    SelectedSimMat <- simul636Loc[, 1:length(SavedVecLoc)]
  }
  
  SVMAT <- t(matrix(SavedVecLoc, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  CarbonMAT <- t(matrix(CarbonSelected, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  # RedSquirrelMAT <- t(matrix(as.numeric(RedSquirrelSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelected)) {
    specie_name <- names(SpeciesListSelected)[i]
    specie_value <- SpeciesListSelected[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "Selected"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  AreaMAT <- t(matrix(AreaSelected, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected), length(SavedVecLoc), dim(SelectedSimMat)[1]))
  
  CarbonSDMAT <- t(matrix(CarbonSelectedSD, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  # RedSquirrelSDMAT <- t(matrix(as.numeric(RedSquirrelSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelectedSD)) {
    specie_name <- names(SpeciesListSelectedSD)[i]
    specie_value <- SpeciesListSelectedSD[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "SelectedSD"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVecLoc), dim(SelectedSimMat)[1]))
  
  # Create a data frame representing the selected similarity matrix
  SelectedSimMat <- data.frame(1 * (SelectedSimMat | SVMAT))
  
  #SelecTargetCarbon <- input$SliderMain
  # SelecTargetBio <- input$BioSlider
  #SelecTargetBioVector <- c()
  #SelecTargetBioList <- list()
  #for (x in names(SpeciesListSelected)) {
  #  var_name <- paste0("SelecTargetBio", x)
  #  # input[paste0("BioSlider", x)] bugs because it is a reactivevalue
  #  value <- input[[paste0("BioSlider", x)]]
  #  assign(var_name, value)
  #  SelecTargetBioVector <- c(SelecTargetBioVector, value)
  #  SelecTargetBioList[var_name] <- value
  #}
  #SelecTargetArea <- input$AreaSlider
  #SelecTargetVisits <- input$VisitsSlider
  
  speciesMat <- do.call("data.frame", setNames(lapply(names(SpeciesListSelected),
                                                      function(x) bquote(rowMeans(SelectedSimMat * get(paste0(.(x), "MAT"))))),
                                               names(SpeciesListSelected)))
  speciesMatSD <- do.call("data.frame", setNames(lapply(names(SpeciesListSelectedSD),
                                                        function(x) bquote(sqrt(rowSums(SelectedSimMat * (get(paste0(.(x), "MAT"))^2) / length(SavedVecLoc))))),
                                                 names(SpeciesListSelectedSD)))
  SelectedSimMat2 <- data.frame(SelectedSimMat,
                                Carbon = rowSums(SelectedSimMat * CarbonMAT),
                                # redsquirrel = rowMeans(SelectedSimMat * RedSquirrelMAT),
                                speciesMat,
                                Area = rowSums(SelectedSimMat * AreaMAT),
                                Visits = rowMeans(SelectedSimMat * (VisitsMAT)),
                                CarbonSD = sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
                                # redsquirrelSD = sqrt(rowSums(SelectedSimMat * (RedSquirrelSDMAT^2))) / length(SavedVec),
                                speciesMatSD,
                                VisitsSD = sqrt(rowSums(SelectedSimMat * (VisitsSDMAT^2))) / length(SavedVecLoc))
  # for (specie_name in names(SpeciesListSelected)) {
  #   value <- rowMeans(SelectedSimMat * get(paste0(specie_name, "MAT")))
  #   SelectedSimMat2[specie_name] <- value
  # }
  # for (specie_name in names(SpeciesListSelectedSD)) {
  #   value <- sqrt(rowSums(SelectedSimMat * (get(paste0(specie_name, "MAT"))^2) / length(SavedVec)))
  #   SelectedSimMat2[specie_name] <- value
  # }
  
  tolvec <- c("Carbon" = abs(mean(SelectedSimMat2$Carbon)) / 150,
              abs(colMeans(speciesMat)) / 150,
              "Area" = mean(SelectedSimMat2$Area) / 150,
              "Visits" = mean(SelectedSimMat2$Visits) / 150)
  for(i in 1:length(tolvec)) {
    # tolvec is a named vector, so tolvec[i] == 0 produces a named vector with the value, not the value directly
    # this causes a bug, isTRUE returns the boolean only
    if (isTRUE(tolvec[i] == 0)) {
      tolvec[i] <- 0.1
    }
  }
  # tolVec <- c(4, 0.05, 0.1, 2)
  #  Icalc <- MultiImpl(# TargetsVec = c(SelecTargetCarbon, SelecTargetBio, SelecTargetArea, SelecTargetVisits),
  #   TargetsVec = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetArea, SelecTargetVisits),
  #  EYMat = data.frame(SelectedSimMat2$Carbon, speciesMat, SelectedSimMat2$Area, SelectedSimMat2$Visits),
  # SDYMat = data.frame(SelectedSimMat2$CarbonSD, speciesMatSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
  #  alpha = alpha, tolVec = tolvec)
  
  LimitsMat <- (-data.frame(SelectedSimMat2$Carbon,
                            # SelectedSimMat2$redsquirrel,
                            speciesMat,
                            SelectedSimMat2$Area,
                            # SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + 4^2, SelectedSimMat2$redsquirrelSD^2 + 2^2, rep(0, length(SelectedSimMat2$Area)) + 100^2, SelectedSimMat2$VisitsSD + 2^2))
                            SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + tolvec[1],
                                                                       speciesMatSD^2 + tolvec[1 + (1:ncol(speciesMat))],
                                                                       rep(0, length(SelectedSimMat2$Area)) + tolvec[length(tolvec) - 1],
                                                                       SelectedSimMat2$VisitsSD^2 + tolvec[length(tolvec)]))
  # we would want here the inverse truncated normal,
  CarbonMax<-max(0,trunc(sqrt(SelectedSimMat2$CarbonSD[1]^2 + tolvec[1])*qtruncnorm(p=1-alpha,a=LimitsMat[1,1],b=Inf,mean=0,sd=1)+SelectedSimMat2$Carbon[1]))
  bioMaxList<-list()
  for (ijj in 1:length(names(speciesMat)))
  {NM<-names(speciesMat)[ijj]
  NMSD<-names(speciesMatSD)[ijj]
  bioMaxList[[ijj]]<-max(0,trunc(sqrt(SelectedSimMat2[[NMSD]][1]^2 + tolvec[1+ijj])*qtruncnorm(p=1-alpha,a=LimitsMat[1,1+ijj],b=Inf,mean=0,sd=1)+SelectedSimMat2[[NM]][1]))
  
  }
  AreaMax<-max(0,trunc(1+sqrt( tolvec[dim(LimitsMat)[2]-1])*qtruncnorm(p=alpha,a=LimitsMat[1,dim(LimitsMat)[2]-1],b=Inf,mean=0,sd=1)+SelectedSimMat2$Area[1]))
  AreaMin<-max(0,trunc(1+sqrt( tolvec[dim(LimitsMat)[2]-1])*qtruncnorm(p=alpha,a=0,b=Inf,mean=0,sd=1)))
  VisistMax<-max(0,trunc(sqrt( SelectedSimMat2$VisitsSD[1]^2+tolvec[dim(LimitsMat)[2]])*qtruncnorm(p=1-alpha,a=LimitsMat[1,dim(LimitsMat)[2]],b=Inf,mean=0,sd=1)+SelectedSimMat2$Visits[1]))
  
  
  return(list(CarbonMax=CarbonMax,bioMaxList=bioMaxList,AreaMax=AreaMax,AreaMin=AreaMin,VisistMax=VisistMax,tolvec=tolvec))
}

InitFindMaxSliderValuesYear <- function(SavedVecLoc,
                                        AreaSelected,
                                        CarbonSelected,
                                        CarbonSelectedYear,
                                        CarbonSelectedYear85,
                                        # RedSquirrelSelected,
                                        SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                        VisitsSelected,
                                        CarbonSelectedSD,
                                        CarbonSelectedSDYear,
                                        CarbonSelectedSDYear85,
                                        # RedSquirrelSelectedSD,
                                        SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                        VisitsSelectedSD,
                                        input_areaSlider_multiplicative_coefficient = TRUE,
                                        alpha,
                                        SavedVecYearLoc,
                                        MAXYEAR) {
  
  
  simul636Loc <- matrix(1,2,length(SavedVecYearLoc))
  simul636YearLoc <- matrix(1,2,length(SavedVecYearLoc))
  #TODO
  
  # If only one element in SavedVec, select corresponding column in simul636
  if (length(SavedVecLoc) == 1) {
    SelectedSimMat <- as.matrix(simul636Loc[, 1:length(SavedVecYearLoc)])
    SelectedSimMatBinary<- 1*(as.matrix(simul636YearLoc[, 1:length(SavedVecYearLoc)])!=(MAXYEAR+1))
    
  } else {
    SelectedSimMat <- simul636YearLoc[, 1:length(SavedVecYearLoc)]
    SelectedSimMatBinary <- 1*(simul636YearLoc[, 1:length(SavedVecYearLoc)]!=(MAXYEAR+1))
  }
  
  
  SelectedSimMatYearORSavedVec<-SelectedSimMat
  #We assume that if the land parcel has been clicked by the user, it overrides previous year of planting
  # and forces the whole column of SelectedSimMat to be equal to 0 (plant at year 0)
  # Note that SelectedSimMat is only used for CO2 as for the others, 
  #for(bcc in 1:dim(SelectedSimMatYearORSavedVec)[2])
  #{if(SavedVecLoc[bcc]==1){
  #  SelectedSimMatYearORSavedVec[,bcc] <- 0}
  #}
  
  
  SVMAT <- t(matrix(SavedVecYearLoc, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  #CarbonMAT <- t(matrix(CarbonSelected, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  CarbonMATYearORSavedVec <- t(matrix(CarbonSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  CarbonMATYearORSavedVec85 <- t(matrix(CarbonSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  
  
  for(abb in 1:dim(CarbonMATYearORSavedVec)[1])
  {
    for(bcc in 1:dim(CarbonMATYearORSavedVec)[2])
    {
      #CarbonMAT[abb,bcc]<-CarbonSelectedYear[bcc,paste0("JulesMeanY",SelectedSimMat[abb,bcc])]
      CarbonMATYearORSavedVec[abb,bcc]<-CarbonSelectedYear[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
      CarbonMATYearORSavedVec85[abb,bcc]<-CarbonSelectedYear85[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
    }
    
  }
  
  # RedSquirrelMAT <- t(matrix(as.numeric(RedSquirrelSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelected)) {
    specie_name <- names(SpeciesListSelected)[i]
    specie_value <- SpeciesListSelected[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "Selected"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  AreaMAT <- t(matrix(AreaSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected),  length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  #  CarbonSDMAT <- t(matrix(CarbonSelectedSD, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  CarbonSDMATYearORSavedVec <- t(matrix(CarbonSelectedSD, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  CarbonSDMATYearORSavedVec85 <- t(matrix(CarbonSelectedSD, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  for(abb in 1:dim(CarbonSDMATYearORSavedVec)[1])
  {
    for(bcc in 1:dim(CarbonSDMATYearORSavedVec)[2])
    {
      CarbonSDMATYearORSavedVec[abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
      CarbonSDMATYearORSavedVec85[abb,bcc]<-CarbonSelectedSDYear85[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
    }
    
  }
  
  # RedSquirrelSDMAT <- t(matrix(as.numeric(RedSquirrelSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelectedSD)) {
    specie_name <- names(SpeciesListSelectedSD)[i]
    specie_value <- SpeciesListSelectedSD[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "SelectedSD"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  # Create a data frame representing the selected similarity matrix
  #SelectedSimMat <- data.frame(1 * (SelectedSimMat | SVMAT))
  SelectedSimMatBinary <- data.frame(1 * (SelectedSimMatBinary | SVMAT))
  
  
  #SelecTargetCarbon <- input$SliderMain
  # SelecTargetBio <- input$BioSlider
  #SelecTargetBioVector <- c()
  #SelecTargetBioList <- list()
  #for (x in names(SpeciesListSelected)) {
  #  var_name <- paste0("SelecTargetBio", x)
  #  # input[paste0("BioSlider", x)] bugs because it is a reactivevalue
  #  value <- input[[paste0("BioSlider", x)]]
  #  assign(var_name, value)
  #  SelecTargetBioVector <- c(SelecTargetBioVector, value)
  #  SelecTargetBioList[var_name] <- value
  #}
  #SelecTargetArea <- input$AreaSlider
  #SelecTargetVisits <- input$VisitsSlider
  
  speciesMat <- do.call("data.frame", setNames(lapply(names(SpeciesListSelected),
                                                      function(x) bquote(rowMeans(SelectedSimMat * get(paste0(.(x), "MAT"))))),
                                               names(SpeciesListSelected)))
  speciesMatSD <- do.call("data.frame", setNames(lapply(names(SpeciesListSelectedSD),
                                                        function(x) bquote(sqrt(rowSums(SelectedSimMat * (get(paste0(.(x), "MAT"))^2) / length(SavedVecYearLoc))))),
                                                 names(SpeciesListSelectedSD)))
  #SelectedSimMat2 <- data.frame(SelectedSimMat,
  #                             Carbon = rowSums(SelectedSimMat * CarbonMAT),
  #                            # redsquirrel = rowMeans(SelectedSimMat * RedSquirrelMAT),
  #                           speciesMat,
  #                          Area = rowSums(SelectedSimMat * AreaMAT),
  #                         Visits = rowMeans(SelectedSimMat * (VisitsMAT)),
  #                        CarbonSD = sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
  #                       # redsquirrelSD = sqrt(rowSums(SelectedSimMat * (RedSquirrelSDMAT^2))) / length(SavedVec),
  #                      speciesMatSD,
  #                     VisitsSD = sqrt(rowSums(SelectedSimMat * (VisitsSDMAT^2))) / length(SavedVecLoc))
  
  SelectedSimMat2 <- data.frame(SelectedSimMat=SelectedSimMat,SelectedSimMatBinary=SelectedSimMatBinary,
                                SelectedSimMatYearORSavedVec=SelectedSimMatYearORSavedVec,
                                Carbon = rowSums(CarbonMATYearORSavedVec85),#rowSums(SelectedSimMat * CarbonMAT),
                                speciesMat,
                                Area = rowSums(SelectedSimMatBinary * AreaMAT),
                                Visits = rowMeans(SelectedSimMatBinary * (VisitsMAT)),
                                CarbonSD = sqrt(rowSums(CarbonSDMATYearORSavedVec85^2)),#sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
                                speciesMatSD,
                                VisitsSD = sqrt(rowSums(SelectedSimMatBinary * (VisitsSDMAT^2))) / length(SavedVecYearLoc))
  
  
  # for (specie_name in names(SpeciesListSelected)) {
  #   value <- rowMeans(SelectedSimMat * get(paste0(specie_name, "MAT")))
  #   SelectedSimMat2[specie_name] <- value
  # }
  # for (specie_name in names(SpeciesListSelectedSD)) {
  #   value <- sqrt(rowSums(SelectedSimMat * (get(paste0(specie_name, "MAT"))^2) / length(SavedVec)))
  #   SelectedSimMat2[specie_name] <- value
  # }
  
  tolvec <- c(abs(mean(SelectedSimMat2$Carbon)) / 150,
              abs(colMeans(speciesMat)) / 150,
              mean(SelectedSimMat2$Area) / 150,
              mean(SelectedSimMat2$Visits) / 150)
  for(i in 1:length(tolvec)) {
    # tolvec is a named vector, so tolvec[i] == 0 produces a named vector with the value, not the value directly
    # this causes a bug, isTRUE returns the boolean only
    if (isTRUE(tolvec[i] == 0)) {
      tolvec[i] <- 0.1
    }
  }
  # tolVec <- c(4, 0.05, 0.1, 2)
  #  Icalc <- MultiImpl(# TargetsVec = c(SelecTargetCarbon, SelecTargetBio, SelecTargetArea, SelecTargetVisits),
  #   TargetsVec = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetArea, SelecTargetVisits),
  #  EYMat = data.frame(SelectedSimMat2$Carbon, speciesMat, SelectedSimMat2$Area, SelectedSimMat2$Visits),
  # SDYMat = data.frame(SelectedSimMat2$CarbonSD, speciesMatSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
  #  alpha = alpha, tolVec = tolvec)
  
  LimitsMat <- (-data.frame(SelectedSimMat2$Carbon,
                            # SelectedSimMat2$redsquirrel,
                            speciesMat,
                            SelectedSimMat2$Area,
                            # SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + 4^2, SelectedSimMat2$redsquirrelSD^2 + 2^2, rep(0, length(SelectedSimMat2$Area)) + 100^2, SelectedSimMat2$VisitsSD + 2^2))
                            SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + tolvec[1],
                                                                       speciesMatSD^2 + tolvec[1 + (1:ncol(speciesMat))],
                                                                       rep(0, length(SelectedSimMat2$Area)) + tolvec[length(tolvec) - 1],
                                                                       SelectedSimMat2$VisitsSD^2 + tolvec[length(tolvec)]))
  # we would want here the inverse truncated normal,

  CarbonMax<-max(0,trunc(sqrt(sum((CarbonSDMATYearORSavedVec[1,])^2) + tolvec[1])*qtruncnorm(p=1-alpha,a=LimitsMat[1,1],b=Inf,mean=0,sd=1)+SelectedSimMat2$Carbon[1]))
  bioMaxList<-list()
  for (ijj in 1:length(names(speciesMat)))
  {NM<-names(speciesMat)[ijj]
  NMSD<-names(speciesMatSD)[ijj]
  bioMaxList[[ijj]]<-max(0,trunc(sqrt(SelectedSimMat2[[NMSD]][1]^2 + tolvec[1+ijj])*qtruncnorm(p=1-alpha,a=LimitsMat[1,1+ijj],b=Inf,mean=0,sd=1)+SelectedSimMat2[[NM]][1]))
  
  }
  AreaMax<-max(0,trunc(1+sqrt( tolvec[dim(LimitsMat)[2]-1])*qtruncnorm(p=alpha,a=LimitsMat[1,dim(LimitsMat)[2]-1],b=Inf,mean=0,sd=1)+SelectedSimMat2$Area[1]))
  AreaMin<-max(0,trunc(1+sqrt( tolvec[dim(LimitsMat)[2]-1])*qtruncnorm(p=alpha,a=0,b=Inf,mean=0,sd=1)))
  VisistMax<-max(0,trunc(sqrt( SelectedSimMat2$VisitsSD[1]^2+tolvec[dim(LimitsMat)[2]])*qtruncnorm(p=1-alpha,a=LimitsMat[1,dim(LimitsMat)[2]],b=Inf,mean=0,sd=1)+SelectedSimMat2$Visits[1]))
  
  cat(proc.time()-tt)
  cat("\n")
  return(list(CarbonMax=CarbonMax,bioMaxList=bioMaxList,AreaMax=AreaMax,AreaMin=AreaMin,VisistMax=VisistMax,tolvec=tolvec))
}

add_suffix_to_duplicates <- function(vec) {
  seen <- list()
  for (i in seq_along(vec)) {
    if (vec[i] %in% names(seen)) {
      seen[[vec[i]]] <- seen[[vec[i]]] + 1
      vec[i] <- paste(vec[i], seen[[vec[i]]], sep = "_")
    } else {
      seen[[vec[i]]] <- 1
    }
  }
  return(vec)
}

InitFindMaxSliderValuesYearType_NegativeVals <- function(SavedVecLoc,
                                        AreaSelected,
                                        CarbonSelected,
                                        CarbonSelectedYear,
                                        CarbonSelectedYear85,
                                        # RedSquirrelSelected,
                                        SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                        VisitsSelected,
                                        CarbonSelectedSD,
                                        CarbonSelectedSDYear,
                                        CarbonSelectedSDYear85,
                                        # RedSquirrelSelectedSD,
                                        SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                        VisitsSelectedSD,
                                        input_areaSlider_multiplicative_coefficient = TRUE,
                                        alpha,
                                        SavedVecYearLoc,
                                        MAXYEAR,
                                        PrecalcCarbonAllExtentsType,
                                        PrecalcCarbonAllExtentsSDType) {
  
  simul636Loc <- matrix(1,2,length(SavedVecYearLoc))
  simul636YearLoc <- matrix(1,2,length(SavedVecYearLoc))
  #TODO
  
  # If only one element in SavedVec, select corresponding column in simul636
  if (length(SavedVecLoc) == 1) {
    SelectedSimMat <- as.matrix(simul636Loc[, 1:length(SavedVecYearLoc)])
    SelectedSimMatBinary<- 1*(as.matrix(simul636YearLoc[, 1:length(SavedVecYearLoc)])!=(MAXYEAR+1))
    
  } else {
    SelectedSimMat <- simul636YearLoc[, 1:length(SavedVecYearLoc)]
    SelectedSimMatBinary <- 1*(simul636YearLoc[, 1:length(SavedVecYearLoc)]!=(MAXYEAR+1))
  }
  
  
  SelectedSimMatYearORSavedVec<-SelectedSimMat
  #We assume that if the land parcel has been clicked by the user, it overrides previous year of planting
  # and forces the whole column of SelectedSimMat to be equal to 0 (plant at year 0)
  # Note that SelectedSimMat is only used for CO2 as for the others, 
  #for(bcc in 1:dim(SelectedSimMatYearORSavedVec)[2])
  #{if(SavedVecLoc[bcc]==1){
  #  SelectedSimMatYearORSavedVec[,bcc] <- 0}
  #}
  
  
  SVMAT <- t(matrix(SavedVecYearLoc, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  #CarbonMAT <- t(matrix(CarbonSelected, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  CarbonMATYearORSavedVec <- t(matrix(CarbonSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  CarbonMATYearORSavedVec85 <- t(matrix(CarbonSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  
  
  for(abb in 1:dim(CarbonMATYearORSavedVec)[1])
  {
    for(bcc in 1:dim(CarbonMATYearORSavedVec)[2])
    {
      #CarbonMAT[abb,bcc]<-CarbonSelectedYear[bcc,paste0("JulesMeanY",SelectedSimMat[abb,bcc])]
      CarbonMATYearORSavedVec[abb,bcc]<-CarbonSelectedYear[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieConifers_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
      CarbonMATYearORSavedVec85[abb,bcc]<-CarbonSelectedYear85[bcc,paste0("Carbon_Mean_Scenario26_TreeSpecieDeciduous_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
    }
    
  }
  
  # RedSquirrelMAT <- t(matrix(as.numeric(RedSquirrelSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelected)) {
    specie_name <- names(SpeciesListSelected)[i]
    specie_value <- SpeciesListSelected[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "Selected"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  AreaMAT <- t(matrix(AreaSelected, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected),  length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  #  CarbonSDMAT <- t(matrix(CarbonSelectedSD, length(SavedVecLoc), dim(SelectedSimMat)[1]))
  CarbonSDMATYearORSavedVec <- t(matrix(CarbonSelectedSD, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  CarbonSDMATYearORSavedVec85 <- t(matrix(CarbonSelectedSD, length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  for(abb in 1:dim(CarbonSDMATYearORSavedVec)[1])
  {
    for(bcc in 1:dim(CarbonSDMATYearORSavedVec)[2])
    {
      CarbonSDMATYearORSavedVec[abb,bcc]<-CarbonSelectedSDYear[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieConifers_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
      CarbonSDMATYearORSavedVec85[abb,bcc]<-CarbonSelectedSDYear85[bcc,paste0("Carbon_SD_Scenario26_TreeSpecieDeciduous_PlantingYear",SelectedSimMatYearORSavedVec[abb,bcc])]
    }
    
  }
  
  # RedSquirrelSDMAT <- t(matrix(as.numeric(RedSquirrelSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelectedSD)) {
    specie_name <- names(SpeciesListSelectedSD)[i]
    specie_value <- SpeciesListSelectedSD[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "SelectedSD"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVecYearLoc), dim(SelectedSimMat)[1]))
  
  # Create a data frame representing the selected similarity matrix
  #SelectedSimMat <- data.frame(1 * (SelectedSimMat | SVMAT))
  SelectedSimMatBinary <- data.frame(1 * (SelectedSimMatBinary | SVMAT))
  
  
  #SelecTargetCarbon <- input$SliderMain
  # SelecTargetBio <- input$BioSlider
  #SelecTargetBioVector <- c()
  #SelecTargetBioList <- list()
  #for (x in names(SpeciesListSelected)) {
  #  var_name <- paste0("SelecTargetBio", x)
  #  # input[paste0("BioSlider", x)] bugs because it is a reactivevalue
  #  value <- input[[paste0("BioSlider", x)]]
  #  assign(var_name, value)
  #  SelecTargetBioVector <- c(SelecTargetBioVector, value)
  #  SelecTargetBioList[var_name] <- value
  #}
  #SelecTargetArea <- input$AreaSlider
  #SelecTargetVisits <- input$VisitsSlider
  
  speciesMat <- do.call("data.frame", setNames(lapply(names(SpeciesListSelected),
                                                      function(x) bquote(rowMeans(SelectedSimMat * get(paste0(.(x), "MAT"))))),
                                               names(SpeciesListSelected)))
  speciesMatSD <- do.call("data.frame", setNames(lapply(names(SpeciesListSelectedSD),
                                                        function(x) bquote(sqrt(rowSums(SelectedSimMat * (get(paste0(.(x), "MAT"))^2) / length(SavedVecYearLoc))))),
                                                 names(SpeciesListSelectedSD)))
  #SelectedSimMat2 <- data.frame(SelectedSimMat,
  #                             Carbon = rowSums(SelectedSimMat * CarbonMAT),
  #                            # redsquirrel = rowMeans(SelectedSimMat * RedSquirrelMAT),
  #                           speciesMat,
  #                          Area = rowSums(SelectedSimMat * AreaMAT),
  #                         Visits = rowMeans(SelectedSimMat * (VisitsMAT)),
  #                        CarbonSD = sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
  #                       # redsquirrelSD = sqrt(rowSums(SelectedSimMat * (RedSquirrelSDMAT^2))) / length(SavedVec),
  #                      speciesMatSD,
  #                     VisitsSD = sqrt(rowSums(SelectedSimMat * (VisitsSDMAT^2))) / length(SavedVecLoc))
  SelectedSimMat2 <- data.frame(SelectedSimMat=SelectedSimMat,SelectedSimMatBinary=SelectedSimMatBinary,
                                SelectedSimMatYearORSavedVec=SelectedSimMatYearORSavedVec,
                                Carbon = rowSums(CarbonMATYearORSavedVec85),#rowSums(SelectedSimMat * CarbonMAT),
                                speciesMat,
                                Area = rowSums(SelectedSimMatBinary * AreaMAT),
                                Visits = rowMeans(SelectedSimMatBinary * (VisitsMAT)),
                                CarbonSD = sqrt(rowSums(CarbonSDMATYearORSavedVec85^2)),#sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
                                speciesMatSD,
                                VisitsSD = sqrt(rowSums(SelectedSimMatBinary * (VisitsSDMAT^2))) / length(SavedVecYearLoc))
  
  
  # for (specie_name in names(SpeciesListSelected)) {
  #   value <- rowMeans(SelectedSimMat * get(paste0(specie_name, "MAT")))
  #   SelectedSimMat2[specie_name] <- value
  # }
  # for (specie_name in names(SpeciesListSelectedSD)) {
  #   value <- sqrt(rowSums(SelectedSimMat * (get(paste0(specie_name, "MAT"))^2) / length(SavedVec)))
  #   SelectedSimMat2[specie_name] <- value
  # }
  
  tolvec <- c(abs(mean(SelectedSimMat2$Carbon)) / 150,
              abs(colMeans(speciesMat)) / 150,
              mean(SelectedSimMat2$Area) / 150,
              mean(SelectedSimMat2$Visits) / 150)
  for(i in 1:length(tolvec)) {
    # tolvec is a named vector, so tolvec[i] == 0 produces a named vector with the value, not the value directly
    # this causes a bug, isTRUE returns the boolean only
    if (isTRUE(tolvec[i] == 0)) {
      tolvec[i] <- 0.1
    }
  }
  # tolVec <- c(4, 0.05, 0.1, 2)
  #  Icalc <- MultiImpl(# TargetsVec = c(SelecTargetCarbon, SelecTargetBio, SelecTargetArea, SelecTargetVisits),
  #   TargetsVec = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetArea, SelecTargetVisits),
  #  EYMat = data.frame(SelectedSimMat2$Carbon, speciesMat, SelectedSimMat2$Area, SelectedSimMat2$Visits),
  # SDYMat = data.frame(SelectedSimMat2$CarbonSD, speciesMatSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
  #  alpha = alpha, tolVec = tolvec)
  
  LimitsMat <- (-data.frame(SelectedSimMat2$Carbon,
                            # SelectedSimMat2$redsquirrel,
                            speciesMat,
                            SelectedSimMat2$Area,
                            # SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + 4^2, SelectedSimMat2$redsquirrelSD^2 + 2^2, rep(0, length(SelectedSimMat2$Area)) + 100^2, SelectedSimMat2$VisitsSD + 2^2))
                            SelectedSimMat2$Visits)) / sqrt(data.frame(SelectedSimMat2$CarbonSD^2 + tolvec[1],
                                                                       speciesMatSD^2 + tolvec[1 + (1:ncol(speciesMat))],
                                                                       rep(0, length(SelectedSimMat2$Area)) + tolvec[length(tolvec) - 1],
                                                                       SelectedSimMat2$VisitsSD^2 + tolvec[length(tolvec)]))
  # we would want here the inverse truncated normal,
  
  CarbonMax<-
    
    
    max(0,trunc(sqrt(sum((CarbonSDMATYearORSavedVec[1,])^2) + tolvec[1])*qtruncnorm(p=1-alpha,a=LimitsMat[1,1],b=Inf,mean=0,sd=1)+SelectedSimMat2$Carbon[1]))
  bioMaxList<-list()
  for (ijj in 1:length(names(speciesMat)))
  {NM<-names(speciesMat)[ijj]
  NMSD<-names(speciesMatSD)[ijj]
  bioMaxList[[ijj]]<-max(0,trunc(sqrt(SelectedSimMat2[[NMSD]][1]^2 + tolvec[1+ijj])*qtruncnorm(p=1-alpha,a=LimitsMat[1,1+ijj],b=Inf,mean=0,sd=1)+SelectedSimMat2[[NM]][1]))
  
  }
  AreaMax<-max(0,trunc(1+sqrt( tolvec[dim(LimitsMat)[2]-1])*qtruncnorm(p=alpha,a=LimitsMat[1,dim(LimitsMat)[2]-1],b=Inf,mean=0,sd=1)+SelectedSimMat2$Area[1]))
  AreaMin<-max(0,trunc(1+sqrt( tolvec[dim(LimitsMat)[2]-1])*qtruncnorm(p=alpha,a=0,b=Inf,mean=0,sd=1)))
  VisistMax<-max(0,trunc(sqrt( SelectedSimMat2$VisitsSD[1]^2+tolvec[dim(LimitsMat)[2]])*qtruncnorm(p=1-alpha,a=LimitsMat[1,dim(LimitsMat)[2]],b=Inf,mean=0,sd=1)+SelectedSimMat2$Visits[1]))
  
  
  return(list(CarbonMax=CarbonMax,bioMaxList=bioMaxList,AreaMax=AreaMax,AreaMin=AreaMin,VisistMax=VisistMax,tolvec=tolvec))
}




check_targets_met <- function(PROBAMAT, target, nb_targets_met) {
  n_metrics <- ncol(PROBAMAT)
  # in case it is a vector
  combinations <- as.data.frame(t(combn(n_metrics, nb_targets_met)))
  # For some reason, Bertrand prefers it from n_metrics to 1, instead of 1 to n_metrics
  combinations <- as.data.frame(combinations[nrow(combinations):1, ])
  prob_list <- list()
  
  for (i in 1:nrow(combinations)) {
    condition <- rep(TRUE, nrow(PROBAMAT))
    for (j in 1:n_metrics) {
      if (j %in% combinations[i, ]) {
        condition <- condition & (PROBAMAT[, j] >= target)
      } else {
        condition <- condition & (PROBAMAT[, j] < target)
      }
    }
    prob_list[[i]] <- condition
  }
  return(prob_list)
}

subset_meet_targets <- function(PROBAMAT, SelectedSimMat2, CONDPROBAPositiveLIST, TARGETS, nb_targets_met) {
  n_metrics <- ncol(PROBAMAT)
  targets_met <- t(combn(n_metrics, nb_targets_met))
  if (nb_targets_met != n_metrics) {
    targets_met <- as.matrix(targets_met[nrow(targets_met):1, ])
  }
  
  SubsetMeetTargets <- data.frame()
  for (i in 1:nrow(targets_met)) {
    combination <- targets_met[i, ]
    if (length(CONDPROBAPositiveLIST[[i]]) != 0) {
      SubsetMeetTargets <- rbind(SubsetMeetTargets,
                                 data.frame(SelectedSimMat2[CONDPROBAPositiveLIST[[i]], ],
                                            Met = rep(paste(TARGETS[combination], collapse = ","),
                                                      sum(CONDPROBAPositiveLIST[[i]])),
                                            NotMet = rep(paste(TARGETS[-combination], collapse = ","),
                                                         sum(CONDPROBAPositiveLIST[[i]]))))
    }
  }
  return(SubsetMeetTargets)
}

# The richness are average probabilities for a group of species to appear in a parcel
add_richness_columns <- function(FullTable, groups, maxyear, NAME_CONVERSION, SCENARIO = 26) {
  # Convert from sf to tibble
  FullTable2 <- FullTable %>%
    sf::st_drop_geometry() %>%
    dplyr::select(starts_with("Bio_Mean") &
                    contains(paste0("Scenario", SCENARIO))) %>%
    mutate(parcel_id = 1:nrow(FullTable)) |>
    data.table::setDT()
  
  # Compute the biodiversity
  # colnames_to_find <- paste0("BioMean_", species_in_group)
  # col_indices_of_group <- which(colnames(FullTable2) %in% colnames_to_find)
  
  # Biodiversity for planting (year 0)
  small_fulltable_dt <- FullTable2 %>%
    dplyr::select(contains("_Planting"), parcel_id) %>%
    data.table::setDT()
  # Melt the data table to convert from wide to long format
  melted_dt <- data.table::melt(small_fulltable_dt, 
                                id.vars = c("parcel_id"), 
                                measure.vars = grep("_Planting", colnames(small_fulltable_dt), value = TRUE), 
                                variable.name = "column_name", 
                                value.name = "biodiversity")
  # Extract the species name from the column names (e.g., 'Alauda_arvensis', 'Carex_magellanica')
  melted_dt <- melted_dt[, colname_specie := gsub(".*BioSpecie(.*)_Scenario.*", "\\1", column_name)]
  # Extract the column name's tree specie
  melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*)_.*", "\\1", column_name)]
  # Extract the column name's scenario
  melted_dt <- melted_dt[, colname_scenario := gsub(".*Scenario(.*?)_.*", "\\1", column_name)]
  # Re-order by parcel_id
  melted_dt <- data.table::setorder(melted_dt, parcel_id)
  # Long to wide
  biodiversity_planting <- data.table::dcast(melted_dt,
                                             parcel_id ~ paste0("Bio_Mean_BioSpecie", colname_specie, "_TreeSpecie", colname_treespecie, "_Scenario", colname_scenario, "_Planting"),
                                             value.var = "biodiversity",
                                             sep = "")
  # Remove parcel_id
  biodiversity_planting <- biodiversity_planting[, parcel_id := NULL]
  
  
  # Biodiversity for no_planting (year maxyear)
  small_fulltable_dt <- FullTable2 %>%
    dplyr::select(contains("_NoPlanting"), parcel_id) %>%
    data.table::setDT()
  # Melt the data table to convert from wide to long format
  melted_dt <- data.table::melt(small_fulltable_dt, 
                                id.vars = c("parcel_id"), 
                                measure.vars = grep("_NoPlanting", colnames(small_fulltable_dt), value = TRUE), 
                                variable.name = "column_name", 
                                value.name = "biodiversity")
  # Extract the species name from the column names (e.g., 'Alauda_arvensis', 'Carex_magellanica')
  melted_dt <- melted_dt[, colname_specie := gsub(".*BioSpecie(.*)_Scenario.*", "\\1", column_name)]
  # Extract the column name's tree specie
  melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*)_.*", "\\1", column_name)]
  # Extract the column name's scenario
  melted_dt <- melted_dt[, colname_scenario := gsub(".*Scenario(.*?)_.*", "\\1", column_name)]
  # Re-order by parcel_id
  melted_dt <- data.table::setorder(melted_dt, parcel_id)
  # Long to wide
  biodiversity_no_planting <- data.table::dcast(melted_dt,
                                                parcel_id ~ paste0("Bio_Mean_BioSpecie", colname_specie, "_TreeSpecie", colname_treespecie, "_Scenario", colname_scenario, "_NoPlanting"),
                                                value.var = "biodiversity",
                                                sep = "")
  # Remove parcel_id
  biodiversity_no_planting <- biodiversity_no_planting[, parcel_id := NULL]
  
  
  
  # Add an artifical group for all species
  # unique_groups <- c(unique(NAME_CONVERSION$Group), "All")
  unique_groups <- groups
  
  richnesses <- data.table()
  for (group in unique_groups) {
    if (group == "All") {
      species_in_group <- NAME_CONVERSION$Specie
    } else {
      species_in_group <- NAME_CONVERSION[NAME_CONVERSION$Group == group, "Specie"]
    }
    
    for (year in 0:(maxyear+1)) {
      
      for (treespecie in c("Conifers", "Deciduous")) {
        mypattern <- paste0("TreeSpecie", treespecie)
        biodiversity_planting_treespecie <- biodiversity_planting[, .SD, .SDcols = grep(mypattern, colnames(biodiversity_planting), value = TRUE)]
        biodiversity_no_planting_treespecie <- biodiversity_no_planting[, .SD, .SDcols = grep(mypattern, colnames(biodiversity_no_planting), value = TRUE)]
        
        richness <- calculate_richness(group = group,
                                       all_biodiversity_planting = biodiversity_planting_treespecie,
                                       all_biodiversity_no_planting = biodiversity_no_planting_treespecie,
                                       MAXYEAR = maxyear,
                                       year_of_planting_from_0 = rep(year, nrow(biodiversity_no_planting_treespecie)),
                                       NAME_CONVERSION = NAME_CONVERSION)
        colnames(richness) <- paste0(colnames(richness), "_PlantingYear", year)
        richnesses <- cbind(richnesses, richness)
      }
      
    }
  }
  rm(FullTable2)
  # Convert back to sf object
  FullTable <- cbind(FullTable, richnesses)
  return(FullTable)
}

get_regressed_biodiversity <- function(biodiversity_planting,
                                       biodiversity_no_planting,
                                       MAXYEAR,
                                       year_of_planting_from_0,
                                       # Get the difference in probability (planting - not_planting)
                                       difference = FALSE) {
  # Fit a line on (year_planting, biodiversity_planting) from (0, biodiversity_planting) to (MAXYEAR+1, biodiversity_no_planting)
  
  # tibbles are lists, but cause warnings when substracting one from another
  if ("tbl" %in% class(biodiversity_planting) &&
      typeof(biodiversity_planting) == "list") {
    biodiversity_planting <- as.data.frame(biodiversity_planting)
    biodiversity_no_planting <- as.data.frame(biodiversity_no_planting)
  }
  
  # Calculate the slope
  slope <- (biodiversity_no_planting - biodiversity_planting) / (MAXYEAR - 0)
  
  # The intercept is simply the planting value at X = 0
  intercept <- biodiversity_planting
  
  # Compute the regressed value at year_of_planting_from_0
  result <- intercept + slope * year_of_planting_from_0
  
  # We want the change: value if we plant - value if we never plant
  if (isTRUE(difference)) {
    result <- result - biodiversity_no_planting
  }
  
  # Return the regressed value
  return(result)
}

calculate_richness <- function(group,
                               all_biodiversity_planting,
                               all_biodiversity_no_planting,
                               MAXYEAR,
                               year_of_planting_from_0,
                               NAME_CONVERSION) {
  if (group == "All") {
    species_in_group <- NAME_CONVERSION$Specie
  } else {
    species_in_group <- NAME_CONVERSION$Specie[NAME_CONVERSION$Group == group]
  }
  
  species_pattern <- paste0(".*BioSpecie(", paste0("", species_in_group, collapse = "|"), ").*")
  
  # Select columns of species in the group when planting
  mytable_planting <- all_biodiversity_planting %>%
    dplyr::select(contains("Bio") &
                    contains("Mean") &
                    matches(species_pattern))
  
  # Select columns of species in the group when not planting
  mytable_noplanting <- all_biodiversity_no_planting %>%
    dplyr::select(contains("Bio") &
                    contains("Mean") &
                    matches(species_pattern))
  
  # Regress to find the biodiversity values for the planting year strategy
  biodiversity_evaluated <- get_regressed_biodiversity(biodiversity_planting = mytable_planting,
                                                       biodiversity_no_planting = mytable_noplanting,
                                                       MAXYEAR = MAXYEAR,
                                                       year_of_planting_from_0 = year_of_planting_from_0)
  
  len_col_indices_of_group <- ncol(biodiversity_evaluated)
  
  if (len_col_indices_of_group == 1) {
    result <- data.table(richness = round(biodiversity_evaluated, 2))
  } else {
    result <- data.table(
      # Original method for species richness
      # richness = round(rowSums(biodiversity_evaluated)))
      # We prefer to use an average because a large group would naturally have a much higher richness than a small group, and we don't want that
      richness = round(rowSums(biodiversity_evaluated) / len_col_indices_of_group, 2))
  }
  colnames(result) <- gsub(pattern = "Bio_Mean_BioSpecie.*(_TreeSpecie.*?_Scenario.*)(_Planting)",
                           replacement = paste0("Richness_Group", group, "\\1"),
                           x = colnames(mytable_planting)[1])
  
  result <- result |> setDT()
  
  return(result)
}

get_richness_from_fulltable <- function(strategy_vector, FullTable_arg = FullTable) {
  FullTable <- FullTable_arg
  
  group_size <- length(strategy_vector) / 3
  
  indices <- 1:group_size
  area_vector <- as.numeric(strategy_vector[indices])
  indices <- indices + group_size
  year_vector <- as.numeric(strategy_vector[indices])
  indices <- indices + group_size
  treespecie_vector <- strategy_vector[indices]
  
  small_fulltable_dt <- FullTable %>%
    mutate(area = area_vector,
           year = year_vector,
           treespecie = treespecie_vector) %>%
    dplyr::select(area, year, treespecie, starts_with("Richness")) %>%
    mutate(parcel_id = 1:nrow(FullTable)) |>
    setDT()
  
  # Melt the data table to convert from wide to long format
  melted_dt <- data.table::melt(small_fulltable_dt, 
                    id.vars = c("area", "year", "treespecie", "parcel_id"), 
                    measure.vars = grep("Richness", colnames(small_fulltable_dt), value = TRUE),
                    variable.name = "column_name", 
                    value.name = "richness")
  
  # Extract the column name's tree specie
  melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*?)_.*", "\\1", column_name)]
  # Extract the column name's planting year
  melted_dt <- melted_dt[, colname_plantingyear := gsub(".*PlantingYear(.*?)", "\\1", column_name)]
  # Extract the column name's richness group
  melted_dt <- melted_dt[, colname_group := gsub(".*Group(.*?)_.*", "\\1", column_name)]
  
  # Remove rows where the column_name's tree specie is not the strategy treespecie
  # and where the planting year is not the strategy plantingyear
  # and where we don't plant (area is 0)
  # and where plantingyear is 25
  melted_dt <- melted_dt[colname_treespecie == treespecie &
                           colname_plantingyear == year &
                           area != 0 &
                           colname_plantingyear != MAXYEAR + 1, ]
  
  # Sum richness by group
  tmp <- melted_dt[, .(total_richness = sum(richness)), by = colname_group]
  richness <- tmp$total_richness
  names(richness) <- tmp$colname_group
  
  return(richness)
}

get_outcomes_from_strategy <- function(parameter_vector, FullTable_arg = FullTable, SCENARIO_ARG = SCENARIO, MAXYEAR_ARG = MAXYEAR) {
  SCENARIO <- SCENARIO_ARG
  FullTable <- FullTable_arg
  MAXYEAR <- MAXYEAR_ARG
  
  
  # Prepare data once to avoid re-doing it everywhere ----
  FullTable <- FullTable %>%
    sf::st_drop_geometry() %>%
    dplyr::select(!contains("Bio_SD"), -extent, -x, -y)
  
  
  
  
  # Area ----
  group_size <- length(parameter_vector) / 3
  indices <- 1:group_size
  area_vector <- as.numeric(parameter_vector[indices])
  indices <- indices + group_size
  year_vector <- as.numeric(parameter_vector[indices])
  indices <- indices + group_size
  treespecie_vector <- parameter_vector[indices]
  
  FullTable <- FullTable %>%
    mutate(area = area_vector,
           year = year_vector,
           treespecie = treespecie_vector)
  
  # Calculate outcomes (sumCarbon, sumArea, sumBiodiversity, sumVisits)
  
  
  
  # Carbon ----
  small_fulltable <- FullTable %>%
    dplyr::select(area, year, treespecie,
                  (
                    contains("Carbon") &
                      contains(c("Mean", "SD")) &
                      contains(paste0("Scenario", SCENARIO)) &
                      contains("PlantingYear")
                  ))
  
  # sum_carbon <- small_fulltable %>%
  #   dplyr::select(contains(c("area", "year", "treespecie", "Mean"), ignore.case = FALSE)) %>%
  #   rowwise() %>%
  #   mutate(
  #     # Check if area is 0 and ignore the row by setting sum to 0 if true
  #     # Also set the value to 0 if the year of planting is less than the minimum specified in year_vector
  #     # or if the tree specie is the wrong one
  #     final_value = ifelse(area == 0,
  #                          0,
  #                          across(contains("PlantingYear"),
  #                                 ~ ifelse(
  #                                   # Correct planting year
  #                                   (as.numeric(sub(".*PlantingYear([0-9]+).*",
  #                                                   "\\1",
  #                                                   cur_column())) == year) &
  #                                     # Correct tree specie
  #                                     (sub(".*TreeSpecie(.*?)_PlantingYear.*",
  #                                          "\\1",
  #                                          cur_column()) == treespecie)
  #                                   ,
  #                                   .,
  #                                   0)
  #                          ) %>%
  #                            sum())
  #   ) %>%
  #   ungroup() %>%
  #   dplyr::select(final_value) %>%
  #   sum()
  
  small_fulltable_dt <- small_fulltable %>%
    dplyr::select(area, year, treespecie, contains("Mean")) |>
    setDT()
  
  melted_dt <- data.table::melt(small_fulltable_dt,
                                id.vars = c("area", "year", "treespecie"),
                                measure.vars = patterns("PlantingYear"),
                                variable.name = "carbon_column",
                                value.name = "carbon_value")
  
  # Extract planting year and tree specie from the column names
  melted_dt[, col_planting_year := as.numeric(sub(".*PlantingYear([0-9]+).*", "\\1", carbon_column))]
  melted_dt[, col_treespecie := sub(".*TreeSpecie(.*?)_PlantingYear.*", "\\1", carbon_column)]
  
  # Apply the conditions and calculate the sum
  sum_carbon <- melted_dt[
    # Filter rows where area is non-zero and conditions on planting year and species match
    area != 0 & col_planting_year == year & col_treespecie == treespecie,
    sum(carbon_value, na.rm = TRUE),  # Summing the matching carbon values
    by = .(area, year, treespecie)    # Summing for each combination of area, year, and treespecie
  ]$V1 |> sum()  # Extract the result directly
  
  
  
  
  
  # Carbon SD (similar to above) ----
  small_fulltable_dt <- small_fulltable %>%
    dplyr::select(area, year, treespecie, contains("SD")) |>
    setDT()
  
  melted_dt <- data.table::melt(small_fulltable_dt,
                                id.vars = c("area", "year", "treespecie"),
                                measure.vars = patterns("PlantingYear"),
                                variable.name = "carbon_column",
                                value.name = "carbon_value")
  
  # Extract planting year and tree specie from the column names
  melted_dt[, col_planting_year := as.numeric(sub(".*PlantingYear([0-9]+).*", "\\1", carbon_column))]
  melted_dt[, col_treespecie := sub(".*TreeSpecie(.*?)_PlantingYear.*", "\\1", carbon_column)]
  
  # Apply the conditions and calculate the sum
  sum_carbon_sd <- melted_dt[
    # Filter rows where area is non-zero and conditions on planting year and species match
    area != 0 & col_planting_year == year & col_treespecie == treespecie,
    sum(carbon_value, na.rm = TRUE),  # Summing the matching carbon values
    by = .(area, year, treespecie)    # Summing for each combination of area, year, and treespecie
  ]$V1 # Extract the result directly
  sum_carbon_sd <- sqrt(sum(sum_carbon_sd^2))
  
  
  
  
  
  # Visits ----
  sum_visits <- sum(FullTable[area_vector != 0, "VisitsMean"])
  sum_visits_sd <- sqrt(sum(FullTable[area_vector != 0, "VisitsMean"]^2))
  
  
  
  
  # Biodiversity ----
  species_indices <- which(SPECIES %in% NAME_CONVERSION$Specie)
  if (length(species_indices) == 0) {
    sum_biodiversity <- sum_biodiversity_sd <- c()
  } else {
    species_pattern <- paste0(".*Specie(", paste0(SPECIES[species_indices], collapse = "|"), ").*")
    
    small_fulltable <- FullTable %>%
      dplyr::select(area, year, treespecie,
                    (
                      matches(species_pattern) &
                        contains("Bio_Mean") &
                        contains(paste0("Scenario", SCENARIO))
                    ))
    
    # biodiversity_planting <- small_fulltable %>%
    #   dplyr::select(area, year, treespecie, contains("_Planting")) %>%
    #   rowwise() %>%
    #   mutate(
    #     # Check if area is 0 and ignore the row by setting sum to 0 if true
    #     # Also set the value to 0 if the tree specie is the wrong one
    #     final_value = ifelse(area == 0,
    #                          0,
    #                          across(contains("Bio_Mean"),
    #                                 ~ ifelse(
    #                                   # Correct tree specie
    #                                   sub(".*TreeSpecie(.*?)_.*",
    #                                       "\\1",
    #                                       cur_column()) == treespecie,
    #                                   .,
    #                                   0)
    #                          ) %>%
    #                            sum())
    #   ) %>%
    #   ungroup() %>%
    #   dplyr::select(final_value)
    
    
    
    
    
    ## Biodiversity per specie per parcel, when planting ----
    small_fulltable_dt <- small_fulltable %>%
      dplyr::select(area, year, treespecie, contains("_Planting")) %>%
      mutate(parcel_id = 1:nrow(FullTable)) |>
      setDT()
    
    # Melt the data table to convert from wide to long format
    melted_dt <- data.table::melt(small_fulltable_dt, 
                      id.vars = c("area", "year", "treespecie", "parcel_id"), 
                      measure.vars = grep("_Planting", colnames(small_fulltable_dt), value = TRUE), 
                      variable.name = "column_name", 
                      value.name = "biodiversity")
    
    # Extract the species name from the column names (e.g., 'Alauda_arvensis', 'Carex_magellanica')
    melted_dt <- melted_dt[, colname_specie := gsub(".*BioSpecie(.*?)_Scenario.*", "\\1", column_name)]
    # Extract the column name's tree specie
    melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*?)_.*", "\\1", column_name)]
    
    # Remove rows where the column_name's tree specie is not the strategy treespecie
    melted_dt <- melted_dt[colname_treespecie == treespecie, ]
    
    biodiversity_planting <- dcast(melted_dt, parcel_id ~ colname_specie, value.var = "biodiversity")
    
    # Re-order by parcel_id
    biodiversity_planting <- setorder(biodiversity_planting, parcel_id)
    # Remove parcel_id
    biodiversity_planting[, parcel_id := NULL]
    
    
    
    
    ## Biodiversity per specie per parcel, when NOT planting ----
    small_fulltable_dt <- small_fulltable %>%
      dplyr::select(area, year, treespecie, contains("_NoPlanting")) %>%
      mutate(parcel_id = 1:nrow(FullTable)) |>
      setDT()
    
    # Melt the data table to convert from wide to long format
    melted_dt <- data.table::melt(small_fulltable_dt, 
                      id.vars = c("area", "year", "treespecie", "parcel_id"), 
                      measure.vars = grep("_NoPlanting", colnames(small_fulltable_dt), value = TRUE), 
                      variable.name = "column_name", 
                      value.name = "biodiversity")
    
    # Extract the species name from the column names (e.g., 'Alauda_arvensis', 'Carex_magellanica')
    melted_dt <- melted_dt[, colname_specie := gsub(".*BioSpecie(.*?)_Scenario.*", "\\1", column_name)]
    # Extract the column name's tree specie
    melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*?)_.*", "\\1", column_name)]
    
    # Remove rows where the column_name's tree specie is not the strategy treespecie
    melted_dt <- melted_dt[colname_treespecie == treespecie, ]
    
    # Re-order by parcel_id
    melted_dt <- setorder(melted_dt, parcel_id)
    
    biodiversity_no_planting <- dcast(melted_dt, parcel_id ~ colname_specie, value.var = "biodiversity")
    
    # Re-order by parcel_id
    biodiversity_no_planting <- setorder(biodiversity_no_planting, parcel_id)
    # Remove parcel_id
    biodiversity_no_planting[, parcel_id := NULL]
    
    
    
    
    # Biodiversity total ----
    sum_biodiversity <- get_regressed_biodiversity(biodiversity_planting = biodiversity_planting,
                                                   biodiversity_no_planting = biodiversity_no_planting,
                                                   MAXYEAR = MAXYEAR,
                                                   year_of_planting_from_0 = year_vector) |>
      colSums()
    
    # Biodiversity SD
    sum_biodiversity_sd <- 0
    sum_biodiversity_sd <- sqrt(sum(sum_biodiversity_sd^2))
  }
  
  
  
  
  # Richness ----
  # For now, the code below only works for the 2 tree species Conifers and Deciduous. We will have a new biodiversity model later on, so it's not important
  richness <- c()
  
  # # apply_biodiversity_logic <- function(specie, SCENARIO, all_biodiversity_planting, all_biodiversity_no_planting) {
  # #   
  # #   # Create column names for both the 'Planting' and 'NoPlanting' cases
  # #   conifer_col_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_Planting")
  # #   deciduous_col_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_Planting")
  # #   new_col_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_Planting")
  # #   
  # #   conifer_col_no_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_NoPlanting")
  # #   deciduous_col_no_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_NoPlanting")
  # #   new_col_no_planting <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_NoPlanting")
  # #   
  # #   # Apply the logic for 'Planting'
  # #   all_biodiversity_planting[, (new_col_planting) := fifelse(treespecie == "Conifers",
  # #                                                             get(conifer_col_planting),
  # #                                                             get(deciduous_col_planting))]
  # #   
  # #   # Apply the logic for 'NoPlanting'
  # #   all_biodiversity_no_planting[, (new_col_no_planting) := fifelse(treespecie == "Conifers",
  # #                                                                   get(conifer_col_no_planting),
  # #                                                                   get(deciduous_col_no_planting))]
  # #   
  # #   # Remove the original columns after applying the logic
  # #   all_biodiversity_planting[, c(conifer_col_planting, deciduous_col_planting) := NULL]
  # #   all_biodiversity_no_planting[, c(conifer_col_no_planting, deciduous_col_no_planting) := NULL]
  # #   
  # #   # Return the updated data.tables
  # #   return(list(all_biodiversity_planting = all_biodiversity_planting,
  # #               all_biodiversity_no_planting = all_biodiversity_no_planting))
  # # }
  # 
  # small_fulltable <- FullTable %>%
  #   dplyr::select(area, year, treespecie,
  #                 contains("Bio_Mean") &
  #                   contains(paste0("Scenario", SCENARIO))
  #   ) %>%
  #   mutate(parcel_id = 1:nrow(FullTable))
  # for (group in SPECIES) {
  #   if (group == "All" || group %in% NAME_CONVERSION$Group) {
  #     
  #     if (group == "All") {
  #       species_in_group <- NAME_CONVERSION$Specie
  #     } else {
  #       species_in_group <- NAME_CONVERSION$Specie[NAME_CONVERSION$Group == group]
  #     }
  #     
  #     species_pattern <- paste0(".*BioSpecie(", paste0(species_in_group, collapse = "|"), ").*")
  #     
  #     # Table with biodiversity values per parcel where we plant the correct tree specie
  #     all_biodiversity_planting <- small_fulltable %>%
  #       dplyr::select(parcel_id, treespecie,
  #                     contains("_Planting") &
  #                       matches(species_pattern)) %>%
  #       setDT()
  #     
  #     # Table with biodiversity values per parcels where we don't plant the correct tree specie
  #     all_biodiversity_no_planting <- small_fulltable %>%
  #       dplyr::select(parcel_id, treespecie, 
  #                     contains("_NoPlanting") &
  #                       matches(species_pattern)) %>%
  #       setDT()
  #     
  #     # Create a new column with value equal to the correct tree specie
  #     for (specie in species_in_group) {
  #       # When planting
  #       conifer_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_Planting")
  #       deciduous_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_Planting")
  #       new_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_Planting")
  #       
  #       all_biodiversity_planting[, (new_col) := fifelse(treespecie == "Conifers",
  #                                                        get(conifer_col),
  #                                                        get(deciduous_col))]
  #       all_biodiversity_planting[, c(conifer_col, deciduous_col) := NULL]
  #       
  #       # When not planting
  #       conifer_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Conifers", "_NoPlanting")
  #       deciduous_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_TreeSpecie", "Deciduous", "_NoPlanting")
  #       new_col <- paste0("Bio_Mean_BioSpecie", specie, "_Scenario", SCENARIO, "_NoPlanting")
  #       
  #       all_biodiversity_no_planting[, (new_col) := fifelse(treespecie == "Conifers",
  #                                                           get(conifer_col),
  #                                                           get(deciduous_col))]
  #       all_biodiversity_no_planting[, c(conifer_col, deciduous_col) := NULL]
  #     }
  #     
  #     # Re-order by parcel_id
  #     all_biodiversity_planting <- setorder(all_biodiversity_planting, parcel_id)
  #     all_biodiversity_no_planting <- setorder(all_biodiversity_no_planting, parcel_id)
  #     
  #     # Remove useless columns
  #     all_biodiversity_planting <- all_biodiversity_planting[, c("parcel_id", "treespecie") := NULL]
  #     all_biodiversity_no_planting <- all_biodiversity_no_planting[, c("parcel_id", "treespecie"):= NULL]
  # 
  #     # # Apply the function over all species in the group and get the updated data.tables
  #     # result <- lapply(species_in_group, apply_biodiversity_logic, SCENARIO = SCENARIO,
  #     #                  all_biodiversity_planting = all_biodiversity_planting,
  #     #                  all_biodiversity_no_planting = all_biodiversity_no_planting)
  # 
  #     # Extract updated results from the list
  #     # all_biodiversity_planting <- result[[length(result)]]$all_biodiversity_planting
  #     # all_biodiversity_no_planting <- result[[length(result)]]$all_biodiversity_no_planting
  
  # tmp <- get_richness(group = group,
  #                     all_biodiversity_planting = all_biodiversity_planting,
  #                     all_biodiversity_no_planting = all_biodiversity_no_planting,
  #                     MAXYEAR = MAXYEAR,
  #                     year_of_planting_from_0 = year_vector,
  #                     NAME_CONVERSION = NAME_CONVERSION)
  
  
  
  small_fulltable_dt <- FullTable %>%
    dplyr::select(area, year, treespecie, starts_with("Richness")) %>%
    mutate(parcel_id = 1:nrow(FullTable)) |>
    setDT()
  
  # Melt the data table to convert from wide to long format
  melted_dt <- data.table::melt(small_fulltable_dt, 
                    id.vars = c("area", "year", "treespecie", "parcel_id"), 
                    measure.vars = grep("Richness", colnames(small_fulltable_dt), value = TRUE),
                    variable.name = "column_name", 
                    value.name = "richness")
  
  # Extract the column name's tree specie
  melted_dt <- melted_dt[, colname_treespecie := gsub(".*TreeSpecie(.*?)_.*", "\\1", column_name)]
  # Extract the column name's planting year
  melted_dt <- melted_dt[, colname_plantingyear := gsub(".*PlantingYear(.*?)", "\\1", column_name)]
  # Extract the column name's richness group
  melted_dt <- melted_dt[, colname_group := gsub(".*Group(.*?)_.*", "\\1", column_name)]
  
  # Remove rows where the column_name's tree specie is not the strategy treespecie
  # and where the planting year is not the strategy plantingyear
  # and where we don't plant (area is 0)
  # and where plantingyear is 25
  melted_dt <- melted_dt[colname_treespecie == treespecie &
                           colname_plantingyear == year &
                           area != 0 &
                           colname_plantingyear != MAXYEAR + 1, ]
  
  # Sum richness by group
  tmp <- melted_dt[, .(total_richness = sum(richness)), by = colname_group]
  richness <- tmp$total_richness
  names(richness) <- tmp$colname_group
  
  # End ----
  
  return(list("sum_area" = sum(area_vector),
              "sum_carbon" = sum_carbon,
              "sum_carbon_sd" = sum_carbon_sd,
              "richness" = richness,
              "sum_biodiversity" = sum_biodiversity,
              "sum_biodiversity_sd" = sum_biodiversity_sd,
              "sum_visits" = sum_visits,
              "sum_visits_sd" = sum_visits_sd
  ))
}

convert_bio_to_polygons_from_elicitor_and_merge_into_FullTable <- function(Elicitor_table,seer2km,speciesprob_list,jncc100,climatecells, MAXYEAR = MAXYEAR, limit_log_level = LOG_LEVEL) {
  
  # Take the Biodiversity probabilities from Matlab
  # and merge them with BristolFullTableMerged.geojson
  # JulesOP/<baseline/scenario>_species_prob_40_unmanaged_<conifers/deciduous>.csv
  
  # Load the shapefile mapping new2kid with Polygons
  id_polygons <- seer2km %>% dplyr::select(c(new2kid, geometry))
  
  # How many biodiversity tables to load
  biodiversity_tables_number <- length(speciesprob_list)
  
  # Load new2kid
  new2kid <- climatecells$new2kid
  
  # Load species names
  all_species_names <- colnames(jncc100)[-1]
  biodiversity <- c()
  
  # Fit a line and load values into a table
  tree_species <- unique(sapply(speciesprob_list, function(x) return(x$tree_specie)))
  for (tree_specie in tree_species) {
    table_planting_idx <- sapply(speciesprob_list, function(x) isTRUE((x$tree_specie == tree_specie) && isTRUE(x$planting)))
    table_no_planting_idx <- sapply(speciesprob_list, function(x) isTRUE((x$tree_specie == tree_specie) && isFALSE(x$planting)))
    
    table_planting <- speciesprob_list[table_planting_idx][[1]]$table
    colnames(table_planting) <- all_species_names
    table_planting <- table_planting %>%
      dplyr::rename_with(~ paste0("Bio_Mean_BioSpecie", .x,
                                  "_Scenario", 26,
                                  "_TreeSpecie", tree_specie,
                                  "_Planting"))
    
    table_no_planting <- speciesprob_list[table_no_planting_idx][[1]]$table
    colnames(table_no_planting) <- all_species_names
    table_no_planting <- table_no_planting %>%
      dplyr::rename_with(~ paste0("Bio_Mean_BioSpecie", .x,
                                  "_Scenario", 26,
                                  "_TreeSpecie", tree_specie,
                                  "_NoPlanting"))
    
    # Column names are 
    biodiversity <- biodiversity %>%
      bind_cols(table_planting,
                table_no_planting)
  }
  
  # Replace NAs with column mean
  biodiversity <- biodiversity %>%
    mutate(across(everything(), ~ replace(.x,
                                          is.na(.x),
                                          mean(.x, na.rm = TRUE))))
  
  # Read the GeoJSON polygon layer
  polygons_jules <- Elicitor_table %>%
    # Duplicate the geometry column
    dplyr::mutate(geometry_jules = geometry) %>%
    # # Only keep useful columns
    dplyr::select(-starts_with("Bio"))
  # select(c(JulesMean, JulesSD,
  #          VisitsMean, VisitsSD,
  #          geometry, geometry_jules))
  
  # Add new2kid to each data.frame, and change into percentages
  polygons_bio <- tibble(new2kid) %>%
    bind_cols(100 * biodiversity) %>%
    # Add polygons
    dplyr::left_join(id_polygons, by = "new2kid") %>%
    # Transform to sf object
    st_as_sf() %>%
    # Convert the CRS
    st_transform(crs = st_crs(polygons_jules)) %>%
    # Duplicate the geometry column
    dplyr::mutate(geometry_bio = geometry)
  
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
  msg <- "Intersecting biodiversity square cells and the shapefile's polygons ..."
  notif(msg, limit_log_level = limit_log_level)
  intersection <- st_intersection(polygons_bio, polygons_jules)
  notif(paste(msg, "done"), limit_log_level = limit_log_level)
  
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
    dplyr::mutate(area_bio = st_area(geometry_bio),
                  area_jules = st_area(geometry_jules),
                  area_intersection = st_area(geometry)) %>%
    
    # Calculate the ratio of areas
    dplyr::mutate(proportion_intersection_in_bio = area_intersection / area_bio,
                  proportion_intersection_in_jules = area_intersection / area_jules) %>%
    
    # Assume uniformity, multiply probability and standard deviations by proportion
    dplyr::mutate(dplyr::across(starts_with("Bio"),
                                ~ .x * proportion_intersection_in_bio)) %>%
    
    # Reduce to 100(%) if value is above
    # 100 and the columns need to be in the same unit
    dplyr::mutate(dplyr::across(starts_with("Bio"),
                                ~ pmin(.x, units::as_units(100, units(.x))))) %>%
    
    as_tibble() %>%
    
    # Group by polygon_id_jules
    group_by(polygon_id_jules) %>%
    summarise(
      # For each polygon from Jules, sum over (pre-weighted) probabilities from areas that
      # intersect and divide by the sum of proportions, to get a weighted average
      across(starts_with("Bio_Mean"),
             ~ sum(.x) / sum(proportion_intersection_in_bio)),
      
      geometry_union = st_union(geometry),
      geometry_jules = st_union(geometry_jules),
      
      polygon_id_jules = mean(polygon_id_jules)
    ) %>%
    
    dplyr::mutate(area_diff = map2_dbl(geometry_union, geometry_jules, compute_difference_area)) %>%
    
    # Remove useless columns
    as_tibble() %>%
    dplyr::select(c(starts_with("Bio"), polygon_id_jules, area_diff)) %>%
    
    # Merge back to original table
    left_join(polygons_jules, by = "polygon_id_jules") %>%
    
    dplyr::select(-c(polygon_id_jules, starts_with("geometry_")))
  
  # Re-order
  FullTable <- bind_cols(FullTable %>%
                           dplyr::select(!starts_with("Bio_Mean")),
                         FullTable %>%
                           dplyr::select(starts_with("Bio_Mean")))
  
  # Add Bio_SD columns set to 0
  FullTableBioSD <- FullTable %>%
    dplyr::select(starts_with("Bio")) %>%
    dplyr::rename_with(~ gsub("Bio_Mean", "Bio_SD", .x)) %>%
    dplyr::mutate(across(everything(), ~ 0))
  FullTable <- FullTable %>%
    bind_cols(FullTableBioSD)
  
  if (any(FullTable$area_diff >= 1)) {
    msg <- "The merged geometries from the intersections do not sum the ones intersected with the elicitor (jules): more than 1km square difference"
    notif(msg, log_level = "warning", limit_log_level = limit_log_level)
  }
  
  FullTable <- FullTable %>% dplyr::select(-area_diff) %>% st_as_sf()
  
  return(FullTable)
}

get_pretty_specie <- function(ugly_specie, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (ugly_specie %in% NAME_CONVERSION_ARG$Specie_pretty) return(ugly_specie)
  idx <- which(NAME_CONVERSION_ARG$Specie == ugly_specie)
  if (length(idx) == 0) return(ugly_specie)
  result <- NAME_CONVERSION_ARG$Specie_pretty[idx]
  return(result)
}

get_ugly_specie <- function(pretty_specie, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (pretty_specie %in% NAME_CONVERSION_ARG$Specie) return(pretty_specie)
  idx <- which(NAME_CONVERSION_ARG$Specie_pretty == pretty_specie)
  if (length(idx) == 0) return(pretty_specie)
  result <- NAME_CONVERSION_ARG$Specie[idx]
  return(result)
}

get_pretty_group <- function(ugly_group, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (ugly_group %in% NAME_CONVERSION_ARG$Group_pretty) return(ugly_group)
  idx <- which(NAME_CONVERSION_ARG$Group == ugly_group)
  if (length(idx) == 0) return(ugly_group)
  result <- NAME_CONVERSION_ARG$Group_pretty[idx[1]]
  return(result)
}

get_ugly_group <- function(pretty_group, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (pretty_group %in% NAME_CONVERSION_ARG$Group) return(pretty_group)
  idx <- which(NAME_CONVERSION_ARG$Group_pretty == pretty_group)
  if (length(idx) == 0) return(pretty_group)
  result <- NAME_CONVERSION_ARG$Group[idx[1]]
  return(result)
}

get_pretty_english_specie <- function(ugly_english_specie, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (ugly_english_specie %in% NAME_CONVERSION_ARG$English_specie_pretty) return(ugly_english_specie)
  idx <- which(NAME_CONVERSION_ARG$English_specie == ugly_english_specie)
  if (length(idx) == 0) return(ugly_english_specie)
  result <- NAME_CONVERSION_ARG$English_specie_pretty[idx]
  return(result)
}

get_ugly_english_specie <- function(pretty_english_specie, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (pretty_english_specie %in% NAME_CONVERSION_ARG$English_specie) return(pretty_english_specie)
  idx <- which(NAME_CONVERSION_ARG$English_specie_pretty == pretty_english_specie)
  if (length(idx) == 0) return(pretty_english_specie)
  result <- NAME_CONVERSION_ARG$English_specie[idx]
  return(result)
}

get_english_specie_from_specie <- function(specie, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (specie %in% NAME_CONVERSION_ARG$English_specie) return(specie)
  idx <- which(NAME_CONVERSION_ARG$Specie == specie)
  if (length(idx) == 0) return(specie)
  result <- NAME_CONVERSION_ARG$English_specie[idx]
  return(result)
}

get_specie_from_english_specie <- function(english_specie, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  if (english_specie %in% NAME_CONVERSION_ARG$Specie) return(english_specie)
  idx <- which(NAME_CONVERSION_ARG$English_specie == english_specie)
  if (length(idx) == 0) return(english_specie)
  result <- NAME_CONVERSION_ARG$Specie[idx]
  return(result)
}

get_group_from_specie <- function(specie, NAME_CONVERSION_ARG = NAME_CONVERSION) {
  idx <- which(NAME_CONVERSION_ARG$Specie == specie)
  if (length(idx) == 0) return(specie)
  result <- NAME_CONVERSION_ARG$Group[idx]
  return(result)
}

normalizePath <- function(path) {
  return(base::normalizePath(path, mustWork = FALSE))
}

user_path <- function() {
  # Determine the OS
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
  
  if (Sys.info()['sysname'] == "Windows") {
    
    # Windows
    OS <- "Windows"
    
  } else if (.Platform$OS.type == "unix") {
    
    # if (grepl("Microsoft", readLines("/proc/version"), ignore.case = TRUE))
    if (grepl("WSL", Sys.info()["release"], ignore.case = TRUE)) {
      
      # Windows Subsystem for Linux
      OS <- "WSL"
      
    } else {
      
      # Linux
      OS <- "Linux"
      
    }
  }
  
  
  if (OS == "Windows") {
    
    # Pure Windows
    UserPath <- normalizePath(file.path(Sys.getenv("USERPROFILE")))
    
  } else if (OS == "Linux") {
    
    # Pure Linux
    UserPath <- normalizePath(file.path(Sys.getenv("HOME")))
    
  } else if (OS == "WSL") {
    
    # https://stackoverflow.com/questions/23513045/how-to-check-if-a-process-is-running-inside-docker-container
    if (any(grepl("docker", readLines("/proc/1/cgroup")))) {
      
      # DOCKER
      UserPath <- normalizePath(file.path(Sys.getenv("R_HOME")))
      
    } else {
      
      # Pure WSL
      # https://superuser.com/a/1568668
      UserPath <- normalizePath(file.path(system('wslpath "$(wslvar USERPROFILE)"', intern = TRUE)))
      
    }
  }
  return(UserPath)
}

# Function to generate a unique ID
generate_unique_id <- function(used_ids_reactive, sample_space) {
  id <- sample(sample_space, 1)
  while (id %in% used_ids_reactive()) {
    id <- sample(sample_space, 1)
  }
  return(id)
}

install_and_load_packages <- function(packages, update = FALSE, verbose = TRUE) {
  # Load packages
  libs <- unique(c(normalizePath(.libPaths()),
                   normalizePath(Sys.getenv("R_LIBS_USER")),
                   normalizePath(file.path(getwd(), "myRlibrary"))))
  repo <- "https://cran.rstudio.com/"
  
  # On Windows, don't build from source
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
  if (os == "Windows") {
    type <- "win.binary"
  } else if (os == "osx") {
    type <- "mac.binary"
  } else if (os == "linux") {
    type <- "source"
  }
  
  # Loop through libraries until one is writable
  error_happened <- TRUE
  i <- 1
  while (isTRUE(error_happened) && i <= length(libs)) {
    lib <- libs[i]
    tryCatch({
      # Unload packages
      sapply(packages, function(x) {tryCatch(detach(paste0("package:", x), character.only = TRUE, unload = TRUE, force = TRUE), error = function(e) {}, warning = function(w) {})})
      
      if (isTRUE(update)) {
        update.packages(lib.loc = lib, instlib = lib, repos = repo, oldPkgs = packages, type = type, ask = FALSE, quiet = !verbose)
      }
      
      # Only load prefeR namespace
      if ("prefeR" %in% packages && !requireNamespace("prefeR", quietly = TRUE)) {
        install.packages("prefeR", lib = lib, repos = repo, quiet = !verbose)
        packages <- packages[packages != "prefeR"]
        loadNamespace("prefeR")
      }
      
      # Load the packages already installed
      packages_status <- sapply(packages, require, character.only = TRUE, quietly = !verbose, lib.loc = lib)
      
      # Other packages to install
      packages_to_install <- packages[packages_status == FALSE]
      
      # Unload packages
      sapply(packages, function(x) {tryCatch(detach(paste0("package:", pkg), unload = TRUE, force = TRUE), error = function(e) {}, warning = function(w) {})})
      
      # Remove packages that failed to load if they are already available
      tryCatch(remove.packages(packages_to_install, lib = lib), error = function(e) {}, warning = function(w) {})
      
      # Install packages
      install.packages(packages_to_install, lib = lib, repos = repo, type = type, quiet = !verbose)
      
      # Load packages
      sapply(packages, library, character.only = TRUE, quietly = !verbose, lib.loc = lib)
      
      # Stop the loop if we reach here
      error_happened <- FALSE
    },
    error = function(e) {},
    finally = {
      i <- i + 1
    })
  }
  return(!error_happened)
}



Plus_Or_Minus_Button<-function(Selected_Cluster_To_Display_Loc,
                                Selected_Point_In_Cluster_To_Display_Loc,
                                Clustering_Category_VectorLoc,
                                SubsetMeetTargetsReactiveUniqueLoc,
                                Projected_TSNE_Data_Clusters_Loc,
                                Basis_Clustering_Loc,
                                Mean_Clusters_Loc,
                               Limits_Direction_Clusters_Loc,
                               Output_Name,
                               Sign_Button
                                )
{
  SubsetMeetTargetsUniqueInSelectedCluster<-list(YEAR=SubsetMeetTargetsReactiveUniqueLoc$YEAR[Clustering_Category_VectorLoc==Selected_Cluster_To_Display_Loc,],
                                                 TYPE=SubsetMeetTargetsReactiveUniqueLoc$YEAR[Clustering_Category_VectorLoc==Selected_Cluster_To_Display_Loc,],
                                                 OUTPUTS=SubsetMeetTargetsReactiveUniqueLoc$OUTPUTS[Clustering_Category_VectorLoc==Selected_Cluster_To_Display_Loc,],
                                                 TSNE_PROJECTION=Projected_TSNE_Data_Clusters_Loc[[Selected_Cluster_To_Display_Loc]],
                                                 Basis=Basis_Clustering_Loc[[Selected_Cluster_To_Display_Loc]],
                                                 MEANS=Mean_Clusters_Loc[[Selected_Cluster_To_Display_Loc]])
  
  
  
  SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint<-
    list(YEAR=SubsetMeetTargetsUniqueInSelectedCluster$YEAR[Selected_Point_In_Cluster_To_Display_Loc,],
         TYPE=SubsetMeetTargetsUniqueInSelectedCluster$TYPE[Selected_Point_In_Cluster_To_Display_Loc,],
         OUTPUTS=SubsetMeetTargetsUniqueInSelectedCluster$OUTPUTS[Selected_Point_In_Cluster_To_Display_Loc,],
         TSNE_PROJECTION=SubsetMeetTargetsUniqueInSelectedCluster$TSNE_PROJECTION[Selected_Point_In_Cluster_To_Display_Loc,],
         Basis=SubsetMeetTargetsUniqueInSelectedCluster$Basis,
         MEANS=SubsetMeetTargetsUniqueInSelectedCluster$MEANS
    )
  
  
  SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint<-
    list(YEAR=SubsetMeetTargetsUniqueInSelectedCluster$YEAR[-Selected_Point_In_Cluster_To_Display_Loc,],
         TYPE=SubsetMeetTargetsUniqueInSelectedCluster$TYPE[-Selected_Point_In_Cluster_To_Display_Loc,],
         OUTPUTS=SubsetMeetTargetsUniqueInSelectedCluster$OUTPUTS[-Selected_Point_In_Cluster_To_Display_Loc,],
         TSNE_PROJECTION=SubsetMeetTargetsUniqueInSelectedCluster$TSNE_PROJECTION[-Selected_Point_In_Cluster_To_Display_Loc,],
         Basis=SubsetMeetTargetsUniqueInSelectedCluster$Basis,
         MEANS=SubsetMeetTargetsUniqueInSelectedCluster$MEANS
    )
  if(!is.null(dim(SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$YEAR))){
    #we only continue if there are at least 5 points in the cluster
    if(dim(SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$YEAR)[1]>=4){
  if( Sign_Button=="+"){
  Set_Output_GreaterOrLess_Than_current<-which(SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$OUTPUTS[[Output_Name]]>
                                           SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$OUTPUTS[[Output_Name]])
  }else{
    Set_Output_GreaterOrLess_Than_current<-which(SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$OUTPUTS[[Output_Name]]<
                                                   SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$OUTPUTS[[Output_Name]])
    
  }
  
  if(length(Set_Output_GreaterOrLess_Than_current)>0){
    
    
    SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPointGREATERorLESSOutput<-
      list(YEAR=SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$YEAR[Set_Output_GreaterOrLess_Than_current,],
           TYPE=SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$TYPE[Set_Output_GreaterOrLess_Than_current,],
           OUTPUTS=SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$OUTPUTS[Set_Output_GreaterOrLess_Than_current,],
           TSNE_PROJECTION=SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$TSNE_PROJECTION[Set_Output_GreaterOrLess_Than_current,],
           Basis=SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$Basis,
           MEANS=SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPoint$MEANS
      )
    
    
    #Find the one with the Carbon the closes to the current value
      
    if(length(Set_Output_GreaterOrLess_Than_current)>1){
      if( Sign_Button=="+"){
      NewCoord<-SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPointGREATERorLESSOutput$TSNE_PROJECTION[which.min(SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPointGREATERorLESSOutput$OUTPUTS[[Output_Name]]),]}else{
        NewCoord<-SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPointGREATERorLESSOutput$TSNE_PROJECTION[which.max(SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPointGREATERorLESSOutput$OUTPUTS[[Output_Name]]),]  
      }
    
 }else{NewCoord<-SubsetMeetTargetsUniqueInSelectedClusterWITHOUTSelectedPointGREATERorLESSOutput$TSNE_PROJECTION}
    
    Limits_Direction_SelectedCluster_Loc<-Limits_Direction_Clusters_Loc[[Selected_Cluster_To_Display_Loc]]
    min_x_t<-Limits_Direction_SelectedCluster_Loc$min_dir1
    min_y_t<-Limits_Direction_SelectedCluster_Loc$min_dir2
    max_x_t<-Limits_Direction_SelectedCluster_Loc$max_dir1
    max_y_t<-Limits_Direction_SelectedCluster_Loc$max_dir2
    
    NewValueOnSlider_x<-max(min((NewCoord[1]-min_x_t)/(max_x_t-min_x_t)*100,100),0)
    NewValueOnSlider_y<-max(min((NewCoord[2]-min_y_t)/(max_y_t-min_y_t)*100,100),0) 
    return(list(NewValueOnSlider_x=NewValueOnSlider_x,NewValueOnSlider_y=NewValueOnSlider_y,UpdateSliders=TRUE))

    # this else is in the case that there are no points in the set that is greater than or less the point currently selected
    # in terms of outcome. This means we reached the point with either the largest values (for +) or the smallest value (for -)
    # in terms of outcome in this target compatible cluster.
  }else{return(list(NewValueOnSlider_x=SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$TSNE_PROJECTION[1],
                    NewValueOnSlider_y=SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$TSNE_PROJECTION[2],
                    UpdateSliders=FALSE))}
   # this else is in the case that there are only 3 or 4 points in the cluster 
  }else{return(list(NewValueOnSlider_x=SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$TSNE_PROJECTION[1],
                   NewValueOnSlider_y=SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$TSNE_PROJECTION[2],
                   UpdateSliders=FALSE))}
  # this else is in the case that there are only 2 points in the cluster
  }else{return(list(NewValueOnSlider_x=SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$TSNE_PROJECTION[1],
                    NewValueOnSlider_y=SubsetMeetTargetsUniqueInSelectedClusterCurrentSelectedPoint$TSNE_PROJECTION[2],
                    UpdateSliders=FALSE))}
  
  
}
