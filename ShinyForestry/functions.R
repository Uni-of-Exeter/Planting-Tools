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
  Im<-(Target-EY)/sqrt(SDY^2+tol)
  # Not Ruled-Out space Yet
  NROY<-(Im<=sqrt((1-alpha)/alpha))
  return(list(Im=Im,NROY=NROY))
}

MultiImpl <- function(TargetsVec,EYMat,SDYMat,alpha,tolVec) {
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
  
    map<-leaflet() 
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
    map<-leaflet() 
    map<-  addTiles(map) 
    map<-fitBounds(map,lng1 = min_x2, lat1 = min_y2, 
                   lng2 = max_x2, lat2 =max_y2) 
    
  }  
          map<-addPolygons(map,data=ListMaps,color="grey",weight=GreyPolygonWidth, fillOpacity = 0.5)
    
  return(list(map=map,max_x2=max_x2,min_x2=min_x2,max_y2=max_y2,min_y2=min_y2))
}

map_sell_not_avail <- function(FullTableNotAvail,
                               SelectedDropdown,
                               listMaps = NULL,
                               map = NULL) {
  if(dim(FullTableNotAvail)[1]>0){  SELLNOTAVAIL <- FullTableNotAvail$extent==SelectedDropdown}else{SELLNOTAVAIL<-NULL}
  if(sum(SELLNOTAVAIL)>0){
    # here we assume that not avail is only list of polygons
    
    #sellngNotAvail <- FullTableNotAvail[SELLNOTAVAIL, c("lgn.1", "lgn.2", "lgn.3", "lgn.4", "lgn.5")]
    #sellatNotAvail <- FullTableNotAvail[SELLNOTAVAIL, c("lat.1", "lat.2", "lat.3", "lat.4", "lat.5")]
    
    SELGEO<-FullTableNotAvail$geometry[SELLNOTAVAIL]
    
    
    for (iii in 1:length(SELGEO)) {
      if (is.null(map) && !is.null(listMaps)) {
        listMaps[[1]] <- leaflet::addPolygons(listMaps[[1]],
                                              lng = SELGEO[[iii]][[1]][,1],#as.numeric(sellngNotAvail[iii, ]),
                                              lat = SELGEO[[iii]][[1]][,2],#as.numeric(sellatNotAvail[iii, ]),
                                              layerId = paste0("SquareNotAvail", iii),
                                              color = "yellow")
        listMaps[[2]] <- leaflet::addPolygons(listMaps[[2]],
                                              lng = SELGEO[[iii]][[1]][,1],# as.numeric(sellngNotAvail[iii,]),
                                              lat =  SELGEO[[iii]][[1]][,2],#as.numeric(sellatNotAvail[iii,]),
                                              layerId = paste0("SquareNotAvail",iii),
                                              color ="yellow")
        
        midlng <- ( SELGEO[[iii]][[1]][1,1] + SELGEO[[iii]][[1]][3,1]) / 2
        midlat <- ( SELGEO[[iii]][[1]][1,2] +  SELGEO[[iii]][[1]][3,2]) / 2
        
        listMaps[[1]] <- leaflet::addPolylines(listMaps[[1]],
                                               lat = c(midlat - 0.0025, midlat + 0.0025),
                                               lng = c(midlng - 0.004, midlng + 0.004),
                                               color = "orange",
                                               weight = 5)
        listMaps[[1]] <- leaflet::addPolylines(listMaps[[1]],
                                               lat = c(midlat - 0.0025, midlat + 0.0025),
                                               lng = c(midlng + 0.004, midlng - 0.004),
                                               color = "orange",
                                               weight = 5)
        listMaps[[2]] <- leaflet::addPolylines(listMaps[[2]],
                                               lat = c(midlat - 0.0025, midlat + 0.0025),
                                               lng = c(midlng - 0.004, midlng + 0.004),
                                               color = "orange",
                                               weight = 5)
        listMaps[[2]] <- leaflet::addPolylines(listMaps[[2]],
                                               lat = c(midlat - 0.0025, midlat + 0.0025),
                                               lng = c(midlng + 0.004, midlng - 0.004),
                                               color = "orange",
                                               weight = 5)
      } else if (!is.null(map) && is.null(listMaps)) {
        map <- leaflet::addPolygons(map,
                                    lng = SELGEO[[iii]][[1]][,1],
                                    lat = SELGEO[[iii]][[1]][,2],
                                    layerId = paste0("SquareNotAvail", iii),
                                    color = "yellow")
        
        midlng <- (SELGEO[[iii]][[1]][1,1] + SELGEO[[iii]][[1]][3,1]) / 2
        midlat <- (SELGEO[[iii]][[1]][1,2] + SELGEO[[iii]][[1]][3,2]) / 2
        
        map <- leaflet::addPolylines(map,
                                     lat = c(midlat - 0.0025, midlat + 0.0025),
                                     lng = c(midlng - 0.004, midlng + 0.004),
                                     color = "orange",
                                     weight = 5)
        map <- leaflet::addPolylines(map,
                                     lat = c(midlat - 0.0025, midlat + 0.0025),
                                     lng = c(midlng + 0.004, midlng - 0.004),
                                     color = "orange",
                                     weight = 5)
      }
    }}
  if (is.null(map) && !is.null(listMaps)) {
    return(listMaps)
  } else if (!is.null(map) && is.null(listMaps)) {
    return(map)
  }
  
}

observe_event_function <- function(choose = 1, # 1 for input$choose1, 2 for input$choose2
                                   input,
                                   output,
                                   session,
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
                                   pref,
                                   ColourScheme,
                                   ColorLighteningFactor,
                                   ColorDarkeningFactor,
                                   SPECIES_ARG3,
                                   SPECIES_ENGLISH_ARG3,
                                   N_TARGETS_ARG2,
                                   GreyPolygonWidth,
                                   UnitPolygonColours) {
  SPECIES <- SPECIES_ARG3
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG3
  N_TARGETS <- N_TARGETS_ARG2
  SavedVec <- ClickedVector()
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
      if (choose == 1) {
        pref$addPref(prefeR::`%>%`(LinesToCompare[CR,1],LinesToCompare[CR,2]))
      } else if (choose == 2) {
        pref$addPref(prefeR::`%>%`(LinesToCompare[CR,2],LinesToCompare[CR,1]))
      }
      if(CR<dim(LinesToCompare)[1]){
        LinesToCompare[CR+1,] <- prefeR::suggest(pref,maxComparisons = 5)
      }
      LinesToCompareReactive(LinesToCompare)
      
      CR <- CR+1
      CurrentRound(CR)
      
      listMaps <- list()
      listMaps[[1]] <- calcBaseMap$map
      listMaps[[2]] <- calcBaseMap$map
      
      SelectedLine <- list()
      SelectedLine[[1]] <- SelectedSimMat2[ConvertSample[LinesToCompare[CR,1]],]
      SelectedLine[[2]] <- SelectedSimMat2[ConvertSample[LinesToCompare[CR,2]],]
      for(aai in 1:2){
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
        
        SELL <- (FullTable$extent==SelectedDropdown)
        if(!is.null(SELL)){
          SELGEO<-FullTable$geometry[SELL]
          ############
          UnitsSel<-unique(FullTable$units[SELL])
          Cols<-rainbow(length(UnitsSel))
          FullColVec<-rep(0,dim(FullTable[SELL,])[1])
          for (iii in 1:length(Cols)){
            FullColVec[FullTable$units[SELL]==UnitsSel[iii]]<-Cols[iii]
          }
          ############  
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
      listMaps <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail,
                                     SelectedDropdown = SelectedDropdown,
                                     listMaps = listMaps)
      
      output$ClusterPage <- renderLeaflet({listMaps[[1]]})
      output$ClusterPage2 <- renderLeaflet({listMaps[[2]]})  
      shinyjs::enable("choose1")
      shinyjs::enable("choose2")
      
    } else {
      CR <- CurrentRound()
      pref$addPref(prefeR::`%>%`(LinesToCompare[CR,1],LinesToCompare[CR,2]))
      
      
      shinyjs::disable("choose1")
      shinyjs::disable("choose2")
      infpref <<- pref$infer()
      
      infpref[is.na(infpref)] <- 1e-5
      infpref[infpref<0] <- 1e-5
      
      SelectedSimMat2 <- SelectedSimMatGlobal
      VecNbMet <- VecNbMet0()
      
      # columns <- c("Carbon","redsquirrel","Area","Visits")
      SelectedSimMat2columns <- c("Carbon", SPECIES, "Area","Visits")
      ClusteringDat <- data.frame(sqrt(infpref)*SelectedSimMat2[,SelectedSimMat2columns],NbTargetsMet=VecNbMet)
      ClusteringDat <- ClusteringDat[ClusteringDat$NbTargetsMet>0,]
      ClusteringDat <- unique(ClusteringDat)
      set.seed(123)
      
      FailedTsne <- TRUE
      PerpVec <- c(10,20,30,5,2,1,0.1,40,50,60,70,80,100)
      IndexPerp <- 1
      
      while((FailedTsne)&(IndexPerp<=length(PerpVec))){
        Perp <- PerpVec[IndexPerp]
        tsRes <-try(Rtsne(ClusteringDat, perplexity = Perp))
        IndexPerp <- IndexPerp+1       
        if(class(tsRes)[1]!="try-error"){FailedTsne <- FALSE}
      }
      
      if(FailedTsne){
        
        pp <- ggplot() +theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "Clustering Failed",
                   size = 10, color = "black", hjust = 0.5, vjust = 0.5)
        output$plotOP1 <- renderPlot({pp})
        updateCheckboxInput(session,"Trigger", label = "", value = FALSE)
        
      }else{
        
        tsneclusters <- Mclust(tsRes$Y, 1:N_TARGETS)
        ClusterPlot <- mutate(ClusteringDat, cluster=as.factor(tsneclusters$classification)) %>%
          ggpairs(columns=1:N_TARGETS, aes(color=cluster),upper=list(continuous="points"))
        output$plotOP1 <- renderPlot({ClusterPlot})
        
        
        #pp< <- ggplot(data=data.frame(x=tsRes$Y[,1],y=tsRes$Y[,2]),aes(x,y))+
        #  geom_point(aes(colour =factor(ClusteringDat$NbTargetsMet)))+
        #  labs(x="dim1",y="dim2",color = "Number of Targets Met")+theme_minimal()
        #output$plotOP1 <- renderPlot({pp})
        updateCheckboxInput(session,"Trigger", label = "", value = FALSE)
      }
      
    }  }
}

outputmap_calculateMats <- function(input,
                                    SavedVec,
                                    simul636,
                                    AreaSelected,
                                    CarbonSelected,
                                    # RedSquirrelSelected,
                                    SpeciesListSelected, # list(Acanthis_cabaretSelected = Acanthis_cabaretSelected, ...)
                                    VisitsSelected,
                                    CarbonSelectedSD,
                                    # RedSquirrelSelectedSD,
                                    SpeciesListSelectedSD, # list(Acanthis_cabaretSelectedSD = Acanthis_cabaretSelectedSD, ...)
                                    VisitsSelectedSD,
                                    input_areaSlider_multiplicative_coefficient = TRUE) {
  # If only one element in SavedVec, select corresponding column in simul636
  if (length(SavedVec) == 1) {
    SelectedSimMat <- as.matrix(simul636[, 1:length(SavedVec)])
  } else {
    SelectedSimMat <- simul636[, 1:length(SavedVec)]
  }
  
  SVMAT <- t(matrix(SavedVec, length(SavedVec), dim(SelectedSimMat)[1]))
  CarbonMAT <- t(matrix(CarbonSelected, length(SavedVec), dim(SelectedSimMat)[1]))
  # RedSquirrelMAT <- t(matrix(as.numeric(RedSquirrelSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelected)) {
    specie_name <- names(SpeciesListSelected)[i]
    specie_value <- SpeciesListSelected[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "Selected"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVec), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  AreaMAT <- t(matrix(AreaSelected, length(SavedVec), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  
  CarbonSDMAT <- t(matrix(CarbonSelectedSD, length(SavedVec), dim(SelectedSimMat)[1]))
  # RedSquirrelSDMAT <- t(matrix(as.numeric(RedSquirrelSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  for (i in 1:length(SpeciesListSelectedSD)) {
    specie_name <- names(SpeciesListSelectedSD)[i]
    specie_value <- SpeciesListSelectedSD[[i]]
    mat_name <- paste0(specie_name, "MAT")
    # specieSelected <- get(paste0(specie_name, "SelectedSD"))
    value <- t(matrix(as.numeric(specie_value), length(SavedVec), dim(SelectedSimMat)[1]))
    assign(mat_name, value)
  }
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  
  # Create a data frame representing the selected similarity matrix
  SelectedSimMat <- data.frame(1 * (SelectedSimMat | SVMAT))
  
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
  SelecTargetVisits <- input$VisitsSlider
  
  speciesMat <- do.call("data.frame", setNames(lapply(names(SpeciesListSelected),
                                                      function(x) bquote(rowMeans(SelectedSimMat * get(paste0(.(x), "MAT"))))),
                                               names(SpeciesListSelected)))
  speciesMatSD <- do.call("data.frame", setNames(lapply(names(SpeciesListSelectedSD),
                                                        function(x) bquote(sqrt(rowSums(SelectedSimMat * (get(paste0(.(x), "MAT"))^2) / length(SavedVec))))),
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
                                VisitsSD = sqrt(rowSums(SelectedSimMat * (VisitsSDMAT^2))) / length(SavedVec))
  # for (specie_name in names(SpeciesListSelected)) {
  #   value <- rowMeans(SelectedSimMat * get(paste0(specie_name, "MAT")))
  #   SelectedSimMat2[specie_name] <- value
  # }
  # for (specie_name in names(SpeciesListSelectedSD)) {
  #   value <- sqrt(rowSums(SelectedSimMat * (get(paste0(specie_name, "MAT"))^2) / length(SavedVec)))
  #   SelectedSimMat2[specie_name] <- value
  # }
  
  tolvec <- c(mean(SelectedSimMat2$Carbon) / 50,
              colMeans(speciesMat) / 50,
              mean(SelectedSimMat2$Area) / 50,
              mean(SelectedSimMat2$Visits)) / 50)
  for(fgh in 1:length(tolvec)) {
    if (tolvec[i] == 0) {
      tolvec[i] <- 0.1
    }
  }                 
  # tolVec <- c(4, 0.05, 0.1, 2)
  Icalc <- MultiImpl(# TargetsVec = c(SelecTargetCarbon, SelecTargetBio, SelecTargetArea, SelecTargetVisits),
                     TargetsVec = c(SelecTargetCarbon, SelecTargetBioVector, SelecTargetArea, SelecTargetVisits),
                     # EYMat = data.frame(SelectedSimMat2$Carbon, SelectedSimMat2$redsquirrel, SelectedSimMat2$Area, SelectedSimMat2$Visits),
                     EYMat = data.frame(SelectedSimMat2$Carbon, speciesMat, SelectedSimMat2$Area, SelectedSimMat2$Visits),
                     # SDYMat = data.frame(SelectedSimMat2$CarbonSD, SelectedSimMat2$redsquirrelSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
                     SDYMat = data.frame(SelectedSimMat2$CarbonSD, speciesMatSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
                     alpha = 0.05, tolVec = tolvec)
  
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

outputmap_createResults <- function(map,
                                    SubsetMeetTargets,
                                    alphaLVL,
                                    FullTable,
                                    SavedVec,
                                    SelectedDropdown,
                                    randomValue,
                                    ColourScheme,
                                    ColorLighteningFactor,
                                    ColorDarkeningFactor,
                                    SPECIES_ARG2,
                                    SPECIES_ENGLISH_ARG2,
                                    UnitPolygonColours) {
  SPECIES <- SPECIES_ARG2
  SPECIES_ENGLISH <- SPECIES_ENGLISH_ARG2
  SavedRVs <- randomValue()
  LSMT <- dim(SubsetMeetTargets)[1]
  SelectedLine <- SubsetMeetTargets[as.integer(trunc(SavedRVs * LSMT) + 1),]
  
  SwitchedOnCells <- SelectedLine[1:length(SavedVec)]
  SelectedTreeCarbon <- SelectedLine$Carbon
  # SelectedBio <- SelectedLine$redsquirrel
  SelectedBioList <- list()
  for (x in SPECIES) {
    var_name <- paste0("SelectedBio", x)
    value <- SelectedLine[[x]]
    assign(var_name, value)
    SelectedBioList[var_name] <- value
  }
  SelectedArea <- SelectedLine$Area
  SelectedVisits <- SelectedLine$Visits
  
  SelectedTreeCarbonSD <- SelectedLine$CarbonSD
  # SelectedBioSD <- SelectedLine$redsquirrelSD
  SelectedBioSDList <- list()
  for (x in SPECIES) {
    var_name <- paste0("SelectedBioSD", x)
    value <- SelectedLine[[paste0(x, "SD")]]
    assign(var_name, value)
    SelectedBioSDList[var_name] <- value
  }
  SelectedVisitsSD <- SelectedLine$VisitsSD
  
  SELL <- (FullTable$extent == SelectedDropdown)
  
  if (!is.null(SELL)) {
    # sellng <- FullTable[SELL, c("lgn.1", "lgn.2", "lgn.3", "lgn.4", "lgn.5")]
    #  sellat <- FullTable[SELL, c("lat.1", "lat.2", "lat.3", "lat.4", "lat.5")]
    SELGEO<-  FullTable$geometry[SELL]
    ############
    UnitsSel<-unique(FullTable$units[SELL])
    ColObtained<-getCols(ColourScheme,UnitsVec=FullTable$units[SELL],
                         ColorLighteningFactor,ColorDarkeningFactor)
    
    #Cols<-rainbow(length(UnitsSel))
    #FullColVec<-rep(0,dim(FullTable[SELL,])[1])
    #for (iii in 1:length(Cols)){
    #  FullColVec[FullTable$units[SELL]==UnitsSel[iii]]<-Cols[iii]
    #}
    #ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
    FullColVec<-ColObtained$FullColVec#darken(FullColVec,ColorDarkeningFactor)
    ClickedCols<-ColObtained$ClickedCols#rep("red",length(ClickedCols))
    ############  
    for (iii in 1:length(SwitchedOnCells)) {
      if (SavedVec[iii] == 1) {
        
        if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
        map <- addPolygons(map, lng = as.numeric(SELGEO[[iii]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[1]][,2]), layerId = paste0("Square", iii), color = ClickedCols[iii],weight=UnitPolygonColours)
        }else{
          for(kk in 1:length(SELGEO[[iii]])) {
            map <- addPolygons(map, lng = as.numeric(SELGEO[[iii]][[kk]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[kk]][[1]][,2]), layerId = paste0("Square", iii,"_",kk), color = ClickedCols[iii],weight=UnitPolygonColours)
            
          }
          
        }
        
      } else {
        if (SwitchedOnCells[iii] == 1) {
          if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
          
          map <- addPolygons(map, lng =as.numeric(SELGEO[[iii]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[1]][,2]), layerId = paste0("Square", iii),color=FullColVec[iii],weight=UnitPolygonColours)
          }else{
            for(kk in 1:length(SELGEO[[iii]])) {
              map <- addPolygons(map, lng =as.numeric(SELGEO[[iii]][[kk]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[kk]][[1]][,2]), layerId = paste0("Square", iii,"_",kk),color=FullColVec[iii],weight=UnitPolygonColours)
              
            }
            
          }
        }
      }
    }
  }
  
  return(c(list(map = map,
                SelectedLine = SelectedLine,
                SelectedTreeCarbon = SelectedTreeCarbon,
                SelectedTreeCarbonSD = SelectedTreeCarbonSD,
                # SelectedBio = SelectedBio,
                # SelectedBioSD = SelectedBioSD,
                SelectedArea = SelectedArea,
                SelectedVisits = SelectedVisits,
                SelectedVisitsSD = SelectedVisitsSD,
                SubsetMeetTargets = SubsetMeetTargets,
                SavedRVs = SavedRVs,
                LSMT = LSMT),
           SelectedBioList,
           SelectedBioSDList))
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
  vec
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
  if (length(prob_list) == 1) {
    prob_list <- unlist(prob_list)
  }
  # if(anyNA(condition, recursive = TRUE)) browser()
  return(prob_list)
}

subset_meet_targets <- function(PROBAMAT, SelectedSimMat2, CONDPROBAPositiveLIST, TARGETS, nb_targets_met) {
  n_metrics <- ncol(PROBAMAT)
  targets_met <- t(combn(n_metrics, nb_targets_met))
  targets_met <- as.matrix(targets_met[nrow(targets_met):1, ])
  
  SubsetMeetTargets <- data.frame()
  for (i in 1:nrow(targets_met)) {
    combination <- targets_met[i, ]
    SubsetMeetTargets <- rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBAPositiveLIST[[i]], ],
                                          Met = rep(paste(TARGETS[combination], collapse = ","),
                                                    sum(CONDPROBAPositiveLIST[[i]])),
                                          NotMet = rep(paste(TARGETS[-combination], collapse = ","),
                                                       sum(CONDPROBAPositiveLIST[[i]]))))
  }
  return(SubsetMeetTargets)
}

add_richness_columns <- function(FullTable, name_conversion) {
  # Convert from sf to tibble
  FullTable2 <- as_tibble(FullTable)
  # Add an artifical group for all species
  unique_groups <- c(unique(name_conversion$Group), "All")
  for (group in unique_groups) {
    if (group == "All") {
      species_in_group <- name_conversion$Specie
    } else {
      species_in_group <- name_conversion[name_conversion$Group == group, "Specie"]
    }
    colnames_to_find <- paste0("BioMean_", species_in_group)
    col_indices_of_group <- which(colnames(FullTable2) %in% colnames_to_find)
    
    if (length(col_indices_of_group) == 1) {
      FullTable2 <- cbind(FullTable2,
                          biomean = round(FullTable2[, col_indices_of_group]),
                          biosd = 0)
    } else {
      FullTable2 <- cbind(FullTable2,
                          biomean = round(rowSums(FullTable2[, col_indices_of_group]) / length(col_indices_of_group)),
                          biosd = 0)
    }
    number_of_columns <- length(colnames(FullTable2))
    column_names <- colnames(FullTable2)
    column_names[(number_of_columns-1):number_of_columns] <- paste0(c("BioMean_", "BioSD_"), group)
    colnames(FullTable2) <- column_names
  }
  # Convert back to sf object
  FullTable2 <- st_as_sf(FullTable2)
  return(FullTable2)
}

convert_bio_to_polygons_from_elicitor_and_merge_into_FullTable <- function(Elicitor_table,seer2km,speciesprob40,jncc100,climatecells) {
  # Take the Biodiversity probabilities from Matlab results/scenario_species_prob_40.csv
  # and merge them with BristolFullTableMerged.geojson
    
  # Load the shapefile mapping new2kid with Polygons
  id_polygons <- seer2km %>%dplyr::select(c(new2kid, geometry))
  
  # Load the biodiversity results from Matlab
  biodiversity <-speciesprob40
  
  # Replace NAs with column mean
  biodiversity <- biodiversity %>%
    mutate(across(everything(), ~ replace(.x,
                                          is.na(.x),
                                          mean(.x, na.rm = TRUE))))
  
  # Load new2kid
  new2kid <- climatecells$new2kid
  
  # Load species names
  all_species_names <- colnames(jncc100)[-1]
  colnames(biodiversity) <- all_species_names
  
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
  polygons_bio <- data.frame(new2kid, 100 * biodiversity) %>%
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
    dplyr::mutate(area_bio = st_area(geometry_bio),
           area_jules = st_area(geometry_jules),
           area_intersection = st_area(geometry)) %>%
    
    # Calculate the ratio of areas
    dplyr::mutate(proportion_intersection_in_bio = area_intersection / area_bio,
           proportion_intersection_in_jules = area_intersection / area_jules) %>%
    
    # Assume uniformity, multiply probability by proportion
    dplyr::mutate(dplyr::across(all_of(all_species_names),
                  ~ .x * proportion_intersection_in_bio)) %>%
  
    # Rename columns, add BioMean_ and BioSD_ to species
    dplyr::rename_with(.fn = ~ paste0("BioMean_", .x), .cols = all_of(all_species_names)) %>%
    
    # Add empty SD columns for species
    dplyr::bind_cols(df0) %>%
    
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
  
    dplyr::mutate(area_diff = map2_dbl(geometry_union, geometry_jules, compute_difference_area)) %>%
    
    # Remove useless columns
    as_tibble() %>%
    dplyr::select(c(starts_with("Bio"), polygon_id_jules, area_diff)) %>%
    
    # Merge back to original table
    left_join(polygons_jules, by = "polygon_id_jules") %>%
    
    dplyr::select(-c(polygon_id_jules, starts_with("geometry_")))
    
  rm(polygons_bio, polygons_jules)
  
  if (any(FullTable$area_diff >= 1)) {
    warning("The merged geometries from the intersections do not sum the ones intersected with the elicitor (jules): more than 1km square difference")
  }
  rm(df0)
  
  FullTable <- FullTable %>% dplyr::select(-area_diff) %>% st_as_sf()

  return(FullTable)
}
