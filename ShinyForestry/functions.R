




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

normalize<-
  function(x) {
    return(scale(x, center=TRUE, scale=TRUE)[,1])
  }

Impl<-function(Target,EY,SDY,alpha,tol)
{
  Im<-(Target-EY)/sqrt(SDY^2+tol)
  NROY<-(Im<=sqrt((1-alpha)/alpha))
  return(list(Im=Im,NROY=NROY))
}
MultiImpl<-function(TargetsVec,EYMat,SDYMat,alpha,tolVec)
{
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

BaseMap<-function(SelectedMap,layerId=NULL,shconv)
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
    for(ii in 1: length(ListMaps)){
      for(jj in 1: length(ListMaps[[ii]])){
          map<-addPolygons(map,lng=ListMaps[[ii]][[jj]][,1],lat=ListMaps[[ii]][[jj]][,2],color="grey",layerId==paste0(layerId,ii,"_",jj))}}
    
  
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
                   lng2 = max_x2, lat2 =max_y2) #%>%
    for(ii in 1: length(ListMaps)){
      for(jj in 1: length(ListMaps[[ii]])){
        for(kk in 1: length(ListMaps[[ii]][[jj]])){
          map<-addPolygons(map,lng=ListMaps[[ii]][[jj]][[kk]][,1],lat=ListMaps[[ii]][[jj]][[kk]][,2],color="grey",layerId==paste0(layerId,ii,"_",jj,"_",kk))}}}
    
    
  }  
    
  return(list(map=map,max_x2=max_x2,min_x2=min_x2,max_y2=max_y2,min_y2=min_y2))
}



BaseMap2<-function(SelectedMap,layerId=NULL,shconv)
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
          map<-addPolygons(map,data=ListMaps,color="grey")
    
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
                                   AreaSelected0,
                                   CarbonSelected0,
                                   RedSquirrelSelected0,
                                   VisitsSelected0,
                                   CarbonSelectedSD0,
                                   RedSquirrelSelectedSD0,
                                   VisitsSelectedSD0,
                                   DatBinaryCode0,
                                   NbRoundsMax,
                                   CurrentRound,
                                   FullTable,
                                   FullTableNotAvail,
                                   VecNbMet0,
                                   shconv,
                                   SelectedSimMatGlobal,
                                   pref,
                                   ColorLighteningFactor,
                                   ColorDarkeningFactor) {
  SavedVec <- ClickedVector()
  LinesToCompare <- as.matrix(LinesToCompareReactive())
  SelectedDropdown <- input$inSelect
  
  shinyjs::disable("choose1")
  shinyjs::disable("choose2")
  
  if((!is.null(SavedVec))&(CurrentRound()>0)){
    calcBaseMap <- BaseMap(SelectedDropdown,layerId="main100",shconv=shconv)
    
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
        SelectedTreeCarbon <- SelectedLine[[aai]]$carbon
        SelectedBio <- SelectedLine[[aai]]$redsquirel
        SelectedArea <- SelectedLine[[aai]]$Area
        SelectedVisits <- SelectedLine[[aai]]$Visits
        
        SelectedTreeCarbonSD <- SelectedLine[[aai]]$carbonSD
        SelectedBioSD <- SelectedLine[[aai]]$redsquirelSD
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
          #  sellng <- FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
          #  sellat <- FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
          for (iii in 1:length(SwitchedOnCells)){
            if(SavedVec[iii]==1){
              
              if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
              listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng= as.numeric(SELGEO[[iii]][[1]][,1]),lat= as.numeric(SELGEO[[iii]][[1]][,2]),layerId =paste0("Square",iii),color =ClickedCols[iii])
              }else{
                for(kk in 1:length(SELGEO[[iii]])) {
                  listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng= as.numeric(SELGEO[[iii]][[kk]][[1]][,1]),lat= as.numeric(SELGEO[[iii]][[kk]][[1]][,2]),layerId =paste0("Square",iii,"_",kk),color =ClickedCols[iii])
                }
                
              }
              
              }
            else{
              if(SwitchedOnCells[iii]==1){
                if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
                listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng=  as.numeric(SELGEO[[iii]][[1]][,1]),lat=  as.numeric(SELGEO[[iii]][[1]][,2]),layerId =paste0("Square",iii),color=FullColVec[iii])
                }else{
                  for(kk in 1:length(SELGEO[[iii]])) {
                    listMaps[[aai]] <- addPolygons(listMaps[[aai]],lng=  as.numeric(SELGEO[[iii]][[kk]][[1]][,1]),lat=  as.numeric(SELGEO[[iii]][[kk]][[1]][,2]),layerId =paste0("Square",iii),color=FullColVec[iii])
                    
                  }
                  
                }
                }
            }
          }
        }
        
        listMaps[[aai]] <- listMaps[[aai]]%>%  
          addControl(html = paste0("<p>Carbon:",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Red Squirrel:",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Area Planted:",round(SelectedArea,2),"<br>
                                 Visitors:",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                   "</p>"), position = "topright")
      }#/1e6
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
      
      ClusteringDat <- data.frame(sqrt(infpref)*SelectedSimMat2[,c("carbon","redsquirel","Area","Visits")],NbTargetsMet=VecNbMet)
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
        
        tsneclusters <- Mclust(tsRes$Y, 1:4)
        ClusterPlot <- mutate(ClusteringDat, cluster=as.factor(tsneclusters$classification)) %>%
          ggpairs(columns=1:4, aes(color=cluster),upper=list(continuous="points"))
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
                                    RedSquirrelSelected,
                                    VisitsSelected,
                                    CarbonSelectedSD,
                                    RedSquirrelSelectedSD,
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
  RedSquirrelMAT <- t(matrix(as.numeric(RedSquirrelSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  AreaMAT <- t(matrix(AreaSelected, length(SavedVec), dim(SelectedSimMat)[1]))
  VisitsMAT <- t(matrix(as.numeric(VisitsSelected), length(SavedVec), dim(SelectedSimMat)[1]))
  
  CarbonSDMAT <- t(matrix(CarbonSelectedSD, length(SavedVec), dim(SelectedSimMat)[1]))
  RedSquirrelSDMAT <- t(matrix(as.numeric(RedSquirrelSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  VisitsSDMAT <- t(matrix(as.numeric(VisitsSelectedSD), length(SavedVec), dim(SelectedSimMat)[1]))
  
  # Create a data frame representing the selected similarity matrix
  SelectedSimMat <- data.frame(1 * (SelectedSimMat | SVMAT))
  
  SelecTargetCarbon <- input$SliderMain
  SelecTargetBio <- input$BioSlider
  SelecTargetArea <- input$AreaSlider#ifelse(input_areaSlider_multiplicative_coefficient == TRUE, input$AreaSlider , input$AreaSlider)
  SelecTargetVisits <- input$VisitsSlider
  
  #* 1e6
  
  SelectedSimMat2 <- data.frame(SelectedSimMat,
                                carbon = rowSums(SelectedSimMat * CarbonMAT),
                                redsquirel = rowMeans(SelectedSimMat * RedSquirrelMAT),
                                Area = rowSums(SelectedSimMat * AreaMAT),
                                Visits = rowMeans(SelectedSimMat * (VisitsMAT)),
                                carbonSD = sqrt(rowSums(SelectedSimMat * (CarbonSDMAT^2))),
                                redsquirelSD = sqrt(rowSums(SelectedSimMat * (RedSquirrelSDMAT^2))) / length(SavedVec),
                                VisitsSD = sqrt(rowSums(SelectedSimMat * (VisitsSDMAT^2))) / length(SavedVec))
  
  Icalc <- MultiImpl(TargetsVec = c(SelecTargetCarbon, SelecTargetBio, SelecTargetArea, SelecTargetVisits),
                     EYMat = data.frame(SelectedSimMat2$carbon, SelectedSimMat2$redsquirel, SelectedSimMat2$Area, SelectedSimMat2$Visits),
                     SDYMat = data.frame(SelectedSimMat2$carbonSD, SelectedSimMat2$redsquirelSD, rep(0, length(SelectedSimMat2$Area)), SelectedSimMat2$VisitsSD),
                     alpha = 0.05, tolVec = c(4, 0.05, 0.1, 2))
  
  LimitsMat <- (-data.frame(SelectedSimMat2$carbon,
                            SelectedSimMat2$redsquirel,
                            SelectedSimMat2$Area,
                            SelectedSimMat2$Visits)) / 
  #  sqrt(data.frame(SelectedSimMat2$carbonSD^2 + 4, SelectedSimMat2$redsquirelSD^2 + 2, rep(0, length(SelectedSimMat2$Area)) + 0.1, 
   #                 SelectedSimMat2$VisitsSD^2 + 2))
  #/ 
    sqrt(data.frame(SelectedSimMat2$carbonSD^2+4 , SelectedSimMat2$redsquirelSD^2+0.05, rep(0, length(SelectedSimMat2$Area)) + 0.1, 
                    SelectedSimMat2$VisitsSD^2+2))
  
  
  return(list(SelectedSimMat2 = SelectedSimMat2, Icalc = Icalc, LimitsMat = LimitsMat, SelecTargetCarbon = SelecTargetCarbon,
              SelecTargetBio = SelecTargetBio, SelecTargetArea = SelecTargetArea, SelecTargetVisits = SelecTargetVisits))
}

outputmap_createResults <- function(map,
                                    SubsetMeetTargets,
                                    alphaLVL,
                                    FullTable,
                                    SavedVec,
                                    SelectedDropdown,
                                    randomValue,
                                    ColorLighteningFactor,
                                    ColorDarkeningFactor) {
  SavedRVs <- randomValue()
  LSMT <- dim(SubsetMeetTargets)[1]
  SelectedLine <- SubsetMeetTargets[as.integer(trunc(SavedRVs * LSMT) + 1),]
  
  SwitchedOnCells <- SelectedLine[1:length(SavedVec)]
  SelectedTreeCarbon <- SelectedLine$carbon
  SelectedBio <- SelectedLine$redsquirel
  SelectedArea <- SelectedLine$Area
  SelectedVisits <- SelectedLine$Visits
  
  SelectedTreeCarbonSD <- SelectedLine$carbonSD
  SelectedBioSD <- SelectedLine$redsquirelSD
  SelectedVisitsSD <- SelectedLine$VisitsSD
  
  SELL <- (FullTable$extent == SelectedDropdown)
  
  if (!is.null(SELL)) {
    # sellng <- FullTable[SELL, c("lgn.1", "lgn.2", "lgn.3", "lgn.4", "lgn.5")]
    #  sellat <- FullTable[SELL, c("lat.1", "lat.2", "lat.3", "lat.4", "lat.5")]
    SELGEO<-  FullTable$geometry[SELL]
    ############
    UnitsSel<-unique(FullTable$units[SELL])
    Cols<-rainbow(length(UnitsSel))
    FullColVec<-rep(0,dim(FullTable[SELL,])[1])
    for (iii in 1:length(Cols)){
      FullColVec[FullTable$units[SELL]==UnitsSel[iii]]<-Cols[iii]
    }
    ClickedCols<-lighten(FullColVec,ColorLighteningFactor)
    FullColVec<-darken(FullColVec,ColorDarkeningFactor)
    ClickedCols<-rep("red",length(ClickedCols))
    ############  
    for (iii in 1:length(SwitchedOnCells)) {
      if (SavedVec[iii] == 1) {
        
        if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
        map <- addPolygons(map, lng = as.numeric(SELGEO[[iii]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[1]][,2]), layerId = paste0("Square", iii), color = ClickedCols[iii])
        }else{
          for(kk in 1:length(SELGEO[[iii]])) {
            map <- addPolygons(map, lng = as.numeric(SELGEO[[iii]][[kk]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[kk]][[1]][,2]), layerId = paste0("Square", iii,"_",kk), color = ClickedCols[iii])
            
          }
          
        }
        
      } else {
        if (SwitchedOnCells[iii] == 1) {
          if(st_geometry_type(SELGEO[[iii]])=="POLYGON"){
          
          map <- addPolygons(map, lng =as.numeric(SELGEO[[iii]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[1]][,2]), layerId = paste0("Square", iii),color=FullColVec[iii])
          }else{
            for(kk in 1:length(SELGEO[[iii]])) {
              map <- addPolygons(map, lng =as.numeric(SELGEO[[iii]][[kk]][[1]][,1]), lat = as.numeric(SELGEO[[iii]][[kk]][[1]][,2]), layerId = paste0("Square", iii,"_",kk),color=FullColVec[iii])
              
            }
            
          }
        }
      }
    }
  }
  
  return(list(map = map,
              SelectedLine = SelectedLine,
              SelectedTreeCarbon = SelectedTreeCarbon,
              SelectedTreeCarbonSD = SelectedTreeCarbonSD,
              SelectedBio = SelectedBio,
              SelectedBioSD = SelectedBioSD,
              SelectedArea = SelectedArea,
              SelectedVisits = SelectedVisits,
              SelectedVisitsSD = SelectedVisitsSD,
              SubsetMeetTargets = SubsetMeetTargets,
              SavedRVs = SavedRVs,
              LSMT = LSMT))
}


