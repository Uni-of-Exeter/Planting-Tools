  library(shiny)
  library(shinyjs)
  #library(tidyverse)
  
  library(leaflet)
  library(sf)
  library(ggplot2)
  library(geosphere)
  shfile<-sf::st_read("exp0308154002.gdb.zip",layer = "INV_BLKDATA")
  shconv <- st_transform(shfile, crs = st_crs("+proj=longlat +datum=WGS84"))
  
  redSquirel<-read.csv("RedSquirel.csv",header=F)
  enn<-readRDS("jules_ennerdale.rds")
  simuDF<-read.csv(file="SimuDF2.csv")[,-1]
  
  
  
  
  squaresinit<-read.csv("FEsoil1km.csv")
  squares<-squaresinit[,c("x","y","extent")]#
  names(squares)<-c("lng","lat","extent")
  squares<-squares[(!is.na(squares[,"lng"]))&(!is.na(squares[,"lat"])),]
  sqsf<-st_as_sf(squares,coords = c("lng","lat"),crs=st_crs(shfile))
  sqconv <- st_transform(sqsf, crs =st_crs("+proj=longlat +datum=WGS84"))#4326)# st_crs("+proj=longlat +datum=WGS84"))
  
  sqlng<-matrix(0,dim(sqconv)[1],5)
  sqlat<-matrix(0,dim(sqconv)[1],5)
  for(abb in 1:dim(sqconv)[1]){
    sqlng[abb,1]<-as.numeric(sqconv$geometry[[abb]])[1]
    sqlat[abb,1]<-as.numeric(sqconv$geometry[[abb]])[2]
  }
  destt<-destPoint(destPoint(cbind(sqlng[,1],sqlat[,1]),0,1000),90,1000)
  
  
  sqlng[,2]<-sqlng[,1]
  sqlng[,3]<-destt[,1]
  sqlng[,4]<-destt[,1]
  sqlng[,5]<-sqlng[,1]
  
  sqlat[,2]<-destt[,2]
  sqlat[,3]<-destt[,2]
  sqlat[,4]<-sqlat[,1]
  sqlat[,5]<-sqlat[,1]
  
  ennSq<-squaresinit[squaresinit$OBJECTID==193,]
  TreeCarbonEnn<-enn[enn$time==337,]
  PositionVec<-rep(0,dim(TreeCarbonEnn)[1])
  for(ijkkl in 1:dim(TreeCarbonEnn)[1])
  {
    PositionVec[ijkkl]<-sum((1:dim(ennSq)[1])*((ennSq$x==TreeCarbonEnn$x[ijkkl])&(ennSq$y==TreeCarbonEnn$y[ijkkl])))
  }
  
  #####################################
  TargetLevelTree<-500
  TargetLevelBio<-20
  QuantileLevels<-t(matrix(c(0.75,0.75,0.5,0.5,0.5,0.5,0.25,0.25),2,4))
  AboveTargets<-(simuDF$carbon>=TargetLevelTree)&(simuDF$redsquirel>=TargetLevelBio)
  AboveTargetSet<-simuDF[AboveTargets,]
  
  CornerSets<-list()
  QuantMat<-QuantileLevels
  for(i in 1:4){
    QuantMat[i,1]<-quantile(AboveTargetSet$carbon,QuantileLevels[i,1])
    QuantMat[i,2]<-quantile(AboveTargetSet$redsquirel,QuantileLevels[i,2])
  }
  QuantBio<-quantile(AboveTargetSet$redsquirel,QuantileLevels)
  CornerSets[[1]]<-AboveTargetSet[(AboveTargetSet$carbon>=QuantMat[1,1])&(AboveTargetSet$redsquirel>=QuantMat[1,2]),]
  CornerSets[[2]]<-AboveTargetSet[(AboveTargetSet$carbon>=QuantMat[2,1])&(AboveTargetSet$redsquirel<=QuantMat[2,2]),]
  CornerSets[[3]]<-AboveTargetSet[(AboveTargetSet$carbon<=QuantMat[3,1])&(AboveTargetSet$redsquirel>=QuantMat[3,2]),]
  CornerSets[[4]]<-AboveTargetSet[(AboveTargetSet$carbon<=QuantMat[4,1])&(AboveTargetSet$redsquirel<=QuantMat[4,2]),]
  
  
  #############################################
  BaseMap<-function(SelectedMap)
  {
    
    ListMaps<-shconv[shconv$extent==SelectedMap,]$shape[[1]][[1]]
    max_x2<-(-Inf);min_x2<-(Inf);max_y2<-(-Inf);min_y2<-Inf;
    for(ii in 1: length(ListMaps)){
      xvec<-ListMaps[[ii]][,1]
      yvec<-ListMaps[[ii]][,2]
      xvec<-xvec[!is.na(xvec)]
      yvec<-yvec[!is.na(yvec)]
      max_x2<-max(max_x2,xvec)
      min_x2<-min(min_x2,xvec)
      
      max_y2<-max(max_y2,yvec)
      min_y2<-min(min_y2,yvec)
    }
    
    map<-leaflet() 
    map<-  addTiles(map) 
    map<-fitBounds(map,lng1 = min_x2, lat1 = min_y2, 
                   lng2 = max_x2, lat2 =max_y2) #%>%
    for (ii in 1:length(ListMaps)){
      map<-addPolygons(map,lng=ListMaps[[ii]][,1],lat=ListMaps[[ii]][,2],color="black")}
    return(list(map=map,max_x2=max_x2,min_x2=min_x2,max_y2=max_y2,min_y2=min_y2))
  }
  
  
  
  
  #########################################
  ui <- tabsetPanel(id = "tabs",
                    tabPanel("Maps",fluidPage(fluidRow(
                      column(9,
                             tags$head(tags$style(HTML("#map {pointer-events: none;}"))),
                             selectInput("inSelect", "area",sort(unique(  squaresinit$extent )),"Ennerdale"),
                             leafletOutput("map",height = 800,width="100%")
                      ),
                      #####################
                      column(3,
                             verticalLayout(sliderInput("SliderMain","Total Carbon:",min=0,max=870,value=800),
                                            textOutput("SoilCarbonNotIncluded"),
                                            sliderInput("BioSlider","Average Red Squirrel % increase over all cells:",min=0,max=36,value=25)
                                            )))
                    )
                    ), 
                    ##########################
                    tabPanel("Exploration",
                             fluidPage(fluidRow(
                               ################################
                               column(5,  
                                      verticalLayout(textOutput("FirstMapTxt"),leafletOutput("map2",height = 400,width="100%"))
                               ),
                               column(5,
                                      verticalLayout(textOutput("SecondMapTxt"),leafletOutput("map3",height = 400,width="100%"))     
                               ),
                               column(2, verticalLayout(verbatimTextOutput("TargetText"),
                                                      #  selectInput("chooseGrouping", "Grouping Type:",c("carbon level"),"carbon level"),
                                      actionButton("random", "Randomize!"))                
                               )
                             ),
                             fluidRow(
                               ################################
                               column(5,  
                                      verticalLayout(textOutput("ThirdMapTxt"),leafletOutput("map4",height = 400,width="100%"))
                               ),
                               column(5,
                                      verticalLayout(textOutput("FourthMapTxt"),leafletOutput("map5",height = 400,width="100%"))      
                               ),
                               column(2,"")
                             )
                             
                             )
                    )
                    ########################
  )
  
  
  
  server <- function(input, output, session) {
    ######################
    output$TargetText<-renderText({paste0("Tree Carbon target: ",TargetLevelTree,"\nAvg red squirrel Inc. target: ",TargetLevelBio)})
    output$SoilCarbonNotIncluded<-renderText({paste0("Note that Soil carbon is currently not included")})
    output$FirstMapTxt<-renderText({paste0("Tree Carbon above : ",round(QuantMat[1,1],1)," and Red Squirrel above :",round(QuantMat[1,2],1))})
    output$SecondMapTxt<-renderText({paste0("Tree Carbon above: ",round(QuantMat[2,1],1)," and Red Squirrel below :",round(QuantMat[2,2],1))})
      output$ThirdMapTxt<-renderText({paste0("Tree Carbon below : ",round(QuantMat[3,1],1)," and Red Squirrel above :",round(QuantMat[3,2],1))})
    output$FourthMapTxt<-renderText({paste0("Tree Carbon below: ",round(QuantMat[4,1],1)," and Red Squirrel below :",round(QuantMat[4,2],1))})
    ######################
    randomValues <- eventReactive(input$random, {
      retVec<-rep(0,length(CornerSets))
      for(jjjj in 1:length(CornerSets)){
        retVec[jjjj]<-sample(1:dim(CornerSets[[jjjj]])[1],1)}
      retVec
    })
    
    ##################################
    
    output$map <- renderLeaflet({
      
      
      SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#
      calcBaseMap<-BaseMap(SelectedDropdown)
      map<-calcBaseMap$map
      
      
      
      SelecTarget<-input$SliderMain
      SelecTargetBio<-input$BioSlider
      
      
      
      SubsetMeetTargets<-simuDF[(simuDF$carbon>=SelecTarget)&(simuDF$redsquirel>=SelecTargetBio),]
      
      DistSliderCarbon<-(SubsetMeetTargets$carbon-SelecTarget)/(max(simuDF$carbon)-min(simuDF$carbon))
      DistSliderBio<-(SubsetMeetTargets$redsquirel-SelecTargetBio)/(max(simuDF$redsquirel)-min(simuDF$redsquirel))
      
      SelecdMinRows<-which((DistSliderCarbon+DistSliderBio)==min(DistSliderCarbon+DistSliderBio))
      SelectedMins<-SubsetMeetTargets[SelecdMinRows,]
      
      SelecRow<-which.min(rowSums(SelectedMins[,1:47]))
      
      
      SwitchedOnCells<-SelectedMins[SelecRow,1:47]
      SELL<-(sqconv$extent==SelectedDropdown)
      SelectedTreeCarbon<-SelectedMins[SelecRow,]$carbon
      SelectedBio<-SelectedMins[SelecRow,]$redsquirel
      
      sellng<-sqlng[SELL,]
      sellat<-sqlat[SELL,]
      
      for (iii in 1:dim(sellng)[1]){
        if(iii%in%(PositionVec*SwitchedOnCells)){
          map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
      map
    })
    #############################################
    output$map2<-renderLeaflet({
      SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#input$inSelect#
      map<-BaseMap(SelectedDropdown)$map
      
      #if(input$chooseGrouping=="carbon level"){
        
        SavedRVs<-randomValues()
        SwitchedOnCells<-CornerSets[[1]][SavedRVs[1],1:47]
        
        SelectedTreeCarbon<-CornerSets[[1]][SavedRVs[1],]$carbon
        SelectedBio<-CornerSets[[1]][SavedRVs[1],]$redsquirel
        
        
        SELL<-(sqconv$extent==SelectedDropdown)
        sellng<-sqlng[SELL,]
        sellat<-sqlat[SELL,]
        
        for (iii in 1:dim(sellng)[1]){
          if(iii%in%(PositionVec*SwitchedOnCells)){
            map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
        
      #}
      
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
      map
    })
    #############################################
    output$map3<-renderLeaflet({
      SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#input$inSelect#
      map<-BaseMap(SelectedDropdown)$map
      
      #if(input$chooseGrouping=="carbon level"){
        
        SavedRVs<-randomValues()
        SwitchedOnCells<-CornerSets[[2]][SavedRVs[2],1:47]
        
        SelectedTreeCarbon<-CornerSets[[2]][SavedRVs[2],]$carbon
        SelectedBio<-CornerSets[[2]][SavedRVs[2],]$redsquirel
        
        
        SELL<-(sqconv$extent==SelectedDropdown)
        sellng<-sqlng[SELL,]
        sellat<-sqlat[SELL,]
        
        for (iii in 1:dim(sellng)[1]){
          if(iii%in%(PositionVec*SwitchedOnCells)){
            map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
        
      #}
      
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
      map
    })
    
    #############################################
    output$map4<-renderLeaflet({
      SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#input$inSelect#
      map<-BaseMap(SelectedDropdown)$map
      
      #if(input$chooseGrouping=="carbon level"){
        
        SavedRVs<-randomValues()
        SwitchedOnCells<-CornerSets[[3]][SavedRVs[3],1:47]
        SelectedTreeCarbon<-CornerSets[[3]][SavedRVs[3],]$carbon
        SelectedBio<-CornerSets[[3]][SavedRVs[3],]$redsquirel
        
        
        SELL<-(sqconv$extent==SelectedDropdown)
        sellng<-sqlng[SELL,]
        sellat<-sqlat[SELL,]
        
        for (iii in 1:dim(sellng)[1]){
          if(iii%in%(PositionVec*SwitchedOnCells)){
            map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
        
      #}
      
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
      map
    })
    #############################################
    output$map5<-renderLeaflet({
      SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#input$inSelect#
      map<-BaseMap(SelectedDropdown)$map
      
     # if(input$chooseGrouping=="carbon level"){
        
        SavedRVs<-randomValues()
        SwitchedOnCells<-CornerSets[[4]][SavedRVs[4],1:47]
        
        SelectedTreeCarbon<-CornerSets[[4]][SavedRVs[4],]$carbon
        SelectedBio<-CornerSets[[4]][SavedRVs[4],]$redsquirel
        
        
        SELL<-(sqconv$extent==SelectedDropdown)
        sellng<-sqlng[SELL,]
        sellat<-sqlat[SELL,]
        
        for (iii in 1:dim(sellng)[1]){
          if(iii%in%(PositionVec*SwitchedOnCells)){
            map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
        
      #}
      
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
      map
    })
    
    
    
    
  }
  
  shinyApp(ui, server)
  
