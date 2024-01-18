library(shiny)
library(shinyjs)
#library(tidyverse)
library(leaflet)
library(sf)
library(ggplot2)
library(geosphere)

PROJdir<-system.file("proj/proj.db", package = "sf")
PROJdir<-substring(PROJdir,1,nchar(PROJdir)-8)
sf_proj_search_paths(PROJdir)

STDMEAN<-0.05
STDSTD<-0.01

############## shape file for all land
shfile<-sf::st_read("_exp0308154002.gdb.zip",layer = "INV_BLKDATA")
shconv <- st_transform(shfile, crs = 4326)

############## red squirrel for Ennerdale only
redSquirelEnnerdale<-as.numeric(read.csv("RedSquirel.csv",header=F))

############## Jules grid coverage for all GB
squaresinit<-read.csv("FE_Soil_Intersect_Area.csv")#FEsoil1km_Intersect_Area

############## get lat and lng and shape for all squares
squares<-squaresinit[,c("x","y","extent")]#
names(squares)<-c("lng","lat","extent")
squares<-squares[(!is.na(squares[,"lng"]))&(!is.na(squares[,"lat"])),]
sqsf<-st_as_sf(squares,coords = c("lng","lat"),crs=st_crs(shfile))
sqconv <- st_transform(sqsf, crs =4326)#4326)# st_crs("+proj=longlat +datum=WGS84"))

sqlng<-matrix(0,dim(sqconv)[1],5)
sqlat<-matrix(0,dim(sqconv)[1],5)
for(abb in 1:dim(sqconv)[1]){
  sqlng[abb,1]<-as.numeric(sqconv$geometry[[abb]])[1]
  sqlat[abb,1]<-as.numeric(sqconv$geometry[[abb]])[2]
}
destt<-destPoint(destPoint(cbind(sqlng[,1],sqlat[,1]),0,1000),90,1000)
sqlng[,2]<-sqlng[,1];sqlng[,3]<-destt[,1];sqlng[,4]<-destt[,1];sqlng[,5]<-sqlng[,1]
sqlat[,2]<-destt[,2];sqlat[,3]<-destt[,2];sqlat[,4]<-sqlat[,1];sqlat[,5]<-sqlat[,1]

################Area that can be planted in each square for Jules
Area<-squaresinit$area

ennSq<-squares[squares$extent=="Ennerdale",]


enn<-readRDS("jules_ennerdale.rds")


TreeCarbonEnnTable<-enn[enn$time==337,]
PositionVecEnnerdale<-rep(0,dim(TreeCarbonEnnTable)[1])
for(ijkkl in 1:dim(TreeCarbonEnnTable)[1])
{
  PositionVecEnnerdale[ijkkl]<-sum((1:dim(ennSq)[1])*((ennSq$lng==TreeCarbonEnnTable$x[ijkkl])&(ennSq$lat==TreeCarbonEnnTable$y[ijkkl])))
}
################## Carbon only for Ennerdale
CarbonEnn<-TreeCarbonEnnTable$carbon
################## Number of Visits for Ennerdale
NbVisitsEnn<-as.numeric(read.csv(file="VisitsEnn.csv",header=F))
################## Simulation Matrix
simuDF<-read.csv(file="SimuDF2.csv")[,-c(1)]#[,-c(1,49,50)]
simul636<-read.csv(file="Simul636.csv")[,-1]

#####################################
NbOuputs<-4
outputNames<-c("carbon","redSquirrel","plantedArea","NumberVisitsPerYear")
STDRAND<-list()
for(abc in 1:NbOuputs)
{STDRAND[[abc]]<-matrix(pmax(rnorm(dim(simul636)[1]*dim(simul636)[2],STDMEAN,STDSTD),0),dim(simul636)[1],dim(simul636)[2])
}

#####################################
TargetLevelTree<-500
TargetLevelBio<-20
TargetLevelArea<-15
TargetLevelVisits<-15


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
BaseMap<-function(SelectedMap,layerId=NULL)
{
  
  ListMaps<-shconv[shconv$extent==SelectedMap,]$shape[[1]]
  max_x2<-(-Inf);min_x2<-(Inf);max_y2<-(-Inf);min_y2<-Inf;
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
      map<-addPolygons(map,lng=ListMaps[[ii]][[jj]][,1],lat=ListMaps[[ii]][[jj]][,2],color="black",layerId==paste0(layerId,ii,"_",jj))}}
  return(list(map=map,max_x2=max_x2,min_x2=min_x2,max_y2=max_y2,min_y2=min_y2))
}




#########################################
ui <- tabsetPanel(id = "tabs",
                  tabPanel("Maps",fluidPage(fluidRow(
                    column(9,
                         #  tags$head(tags$style(HTML("#map {pointer-events: none;}"))),
                           selectInput("inSelect", "area",sort(unique(  squaresinit$extent )),"Ennerdale"),
                           leafletOutput("map",height = 800,width="100%")
                    ),
                    #####################
                    column(3,
                           verticalLayout(sliderInput("SliderMain","Total Carbon:",min=0,max=870,value=800),
                                          textOutput("SoilCarbonNotIncluded"),
                                          sliderInput("BioSlider","Average Red Squirrel % increase over all cells:",min=0,max=36,value=25),
                                          sliderInput("AreaSlider","Total Area Planted (m^2):",min=0,max=25,value=15),
                                          sliderInput("VisitsSlider","Average Number of Visitors on cells:",min=0,max=750,value=400)
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
 
  ClickedVector<-reactiveVal(NULL)
  AreaSelected0<-reactiveVal(NULL)
  CarbonSelected0<-reactiveVal(NULL)
RedSquirrelSelected0<-reactiveVal(NULL)
VisitsSelected0<-reactiveVal(NULL)
  
  
  
  observeEvent(input$inSelect,{
    SelectedDropdown<-input$inSelect
    ClickedVector(NULL)
    AreaSelected0(NULL)
    CarbonSelected0(NULL)
    RedSquirrelSelected0(NULL)
    VisitsSelected0(NULL)
    if(SelectedDropdown=="Ennerdale"){SelectedSquares<-ennSq[PositionVecEnnerdale,]}else
    {SelectedSquares<-squares[squares$extent==SelectedDropdown,]}
      ClickedVector(rep(0,dim(SelectedSquares)[1]))
      
      
      AreaSelected<-Area[squaresinit$extent==SelectedDropdown]
      if(SelectedDropdown=="Ennerdale"){AreaSelected<-AreaSelected[PositionVecEnnerdale]}
      AreaSelected0(AreaSelected)

      if(SelectedDropdown=="Ennerdale"){CarbonSelected<-CarbonEnn}else{CarbonSelected<-sample(CarbonEnn,length(AreaSelected),replace=T)}
      CarbonSelected<-(CarbonSelected*AreaSelected)/1e6
      CarbonSelected0(CarbonSelected)
      
      if(SelectedDropdown=="Ennerdale"){RedSquirrelSelected<-redSquirelEnnerdale}else{RedSquirrelSelected<-sample(redSquirelEnnerdale,length(AreaSelected),replace=T)}
      RedSquirrelSelected0(RedSquirrelSelected)
      
      if(SelectedDropdown=="Ennerdale"){VisitsSelected<-NbVisitsEnn}else{VisitsSelected<-sample(NbVisitsEnn,length(AreaSelected),replace=T)}
      VisitsSelected0(VisitsSelected)
            
      updateSliderInput(session, "SliderMain", max = trunc(sum(CarbonSelected)),value=trunc(sum(CarbonSelected)/2))
      updateSliderInput(session, "BioSlider", max = trunc(mean(RedSquirrelSelected)),value=trunc(mean(RedSquirrelSelected)/2))
      updateSliderInput(session, "AreaSlider", max = trunc(sum(AreaSelected)),value=trunc(sum(AreaSelected)/2))
      updateSliderInput(session, "VisitsSlider", max = trunc(mean(VisitsSelected)),value=trunc(mean(VisitsSelected)/2))
      
      
      })
  
  
  #################################
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
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main")
    map<-calcBaseMap$map
   ##########################################################
    AreaSelected<-AreaSelected0()
    CarbonSelected<-CarbonSelected0()
    RedSquirrelSelected<-RedSquirrelSelected0()
    VisitsSelected<-VisitsSelected0()
    
  #  ######################### At the moment, if "Ennerdale, then use enn results from Jules, otherwise
    SelectedSimMat<-simul636[,1:length(SavedVec)]
    
    SVMAT<-t(matrix(SavedVec,length(SavedVec),dim(SelectedSimMat)[1]))
    CarbonMAT<-t(matrix(CarbonSelected,length(SavedVec),dim(SelectedSimMat)[1]))
    RedSquirrelMAT<-t(matrix(as.numeric(RedSquirrelSelected),length(SavedVec),dim(SelectedSimMat)[1]))
    AreaMAT<-t(matrix(AreaSelected,length(SavedVec),dim(SelectedSimMat)[1]))
    VisitsMAT<-t(matrix(as.numeric(VisitsSelected),length(SavedVec),dim(SelectedSimMat)[1]))
    
    SelectedSimMat<-data.frame(1*(SelectedSimMat|SVMAT))
  #  #######################################
    SelecTargetCarbon<-input$SliderMain
    SelecTargetBio<-input$BioSlider
    SelecTargetArea<-input$AreaSlider
   SelecTargetVisits<-input$VisitsSlider
    
   SelectedSimMat2<-data.frame(SelectedSimMat,
                              carbon=rowSums(SelectedSimMat*CarbonMAT),
                               redsquirel=rowMeans(SelectedSimMat*RedSquirrelMAT),
                               Area=rowSums(SelectedSimMat*AreaMAT),
                              Visits=rowMeans(SelectedSimMat*(VisitsMAT)))
    
  SubsetMeetTargets<-SelectedSimMat2[(SelectedSimMat2$carbon>=SelecTargetCarbon)&
                              (SelectedSimMat2$redsquirel>=SelecTargetBio)&
                              (SelectedSimMat2$Area>=SelecTargetArea)&
                              (SelectedSimMat2$Visits>=SelecTargetVisits),]
  if(dim(SubsetMeetTargets)[1]>0){
  DistSliderCarbon<-(SubsetMeetTargets$carbon-SelecTargetCarbon)/(max(SelectedSimMat2$carbon)-min(SelectedSimMat2$carbon))
  DistSliderBio<-(SubsetMeetTargets$redsquirel-SelecTargetBio)/(max(SelectedSimMat2$redsquirel)-min(SelectedSimMat2$redsquirel))
  DistSliderArea<-(SubsetMeetTargets$Area-SelecTargetArea)/(max(SelectedSimMat2$Area)-min(SelectedSimMat2$Area))
  DistSliderVisits<-(SubsetMeetTargets$Visits-SelecTargetVisits)/(max(SelectedSimMat2$Visits)-min(SelectedSimMat2$Visits))
  
  SelecdMinRows<-which((DistSliderCarbon+DistSliderBio+DistSliderArea+DistSliderVisits)==min(DistSliderCarbon+DistSliderBio+DistSliderArea+DistSliderVisits))
  SelectedMins<-SubsetMeetTargets[SelecdMinRows,]
  SelecRow<-which.min(rowSums(SelectedMins[1:length(SavedVec),]))
  SwitchedOnCells<-SelectedMins[SelecRow,1:length(SavedVec)]
  SELL<-(sqconv$extent==SelectedDropdown)
  SelectedTreeCarbon<-SelectedMins[SelecRow,]$carbon
  SelectedBio<-SelectedMins[SelecRow,]$redsquirel
  SelectedArea<-SelectedMins[SelecRow,]$Area
  SelectedVisits<-SelectedMins[SelecRow,]$Visits
  
  
    
  sellng<-sqlng[SELL,]
  sellat<-sqlat[SELL,]
  if(SelectedDropdown=="Ennerdale"){sellng<-sellng[PositionVecEnnerdale,];sellat<-sellat[PositionVecEnnerdale,]}
  for (iii in 1:length(SwitchedOnCells)){
      if(SavedVec[iii]==1){map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,],layerId =paste0("Square",iii),color ="red")}
      else{
       if(SwitchedOnCells[iii]==1){
        map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,],layerId =paste0("Square",iii))}
      }
      }
    
    map<-map%>%  
      addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"<br>
                                 Actual Area Planted:",round(SelectedArea,2),"<br>
                                 Actual Average Vists:",round(SelectedVisits,2),
                               "</p>"), position = "topright")

  }else{ map<-map%>%  
    addControl(html = paste0("<p> Targets Cannot be met</p>"), position = "topright")
  }
    map
  })
  #############################################
  output$map2<-renderLeaflet({
    SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#input$inSelect#
    map<-BaseMap(SelectedDropdown,layerId="main2")$map
    
    
    SavedRVs<-randomValues()
    SwitchedOnCells<-CornerSets[[1]][SavedRVs[1],1:47]
    
    SelectedTreeCarbon<-CornerSets[[1]][SavedRVs[1],]$carbon
    SelectedBio<-CornerSets[[1]][SavedRVs[1],]$redsquirel
    
    
    SELL<-(sqconv$extent==SelectedDropdown)
    sellng<-sqlng[SELL,]
    sellat<-sqlat[SELL,]
    
    for (iii in 1:dim(sellng)[1]){
      if(iii%in%(PositionVecEnnerdale*SwitchedOnCells)){
        map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
    
    
    
    map<-map%>%  
      addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
    map
  })
  #############################################
  output$map3<-renderLeaflet({
    SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#input$inSelect#
    map<-BaseMap(SelectedDropdown,layerId="main3")$map
    
    
    SavedRVs<-randomValues()
    SwitchedOnCells<-CornerSets[[2]][SavedRVs[2],1:47]
    
    SelectedTreeCarbon<-CornerSets[[2]][SavedRVs[2],]$carbon
    SelectedBio<-CornerSets[[2]][SavedRVs[2],]$redsquirel
    
    
    SELL<-(sqconv$extent==SelectedDropdown)
    sellng<-sqlng[SELL,]
    sellat<-sqlat[SELL,]
    
    for (iii in 1:dim(sellng)[1]){
      if(iii%in%(PositionVecEnnerdale*SwitchedOnCells)){
        map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
    
    
    
    map<-map%>%  
      addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
    map
  })
  
  #############################################
  output$map4<-renderLeaflet({
    SelectedDropdown<-input$inSelect#"Abbeyford"#"Ennerdale"#input$inSelect#
    map<-BaseMap(SelectedDropdown,layerId="main4")$map
    
    #if(input$chooseGrouping=="carbon level"){
    
    SavedRVs<-randomValues()
    SwitchedOnCells<-CornerSets[[3]][SavedRVs[3],1:47]
    SelectedTreeCarbon<-CornerSets[[3]][SavedRVs[3],]$carbon
    SelectedBio<-CornerSets[[3]][SavedRVs[3],]$redsquirel
    
    
    SELL<-(sqconv$extent==SelectedDropdown)
    sellng<-sqlng[SELL,]
    sellat<-sqlat[SELL,]
    
    for (iii in 1:dim(sellng)[1]){
      if(iii%in%(PositionVecEnnerdale*SwitchedOnCells)){
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
    map<-BaseMap(SelectedDropdown,layerId="main5")$map
    
    # if(input$chooseGrouping=="carbon level"){
    
    SavedRVs<-randomValues()
    SwitchedOnCells<-CornerSets[[4]][SavedRVs[4],1:47]
    
    SelectedTreeCarbon<-CornerSets[[4]][SavedRVs[4],]$carbon
    SelectedBio<-CornerSets[[4]][SavedRVs[4],]$redsquirel
    
    
    SELL<-(sqconv$extent==SelectedDropdown)
    sellng<-sqlng[SELL,]
    sellat<-sqlat[SELL,]
    
    for (iii in 1:dim(sellng)[1]){
      if(iii%in%(PositionVecEnnerdale*SwitchedOnCells)){
        map<-addPolygons(map,lng= sellng[iii,],lat= sellat[iii,])}  }
    
    #}
    
    
    map<-map%>%  
      addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"</p>"), position = "topright")
    map
  })
  
  
  
  
}

shinyApp(ui, server)

