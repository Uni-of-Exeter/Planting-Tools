library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(ggplot2)
library(geosphere)
library(feather)
############### Implausibility
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



############## load all shapes
PROJdir<-system.file("proj/proj.db", package = "sf")
PROJdir<-substring(PROJdir,1,nchar(PROJdir)-8)
sf_proj_search_paths(PROJdir)
shfile<-sf::st_read("_exp0308154002.gdb.zip",layer = "INV_BLKDATA")
shconv <- st_transform(shfile, crs = 4326)
FullTable<-data.frame(read.csv("FullTable.csv")[,-1])
FullTableNotAvail<-data.frame(read.csv("FullTableNotAvail.csv")[,-1])
STDMEAN<-0.05
STDSTD<-0.01
simuDF<-read.csv(file="SimuDF2.csv")[,-c(1)]#[,-c(1,49,50)]
simul636<-read.csv(file="Simul636.csv")[,-1]

alphaVEC<-c(0.95,0.5,0.1,0.01)
#QuantilesVEC<-c(0.05,0.25,0.50,0.95)

#IVALSVEC<-c()
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
                           selectInput("inSelect", "area",sort(unique(c(FullTable$extent,FullTableNotAvail$extent))),"Ennerdale"),
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
                                    verticalLayout(verbatimTextOutput("FirstMapTxt"),leafletOutput("map2",height = 400,width="100%"))
                             ),
                             column(5,
                                    verticalLayout(verbatimTextOutput("SecondMapTxt"),leafletOutput("map3",height = 400,width="100%"))     
                             ),
                             column(2, verticalLayout(verbatimTextOutput("TargetText"),
                                                      #  selectInput("chooseGrouping", "Grouping Type:",c("carbon level"),"carbon level"),
                                                      actionButton("random", "Randomize!"))                
                             )
                           ),
                           fluidRow(
                             ################################
                             column(5,  
                                    verticalLayout(verbatimTextOutput("ThirdMapTxt"),leafletOutput("map4",height = 400,width="100%"))
                             ),
                             column(5,
                                    verticalLayout(verbatimTextOutput("FourthMapTxt"),leafletOutput("map5",height = 400,width="100%"))      
                             ),
                             column(2,"")
                           )
                           
                           )
                  )
                  ########################
)



server <- function(input, output, session) {
CarbonSliderVal<- reactive({input$SliderMain})
bioSliderVal<- reactive({input$BioSlider})
AreaSliderVal<- reactive({input$AreaSlider})
VisitsSliderVal<- reactive({input$VisitsSlider})

MinProbChart1<-reactiveVal(rep(0,4))
MaxProbChart1<-reactiveVal(rep(0,4))
MinProbChart2<-reactiveVal(rep(0,4))
MaxProbChart2<-reactiveVal(rep(0,4))
MinProbChart3<-reactiveVal(rep(0,4))
MaxProbChart3<-reactiveVal(rep(0,4))
MinProbChart4<-reactiveVal(rep(0,4))
MaxProbChart4<-reactiveVal(rep(0,4))


  ######################
  output$TargetText<-renderText({paste0("Tree Carbon target: ",as.numeric(CarbonSliderVal()),
                                        "\nAvg red squirrel Inc. target: ",as.numeric(bioSliderVal()),
                                        "\nArea target: ",as.numeric(AreaSliderVal()),
                                        "\nAvg Visits per km^2 target: ",as.numeric(VisitsSliderVal())
                                        
                                        )})
  output$SoilCarbonNotIncluded<-renderText({paste0("Note that Soil carbon is currently not included")})
  output$FirstMapTxt<-renderText({paste0(round(MinProbChart1()[1],4),"\u2264 P(Carbon>Target) \u2264",round(MaxProbChart1()[1],4),";"
                                         ,round(MinProbChart1()[2],4),"\u2264 P(Squirrel>Target) \u2264",round(MaxProbChart1()[2],4),";\n"
                                         ,round(MinProbChart1()[3],4),"\u2264 P(Area>Target) \u2264",round(MaxProbChart1()[3],4),";"
                                         ,round(MinProbChart1()[4],4),"\u2264 P(Visits>Target) \u2264",round(MaxProbChart1()[4],4),";"
                                         )})
  output$SecondMapTxt<-renderText({paste0(round(MinProbChart2()[1],4),"\u2264 P(Carbon>Target) \u2264",round(MaxProbChart2()[1],4),";"
                                               ,round(MinProbChart2()[2],4),"\u2264 P(Squirrel>Target) \u2264",round(MaxProbChart2()[2],4),";\n"
                                               ,round(MinProbChart2()[3],4),"\u2264 P(Area>Target) \u2264",round(MaxProbChart2()[3],4),";"
                                               ,round(MinProbChart2()[4],4),"\u2264 P(Visits>Target) \u2264",round(MaxProbChart2()[4],4),";"
                                 )})
  output$ThirdMapTxt<-renderText({paste0(round(MinProbChart3()[1],4),"\u2264 P(Carbon>Target) \u2264",round(MaxProbChart3()[1],4),";"
                                         ,round(MinProbChart3()[2],4),"\u2264 P(Squirrel>Target) \u2264",round(MaxProbChart3()[2],4),";\n"
                                         ,round(MinProbChart3()[3],4),"\u2264 P(Area>Target) \u2264",round(MaxProbChart3()[3],4),";"
                                         ,round(MinProbChart3()[4],4),"\u2264 P(Visits>Target) \u2264",round(MaxProbChart3()[4],4),";"
  )})
  output$FourthMapTxt<-renderText({paste0(round(MinProbChart4()[1],4),"\u2264 P(Carbon>Target) \u2264",round(MaxProbChart4()[1],4),";"
                                          ,round(MinProbChart4()[2],4),"\u2264 P(Squirrel>Target) \u2264",round(MaxProbChart4()[2],4),";\n"
                                          ,round(MinProbChart4()[3],4),"\u2264 P(Area>Target) \u2264",round(MaxProbChart4()[3],4),";"
                                          ,round(MinProbChart4()[4],4),"\u2264 P(Visits>Target) \u2264",round(MaxProbChart4()[4],4),";"
  )})
  ######################
  randomValue <- eventReactive(input$random, {runif(1)})
  ClickedVector<-reactiveVal(NULL)
  AreaSelected0<-reactiveVal(NULL)
  CarbonSelected0<-reactiveVal(NULL)
  RedSquirrelSelected0<-reactiveVal(NULL)
  VisitsSelected0<-reactiveVal(NULL)
  CarbonSelectedSD0<-reactiveVal(NULL)
  RedSquirrelSelectedSD0<-reactiveVal(NULL)
  VisitsSelectedSD0<-reactiveVal(NULL)
  ######################

  
  
  observeEvent(input$inSelect,{
    SelectedDropdown<-input$inSelect
    ClickedVector(NULL)
    AreaSelected0(NULL)
    CarbonSelected0(NULL)
    RedSquirrelSelected0(NULL)
    VisitsSelected0(NULL)
  
    CarbonSelectedSD0(NULL)
    RedSquirrelSelectedSD0(NULL)
    VisitsSelectedSD0(NULL)  
      
      SelectedSquares<-cbind(extent=FullTable[FullTable$extent==SelectedDropdown,c("extent")],FullTable[FullTable$extent==SelectedDropdown,c("lgn.1","lat.1")])
      
      if(!(dim(SelectedSquares)[1]==0)){
        AreaSelected<-FullTable[FullTable$extent==SelectedDropdown,c("area")]
        CarbonSelected<-(FullTable[FullTable$extent==SelectedDropdown,c("JulesMean")]*AreaSelected)/1e6
        RedSquirrelSelected<-FullTable[FullTable$extent==SelectedDropdown,c("BioMean")]
        VisitsSelected<-FullTable[FullTable$extent==SelectedDropdown,c("VisitsMean")]
        

        CarbonSelectedSD<-(FullTable[FullTable$extent==SelectedDropdown,c("JulesSD")]*AreaSelected)/1e6
        RedSquirrelSelectedSD<-FullTable[FullTable$extent==SelectedDropdown,c("BioSD")]
        VisitsSelectedSD<-FullTable[FullTable$extent==SelectedDropdown,c("VisitsSD")]
        
        
        
        ClickedVector(rep(0,dim(SelectedSquares)[1]))
        AreaSelected0(AreaSelected)
        CarbonSelected0(CarbonSelected)
        RedSquirrelSelected0(RedSquirrelSelected)
        VisitsSelected0(VisitsSelected)
        
        CarbonSelectedSD0(CarbonSelectedSD)
        RedSquirrelSelectedSD0(RedSquirrelSelectedSD)
        VisitsSelectedSD0(VisitsSelectedSD)
        
        updateSliderInput(session, "SliderMain", max = trunc(sum(CarbonSelected)),value=trunc(sum(CarbonSelected)))
        updateSliderInput(session, "BioSlider", max = trunc(mean(RedSquirrelSelected)),value=trunc(mean(RedSquirrelSelected)))
        updateSliderInput(session, "AreaSlider", max = trunc(sum(AreaSelected)),value=trunc(sum(AreaSelected)))
        updateSliderInput(session, "VisitsSlider", max = trunc(mean(VisitsSelected)),value=trunc(mean(VisitsSelected)))
      }
      
      
      
      
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
    if(!is.null(SavedVec)){
    
    AreaSelected<-AreaSelected0()
    CarbonSelected<-CarbonSelected0()
    RedSquirrelSelected<-RedSquirrelSelected0()
    VisitsSelected<-VisitsSelected0()
    
    CarbonSelectedSD<-CarbonSelectedSD0()
    RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
    VisitsSelectedSD<-VisitsSelectedSD0()
    
    if(length(SavedVec)==1){
    SelectedSimMat<-(as.matrix(simul636[,1:length(SavedVec)]))}else{ SelectedSimMat<-simul636[,1:length(SavedVec)]}
    
    SVMAT<-t(matrix(SavedVec,length(SavedVec),dim(SelectedSimMat)[1]))
    CarbonMAT<-t(matrix(CarbonSelected,length(SavedVec),dim(SelectedSimMat)[1]))
    RedSquirrelMAT<-t(matrix(as.numeric(RedSquirrelSelected),length(SavedVec),dim(SelectedSimMat)[1]))
    AreaMAT<-t(matrix(AreaSelected,length(SavedVec),dim(SelectedSimMat)[1]))
    VisitsMAT<-t(matrix(as.numeric(VisitsSelected),length(SavedVec),dim(SelectedSimMat)[1]))
  
    
    CarbonSDMAT<-t(matrix(CarbonSelectedSD,length(SavedVec),dim(SelectedSimMat)[1]))
    RedSquirrelSDMAT<-t(matrix(as.numeric(RedSquirrelSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
    VisitsSDMAT<-t(matrix(as.numeric(VisitsSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
    
      
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
                              Visits=rowMeans(SelectedSimMat*(VisitsMAT)),
                              carbonSD=sqrt(rowSums(SelectedSimMat*(CarbonSDMAT^2))),
                              redsquirelSD=sqrt(rowSums(SelectedSimMat*(RedSquirrelSDMAT^2)))/length(SavedVec),
                              VisitsSD=sqrt(rowSums(SelectedSimMat*(VisitsSDMAT^2)))/length(SavedVec)
   ) 
   
   
   
   Icalc<-MultiImpl(TargetsVec=c(SelecTargetCarbon,SelecTargetBio,SelecTargetArea,SelecTargetVisits),
                    EYMat=data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits),
                    SDYMat=data.frame(SelectedSimMat2$carbonSD,SelectedSimMat2$redsquirelSD,rep(0,length(SelectedSimMat2$Area)),SelectedSimMat2$VisitsSD),
                    alpha=0.05,
                    tolVec=c(4,2,100,2))
   
   
   
   
  SubsetMeetTargets<-SelectedSimMat2[(SelectedSimMat2$carbon>=SelecTargetCarbon)&
                              (SelectedSimMat2$redsquirel>=SelecTargetBio)&
                              (SelectedSimMat2$Area>=SelecTargetArea)&
                              (SelectedSimMat2$Visits>=SelecTargetVisits),]
  
  
  SubsetMeetTargets<-SelectedSimMat2[Icalc$NROYTotal,]
  
  if(dim(SubsetMeetTargets)[1]>0){
  if(max(SelectedSimMat2$carbon)!=min(SelectedSimMat2$carbon)){
  DistSliderCarbon<-(SubsetMeetTargets$carbon-SelecTargetCarbon)/(max(SelectedSimMat2$carbon)-min(SelectedSimMat2$carbon))}else{
    DistSliderCarbon<-(SubsetMeetTargets$carbon-SelecTargetCarbon)/(max(SelectedSimMat2$carbon))
  }
    if(max(SelectedSimMat2$redsquirel)!=min(SelectedSimMat2$redsquirel)){
      
  DistSliderBio<-(SubsetMeetTargets$redsquirel-SelecTargetBio)/(max(SelectedSimMat2$redsquirel)-min(SelectedSimMat2$redsquirel))}else{
    DistSliderBio<-(SubsetMeetTargets$redsquirel-SelecTargetBio)/(max(SelectedSimMat2$redsquirel))
  }
    if(max(SelectedSimMat2$Area)!=min(SelectedSimMat2$Area)){
  DistSliderArea<-(SubsetMeetTargets$Area-SelecTargetArea)/(max(SelectedSimMat2$Area)-min(SelectedSimMat2$Area))}else{
    DistSliderArea<-(SubsetMeetTargets$Area-SelecTargetArea)/(max(SelectedSimMat2$Area))  
  }
    if(max(SelectedSimMat2$Visits)!=min(SelectedSimMat2$Visits)){
  DistSliderVisits<-(SubsetMeetTargets$Visits-SelecTargetVisits)/(max(SelectedSimMat2$Visits)-min(SelectedSimMat2$Visits))}else{
    DistSliderVisits<-(SubsetMeetTargets$Visits-SelecTargetVisits)/(max(SelectedSimMat2$Visits))  
  }
  
  SelecdMinRows<-which((DistSliderCarbon+DistSliderBio+DistSliderArea+DistSliderVisits)==min(DistSliderCarbon+DistSliderBio+DistSliderArea+DistSliderVisits))
  SelectedMins<-SubsetMeetTargets[SelecdMinRows,]
  SelecRow<-which.min(rowSums(SelectedMins[1:length(SavedVec),]))
  SwitchedOnCells<-SelectedMins[SelecRow,1:length(SavedVec)]
  
  SELL<-(FullTable$extent==SelectedDropdown)
  if(!is.null(SELL)){
  SelectedTreeCarbon<-SelectedMins[SelecRow,]$carbon
  SelectedBio<-SelectedMins[SelecRow,]$redsquirel
  SelectedArea<-SelectedMins[SelecRow,]$Area
  SelectedVisits<-SelectedMins[SelecRow,]$Visits
  
  SelectedTreeCarbonSD<-SelectedMins[SelecRow,]$carbonSD
  SelectedBioSD<-SelectedMins[SelecRow,]$redsquirelSD
  SelectedVisitsSD<-SelectedMins[SelecRow,]$VisitsSD
  
  
    
  sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
  sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
  for (iii in 1:length(SwitchedOnCells)){
      if(SavedVec[iii]==1){map<-addPolygons(map,lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
      else{
       if(SwitchedOnCells[iii]==1){
        map<-addPolygons(map,lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
      }
  }
  map<-map%>%  
    addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Actual Area Planted:",round(SelectedArea,2),"<br>
                                 Actual Average Vists:",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                             "</p>"), position = "topright")
  
  }else{ map<-map%>%  
    addControl(html = paste0("<p> Targets Cannot be met</p>"), position = "topright")
  }
  }
    }
  SELLNOTAVAIL<-(FullTableNotAvail$extent==SelectedDropdown)
  if(!is.null(SELLNOTAVAIL)){
    
    sellngNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
    sellatNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
    for (iii in 1:dim(sellngNotAvail)[1]){
      map<-addPolygons(map,lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
      midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
      midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
      map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
      map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
      
    }
  
      
    
  }
  
  
 
    map
  })
  #############################################
  output$map2<-renderLeaflet({
    
    ##########################
    
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main2")
    map<-calcBaseMap$map
    
    #  SavedVec<-rep(0,47)
    
    ##########################################################
    if(!is.null(SavedVec)){
      
      AreaSelected<-AreaSelected0()
      CarbonSelected<-CarbonSelected0()
      RedSquirrelSelected<-RedSquirrelSelected0()
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      VisitsSelectedSD<-VisitsSelectedSD0()
      
      if(length(SavedVec)==1){
        SelectedSimMat<-(as.matrix(simul636[,1:length(SavedVec)]))}else{ SelectedSimMat<-simul636[,1:length(SavedVec)]}
      
      SVMAT<-t(matrix(SavedVec,length(SavedVec),dim(SelectedSimMat)[1]))
      CarbonMAT<-t(matrix(CarbonSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelMAT<-t(matrix(as.numeric(RedSquirrelSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      AreaMAT<-t(matrix(AreaSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsMAT<-t(matrix(as.numeric(VisitsSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
      CarbonSDMAT<-t(matrix(CarbonSelectedSD,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelSDMAT<-t(matrix(as.numeric(RedSquirrelSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsSDMAT<-t(matrix(as.numeric(VisitsSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
      SelectedSimMat<-data.frame(1*(SelectedSimMat|SVMAT))
      #  #######################################
      SelecTargetCarbon<-input$SliderMain
      SelecTargetBio<-input$BioSlider
      SelecTargetArea<-input$AreaSlider
      SelecTargetVisits<-input$VisitsSlider
      #SelecTargetCarbon<-240;      SelecTargetBio<-19;SelecTargetArea<-13890596;SelecTargetVisits<-17
      
      
      SelectedSimMat2<-data.frame(SelectedSimMat,
                                  carbon=rowSums(SelectedSimMat*CarbonMAT),
                                  redsquirel=rowMeans(SelectedSimMat*RedSquirrelMAT),
                                  Area=rowSums(SelectedSimMat*AreaMAT),
                                  Visits=rowMeans(SelectedSimMat*(VisitsMAT)),
                                  carbonSD=sqrt(rowSums(SelectedSimMat*(CarbonSDMAT^2))),
                                  redsquirelSD=sqrt(rowSums(SelectedSimMat*(RedSquirrelSDMAT^2)))/length(SavedVec),
                                  VisitsSD=sqrt(rowSums(SelectedSimMat*(VisitsSDMAT^2)))/length(SavedVec)
      ) 
      
      Icalc<-MultiImpl(TargetsVec=c(SelecTargetCarbon,SelecTargetBio,SelecTargetArea,SelecTargetVisits),
                       EYMat=data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits),
                       SDYMat=data.frame(SelectedSimMat2$carbonSD,SelectedSimMat2$redsquirelSD,rep(0,length(SelectedSimMat2$Area)),SelectedSimMat2$VisitsSD),
                       alpha=0.05,
                       tolVec=c(4,2,100,2))
      
      #SELVAR<-which.max(apply(Icalc$IVEC,2,max))
      
      PROBAMAT<-1-pnorm(Icalc$IVEC)
      MAXProbVEC<-apply(PROBAMAT,2,max)
      MINProbVEC<-apply(PROBAMAT,2,min)
      
      ActualAlpha<-pmin(alphaVEC[1],MAXProbVEC)
      
      #QuantilesIcalc<-c(min(Icalc$IVEC[,SELVAR]),quantile(Icalc$IVEC[,SELVAR],c(QuantilesVEC[1])))
      
      CONDPROBA<-(PROBAMAT[,1]>=ActualAlpha[1])&(PROBAMAT[,2]>=ActualAlpha[2])&(PROBAMAT[,3]>=ActualAlpha[3])&(PROBAMAT[,4]>=ActualAlpha[4])
      
            
      SubsetMeetTargets<-SelectedSimMat2[CONDPROBA,]
      SelIMAT<-Icalc$IVEC[CONDPROBA,]
      if(!is.null(dim(SelIMAT))){
        MinProbChart1(1-pnorm(apply(SelIMAT,2,max)))
        MaxProbChart1(1-pnorm(apply(SelIMAT,2,min)))}else{MinProbChart1(1-pnorm(SelIMAT));MaxProbChart1(1-pnorm(SelIMAT))}
      
      
      
      SavedRVs<-randomValue()
      LSMT<-dim(SubsetMeetTargets)[1]
      SelectedLine<-SubsetMeetTargets[as.integer(trunc(SavedRVs[1]*LSMT)+1),]
      
      SwitchedOnCells<-SelectedLine[1:length(SavedVec)]
      SelectedTreeCarbon<-SelectedLine$carbon
      SelectedBio<-SelectedLine$redsquirel
      SelectedArea<-SelectedLine$Area
      SelectedVisits<-SelectedLine$Visits
    
      SelectedTreeCarbonSD<-SelectedLine$carbonSD
      SelectedBioSD<-SelectedLine$redsquirelSD
      SelectedVisitsSD<-SelectedLine$VisitsSD
      
        
     
         SELL<-(FullTable$extent==SelectedDropdown)
        if(!is.null(SELL)){
          sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
          sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
          for (iii in 1:length(SwitchedOnCells)){
            if(SavedVec[iii]==1){map<-addPolygons(map,lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
            else{
              if(SwitchedOnCells[iii]==1){
                map<-addPolygons(map,lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
            }
         }
   }
    
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Actual Area Planted:",round(SelectedArea,2),"<br>
                                 Actual Average Vists:",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                 "</p>"), position = "topright")
      }
      

    SELLNOTAVAIL<-(FullTableNotAvail$extent==SelectedDropdown)
    if(!is.null(SELLNOTAVAIL)){
      
      
      
      sellngNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
      sellatNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
      for (iii in 1:dim(sellngNotAvail)[1]){
        map<-addPolygons(map,lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
        midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
        midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
        
      }
    }
    
    
    map
  })
  #############################################
  output$map3<-renderLeaflet({
    
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main3")
    map<-calcBaseMap$map
    
    #  SavedVec<-rep(0,47)
    
    ##########################################################
    if(!is.null(SavedVec)){
      
      AreaSelected<-AreaSelected0()
      CarbonSelected<-CarbonSelected0()
      RedSquirrelSelected<-RedSquirrelSelected0()
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      VisitsSelectedSD<-VisitsSelectedSD0()
      
      if(length(SavedVec)==1){
        SelectedSimMat<-(as.matrix(simul636[,1:length(SavedVec)]))}else{ SelectedSimMat<-simul636[,1:length(SavedVec)]}
      
      SVMAT<-t(matrix(SavedVec,length(SavedVec),dim(SelectedSimMat)[1]))
      CarbonMAT<-t(matrix(CarbonSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelMAT<-t(matrix(as.numeric(RedSquirrelSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      AreaMAT<-t(matrix(AreaSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsMAT<-t(matrix(as.numeric(VisitsSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
      CarbonSDMAT<-t(matrix(CarbonSelectedSD,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelSDMAT<-t(matrix(as.numeric(RedSquirrelSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsSDMAT<-t(matrix(as.numeric(VisitsSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
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
                                  Visits=rowMeans(SelectedSimMat*(VisitsMAT)),
                                  carbonSD=sqrt(rowSums(SelectedSimMat*(CarbonSDMAT^2))),
                                  redsquirelSD=sqrt(rowSums(SelectedSimMat*(RedSquirrelSDMAT^2)))/length(SavedVec),
                                  VisitsSD=sqrt(rowSums(SelectedSimMat*(VisitsSDMAT^2)))/length(SavedVec)
      ) 
      
      Icalc<-MultiImpl(TargetsVec=c(SelecTargetCarbon,SelecTargetBio,SelecTargetArea,SelecTargetVisits),
                       EYMat=data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits),
                       SDYMat=data.frame(SelectedSimMat2$carbonSD,SelectedSimMat2$redsquirelSD,rep(0,length(SelectedSimMat2$Area)),SelectedSimMat2$VisitsSD),
                       alpha=0.05,
                       tolVec=c(4,2,100,2))
      
     
      
      
    #  SELVAR<-which.max(apply(Icalc$IVEC,2,max))
      
     # QuantilesIcalc<-quantile(Icalc$IVEC[,SELVAR],c(QuantilesVEC[1],QuantilesVEC[2]))

      
    #  QuantilesIcalc<-c(-2,-1)
      
      
      
      PROBAMAT<-1-pnorm(Icalc$IVEC)
      MAXProbVEC<-apply(PROBAMAT,2,max)
      MINProbVEC<-apply(PROBAMAT,2,min)
      
      ActualAlphaUpperTarget<-pmin(alphaVEC[1],MAXProbVEC)
      TempAlphaUpper<-rep(1,4)
      ActualAlphaLower<-pmax(alphaVEC[2],MINProbVEC)
      #ActualAlphaLower<-rep(0,4)
      
      #QuantilesIcalc<-c(min(Icalc$IVEC[,SELVAR]),quantile(Icalc$IVEC[,SELVAR],c(QuantilesVEC[1])))
      ContinueReduction<-rep(T,4)
        
        while(sum(ContinueReduction)>0){
          for (aa in 1:4){
            if(ContinueReduction[aa]){
                 TempAlphaUpper[aa]<-max(ActualAlphaUpperTarget[aa],TempAlphaUpper[aa]-0.01)
                if(TempAlphaUpper[aa]==ActualAlphaUpperTarget[aa]){ContinueReduction[aa]<-F}
                TestCondProba<-(PROBAMAT[,1]>=ActualAlphaLower[1])&(PROBAMAT[,1]<=TempAlphaUpper[1])&
                (PROBAMAT[,2]>=ActualAlphaLower[2])&(PROBAMAT[,2]<=TempAlphaUpper[2])&
                  (PROBAMAT[,3]>=ActualAlphaLower[3])& (PROBAMAT[,3]<=TempAlphaUpper[3])&
                (PROBAMAT[,4]>=ActualAlphaLower[4])&(PROBAMAT[,4]<=TempAlphaUpper[4])
                if(sum(TestCondProba)==0){TempAlphaUpper[aa]<-TempAlphaUpper[aa]+0.01;ContinueReduction[aa]<-F}
                  
              }
            }
          }
          CONDPROBA<-(PROBAMAT[,1]>=ActualAlphaLower[1])&(PROBAMAT[,1]<=TempAlphaUpper[1])&
            (PROBAMAT[,2]>=ActualAlphaLower[2])&(PROBAMAT[,2]<=TempAlphaUpper[2])&
            (PROBAMAT[,3]>=ActualAlphaLower[3])& (PROBAMAT[,3]<=TempAlphaUpper[3])&
            (PROBAMAT[,4]>=ActualAlphaLower[4])&(PROBAMAT[,4]<=TempAlphaUpper[4])
          
      if(sum(CONDPROBA)==0){CONDPROBA[1]<-T}  
      
      SubsetMeetTargets<-SelectedSimMat2[CONDPROBA,]
      SelIMAT<-Icalc$IVEC[CONDPROBA,]
      
      
      
      
      
      
      
            
      #SubsetMeetTargets<-SelectedSimMat2[(Icalc$IVEC[,SELVAR]<=QuantilesIcalc[2])&(Icalc$IVEC[,SELVAR]>=QuantilesIcalc[1]),]
      #SelIMAT<-Icalc$IVEC[(Icalc$IVEC[,SELVAR]<=QuantilesIcalc[2])&(Icalc$IVEC[,SELVAR]>=QuantilesIcalc[1]),]
      if(!is.null(dim(SelIMAT))){
        MinProbChart2(1-pnorm(apply(SelIMAT,2,max)))
        MaxProbChart2(1-pnorm(apply(SelIMAT,2,min)))}else{MinProbChart2(1-pnorm(SelIMAT));MaxProbChart2(1-pnorm(SelIMAT))}
      
      
      
      SavedRVs<-randomValue()
      LSMT<-dim(SubsetMeetTargets)[1]
      SelectedLine<-SubsetMeetTargets[as.integer(trunc(SavedRVs[1]*LSMT)+1),]
      
      SwitchedOnCells<-SelectedLine[1:length(SavedVec)]
      SelectedTreeCarbon<-SelectedLine$carbon
      SelectedBio<-SelectedLine$redsquirel
      SelectedArea<-SelectedLine$Area
      SelectedVisits<-SelectedLine$Visits
      
      SelectedTreeCarbonSD<-SelectedLine$carbonSD
      SelectedBioSD<-SelectedLine$redsquirelSD
      SelectedVisitsSD<-SelectedLine$VisitsSD
      
      
      
      SELL<-(FullTable$extent==SelectedDropdown)
      if(!is.null(SELL)){
        sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
        sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
        for (iii in 1:length(SwitchedOnCells)){
          if(SavedVec[iii]==1){map<-addPolygons(map,lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
          else{
            if(SwitchedOnCells[iii]==1){
              map<-addPolygons(map,lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
          }
        }
      }
      
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Actual Area Planted:",round(SelectedArea,2),"<br>
                                 Actual Average Vists:",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                 "</p>"), position = "topright")
    }
    
    
    SELLNOTAVAIL<-(FullTableNotAvail$extent==SelectedDropdown)
    if(!is.null(SELLNOTAVAIL)){
      
      
      
      sellngNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
      sellatNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
      for (iii in 1:dim(sellngNotAvail)[1]){
        map<-addPolygons(map,lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
        midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
        midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
        
      }
    }
    
    
    map
  })
  
  #############################################
  output$map4<-renderLeaflet({
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main4")
    map<-calcBaseMap$map
    
    #  SavedVec<-rep(0,47)
    
    ##########################################################
    if(!is.null(SavedVec)){
      
      AreaSelected<-AreaSelected0()
      CarbonSelected<-CarbonSelected0()
      RedSquirrelSelected<-RedSquirrelSelected0()
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      VisitsSelectedSD<-VisitsSelectedSD0()
      
      if(length(SavedVec)==1){
        SelectedSimMat<-(as.matrix(simul636[,1:length(SavedVec)]))}else{ SelectedSimMat<-simul636[,1:length(SavedVec)]}
      
      SVMAT<-t(matrix(SavedVec,length(SavedVec),dim(SelectedSimMat)[1]))
      CarbonMAT<-t(matrix(CarbonSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelMAT<-t(matrix(as.numeric(RedSquirrelSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      AreaMAT<-t(matrix(AreaSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsMAT<-t(matrix(as.numeric(VisitsSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
      CarbonSDMAT<-t(matrix(CarbonSelectedSD,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelSDMAT<-t(matrix(as.numeric(RedSquirrelSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsSDMAT<-t(matrix(as.numeric(VisitsSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
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
                                  Visits=rowMeans(SelectedSimMat*(VisitsMAT)),
                                  carbonSD=sqrt(rowSums(SelectedSimMat*(CarbonSDMAT^2))),
                                  redsquirelSD=sqrt(rowSums(SelectedSimMat*(RedSquirrelSDMAT^2)))/length(SavedVec),
                                  VisitsSD=sqrt(rowSums(SelectedSimMat*(VisitsSDMAT^2)))/length(SavedVec)
      ) 
      
      Icalc<-MultiImpl(TargetsVec=c(SelecTargetCarbon,SelecTargetBio,SelecTargetArea,SelecTargetVisits),
                       EYMat=data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits),
                       SDYMat=data.frame(SelectedSimMat2$carbonSD,SelectedSimMat2$redsquirelSD,rep(0,length(SelectedSimMat2$Area)),SelectedSimMat2$VisitsSD),
                       alpha=0.05,
                       tolVec=c(4,2,100,2))
      
      
      
      
      PROBAMAT<-1-pnorm(Icalc$IVEC)
      MAXProbVEC<-apply(PROBAMAT,2,max)
      MINProbVEC<-apply(PROBAMAT,2,min)
      
      ActualAlphaUpperTarget<-pmin(alphaVEC[2],MAXProbVEC)
      TempAlphaUpper<-rep(1,4)
      ActualAlphaLower<-pmax(alphaVEC[3],MINProbVEC)
      #ActualAlphaLower<-rep(0,4)
      
      #QuantilesIcalc<-c(min(Icalc$IVEC[,SELVAR]),quantile(Icalc$IVEC[,SELVAR],c(QuantilesVEC[1])))
      ContinueReduction<-rep(T,4)
      
      while(sum(ContinueReduction)>0){
        
        for (aa in 1:4){
          if(ContinueReduction[aa]){
            TempAlphaUpper[aa]<-max(ActualAlphaUpperTarget[aa],TempAlphaUpper[aa]-0.01)
            if(TempAlphaUpper[aa]==ActualAlphaUpperTarget[aa]){ContinueReduction[aa]<-F}
            TestCondProba<-(PROBAMAT[,1]>=ActualAlphaLower[1])&(PROBAMAT[,1]<=TempAlphaUpper[1])&
              (PROBAMAT[,2]>=ActualAlphaLower[2])&(PROBAMAT[,2]<=TempAlphaUpper[2])&
              (PROBAMAT[,3]>=ActualAlphaLower[3])& (PROBAMAT[,3]<=TempAlphaUpper[3])&
              (PROBAMAT[,4]>=ActualAlphaLower[4])&(PROBAMAT[,4]<=TempAlphaUpper[4])
            if(sum(TestCondProba)==0){TempAlphaUpper[aa]<-TempAlphaUpper[aa]+0.01;ContinueReduction[aa]<-F}
            
          }
          
        }}
      CONDPROBA<-(PROBAMAT[,1]>=ActualAlphaLower[1])&(PROBAMAT[,1]<=TempAlphaUpper[1])&
        (PROBAMAT[,2]>=ActualAlphaLower[2])&(PROBAMAT[,2]<=TempAlphaUpper[2])&
        (PROBAMAT[,3]>=ActualAlphaLower[3])& (PROBAMAT[,3]<=TempAlphaUpper[3])&
        (PROBAMAT[,4]>=ActualAlphaLower[4])&(PROBAMAT[,4]<=TempAlphaUpper[4])
      
      
      if(sum(CONDPROBA)==0){CONDPROBA[1]<-T}  
      
      SubsetMeetTargets<-SelectedSimMat2[CONDPROBA,]
      SelIMAT<-Icalc$IVEC[CONDPROBA,]
      
      
      
      
      
      
      
    #  QuantilesIcalc<-quantile(Icalc$ImTotMax,c(QuantilesVEC[3],QuantilesVEC[4]))
     # IVecSavedVals<-IVecReactive()    
    #  IVecSavedVals[4]<-QuantilesIcalc[2]
    
      #  IVecReactive(IVecSavedVals)
      
      
     # SubsetMeetTargets<-SelectedSimMat2[(Icalc$ImTotMax<=QuantilesIcalc[1]),]
      
      #SELVAR<-which.max(apply(Icalc$IVEC,2,max))
      
    #  QuantilesIcalc<-quantile(Icalc$IVEC[,SELVAR],c(QuantilesVEC[2],QuantilesVEC[3]))
  
     # QuantilesIcalc<-c(-1,1)
          
     # SubsetMeetTargets<-SelectedSimMat2[(Icalc$IVEC[,SELVAR]<=QuantilesIcalc[2])&(Icalc$IVEC[,SELVAR]>=QuantilesIcalc[1]),]
    #  SelIMAT<-Icalc$IVEC[(Icalc$IVEC[,SELVAR]<=QuantilesIcalc[2])&(Icalc$IVEC[,SELVAR]>=QuantilesIcalc[1]),]
      if(!is.null(dim(SelIMAT))){
        MinProbChart3(1-pnorm(apply(SelIMAT,2,max)))
        MaxProbChart3(1-pnorm(apply(SelIMAT,2,min)))}else{MinProbChart3(1-pnorm(SelIMAT));MaxProbChart3(1-pnorm(SelIMAT))}
      
      
      SavedRVs<-randomValue()
      LSMT<-dim(SubsetMeetTargets)[1]
      SelectedLine<-SubsetMeetTargets[as.integer(trunc(SavedRVs[1]*LSMT)+1),]
      
      SwitchedOnCells<-SelectedLine[1:length(SavedVec)]
      SelectedTreeCarbon<-SelectedLine$carbon
      SelectedBio<-SelectedLine$redsquirel
      SelectedArea<-SelectedLine$Area
      SelectedVisits<-SelectedLine$Visits
      
      SelectedTreeCarbonSD<-SelectedLine$carbonSD
      SelectedBioSD<-SelectedLine$redsquirelSD
      SelectedVisitsSD<-SelectedLine$VisitsSD
      
      
      
      SELL<-(FullTable$extent==SelectedDropdown)
      if(!is.null(SELL)){
        sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
        sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
        for (iii in 1:length(SwitchedOnCells)){
          if(SavedVec[iii]==1){map<-addPolygons(map,lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
          else{
            if(SwitchedOnCells[iii]==1){
              map<-addPolygons(map,lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
          }
        }
      }
      
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Actual Area Planted:",round(SelectedArea,2),"<br>
                                 Actual Average Vists:",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                 "</p>"), position = "topright")
    }
    
    
    SELLNOTAVAIL<-(FullTableNotAvail$extent==SelectedDropdown)
    if(!is.null(SELLNOTAVAIL)){
      
      
      
      sellngNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
      sellatNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
      for (iii in 1:dim(sellngNotAvail)[1]){
        map<-addPolygons(map,lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
        midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
        midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
        
      }
    }
    

    map
  })
  #############################################
  output$map5<-renderLeaflet({
    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main5")
    map<-calcBaseMap$map
    
    
    ##########################################################
    if(!is.null(SavedVec)){
      
      AreaSelected<-AreaSelected0()
      CarbonSelected<-CarbonSelected0()
      RedSquirrelSelected<-RedSquirrelSelected0()
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      VisitsSelectedSD<-VisitsSelectedSD0()
      
      if(length(SavedVec)==1){
        SelectedSimMat<-(as.matrix(simul636[,1:length(SavedVec)]))}else{ SelectedSimMat<-simul636[,1:length(SavedVec)]}
      
      SVMAT<-t(matrix(SavedVec,length(SavedVec),dim(SelectedSimMat)[1]))
      CarbonMAT<-t(matrix(CarbonSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelMAT<-t(matrix(as.numeric(RedSquirrelSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      AreaMAT<-t(matrix(AreaSelected,length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsMAT<-t(matrix(as.numeric(VisitsSelected),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
      CarbonSDMAT<-t(matrix(CarbonSelectedSD,length(SavedVec),dim(SelectedSimMat)[1]))
      RedSquirrelSDMAT<-t(matrix(as.numeric(RedSquirrelSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      VisitsSDMAT<-t(matrix(as.numeric(VisitsSelectedSD),length(SavedVec),dim(SelectedSimMat)[1]))
      
      
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
                                  Visits=rowMeans(SelectedSimMat*(VisitsMAT)),
                                  carbonSD=sqrt(rowSums(SelectedSimMat*(CarbonSDMAT^2))),
                                  redsquirelSD=sqrt(rowSums(SelectedSimMat*(RedSquirrelSDMAT^2)))/length(SavedVec),
                                  VisitsSD=sqrt(rowSums(SelectedSimMat*(VisitsSDMAT^2)))/length(SavedVec)
      ) 
      
      Icalc<-MultiImpl(TargetsVec=c(SelecTargetCarbon,SelecTargetBio,SelecTargetArea,SelecTargetVisits),
                       EYMat=data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits),
                       SDYMat=data.frame(SelectedSimMat2$carbonSD,SelectedSimMat2$redsquirelSD,rep(0,length(SelectedSimMat2$Area)),SelectedSimMat2$VisitsSD),
                       alpha=0.05,
                       tolVec=c(4,2,100,2))
      
      
      PROBAMAT<-1-pnorm(Icalc$IVEC)
      MAXProbVEC<-apply(PROBAMAT,2,max)
      MINProbVEC<-apply(PROBAMAT,2,min)
      
      ActualAlphaUpperTarget<-pmin(alphaVEC[3],MAXProbVEC)
      TempAlphaUpper<-rep(1.0,4)
      ActualAlphaLower<-pmax(alphaVEC[4],MINProbVEC)
      #ActualAlphaLower<-rep(0,4)
      
      #QuantilesIcalc<-c(min(Icalc$IVEC[,SELVAR]),quantile(Icalc$IVEC[,SELVAR],c(QuantilesVEC[1])))
      ContinueReduction<-rep(T,4)
      
      while(sum(ContinueReduction)>0){
        
        for (aab in 1:4){
          if(ContinueReduction[aab]==T){
            TempAlphaUpper[aab]<-max(ActualAlphaUpperTarget[aab],TempAlphaUpper[aab]-0.01)
            if(TempAlphaUpper[aab]<=ActualAlphaUpperTarget[aab]){ContinueReduction[aab]<-F}
            TestCondProba<-(PROBAMAT[,1]>=ActualAlphaLower[1])&(PROBAMAT[,1]<=TempAlphaUpper[1])&
              (PROBAMAT[,2]>=ActualAlphaLower[2])&(PROBAMAT[,2]<=TempAlphaUpper[2])&
             (PROBAMAT[,3]>=ActualAlphaLower[3])& (PROBAMAT[,3]<=TempAlphaUpper[3])&
              (PROBAMAT[,4]>=ActualAlphaLower[4])&(PROBAMAT[,4]<=TempAlphaUpper[4])
            if(sum(TestCondProba)==0){TempAlphaUpper[aab]<-TempAlphaUpper[aab]+0.01;ContinueReduction[aab]<-F}
          }
          
        }}
      CONDPROBA<-(PROBAMAT[,1]>=ActualAlphaLower[1])&(PROBAMAT[,1]<=TempAlphaUpper[1])&
        (PROBAMAT[,2]>=ActualAlphaLower[2])&(PROBAMAT[,2]<=TempAlphaUpper[2])&
        (PROBAMAT[,3]>=ActualAlphaLower[3])& (PROBAMAT[,3]<=TempAlphaUpper[3])&
        (PROBAMAT[,4]>=ActualAlphaLower[4])&(PROBAMAT[,4]<=TempAlphaUpper[4])
      
      if(sum(CONDPROBA)==0){CONDPROBA[1]<-T}  
      
      SubsetMeetTargets<-SelectedSimMat2[CONDPROBA,]
      SelIMAT<-Icalc$IVEC[CONDPROBA,]
      
      
      
      
      
      #QuantilesIcalc<-quantile(Icalc$ImTotMax,c(QuantilesVEC[4],QuantilesVEC[5]))
     # IVecSavedVals<-IVecReactive()    
      #IVecSavedVals[5]<-QuantilesIcalc[2]
    #  IVecReactive(IVecSavedVals)
      
     # SubsetMeetTargets<-SelectedSimMat2[(Icalc$ImTotMax<=QuantilesIcalc[1]),]
      
      
     # SELVAR<-which.max(apply(Icalc$IVEC,2,max))
      
    #  QuantilesIcalc<-quantile(Icalc$IVEC[,SELVAR],c(QuantilesVEC[3],QuantilesVEC[4]))
 
 #     QuantilesIcalc<-c(1,2)
      
           
     # SubsetMeetTargets<-SelectedSimMat2[(Icalc$IVEC[,SELVAR]<=QuantilesIcalc[2])&(Icalc$IVEC[,SELVAR]>=QuantilesIcalc[1]),]
    #  SelIMAT<-Icalc$IVEC[(Icalc$IVEC[,SELVAR]<=QuantilesIcalc[2])&(Icalc$IVEC[,SELVAR]>=QuantilesIcalc[1]),]
      if(!is.null(dim(SelIMAT))){
      MinProbChart4(1-pnorm(apply(SelIMAT,2,max)))
      MaxProbChart4(1-pnorm(apply(SelIMAT,2,min)))}else{MinProbChart4(1-pnorm(SelIMAT));MaxProbChart4(1-pnorm(SelIMAT))}
      
      
      
      SavedRVs<-randomValue()
      LSMT<-dim(SubsetMeetTargets)[1]
      SelectedLine<-SubsetMeetTargets[as.integer(trunc(SavedRVs[1]*LSMT)+1),]
      
      SwitchedOnCells<-SelectedLine[1:length(SavedVec)]
      SelectedTreeCarbon<-SelectedLine$carbon
      SelectedBio<-SelectedLine$redsquirel
      SelectedArea<-SelectedLine$Area
      SelectedVisits<-SelectedLine$Visits
      
      SelectedTreeCarbonSD<-SelectedLine$carbonSD
      SelectedBioSD<-SelectedLine$redsquirelSD
      SelectedVisitsSD<-SelectedLine$VisitsSD
      
      
      
      SELL<-(FullTable$extent==SelectedDropdown)
      if(!is.null(SELL)){
        sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
        sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
        for (iii in 1:length(SwitchedOnCells)){
          if(SavedVec[iii]==1){map<-addPolygons(map,lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
          else{
            if(SwitchedOnCells[iii]==1){
              map<-addPolygons(map,lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
          }
        }
      }
      
      
      map<-map%>%  
        addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Actual Area Planted:",round(SelectedArea,2),"<br>
                                 Actual Average Vists:",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                 "</p>"), position = "topright")
    }
    
    
    SELLNOTAVAIL<-(FullTableNotAvail$extent==SelectedDropdown)
    if(!is.null(SELLNOTAVAIL)){
      
      
      
      sellngNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
      sellatNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
      for (iii in 1:dim(sellngNotAvail)[1]){
        map<-addPolygons(map,lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
        midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
        midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
        map<-addPolylines(map,lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
        
      }
    }
    
    
    map
  })
  
  
  
  
}

shinyApp(ui, server)


