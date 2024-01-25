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


source("functions.R")

FolderSource<-""
# Sys.setenv(PROJ_LIB="/usr/share/proj")
# Sys.setenv(GDAL_DATA = "/usr/share/proj/")

# load all shapes
# PROJdir<-system.file("proj/proj.db", package = "sf")
# PROJdir<-substring(PROJdir,1,nchar(PROJdir)-8)
# sf_proj_search_paths(PROJdir)
shfile<-sf::st_read(paste0(FolderSource,"_exp0308154002.gdb.zip"),layer = "INV_BLKDATA")
shconv <- st_transform(shfile, crs = 4326)
FullTable<-data.frame(read.csv(paste0(FolderSource,"FullTableResult.csv"))[,-1])
FullTableNotAvail<-data.frame(read.csv(paste0(FolderSource,"FullTableNotAvail.csv"))[,-1])
STDMEAN<-0.05
STDSTD<-0.01
simul636<-read.csv(file=paste0(FolderSource,"Simul636.csv"))[,-1]

alphaLVL<-0.9

MaxRounds<-5

ConvertSample<-sample(1:5000,200)

ui <- fluidPage(useShinyjs(),tabsetPanel(id = "tabs",
                                         tabPanel("Maps",fluidPage(fluidRow(
                                           column(9,
                                                  #  tags$head(tags$style(HTML("#map {pointer-events: none;}"))),
                                                  selectInput("inSelect", "area",sort(unique(c(FullTable$extent,FullTableNotAvail$extent))),"Ennerdale"),
                                                  jqui_resizable(leafletOutput("map",height = 800,width="100%"))
                                           ),
                                           column(3,
                                                  verticalLayout(sliderInput("SliderMain","Tree Carbon Stored (2050):",min=0,max=870,value=800),
                                                                 textOutput("SoilCarbonNotIncluded"),
                                                                 sliderInput("BioSlider","Average Red Squirrel % increase:",min=0,max=36,value=25),
                                                                 sliderInput("AreaSlider","Total Area Planted (km^2):",min=0,max=25,value=15),
                                                                 sliderInput("VisitsSlider","Average Number of Visitors per cell:",min=0,max=750,value=400)
                                                  )))
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
                                                                             #  selectInput("chooseGrouping", "Grouping Type:",c("carbon level"),"carbon level"),
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

server <- function(input, output, session) {
  CarbonSliderVal<- reactive({input$SliderMain})
  bioSliderVal<- reactive({input$BioSlider})
  AreaSliderVal<- reactive({input$AreaSlider})
  VisitsSliderVal<- reactive({input$VisitsSlider})
  
  Text1<-reactiveVal("")
  Text2<-reactiveVal("")
  Text3<-reactiveVal("")
  Text4<-reactiveVal("")
  
  
  
  output$TargetText<-renderText({paste0("Targets:\n", "Tree Carbon: ",as.numeric(CarbonSliderVal()),
                                        "\nRed Squirrel: ",as.numeric(bioSliderVal()),
                                        "\nArea Planted: ",as.numeric(AreaSliderVal()),
                                        "\nVisits/km^2: ",as.numeric(VisitsSliderVal())
  )})
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
  RedSquirrelSelected0<-reactiveVal(NULL)
  VisitsSelected0<-reactiveVal(NULL)
  CarbonSelectedSD0<-reactiveVal(NULL)
  RedSquirrelSelectedSD0<-reactiveVal(NULL)
  VisitsSelectedSD0<-reactiveVal(NULL)
  
  DatBinaryCode0<-reactiveVal(NULL)
  NbRoundsMax<-reactiveVal(0)
  CurrentRound<-reactiveVal(0)
  LinesToCompareReactive<-reactiveVal(0)
  
  VecNbMet0<-reactiveVal(NULL)
  
  output$Trigger<-reactiveVal(TRUE)
  #inputOptions(intput, 'Trigger', suspendWhenHidden = FALSE)
  
  
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
      RedSquirrelSelected<-FullTable[FullTable$extent==SelectedDropdown,c("BioMean_Sciurus_vulgaris")]
      VisitsSelected<-FullTable[FullTable$extent==SelectedDropdown,c("VisitsMean")]
      
      
      CarbonSelectedSD<-(FullTable[FullTable$extent==SelectedDropdown,c("JulesSD")]*AreaSelected)/1e6
      RedSquirrelSelectedSD<-FullTable[FullTable$extent==SelectedDropdown,c("BioSD_Sciurus_vulgaris")]
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
      updateSliderInput(session, "AreaSlider", max = trunc(sum(AreaSelected)/1e6),value=trunc(sum(AreaSelected)/1e6))
      updateSliderInput(session, "VisitsSlider", max = trunc(mean(VisitsSelected)),value=trunc(mean(VisitsSelected)))
    }
    
    
    
    
  })
  
  observeEvent(input$tabs == "Clustering",{
    
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
      RedSquirrelSelected<-RedSquirrelSelected0()
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      VisitsSelectedSD<-VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     RedSquirrelSelected = RedSquirrelSelected,
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     RedSquirrelSelectedSD = RedSquirrelSelectedSD,
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
      
      CONDPROB_AtLeast1<-(PROBAMAT[,1]>=alphaLVL)|(PROBAMAT[,2]>=alphaLVL)|(PROBAMAT[,3]>=alphaLVL)|(PROBAMAT[,4]>=alphaLVL)
      
      #  datAll = data.frame(carbonMet=1*(PROBAMAT[,1]>=alphaLVL),
      ##                     redSquirrel=1*(PROBAMAT[,2]>=alphaLVL),
      #                   Area=1*(PROBAMAT[,3]>=alphaLVL),
      #                  Visits=1*(PROBAMAT[,4]>=alphaLVL))
      AtleastOneDat<-unique(SelectedSimMat2[CONDPROB_AtLeast1,])
      
      datAll=as.matrix(data.frame(carbon=SelectedSimMat2$carbon,
                                  redsquirel=SelectedSimMat2$redsquirel,
                                  Area=SelectedSimMat2$Area,
                                  Visits=SelectedSimMat2$Visits))
      datAll2<-datAll[ConvertSample,]
      
      DatBinaryCode<-paste0(1*(PROBAMAT[,1]>=alphaLVL),1*(PROBAMAT[,2]>=alphaLVL),1*(PROBAMAT[,3]>=alphaLVL),Visits=1*(PROBAMAT[,4]>=alphaLVL))
      
      DatBinaryCode0(DatBinaryCode)
      VecNbMet<-rep(0,length(CONDPROB_AtLeast1))
      VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
                  ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
                  ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
                  ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL)))]<-1
      VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
                  ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
                  ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
                  ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
                  ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
                  ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))
      )]<-2
      VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL))|
                  ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
                  ((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))|
                  ((PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))
      )]<-3
      VecNbMet[(((PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL))
      )]<-4
      
      VecNbMet0(VecNbMet)
      
      priors <- c(prefeR::Normal(125,60),
                  prefeR::Normal(5,3),
                  prefeR::Normal(10,20),
                  prefeR::Normal(8,5)
      )
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
          SelectedTreeCarbon<-SelectedLine[[aai]]$carbon
          SelectedBio<-SelectedLine[[aai]]$redsquirel
          SelectedArea<-SelectedLine[[aai]]$Area
          SelectedVisits<-SelectedLine[[aai]]$Visits
          
          SelectedTreeCarbonSD<-SelectedLine[[aai]]$carbonSD
          SelectedBioSD<-SelectedLine[[aai]]$redsquirelSD
          SelectedVisitsSD<-SelectedLine[[aai]]$VisitsSD
          
          SELL<-(FullTable$extent==SelectedDropdown)
          if(!is.null(SELL)){
            sellng<-FullTable[SELL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
            sellat<-FullTable[SELL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
            for (iii in 1:length(SwitchedOnCells)){
              if(SavedVec[iii]==1){listMaps[[aai]]<-addPolygons(listMaps[[aai]],lng= as.numeric(sellng[iii,]),lat= as.numeric(sellat[iii,]),layerId =paste0("Square",iii),color ="red")}
              else{
                if(SwitchedOnCells[iii]==1){
                  listMaps[[aai]]<-addPolygons(listMaps[[aai]],lng=  as.numeric(sellng[iii,]),lat=  as.numeric(sellat[iii,]),layerId =paste0("Square",iii))}
              }
            }
          }
          
          listMaps[[aai]]<-listMaps[[aai]]%>%
            addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Area Planted: ",round(SelectedArea/1e6,2),"<br>
                                 Visitors: ",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
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
                           AreaSelected0 = AreaSelected0,
                           CarbonSelected0 = CarbonSelected0,
                           RedSquirrelSelected0 = RedSquirrelSelected0,
                           VisitsSelected0 = VisitsSelected0,
                           CarbonSelectedSD0 = CarbonSelectedSD0,
                           RedSquirrelSelectedSD0 = RedSquirrelSelectedSD0,
                           VisitsSelectedSD0 = VisitsSelectedSD0,
                           DatBinaryCode0 = DatBinaryCode0,
                           NbRoundsMax = NbRoundsMax,
                           CurrentRound = CurrentRound,
                           FullTable = FullTable,
                           FullTableNotAvail = FullTableNotAvail,
                           VecNbMet0 = VecNbMet0,
                           shconv = shconv,
                           SelectedSimMatGlobal = SelectedSimMatGlobal,
                           pref = pref)
  })
  
  observeEvent(input$choose2, {
    observe_event_function(choose = 2, # 1 for input$choose1, 2 for input$choose2
                           input = input,
                           output = output,
                           session = session,
                           ConvertSample = ConvertSample,
                           LinesToCompareReactive = LinesToCompareReactive,
                           ClickedVector = ClickedVector,
                           AreaSelected0 = AreaSelected0,
                           CarbonSelected0 = CarbonSelected0,
                           RedSquirrelSelected0 = RedSquirrelSelected0,
                           VisitsSelected0 = VisitsSelected0,
                           CarbonSelectedSD0 = CarbonSelectedSD0,
                           RedSquirrelSelectedSD0 = RedSquirrelSelectedSD0,
                           VisitsSelectedSD0 = VisitsSelectedSD0,
                           DatBinaryCode0 = DatBinaryCode0,
                           NbRoundsMax = NbRoundsMax,
                           CurrentRound = CurrentRound,
                           FullTable = FullTable,
                           FullTableNotAvail = FullTableNotAvail,
                           VecNbMet0 = VecNbMet0,
                           shconv = shconv,
                           SelectedSimMatGlobal = SelectedSimMatGlobal,
                           pref = pref)
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
      RedSquirrelSelected<-RedSquirrelSelected0()
      VisitsSelected<-VisitsSelected0()
      
      CarbonSelectedSD<-CarbonSelectedSD0()
      RedSquirrelSelectedSD<-RedSquirrelSelectedSD0()
      VisitsSelectedSD<-VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     RedSquirrelSelected = RedSquirrelSelected,
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     RedSquirrelSelectedSD = RedSquirrelSelectedSD,
                                     VisitsSelectedSD = VisitsSelectedSD)
      SelectedSimMat2 <- tmp$SelectedSimMat2
      Icalc <- tmp$Icalc
      SelecTargetCarbon <- tmp$SelecTargetCarbon
      SelecTargetBio <- tmp$SelecTargetBio
      SelecTargetArea <- tmp$SelecTargetArea
      SelecTargetVisits <- tmp$SelecTargetVisits
      rm(tmp)
      
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
            addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Area Planted: ",round(SelectedArea/1e6,2),"<br>
                                 Visitors: ",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
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
      RedSquirrelSelected <- RedSquirrelSelected0()
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     RedSquirrelSelected = RedSquirrelSelected,
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     RedSquirrelSelectedSD = RedSquirrelSelectedSD,
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
      
      CONDPROBA <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      
      SubsetMeetTargets <- SelectedSimMat2[CONDPROBA, ]
      SelIMAT <- Icalc$IVEC[CONDPROBA, ]
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea/1e6, 2), "<br>
                                               Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        Text1(paste0("Strategies that meet all 4 targets:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1)))
        
      } else {
        Text1("No strategy where all 4 targets are met found")
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
      RedSquirrelSelected <- RedSquirrelSelected0()
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     RedSquirrelSelected = RedSquirrelSelected,
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     RedSquirrelSelectedSD = RedSquirrelSelectedSD,
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
      
      CONDPROBA3PositiveLIST <- list()
      CONDPROBA3PositiveLIST[[1]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA3PositiveLIST[[2]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA3PositiveLIST[[3]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA3PositiveLIST[[4]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      
      SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[1]], ], NotMet = rep("carbon", sum(CONDPROBA3PositiveLIST[[1]])))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[2]], ], NotMet = rep("redSquirrel", sum(CONDPROBA3PositiveLIST[[2]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[3]], ], NotMet = rep("Area", sum(CONDPROBA3PositiveLIST[[3]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[4]], ], NotMet = rep("NbVisits", sum(CONDPROBA3PositiveLIST[[4]]))))
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea/1e6, 2), "<br>
                                               Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        Text2(paste0("Strategies that meet exactly 3 targets:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Target Not Met:", mapresults$SelectedLine$NotMet))
        
      } else {
        Text2("No strategy where exactly 3 targets are met found")
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
      RedSquirrelSelected <- RedSquirrelSelected0()
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     RedSquirrelSelected = RedSquirrelSelected,
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     RedSquirrelSelectedSD = RedSquirrelSelectedSD,
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
      
      CONDPROBA2PositiveLIST <- list()
      CONDPROBA2PositiveLIST[[1]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA2PositiveLIST[[2]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA2PositiveLIST[[3]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA2PositiveLIST[[4]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      CONDPROBA2PositiveLIST[[5]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA2PositiveLIST[[6]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      
      SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[1]], ], NotMet = rep("carbon,redSquirred", sum(CONDPROBA2PositiveLIST[[1]])))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[2]], ], NotMet = rep("carbon,Area", sum(CONDPROBA2PositiveLIST[[2]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[3]], ], NotMet = rep("carbon,NbVisits", sum(CONDPROBA2PositiveLIST[[3]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[4]], ], NotMet = rep("redSquirred,Area", sum(CONDPROBA2PositiveLIST[[4]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[5]], ], NotMet = rep("redSquirred,NbVisits", sum(CONDPROBA2PositiveLIST[[5]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[6]], ], NotMet = rep("Area,NbVisits", sum(CONDPROBA2PositiveLIST[[6]]))))
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea/1e6, 2), "<br>
                                               Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        
        Text3(paste0("Strategies that meet exactly 2 targets:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Targets Not Met:", mapresults$SelectedLine$NotMet))
        
      } else {
        Text3("No strategy where exactly 2 targets are met found")
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
      RedSquirrelSelected <- RedSquirrelSelected0()
      VisitsSelected <- VisitsSelected0()
      
      CarbonSelectedSD <- CarbonSelectedSD0()
      RedSquirrelSelectedSD <- RedSquirrelSelectedSD0()
      VisitsSelectedSD <- VisitsSelectedSD0()
      
      tmp <- outputmap_calculateMats(input = input,
                                     SavedVec = SavedVec,
                                     simul636 = simul636,
                                     AreaSelected = AreaSelected,
                                     CarbonSelected = CarbonSelected,
                                     RedSquirrelSelected = RedSquirrelSelected,
                                     VisitsSelected = VisitsSelected,
                                     CarbonSelectedSD = CarbonSelectedSD,
                                     RedSquirrelSelectedSD = RedSquirrelSelectedSD,
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
      
      CONDPROBA1PositiveLIST <- list()
      CONDPROBA1PositiveLIST[[1]] <- (PROBAMAT[, 1] >= alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA1PositiveLIST[[2]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] >= alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA1PositiveLIST[[3]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] >= alphaLVL) & (PROBAMAT[, 4] < alphaLVL)
      CONDPROBA1PositiveLIST[[4]] <- (PROBAMAT[, 1] < alphaLVL) & (PROBAMAT[, 2] < alphaLVL) & (PROBAMAT[, 3] < alphaLVL) & (PROBAMAT[, 4] >= alphaLVL)
      
      SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[1]], ], Met = rep("carbon", sum(CONDPROBA1PositiveLIST[[1]])))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[2]], ], Met = rep("redSquirred", sum(CONDPROBA1PositiveLIST[[2]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[3]], ], Met = rep("Area", sum(CONDPROBA1PositiveLIST[[3]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[4]], ], Met = rep("NbVisits", sum(CONDPROBA1PositiveLIST[[4]]))))
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue)
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea/1e6, 2), "<br>
                                               Visitors: ", round(SelectedVisits, 2), "\u00B1", round(2 * SelectedVisitsSD, 2),
                                               "</p>"), position = "topright"))
        Text4(paste0("Strategies that meet only 1 target:", round(dim(SubsetMeetTargets)[1]/5000 * 100, 2), "%\nDisplayed Strategy Nb:", as.integer(trunc(mapresults$SavedRVs * mapresults$LSMT) + 1), "; Target Met:", mapresults$SelectedLine$Met))
        
      } else {
        Text4("No strategy where only 1 target is met found")
      }
      
    }
    map <- map_sell_not_avail(FullTableNotAvail = FullTableNotAvail, SelectedDropdown = SelectedDropdown, map = map)
    map
  })
  
}

shinyApp(ui, server)
