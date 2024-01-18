library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(ggplot2)
library(geosphere)
library(feather)
###################
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
###############################
#  SavedVec<-rep(0,47)
#SelecTargetCarbon<-240;      SelecTargetBio<-19;SelecTargetArea<-13890596;SelecTargetVisits<-17
#SelecTargetCarbon<-1000;      SelecTargetBio<-1100;SelecTargetArea<-1000000000;SelecTargetVisits<-1000000
#SelectedDropdown<-"Ennerdale"

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
simul636<-read.csv(file="Simul636.csv")[,-1]

alphaLVL<-0.9


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
############################################

#########################################
ui <- fluidPage(useShinyjs(),tabsetPanel(id = "tabs",
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
                  ),
                  ########################
                  tabPanel("ClusterNew",
                          fluidPage(

                            shinyjs::hidden(
                              fluidRow(12, checkboxInput("Trigger", "", value = FALSE, width = NULL))),
                                     conditionalPanel(
                                       condition="input.Trigger==true",
                                       fluidRow(
                      ##              #     ################################
                                         column(6,verticalLayout(leafletOutput("ClusterPage"),actionButton("choose1", "choose"))
                                         ),
                                         column(6,verticalLayout(leafletOutput("ClusterPage2"),actionButton("choose2", "choose"))
                                                )
                                         )),
                      conditionalPanel(
                        condition="input.Trigger==false", fluidRow(
                                
                        column(12,plotOutput("plotOP1")))
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


  ######################
 output$TargetText<-renderText({paste0("Tree Carbon target: ",as.numeric(CarbonSliderVal()),
                                        "\nAvg red squirrel Inc. target: ",as.numeric(bioSliderVal()),
                                        "\nArea target: ",as.numeric(AreaSliderVal()),
                                        "\nAvg Visits per km^2 target: ",as.numeric(VisitsSliderVal())
                                                                                )})
  output$SoilCarbonNotIncluded<-renderText({paste0("Note that Soil carbon is currently not included")})

  output$FirstMapTxt<-renderText({Text1()})
  output$SecondMapTxt<-renderText({Text2()})
  output$ThirdMapTxt<-renderText({Text3()})
  output$FourthMapTxt<-renderText({Text4()})
  ######################
  randomValue <- eventReactive({input$random
    input$tabsetPanel == "Exploration"}, {runif(1)})
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
    ######################
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
  
  ##############
 
  
  
  
  ##################################################
  observeEvent(input$tabs == "ClusterNew",{

    SavedVec<-ClickedVector()
    SelectedDropdown<-input$inSelect
    
    updateCheckboxInput(session,"Trigger", label = "", value = TRUE)
    #input$Trigger<-TRUE
    calcBaseMap<-BaseMap(SelectedDropdown,layerId="main20")
    
                 shinyjs::disable("choose1")
                 shinyjs::disable("choose2")
                 CurrentRound(0)
                 listMaps<-list()
                 listMaps[[1]]<-calcBaseMap$map
                 listMaps[[2]]<-calcBaseMap$map
                 
  #               ##########################################################
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
                   SelectedSimMatGlobal<<-SelectedSimMat2
                   Icalc<-MultiImpl(TargetsVec=c(SelecTargetCarbon,SelecTargetBio,SelecTargetArea,SelecTargetVisits),
                                    EYMat=data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits),
                                    SDYMat=data.frame(SelectedSimMat2$carbonSD,SelectedSimMat2$redsquirelSD,rep(0,length(SelectedSimMat2$Area)),SelectedSimMat2$VisitsSD),
                                    alpha=0.05,
                                    tolVec=c(4,2,100,2))
                   
                   LimitsMat<-(-data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits))/
                     sqrt(data.frame(SelectedSimMat2$carbonSD^2+4^2,SelectedSimMat2$redsquirelSD^2+2^2,
                                     rep(0,length(SelectedSimMat2$Area))+100^2,SelectedSimMat2$VisitsSD+2^2))
                   
                   
                   PROBAMAT<-Icalc$IVEC
                   for(abc in 1:dim(Icalc$IVEC)[2]){
                     PROBAMAT[,abc]<-1-ptruncnorm(Icalc$IVEC[,abc],a=LimitsMat[,abc], b=Inf)}
                   
                   CONDPROB_AtLeast1<-(PROBAMAT[,1]>=alphaLVL)|(PROBAMAT[,2]>=alphaLVL)|(PROBAMAT[,3]>=alphaLVL)|(PROBAMAT[,4]>=alphaLVL)
                   
                   datAll = data.frame(carbonMet=1*(PROBAMAT[,1]>=alphaLVL),
                                       redSquirrel=1*(PROBAMAT[,2]>=alphaLVL),
                                       Area=1*(PROBAMAT[,3]>=alphaLVL),
                                       Visits=1*(PROBAMAT[,4]>=alphaLVL))
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
                   
                   priors <- c(prefeR::Normal(0.5,0.1), 
                               prefeR::Normal(0.5,0.1), 
                               prefeR::Normal(0.5,0.1), 
                               prefeR::Normal(0.5,0.1) 
                   ) 
                   pref <<- prefeR::prefEl(data=datAll, 
                                          priors = priors)
                   
                   
                   UniqueBinCodes<-unique(DatBinaryCode)
                   
                   
                   
                   if (length(UniqueBinCodes)>=2)
                   {
                     NbRoundsMax((as.integer(trunc(length(UniqueBinCodes)/2))))
                   
                   LinesToCompare<-matrix(0,(as.integer(trunc(length(UniqueBinCodes)/2))),2)
                   indd<-1
                  for(iws in 1:(as.integer(trunc(length(UniqueBinCodes)/2)))){
                    LinesToCompare[iws,1]<-which(DatBinaryCode==UniqueBinCodes[indd])[1]
                   indd<-indd+1
                   LinesToCompare[iws,2]<-which(DatBinaryCode==UniqueBinCodes[indd])[1]
                   indd<-indd+1
                  }
                   
                   
                   CurrentRound(1)
                   LinesToCompareReactive(LinesToCompare)
                   SelectedLine<-list()
                   SelectedLine[[1]]<-SelectedSimMat2[LinesToCompare[1,1],]
                   SelectedLine[[2]]<-SelectedSimMat2[LinesToCompare[1,2],]
                     
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
                       addControl(html = paste0("<p>Actual Total Carbon:",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Actual Red Squirrel Increase:",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Actual Area Planted:",round(SelectedArea,2),"<br>
                                 Actual Average Vists:",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                                "</p>"), position = "topright")
                     
                     
                     
                     
                     }
                 
                   shinyjs::enable("choose1")
                   shinyjs::enable("choose2")
                   
                   
                   
                 }else{
  
                   listMaps[[1]]<-listMaps[[1]]%>%  
                     addControl(html = paste0("<p> Elicitation Not Possible as there is one or zero t 
                                              </p>"), position = "topright")
                   listMaps[[2]]<-listMaps[[2]]%>%  
                     addControl(html = paste0("<p> Elicitation Not Possible as there is one or zero t 
                                              </p>"), position = "topright")
                   shinyjs::disable("choose1")
                   shinyjs::disable("choose2")
                   
                   
                   
                 }  }
                 
                 
                 SELLNOTAVAIL<-(FullTableNotAvail$extent==SelectedDropdown)
                 if(!is.null(SELLNOTAVAIL)){
                   
                   sellngNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lgn.1","lgn.2","lgn.3","lgn.4","lgn.5")]
                   sellatNotAvail<-FullTableNotAvail[SELLNOTAVAIL,c("lat.1","lat.2","lat.3","lat.4","lat.5")]
                   for (iii in 1:dim(sellngNotAvail)[1]){
                     listMaps[[1]]<-addPolygons(listMaps[[1]],lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
                     listMaps[[2]]<-addPolygons(listMaps[[2]],lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
                     midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
                     midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
                     listMaps[[1]]<-addPolylines(listMaps[[1]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
                     listMaps[[1]]<-addPolylines(listMaps[[1]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
                     listMaps[[2]]<-addPolylines(listMaps[[2]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
                     listMaps[[2]]<-addPolylines(listMaps[[2]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
                     
                   }
                   
                 }
                 
                 
          output$ClusterPage<-renderLeaflet({listMaps[[1]]})
      output$ClusterPage2<-renderLeaflet({listMaps[[2]]})  
    
                 
               })
  ####################################################
  ##################################################
  observeEvent(input$choose1,{
    SavedVec<-ClickedVector()
    LinesToCompare<-as.matrix(LinesToCompareReactive())
    SelectedDropdown<-input$inSelect
    
    shinyjs::disable("choose1")
    shinyjs::disable("choose2")
    
    ###########################################################
    if((!is.null(SavedVec))&(CurrentRound()>0)){
      calcBaseMap<-BaseMap(SelectedDropdown,layerId="main5")
      
      SelectedSimMat2<-SelectedSimMatGlobal
      if (dim(LinesToCompare)[1]>=CurrentRound())#NbRoundsMax()
      {
        CR<-CurrentRound()
        pref$addPref(prefeR::`%>%`(LinesToCompare[CurrentRound(),1],LinesToCompare[CurrentRound(),2]))
        
        
                listMaps<-list()
         listMaps[[1]]<-calcBaseMap$map
        listMaps[[2]]<-calcBaseMap$map
        
        

        SelectedLine<-list()
        SelectedLine[[1]]<-SelectedSimMat2[LinesToCompare[CR,1],]
        SelectedLine[[2]]<-SelectedSimMat2[LinesToCompare[CR,2],]
        
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
            listMaps[[1]]<-addPolygons(listMaps[[1]],lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
            listMaps[[2]]<-addPolygons(listMaps[[2]],lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
            midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
            midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
            listMaps[[1]]<-addPolylines(listMaps[[1]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
            listMaps[[1]]<-addPolylines(listMaps[[1]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
            listMaps[[2]]<-addPolylines(listMaps[[2]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
            listMaps[[2]]<-addPolylines(listMaps[[2]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
            
          }
          
        }
    
        output$ClusterPage<-renderLeaflet({listMaps[[1]]})
        output$ClusterPage2<-renderLeaflet({listMaps[[2]]})  
        shinyjs::enable("choose1")
        shinyjs::enable("choose2")
        CR<-CR+1
        CurrentRound(CR)
        
      }else{
        
        shinyjs::disable("choose1")
        shinyjs::disable("choose2")
        infpref<<-pref$infer()

                
                
        ###########
        SelectedSimMat2<-SelectedSimMatGlobal
        VecNbMet<-VecNbMet0()
        
        
        ClusteringDat<-data.frame(infpref*apply(SelectedSimMat2[,c("carbon","redsquirel","Area","Visits")],2,normalize),NbTargetsMet=VecNbMet)
        ClusteringDat<-ClusteringDat[ClusteringDat$NbTargetsMet>0,]
        ClusteringDat<-unique(ClusteringDat)
        set.seed(123)  

        if(dim(ClusteringDat)[1]<5){Perp<-0.1}else{
          if(dim(ClusteringDat)[1]<50){Perp<-5}else{Perp<-30}
        }
        tsRes <- Rtsne(ClusteringDat, perplexity = Perp)
        pp<<-ggplot(data=data.frame(x=tsRes$Y[,1],y=tsRes$Y[,2]),aes(x,y))+
          geom_point(aes(colour =factor(ClusteringDat$NbTargetsMet)))+
          labs(x="dim1",y="dim2",color = "Number of Targets Met")+theme_minimal()
        output$plotOP1 <- renderPlot({pp})
        updateCheckboxInput(session,"Trigger", label = "", value = FALSE)
        
        ############

      }  }
    
    
  })
#######################################################################  
  observeEvent(input$choose2,{
      SavedVec<-ClickedVector()
      LinesToCompare<-as.matrix(LinesToCompareReactive())
      SelectedDropdown<-input$inSelect
      
      shinyjs::disable("choose1")
      shinyjs::disable("choose2")
      
      ###########################################################
      if((!is.null(SavedVec))&(CurrentRound()>0)){
        calcBaseMap<-BaseMap(SelectedDropdown,layerId="main5")
        
        SelectedSimMat2<-SelectedSimMatGlobal
        if (dim(LinesToCompare)[1]>=CurrentRound())#NbRoundsMax()
        {
          CR<-CurrentRound()
          pref$addPref(prefeR::`%>%`(LinesToCompare[CurrentRound(),2],LinesToCompare[CurrentRound(),1]))
             
          
          
          listMaps<-list()
          listMaps[[1]]<-calcBaseMap$map
          listMaps[[2]]<-calcBaseMap$map
          
          
          
          SelectedLine<-list()
          SelectedLine[[1]]<-SelectedSimMat2[LinesToCompare[CR,1],]
          SelectedLine[[2]]<-SelectedSimMat2[LinesToCompare[CR,2],]
          
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
              listMaps[[1]]<-addPolygons(listMaps[[1]],lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
              listMaps[[2]]<-addPolygons(listMaps[[2]],lng= as.numeric(sellngNotAvail[iii,]),lat= as.numeric(sellatNotAvail[iii,]),layerId =paste0("SquareNotAvail",iii),color ="yellow")
              midlng<-(as.numeric(sellngNotAvail[iii,1])+as.numeric(sellngNotAvail[iii,3]))/2
              midlat<-(as.numeric(sellatNotAvail[iii,1])+as.numeric(sellatNotAvail[iii,2]))/2
              listMaps[[1]]<-addPolylines(listMaps[[1]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
              listMaps[[1]]<-addPolylines(listMaps[[1]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
              listMaps[[2]]<-addPolylines(listMaps[[2]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng - 0.004, midlng + 0.004),color = "orange", weight = 5) 
              listMaps[[2]]<-addPolylines(listMaps[[2]],lat = c(midlat - 0.0025, midlat + 0.0025),lng = c(midlng + 0.004, midlng - 0.004),color = "orange", weight = 5) 
              
            }
            
          }
          
          output$ClusterPage<-renderLeaflet({listMaps[[1]]})
          output$ClusterPage2<-renderLeaflet({listMaps[[2]]})  
          shinyjs::enable("choose1")
          shinyjs::enable("choose2")
          CR<-CR+1
          CurrentRound(CR)
          
        }else{
          
          shinyjs::disable("choose1")
          shinyjs::disable("choose2")
          
          updateCheckboxInput(session,"Trigger", label = "", value = FALSE)
          infpref<-pref$infer()
          
          
        }  }
      
      
    })
  
  
  #################################
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
  if(!is.null(click)){SavedVec<-ClickedVector()
  for(iii in 1:length(SavedV.ec)){
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
      
      
      LimitsMat<-(-data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits))/
        sqrt(data.frame(SelectedSimMat2$carbonSD^2+4^2,SelectedSimMat2$redsquirelSD^2+2^2,
                        rep(0,length(SelectedSimMat2$Area))+100^2,SelectedSimMat2$VisitsSD+2^2))
      
      
      #PROBAMAT<-1-pnorm(Icalc$IVEC)
      PROBAMAT<-Icalc$IVEC
      for(abc in 1:dim(Icalc$IVEC)[2]){
        PROBAMAT[,abc]<-1-ptruncnorm(Icalc$IVEC[,abc],a=LimitsMat[,abc], b=Inf)}

      CONDPROBA<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL)
      
              
            
      SubsetMeetTargets<-SelectedSimMat2[CONDPROBA,]
      SelIMAT<-Icalc$IVEC[CONDPROBA,]

   if(dim(SubsetMeetTargets)[1]>0){   
      
      SavedRVs<-randomValue()
      LSMT<-dim(SubsetMeetTargets)[1]
      SelectedLine<-SubsetMeetTargets[as.integer(trunc(SavedRVs*LSMT)+1),]
      
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
      Text1(
        paste0("Estimated % strategies that meet 4 targets:",
               round(dim(SubsetMeetTargets)[1]/5000*100,2),"%\nDisplayed Strategy Nb:",
        as.integer(trunc(SavedRVs*LSMT)+1)))
        
      
      }else{Text1("No strategy where the 4 targets are met found")}
      
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
      
     
      
      
      LimitsMat<-(-data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits))/
        sqrt(data.frame(SelectedSimMat2$carbonSD^2+4^2,SelectedSimMat2$redsquirelSD^2+2^2,
                        rep(0,length(SelectedSimMat2$Area))+100^2,SelectedSimMat2$VisitsSD+2^2))
      
      
      #PROBAMAT<-1-pnorm(Icalc$IVEC)
      PROBAMAT<-Icalc$IVEC
      for(abc in 1:dim(Icalc$IVEC)[2]){
        PROBAMAT[,abc]<-1-ptruncnorm(Icalc$IVEC[,abc],a=LimitsMat[,abc], b=Inf)}

      CONDPROBA3PositiveLIST<-list()
      CONDPROBA3PositiveLIST[[1]]<-(PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL)
      CONDPROBA3PositiveLIST[[2]]<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL)
      CONDPROBA3PositiveLIST[[3]]<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL)
      CONDPROBA3PositiveLIST[[4]]<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL)
      
      

      
      SubsetMeetTargets<-data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[1]],],NotMet=rep("carbon",sum(CONDPROBA3PositiveLIST[[1]])))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[2]],],NotMet=rep("redSquirrel",sum(CONDPROBA3PositiveLIST[[2]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[3]],],NotMet=rep("Area",sum(CONDPROBA3PositiveLIST[[3]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA3PositiveLIST[[4]],],NotMet=rep("NbVisits",sum(CONDPROBA3PositiveLIST[[4]]))))
      
      
      
            
      
      if(dim(SubsetMeetTargets)[1]>0){
      
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
      Text2(
        paste0("Estimated % strategies that meet 3 targets exactly (excl.1, 2 or 4):",
               round(dim(SubsetMeetTargets)[1]/5000*100,2),"%\nDisplayed Strategy Nb:",
               as.integer(trunc(SavedRVs*LSMT)+1),"; Target Not Met:",SelectedLine$NotMet))
      
      }else{Text2("No strategy where 3 targets are met found")}
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
      
      
      
      LimitsMat<-(-data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits))/
        sqrt(data.frame(SelectedSimMat2$carbonSD^2+4^2,SelectedSimMat2$redsquirelSD^2+2^2,
                        rep(0,length(SelectedSimMat2$Area))+100^2,SelectedSimMat2$VisitsSD+2^2))
      
      
      #PROBAMAT<-1-pnorm(Icalc$IVEC)
      PROBAMAT<-Icalc$IVEC
      for(abc in 1:dim(Icalc$IVEC)[2]){
        PROBAMAT[,abc]<-1-ptruncnorm(Icalc$IVEC[,abc],a=LimitsMat[,abc], b=Inf)}
      
      CONDPROBA2PositiveLIST<-list()
      CONDPROBA2PositiveLIST[[1]]<-(PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]>=alphaLVL)
      CONDPROBA2PositiveLIST[[2]]<-(PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL)
      CONDPROBA2PositiveLIST[[3]]<-(PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL)
      CONDPROBA2PositiveLIST[[4]]<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL)
      CONDPROBA2PositiveLIST[[5]]<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL)
      CONDPROBA2PositiveLIST[[6]]<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL)
      
      
      
      
      SubsetMeetTargets<-data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[1]],],NotMet=rep("carbon,redSquirred",sum(CONDPROBA2PositiveLIST[[1]])))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[2]],],NotMet=rep("carbon,Area",sum(CONDPROBA2PositiveLIST[[2]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[3]],],NotMet=rep("carbon,NbVisits",sum(CONDPROBA2PositiveLIST[[3]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[4]],],NotMet=rep("redSquirred,Area",sum(CONDPROBA2PositiveLIST[[4]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[5]],],NotMet=rep("redSquirred,NbVisits",sum(CONDPROBA2PositiveLIST[[5]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[6]],],NotMet=rep("Area,NbVisits",sum(CONDPROBA2PositiveLIST[[6]]))))
      
      
      if(dim(SubsetMeetTargets)[1]>0){
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
    
    
      Text3(
        paste0("Estimated % strategies that meet 2 targets exactly (not 4, 3 or 1):",
               round(dim(SubsetMeetTargets)[1]/5000*100,2),"%\nDisplayed Strategy Nb:",
               as.integer(trunc(SavedRVs*LSMT)+1),"; Targets Not Met:",SelectedLine$NotMet))
      
      }else{Text3("No strategy where the 2 targets are met found")}
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
      
      LimitsMat<-(-data.frame(SelectedSimMat2$carbon,SelectedSimMat2$redsquirel,SelectedSimMat2$Area,SelectedSimMat2$Visits))/
        sqrt(data.frame(SelectedSimMat2$carbonSD^2+4^2,SelectedSimMat2$redsquirelSD^2+2^2,
                        rep(0,length(SelectedSimMat2$Area))+100^2,SelectedSimMat2$VisitsSD+2^2))
      
      
      #PROBAMAT<-1-pnorm(Icalc$IVEC)
      PROBAMAT<-Icalc$IVEC
      for(abc in 1:dim(Icalc$IVEC)[2]){
        PROBAMAT[,abc]<-1-ptruncnorm(Icalc$IVEC[,abc],a=LimitsMat[,abc], b=Inf)}
      
      
      CONDPROBA1PositiveLIST<-list()
      CONDPROBA1PositiveLIST[[1]]<-(PROBAMAT[,1]>=alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL)
      CONDPROBA1PositiveLIST[[2]]<-(PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]>=alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]<alphaLVL)
      CONDPROBA1PositiveLIST[[3]]<-(PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]>=alphaLVL)&(PROBAMAT[,4]<alphaLVL)
      CONDPROBA1PositiveLIST[[4]]<-(PROBAMAT[,1]<alphaLVL)&(PROBAMAT[,2]<alphaLVL)&(PROBAMAT[,3]<alphaLVL)&(PROBAMAT[,4]>=alphaLVL)

      
      
      
      SubsetMeetTargets<-data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[1]],],Met=rep("carbon",sum(CONDPROBA1PositiveLIST[[1]])))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[2]],],Met=rep("redSquirred",sum(CONDPROBA1PositiveLIST[[2]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[3]],],Met=rep("Area",sum(CONDPROBA1PositiveLIST[[3]]))))
      SubsetMeetTargets<-rbind(SubsetMeetTargets,
                               data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[4]],],Met=rep("NbVisits",sum(CONDPROBA1PositiveLIST[[4]]))))
      
      
      
     
      if(dim(SubsetMeetTargets)[1]>0){
      
      SavedRVs<-randomValue()
      LSMT<-dim(SubsetMeetTargets)[1]
      SelectedLine<-SubsetMeetTargets[as.integer(trunc(SavedRVs*LSMT)+1),]
      
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
      Text4(
        paste0("Estimated % strategies that meet 1 target exactly (not 4, 3 or 2):",
               round(dim(SubsetMeetTargets)[1]/5000*100,2),"%\nDisplayed Strategy Nb:",
               as.integer(trunc(SavedRVs*LSMT)+1),"; Target Met:",SelectedLine$Met))
      
      }else{Text4("No strategy where 1 target is met found")}
    
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


