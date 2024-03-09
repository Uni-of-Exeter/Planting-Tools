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
library(colorspace)
library(rjson)
library(arrow)
library(viridis)
library(lwgeom)
#  SavedVec<-rep(0,47)
#SelecTargetCarbon<-240;      SelecTargetBio<-19;SelecTargetArea<-13890596;SelecTargetVisits<-17
#SelecTargetCarbon<-1000;      SelecTargetBio<-1100;SelecTargetArea<-1000000000;SelecTargetVisits<-1000000
#SelectedDropdown<-"Ennerdale"
############################################

source("functions.R")

#PROJdir<-system.file("proj/proj.db", package = "sf")
#PROJdir<-substring(PROJdir,1,nchar(PROJdir)-8)
#sf_proj_search_paths(PROJdir)

ElicitatorAppFolder<-"..//ElicitatorOutput//"
JulesAppFolder<-"..\\JulesOP\\"
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
shconv<-sf::st_read(paste0(UnZipDirName,"//land_parcels.shp"))
if(is.null(shconv$extent)){shconv$extent<-"NoExtent"}
st_write(shconv, paste0(ElicitatorAppFolder,"Parcels.geojson"))
JulesMean<-arrow::read_feather(paste0(JulesAppFolder,"JulesApp-rcp26-06-mean-monthly.feather"))[,c("x","y","mean337")]
JulesSD<-arrow::read_feather(paste0(JulesAppFolder,"JulesApp-rcp26-06-sd-monthly.feather"))[,c("x","y","sd337")]
SquaresLoad<-sf::st_read(paste0(JulesAppFolder,"SEER//Fishnet_1km_to_SEER_net2km.shp"))
Sqconv<-st_transform(SquaresLoad, crs = 4326)
XYMAT<-read.csv(paste0(JulesAppFolder,"XYMat_1km.csv"))[,-1]
CorrespondenceJules<-read.csv(paste0(JulesAppFolder,"/CorrespondanceSqToJules.csv"))[,-1]

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


MER<-list()
for(ii in 1:length(Uni))
{
  SELLL<-shconv$geometry[AllUnits==Uni[ii]]
  MER[[ii]]<-st_union(SELLL[1],SELLL[2])
  if(length(SELLL)>2){
    for (jj in 3:length(SELLL)){
      MER[[ii]]<-st_union(MER[[ii]],st_make_valid(SELLL[jj]))
      }
    
  }
  
}
FullTable <- st_sf(FullTab,geometry=do.call(c,MER),crs=4326)
############################################ Replace the Jules Mean here



XVEC<-sort(unique(c(XYMAT$XMIN,XYMAT$XMAX)))
YVEC<-sort(unique(c(XYMAT$YMIN,XYMAT$YMAX)))


keptLines<-NULL
shconvBack<-st_transform(shconv, crs =st_crs(27700))
for(ii in 1:length(shconv$geometry))
{
  MERconvback<-shconvBack$geometry[[ii]][[1]]

      xmin<-min(MERconvback[,1])
      DIF<-xmin-XVEC
      XMINVEC<-(XVEC[DIF>=0])[which.min(DIF[DIF>=0])]
      
      xmax<-max(MERconvback[,1])
      DIF2<-XVEC-xmax
      XMAXVEC<-(XVEC[DIF2>=0])[which.min(DIF2[DIF2>=0])]
      
      ymin<-min(MERconvback[,2])
      DIF3<-ymin-YVEC
      YMINVEC<-(YVEC[DIF3>=0])[which.min(DIF3[DIF3>=0])]
      
      ymax<-max(MERconvback[,2])
      DIF4<-YVEC-ymax
      YMAXVEC<-(YVEC[DIF4>=0])[which.min(DIF4[DIF4>=0])]
      
      keptLines<-unique(c(keptLines,which((XYMAT$XMAX<=XMAXVEC)&(XYMAT$XMIN>=XMINVEC)&(XYMAT$YMIN>=YMINVEC)&(XYMAT$YMAX<=YMAXVEC))))
   
}

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


###############



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



alphaLVL<-0.9

MaxRounds<-5

ConvertSample<-sample(1:5000,200)



ui <- fluidPage(useShinyjs(),tabsetPanel(id = "tabs",
                                         tabPanel("Maps",fluidPage(fluidRow(
                                           column(9,
                                                  #  tags$head(tags$style(HTML("#map {pointer-events: none;}"))),
                                                  #tags$style("#inSelect {color: white; background-color: transparent; border: white;}"),
                                                  selectInput("inSelect", "area",sort(unique(c(FullTable$extent,FullTableNotAvail$extent))),FullTable$extent[1]),
                                                  fluidRow(column(4,selectInput("ColourScheme","Colour Scheme",c("blue/red","rainbow dark/light",
                                                                                                                 "rainbow dark/red",
                                                                                                                 "Terrain darkened/lightened",
                                                                                                                 "Terrain darkened/red",
                                                                                                                 "Viridis darkened/red"))),
                                                  column(4,sliderInput("Darken","Darkening Factor:",min=-100,max=100,value=70)),
                                                  column(4,sliderInput("Lighten","Lightening Factor:",min=-100,max=100,value=50))),
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
  
  ColorLighteningFactor<-reactiveVal(0.5)
  ColorDarkeningFactor<-reactiveVal(0.7)
  
  observeEvent(input$Darken,{
    ColorDarkeningFactor(input$Darken/100)
  })
  observeEvent(input$Lighten,{
    ColorLighteningFactor(input$Lighten/100)
  })
  
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
    
    SelectedSquares<-cbind(extent=FullTable$extent[FullTable$extent==SelectedDropdown])#,FullTable$lgn.1[FullTable$extent==SelectedDropdown],
                        #   FullTable$lat.1[FullTable$extent==SelectedDropdown])
    
    if(!(dim(SelectedSquares)[1]==0)){
      AreaSelected<-FullTable$area[FullTable$extent==SelectedDropdown]
      CarbonSelected<-(FullTable$JulesMean[FullTable$extent==SelectedDropdown])#*AreaSelected)/1e6
      RedSquirrelSelected<-FullTable$BioMean_Sciurus_vulgaris[FullTable$extent==SelectedDropdown]
      VisitsSelected<-FullTable$VisitsMean[FullTable$extent==SelectedDropdown]
      
      
      CarbonSelectedSD<-(FullTable$JulesSD[FullTable$extent==SelectedDropdown])#*AreaSelected)/1e6
      RedSquirrelSelectedSD<-FullTable$BioSD_Sciurus_vulgaris[FullTable$extent==SelectedDropdown]
      VisitsSelectedSD<-FullTable$VisitsSD[FullTable$extent==SelectedDropdown]
      
      
      
      
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
      updateSliderInput(session, "AreaSlider", max = trunc(100*sum(AreaSelected))/100,value=trunc(100*sum(AreaSelected))/100)#trunc(sum(AreaSelected)/1e6),value=trunc(sum(AreaSelected)/1e6))
      updateSliderInput(session, "VisitsSlider", max = trunc(mean(VisitsSelected)),value=trunc(mean(VisitsSelected)))
    }
    
    
    
    
  })

  
  #################
  observeEvent(input$tabs == "Clustering",{
    
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
            SELGEO<-FullTable$geometry[SELL]
            SELGEOFull<-FullTable[SELL,]
            SELGEOFull$layerId<-paste0("Square",1:dim(SELGEOFull)[1])
            ############
            
            
            ColObtained<-getCols(ColourScheme=input$ColourScheme,UnitsVec=FullTable$units[SELL],
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
          
          listMaps[[aai]]<-listMaps[[aai]]%>%
            addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Area Planted: ",round(SelectedArea,2),"<br>
                                 Visitors: ",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                     "</p>"), position = "topright")
          #/1e6
          
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
                           pref = pref,
                           ColorLighteningFactor=ColorLighteningFactor(),
                           ColorDarkeningFactor=ColorDarkeningFactor())
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
                           pref = pref,
                           ColorLighteningFactor=ColorLighteningFactor(),
                           ColorDarkeningFactor=ColorDarkeningFactor())
  })
  
  ################## TO CHANGE
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    SelectedDropdown <- input$inSelect
    SelectedRowsUnits<-FullTable$units[FullTable$extent==SelectedDropdown]
    
    
    GEOVEC<-st_geometry_type(FullTable$geometry)
    
    if(!is.null(click$id)){
      ChangeDone<-FALSE
      SavedVec<-ClickedVector()
        iii<-1
    
        while((!ChangeDone)&&(iii<=length(SavedVec))){
            #kk<-1
            #while((!ChangeDone)&&(kk<=length(FullTable$geometry[[iii]]))){
               # if(st_geometry_type(FullTable$geometry)[iii]=="POLYGON"){
                            if((click$id == paste0("Square",iii))){
                              SavedVec[SelectedRowsUnits==SelectedRowsUnits[iii]]<-ifelse(SavedVec[iii]==1,0,1);
                              ClickedVector(SavedVec)
                              ChangeDone<-TRUE
                              }#}#else{
                         # if((click$id == paste0("Square",iii,"_",kk))){
                          #  SavedVec[SelectedRowsUnits==SelectedRowsUnits[iii]]<-ifelse(SavedVec[iii]==1,0,1);
                          #  ClickedVector(SavedVec)
                          #  ChangeDone<-TRUE
                            
                           # }
            
                      #}
      
          
          #    kk<-kk+1
           # }
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
      LimitsMat <- tmp$LimitsMat
      SelecTargetCarbon <- tmp$SelecTargetCarbon
      SelecTargetBio <- tmp$SelecTargetBio
      SelecTargetArea <- tmp$SelecTargetArea
      SelecTargetVisits <- tmp$SelecTargetVisits
      rm(tmp)
      PROBAMAT <- Icalc$IVEC
      for (abc in 1:dim(Icalc$IVEC)[2]) {
        PROBAMAT[, abc] <- 1 - ptruncnorm(Icalc$IVEC[, abc], a = LimitsMat[, abc], b = Inf)
      }
      
      
      SubsetMeetTargets<-SelectedSimMat2[(SelectedSimMat2$carbon>=SelecTargetCarbon)&
                                           (SelectedSimMat2$redsquirel>=SelecTargetBio)&
                                           (SelectedSimMat2$Area>=SelecTargetArea)&
                                           (SelectedSimMat2$Visits>=SelecTargetVisits),]
      
      #SubsetMeetTargets<-SelectedSimMat2[Icalc$NROYTotal,]
      
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
          
          
          SELGEOFull<-FullTable[SELL,]
          SELGEOFull$layerId<-paste0("Square",1:dim(SELGEOFull)[1])
          SELGEO<-FullTable$geometry[SELL]
          ############
          ColObtained<-getCols(ColourScheme=input$ColourScheme,UnitsVec=FullTable$units[SELL],
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
          

          





          map<-map%>%
            addControl(html = paste0("<p>Carbon: ",round(SelectedTreeCarbon,2),"\u00B1",round(2*SelectedTreeCarbonSD,2),"<br>
                                 Red Squirrel: ",round(SelectedBio,2),"\u00B1",round(2*SelectedBioSD,2),"<br>
                                 Area Planted: ",round(SelectedArea,2),"<br>
                                 Visitors: ",round(SelectedVisits,2),"\u00B1",round(2*SelectedVisitsSD,2),
                                     "</p>"), position = "topright")
          
        }else{ map<-map%>%
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
                                              randomValue = randomValue,
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor())
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea, 2), "<br>
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
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main3", shconv)
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
                                              randomValue = randomValue,
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor())
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea, 2), "<br>
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
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main4", shconv)
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
      
      SubsetMeetTargets <- data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[1]], ], NotMet = rep("carbon,redSquirrel", sum(CONDPROBA2PositiveLIST[[1]])))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[2]], ], NotMet = rep("carbon,Area", sum(CONDPROBA2PositiveLIST[[2]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[3]], ], NotMet = rep("carbon,NbVisits", sum(CONDPROBA2PositiveLIST[[3]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[4]], ], NotMet = rep("redSquirrel,Area", sum(CONDPROBA2PositiveLIST[[4]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[5]], ], NotMet = rep("redSquirrel,NbVisits", sum(CONDPROBA2PositiveLIST[[5]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA2PositiveLIST[[6]], ], NotMet = rep("Area,NbVisits", sum(CONDPROBA2PositiveLIST[[6]]))))
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor())
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea, 2), "<br>
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
    calcBaseMap <- BaseMap2(SelectedDropdown, layerId = "main5", shconv)
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
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[2]], ], Met = rep("redSquirrel", sum(CONDPROBA1PositiveLIST[[2]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[3]], ], Met = rep("Area", sum(CONDPROBA1PositiveLIST[[3]]))))
      SubsetMeetTargets <- rbind(SubsetMeetTargets, data.frame(SelectedSimMat2[CONDPROBA1PositiveLIST[[4]], ], Met = rep("NbVisits", sum(CONDPROBA1PositiveLIST[[4]]))))
      
      if (dim(SubsetMeetTargets)[1] > 0) {
        mapresults <- outputmap_createResults(map = map,
                                              SubsetMeetTargets = SubsetMeetTargets,
                                              alphaLVL = alphaLVL,
                                              FullTable = FullTable,
                                              SavedVec = SavedVec,
                                              SelectedDropdown = SelectedDropdown,
                                              randomValue = randomValue,
                                              ColorLighteningFactor=ColorLighteningFactor(),
                                              ColorDarkeningFactor=ColorDarkeningFactor())
        SavedRVs <- mapresults$SavedRVs
        LSMT <- mapresults$LSMT
        map <- mapresults$map
        
        map <- with(mapresults, map %>%
                      addControl(html = paste0("<p>Carbon: ", round(SelectedTreeCarbon, 2), "\u00B1", round(2 * SelectedTreeCarbonSD, 2), "<br>
                                               Red Squirrel: ", round(SelectedBio, 2), "\u00B1", round(2 * SelectedBioSD, 2), "<br>
                                               Area Planted: ", round(SelectedArea, 2), "<br>
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
