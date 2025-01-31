require(adaptMCMC)

TOTAL_USER_NB<-9
FolderSource<-paste0(getwd(),"\\ShinyForestry")
#FolderSource<-"C:/Users/bn267/OneDrive - University of Exeter/Documents/GitHub/Planting-Tools/ShinyForestry"

source(paste0(FolderSource, "//preferTrees.R"), local = FALSE)
BasePrefObj<-readRDS(paste0(FolderSource,"\\AllResults\\BasePrefObj.RDS"))

L1<-readRDS(paste0(FolderSource,"\\AllResults\\pref_LIST1.RData"))
DATT<-L1$data
PREFF<-L1$prefs

for(ii in 2:TOTAL_USER_NB){
Lnext<-readRDS(paste0(FolderSource,"\\AllResults\\pref_LIST",ii,".RData"))
CurrentRows<-dim(DATT)[1]
DATT<-rbind(DATT,Lnext$data)
PREFF<-rbind(PREFF,Lnext$prefs+CurrentRows)
}

NewObj<-prefObject(priors = BasePrefObj$priors,data=DATT,prefs=PREFF)
saveRDS(NewObj,paste0(FolderSource,"\\FixedStrats\\pref_reactive.RDS"))
