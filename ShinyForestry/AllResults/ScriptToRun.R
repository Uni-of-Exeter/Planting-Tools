require(adaptMCMC)

TOTAL_USER_NB<-9
FolderSource<-normalizePath(file.path(getwd(),"ShinyForestry"))
#FolderSource<-"C:/Users/bn267/OneDrive - University of Exeter/Documents/GitHub/Planting-Tools/ShinyForestry"

source(normalizePath(file.path(FolderSource, "preferTrees.R")), local = FALSE)
BasePrefObj<-readRDS(normalizePath(file.path(FolderSource,"AllResults", "BasePrefObj.RDS")))

L1<-readRDS(normalizePath(file.path(FolderSource,"AllResults", "pref_LIST1.RData")))
DATT<-L1$data
PREFF<-L1$prefs

for(ii in 2:TOTAL_USER_NB){
# Lnext<-readRDS(paste0(FolderSource,"\\AllResults\\pref_LIST",ii,".RData"))
file <- normalizePath(file.path(FolderSource,
                                "AllResults",
                                paste0("pref_LIST", ii, ".RData")))
Lnext<-readRDS(file)
CurrentRows<-dim(DATT)[1]
DATT<-rbind(DATT,Lnext$data)
PREFF<-rbind(PREFF,Lnext$prefs+CurrentRows)
}

NewObj<-prefObject(priors = BasePrefObj$priors,data=DATT,prefs=PREFF)
saveRDS(NewObj,normalizePath(file.path(FolderSource,"FixedStrats", "pref_reactive.RDS")))
