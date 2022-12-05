rm(list=ls())
#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("UtilityFunctions.RData")
load("Directories.RData")
rev_routes=seq(1,12)
auto_late=c("CL","OL","HL","ND")

PossibleCodes=c("ED","LD","DH", "LR","SD","AD")
#SD=LRL+DH>300 but both are <300

LatenessThreshold=300
DelayThreshold=300
ADThreshold=30

setwd(data_references_dir)

s=readxl::read_excel("DelayTables.xlsx","DelayTypes")
s=reordordt(s,cc("Id Abbr SortOrder"))
s=as.data.table(s)
DelayTypesSortOrder=s

s=readxl::read_excel("DelayTables.xlsx","LocationList")
s=as.data.table(s)
LocationList=s

setwd(data_rdata_dir)
save("DelayTypesSortOrder", "LocationList","ADThreshold", "DelayThreshold","PossibleCodes",file="References.RData")
