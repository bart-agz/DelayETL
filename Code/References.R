rm(list=ls())
#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("Directories.RData")
load("UtilityFunctions.RData")

PossibleCodes=c("ED","LD","DH", "LR","SD","AD")
#SD=LRL+DH>300 but both are <300

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