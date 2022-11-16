rm(list=ls())
# Main Process Function
library("rprojroot")
rm(list=ls())

#setwd to root of project
setwd(find_rstudio_root_file())


source(paste0(sep="", find_rstudio_root_file(), "/Code/Libraries.R"))
source(paste0(sep="", find_rstudio_root_file(), "/Code/UtilityFunctions.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/References.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/DelayFunctions.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/SQLFunctions.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/MainFunction.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/CombineRDatas.R"))

#setwd("C:/R_PRojects/Hello-world-r/Data/RData")

setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("AllRDatas.RData")

setwd(data_rdata_dir)
# Sys.sleep(60*15)
PurgeData(all=T)

TransferData()
#transfers data from G drive to C drive

LoadData_Bulk()
#initial load data
timestamp()


ProcessData_Bulk()

