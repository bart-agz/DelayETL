rm(list=ls())

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
