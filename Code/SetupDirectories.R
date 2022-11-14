#Setup Directories and Directory References
#Only needs to be executed once to setup Directories
#11/9/22 AL
library("rprojroot")

rm(list=ls())
#dir="C:/R_PRojects/Hello-world-r"
dir=find_rstudio_root_file()

setwd(dir)
dir.create("Data",showWarnings = F)
dir.create("Code",showWarnings = F)
data_dir=(paste0(dir,"/","Data"))
code_dir=(paste0(dir,"/","Code"))

setwd(data_dir)
dir.create("Processed",showWarnings = F)
dir.create("Individual",showWarnings = F)
dir.create("Bulk",showWarnings = F)
dir.create("RData",showWarnings = F)
dir.create("References",showWarnings = F)

data_processed_dir=paste0(data_dir,"/Processed")
data_bulk_dir=paste0(data_dir,"/Bulk")
data_individual_dir=paste0(data_dir,"/Individual")
data_rdata_dir=paste0(data_dir,"/RData")
data_references_dir=paste0(data_dir,"/References")


setwd(data_rdata_dir)
# "C:/R_PRojects/Hello-world-r/Data/RData"
GDrive="G:/SAS_VIYA/New_TMAP/data"
setwd(GDrive)

setwd(data_rdata_dir)
save.image("Directories.RData")

#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
load("Directories.RData")
