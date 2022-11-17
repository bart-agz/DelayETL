rm(list=ls())
# Main Process Function
rm(list=ls())

#setwd to root of project
setwd(find_rstudio_root_file())
setwd(paste0(find_rstudio_root_file(),"/Code"))

fs=list.files(pattern=".R")
fs=setdiff(fs,"Process.R")
fs
source(paste0(sep="",find_rstudio_root_file(), "/Code/Libraries.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/SetupDirectories.R"))

source(paste0(sep="",find_rstudio_root_file(), "/Code/InitialLoad.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/ADFunctions.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/DelayFunctions.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/TransferData.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/UtilityFunctions.R"))


source(paste0(sep="",find_rstudio_root_file(), "/Code/References.R"))

source(paste0(sep="",find_rstudio_root_file(), "/Code/SQLFunctions.R"))
source(paste0(sep="",find_rstudio_root_file(), "/Code/CombineRDatas.R"))

#setwd("C:/R_PRojects/Hello-world-r/Data/RData")

setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("AllRDatas.RData")

setwd(data_rdata_dir)
# Sys.sleep(60*15)
PurgeData(all=T)

TransferData()
#transfers data from G drive to C drive

LoadData_Bulk(T)
# LoadData("15NOV2022")
#initial load data
timestamp()

# ProcessData_Bulk=function(){
  setwd(data_processed_dir)
  ds=list.files()
  # ds=SortDays(ds)
  for (date in ds){
    d<<-date
    print(date)
    wd<<-paste0(data_processed_dir,'/',date)
    wd_out<<-paste0(data_processed_dir,'/',date,"/FinalOutputs")
    if (!dir.exists(wd_out)){
      print(date)
      source(paste0(sep="",find_rstudio_root_file(), "/Code/MainFunction.R"))
    }
  }
  setwd(wd)
# }

date="15NOV2022"
setwd("C:/")
