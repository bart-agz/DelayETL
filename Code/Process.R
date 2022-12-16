rm(list=ls())
# Main Process Function
rm(list=ls())
setwd("C:/R_Projects/DelayETL")
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
# PurgeData("12DEC2022")
date="03DEC2022"
insertDataToSQL_Logic=F

while (T){
  TransferData()
  #transfers data from G drive to C drive
  
  LoadData_Bulk()
  # LoadData("15NOV2022")
  #initial load data
  timestamp()
  insertDataToSQL_Logic=T
  
  setwd(data_processed_dir)
  ds=list.files()
  ds
  date=ds[9]
  # ds=ds[9:length(ds)]
  ds=rev(ds)
  ds=ds[order(as.Date(ds,"%d%B%Y"))]
  date=ds[2]
  #Upload Delays
  find_rstudio_root_file()
  for (date in ds){
    print(date)
    timestamp()
    
    AlreadyCompleted=CheckDates(date,'Delay')
    if (!AlreadyCompleted){
      d<<-date
      print(date)
      wd<<-paste0(data_processed_dir,'/',date)
      # wd_out<<-paste0(data_processed_dir,'/',date,"/FinalOutputs")
      # if (!dir.exists(wd_out)){
      print(date)
      source(paste0(sep="",find_rstudio_root_file(), "/Code/MainFunction.R"))
      # }
    }
    AlreadyCompleted=CheckDates(date,'VehIncident')
    if (!AlreadyCompleted){
      print(date)
      TransferVehicle(date)    
    }
  }
  print("Waiting")
  timestamp()
  Sys.sleep(60*30)
}
#318
