# Main Process Function

rm(list=ls())

#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
# load("Directories.RData")
load("AllRdatas.RData")
# load("UtilityFunctions.RData")
date<-"06NOV2022"



ProcessData=function(date){
  wd<<-paste0(data_processed_dir,'/',date)
  wd_out<<-paste0(data_processed_dir,'/',date,"/FinalOutputs")
  dir.create(wd_out,F)
  fn_delays<<-paste0(date," delays.csv")
  fn_us<<-paste0(date," unscheduled.csv")
  fn_pot<<-paste0(date," POT.csv")
  
  fn_carlist<<-paste0(date," carlist.csv")
  setwd(wd)
  us<<-fread(fn_us)
  pot<<-fread(fn_pot)
  carlist<<-fread(fn_carlist)
  x<<-fread(fn_delays)
  mode(x$Code)<<-"character"
  x[is.na(x$Code),]$Code<<-""
  x0<<-x
  
  SaveReload("Phase00.RData")
  print(head(x))
  x<<-Correct_WrongData(x)
  verb<<-F
  x<<-Correct_OL(x)
  x<<-Remove_All_ND(x)
  x<<-OL_ND(x)
  x<<-Calculate_NextRK(x)
  
  
  SaveReload("Phase01.RData")
  
  x<<-Fix_CL(x)
  x<<-Add_DH_LR(x)
  x<<-Add_OL_CN_HL_ND(x) 
  
  x<<-HL_ND_OL(x)
  x<<-Add_LD_ED(x)
  
  SaveReload("Phase02.RData")
  
  x<<-Delete_Error_ER(x)
  x<<-Add_CD(x)
  
  SaveReload("Phase03.RData")
  
  print("accumulated delays")
  x<<-Add_AD(x)
  
  SaveReload("Phase04.RData")
  
  x<<-Sum_AD(x)
  x<<-Calculate_Passenger_Delay(x)
  x<<-Expand_AD(x)
  
  # x<<-Override_AD(x)
  # x<<-Expand_AD_Locs(x)
  x<<-Calculate_LA(x)
  x<<-Add_Del_Locations(x)
  
  SaveReload("Phase05.RData")
  
  print("Collapsing Delays")
  del<<-CollapseDelays(x)
  del<<-GetPreviousTrain(del)
  
  SaveReload("Phase06.RData")
  
  print("Grouping Delays")
  del<<-GroupDelays(del,x)
  del<<-Group_Delays_LA(del)
  SaveReload("Phase07.RData")
  
  del<<-GroupDelays2(del,x)
  
  SaveReload("Phase08.RData")
  SaveReload("Phase08.RData",F,T)
  
  
  del<<-Normalize_Groups(del)
  del<<-HL_Correction(del)
  del<<-SortID(del)
  
  SaveReload("Phase09.RData")
  
  print("Normalizing")
  
  di<<-CalculateDispatches(x)
  di<<-MergePot()
  
  #adds non scheduled POTs
  vx<<-cc("Id RK RevDate Rt SDispatch STrain Or De SOr SDe EOL TotalDelay DelayCount DelayCodes PDelayed PDelay Locs Exits HPOT LPOT UPOT")
  di<<-reordordt(di,vx)
  di1<<-di
  
  del0<<-fread(fn_delays)
  ex<<-del0[del0$RK %in% unique(setdiff(del0$RK,x$RK)),]
  x<<-rbindlist(list(x,ex),fill=T)
  
  print(length(unique(x$RK))==length(unique(carlist$RK)))
  
  SaveReload("Phase10.RData")
  print("holes")
  print(del[del$Code=="HL",])
  
  
  
  
  
  
  del<<-CleanDel(del)
  x<<-CleanDetailed(x)
  di<<-CleanDetailed(di)
  di$Id<<-seq(1,nrow(di))
  
  SaveReload("Phase11.RData")
  SaveReload("Phase11.RData",save=F,reload=T)
  
  v<<-c("Id",setdiff(colnames(x),"ind"))
  x$Id<<-seq(1,nrow(x))
  x<<-x[,!"ind"]
  x<<-reordordt(x,v)
  x1<<-x;del1<<-del;di1<<-di
  
  vars<<-cc("SchedOrigin SchedDest ActOrigin ActDest StartLoc EndLoc")
  del<<-Normalize(del,vars,"LocationList",T)
  x<<-Normalize(x,cc("SchedOrigin SchedDest ActOrigin ActDest Loc LLoc"),"LocationList",T)
  del<<-Normalize(del,cc("DelayCode"),"DelayTypesSortOrder",T)
  x<<-Normalize(x,cc("DelayCode"),"DelayTypesSortOrder",T)
  di<<-Normalize(di,cc("SchedOrigin SchedDest"),"LocationList",T)
  
  # del<<-del[,!"SortId"]
  print("Outputing")
  x<<-x[,!"ind"]
  del<<-del[,!"ind"]
  carlist<<-carlist[,!"ind"]
  di<<-di[,!"ind"]
  
  x1<<-x[,!"ind"]
  del1<<-del[,!"ind"]
  di1<<-di[,!"ind"]
  colnames(x)[colnames(x)=="RunKey"]<<-"RK"
  colnames(x1)[colnames(x1)=="RunKey"]<<-"RK"
  di<<-reordordt(di,cc("Id RevDate RunKey Route SDispatch SchedTrain ActOrigin ActDest SchedOrigin SchedDest EOLDelay TotalDelay DelayCount DelayCodes PassDelay PDelay Locs Exits HPOT LPOT UPOT"))
  di1<<-reordordt(di1,cc("Id RevDate RunKey Route SDispatch SchedTrain ActOrigin ActDest SchedOrigin SchedDest EOLDelay TotalDelay DelayCount DelayCodes PassDelay PDelay Locs Exits HPOT LPOT UPOT"))
  
  SaveReload("Phase12.RData")
  SaveReload("Phase12.RData",save=F,reload =T)
  setwd(wd_out)
  fwrite(x,paste0(date,' TrainRun.csv'))
  fwrite(del,paste0(date,' Delay.csv'))
  carlist<<-reordordt(carlist,c("Id",setdiff(colnames(carlist),"Id")))
  fwrite(carlist,paste0(date,' CarList.csv'))
  fwrite(di,paste0(date,' Dispatches.csv'))
  
  fwrite(x1,paste0(date,' TrainRun Non-Norm.csv'))
  fwrite(del1,paste0(date,' Delay Non-Norm.csv'))
  fwrite(di1,paste0(date,' Dispatches Non-Norm.csv'))
}

ProcessData_Bulk=function(){
  setwd(data_processed_dir)
  ds=list.files()
  # ds=SortDays(ds)
  for (date in ds){
    wd<<-paste0(data_processed_dir,'/',date)
    wd_out<<-paste0(data_processed_dir,'/',date,"/FinalOutputs")
    if (!dir.exists(wd_out)){
      ProcessData(date)
    }
  }
  setwd(wd)
}

save.image("MainFunction.RData")