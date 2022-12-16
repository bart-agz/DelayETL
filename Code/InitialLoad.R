# InitialLoad
rm(list=ls())
#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("Directories.RData")
# load("UtilityFunctions.RData")
setwd(data_rdata_dir)
LoadData=function(date){
  print(date)
  # initial load and cleanup of badown data
  # date="08NOV2022"
  # date="13DEC2022"
  fn=paste0(date,'.csv')
  setwd(data_processed_dir)
  dir.create(date,F)
  setwd(data_individual_dir)
  del=fread(fn)
  hol=as.data.table(unique(cbind(del$RevDate,del$SchDate,del$SchCode)))
  colnames(hol)=c("RevDate","SchDate","SchCode")
  del100=del
  del=as.data.table(del)
  del=ConvertTimes_ToTSec(del)
  for (i in c("Calculating RK and generating: carlist, dates, and pot")){
    print(i)
    print(nrow(del[del$RUNKEY=="",])==0)
    del=del[order(del$SDispatch,del$DC1),]
    v1=nrow(del)
    
    rk=as.data.table(unique(del$RUNKEY))
    colnames(rk)="RUNKEY"
    rk$RK=seq(1,nrow(rk))
    by="RUNKEY"
    del=merge(del,rk,by)
    print(nrow(del)==v1)
    vx=cc("RevDate RUNKEY RK SRK Route LastStation Location TransfersOff TransfersOn")
    vx %in% colnames(del)
    off=reordordt(del,vx)
    cars=reordordt(del,cc("RevDate RUNKEY RK SRK SLength Length FOTF CAR1 CAR2 CAR3 CAR4 CAR5 CAR6 CAR7 CAR8 CAR9 CAR10") )
    cars=unique(cars)
    cars$Id=seq(1,nrow(cars))
    
    
    print(length(unique(cars$RK))==length(unique(del$RK)))
    
    
    del=reordordt(del,setdiff(colnames(del),c("Color",paste0("CAR",seq(1,10)))))
    dates=reordordt(del,cc("DayType RevDate SchDate SchCode"))
    dates=unique(dates)
    dates$Id=seq(1,nrow(dates))
    
    del=reordordt(del,setdiff(colnames(del),cc("DayType SchDate SchCode")))
    del
    
    pot=reordordt(del,cc("RevDate RK RUNKEY Exits HPOT LPOT UPOT"))
    pot=unique(pot)
    pot$Id=seq(1,nrow(pot))
    
    del=reordordt(del,setdiff(colnames(del),cc("Exits HPOT LPOT UPOT")))
    
    del=del[,!"SRoute"]
    # del=del[,!"STrain"]
    del=del[,!"Length"]
    
    del=del[,!"OnePassups"]
    del=del[,!"MultiPassups"]
    del=del[,!"FOTF"]
    del=del[,!"RUNKEY"]
    pot=pot[order(pot$RK,pot$Exits),]
    pot=pot[!duplicated(pot$RK),]
    # pot[pot$RK==16,]
    print(table(del$ScheduleNote))
    vx=names(table(cars$RK)[table(cars$RK)>=2])
    if (length(vx)>=1){
      for (i in seq(1,length(vx))){
        te=cars[cars$RK==vx[i]]
        te=te[!is.na(te$SLength),]
        cars=cars[!(cars$RK %in% vx[i]),]
        cars=rbindlist(list(cars,te))
      }
    }
    print(nrow(cars)==nrow(pot))
  }
  for (i in c("Renaming Variables")){
    #short hand column names, names will be reverted back later on
    print(i)
    colnames(del)[colnames(del)=="PL"]="PLe"
    colnames(del)[colnames(del)=="DwellDelay"]="DD"
    colnames(del)[colnames(del)=="ScheduleNote"]="Note"
    # colnames(del)[colnames(del)=="PreviousLateness"]="PL"
    # colnames(del)[colnames(del)=="CurrentLateness"]="CL"
    colnames(del)[colnames(del)=="Lateness"]="L"
    colnames(del)[colnames(del)=="Origin"]="Or"
    colnames(del)[colnames(del)=="Dest"]="De"
    colnames(del)[colnames(del)=="SOrigin"]="SOr"
    colnames(del)[colnames(del)=="SDest"]="SDe"
    colnames(del)[colnames(del)=="Route"]="Rt"
    colnames(del)[colnames(del)=="ATrain"]="Tr"
    colnames(del)[colnames(del)=="TrackNumber"]="TN"
    colnames(del)[colnames(del)=="ReverseRun"]="RR"
    # colnames(del)[colnames(del)=="LateDispatch"]="LD"
    # colnames(del)[colnames(del)=="ActualRunTime"]="ART"
    # colnames(del)[colnames(del)=="ScheduledRunTime"]="SRT"
    colnames(del)[colnames(del)=="RUNKEY"]="RK"
    colnames(del)[colnames(del)=="TotalPatronsDO"]="PDO"
    colnames(del)[colnames(del)=="TotalPatronsDC"]="PDC"
    colnames(del)[colnames(del)=="LinkRunDelay"]="LRD"
    colnames(del)[colnames(del)=="Delayed"]="Del"
    colnames(del)[colnames(del)=="NoService"]="NS"
    colnames(del)[colnames(del)=="Location"]="Loc"
    colnames(del)[colnames(del)=="LastLoc"]="LLoc"
  }
  del$ind=seq(1,nrow(del))
  rks=unique(del$RK)
  rks=setdiff(rks,"")
  print(all(na.omit(sort(pot$RUNKEY))==na.omit(sort(cars$RUNKEY))))
  print(nrow(pot)==nrow(cars))
  # del=del[,!"LD"]
  vx=cc("RevDate RK SRK Rt STrain Train SOr SDe Or De TN RR Dir Loc LLoc DO DC SDO SDC DO1 DC1 SDO1 SDC1 LRD DD PLe Delay Note Del PDO PDC EOL TS")
  vx[!vx %in% colnames(del)]
  
  colnames(del)[!colnames(del) %in% vx]
  vx[!vx %in% colnames(del)]
  del=reordordt(del,vx)
  del=del[order(del$RK,del$DC1),]
  del$ind=seq(1,nrow(del))
  del=StripSeconds(del)
  del$Code=""
  # v1=nrow(pot)
  # by=cc("RUNKEY RevDate")
  # pot=merge(pot,cars,by)
  # pot[pot$RUNKEY=="467/ 0:47:48",]
  # print(nrow(pot)==nrow(cars)-sum(cars$RUNKEY=="Hole"))
  # print(nrow(del[del$Loc==del$SOr,]))
  
  # pot[!pot$RUNKEY %in% del[del$Loc==del$SOr,]$RUNKEY,]
  # if (nrow(pot)!=v1){sys}
  # pot=pot[,!"FOTF"]
  # pot=pot[,!"CAR1"];  pot=pot[,!"CAR2"]
  # pot=pot[,!"CAR3"];  pot=pot[,!"CAR4"]
  # pot=pot[,!"CAR5"];  pot=pot[,!"CAR6"]
  # pot=pot[,!"CAR7"];  pot=pot[,!"CAR8"]
  # pot=pot[,!"CAR9"];  pot=pot[,!"CAR10"]
  # cars=cars[cars$RUNKEY!="Hole",]
  dates=reordordt(dates,cc("Id RevDate DayType SchDate SchCode"))
  carlist<<-cars
  pot<<-pot
  datesl<<-dates
  print(nrow(pot)==nrow(cars))
  
  fn=paste0(data_processed_dir,"/",date)
  setwd(fn)
  fwrite(del,paste(datesl[1]$RevDate,"Delays.csv"))
  fwrite(carlist,paste(datesl[1]$RevDate,"CarList.csv"))
  fwrite(datesl,paste(datesl[1]$RevDate,"Dates.csv"))
  fwrite(pot,paste(datesl[1]$RevDate,"POT.csv"))
  fwrite(hol,paste(datesl[1]$RevDate,"HOL.csv"))
  
  scheduled_routes=seq(1,99)
  
  del0<<-del
  us=del[!(del$Rt %in% scheduled_routes),]
  del=del[del$Rt %in% scheduled_routes,]
  
  print(nrow(cars)==nrow(pot))
  # +sum(del$Note=="HL"))
  fwrite(us,paste(datesl[1]$RevDate,"Unscheduled.csv"))
  fwrite(off,paste(datesl[1]$RevDate,"Offloads.csv"))
  us<<-us  
  x=del
  x<<-x
  # runkey<<-q
  del<<-del
  del100<<-del100
  hol<<-hol
  print(date)
}
# date="08NOV2022"
# LoadData(date)
LoadData_Bulk=function(full=F){
  setwd(data_individual_dir)
  ds=list.files()
  # ds=SortDays(ds)
  for (d in ds){
    date=strsplit(d,"\\.")[[1]][1]
    setwd(data_processed_dir)
    if (!(date %in% list.files()) | full){
      LoadData(date)      
    }
  }
}

save.image("LoadData.RData")
