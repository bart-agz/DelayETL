# Main Process Function

# rm(list=ls())

#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
# setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
# load("Directories.RData")
# load("AllRdatas.RData")
# load("UtilityFunctions.RData")
# date<-"06NOV2022"

# date="15NOV2022"

# ProcessData=function(date){
# date=date
wd=paste0(data_processed_dir,'/',date)
wd_out=paste0(data_processed_dir,'/',date,"/FinalOutputs")
dir.create(wd_out,F)
fn_delays=paste0(date," delays.csv")
fn_us=paste0(date," unscheduled.csv")
fn_pot=paste0(date," POT.csv")


fn_carlist=paste0(date," carlist.csv")
setwd(wd)
off=fread(paste0(date," Offloads.csv"))
us=fread(fn_us)
pot=fread(fn_pot)
carlist=fread(fn_carlist)
x=fread(fn_delays)
mode(x$Code)="character"
x[is.na(x$Code),]$Code=""
x0=x

SaveReload("Phase00.RData")
load("Phase00.RData")
print(head(x))
x=Correct_WrongData(x)
SaveReload("Phase00a.RData")
load("Phase00a.RData")
x=Extend_Scheduled_Metrics(x)



verb=F
x=Correct_OL(x)
save.image("PhaseAB.RData")
load("phaseAB.RData")
x=Modify_All_ND(x)

x=OL_ND(x)
x=Calculate_NextRK(x)

SaveReload("Phase01.RData")
load("Phase01.RData")

x=Fix_CL(x)
x=Add_DH_LR(x)
x=Add_OL_CN_HL_ND(x) 

SaveReload("Phase01a.RData")
load("Phase01a.RData")

###
# off
# x
###

x=HL_ND_OL(x)
x=Add_LD_ED(x)

SaveReload("Phase02.RData")
load("PHase02.RData")
x=OverrideNA(x,"Code")
x=Delete_Error_ER(x)
x=Add_CD(x)

SaveReload("Phase03.RData")
# load("Phase03.RDAta")

print("accumulated delays")
x=Add_AD(x)

SaveReload("Phase04.RData")
load("phase04.RData")

x=Sum_AD(x)
x=Expand_AD(x)
print("ADS" %in% colnames(x))


SaveReload("Phase04_1.RData")
# rm(list=ls())
load("phase04_1.RData")

x=Calculate_Passenger_Delay(x)
SaveReload("Phase04_2.RData")
load("phase04_2.RData")


# print("Added Passenger Delays")
# print(x)
# x=Override_AD(x)
# x=Expand_AD_Locs(x)
x=Calculate_LA(x)
x=Add_Del_Locations(x)

SaveReload("Phase05.RData")
load("Phase05.RData")

print("Collapsing Delays")
del=CollapseDelays(x)
del=GetPreviousTrain(del)

SaveReload("Phase06.RData")
load("Phase06.RData")
print("Grouping Delays")
del=GroupDelays(del,x)
SaveReload("Phase06a.RData")
load("Phase06a.RData")
del=Group_Delays_LA(del)
SaveReload("Phase07.RData")
load("Phase07.RData")

del=GroupDelays2(del,x)

SaveReload("Phase08.RData")
load("Phase08.RData")

del=Group_Adjacent_Stations_RK(del)

SaveReload("Phase08a.RData")
load("Phase08a.RData")
del=Normalize_Groups(del)
del=HL_Correction(del)
del=SortID(del)

SaveReload("Phase09.RData")
rm(list=ls())
load("Phase09.RData")
print("Normalizing")

di=CalculateDispatches(x)
di=MergePot()
SaveReload("Phase09a.RData")
load("Phase09a.RData")
di=ExpandDI(di)
#adds non scheduled POTs
vx=cc("Id RK RevDate Rt SDispatch STrain Or De SOr SDe EOL TotalDelay DelayCount DelayCodes PDelayed PDelay Locs Exits HPOT LPOT UPOT Scheduled Delayed Late")
di=reordordt(di,vx)
di1=di

del0=fread(fn_delays)
ex=del0[del0$RK %in% unique(setdiff(del0$RK,x$RK)),]
x=rbindlist(list(x,ex),fill=T)

print(length(unique(x$RK))==length(unique(carlist$RK)))

SaveReload("Phase10.RData")
load("Phase10.RData")
print("holes")
print(del[del$Code=="HL",])






del=CleanDel(del)
x=CleanDetailed(x)
di=CleanDetailed(di)
di$Id=seq(1,nrow(di))

SaveReload("Phase11.RData")
SaveReload("Phase11.RData",save=F,reload=T)
# setwd("C:/R_Projects/DelayETL/Data/Processed/13NOV2022")

load("Phase11.RData")
v=c("Id",setdiff(colnames(x),"ind"))
x$Id=seq(1,nrow(x))
x=x[,!"ind"]
x=reordordt(x,v)
x1=x;del1=del;di1=di
delNonNorm=del1
vars=cc("SchedOrigin SchedDest ActOrigin ActDest StartLoc EndLoc")
del=Normalize(del,vars,"LocationList",T)
x=Normalize(x,cc("SchedOrigin SchedDest ActOrigin ActDest Loc LLoc"),"LocationList",T)
del=Normalize(del,cc("DelayCode"),"DelayTypesSortOrder",T)
x=Normalize(x,cc("DelayCode"),"DelayTypesSortOrder",T)
di=Normalize(di,cc("SchedOrigin SchedDest ActOrigin ActDest"),"LocationList",T)

# del=del[,!"SortId"]
print("Outputing")
x=x[,!"ind"]
del=del[,!"ind"]
carlist=carlist[,!"ind"]
di=di[,!"ind"]

x1=x[,!"ind"]
del1=del[,!"ind"]
di1=di[,!"ind"]
colnames(x)[colnames(x)=="RunKey"]="RK"
colnames(x1)[colnames(x1)=="RunKey"]="RK"
di=reordordt(di,cc("Id RevDate RunKey Route SDispatch SchedTrain ActOrigin ActDest SchedOrigin SchedDest EOLDelay TotalDelay DelayCount DelayCodes PassDelay PDelay Locs Exits HPOT LPOT UPOT Scheduled Delayed Late"))
di1=reordordt(di1,cc("Id RevDate RunKey Route SDispatch SchedTrain ActOrigin ActDest SchedOrigin SchedDest EOLDelay TotalDelay DelayCount DelayCodes PassDelay PDelay Locs Exits HPOT LPOT UPOT Scheduled Delayed Late"))

SaveReload("Phase12.RData")
SaveReload("Phase12.RData",save=F,reload =T)
# setwd("C:/Users/jagnew/source/repos/bart-agz/DelayETL/Data/Processed/13NOV2022/")
load("Phase12.RData")
setwd(wd_out)
di1[di1=="",]=NA
di1[di1==" ",]=NA
x$Note=trimws(x$Note)

x[x=="",]=NA
x[x==" ",]=NA

del[del=="",]=NA
del[del==" ",]=NA

carlist[carlist=="",]=NA
carlist[carlist==" ",]=NA


di[di=="",]=NA
di[di==" ",]=NA
di[is.na(di$Locs),]$Locs=""
di[is.na(di$DelayCodes),]$DelayCodes=""

vars=cc("DO DC SDC SDO AD Note DL")
for (var in vars){
  x[is.na(x[[var]]),][[var]]=""
}
vars=cc("AD Note DL DO DC SDO SDC")
for (var in vars){
  x[x[[var]]=="",][[var]]="null"
}
SaveReload("Phase13.RData")
SaveReload("Phase13.RData",save=F,reload =T)
load("Phase13.RData")

fwrite(x,paste0(date,' TrainRun.csv'))
fwrite(del,paste0(date,' Delay.csv'))
carlist=reordordt(carlist,c("Id",setdiff(colnames(carlist),"Id")))
fwrite(carlist,paste0(date,' CarList.csv'))
fwrite(di,paste0(date,' Dispatches.csv'))

fwrite(x1,paste0(date,' TrainRun Non-Norm.csv'))
fwrite(delNonNorm,paste0(date,' Delay Non-Norm.csv'))
fwrite(di1,paste0(date,' Dispatches Non-Norm.csv'))

print(nrow(carlist)==nrow(di) & length(unique(di$RunKey))==length(unique(carlist$RUNKEY)) & length(intersect(carlist$RK,di$RunKey))==nrow(di))
colnames(off)[colnames(off)=="Location"]="Loc"
byv=cc("RevDate RK Loc")
off=reordordt(off,c(byv,cc("TransfersOff TransfersOn")))
mode(x[[byv[1]]])==mode(off[[byv[1]]])
mode(x[[byv[2]]])==mode(off[[byv[2]]])
mode(off[[byv[3]]])=mode(x[[byv[3]]])



xt=merge(x,off,by=byv,all.x=T)
print(nrow(xt)==nrow(x))
x=xt
#### start writing data.table to sql
#insert delay data into sql table
if (insertDataToSQL_Logic){
  insertDataToSQL_Bulk(del, date, "Delay")
  
  #insert CarList data into sql table
  insertDataToSQL_Bulk(carlist, date, "CarList")
  
  #insert DispatchList data into sql table
  insertDataToSQL_Bulk(di, date, "DispatchList")
  
  #insert RunList data into sql table
  insertDataToSQL_Bulk(x, date, "RunList")
  # }
}

# save.image("MainFunction.RData")