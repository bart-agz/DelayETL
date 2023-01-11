#Transfer RAW Badown and schedule from G drive to C Drive
#data is generated daily within SAS_VIYA
#SAS Viya takes whatever PFM did and simply puts it on the G Drive
#11/9/22 AL

rm(list=ls())
#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("Directories.RData")

TransferData=function(full_save=F){
  setwd(GDrive)
  ba=fread("Badown.csv")
  # etime=fread("etime.csv")
  
  if (full_save){
    setwd(data_bulk_dir)
    fwrite(ba,file="badown.csv")
  }
  # fwrite(etime,file="etime.csv")
  
  
  #split badown into individual days
  
  setwd(data_individual_dir)
  # file.remove(list.files())
  dz=unique(ba$RevDate)
  for (d in dz){
    print(d)
    te=ba[ba$RevDate==d,]
    fn=paste0(d,".csv")
    if (!(fn %in% list.files())){
      fwrite(te,fn)
    }
  }
}

# Vehicle_Folder=paste0(find_rstudio_root_file(),"/Data/Vehicles")
# setwd(Vehicle_Folder)
# C:\R_Projects\DelayETL\Data\Vehicles

TransferVehicleData=function(){
  setwd(GDrive_Vehicles)
  fs=list.files(pattern=".xlsx")
  fs=setdiff(fs,list.files(pattern="~"))
  fs=setdiff(fs,list.files(pattern=".bak"))
  for (f in fs){
    setwd(Vehicle_Folder)
    fn=strsplit(f,"\\.")[[1]]
    fn=fn[1:3]
    fn=paste0(fn,collapse=".")
    if (!(fn %in% list.files())){
      setwd(GDrive_Vehicles)
      x=readxl::read_xlsx(f)
      print(f)
      if ("VandalismFlags" %in% colnames(x)){
        fwrite(x,file=paste0(fn,".csv"))
      }
    }
  }
}


# Vehicles
TransferVehicle_OLD=function(){
  # setwd(Vehicle_Folder)
  fs=list.files(Vehicle_Folder)
  f=fs[1]
  for (f in fs){
    x=fread(f)
    x=x[!is.na(x$Date),]
    fn=strsplit(f,split=" ")[[1]][3]
    fn=paste0(strsplit(fn,"\\.")[[1]][1:3],collapse=".")
    date=as.Date(fn,"%m.%d.%Y")
    present=CheckDates(date,"VehIncident")
    present
    if (!present){
      x$VTDNum="null"
      x0=x
      x=Normalize(x,"Location","LocationList")
      x$Time=unlist(lapply(x$Time,function(x) strsplit(as.character(x),"\\ ")[[1]][2]))
      vx=cc("VehDate WorkOrderNum VehTime Train Car CarHours Yard Location ProblemCode VandalTags ShortDesc Narrative VTDNum")
      colnames(x)=vx
      cns=cc("VehDate VehTime WorkOrderNum VTDNum Train Car CarHours Yard Location ProblemCode VandalTags ShortDesc Narrative")
      # x$VehTime=format(strptime(x$VehTime, format="%H:%M:%S"), format = "%H:%M:%S")
      x$VehTime=period_to_seconds(hms(x$VehTime))
      # x$VehTime=hms(x$VehTime)
      vx=x$Train==""
      if (sum(vx)>=1){
        x[vx,]$Train="null"        
      }
      vx=x$VandalTags=="" | is.na(x$VandalTags)
      if (sum(vx)>=1){
        mode(x$VandalTags)="character"
        x[vx,]$VandalTags="null"
      }
      # x$Time=period_to_seconds(hms(unlist(lapply(x$Time,function(x) strsplit(as.character(x),"\\ ")[[1]][2]))))
      x[['VandalTags']]=paste0("'",x[['VandalTags']],"'")
      x=reordordt(x,cns)
      x$VehDate=as.Date(x$VehDate)
      x$Narrative=gsub("\\:","\\.",x$Narrative)
      x$Narrative=gsub("\\'","\\ ",x$Narrative)
      
      x$ProblemCode=gsub("\\:","\\.",x$ProblemCode)
      x$ProblemCode=gsub("\\'","\\ ",x$ProblemCode)
      
      
      x$ShortDesc=gsub("\\:","\\.",x$ShortDesc)
      x$ShortDesc=gsub("\\'","\\ ",x$ShortDesc)                                                                                           
      insertDataToSQL_Bulk(x,date,"VehIncident",T)
    }
  }
  # data=x[1,]
  # date="abc"
  # tableName="VehIncident"
}
TransferVehicle=function(date){
  # as.Date(date)
  # 21NOV2022
  d=as.Date(date,format="%d%B%Y")
  d1=month(d)
  d2=day(d)
  if (d1<=9){
    d1=paste0(0,d1)
  }
  if (d2<=9){
    d2=paste0(0,d2)
  }
  f=paste0(d1,".",d2,".",year(d))
  # setwd(GDrive_Vehicles)
  fs=list.files(path=GDrive_Vehicles,pattern=".xlsx")
  fs=setdiff(fs,fs[agrepl(".bak",fs,0)])
  fs=setdiff(fs,fs[agrepl("~",fs,0)])
  fn=fs[agrepl(f,fs,max.distance = 0)]
  file=paste0(GDrive_Vehicles,"/",fn)
  if (exists(file)){
    x=readxl::read_xlsx(file)
    x=as.data.table(x)
    x$VTDNum="null"
    x0=x
    x=Normalize(x,"Location","LocationList")
    x$Time=unlist(lapply(x$Time,function(x) strsplit(as.character(x),"\\ ")[[1]][2]))
    vx=cc("VehDate WorkOrderNum VehTime Train Car CarHours Yard Location ProblemCode VandalTags ShortDesc Narrative VTDNum")
    colnames(x)=vx
    cns=cc("VehDate VehTime WorkOrderNum VTDNum Train Car CarHours Yard Location ProblemCode VandalTags ShortDesc Narrative")
    # x$VehTime=format(strptime(x$VehTime, format="%H:%M:%S"), format = "%H:%M:%S")
    x$VehTime=period_to_seconds(hms(x$VehTime))
    # x$VehTime=hms(x$VehTime)
    # vx=x$Train==""
    # if (sum(vx)>=1){
    #   x[vx,]$Train="null"        
    # }
    vx=x$VandalTags=="" | is.na(x$VandalTags)
    if (sum(vx)>=1){
      mode(x$VandalTags)="character"
      x[vx,]$VandalTags="null"
    }
    # x$Time=period_to_seconds(hms(unlist(lapply(x$Time,function(x) strsplit(as.character(x),"\\ ")[[1]][2]))))
    x[['VandalTags']]=paste0("'",x[['VandalTags']],"'")
    x=reordordt(x,cns)
    x$VehDate=as.Date(x$VehDate)
    x$Narrative=gsub("\\:","\\.",x$Narrative)
    x$Narrative=gsub("\\'","\\ ",x$Narrative)
    
    x$ProblemCode=gsub("\\:","\\.",x$ProblemCode)
    x$ProblemCode=gsub("\\'","\\ ",x$ProblemCode)
    
    
    x$ShortDesc=gsub("\\:","\\.",x$ShortDesc)
    x$ShortDesc=gsub("\\'","\\ ",x$ShortDesc)                                                                                           
    insertDataToSQL_Bulk(x,date,"VehIncident")
  }
}
setwd(data_rdata_dir)
save.image("TransferData.RData")
