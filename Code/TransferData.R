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

setwd(data_rdata_dir)
save.image("TransferData.RData")
