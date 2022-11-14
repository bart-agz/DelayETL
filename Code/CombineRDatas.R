#combine all RDatas to one for one time load
rm(list=ls())

#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
rdatas=list.files(pattern=".RData")
rdatas=setdiff(rdatas,"AllRDatas.RData")

for (r in rdatas){
  load(r)
}
save.image("AllRDatas.RData")

setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))

#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
load("AllRDatas.RData")
