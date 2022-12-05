
d="20NOV2022"
wd<<-paste0(data_processed_dir,'/',d)
setwd(wd)
rm(list=ls())
load("Phase01.RData")


x[x$RK==359,]

nds=which(agrepl("ND",x$Note,0))
nd=nds[1]
for (nd in nds){
  x[x$RK==x[nd,]$RK
,]
}
