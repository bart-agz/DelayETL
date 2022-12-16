# SQLLoad(DispatchList)
# x=SQLLoad("DispatchList")
# x
# x[agrepl("All_ND",x$DelayCodes),]
# unique(x$DelayCodes)

# z=x[x$RevDate=="2022-12-12",]
# z$DelayCodes


setwd("C:/R_Projects/DelayETL/Data/Processed")
fs=list.files()
rm(tx)
for (f in fs){
  setwd(paste0("C:/R_Projects/DelayETL/Data/Processed/",f))
  load("PhaseAB.RData")
  rk=11
  rks=SelectRKs(x)
  del=c()
  print("Checking for All_NDs")
  for (rk in rks){
    t=x[x$RK==rk,]
    hl=t[1,]$Note=="HL"
    nd="ND" %in% t$Note
    if (!hl & nd){
      t=OverrideNA(t)
      l1=sum(t$Code=="ND")==nrow(t)
      l2=all(t$DC1==t$DO1) & nrow(t)>=5
      # print(paste(rk,l1))
      if (l1 | l2){
        print("modifying all ND to ALL_ND")
        t$Del=0
        t$Note="All_ND"
        vx=which(t$LRD>=1000)
        if (length(vx)>=1){
          t[vx,]$LRD=0
          t[vx,]$Delay=0
        }
        if (t[1,]$SDO==""){
          t[1,]$SDO=t[1,]$DO
        }
        if (t[1,]$SDC==""){
          t[1,]$SDC=t[1,]$DC
        }
        if (t[1,]$SDO1==""){
          t[1,]$SDO1=t[1,]$DO1
        }
        if (t[1,]$SDC1==""){
          t[1,]$SDC1=t[1,]$DC1
        }
        t[1,]$SDC
        x=x[x$RK!=t$RK[1],]
        t[1,]$Note="HL"
        t[1,]$Del=1
        # t$LRD=0
        t$DD=0
        print(t)
        if (exists('tx')){
          tx=rbindlist(list(tx,t))
        } else {
          tx=t
        }
        x=rbindlist(list(x,t))
      }
    }
  }
}  
# 03DEC2022 
# 11DEC2022 
# 09DEC2022 
# 13DEC2022 
ta=table(tx$RevDate)

for (i in seq(1,length(ta))){
  dz=names(table(tx$RevDate))[i]
  a=tx[tx$RevDate==dz,]
  print(a)
  setwd(paste0("C:/R_Projects/DelayETL/Data/Processed/",dz))
  load("PhaseAB.RData")
  print(carlist[a$RK[1]==carlist$RK,])
}


