rm(list=ls())
#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("Directories.RData")

Add_AD=function(x){
  # Function: Calculates AD Accumulated Delays with delays > ADThreshold (30) per runkey
  # adds AD column with index
  x$AD=""
  rks=SelectRKs(x)
  for (rk in rks){
    ad=1;t=sr(x,rk)
    for (i in seq(1,nrow(t))){
      t=sr(x,rk)
      t=t[i:nrow(t),]
      if (all(t$Code=="")){
        #all t$Code=="" to not accumulated already identified delays
        #6/27
        # t=KeepPositive(t,"Delay")
        t=PositiveDelays(t,ADThreshold)
        l1=sum(t$Delay)>=DelayThreshold & nrow(t)>1
        if (l1){
          l2=sum(agrepl("AD",t$Code,max.distance = 0))>0
          if (!l2){
            # 5/31 not needed? reversed condition
            t=AddCode(t,ad,"AD")
            t=AddCode(t,"AD")
            ad=ad+1
            x=Update(x,t)
          }
        }
      }
    }
  }
  return(x)
}
PositiveDelays=function(t,ADThreshold){
  # Function: return positive delays above threshold in a continous link
  # if 1 row, return input t
  # ADThreshold=30 (default)
  # 60 45 31 30 29 45 will return 60 to 30 (4/6)
  # 60 50 20 24 55 11 will return 60 to 50 (2/6)
  # Necessary for AD calculations
  if (nrow(t)==1){
    return(t)
  }
  vx=c(t$Delay>=ADThreshold)
  # if (sum(c(t$Delay>30 & t$Delay<45))){
  #   print(t$RK)
  # }
  vx1=which(vx==1)
  if (!all(diff(vx1)==1) | F %in% vx){
    vx0=which(vx==0)  
    t=t[vx1[vx1<min(vx0)],]
  }
  return(t)
}
Sum_AD=function(x){
  # Function: adds ADS (Sum of ADS per AD group)
  # modifies del event to 1 for each station with AD group #
  x$ADS=NA
  rks=SelectRKs(x)
  for (rk in rks){
    t=sr(x,rk)
    l1=agrepl("AD",t$Code,0)
    if (sum(l1)>=1){
      q=t[,sum(Delay),by="AD"]
      colnames(q)[2]="ADS"
      t=merge(t,q,by="AD")
      t[!agrepl("AD",t$Code,0),]$ADS.y=NA
      t$ADS.x=t$ADS.y
      t=t[,!"ADS.y"]
      colnames(t)[colnames(t)=="ADS.x"]="ADS"
      t[t$AD>=1,]$Del=1
      x=Update(x,t)
    }
  }
  return(x)
}
Override_AD=function(x){
  # not used
  # function, overrides code to AD if AD + any other code
  # does not override CL or OL codes, cancellations or offloads
  rks=SelectRKs(x)
  for (rk in rks){
    t=sr(x,rk)
    l0=l1=agrepl("AD",t$Code, 0)
    l1=agrepl("AD",t$Code, 0) & unlist(lapply(t$Code,function(x) length(strsplit(x,"\\ ")[[1]])))>1
    if (sum(l1)>=1){
      v=lapply(t[l1,]$Code,function(x) strsplit(x,"\\ ")[[1]])
      # v=lapply(v,function(x) intersect(x,cc("AD OL CL")))
      #removed 6.23.22
      v=unique(unlist(v))
      v=paste0(v,collapse = " ")
      t[l1,]$Code=v
      x=Update(x,t)
    }
  }
  return(x)
}
Expand_AD=function(x){
  x$DL=""
  rks=SelectRKs(x)
  for (rk in rks){
    t=sr(x,rk)
    l1=agrepl("AD",t$Code, 0)
    if (sum(l1)>=1){
      # print(rk).
      # print(unique(t[l1,]$Loc))
      t[l1,]$DL=t[l1,]$Loc
      q=t[,max(ind),"AD"]
      q=q[q[[1]]!="",]
      colnames(q)[2]="ind"
      t=merge(q,t[l1,],by=cc("AD ind"))
      # t$Delay=t$ADS
      x=Update(x,t)
      t=sr(x,rk)
      t=t[l1 & t$DL=="",]
      t$DL=t$Loc
      t$Del=0
      #overwrite ADS Delay to 0
      t$ADS=0
      #Combine DL per AD group
      ads=unique(t$AD)
      ads=ads[ads!=""]
      # for (ad in ads){
      #   vx=t$AD==ad
      #   t[vx,][nrow(t[vx,])]$DL=paste(t[vx,]$DL,collapse=" ")
      # }
      x=Update(x,t)
    }
  }
  x[x$Code!="" & x$L=="",]$L=x[x$Code!="" & x$L=="",]$Loc
  return(x)
}
save.image("AD_Functions.RData")
