rm(list=ls())
#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("Directories.RData")

Correct_OL=function(x){
  # delete OL for trains going beyond scheduled destinations
  rks=SelectRKs(x)
  for (rk in rks){
    t=sr(x,rk)
    if ("OL" %in% t$Note){
      # print(rk)
      last_scheduled_station=which(t$SDe==t$Loc)
      OL=which(t$Note=="OL")
      if (length(OL)>=2){
        sys_OL_ERROR
      }
      if (length(last_scheduled_station)+length(OL)>=2){
        if (OL>last_scheduled_station){
          t[OL,]$Del=0
          t[OL,]$Note=""
        }
      }
      x=Update(x,t)
    }
  }
  return(x)
}

Add_DH_LR=function(x){
  # Function: identifies DH Door Hold and LR Link Run Delays at a particular location
  # DH is DL Dwell Lateness >= DelayThreshold
  # LR is LRL Link Run Lateness >=DelayThreshold
  # DelayThreshold is 300 seconds (5 minutes)
  # Locations may have DH, LR, none, or both
  rks=SelectRKs(x)
  for (rk in rks){
    t=sr(x,rk)
    l1=(t$LRD>=DelayThreshold)
    if (sum(l1)>=1){
      t[l1,]$Del=1
      t=AddCode(t[l1,],"LR")
      x=Update(x,t)
    }
    t=sr(x,rk)
    l2=(t$DD>=DelayThreshold)
    if (sum(l2)>=1){
      # print(rk)
      t[l2,]$Del=1
      t=AddCode(t[l2,],"DH")
      x=Update(x,t)
    }
  }
  return(x)
}
Add_LD_ED=function(x){
  # Function: Adds LD and ED, late and early dispatch at scheduled origin 
  rks=SelectRKs(x)
  for (rk in rks){
    t=sr(x,rk)
    vx=t$SOr==t$Or & t$Loc==t$Or & t$Delay>=DelayThreshold & !(agrepl("HL",t$Code))
    l1=t[vx,]
    if (nrow(l1)>=1){
      t=AddCode(t[vx,],"LD")
      x=Update(x,t)
    }
    vx=t$SOr==t$Or & t$Loc==t$Or & t$Delay<=-DelayThreshold & t$Delay>(-10000) & !(agrepl("HL",t$Code))
    l1=t[vx,]
    if (nrow(l1)>=1){
      t=AddCode(t[vx,],"ED")
      t$Del=1
      x=Update(x,t)
    }
  }
  return(x)
}
Add_CD=function(x){
  # Function Adds "CD" Cumulative Delay Code if not EL, LD, LR, or DL
  # no dispatch errors and no LR or DL delays
  # delayed (>=300) but both LR and DL < 300
  vx=which(x$Del==1 & x$Code=="" & (x$Note=="" | agrepl("OF",x$Note,0) | agrepl("CN",x$Note,0)))
  if (sum(vx)>=1){
    x[vx,]$Code="CD"
  }
  return(x)
}

KeepNextCodes=function(t,i){
  #keep future delays if they have  code
  t=t[i:nrow(t),]
  vx=t$Code!=""
  if (sum(vx)==length(vx)){
    t=t[vx]
  } else {
    t=t[1:min(which(diff(as.numeric(vx))==-1)),]
  }
  return(t)
}

SimplifyDelays=function(x){
  rks=unique(x$RK)
  for (cd in setdiff(PossibleCodes,"AD")){
    x[[cd]]=0
  }
  for (rk in rks){
    t=x[x$RK==rk]
    cds=StripCodes(t$Code,T)
    cds=setdiff(cds,"AD")
    if (length(cds)>=1){
      for (i in seq(1,nrow(t))){
        cd=strsplit(t[i,]$Code,"\\ ")[[1]]
        if (length(intersect(cd,c("ED")))>=1){
          t[i,][["ED"]]=t[i,]$Delay
        }
        if (length(intersect(cd,c("LD")))>=1){
          t[i,][["LD"]]=t[i,]$Delay
        }
        if (length(intersect(cd,c("DH")))>=1){
          t[i,][["DH"]]=t[i,]$DL
        }
        if (length(intersect(cd,c("LR")))>=1){
          t[i,][["LR"]]=t[i,]$LRL
        }
      }
    }
    x=Update(x,t)
  }
  return(x)
}
CleanCodes=function(cds){
  cds=strsplit(cds,"")[[1]]
  if (length(cds)==0){
    return("")
  }
  if (cds[1]==" "){
    cds=cds[2:length(cds)]
  }
  cds=paste0(cds,collapse="")
  return(cds)
}
SelectCause=function(x){
  rks=unique(x$RK)
  x$M=0
  for (rk in rks){
    if (verb){print(rk)}
    t=x[x$RK==rk]; t0=t
    cds=StripCodes(t$Code,T)
    if (length(cds)>=1){
      for (i in seq(1,nrow(t))){
        t=x[x$RK==rk,]
        if (t[i,]$Code!="" & t[i,]$M==0 & i!=nrow(t)){
          t=KeepNextCodes(t,i)
          cds=StripCodes(t$Code,T)
          if (nrow(t)>=2){
            v=t[nrow(t),]$ADS
            if (!is.na(v)){
              t[nrow(t),]$Delay=v
              t[nrow(t),]$Code=paste0(unique(StripCodes(t$Code,T)),collapse=" ")
              t[seq(1,nrow(t)-1),]$Del=0
              t$M=1
              x=Update(x,t)
            }
          }
          if (nrow(t)==1){
            if (cds=="DH"){
              t[1,]$Delay=t[1,]$DL              
            } else if (cds=="LD" | cds=="ED"){
              t[1,]$Delay=t[1,]$Delay
            } else if (cds=="LR"){
              t[1,]$Delay=t[1,]$LRL
            } else if (cds=="DH"){
              t[1,]$Delay=t[1,]$DL
            } else if (cds=="SD"){
              t[1,]$Delay=t[1,]$Delay
            } else {
              sysundefinederror
            }
            t$M=1
            x=Update(x,t)
          }
        }
      }
    }
  }
  return(x)
}
StripCodes=function(t1,unique=T){
  # cds=paste0(unique(t1),collapse="")
  cds=paste0(unique(t1),collapse=" ")
  cds=gsub("\\  ","\\ ",cds)
  
  cds=strsplit(cds,"\\ ")[[1]]
  if (unique){
    cds=unique(cds)
  }
  cds=cds[cds!=""]
  cds=sort(cds)
  return(cds)
}
Summarize=function(x){
  rks=unique(x$RK)
  y=as.data.table(rks)
  cds0=StripCodes(x$Code,unique=T)
  for (cd in cds0){
    y[[cd]]=0
  }
  for (rk in rks){
    # print(rk)
    t=x[x$RK==rk,]
    cds=StripCodes(t$Code,unique=F)
    cds=table(cds)
    if (length(cds)>=1){
      for (cd in names(cds)){
        y[y$rks==rk,][[cd]]=cds[[cd]]        
      }
    }
  }
  colnames(y)[colnames(y)=="rks"]="rk"
  y$Sum=0
  y$Count=0
  for (rk in rks){
    v=c()
    for (cd in cds0){
      vx=y$rk==rk
      v=c(v,y[vx,][[cd]])
    }
    y[vx,]$Sum=sum(v)
    y[vx,]$Count=sum(as.numeric(v>0))
  }
  y=reordordt(y,c("rk","Sum","Count",cds0))
  return(y)
}

Combine_ADS=function(x){
  rks=unique(x$RK)
  for (rk in rks){
    t=x[x$RK==rk,]
    ads=unique(t$AD)
    ads=setdiff(ads,"")
    if (length(ads)>=2){
      # print(rks)
      for (i in seq(1,nrow(t))){
        t=x[x$RK==rk,]
        if (t$AD[i]!=""){
          t=KeepNextCodes(t,i)
          t0=t
          t=t[t$AD!="",]
          if (nrow(t0)==nrow(t)){
            v=sum(t[,min(ADS),AD][[2]])
            t$AD=min(t$AD)
            t$ADS=v
            x=Update(x,t)
          }
        }
      }
    }
  }
  return(x)
}
Fill_Empty_Codes=function(x){
  vx=x$Code=="" & x$Note!=""
  if (sum(vx)){
    x[vx,]$Code=x[vx,]$Note
  }
  return(x)
}

Delay_CleanCodes=function(x){
  for (i in seq(1,nrow(x))){
    v=strsplit(x[i,]$Code,"\\ ")[[1]]
    if (length(v)>=2){
      q=intersect(v,"AD")
      if (length(q)==1){
        x[i,]$Code=q
      } else {
        q=sort(v)[1]
        x[i,]$Code=q
      }
    }
  }
  return(x)
}

Add_OL_CN_HL_ND=function(x){
  # codes CL HL ND schedule notes and set delays to 0 (not OL)
  vx=x$Note=="CL" | x$Note=="Cancel Location"
  vx1=x$Or==x$Loc
  if (sum(vx)>=1){
    # x[vx & vx1,]$Del=1
    x[vx & vx1,]$Code="CL"
    x[vx & vx1,]$Delay=0
  }
  vx=x$Note=="Offload Location" | x$Note=="OL" 
  vx1=x$De==x$Loc
  if (sum(vx)>=1){
    # x[vx & vx1,]$Del=1
    x[vx & vx1,]$Code="OL"
    # x[vx & vx1,]$Delay=0
  }
  vx=x$Note=="No Dwell" | x$Note=="ND" | agrepl("ND",x$Note,0)
  if (sum(vx)>=1){
    # x[vx,]$Del=1
    x[vx,]$Code="ND"
    x[vx,]$Delay=0
  }
  vx=x$Note=="Hole" | x$Note=="HL"
  if (sum(vx)>=1){
    # x[vx,]$Del=1
    x[vx,]$Code="HL"
  }
  return(x)
}
Delete_Error_ER=function(x){
  # function: deletes erronous ED if 0 DC at dispatch
  # ED and next if LR delay occurs
  # should be very rare error
  # Sets delay to DL if ED==>LRL and DL
  rks=SelectRKs(x)
  del=c()
  for (rk in rks){
    t=x[x$RK==rk,]
    # print(t)
    t=OverrideNA(t)
    t0=t[1,]$Note!="HL"
    if (t0){
      l1=t[1,]$Code=="ED" & t[1,]$DO1==0
      if (l1){
        del=c(del,t[1,]$ind)
        cd=intersect(strsplit(t[2,]$Code,"\\ ")[[1]],c("DH","ND"))
        if (length(cd)==0){
          cd=""
        }
        t[2,]$Code=cd
        t[1,]$Del=0
        t[1,]$Code=""
        if (length(cd)==1 & cd==""){
          t[2,]$Del=0
        }
        t$Delay=t$DD
        #set delay to DD, does not include LR
      }
      x=Update(x,t)
    }
  }
  return(x)
}
Calculate_NextRK=function(del){
  # Function: Calculates next/previous runkey and previous train
  # Used for reclassifying LD as LA Late Arrival (recode LD's as LA)
  # based on terminal location and 
  # cars=cars[,!"RevDate"]
  vx=c(colnames(del),c("PRK","NRK"))
  # vx=cc("RevDate RK Rt Tr SOr SDe Or De TN RR Dir Loc LLoc DO DC SDO SDC DO1 DC1 SDO1 SDC1 DD CL PL PDO PDC ART SRT LRD PLe Delay Note Del EOL ind PRK NRK TS")
  # vx=colnames(del)
  v1=nrow(del)
  cl=reordordt(carlist,c("RK",paste0("CAR",seq(1,10))))
  del
  del=merge(del,cl,by="RK",all.x=T)
  
  del=del[order(del$ind),]
  rks=unique(del$RK)
  rk=rks[1]
  del$PRK=0
  del$NRK=0
  del$PTr=0
  for (rk in rks){
    te=sr(del,rk)
    lr=te[nrow(te),]
    nt=del[del$Loc==lr$Loc & del$DC1>lr$DC1 & del$CAR1==lr$CAR1 & !is.na(del$CAR1),]
    if (nrow(nt)>=1){
      nt=nt[1,]
      nt=sr(del,nt[1,]$RK)
      nt$PRK=rk
      te$NRK=nt$RK[1]
      nt$PTr=te[1,]$Tr
      nt$ind
      te$ind
      del=Update(del,nt)
      del=Update(del,te)
    }
  }
  del=reordordt(del,vx)
  if (0 %in% del$PRK){
    del[del$PRK==0,]$PRK=NA
  }
  if (0 %in% del$NRK){
    del[del$NRK==0,]$NRK=NA
  }
  return(del)
}
Calculate_Passenger_Delay=function(x){
  # save.image("dgv.RData")
  # load("phase04_1.RData")
  vx=x$Delay<0 & x$Code!="" & !agrepl("ED",x$Code,0)
  if (sum(vx)>=1){
    x[vx,]$Delay=0
  }
  # Function: estimate passenger delay using door and flow delay and passenger flows
  # never use negative delay or 0 delays
  x$PDelayed=0
  x$PDelay=0
  vx=agrepl("LR",x$Code,0)
  if (sum(vx)>=1){
    x[vx,]$PDelay=x[vx,]$PDelay+x[vx,]$PDO*(x[vx,]$LRD/(60*60))
    x[vx,]$PDelayed=x[vx,]$PDelayed+x[vx,]$PDO
  }
  vx=agrepl("DH",x$Code,0)
  if (sum(vx)>=1){
    x[vx,]$PDelay=x[vx,]$PDelay+x[vx,]$PDC*(x[vx,]$DD/(60*60))
    x[vx,]$PDelayed=x[vx,]$PDelayed+x[vx,]$PDC
  }
  vx=agrepl("CD",x$Code,0) 
  if (sum(vx)>=1){
    x[vx,]$PDelay=x[vx,]$PDelay+x[vx,]$PDO*(max(0,x[vx,]$LRD/(60*60)))+x[vx,]$PDC*(max(0,x[vx,]$DD/(60*60)))
    x[vx,]$PDelayed=x[vx,]$PDelayed+(x[vx,]$PDO+x[vx,]$PDC)
  }
  vx=agrepl("LD",x$Code,0)  | agrepl("LA",x$Code,0)
  if (sum(vx)>=1){
    x[vx,]$PDelay=x[vx,]$PDelay+x[vx,]$PDC*(x[vx,]$Delay/(60*60))
    x[vx,]$PDelayed=x[vx,]$PDelayed+x[vx,]$PDC
  }
  vx=agrepl("AD",x$Code,0)
  if (sum(vx)>=1){
    vx1=vx & x$LRD>0
    vx2=vx & x$DD>0
    x[vx1,]$PDelayed=x[vx1,]$PDelayed+x[vx1,]$PDO
    x[vx2,]$PDelayed=x[vx2,]$PDelayed+x[vx2,]$PDC
    x[vx1,]$PDelay=x[vx1,]$PDelay+x[vx1,]$PDO*((x[vx1,]$LRD)/(60*60))
    x[vx2,]$PDelay=x[vx2,]$PDelay+x[vx2,]$PDC*((x[vx2,]$DD)/(60*60))
  }
  vx=agrepl("ED",x$Code,0)
  if (sum(vx)>=1){
    x[vx,]$PDelay=x[vx,]$PDelay+0
    x[vx,]$PDelayed=x[vx,]$PDelayed+0
  }
  vx=agrepl("ND",x$Code,0)
  if (sum(vx)>=1){
    inds=x[vx,]$ind
    ind=inds[1]
    for (ind in inds){
      v=x$ind %in% c(ind)
      # if (x[v,]$Rt%%2==0){
      #   rr=x[v,]$Rt-1
      # } else {
      #   rr=x[v,]$Rt+1
      # }
      # xt=x[x$Rt==rr & x$DC1>(x[v,]$DC1+60*5) & x$LLoc==x[(which(v)+1),]$Loc & x$Loc==x[(which(v)+1),]$LLoc,]
      # xt=xt[order(xt$DC1),]
      # xt=xt[1,]
      # off[off$Location==x[v,]$Loc & off$RK==x[v,]$RK,]
      val=off[off$RK==x[v,]$RK & off$Location==x[v,]$Loc,]$TransfersOff
      # if (val>=1){
      #   sysND
      # }
      x[v,]$PDelayed=x[v,]$PDelayed+val
      # tval=max(x[x$ind %in% c(xt$ind,xt$ind-1),]$DC1)
      # TimeBetweenRoutes=tval-as.numeric(x[v,]$DC1)
      # TimeBetweenRoutes
      # if (is.na(TimeBetweenRoutes)){
      #   TimeBetweenRoutes=30*60
      # }
      TimeBetweenRoutes=15*60
      #estimate
      x[v,]$PDelay=x[v,]$PDelay+(val*(TimeBetweenRoutes))/(60*60)
    }
    }
  vx=agrepl("CL",x$Code,0)
  if (sum(vx)>=1){
    inds=x[vx,]$ind
    for (ind in inds){
      v=x$ind==ind
      val=off[off$RK==x[v,]$RK & off$Location==x[v,]$Loc,]$TransfersOn
      x[v,]$PDelayed=x[v,]$PDelayed+val
      x[v,]$PDelay=x[v,]$PDelay+0
    }
  }
  vx=agrepl("OL",x$Code,0)
  if (sum(vx)>=1){
    inds=x[vx,]$ind
    for (ind in inds){
      v=x$ind==ind
      val=off[off$RK==x[v,]$RK & off$Location==x[v,]$Loc,]$TransfersOff
      x[v,]$PDelayed=x[v,]$PDelayed+val
      ro=x[v,]
      ro2=x[x$Rt==ro$Rt & x$DC1>ro$DC1 & x$Loc==ro$Loc,][1,]
      TimeBetweenRoutes=ro2$DC1-ro$DC1
      if (is.na(TimeBetweenRoutes)){
        TimeBetweenRoutes=30*60
      }
      x[v,]$PDelay=x[v,]$PDelay+(val*(TimeBetweenRoutes))/(60*60)
    }
    # v=off[paste(off$RK,off$Location)%in% paste(x[vx,]$RK,x[vx,]$Loc),]$TransfersOff
    # v
  }
  vx=agrepl("HL",x$Code,0)
  if (sum(vx)>=1){
    x[vx,]$PDelayed=x[vx,]$PDelayed+0
    x[vx,]$PDelay=x[vx,]$PDelay+0
  }
  
  # vx=agrepl("LA",x$Code,0) | agrepl("AD",x$Code,0) 
  # if (sum(vx)>=1){
  #   x[vx,]$PDelay=x[vx,]$PDO*(x[vx,]$Delay/(60*60))
  #   x[vx,]$PDelayed=x[vx,]$PDO
  # }
  
  return(x)
}
Correct_WrongData=function(x){
  vx=x$LRD>10000 | x$Delay<(-10000)
  if (sum(vx)>=1){
    x[vx,]$Del=0
  }
  return(x)
}


Calculate_LA=function(x){
  # Calculate LAs, LD's due to previous delays and EOL Late
  load("phase04_2.RData")
  rks=SelectRKs(x)
  for (rk in rks){
    te=sr(x,rk)
    if ("LD" %in% StripCodes(te$Code)){
      rk2=na.omit(unique(te$PRK))
      if (length(rk2)==1 & rk2!=""){
        te2=sr(x,rk2)
        dc=StripCodes(te2$Code,T)
        dc=setdiff(dc,"ND")
        #11/21/22 removed ND as possible delays
        if (length(dc)>=1 & (Logic_Eval_OR(as.numeric(last(te2$EOL))>=DelayThreshold,"OL" %in% te2$Code) | "CL" %in% te2$Code | "HL" %in% te2$Code)){
          #11/21/22: do not reclassify as LA train catches up after non-ND delay
          te2$Lateness=as.numeric(te2$DC1)-as.numeric(te2$SDC1)
          vv=which(te2$Code!="" & te2$Code!="ND")
          change=F
          for (v in vv){
            l=na.omit(te2[v:nrow(te2),]$Lateness>=LatenessThreshold)
            if (all(l)){
              change=T
            }
          }
          if (change){
            # print(te)
            # print(te2)
            # print(rk)
            # print("--------------")
            te$Code=gsub("LD","LA",te$Code)
            x=Update(x,te)
          }          
        }    
      }
    }
  }
  return(x)
}
Modify_All_ND=function(x){
  # Function, modifies "all" ND runs to Hole (pfm/ics error?, should be very rare)
  # unsure how to handle, not a hole but does not carry passengers?
  # 11/17/22
  # Classified as Hole 11/18/22
  # leave as is, zero out dwell delay
  # load("PhaseAB.RData")
  # rk=585
  rk=12
  rks=SelectRKs(x)
  del=c()
  print("Checking for All_NDs")
  for (rk in rks){
    t=x[x$RK==rk,]
    hl=t[1,]$Note=="HL" # All_ND
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
        t[1,]$Note="HL" # All_ND
        t[1,]$Del=1
        # t$LRD=0
        t$DD=0
        print(t)
        x=rbindlist(list(x,t))
      }
    }
  }
  return(x)
}

Fix_CL=function(x){
  # function: fixes hopefully rare scenarios of CL and OL's
  #deletes 
  rks=SelectRKs(x)
  # verb=T
  for (rk in rks){
    te=sr(x,rk)
    l1=te[1,]$Loc==te[1,]$Or
    l2=te[nrow(te),]$Loc==te[nrow(te),]$De
    hl=!("HL" %in% te$Note)
    ALL_ND=te[1,]$Note=="All_ND"
    if (!l2 & hl & !ALL_ND){
      if (verb){
        print("overriding cancellation:")
        print(te)
      }
      te$De=te[nrow(te),]$Loc
      te$Note[1:nrow(te)]="OF"
      te[nrow(te),]$Note="OL"
      x=Update(x,te)
    }
    if (!l1 & hl & !ALL_ND){
      if (verb){
        print("overriding offload:")
        print(te)
      }
      te$Or=te[1,]$Loc
      te$Note[1:nrow(te)]=paste(te$Note[1:nrow(te)],"CN")
      te[1,]$Note="CL"
      x=Update(x,te)
    }
    if (!l1 & !l2 & hl){
      # print("error, runkey CL and OL")
      # print(te)
      x=x[x$RK!=te$RK[1]]
    }
  }
  return(x)
}


SortID=function(del){
  # del=Normalize_Groups(del)
  # del=SortID(del)
  # load("Phase08b.RData")
  v1=nrow(del)
  # re-sorts del to favor same runkeys but different delays then time
  cn=colnames(del)
  rks=SelectRKs(del)
  
  q=reordordt(del,c("RK","Code"))
  q=merge(q,q[,length(Code),RK])
  q=q[order(q$RK),]
  colnames(q)[ncol(q)]="Count"
  s=DelayTypesSortOrder
  colnames(s)[2]="Code"
  del2=merge(del,s,by="Code")
  if (nrow(del2)!=nrow(del)){
    sys_new_code
  }
  del=del2
  del2=del[-seq(1,nrow(q))]
  for (rk in rks){
    te=sr(del,rk)
    if (nrow(te)>1){
      te=te[order(te$SortOrder,te$Time1),]
    }
    del2=rbindlist(list(del2,te))
    if (sum(is.na(del2$Code))>=1){
      sys
    }
  }
  del2$Id=seq(1,nrow(del2))
  te
  del
  del2$TimeG=0
  # del2[del2$RK==669,]
  tz=del2[,min(Time2),Group]
  for (i in seq(1,nrow(tz))){
    del2[del2$Group==tz[i,]$Group,]$TimeG=tz[i,]$V1
  }
  del2[is.na(del2$Group),]$TimeG=del2[is.na(del2$Group),]$Time2
  # del2=del2[del2$Group==13,]
  # del2[is.na(del$Group),]$Group=0
  del2=del2[order(del2$TimeG,del2$Group,del2$SortOrder,-del2$Time2),]
  del2$SortId=seq(1,nrow(del2))
  del2$Id=seq(1,nrow(del2))
  del2=reordordt(del2,cn)
  # del2=del2[,!"ind"]
  if (nrow(del2)!=v1){
    sys_invalid_rows_del2
  }
  return(del2)
}
Normalize_Groups=function(del){
  z=unique(as.data.table(na.omit(sort(del$Group))))
  colnames(z)="Group"
  z=merge(z,del[,min(Time1),Group])
  z=z[order(z[[2]]),]
  z$G=seq(1,nrow(z))
  colnames(z)[1]="Group"
  cns=colnames(del)
  v1=nrow(del)
  del=merge(del,z,all.x=T)
  del$Group=del$G
  del=reordordt(del,cns)
  if (nrow(del)!=v1){
    sys
  }
  return(del)
}
HL_Correction=function(del){
  # Function: Override data to be blank for HL (if trains were previously all ND's)
  vx=which(del$DelayCode=="HL")
  if (sum(vx)>=1){
    del[vx,]$Origin=""
    del[vx,]$Dest=""
    del[vx,]$Train=NA
    del[vx,]$DelaySec=NA
  }
  return(del)
}

OL_ND=function(x){
  # function: most ND to OL Offload 
  # rewrite multiple ND to OL
  # Inservice...In service...ND..ND..ND..ND..Arrival at Dest==>IS IS OL..""..""
  # EXMAPLE: rk=85 507 04JUL2022
  rks=SelectRKs(x)
  for (rk in rks){
    t=x[x$RK==rk,]
    hl=sum(agrepl("HL",t$Code) | agrepl("HL",t$Note))>=1
    if (nrow(t)>=1 & !hl){
      if (sum(agrepl("ND",t$Code) | agrepl("ND",t$Note))>=1){
        if (nrow(t)>=5){
          vx=which(agrepl("ND",t$Code) | agrepl("ND",t$Note))
          if (length(vx)>=1){
            if (sum(vx+1==nrow(t))>=1 & all(diff(vx)==1) & t[max(vx),]$PDC==0){
              print("changing many ND to OL")
              print(t)
              # t[vx,]$Note=""
              # t[vx,]$Code=""
              t[vx,]$Del=0
              t[min(vx)-1,]$Note="OL"
              t[min(vx)-1,]$Code="OL"
              t[min(vx)-1,]$Del=1
              print(t)
              x=Update(x,t)
            }
          }
        }
      }
    }
  }
  return(x)
}

Normalize=function(x,vars,sheet,dir=T){
  # function to automatically convert code to Id and vice versa
  # T to go from abbr to Id
  # F to go from Id to Abbr
  # x=del;sheet="LocationList"
  #
  # x0=x
  # x=x0
  # vars="Location"; sheet= "LocationList";dir=T
            # print(x)
  cns=colnames(x)
  n1=nrow(x)
  s=eval(eval(as.symbol(sheet)))
  s=adt(s)
  s=reordordt(s,cc("Id Abbr"))
  if (dir){
    colnames(s)[1]="F"
  } else {
    colnames(s)[2]="F"
  }
  var=vars[1]
  for (var in vars){
    if (dir){
      colnames(s)[2]=var
    } else {
      colnames(s)[1]=var
    }
    d2=merge(x,s,all.x = T, by=var)
    d2[[var]]=d2$F
    d2=d2[,!"F"]
    d2=reordordt(d2,colnames(x))
    x=d2
    if (nrow(x)==0){
      syserror_0_rows
    }
  }
  if (nrow(x)!=n1){
    sysRowError
  }
  if ("Id" %in% colnames(x)){
    x=x[order(x$Id),]
  } else if ("ind" %in% colnames(x)) {
    x=x[order(x$ind),]
  }
  return(x)
}
GetPreviousTrain=function(del){
  del$PTr=0
  for (i in seq(1,nrow(del))){
    v=del[i,]$PRK
    if (!is.na(v)){
      del[i,]$PTr=x[x$RK==v]$Tr[1]
    }
  }
  del=reordordt(del,cc("Id RevDate RK PRK PTr STrain Tr Rt SOr SDe Or De Time1 Time2 Loc1 Loc2 Delay Code EOL PDelay PDelayed"))
  return(del)
}
ExpandDI=function(di){
  # x[x$RK==90,]
  # di[di$RunKey==90,]
  # 
  # di[di$RunKey==585,]
  # del[del$RunKey==585.]
  # load("Phase09a.RData")
  di$Scheduled=as.numeric(di$Rt %in% rev_routes)
  di$Delayed=as.numeric(di$Rt %in% rev_routes & di$DelayCodes!="")
  di$Late=as.numeric(di$Rt %in% rev_routes & (di$EOL>=LatenessThreshold))
  for (i in seq(1,nrow(di))){
    ro=di[i,]
    if (is.na(ro$Late) & ro$Rt %in% rev_routes){
      v=intersect(strsplit(ro$DelayCodes,"\\ ")[[1]],auto_late)
      if (length(v)>=1){
        di[i,]$Late=1
      }
    }
  }
  # di=di[464,]
  sum(di$Late)
  for (i in seq(1,nrow(di))){
    ro=di[i,]
    cds=ro$DelayCodes
    if (ro$Rt %in% rev_routes){
      v=max(unlist(lapply(auto_late,function(x)agrepl(x,cds,0))))
      if (v==1){
        di[i,]$Late=1
      }
    }
  }
  # di$Late=as.numeric((di$Late | max(unlist(lapply(auto_late,function(x)agrepl(x,di$DelayCodes,0))))) & di$Rt %in% rev_routes)
  di
  # sum(di$Late)
  
  all(di[di$Scheduled==0,]$Delayed==0)
  all(di[di$Scheduled==0,]$Late==0)
  
  di[di$DelayCodes=="HL" & di$Route %in% rev_routes,]
  
  di[di$Route %in% seq(13,14),]$Delayed
  di[di$Route %in% seq(13,14),]$Late
  
  # di$Recoveries=0
  # ii=which(di$Delays>=1)
  # for (i in ii){
  #   te=x[x$RK==di[i,]$RK,]
  #   te[!(agrepl("HL",te$Code) & !(agrepl("HL",te$Code) & te$Code!="",]
  #   
  # }
  print(nrow(di[is.na(di$Late),])==0)
  
  # print(paste(sum(di$Scheduled),"Scheduled Dispatches (Exclude SFO Shuttles)"))
  print(paste(sum(di$Scheduled),"Scheduled Dispatches (Exclude SFO Shuttles)"))
  
  print(paste(sum(di$Delayed)," trains delayed"))
  print(paste(sum(di$Delayed & di$Late)," trains delayed and late"))
  print(paste(sum(di$Delayed & !di$Late)," trains delayed and on time late"))
  
  print(paste(sum(di$Late)," trains late"))
  print(paste(sum(di$Delayed & di$Late)," trains late and delayed"))
  print(paste(sum(!di$Delayed & di$Late)," trains late and not delayed"))
  
  return(di)
}
#430 1315NOV2022
CleanDel=function(del){
  colnames(del)[colnames(del)=="STrain"]="SchedTrain"
  colnames(del)[colnames(del)=="Tr"]="ActTrain"
  colnames(del)[colnames(del)=="Train"]="ActTrain"
  colnames(del)[colnames(del)=="PTr"]="PTrain"
  colnames(del)[colnames(del)=="SOr"]="SchedOrigin"
  colnames(del)[colnames(del)=="SDe"]="SchedDest"
  colnames(del)[colnames(del)=="Or"]="ActOrigin"
  colnames(del)[colnames(del)=="De"]="ActDest"
  colnames(del)[colnames(del)=="Time1"]="StartTime"
  colnames(del)[colnames(del)=="Time2"]="EndTime"
  colnames(del)[colnames(del)=="Loc1"]="StartLoc"
  colnames(del)[colnames(del)=="Loc2"]="EndLoc"
  colnames(del)[colnames(del)=="Code"]="DelayCode"
  colnames(del)[colnames(del)=="Delay"]="DelaySec"
  colnames(del)[colnames(del)=="Group"]="DelayGroup"
  del[del==0]=NA
  del=ChangeColumnNames(del,"RK","RunKey")
  del=ChangeColumnNames(del,"PRK","PrevRunKey")
  del=ChangeColumnNames(del,"PTrain","PrevTrain")
  del=ChangeColumnNames(del,"Rt","Route")
  del=ChangeColumnNames(del,"SOrigin","SchedOrigin")
  del=ChangeColumnNames(del,"SDest","SchedDest")
  del=ChangeColumnNames(del,"EOL","EOLDelay")
  del=ChangeColumnNames(del,"PDelay","PassDelay")
  del=ChangeColumnNames(del,"PDelayed","NumPassDelayed")
  del=del[,!"ind"]
  return(del)
}
CleanDetailed=function(x){
  colnames(x)[colnames(x)=="STrain"]="SchedTrain"
  colnames(x)[colnames(x)=="Tr"]="ActTrain"
  colnames(x)[colnames(x)=="Train"]="ActTrain"
  colnames(x)[colnames(x)=="TS"]="Timestamp"
  colnames(x)[colnames(x)=="PTr"]="PTrain"
  colnames(x)[colnames(x)=="SOr"]="SchedOrigin"
  colnames(x)[colnames(x)=="SDe"]="SchedDest"
  colnames(x)[colnames(x)=="Or"]="ActOrigin"
  colnames(x)[colnames(x)=="De"]="ActDest"
  colnames(x)[colnames(x)=="Time1"]="StartTime"
  colnames(x)[colnames(x)=="Time2"]="EndTime"
  colnames(x)[colnames(x)=="Loc1"]="StartLoc"
  colnames(x)[colnames(x)=="Loc2"]="EndLoc"
  colnames(x)[colnames(x)=="Code"]="DelayCode"
  colnames(x)[colnames(x)=="Delay"]="DelaySec"
  
  colnames(x)[colnames(x)=="DD"]="DwellLateness"
  colnames(x)[colnames(x)=="ScheduleNote"]="Note"
  colnames(x)[colnames(x)=="PL"]="PreviousLateness"
  colnames(x)[colnames(x)=="CL"]="CurrentLateness"
  colnames(x)[colnames(x)=="Rt"]="Route"
  colnames(x)[colnames(x)=="TN"]="TrackNumber"
  colnames(x)[colnames(x)=="RR"]="ReverseRun"
  colnames(x)[colnames(x)=="ART"]="ActualRunTime"
  colnames(x)[colnames(x)=="SRT"]="ScheduledRunTime"
  colnames(x)[colnames(x)=="RUNKEY"]="RK"
  colnames(x)[colnames(x)=="PDO"]="TotalPatronsDO"
  colnames(x)[colnames(x)=="PDC"]="TotalPatronsDC"
  colnames(x)[colnames(x)=="LRD"]="LinkRunDelay"
  colnames(x)[colnames(x)=="Del"]="Delayed"
  colnames(x)[colnames(x)=="PLe"]="PerformanceLevel"
  
  colnames(x)[colnames(x)=="Dir"]="Direction"
  x=ChangeColumnNames(x,"RK","RunKey")
  colnames(x)[colnames(x)=="EOL"]="EOLDelay"
  # x=ChangeColumnNames(x,"EOL","EOLDelay")
  
  x=ChangeColumnNames(x,"PDelayed","PassDelay")
  
  x=ChangeColumnNames(x,"PDelayed","NumPassDelayed")
  x=ChangeColumnNames(x,"SOrigin","SchedOrigin")
  x=ChangeColumnNames(x,"SDest","SchedDest")
  # x[x==0]=NA
  #removed 11/18/22 causing EOL =0 to be NA (error)
  #mayn eed other 0's to NA
  return(x)
}


GroupDelays=function(del,x){
  # load("Phase06.RData")
  #basic grouping by location (overlapping) and time (overlapping and +- 5 minutes)
  del$Group=0
  del$ind=seq(1,nrow(del))
  gr=1
  del$locs=""
  for (i in seq(1,nrow(del))){
    te=del[i,]
    locs=c(te$Loc1,te$Loc2)
    v=which(x[x$RK %in% te$RK,]$Loc %in% locs)
    v=min(v):max(v)
    del[i,]$locs=paste0(x[x$RK %in% te$RK,]$Loc[v],collapse=" ")
  }
  for (i in seq(1,nrow(del))){
    te=del[i,]
    # t2=x[x$RK==te$RK,]
    # t2[which(t2$Loc %in% te$Loc1):which(t2$Loc %in% te$Loc2)]
    # te
    # del[del$Loc1 %in% ]
    
    # pt=del[(del$Loc1 %in% te$Loc1 | del$Loc2 %in% te$Loc2) & (abs(del$Time1-te$Time1)<5*60 | abs(del$Time2-te$Time2)<5*60 | abs(del$Time1-te$Time2)<5*60 | abs(del$Time2-te$Time1)<5*60),]
    locv=agrepl(te$Loc1,del$locs,0) | agrepl(te$Loc2,del$locs,0)
    del$OL=0
    for (q in seq(1,nrow(del))){
      del[i,]$OL=overlap(del[i,]$Time1,del[i,]$Time2,te$Time1,te$Time2)
    }
    
    del$Time1;del$Time2
    pt=del[locv & (del$OL==1 | abs(del$Time1-te$Time1)<5*60 | abs(del$Time2-te$Time2)<5*60 | abs(del$Time1-te$Time2)<5*60 | abs(del$Time2-te$Time1)<5*60),]
    if (all(pt$Group==0)){
      pt$Group=gr
      gr=gr+1
    } else if (length(table(pt$Group))==1){
      #do nothing
    } else if (length(table(pt$Group))==2 & 0 %in% pt$Group){
      # sys1
      co=unique(pt$Group);co=co[co!=0]
      pt$Group=co
    } else {
      pt$Group=max(pt$Group)
    }
    del=Update(del,pt)
    pt;del
  }
  vt=table(del$Group)
  vz=names(vt[vt==1])
  if (length(vz>=1)){
    del[del$Group %in% vz]$Group=NA
  }
  del=del[,!"ind"]
  del=del[,!"locs"]
  del=del[,!"OL"]
  return(del)
}
HL_ND_OL=function(x){
  # function to override all delays at stations were Hole is present 
  rks=SelectRKs(x)
  rk=12
  for (rk in rks){
    t=x[x$RK==rk,]
    hl=sum(agrepl("HL",t$Note))>=1
    v1=(agrepl("OL",t$Note,0)) | (agrepl("ND",t$Note,0)) | (agrepl("CL",t$Note,0))
    v1[1]=hl
    if (all(v1)){
      t[t$Note!="HL",]$Del=0
      # if (t[1,]$Note=="HL AllL_ND"){
      #   t[1,]$Del=1
      # }
      x=Update(x,t)
    }
  }
  return(x)
}
GroupDelays2=function(del,x){
  # load("Phase07.RData")
  rks=SelectRKs(del)
  table(del$Group)
  for (rk in rks){
    t=del[del$RK==rk,]
    if (nrow(t)>1){
      gr=max(na.omit(del$Group))+1
      if (is.na(gr)){gr=1}
      for (i in seq(1,nrow(t)-1)){
        change=F
        next_loc=t[i,]$Loc2==t[i+1,]$Loc1
        locs=c(t[i,]$Loc2)
        locs=c(locs,t[i+1,]$Loc1)
        locs=unique(locs)
        tx=x[x$RK==rk & x$Loc %in% locs,]
        touching=1 %in% diff(sort(tx$ind))
        if (touching){
          if (!is.na(t[i,]$Group) | !is.na(t[i+1,]$Group)){
            # print(rk)
            t[t$Group %in%  t[i,]$Group | t$Group %in% t[i+1,]$Group,]$Group=gr
            # t[i,]$Group=gr;t[i+1,]$Group=gr
            change=T
            if (change){
              del=Update(del,t)
            }
            gr=unique(na.omit(c(t[i,]$Group,t[i+1,]$Group)))
          }
          if (!is.na(t[i,]$Group) & !is.na(t[i+1,]$Group) & t[i,]$Group!=t[i+1,]$Group){
            # do nothing
            sysG #unexpected error, two adjacent delays on a RK but two group ids
          }
          if (length(gr)>=2){
            sys
          }
        }
        if (verb){print(t)}
      }
    }
  }
  return(del)
}

Group_Delays_LA=function(x){
  # load("Phase06a.RData")
  del$ind=seq(1,nrow(del))
  vx=which(del$Code=="LA")
  del[del$Group==28,]
  del[del$Group==40,]
  # i=vx[1]
  if (length(vx)>=1){
    for (i in vx){
      # print(i)
      t1=del[i,]
      # t2=del[del$RK==t1$RK & del$ind!=t1$ind,]
      t2=del[del$RK==t1$PRK]
      if (nrow(t2)==0){
        sysLaNrow
      }
      change=F
      t2=t2[nrow(t2),]
      if (is.na(t1$Group) & !is.na(t2$Group)){
        t1$Group=t2$Group
        change=T
      } else if (!is.na(t1$Group) & is.na(t2$Group)){
        t2$Group=t1$Group
        change=T
      } else if (is.na(t1$Group) & is.na(t2$Group)){
        gr=max(na.omit(del$Group)+1)
        t1$Group=gr;t2$Group=gr
        change=T
      } else if (!is.na(t1$Group) & !is.na(t2$Group)){
        #pick lowest one
        v=min(t2$Group,t1$Group)
        del[del$Group %in% c(t1$Group,t2$Group),]$Group=v
        gr=max(na.omit(del$Group))+1
      } else {
        srtkpwtlq
        #should never happen 4/4 scenarios covered
      }
      if (change){
        del=Update(del,t1)
        del=Update(del,t2)
      }
    }
  }
  
  return(del)
}

CalculateDispatches=function(x){
  rm(list=ls())
  # load("Phase09.RData")

  length(unique(sort(c(unique(x$RK),unique(us$RK)))))
  length(unique(carlist$RK))
  setdiff(unique(sort(c(unique(x$RK),unique(us$RK)))),unique(carlist$RK))
  setdiff(unique(carlist$RK),unique(sort(c(unique(x$RK),unique(us$RK)))))
  carlist[carlist$RK==15,]
  
  
  x=rbindlist(list(x,us),fill=T)
  x=unique(x)
  di=x[x$Loc==x$SOr | (x$RK %in% unique(us$RK) & x$Or==x$Loc) | x$Note=="HL",]
  #agrepl hl to cover hl all_nd too agrepl("HL",x$Note,0)
  di=unique(di)
  rks=x[!x$RK %in% di$RK & x$Or==x$Loc,]
  # offload and cancelled runkeys
  di=rbindlist(list(di,x[!x$RK %in% di$RK & x$Or==x$Loc,]))
  
  # di=rbindlist(list(di,x[x$Note=="CL" | x$Note=="OL",]))
  # di=reordordt(di,cc("ind RevDate RK Rt SDC STrain SOr SDe EOL SDC1"))
  di=reordordt(di,cc("ind RevDate RK Rt SDC STrain Or De SOr SDe EOL SDC1"))
  di=unique(di)
  colnames(di)[colnames(di)=="SDC"]="SDispatch"
  di=di[order(di$Rt,di$SDC1),]
  di=di[,!"SDC1"]
  di$TotalDelay=0
  di$DelayCount=0
  di$DelayCodes=""
  di$PDelayed=0
  di$PDelay=0
  di$Locs=""
  # rare fix to delete duplicated RK in DI
  di=di[!duplicated(di$RK),]
  for (i in seq(1,nrow(di))){
    te=x[x$RK==di[i,]$RK,]
    v1= sum(na.omit(te$ADS))
    v2=sum(te[te$Del==1 & te$Code!="AD",]$Delay)
    if (length(v1)==0){
      v1=0
    }
    if (length(v2)==0){
      v2=0
    }
    cds=te[te$Del==1,]$Code
    cds=cds[cds!=""]
    lo=te$DL
    #changed 11/18/22 to include all AD locations
    lo=te[te$Code!="",]$Loc
    lo=lo[lo!=""]
    lo=paste(lo,collapse=" ")
    di[i,]$TotalDelay=v1+v2
    di[i,]$DelayCount=length(cds)
    di[i,]$DelayCodes=paste0(cds,collapse=" ")
    di[i,]$PDelayed=sum(te$PDelayed)
    di[i,]$PDelay=sum(te$PDelay)
    di[i,]$Locs=lo
  }
  di$ind=seq(1,nrow(di))
  colnames(di)[colnames(di)=="ind"]="Id"
  vx=di$Rt>=100
  if (sum(vx)>=1){
    di[vx,]$Locs=NA
  }
  return(di)
}
MergePot=function(){
  # pot=fread("POT.csv")
  by=c("RK","RevDate")
  pot=pot[,!"Id"]
  v1=nrow(di)
  di=merge(di,pot,by,all=T)
  di=di[,!"RUNKEY"]
  sum(di$Exits)
  return(di)
}
Add_Del_Locations=function(x){
  vx=x$Del==1
  x$DL=""
  x[vx,]$DL=x[vx,]$Loc
  return(x)
}
CollapseDelays=function(x){
  # Function: Collapses Delay
  # load("Phase05.RData")
  rks=SelectRKs(x)
  cns=cc("Id RevDate RK PRK STrain Tr Rt SOr SDe Or De Time1 Time2 Loc1 Loc2 Delay Code EOL PDelay PDelayed")
  del=matrix(ncol=length(cns),nrow=0)
  del=as.data.table(del)
  colnames(del)=cns
  for (rk in rks){
    te=x[x$RK==rk,]
    if (1 %in% te$Del){
      tx=te[te$Del==1,]
      for (i in seq(1,nrow(tx))){
        y=te[te$ind==tx[i,]$ind,]
        pdlay=y$PDelay
        pdlayed=y$PDelayed
        dv=y$Delay
        ad=agrepl("AD",tx[i,]$Code,0)
        if (ad){
          # if (sum(te$AD>=2) | any(diff(which(te$Code=="AD"))>1)){
          #   sysADunexpected
          # }
          pdlay=sum(te[te$AD==y$AD,]$PDelay)
          pdlayed=sum(te[te$AD==y$AD,]$PDelayed)
          dv=y$ADS
          y[y$AD==1,]
        }
        hl=tx[i,]$Code=="HL"
        or=y$Or;de=y$De
        dc=y$DC1
        if (hl){
          dc=y$SDC1
          or=y$SOr;de=y$SDe
        }
        c1=c(0,y$RevDate,y$RK,y$PRK,y$STrain)
        c2=c(y$Tr,y$Rt,y$SOr,y$SDe,y$Or)
        c3=c(y$De, dc,dc,y$Loc,y$Loc)
        c4=c(dv,y$Code,y$EOL,pdlay,pdlayed)
        yt=as.data.table(t(as.data.table(c(c1, c2,c3,c4))))
        del=rbindlist(list(del,yt),use.names=F)
      }
    }
  }  
  del=ann(del)
  del[del$RK==768,]
  table(del$Code)
  for (i in seq(1,nrow(del))){
    te=del[i,]
    ad=agrepl("AD",te$Code,0)
    if (ad){
      tx=x[x$RK==te$RK & x$Loc==te$Loc1,]
      tx=x[x$AD==tx$AD & x$RK==tx$RK,]
      del[i,]$Loc1=tx[1,]$Loc
      del[i,]$Code=paste0(StripCodes(tx$Code),collapse=" ")
      del[i,]$Time1=tx[1,]$DC1
    }
  }
  for (i in seq(1,nrow(del))){
    te=del[i,]
    lr="LR"==te$Code
    if (lr){
      tx=x[x$RK==te$RK,]
      tx=tx[which(tx$Loc==te$Loc2)-1,]
      del[i,]$Loc1=tx[1,]$Loc
      del[i,]$Time1=tx[1,]$DC1
    }
  }
  del$Id=seq(1,nrow(del))
  del$SortId=seq(1,nrow(del))
  mode(del$Time1)='numeric'
  mode(del$Time2)='numeric'
  return(del)
}
Group_Adjacent_Stations_RK=function(del){
  # group adject delays for 1 runkey
  load("Phase08.RData")
  rks=SelectRKs(del)
  del
  print("Grouping Adjacent Stations per RK")
  for (i in seq(1,nrow(del))){
    g=max(na.omit(del$Group))+1
    n=del[i,]
    nn=del[del$RK==n$RK,]
    if (nrow(nn)>1){
      nn=nn[(1:2),]
      t=x[x$RK==n$RK,]
      v1=max(abs(which(nn$Loc2[2]==t$Loc)-which(nn$Loc2[1]==t$Loc)),0)
      v2=max(abs(which(nn$Loc[2]==t$Loc)-which(nn$Loc[1]==t$Loc)),0)
      v3=max(abs(which(nn$Loc2[1]==t$Loc)-which(nn$Loc2[2]==t$Loc)),0)
      v4=max(abs(which(nn$Loc[1]==t$Loc)-which(nn$Loc[2]==t$Loc)),0)
      v5=max(abs(which(nn$Loc2[1]==t$Loc)-which(nn$Loc2[1]==t$Loc)),0)
      v6=max(abs(which(nn$Loc[1]==t$Loc)-which(nn$Loc[1]==t$Loc)),0)
      v7=max(abs(which(nn$Loc2[2]==t$Loc)-which(nn$Loc2[2]==t$Loc)),0)
      v8=max(abs(which(nn$Loc[2]==t$Loc)-which(nn$Loc[2]==t$Loc)),0)
      if (v1==1 | v2==1 | v3==1 | v4==1 | v5==1 | v6==1 | v7==1 | v8==1){
        if (all(is.na(nn$Group))){
          nn$Group=g
          del=Update(del,nn)
          print(nn)
        } else if (length(unique(nn$Group))==1 & all(!is.na(nn$Group))) {
          #do nothing
        } else if (sum(is.na(unique(nn$Group)))==1 & sum(!is.na(unique(nn$Group)))==1) {
          nn$Group=na.omit(nn$Group)
          del=Update(del,nn)
          print(nn)
        } else {
          sysGroup_RK_Error
        }
      }
    }
  }
  return(del)
}
Extend_Scheduled_Metrics=function(x){
  # load("Phase00a.RData")
  print(head(x))
  # x=Correct_WrongData(x)
  # x=Extend_Scheduled_Metrics(x)
  rks=SelectRKs(x)
  rk=19
  for (rk in rks){
    t=x[x$RK==rk,]
    if (all(t$Rt %in% rev_routes)){
      if (sum((t$SOr=="")>=1)){
        t$STrain=na.omit(t$STrain)[1]
        t$SOr=blank.omit(t$SOr)[1]
        t$SDe=blank.omit(t$SDe)[1]
        t$SRK=names(table(t$SRK)[rev(order(table(t$SRK)))])[1]
      }
    }
  }
  Update(x,t)
  # 19 22969-36162-413-7
  # carlist[carlist$RK==19,]
  # 408/10: 0:59-413 11/20/22
}
# rk=19
# CalculateDelayedTransfers=function(){
#   load("Phase01a.RData")
#   nds=x[x$Note=="ND",]
#   i=1
#   for (i in seq(1,nrow(nds))){
#     rk=nds[i,]$RK
#     x[x$RK==nds[i,]$RK,]
#     ind=nds[i,]$ind
#     vx=x$ind %in% c(ind,ind+1)
#     x[vx,]  
#   
#     off[off$RK==rk & off$Location==x[vx,]$Loc[1],]
#     off[off$RK==rk & off$Location==x[vx,]$Loc[2],]
#     off[off$RK==rk,]
#     v=off[off$RK==rk & off$Location==x[vx,]$Loc[2],]$TransfersOff
#     if (v>=1){
#       sys1
#     }
#   }
#   ol=x[x$Note=="OL",]
#   for (i in seq(1,nrow(ol))){
#     rk=ol[i,]$RK
#     x[x$RK==ol[i,]$RK,]
#     ind=ol[i,]$ind
#     vx=x$ind %in% c(ind,ind+1)
#     x[vx,]  
#     
#     off[off$RK==rk & off$Location==x[vx,]$Loc[1],]
#     # off[off$RK==rk & off$Location==x[vx,]$Loc[2],]
#     off[off$RK==rk,]
#     v=off[off$RK==rk & off$Location==x[vx,]$Loc[1],]$TransfersOff
#     if (v>=1){
#       sys1
#     }
#   }
#   
#   
#   
# 
# }

# UploadVehicles=function(){
#   setwd(Vehicle_Folder)
#   fs=list.files()
#   fs=setdiff(fs,list.files(pattern="~"))
#   fs=setdiff(fs,list.files(pattern=".bak"))
#   f=fs[1]
#   for (f in fs){
#     fn=strsplit(f,"\\ ")[[1]][3]
#     fn=strsplit(fn,"\\.")[[1]][1:3]
#     fn=paste0(fn,collapse=".")
#     da=as.Date(fn,"%M.%d.%Y")
#     l=CheckDates(da,"VehIncident")
#     if (!l){
#       x=fread(f)
#       x$Time=period_to_seconds(hms(unlist(lapply(x$Time,function(x) strsplit(as.character(x),"\\ ")[[1]][2]))))
#       insertDataToSQL_Bulk(x,da,"VehIncident",T) 
#     }
#   }
# }

save.image("Delay Functions.RData")




