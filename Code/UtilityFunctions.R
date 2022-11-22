#Utility Functions
#functions to help with various random tasks
#11/9/22 AL

rm(list=ls())
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("Directories.RData")

an=function(x){
  #short hand for as.numeric
  return(as.numeric(x))
}
cc=function(x,y=" "){
  strsplit(x,y)[[1]]
}
SelectRKs=function(x){
  return(unique(x$RK))
}
adt=function(x){
  x=as.data.table(x)
  return(x)
}
reordordt=function(dt,names){
  r=dt[[names[1]]]
  r=adt(r)
  for (i in names[2:length(names)]){
    r=cbind(r,dt[[i]])
  }
  colnames(r)=names
  return(r)
}
AttachZero=function(x){
  if (as.numeric(x)==0 & length(strsplit(x,"")[[1]])==2){
    return("00")
  }
  x=as.numeric(x)
  if (as.numeric(x)<=9){
    x=paste0("0",x)
  }
  return(x)
}
AttachZero_100=function(x){
  x=as.numeric(x)
  if (as.numeric(x)<=9){
    x=paste0("00",x)
    return(x)
  }
  if (as.numeric(x)<=99){
    x=paste0("0",x)
    return(x)
  }
  if (as.numeric(x)<=999){
    return(x)
  }
}
ConvertTime=function(x){
  if (x==0 | x==""){
    return("")
  }
  if (is.na(x)){
    return(NA)
  }
  # print(x)
  x=seconds_to_period(x)
  # print(x)
  x=strsplit(as.character(x),"\\ ")[[1]]
  # print(x)
  if (length(x)==4){
    h=as.numeric(strsplit(x[2],"")[[1]][1])+24
    h=na.omit(h)
    h=paste0(h,collapse="")
    m=as.numeric(strsplit(x[3],"")[[1]][1])
    m=na.omit(m)
    m=paste0(m,collapse="")
  } else {
    h=as.numeric(strsplit(x[1],"")[[1]])
    h=na.omit(h)
    h=paste0(h,collapse="")
    m=as.numeric(strsplit(x[2],"")[[1]])
    m=na.omit(m)
    m=paste0(m,collapse="")
  }
  h=AttachZero(h);m=AttachZero(m)
  z=paste0(h,":",m)
  return(z)
}
ConvertTimes_ToTSec=function(del){
  vs=c("DO","DC","SDO","SDC")
  for (v in vs){
    q=lapply(del[[v]],function(x) strsplit(x,"\\:")[[1]])
    h=unlist(lapply(q,function(x) as.numeric(x[1])))*60*60
    m=unlist(lapply(q,function(x) as.numeric(x[2])))*60
    s=unlist(lapply(q,function(x) as.numeric(x[3])))
    fn=paste0(v,"1")
    del[[fn]]=h+m+s
  }
  return(del)
}


ann=function(x){
  # "as numeric numeric"
  # converts data frame columns to numeric if possible
  # sw()
  if (is.matrix(x)){
    for (i in seq(1,ncol(x))){
      #z=all(an(unique(x[,i]))==unique(x[[i]]))
      z=all(an(unique(x[,i]))==ul(lapply(lapply(strsplit(un(x[,i]),""),function(x) x[x!="" & x!=" "]),function(x)paste(x,collapse=""))))
      if(!is.na(z) | sum(is.na(an(x[,i])))==0){
        if (z){
          mode(x[,i])='numeric'
        }
      }
    }
  } else {
    for (i in seq(1,ncol(x))){
      #        print(i)
      z=all(an(unique(x[[i]]))==ul(lapply(lapply(strsplit(ac(un(x[[i]])),""),function(x) x[x!="" & x!=" "]),function(x)paste(x,collapse=""))))
      #        z=z | all(an(ac(x[[i]]))==an(ac(x[[i]])))
      if (!is.na(z)){
        yz="0 " %in% x[[i]] | " 0" %in% x[[i]]
        if (sum(is.na(an(x[[i]])))==0 | z | yz){
          if (!(colnames(x)[i] %in% c("Ticket","action","VAR1","Ticket_SN","Smart_Card_SN"))){
            mode(x[[i]])='numeric'
          }
        } 
        
      }
    }
  }
  # sw()
  return(x)
}

StripSeconds=function(x){
  vars=cc("DC SDC DO SDO")
  CheckLength=function(x){
    if (length(x)==3){
      v=paste0(c(AttachZero(x[1]),AttachZero(x[2])),collapse=":")
    } else {
      v=""
    }
    return(v)
  }
  for (var in vars){
    if (var %in% colnames(x)){
      v1=lapply(x[[var]],function(x) strsplit(x,"\\:")[[1]])
      v2=unlist(lapply(v1,function(x) CheckLength(x)))
      x[[var]]=v2
    }
  }
  return(x)
}
PurgeData=function(date="",all=F){
  #purge dates
  #PurgeData("08NOV2022")
  if (all){
    setwd(data_individual_dir)
    file.remove(list.files())
    setwd(data_processed_dir)
    unlink(list.files(),force=T,recursive = T)
  } else {
    setwd(data_individual_dir)
    csv=paste0(date,".csv")
    file.remove(csv)
    setwd(data_processed_dir)
    file.remove(date,recursive =T)
    unlink(date,force=T,recursive = T)
  }
}
sr=function(x,rk){
  # select rk subset
  return(x[x$RK==rk,])
}
Update=function(x,t){
  # Function: updates x with t data table 
  # deletes records found in t in x
  # Error Checking: input rows = output row
  v1=nrow(x)
  t=reordordt(t,colnames(x))
  # if ('ind' %in% colnames(x)){
  x=rbindlist(list(t,x[!(x$ind %in% t$ind),]))    
  # } else if ('Id' %in% colnames(x)) {
  #   x=rbindlist(list(t,x[!(x$Id %in% t$Id),]))    
  # } else {
  #   sysErrorWrongColumn
  # }
  v2=nrow(x)
  if (v1!=v2){
    syserror
  }
  if ("ind" %in% colnames(x)){
    x=x[order(x$ind),]
  }
  if ("DC1" %in% colnames(x)){
    x=x[order(x$RK,x$DC1),]
  }
  
  return(x)
}
ul=function(x){
  unlist(x)
}
overlap=function(x1,x2,y1,y2){
  x=seq(an(x1),an(x2))
  y=seq(an(y1),an(y2))
  #assign('x',x,envir=.GlobalEnv)
  #assign('y',y,envir=.GlobalEnv)
  yy=c(x,y)
  if (sum(duplicated(yy))>=1){
    return (T)
  } else {
    return (F)
  }
}
AddCode=function(t,cd,var="Code"){
  # Function appends cd to x
  # input can be single or multi rows
  t[[var]]=paste(t[[var]]," ",cd)
  return(TrimCodes(t,var))
}
TrimCodes=function(x,var="Code"){
  x[[var]]=trimws(x[[var]],which="both")
  for (i in seq(1,5)){
    x[[var]]=gsub("\\  ","\\ ",x[[var]])
  }
  return(x)
}
ac=function(x){as.character(x)}
un=function(x){unique(x)}

ChangeColumnNames=function(x,c1,c2){
  colnames(x)[colnames(x)==c1]=c2
  return(x)
}
Logic_Eval_OR=function(x,y){
  if (sum(c(is.na(c(x,y))))==1){
    if (is.na(x)){
      return(y)
    }
    if (is.na(y)){
      return(x )
    }
  }
  if (sum(c(is.na(c(x,y))))==2){
    syslogerror
  }
  return(x | y)
}
Convert_GroupTimes=function(x,vars=cc("Min2T Max2T")){
  ii=seq(1,nrow(x))
  for (i in ii){
    for (var in vars){
      v2=x[[var]][i]
      if (v2 !=""){
        # sys
        x[[var]][i]=ConvertTime(v2)
        # print(ConvertTime(v2))
      }
    }
  }
  return(x)
}

SaveReload=function(fn,save=T,reload=T){
  # save/reload, function
  # file="Phase1.RData"
  if (save){
    save.image(fn)
  }
  if (reload){
    # v=setdiff(ls(),'fn')
    # # v1=(ls())
    # # # print(!(fn %in% v))
    # # print(length(v))
    # # print(length(v1))
    # rm(v)
    load(fn)
  }
}
SortDays=function(ds){
  ds=sort(as.Date(unlist(lapply(ds,function(x) strsplit(x,"\\.")[[1]][1])),"%d%b%Y"))
  return(ds)
}
OverrideNA=function(t,cn=""){
  if (cn==""){
    if (sum(is.na(t))>=1){
      t[is.na(t)]=""    
    }
  } else{
    if (sum(is.na(t[[cn]]))>=1){
      t[is.na(t[[cn]])][[cn]]=""    
    }
  }
  return(t)
}
AddLateness=function(x){
  x$Lateness=as.numeric(x$DC1)-as.numeric(x$SDC1)
  return(x)
}
setwd(data_rdata_dir)
save.image("UtilityFunctions.RData")
load("UtilityFunctions.RData")
