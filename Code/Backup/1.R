d="20NOV2022"
wd<<-paste0(data_processed_dir,'/',d)
setwd(wd)

load("Phase11.RData")
load("Phase13.RData")

del[del$RunKey==19,]

del[del$RunKey==842,]
l
del[del$RunKey==430,]


v=cc("724 783")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% del[del$RunKey %in% v,]$DelayGroup[1],]

v=cc("593 608")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% del[del$RunKey %in% v,]$DelayGroup[1],]

v=cc("562 586")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% del[del$RunKey %in% v,]$DelayGroup[1],]

v=cc("534 582")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]


v=cc("558 581")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]

v=cc("522 577")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]

v=cc("428 496")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]

v=cc("413 487")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]

v=cc("344 413")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]

v=cc("522 577")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]

v=cc("522 577")
del[del$RunKey==v[1],];del[del$RunKey==v[2],]
del[del$RunKey %in% v,]$DelayGroup
del[del$DelayGroup %in% na.omit(del[del$RunKey %in% v,]$DelayGroup),]





x=AddLateness(x)

x[x$RunKey==349           ,]
x[x$RunKey==344   ,]


x[x$RunKey==608,]

x[x$RunKey==593,]
x$Latesness=as.numeric(x$DC1)-as.numeric(x$SDC1)


table(del$DelayGroup)
del[del$RunKey==783,]$DelayGroup
del[del$DelayGroup==4,]





del[del$DelayGroup==3,]


del[del$DelayGroup==4,]
x[x$RunKey==724,]
x[x$RunKey==783,]


di[di$RK==585,]
#hole but delayed

load("Phase11.RData")
di[di$RunKey==585,]
di[di$DelayCodes=="HL",][25,]
