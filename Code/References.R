rm(list=ls())
# setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
load("UtilityFunctions.RData")
load("Directories.RData")
rev_routes=seq(1,12)
auto_late=c("CL","OL","HL","ND")

PossibleCodes=c("ED","LD","DH", "LR","SD","AD")
#SD=LRL+DH>300 but both are <300

LatenessThreshold=300
DelayThreshold=300
ADThreshold=30
GDrive_Vehicles="G:/SAS_VIYA/Vehicle/Data"


# setwd(data_references_dir)

conn <- dbConnect(odbc(),
                  driver= "SQL Server",
                  server="D-JAGNEW",
                  database ='RelEng',
                  Trusted_Connection = "yes")
# s=readxl::read_excel("DelayTables.xlsx","DelayTypes")

# s=readxl::read_excel("DelayTables.xlsx","LocationList")
# s=as.data.table(s)
# LocationList=s
# 
# LocationList
s=as.data.table(DBI::dbReadTable(conn,"DelayTypes"))
s=reordordt(s,cc("Id Abbr SortOrder"))
s=as.data.table(s)
DelayTypesSortOrder=s

LocationList=as.data.table(DBI::dbReadTable(conn,"Locations"))
dbDisconnect(conn)
setwd(data_rdata_dir)
save("DelayTypesSortOrder", "LocationList","ADThreshold", "DelayThreshold","PossibleCodes",file="References.RData")
