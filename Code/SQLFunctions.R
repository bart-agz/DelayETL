#SQL Functions
#Functions used to write data to SQL Server
#11/14/2022
#by Jonathan R Agnew (jra)

# rm(list=ls())

setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))
# 



# insert data to sql table
insertDataToSQL_Bulk=function(data, date, tableName,individual=F){
  if (individual){
    #  SQL insert 1 row at a time
    for (i in seq(1,nrow(data))){
      # print(i)
      # print(data[i,])
      insertDataToSQL(data[i,],date,tableName)
    }
  } else {
    done=F
    #upload 500 rows at a time
    while (!done){
      i=min(nrow(data),500)
      insertDataToSQL(data[1:i,],date,tableName)
      if (i==nrow(data)){
        done=T
      }
      data=data[(i+1):nrow(data),]
    }
  }
}
# data=x[1,]
# tableName="RunList"
insertDataToSQL = function(data, date, tableName){
  # update so of the column values
  data = data[,!"Id"]  
  
  #add single quotes to column values
  vars=c("RevDate","RUNKEY","SRK","locs","Locs","DelayCodes","SDispatch","Timestamp","Note","DL","ShortDesc","Narrative",'Yard','ProblemCode','VehDate','SchDate',"SchCode")
  for (var in vars){
    if (var %in% colnames(data)){
      data[[var]]=paste0("'",data[[var]],"'")
    }
  }
  
  vars=c("DO","DC","SDO","SDC","AD")
  var="Note"
  
  for (var in vars){
    if (var %in% colnames(data)){
      data[[var]]=paste0("'",data[[var]],"'")
      if (sum(data[[var]]=="'null'")>=1){
        data[data[[var]]=="'null'",][[var]]='null'
      } 
    }
  }
  if ("Note" %in% colnames(data)){
    data$Note=gsub("'null'","null",data$Note)
  }
  
  data[is.na(data)]="null"
  # data$DelayCodes[1]
  if (!(tableName %in% c("VehIncident"))){
    data[data=="''"]="null"
  }
  # data
  
  print(paste0(tableName, " table, writing to sql server for rev date ", date))
  
  conn <- dbConnect(odbc(),
                    driver= "SQL Server",
                    server="D-JAGNEW",
                    database ='RelEng',
                    Trusted_Connection = "yes")
  
  #  dbInfo <- dbGetInfo(conn)
  #  print(dbInfo$dbms.name)
  #  print(dbInfo$db.version)
  #  print(dbInfo$odbc.version)
  #  dbListTables(conn, schema_name = 'dbo');
  #  dbReadTable(conn, "Causes")
  
  insertStatement = sqlAppendTable(conn, tableName, data, row.names = FALSE);
  insertStatement
  dbExecute(conn,insertStatement);
  dbDisconnect(conn)
}

SQLRowSummary=function(){
  vx=c("Delay","RunList","CarList","DispatchList","VehIncident")
  rm(tbl)
  for (v in vx){
    z1=SQLLoad(v)
    if ("RevDate" %in% colnames(z1)){
      dz="RevDate"
    } else {
      dz="VehDate"
    }
    z2=table(z1[[dz]])
    tb=as.data.table(z2)
    colnames(tb)=c("RevDate",v)
    if (v=="DispatchList"){
      z3=reordordt(z1,c("RevDate","SchCode","SchDate"))
      z3=unique(z3)
      tb=merge(tb,z3)
    }
    if (exists('tbl')){
      tbl=merge(tbl,tb,by="RevDate",all=T)
    } else {
      tbl=tb
    }
  }
  tbl=tbl[order(tbl$RevDate),]
  tbl=reordordt(tbl,c("RevDate","SchDate","SchCode","RunList","Delay","DispatchList","CarList","VehIncident"))
    return(tbl)
}

# SQLRowSummary()

SQLLoad=function(tableName="Delay"){
  # "Delay" "RunList" "CarList" "DispatchList"
  conn <- dbConnect(odbc(),
                    driver= "SQL Server",
                    server="D-JAGNEW",
                    database ='RelEng',
                    Trusted_Connection = "yes")
  z=DBI::dbReadTable(conn,tableName)
  dbDisconnect(conn)
  z=as.data.table(z)
  return(z)
}

CheckDates=function(date,tableName="Delay"){
  #delay date format: "2022-11-20"
  # date="2022-11-20"
  # date;tableName='Delay'
  conn <- dbConnect(odbc(),
                    driver= "SQL Server",
                    server="D-JAGNEW",
                    database ='RelEng',
                    Trusted_Connection = "yes")
  # z=DBI::dbReadTable(conn,tableName)
  # MaxDates=dbSendQuery(conn,"SELECT distinct RevDate FROM [RelEng].[dbo].[Delay] order by revDate desc")
  if (tableName=="VehIncident"){
    vardate="VehDate"
  } else if (tableName=="Delay"){
    vardate="RevDate"
  } else {
    sysColError
  }
  MaxDates=dbSendQuery(conn,paste0("SELECT distinct ",vardate," FROM [RelEng].[dbo].[",tableName,"]"))
  MaxDates=dbFetch(MaxDates)
  # dbClearResult(MaxDates)
  dbDisconnect(conn)
  if (tableName=="Delay"){
    date=as.Date(date,"%d%B%Y")
    l=date %in% as.Date(unique(MaxDates[[vardate]]))
  } else if (tableName=="VehIncident"){
    l=as.Date(date,"%d%B%Y") %in% as.Date(unique(MaxDates[[vardate]]))
  } else {
    sys1
  }
  return (l)
}

# 
# DELETE FROM [RelEng].[dbo].[Incident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[VehIncident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[Delay] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[DispatchList] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[RunList] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[CarList] WHERE 1=1;

# SELECT * FROM [RelEng].[dbo].[Locaations] WHERE Abbr="S07";
# SELECT * FROM [RelEng].[dbo].[Locations] WHERE Abbr='S07';

# DELETE FROM [RelEng].[dbo].[Delay] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[Incident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[VehIncident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[DispatchList] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[CarList] WHERE 1=1;
GenerateSQL=function(type="Delete",tableName,Date){
  # type="Delete"
  # tableName="Delay"
  # Date="2022-12-15"
  # GenerateSQL("Delete","VehIncident","2022-12-15")
  if (type=="VehIncident"){
    dz="VehDate"
  } else {
    dz="RevDate"
  }
  cat(paste0(type," from [RelEng].[dbo].[",tableName,"] where ",dz,"='",Date,"'",sep=""))
}
# GenerateSQL("Delete","VehIncident","2022-12-15")
# GenerateSQL("Select","VehIncident","2022-12-15")

# DELETE FROM [RelEng].[dbo].[Delay] WHERE RevDate='2022-12-02'
# DELETE FROM [RelEng].[dbo].[Incident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[VehIncident] WHERE VehDate='2022-12-02'
# DELETE FROM [RelEng].[dbo].[DispatchList] WHERE RevDate='2022-12-02'
# DELETE FROM [RelEng].[dbo].[CarList] WHERE RevDate='2022-12-02'
save.image("SQLFunctions.RData")
# DELETE FROM [RelEng].[dbo].[Incident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[VehIncident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[Delay] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[RunList] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[DispatchList] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[RunList] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[CarList] WHERE 1=1;


# DELETE FROM [RelEng].[dbo].[Delay] WHERE RevDate>='2022-12-31'
# 
# DELETE FROM [RelEng].[dbo].[VehIncident] WHERE VehDate>='2022-12-31'
# 
# DELETE FROM [RelEng].[dbo].[DispatchList] WHERE RevDate>='2022-12-31'
# DELETE FROM [RelEng].[dbo].[CarList] WHERE RevDate >='2022-12-31'
# DELETE FROM [RelEng].[dbo].[Runlist] WHERE RevDate >='2022-12-31'
