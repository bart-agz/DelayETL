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

insertDataToSQL = function(data, date, tableName){
  # update so of the column values
  data = data[,!"Id"]  
  
  #add single quotes to column values
  vars=c("RevDate","RUNKEY","SRK","locs","Locs","DelayCodes","SDispatch","Timestamp","Note","DL","ShortDesc","Narrative",'Yard','ProblemCode','VehDate')
  for (var in vars){
    if (var %in% colnames(data)){
      data[[var]]=paste0("'",data[[var]],"'")
    }
  }
  
  vars=c("DO","DC","SDO","SDC")
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


save.image("SQLFunctions.RData")
# DELETE FROM [RelEng].[dbo].[Incident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[VehIncident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[Delay] WHERE 1=1;
# SELECT * FROM [RelEng].[dbo].[Locaations] WHERE Abbr="S07";
# SELECT * FROM [RelEng].[dbo].[Locations] WHERE Abbr='S07';

# DELETE FROM [RelEng].[dbo].[Delay] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[Incident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[VehIncident] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[DispatchList] WHERE 1=1;
# DELETE FROM [RelEng].[dbo].[CarList] WHERE 1=1;
