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
      print(i)
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
  vars=c("RevDate","RUNKEY","SRK","locs","Locs","DelayCodes","SDispatch","Timestamp","Note","DL")
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
  data[data=="''"]="null"
  # data
  
  print(paste0(tableName, " tabe, writing to sql server for rev date ", date))
  
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
  
  dbExecute(conn,insertStatement);
  dbDisconnect(conn)
  
}

save.image("SQLFunctions.RData")
