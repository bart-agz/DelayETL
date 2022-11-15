#SQL Functions
#Functions used to write data to SQL Server
#11/14/2022
#by Jonathan R Agnew (jra)

rm(list=ls())
#setwd("C:/R_PRojects/Hello-world-r/Data/RData")
setwd(paste(sep="",find_rstudio_root_file(),"./Data/RData"))

#insert delay data to sql table
insertDataToSQL = function(data, date, tableName){

  # update so of the column values
  data = data[,!"Id"]  
  
  #add singele quotes to column values
  vars=c("RevDate","RUNKEY","SRK")
  for (var in vars){
    if (var %in% colnames(data)){
      data[[var]]=paste0("'",data[[var]],"'")
    }
  }

  # change na to nullk  
  data[is.na(data)]="null"
  
  
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
        print(insertStatement)
        dbExecute(conn,insertStatement);
        dbDisconnect(conn)
        
        
}

save.image("SQLFunctions.RData")
