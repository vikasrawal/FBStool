
tradeImportMapping_old=function(input,output,session){


  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  
 
  importData=data.table(df_importsCountry$data_importsCountry)
    
  
  ######
  # importData= data.table(read_excel("Data/NSO_trade_data.xlsx"))
  #######
  


  if(input$importCommodity != ""){
  
  importData = importData[,c(input$importYear, input$importHS,input$importQuantity,input$importCommodity),with=F]
  
  } else {
    
    importData = importData[,c(input$importYear, input$importHS,input$importQuantity),with=F]
  }
  
  
  
  
  #########
  # importData = importData[,c("Year", "HS6","Import (MT)", "Commodity"),with=F]
  ########
  
  
  importData=data.table(importData)
  
  if(input$importCommodity != ""){
  setnames(importData,c(input$importYear,input$importHS,input$importQuantity,input$importCommodity), 
           c("Year","HS6","Value","Commodity"))
  }else {
    
    setnames(importData,c(input$importYear,input$importHS,input$importQuantity), 
             c("Year","HS6","Value"))
    
  }
  

  
  importData <- importData[Year %in% t]
  
  
  
  if (nrow(importData)== 0){
    
    sendSweetAlert(
      session = session,
      title = "No data to map !!",
      text = "No data to map",
      type = "warning"
    )
    
    
    finalData <- df_imports$data_imports
    
    
  } 
  

  
  else{
    


 importData$HS6 = substr(importData$HS6,1,6)

 importData[, c("HS6","Year") := lapply(.SD, as.character), .SDcols =c("HS6","Year")]
 
 importData[, c("Value") := lapply(.SD, as.numeric), .SDcols =c("Value")]
 
 #read trade map 
 
  tradeMap <- data.table(read_excel("Data/tradeMap_2019.xlsx"))
  
  tradeMap <- tradeMap[!cpc == ""]  #new
  tradeMap[,HS6 := substr(hs,1,6)]
 
  tradeMap[,lenght_hs := nchar(hs)]
  # 
  #checking for more than 6 digits of hs codes
  tradeMap[lenght_hs > 6 ]
  
  importMap <- tradeMap[flow == 1]
  
  
  
  importData <- importData[!is.na(HS6)]
  
  if(input$importCommodity != ""){
  
  importData <- importData[, .(Value=sum(Value)), by=.(HS6,Year,Commodity)]
  
  }else{
    
    importData <- importData[, .(Value=sum(Value)), by=.(HS6,Year)]
    
  }
  
  importData <- importData[!is.na(HS6)]
  
  
  importData <- merge(importData,unique(importMap[,c("HS6", "cpc"),with = F]),by= "HS6" ,
                         all.x =  T) ###new
   
  ###commodities that not mapped 

  if(input$importCommodity != ""){
  
  
  importData_Not_mapped <-  importData[is.na(cpc)] [,c("HS6","Commodity","Value")] # new
  
  df_imports_not_mapped$data_imports_not_mapped <- importData_Not_mapped[!is.na(Value)] # assigning to a vector
  
  }else {
    
    importData_Not_mapped <-  importData[is.na(cpc)] [,c("HS6","Value")] # new
    
    df_imports_not_mapped$data_imports_not_mapped <- importData_Not_mapped[!is.na(Value)]  
    
    
  }
  
  
  
  importData <- importData[!is.na(cpc)] #new
   
  if(input$importCommodity!= ""){
  
  importData[, Commodity := NULL]
    
  }
  
  if (nrow(importData) == 0){

    sendSweetAlert(
      session = session,
      title = "WARNING !!",
      text = "There are no CPC codes mapped to the provided HS Codes",
      type = "warning"
    )

    finalData <- df_imports$data_imports

  } else {
  
  
  
  
  importData <- aggregate(
    Value ~ Year+cpc ,
    importData, sum, na.rm = TRUE)
  
  importData <- data.table(importData)
  
  setnames(importData,"cpc","CPCCode")
  
  # importData[, ElementCode := "5610"]
  
  # importData <- merge(importData,all_cpc,by = "CPCCode",all.x = T)
  # importData <- merge(importData,all_elements,by = "ElementCode",all.x = T)
 
  importData=importData[!duplicated(importData),]
  
  importData[,Flag := ""]

  # importData <- importData[!is.na(cpc)]
  
### bind trade other years data
  #melting current trade data
  
  
  
  trade_import_years <- data.table(df_imports$data_imports)
  
  trade_import_years[,hidden := NULL]
  
  
  
  flagcols <- grep("^Symbole", names(trade_import_years), value = TRUE)
  yearcols<- grep("^[[:digit:]]{4}$", names(trade_import_years), value = TRUE)
  
  
  trade_import_years[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
  trade_import_years[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  
  
  trade_import_years <- melt.data.table(trade_import_years, id.vars = c("CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =grep("^[[:digit:]]{4}$", names(trade_import_years), value = TRUE),
                                        value.name= "Value")
  
  
  
  trade_import_years <-subset(countryData, ElementCode == "5610")
  
  trade_import_years[, ElementCode := NULL]
  
  trade_import_years[,c("CountryM49","Country","Commodity","Element") := NULL]
  
  
  trade_import_years <- trade_import_years[!Year %in% t]
  
###merge 
  
  # finalData <- merge(trade_import_years, importData, by=c("CPCCode","Year"), all = TRUE)
  # 
  # finalData[, Value.x := ifelse(!is.na(Value.y), Value.y, Value.x)]
  # finalData[, Flag.x := ifelse(!is.na(Value.y), Flag.y, Flag.x)]
  # 
  # finalData[,c("Value.y","Flag.y") := NULL]
  # 
  # setnames(finalData,c("Value.x","Flag.x"),c("Value","Flag"))
  
  finalData <- rbind(trade_import_years,importData)
  
  data_to_merge <-data.table(expand.grid(CPCCode = as.character(unique(finalData$CPCCode)), 
                                         Year = as.character(c(2010:t[length(t)]))))
  
  finalData <- merge(data_to_merge,finalData, by =c("CPCCode","Year"), all.x = TRUE)
  
  
  finalData[, ElementCode := "5610"]
  
  finalData <- merge(finalData,all_cpc,by = "CPCCode",all.x = TRUE)
  finalData <- merge(finalData,all_elements,by = "ElementCode",all.x = TRUE)
  
  finalData <- wide_format(finalData)

 
  
  finalData
  
  
  
  
 
}
  finalData

}

  
  
  

}
