
tradeExportMapping_old=function(input,output,session, values_exportCountryData){
  
  
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
 
 
  
  
  exportData=data.table(df_exportsCountry$data_exportsCountry)
  # exportData= data.table(read_excel("SUA-FBS Balancing/Trade/Data/trad2014.xlsx"))
  
  
  if(input$exportCommodity != ""){
  
  exportData = exportData[,c(input$exportYear, input$exportHS,input$exportQuantity,input$exportCommodity),with=F]
  
  }else{
    
  exportData = exportData[,c(input$exportYear, input$exportHS,input$exportQuantity),with=F]
    
    
  }
  # exportData = exportData[,c("Year", "SUCODE","IMPORT_QNN"),with=F]
  
  # exportData=exportData[, c("Year","HS6","Value")]
  
  exportData=data.table(exportData)
  
  if(input$exportCommodity != ""){
  setnames(exportData,c(input$exportYear,input$exportHS,input$exportQuantity,input$exportCommodity), 
           c("Year","HS6","Value", "Commodity" ))
    
  }else{
    
    setnames(exportData,c(input$exportYear,input$exportHS,input$exportQuantity), 
             c("Year","HS6","Value" )) 
    
  }
    
  # setnames(exportData,c("Year","SUCODE","IMPORT_QNN"), c("Year","HS6","Value"))
  
  
  # if (length(exportData$HS6) < 6){
  #   
  #   sendSweetAlert(
  #     session = session,
  #     title = "WARNING !!",
  #     text = "Invalid HS Code",
  #     type = "warning"
  #   )
  #   
  #   finalData <- df_exports$data_exports
  #   
  # } else {
  # 
  
  exportData <- exportData[Year %in% t]
  
  
  
  if (nrow(exportData)== 0){
    
    sendSweetAlert(
      session = session,
      title = "No data to map !!",
      text = "No data to map",
      type = "warning"
    )
    
    
    finalData <- df_exports$data_exports
    
    
  } 
  
  
  
  else{
    
    
    
    exportData$HS6 = substr(exportData$HS6,1,6)
    
    exportData[, c("HS6","Year") := lapply(.SD, as.character), .SDcols =c("HS6","Year")]
    
    exportData[, c("Value") := lapply(.SD, as.numeric), .SDcols =c("Value")]
    
    #read trade map 
    
    tradeMap <- data.table(read_excel("Data/tradeMap_2019.xlsx"))
    
    tradeMap <- tradeMap[!cpc == ""]  #new
    tradeMap[,HS6 := substr(hs,1,6)]
    
    tradeMap[,lenght_hs := nchar(hs)]
    # 
    #checking for more than 6 digits of hs codes
    tradeMap[lenght_hs > 6 ]
    
    exportMap <- tradeMap[flow == 2]
    exportMap <- exportMap[!is.na(HS6)]
    
    if(input$exportCommodity != ""){
    
    exportData <- exportData[, .(Value=sum(Value)), by=.(HS6,Year,Commodity)] ###new
    
    } else{
      
      exportData <- exportData[, .(Value=sum(Value)), by=.(HS6,Year)]
    }
    
    exportData <- exportData[!is.na(HS6)]
    
    exportData <- merge(exportData,unique(exportMap[,c("HS6", "cpc"),with = F]),by= "HS6" ,
                        all.x = TRUE)
    
    ###commodities that not mapped 
    if(input$exportCommodity != ""){
      
    exportData_Not_mapped <-  exportData[is.na(cpc)] [,c("HS6","Commodity","Value")] 
    df_exports_not_mapped$data_exports_not_mapped <- exportData_Not_mapped[!is.na(Value)]# new
    
    }else {
      
      exportData_Not_mapped <-  exportData[is.na(cpc)] [,c("HS6","Value")] # new
      df_exports_not_mapped$data_exports_not_mapped <- exportData_Not_mapped[!is.na(Value)]
      
    }
    
   
    
    exportData <- exportData[!is.na(cpc)] #new
    
    if(input$exportCommodity != ""){
      
    exportData[, Commodity := NULL]
      
    }
      
      
      
    
    if (nrow(exportData) == 0){
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "There are no CPC codes mapped to the provided HS Codes",
        type = "warning"
      )
      
      finalData <- df_exports$data_exports
      
    } else {
    
  
    
    exportData <- aggregate(
      Value ~ Year+cpc ,
      exportData, sum, na.rm = TRUE)
    
    exportData <- data.table(exportData)
    
    setnames(exportData,"cpc","CPCCode")
    
    # exportData[, ElementCode := "5610"]
    
    # exportData <- merge(exportData,all_cpc,by = "CPCCode",all.x = T)
    # exportData <- merge(exportData,all_elements,by = "ElementCode",all.x = T)
    
    exportData=exportData[!duplicated(exportData),]
    
    exportData[,Flag := ""]
    
    
    
    ### bind trade other years data
    trade_export_years <- data.table(df_exports$data_exports)
    
    trade_export_years[,hidden := NULL]
    
    
    
    flagcols <- grep("^Symbole", names(trade_export_years), value = TRUE)
    yearcols<- grep("^[[:digit:]]{4}$", names(trade_export_years), value = TRUE)
    
    
    trade_export_years[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
    trade_export_years[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    
    
    trade_export_years <- melt.data.table(trade_export_years, id.vars = c("CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =grep("^[[:digit:]]{4}$", names(trade_export_years), value = TRUE),
                                          value.name= "Value")
    
    
    
    trade_export_years <-subset(countryData, ElementCode == "5910")
    
    trade_export_years[, ElementCode := NULL]
    
    trade_export_years[,c("CountryM49","Country","Commodity","Element") := NULL]
    
    trade_export_years <- trade_export_years[!Year %in% t]
    
    ###merge 
    
    # finalData <- merge(trade_export_years, exportData, by=c("CPCCode","Year"), all = TRUE)
    # 
    # finalData[, Value.x := ifelse(!is.na(Value.y), Value.y, Value.x)]
    # finalData[, Flag.x := ifelse(!is.na(Value.y), Flag.y, Flag.x)]
    # 
    # finalData[,c("Value.y","Flag.y") := NULL]
    # 
    # setnames(finalData,c("Value.x","Flag.x"),c("Value","Flag"))
    # 
    
    
    finalData <- rbind(trade_export_years,exportData)
    
    data_to_merge <-data.table(expand.grid(CPCCode = as.character(unique(finalData$CPCCode)), 
                                           Year = as.character(c(2010:t[length(t)]))))
    
    finalData <- merge(data_to_merge,finalData, by =c("CPCCode","Year"), all.x = TRUE)
    
    
    
    finalData[, ElementCode := "5910"]
    
    finalData <- merge(finalData,all_cpc,by = "CPCCode",all.x = TRUE)
    finalData <- merge(finalData,all_elements,by = "ElementCode",all.x = TRUE)
    
    finalData <- wide_format(finalData)
    
    
    
    finalData
    
  }
  
  
  }
  
  
  finalData


}
