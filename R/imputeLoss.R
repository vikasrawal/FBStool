imputeLoss=function(input,output,session){
  
 
  show_modal_spinner(
    
    spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
  )
  
  
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  
  data_Session <- data.table(df_sua_unbalanced$data_sua_unbalanced )
  
  data_Session <- long_format(data_Session)
  
  if (unique(countryData$CountryM49) %in% "835"){
    
    itemLoss <- data.table(read_excel("Data/lossRatio.xlsx"))
    
    itemLoss=unique(itemLoss[,c("CPCCode")])
    
    
    itemLoss_session=subset(data_Session, ElementCode == "5016")
    itemLoss_session <- unique(itemLoss_session[,c("CPCCode")])
    
    itemLoss <- rbind(itemLoss,itemLoss_session)
    itemLoss <-itemLoss[!duplicated(itemLoss)]
    
  }else {
    
    itemLoss_session=subset(data_Session, ElementCode == "5016")
    itemLoss_session <- unique(itemLoss_session[,c("CPCCode")])
    
    itemLoss <- data.table(read_excel("Data/lossRatio.xlsx"))
    itemLoss <- unique(itemLoss[,c("CPCCode")])
    
    itemLoss <- rbind(itemLoss,itemLoss_session)
    itemLoss <-itemLoss[!duplicated(itemLoss)]
    
    
    
  }
  
  
 
  
  # upload intermediate Loss Data
  lossData = subset(data_Session, ElementCode == 5016 & CPCCode %in% itemLoss$CPCCode )
  
  lossData[is.na(Value), Flag := NA ]
  

  
  
  lossData[, c("ElementCode","Element") := NULL]

  
 
  setnames(lossData,"Value","[5016] Loss [t]")
  setnames(lossData,"Flag","[5016] Flag")
  
  lossData$`[5016] Flag` =ifelse(!is.na(lossData$`[5016] Loss [t]`) & is.na(lossData$`[5016] Flag`), "", lossData$`[5016] Flag`)
  
  
  
  
  # upload production Data
  productionData=subset(data_Session, ElementCode == "5510")
  
  productionData[, c("ElementCode","Element") := NULL] 
  
  
  productionData = subset(productionData, (CPCCode %in% itemLoss$CPCCode))
  
  # productionData$ElementCode = NULL
  # productionData$Element = NULL
  setnames(productionData,"Value","[5510] Production [t]")
  setnames(productionData,"Flag","[5510] Flag")
  
  
  
  productionData$`[5510] Flag` =ifelse(!is.na(productionData$`[5510] Production [t]`) & is.na(productionData$`[5510] Flag`), "", productionData$`[5510] Flag`)
  
  
  #ratio data
  
  ratios <-  read_excel("Data/lossRatio.xlsx")
  setDT(ratios)
  ratios <- long_format(ratios)
  # ratios[, Value := round(Value,0)]
  
  
  ratios$Flag =ifelse(!is.na(ratios$Value) & is.na(ratios$Flag), "", ratios$Flag)
  
  setnames(ratios,"Flag","[Ratio] Flag")
  # merge datasets
 

  ratios$`ElementCode`= NULL
  ratios$Element = NULL
  # ratios$Flag = ""
  
  setnames(ratios,c("Value"),c("Ratios [%]"))
  
  
  # #stock variation data
  # stockData=subset(data_Session, ElementCode == "5071")
  # 
  # stockData[, c("CountryM49","Country","ElementCode","Element") := NULL] 
  # 
  # 
  # stockData = subset(stockData, (CPCCode %in% itemLoss$CPCCode))
  # 
  # # productionData$ElementCode = NULL
  # # productionData$Element = NULL
  # setnames(stockData,"Value","[5071] Stock Variation [t]")
  # setnames(stockData,"Flag","[5071] Flag")
  # 
  # stockData$`[5071] Flag` =ifelse(!is.na(stockData$`[5071] Stock Variation [t]`) & is.na(stockData$`[5071] Flag`), "", stockData$`[5071] Flag`)
  # 
  
  
  #Import data
  importData=subset(data_Session, ElementCode == "5610")
  
  importData[, c("CountryM49","Country","ElementCode","Element") := NULL] 
  
  
  importData = subset(importData, (CPCCode %in% itemLoss$CPCCode))
  
  # productionData$ElementCode = NULL
  # productionData$Element = NULL
  setnames(importData,"Value","[5610] Import Quantity [t]")
  setnames(importData,"Flag","[5610] Flag")
  
  importData$`[5610] Flag` = ifelse(!is.na(importData$`[5610] Import Quantity [t]`) & is.na(importData$`[5610] Flag`), "", importData$`[5610] Flag`)

  #################

 data = merge(lossData[,-c("Commodity")],productionData[,-c("Commodity")],
               by = c( "CPCCode","Year"),
               all  = TRUE)
  
  
  # data = merge(data,stockData,
  #              by = c( "CPCCode","Year"),
  #              all = TRUE)
  
  
  data = merge(data,importData[,-c("Commodity")],
               by = c( "CPCCode","Year"),
               all = TRUE)
  
 
# ratios$Year = as.character(ratios$Year)
  

  data = merge(data,ratios[,-c("Commodity")],
               by = c("CPCCode","Year"),
               all= TRUE)
  

data[, Protected := ifelse(!is.na(`[5016] Loss [t]`) & `[5016] Flag` %in% c("","T","E"), TRUE, FALSE)] 


data <- merge(data,all_cpc, by = "CPCCode" , all.x = TRUE)

 for (j in t){
   

   
   officialData = filter(data, Year %in% c(2010:(j-1)))
   
   
   
   officialData$`Ratios [%]` = ifelse(is.na(officialData$`[Ratio] Flag`), 
                                      (officialData$'[5016] Loss [t]'/sum(officialData$`[5510] Production [t]`,officialData$`[5610] Import Quantity [t]`,na.rm = TRUE)
                                       
                                      )*100, officialData$`Ratios [%]`)
   
   
   # officialData$`Ratios [%]` = round(officialData$`Ratios [%]`,0)
   officialData$`[Ratio] Flag` = ifelse(is.na(officialData$`[Ratio] Flag`) & !is.na(officialData$`Ratios [%]`), "I",officialData$`[Ratio] Flag`)
   
   calcRatios = aggregate(`Ratios [%]` ~ CPCCode,data = officialData,mean)
   calcRatios$`Ratios [%]` = round(calcRatios$`Ratios [%]`,0)
   
   calcRatios$`Ratios [%]` = ifelse(calcRatios$`Ratios [%]` < 0, 0 , calcRatios$`Ratios [%]`)
   
   
   setnames(calcRatios,"Ratios [%]","Ratios ReCalculated")
   #
   
   imputedData = filter(data, Year %in% c(j) 
                        
                        # !is.na(data$"[5510] Production [t]") &
                        # is.na(data$`[5016] Loss [t]`
   )
   
   
   imputedData = merge(imputedData,calcRatios,
                       by = "CPCCode",
                       all.x = TRUE)
   
   
   
   imputedData$`Ratios [%]` = ifelse(is.na(imputedData$`Ratios [%]`) 
                                     ,imputedData$`Ratios ReCalculated`,imputedData$`Ratios [%]`)
   
   
   
   imputedData$`[Ratio] Flag` = ifelse(!imputedData$`[Ratio] Flag` %in% c("","T","E") & !is.na(imputedData$`Ratios [%]`), "I", imputedData$`[Ratio] Flag`)
   
   imputedData$`Ratios ReCalculated` = NULL
   
   
   setDT(imputedData)
   
   
   imputedData[, Protected := ifelse( !`[5016] Flag` %in% c("","T","E") , FALSE, TRUE)]
   
   
   
   # imputedData[,new_loss := (`Ratios [%]`* sum(`[5510] Production [t]`, - `[5071] Stock Variation [t]`,
   #                                             `[5610] Import Quantity [t]`,na.rm = TRUE)) /100, by= 1: nrow(imputedData)]
   
   
   imputedData[,new_loss := (`Ratios [%]`* sum(`[5510] Production [t]`,
                                               `[5610] Import Quantity [t]`,na.rm = TRUE)) /100, by= 1: nrow(imputedData)]
   
   
   imputedData[Protected == FALSE, `[5016] Loss [t]` := new_loss]
   
   imputedData[Protected == FALSE & !is.na(`[5016] Loss [t]`), `[5016] Flag` := "I"]
   
   imputedData[, c("Protected","new_loss") :=NULL]
   
   imputedData[, `[5016] Loss [t]` := round(`[5016] Loss [t]`,0)]
   
   # setcolorder(imputedData,names(officialData))
   
   imputedData=imputedData[!is.na(`[5016] Loss [t]`)]
   
   imputedData <- imputedData[,c("CPCCode", "Year", "[5016] Loss [t]", "[5016] Flag")]
 
   
   data <- merge(data, imputedData, by = c("CPCCode","Year"), all.x = TRUE)
   
   data[, `[5016] Loss [t].x`:= ifelse(Protected == F & Year == j,`[5016] Loss [t].y`,`[5016] Loss [t].x`)]
   
   data[, `[5016] Flag.x`:= ifelse(Protected == F & Year == j,`[5016] Flag.y`,`[5016] Flag.x`)]
   
   data[,c("[5016] Loss [t].y","[5016] Flag.y"):= NULL]
   
   setnames(data,c("[5016] Loss [t].x","[5016] Flag.x"),c("[5016] Loss [t]","[5016] Flag"))
   
  
   
 } 
 
 finalLossData <- copy(data)

  finalLossData$`Ratios [%]` = as.numeric(finalLossData$`Ratios [%]`)
  finalLossData$`[5016] Loss [t]` = as.numeric(finalLossData$`[5016] Loss [t]`)
  finalLossData$`[5510] Production [t]` = as.numeric(finalLossData$`[5510] Production [t]`)
  
  finalLossData[, Protected := NULL]
  
  lossValueData = melt(finalLossData[,-c("[5016] Flag","[5510] Flag","[5610] Flag","[Ratio] Flag")], 
                       
                       id.vars = c("CPCCode","Commodity","Year"),
                       variable.name = "Element", value.name = "Value")
  
  
  # unique(lossValueData$Element)
  # lossValueData$Element = ifelse(lossValueData$Element == "[5016] Loss [t]","Loss [t]",
  #                                ifelse(lossValueData$Element == "[5510] Production [t]","Production [t]",
  #                                       ifelse(lossValueData$Element == "[5071] Stock Variation [t]","Stock Variation [t]",
  #                                              ifelse(lossValueData$Element == "[5610] Import Quantity [t]","Import Quantity [t]",     
  #                                       "Ratios [%]"
  #                                              ))))
  # 
  
  lossValueData$Element = ifelse(lossValueData$Element == "[5016] Loss [t]","Loss [t]",
                                 ifelse(lossValueData$Element == "[5510] Production [t]","Production [t]",
                                        # ifelse(lossValueData$Element == "[5071] Stock Variation [t]","Stock Variation [t]",
                                               ifelse(lossValueData$Element == "[5610] Import Quantity [t]","Import Quantity [t]",     
                                                      "Ratios [%]"
                                               )))
  
  
  # lossFlagData = melt(finalLossData[,-c(6,8,10,12)], id.vars = c("CountryM49","Country","CPCCode","Commodity","Year"),
  #                     variable.name = "Flag Description", value.name = "Flag")
  
  
  # lossFlagData = melt(finalLossData[,-c("[5016] Loss [t]","[5510] Production [t]","[5071] Stock Variation [t]","[5610] Import Quantity [t]"
  #                                       ,"Ratios [%]")], 
  #                     id.vars = c("CPCCode","Commodity","Year"),
  #                     variable.name = "Flag Description", value.name = "Flag")
  
  
  lossFlagData = melt(finalLossData[,-c("[5016] Loss [t]","[5510] Production [t]","[5610] Import Quantity [t]"
                                        ,"Ratios [%]")], 
                      id.vars = c("CPCCode","Commodity","Year"),
                      variable.name = "Flag Description", value.name = "Flag")
  
  
  
  lossFlagData$`Flag Description` = NULL
  
  # finalseedData
  # finalseedData
  finalLossData = cbind(lossValueData,lossFlagData[,"Flag"])
  

  
   # finalLossData$ElementCode = ifelse(lossValueData$Element == "Loss [t]","5016",
   #                                   ifelse(lossValueData$Element == "Production [t]","5510",
   #                                          ifelse(lossValueData$Element == "Stock Variation [t]","5071",
   #                                          ifelse(lossValueData$Element == "Import Quantity [t]","5610",
   #                                                 ifelse(lossValueData$Element == "Export Quantity [t]","5910",
   #                                                       "xxxx"
   #                                                               )))))
  
  finalLossData$ElementCode = ifelse(lossValueData$Element == "Loss [t]","5016",
                                     ifelse(lossValueData$Element == "Production [t]","5510",
                                            # ifelse(lossValueData$Element == "Stock Variation [t]","5071",
                                                   ifelse(lossValueData$Element == "Import Quantity [t]","5610",
                                                          ifelse(lossValueData$Element == "Export Quantity [t]","5910",
                                                                 "xxxx"
                                                          ))))
  
  
  
  finalLossData = setcolorder(finalLossData, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
  finalLossData = finalLossData[order(CPCCode,Year)]
  
  finalLossData[, Value := ifelse(ElementCode =="5016" & Value< 0 , NA, Value)]
  
  
  
  # write.csv(finalLossData,file = "Loss/Data/finalLossData.csv", row.names = FALSE)
  
  finalLossData <-  finalLossData[ElementCode == "5016" & is.na(Value), Flag := NA]
  finalLossData <- finalLossData[!duplicated(finalLossData[,c("CPCCode","Element","Year")])]
  
   
  finalLossData[, Value := ifelse(Value == Inf, NA, Value)]
  
  finalLossData[, Flag := ifelse(is.na(Value), NA, Flag)]
  
  finalLossData
  
  Sys.sleep(3)
  remove_modal_spinner()
  
  observeEvent(input$loss_imputation,{
    sendSweetAlert(
      session = session,
      title = "Imputed !!",
      text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
      type = "success"
    )
    
  })
  
  
  


  
  return(finalLossData)
  
  
}