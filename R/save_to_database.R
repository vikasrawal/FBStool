
save_to_database <- function(data, countryData,year_range,session,input,output){
  
  country_code  <- unique(countryData$CountryM49)
  country  <- unique(countryData$Country)
  element <- unique(data$ElementCode)
  cpc <- unique(data$CPCCode)
  
  
  data <- long_format(data)
  data[, c("Commodity","Element") := NULL]
  data[,ElementCode := as.character(ElementCode)]
  
  # write.csv(data, "test_save.csv", row.names = F)
  
  
  
  # if(!all(unique(data$Flag) %in% c("","M","E","I", NA,"T"))){
  #   
  # 
  #   sendSweetAlert(
  #     session = session,
  #     title = "WARNING !!",
  #     text = "Wrong Flag",
  #     type = "warning"
  #   )
  #   
  #   
  # 
  #   
  # } else {
    
    database <- copy(countryData)
    database[, c("CountryM49","Country", "Commodity","Element") := NULL]
    
    
    new_data <-merge(database,data, by=c("CPCCode","ElementCode","Year"), all = TRUE)
    
    new_data[, Value.x := ifelse(ElementCode %in% element & Year %in% year_range & CPCCode %in% cpc, Value.y,Value.x)]
    new_data[, Flag.x := ifelse(ElementCode %in% element & Year %in% year_range & CPCCode %in% cpc, Flag.y,Flag.x)]
    
    
    new_data[,c("Value.y","Flag.y") := NULL]
    
    setnames(new_data, c("Value.x","Flag.x"),c("Value","Flag"))
    
    new_data[, CountryM49 := country_code]
    
    new_data[, Country := country]
    
    new_data <- merge(new_data,all_cpc, by = "CPCCode", all.x = TRUE)
    new_data <- merge(new_data,all_elements_to_merge, by = "ElementCode", all.x = TRUE)
    
    setcolorder(new_data, c("CountryM49","Country","CPCCode", "Commodity","ElementCode","Element","Year","Value","Flag"))
    
    new_data[, Flag := ifelse(!is.na(Value) & is.na(Flag), "", Flag)]
    
    
    save(new_data, file = "Data/countrySUA.RData")
    
    countryData <<- new_data
    
    
  # }
  
  
  

}




