sua_balanced_all_elements <- function(input,output,session){
  
  t=as.character(input$endyear)
  
  sua_balanced_data = fread(paste0("SUA-FBS Balancing/StandardizedData/_02_AfterSuaFilling_BeforeST",t,".csv"))
  
  
  sua_balanced_data <- subset(sua_balanced_data, timePointYears == input$endyear)
  
  
  
  if(t > 2014){
    
    sua_balanced_data_otheryears=list()
    
    for (i in c(1,as.numeric(t)-2014)){
      
      
      sua_balanced_data_otheryears[[i]] = fread(paste0("SUA-FBS Balancing/StandardizedData/_02_AfterSuaFilling_BeforeST",as.numeric(t)-i,".csv")) 
      
    }
    
    sua_balanced_data_otheryears <-rbindlist(sua_balanced_data_otheryears)
    
    sua_balanced_data <- rbind(sua_balanced_data,sua_balanced_data_otheryears)
    
  }
  
  
  
  
  if (!(t %in% c(2010:2013))){
    
    #sua balanced is fixed for 2010-2013
    sua_balanced_data_2010_2013 <- fread("SUA-FBS Balancing/StandardizedData/sua_balanced_data_2010_2013.csv")
    
    sua_balanced_data_2010_2013[, flagMethod := NULL]
    
    sua_balanced_data <- rbind(sua_balanced_data,sua_balanced_data_2010_2013)
    
  }
  
  
  if (t == 2013){
    
    sua_balanced_data_2010_2013 <- fread("SUA-FBS Balancing/StandardizedData/sua_balanced_data_2010_2013.csv")
    
    sua_balanced_data_2010_2013[, flagMethod := NULL]
    
    sua_balanced_data_2010_2013 <- sua_balanced_data_2010_2013[timePointYears %in% c(2010,2011)]
    
    sua_balanced_data <- rbind(sua_balanced_data,sua_balanced_data_2010_2013)
    
  }
  
  
  sua_balanced_data[!duplicated(sua_balanced_data)]
  
  sua_balanced_data[, c("geographicAreaM49") := NULL]
  
  setnames(sua_balanced_data, "flagObservationStatus","Flag")
  
  sua_balanced_data =  dcast.data.table(sua_balanced_data, measuredItemFbsSua+measuredElementSuaFbs
                                        ~ timePointYears, value.var = c("Value","Flag"))
  
  sua_balanced_data <- merge(sua_balanced_data, commodityName, by.x="measuredItemFbsSua",by.y = "CPCCode", all.x = TRUE)
  sua_balanced_data <- merge(sua_balanced_data, elementName, by.x="measuredElementSuaFbs",by.y = "ElementCode", all.x = TRUE)
  

  flagcols <- grep("^Flag", names(sua_balanced_data), value = TRUE)
  yearcols <- grep("^Value", names(sua_balanced_data), value = TRUE) 
  
  
  flagcols_new=gsub("_", " ", flagcols, fixed=TRUE)
  
  yearcols_new=gsub("^.*?_","",yearcols)
  
  setnames(sua_balanced_data,flagcols, flagcols_new)
  setnames(sua_balanced_data,yearcols, yearcols_new)
  
  addorder <- as.vector(rbind(yearcols_new, flagcols_new))
  
  
  setnames(sua_balanced_data, c("measuredItemFbsSua","measuredElementSuaFbs"),c("CPCCode","ElementCode"))
  
  
  setcolorder(sua_balanced_data, c("CPCCode","Commodity", "ElementCode","Element",addorder))
  
  sua_balanced_data <- sua_balanced_data[order(CPCCode)]
  
  sua_balanced_data[is.na(sua_balanced_data)] <- ""
  
  sua_balanced_data[, (yearcols_new) := lapply(.SD, as.numeric), .SDcols = yearcols_new]
  
  sua_balanced_data <- sua_balanced_data[, (yearcols_new) :=round(.SD,0), .SDcols=yearcols_new]
  
  desiredOrder <- c("5510","5610","5071","5910","5520","5525","5016","5023","5141","5165","664")
  
  sua_balanced_data <- sua_balanced_data[ElementCode %in% desiredOrder]
  

  

  sua_balanced_data=sua_balanced_data[order(match(sua_balanced_data$ElementCode, desiredOrder)),]
 
  sua_balanced_data=sua_balanced_data[order(CPCCode)]
 
 
 
  
  return(sua_balanced_data)
  
  
  
}

