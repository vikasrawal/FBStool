suaAggregate <- function(input,output,session){

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


sua_balanced_data <- sua_balanced_data[!duplicated(sua_balanced_data)]

# 
kcalData <- subset(sua_balanced_data, measuredElementSuaFbs %in% c("664"))

kcalData[,"flagObservationStatus" := NULL]

kcalData<-kcalData %>% group_by(geographicAreaM49,timePointYears) %>%  mutate(totDES=sum(Value,na.rm = T))

kcalData2 = melt.data.table(setDT(kcalData),id.vars = c("geographicAreaM49","measuredItemFbsSua","timePointYears"),
                            measure.vars = c("Value","totDES"))
setnames(kcalData2, "variable", "Aggregation")
setnames(kcalData2, "value", "Value")

kcalData2[,Aggregation:=ifelse(Aggregation=="Value","Item","GRAND TOTAL")]

grandTotal=subset(kcalData2,Aggregation=="GRAND TOTAL")
grandTotal[,measuredItemFbsSua:="S2901"]
grandTotal=unique(grandTotal)


elementName=read_excel("Data/Reference File.xlsx", "Elements")
setDT(elementName)

commodityName=read_excel("Data/Reference File.xlsx", "SUA_Commodities")
setDT(commodityName)

country_name <- read_excel("Data/Reference File.xlsx", "Country")
setDT(country_name)


grandTotal[, measuredItemFbsSua_description := "GRAND TOTAL - DEMAND"]

grandTotal[,timePointYears_description := unique(grandTotal$timePointYears)]

grandTotal[,geographicAreaM49_description :=  country_name[CountryM49 == unique(grandTotal$geographicAreaM49)]$Country]

# grandTotal=nameData("suafbs","fbs_balanced_",grandTotal)

items <- subset(kcalData2,Aggregation=="Item")

items[,geographicAreaM49_description :=  country_name[CountryM49 == unique(items$geographicAreaM49)]$Country] 

items[,timePointYears_description := unique(items$timePointYears)]



items <- merge(items,commodityName, by.x ="measuredItemFbsSua" ,by.y = "CPCCode",all.x =   TRUE)


setnames(items, "Commodity","measuredItemFbsSua_description")

kcalDataWide = dcast.data.table(setDT(items),geographicAreaM49+geographicAreaM49_description+
                                  measuredItemFbsSua+measuredItemFbsSua_description~timePointYears , 
                                fun.aggregate = NULL,value.var = "Value")
GTWide = dcast.data.table(setDT(grandTotal),geographicAreaM49+geographicAreaM49_description+
                            measuredItemFbsSua+measuredItemFbsSua_description~timePointYears , fun.aggregate = NULL,value.var = "Value")

toSend=rbind(GTWide,kcalDataWide)


setnames(toSend,c("geographicAreaM49","geographicAreaM49_description","measuredItemFbsSua","measuredItemFbsSua_description")
         ,c("CountryM49","Country","CPCCode","Commodity"))


columnNumeric <- grep("^[[:digit:]]{4}$", names(toSend), value = TRUE)

toSend <- toSend[, (columnNumeric) :=round(.SD,0), .SDcols=columnNumeric]

toSend[, (columnNumeric) := lapply(.SD, as.numeric), .SDcols = columnNumeric]

toSend[is.na(toSend)] <- ""


return(toSend)



}

