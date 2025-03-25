# Creating Loss Ratios
rm(list = ls())
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)


countryData <- get(load("Data/countrySUA.RData"))
countryData[, Value := as.numeric(Value)]  
live_animals <- c("02151", "02154", "02153", "02194", "02192.01","02191","02152",
                  "02132", "02112","02121.01","02111","02123","02131","02133","02121.02","02192","02122",
                  "02140")

countryData <- countryData[!CPCCode %in% live_animals]




lossData=subset(countryData, ElementCode == "5016")
lossData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(lossData, c("Value","Flag"),c("Loss [t]","Flag Loss"))

productionData=subset(countryData, ElementCode == "5510")


productionData=subset(productionData, CPCCode %in% unique(lossData$CPCCode))


productionData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(productionData, c("Value","Flag"),c("Production [t]","Flag Production"))


# 
# 
# 
# stockChangeData=subset(countryData, ElementCode == "5071")
# stockChangeData=subset(stockChangeData, CPCCode %in% unique(lossData$CPCCode))
# 
# stockChangeData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]
# 
# setnames(stockChangeData, c("Value","Flag"),c("Stock [t]","Flag Stock"))



importData=subset(countryData, ElementCode == "5610")
importData=subset(importData, CPCCode %in% unique(lossData$CPCCode))

importData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(importData, c("Value","Flag"),c("Import [t]","Flag Import"))





data=merge(lossData,productionData, by=c("CPCCode","Year"), all=TRUE)

# data=merge(data,stockChangeData, by = c("CPCCode","Year"), all=TRUE)


data=merge(data,importData,by = c("CPCCode","Year"), all=TRUE)

# data[, `Loss Ratio with stock and production` := (`Loss [t]`/ sum(`Production [t]`,`Stock [t]` , na.rm = TRUE))*100, by = 1: nrow(data)]
# data[, `Loss Ratio` := (`Loss [t]`/ sum(`Production [t]`,`Stock [t]`,
#                                                                           `Import [t]` , na.rm = TRUE))*100, by = 1: nrow(data)]



# data[, `Loss Ratio` := (`Loss [t]`/ sum(`Production [t]`,- `Stock [t]`,`Import [t]` , na.rm = TRUE))*100,by = 1: nrow(data) ]

data[, `Loss Ratio` := (`Loss [t]`/ sum(`Production [t]`,`Import [t]` , na.rm = TRUE))*100,by = 1: nrow(data) ]



data[, Flag := "I"]


data=data[!is.na(`Loss [t]`),]




# data=na.omit(data)


data[, c( "Loss [t]","Flag Loss","Production [t]" , "Flag Production" , "Stock [t]","Flag Stock" , "Import [t]","Flag Import") :=NULL]

setnames(data, "Loss Ratio", "Value")



commodityName <- data.table(read_excel("Data/cpc2.1.xlsx"))



commodityName[,c("CONVERSION FACTORS\r\n(FCL-CPC)", "notes" ) :=NULL]

setnames(commodityName, c("SWS CODE","SWS DESCRIPTOR"), c("CPCCode","Commodity"))

commodityName[CPCCode == "21439.040000000001",CPCCode := "21439.04"]

commodityName <- commodityName[!duplicated(commodityName[,"CPCCode"])]


# commodityName=read_excel("Data/Reference File.xlsx", "SUA_Commodities")
# setDT(commodityName)


data=merge(data,commodityName, by= "CPCCode", all.x = TRUE)



data[, ElementCode := "xxxx"]

data[, Element := "Loss Ratio [%]"]



setcolorder(data,c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))

data <-data[!is.na(Value)]

data[,Flag := ifelse(is.na(Value), NA, Flag)]

data[,Flag := ifelse(is.nan(Value), NA, Flag)]
data[,Value := ifelse(is.nan(Value), NA, Value)]

data[,Flag := ifelse(Value == Inf, NA, Flag)]
data[,Value := ifelse(Value == Inf, NA, Value)]


data=wide_format(data)

# data=na.omit(data)

openxlsx::write.xlsx(data, "Data/lossRatio.xlsx",asTable = TRUE,1)



#create feed ratios

rm(list = ls())
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)


countryData <- get(load("Data/countrySUA.RData"))
countryData[, Value := as.numeric(Value)]  
live_animals <- c("02151", "02154", "02153", "02194", "02192.01","02191","02152",
                  "02132", "02112","02121.01","02111","02123","02131","02133","02121.02","02192","02122",
                  "02140")

countryData <- countryData[!CPCCode %in% live_animals]




feedData=subset(countryData, ElementCode == "5520")
feedData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(feedData, c("Value","Flag"),c("Feed [t]","Flag Feed"))

productionData=subset(countryData, ElementCode == "5510")


productionData=subset(productionData, CPCCode %in% unique(feedData$CPCCode))


productionData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(productionData, c("Value","Flag"),c("Production [t]","Flag Production"))


importData=subset(countryData, ElementCode == "5610")

importData=subset(importData, CPCCode %in% unique(feedData$CPCCode))

importData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(importData, c("Value","Flag"),c("Import [t]","Flag Import"))


exportData=subset(countryData, ElementCode == "5910")

exportData=subset(exportData, CPCCode %in% unique(feedData$CPCCode))

exportData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(exportData, c("Value","Flag"),c("Export [t]","Flag Export"))



stockData=subset(countryData, ElementCode == "5071")

stockData=subset(stockData, CPCCode %in% unique(feedData$CPCCode))

stockData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(stockData, c("Value","Flag"),c("Stock Variation [t]","Flag Stock"))




data=merge(feedData,productionData, by=c("CPCCode","Year"), all=TRUE)


data=merge(data,importData, by=c("CPCCode","Year"), all=TRUE)

data=merge(data,exportData, by=c("CPCCode","Year"), all=TRUE)


data=merge(data,stockData, by=c("CPCCode","Year"), all=TRUE)



# data[, `Feed Ratio` := `Feed [t]`/ (`Production [t]`+`Stock Variation [t]`+ `Import [t]` - `Export [t]`)]

#for feed ratios we dont add stock for time being.

data[, `Feed Ratio` := (`Feed [t]`/ sum(`Production [t]`,`Import [t]` ,- `Export [t]`, na.rm = TRUE))*100,by = 1: nrow(data) ]




data[, Flag := "I"]


data[, `Feed Ratio` := ifelse(`Feed Ratio` < 0, 0 , `Feed Ratio` )]

data <- data[, c("CPCCode","Year","Feed Ratio","Flag")]


commodityName <- data.table(read_excel("Data/cpc2.1.xlsx"))



commodityName[,c("CONVERSION FACTORS\r\n(FCL-CPC)", "notes" ) :=NULL]

setnames(commodityName, c("SWS CODE","SWS DESCRIPTOR"), c("CPCCode","Commodity"))

commodityName[CPCCode == "21439.040000000001",CPCCode := "21439.04"]

commodityName <- commodityName[!duplicated(commodityName[,"CPCCode"])]

# commodityName=read_excel("Data/Reference File.xlsx", "SUA_Commodities")
# setDT(commodityName)



data=merge(data,commodityName, by= "CPCCode", all.x = TRUE)



data[, ElementCode := "xxxx"]

data[, Element := "Feed Ratio [%]"]

setnames(data,"Feed Ratio","Value")

setcolorder(data,c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))

data[,Flag := ifelse(is.na(Value), NA, Flag)]

data[,Flag := ifelse(is.nan(Value), NA, Flag)]
data[,Value := ifelse(is.nan(Value), NA, Value)]

data[,Flag := ifelse(Value == Inf, NA, Flag)]
data[,Value := ifelse(Value == Inf, NA, Value)]



data=wide_format(data)




openxlsx::write.xlsx(data, "Data/feedRatio.xlsx",asTable = TRUE,1)


#seed Rates 
rm(list = ls())
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)



countryData <- get(load("Data/countrySUA.RData"))
countryData[, Value := as.numeric(Value)]  
live_animals <- c("02151", "02154", "02153", "02194", "02192.01","02191","02152",
                  "02132", "02112","02121.01","02111","02123","02131","02133","02121.02","02192","02122",
                  "02140")

countryData <- countryData[!CPCCode %in% live_animals]




seedData=subset(countryData, ElementCode == "5525")
seedData[, c("CountryM49","Country","Commodity","Element", "ElementCode") :=NULL]

setnames(seedData, c("Value","Flag"),c("seedData [t]","Flag seedData"))



areaSown=subset(countryData, ElementCode %in% c("5312", "5025"))
areaSown[, c("CountryM49","Country","Commodity", "ElementCode") :=NULL]


areaSown <- dcast.data.table(areaSown, CPCCode+ Year ~ Element, value.var = c("Value","Flag"))


areaSown[, `Value_Area Sown [ha]` := ifelse(is.na(`Value_Area Sown [ha]`), `Value_Area Harvested [ha]`,`Value_Area Sown [ha]`)]

areaSown[,`Flag_Area Sown [ha]` := ""]


setnames(areaSown, c("Value_Area Sown [ha]","Flag_Area Sown [ha]"),c("areaSown [ha]","Flag areaSown"))
areaSown[,c("Value_Area Harvested [ha]","Flag_Area Harvested [ha]") := NULL]

data=merge(seedData,areaSown, by=c("CPCCode","Year"), all=TRUE)

data[, `Seed Rate [Kg/ha]` := NA_real_]

data[is.na(`seedData [t]`), `seedData [t]` := 0]

data[, `Flag seedData` := ifelse(!is.na(`seedData [t]`), "", `Flag seedData`)]

data[, `Flag seedData` := ifelse(`seedData [t]` == 0, NA, `Flag seedData`)]

# for(j in 2010:2022){
#   if(j != 2022){
#     data[`Année` == j,]$`Seed Rate [Kg/ha]` = ifelse(is.na(data[`Année` == j,]$`Seed Rate [Kg/ha]`), 1000*(data[`Année` == j,]$`seedData [t]`)/(data[`Année` == j+1,]$`Superficie semée ha`),data[`Année` == j,]$`Seed Rate [Kg/ha]`)
#   } else {
#     data[`Année` == j,]$`Seed Rate [Kg/ha]` = ifelse(is.na(data[`Année` == j,]$`Seed Rate [Kg/ha]`), 1000*data[`Année` == j,]$`seedData [t]`/data[`Année` == j,]$`Superficie semée ha`,data[`Année` == j,]$`Seed Rate [Kg/ha]`)
#   }
# }

list_a <- split(data, by = "CPCCode")

data_to_bind <- list()

for (i in unique(data$CPCCode)){
  
  
  
  if ( all(is.na(list_a[[i]]$`Flag seedData`)) == FALSE){
    
    temp <- list_a[i]
    
    
    
    for(j in 2010:2022){
      if(j != 2022){
        
        temp[[i]][Year == j,`Seed Rate [Kg/ha]` := (1000*temp[[i]][Year == j]$`seedData [t]`)/temp[[i]][Year == j+1]$`areaSown [ha]`]
        
      } else {
        
        temp[[i]][Year == j,`Seed Rate [Kg/ha]` := (1000*temp[[i]][Year == j]$`seedData [t]`)/temp[[i]][Year == j]$`areaSown [ha]`]
      }
      
    }
    
    data_to_bind[i] = temp
    
  }
  
  else {
    
    print(i)
  }
  
  
}




data <- rbindlist(data_to_bind)
# for(j in 2010:2022){
#   if(j != 2022){
#     data[Year == j,]$`Seed Rate [Kg/ha]` = ifelse(is.na(data[Year == j,]$`Seed Rate [Kg/ha]`), 1000*(data[Year == j,]$`seedData [t]`)/(data[Year == j+1,]$`areaSown [ha]`),data[Year == j,]$`Seed Rate [Kg/ha]`)
#   } else {
#     data[Year == j,]$`Seed Rate [Kg/ha]` = ifelse(is.na(data[Year == j,]$`Seed Rate [Kg/ha]`), 1000*data[Year == j,]$`seedData [t]`/data[Year == j,]$`areaSown [ha]`,data[Year == j,]$`Seed Rate [Kg/ha]`)
#   }
# }


data=data[!is.na(`Seed Rate [Kg/ha]`)]


data=data[,c("CPCCode", "Year","Seed Rate [Kg/ha]" )]



averageRates=aggregate(`Seed Rate [Kg/ha]` ~ CPCCode,data = data,mean)

years=unique(seedData$Year)

seedRates<- as.data.table(expand.grid(CPCCode = unique(seedData$CPCCode), Year=as.character(min(years):max(years))))


seedRates=merge(seedRates,averageRates,by="CPCCode",all.x = T)



setnames(seedRates, "Seed Rate [Kg/ha]","Value")


seedRates[, ElementCode := "xxxx"]
seedRates[, Element := "Seed Rate [Kg/ha]"]
seedRates[, Flag := " "]




commodityName <- data.table(read_excel("Data/cpc2.1.xlsx"))



commodityName[,c("CONVERSION FACTORS\r\n(FCL-CPC)", "notes" ) :=NULL]

setnames(commodityName, c("SWS CODE","SWS DESCRIPTOR"), c("CPCCode","Commodity"))

commodityName[CPCCode == "21439.040000000001",CPCCode := "21439.04"]

commodityName <- commodityName[!duplicated(commodityName[,"CPCCode"])]


# commodityName=read_excel("Data/Reference File.xlsx", "SUA_Commodities")
# setDT(commodityName)

seedRates=merge(seedRates,commodityName, by= "CPCCode", all.x = TRUE)

setcolorder(seedRates, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))

seedRates[, Value := round(Value,0)]


seedRates[,Flag := ifelse(is.na(Value), NA, Flag)]

seedRates[,Flag := ifelse(is.nan(Value), NA, Flag)]
seedRates[,Value := ifelse(is.nan(Value), NA, Value)]

seedRates[,Flag := ifelse(Value == Inf, NA, Flag)]
seedRates[,Value := ifelse(Value == Inf, NA, Value)]

seedRates= wide_format(seedRates)




openxlsx::write.xlsx(seedRates, "Data/seedRate.xlsx",asTable = TRUE,1)


# Data/seedRate.xlsx


