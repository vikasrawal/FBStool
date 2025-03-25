rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(stringr)
library(readxl)
library(readr)




## using the functions in SWS-Methodology/faoswsTrade -> https://github.com/SWS-Methodology/faoswsTrade/R/
## (remove flog. and rprt_ stuff)
# - extract_hs6fclmap
# - convertComtradeM49ToFAO (dependency: "m49faomap.csv")
# - mapHS6toFCL
# - mapHS2FCL (dependencies: maxHSLength(), hsInRange(), sel1FCL()) 
# - add_fcls_from_links

# load functions
sapply(list.files(path="Trade/R", full.names=TRUE), source)

  
# load mapping table from hs6 to fcl for all countries 
hs6fclmapALL = fread("Trade/Data/hs6fclmapALL.csv")

############################################################################################################################################
# trade data: export (2010-2015)

exportData = read_excel("Trade/Data/Export_SN.xlsx")
exportData = data.table(exportData)
exportData[, `PRODUIT \\ Indicators` := NULL]
colnames(exportData) = c("SH6", "2010", "2011", "2012", "2013", "2014", "2015")

# select trade chapters
hs_chapters <- c(sprintf("%02d", 01:24), 33, 35, 38, 40:41, 43, 50:53)
exportData$HSChapters = substring(exportData$SH6,1,2)
exportData = exportData[HSChapters %in% hs_chapters,]
# str(exportData)

exportData = melt.data.table(data = exportData, id=c("SH6","HSChapters"))

exportData[,value := as.numeric(value)]
exportData[is.na(value), value := 0]
exportData[,value := round(value/1000,0)]

colnames(exportData) = c("HSCode","HSChapters","Year","Value")

exportData[, Element := "Export Quantity [t]"]
exportData[, ElementCode := 5910]

setcolorder(exportData,c("HSChapters","HSCode","ElementCode","Element","Year","Value"))

############################################################################################################################################
# trade data: import (2010-2015)

importData = read_excel("Trade/Data/Import_SN.xlsx")
importData = data.table(importData)
importData[, `PRODUIT \\ Indicators` := NULL]
colnames(importData) = c("SH6", "2010", "2011", "2012", "2013", "2014", "2015")

# select trade chapters
hs_chapters <- c(sprintf("%02d", 01:24), 33, 35, 38, 40:41, 43, 50:53)
importData$HSChapters = substring(importData$SH6,1,2)
importData = importData[HSChapters %in% hs_chapters,]
# str(importData)

importData = melt.data.table(data = importData, id=c("SH6","HSChapters"))

importData[,value := as.numeric(value)]
importData[is.na(value), value := 0]
importData[,value := round(value/1000,0)]

colnames(importData) = c("HSCode","HSChapters","Year","Value")

importData[, Element := "Import Quantity [t]"]
importData[, ElementCode := 5610]

setcolorder(importData,c("HSChapters","HSCode","ElementCode","Element","Year","Value"))

##############################################################################################################################################################################
## Trade Data

# dim(importData)
# dim(exportData)
# 
# importData[1:3,]
# exportData[1:3,]

tradeData = as.data.frame(rbind(importData,exportData))
# head(tradeData)
# tradeData[duplicated(tradeData), ]
# filter(tradeData,HSCode == "090210" & Element == "Export Quantity [t]")
# filter(tradeData,HSCode == "090220" & Element == "Export Quantity [t]")
# filter(tradeData,HSCode == "090230" & Element == "Export Quantity [t]")
# filter(tradeData,HSCode == "090240" & Element == "Export Quantity [t]")

# str(tradeData)
# str(hs6fclmapALL)

tradeData$Year = as.numeric(levels(tradeData$Year)[tradeData$Year])

names(hs6fclmapALL)[2] = "HSCode"

tradeDataFCL = merge(tradeData,hs6fclmapALL,
                  by = c("HSCode","Year"),
                  all.x = TRUE)
tradeDataFCL = tradeDataFCL[!duplicated(tradeDataFCL), ]


fcl2cpc <- fread("Trade/Data/fcl_2_cpc.csv")
fcl2cpc[, fcl := addHeadingsFCL(fcl2cpc$fcl)]


tradeDataCPC = merge(tradeDataFCL,fcl2cpc,
                      by = "fcl",
                      all.x = TRUE)

m49faomap <- fread("Trade/Data/m49faomap.csv")[,-1]

tradeDataCPCM49 = merge(tradeDataCPC,m49faomap,
                      by.x = "reporter",
                      by.y = "fao",
                      all.x = TRUE)


M49CodeDescription <- fread("Trade/Data/M49CodeDescription.csv")[,-1]


colnames(M49CodeDescription) = c("m49","Country")

tradeDataCPCM49 = merge(tradeDataCPCM49,M49CodeDescription,
                         all.x = TRUE)

commodityNames = read_excel("Reference Files/Reference File.xlsx", sheet = "SUA_Commodities")
elementNames = read_excel("Reference Files/Reference File.xlsx", sheet = "Elements")

tradeDataCPCM49 = merge(tradeDataCPCM49,commodityNames,
                        by.x = "cpc",
                        by.y = "CPCCode",
                        all.y = TRUE)


# tradeDataCPCM49[duplicated(tradeDataCPCM49), ]

tradeDataCPCM49 = data.table(tradeDataCPCM49)
tradeDataCPCM49 = select(tradeDataCPCM49,c(m49,Country,HSCode,cpc,Commodity,ElementCode,Element,Year,Value))
# head(tradeDataCPCM49)

colnames(tradeDataCPCM49) = c("CountryM49","Country","HSCode","CPCCode","Commodity","ElementCode","Element", "Year","Value")
# head(tradeDataCPCM49)
# dim(tradeDataCPCM49)

# Trade Data Validation using the Unit Value
# test = filter(tradeDataCPCM49,  HS6Code == "110313" & ElementCode == 5930)
# plot(test$Year, test$Value)
# write.csv(tradeDataCPCM49, "C:/Users/Siriwardena/Documents/GitLab/compiler/my-food_balance_sheet_tool/Data/trade_data.csv")
tradeDataCPCM49 = na.omit(tradeDataCPCM49)
tradeDataCPCM49 = data.table(tradeDataCPCM49)
tradeDataCPCM49[,Value := ifelse(Value == "Inf",NA,Value)]
tradeDataCPCM49 =subset(tradeDataCPCM49 , Element %in% c("Import Quantity [t]", "Export Quantity [t]"))

tradeDataCPCM49 <- tradeDataCPCM49[, list(val_sum=sum(Value)), 
                                    by=c("Year","CPCCode","ElementCode","CountryM49", "Country" )]


tradeDataCPCM49<- merge.data.frame(commodityNames,tradeDataCPCM49,by.x="CPCCode",by.y="CPCCode",all.y=TRUE)

tradeDataCPCM49<- merge.data.frame(elementNames,tradeDataCPCM49,by.x="ElementCode",by.y="ElementCode",all.y=TRUE)

# load sheet in reference file
# merce CPCCode toHSCode
# commodityTradeName = read_excel("Reference Files/Reference File.xlsx",sheet = "Trade_Commodities") 
# commodityTradeName = data.table(commodityTradeName)
# 
# tradeDataCPCM49 = merge(tradeDataCPCM49, commodityTradeName,
#                         by = c("CPCCode","Commodity"),
#                         all.x = TRUE)
# setcolorder(tradeDataCPCM49, c("CountryM49", "Country", "HSCode", "CPCCode","Commodity","ElementCode","Element", "Year","val_sum"))

setcolorder(tradeDataCPCM49, c("CountryM49", "Country", "CPCCode","Commodity","ElementCode","Element", "Year","val_sum"))

tradeDataCPCM49=data.table(tradeDataCPCM49)
setnames(tradeDataCPCM49,"val_sum", "Value")
tradeDataCPCM49[,Flag:=""]

# tradeDataCPCM49[duplicated(tradeDataCPCM49), ]

write.csv(tradeDataCPCM49, "Trade/Data/finalTradeData.csv", row.names = FALSE)

# test
# filter(tradeDataCPCM49, CPCCode == "01620")
# filter(tradeDataCPCM49, CPCCode == "01930.04")
