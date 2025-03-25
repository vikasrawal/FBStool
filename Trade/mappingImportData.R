
mappingImportData <- function(session, input, output){
  
  
countryImportData= read_excel("Data/countryImportData.xls")
setDT(countryImportData)

Year="YEAR"
Value="QTYTHO_sum"
HS6 = "H6 Code"

countryImportData=select(countryImportData,c(Year, Value,HS6 ))
  
setnames(countryImportData, names(countryImportData), c("Year","Value","HS6"))
  
  
  files = dir("Trade/R",full.names = TRUE)
  #sapply(files, source)
  for(i in files){
    source(i, local = TRUE)
  }
  
  
  
  
  
  # load mapping table from hs6 to fcl for all countries 
  hs6fclmapALL = fread("Trade/Data/hs6fclmapALL.csv")
  
  ############################################################################################################################################
  # trade data: export (2010-2015)
  
  # exportData = read_excel("Trade/Data/Export_SN.xlsx")
  # exportData = data.table(exportData)
  # 
  # exportData=fread("Trade/Data/ex.csv")
  # 
  # 
  # 
  # exportData[, `PRODUIT \\ Indicators` := NULL]
  # colnames(exportData) = c("SH6", "2010", "2011", "2012", "2013", "2014", "2015")
  
  # select trade chapters
  hs_chapters <- c(sprintf("%02d", 01:24), 33, 35, 38, 40:41, 43, 50:53)
  countryImportData$HSChapters = substring(countryImportData$HS6,1,2)
  countryImportData = countryImportData[HSChapters %in% hs_chapters,]
  # str(exportData)
  
  # countryImportData = melt.data.table(data = countryImportData, id=c("HS6","HSChapters"))
  
  countryImportData[,Value := as.numeric(Value)]
  countryImportData[is.na(Value), Value := 0]
  
  #If data is not in tons 
  # exportData[,value := round(value/1000,0)]
  
  # colnames(exportData) = c("HSCode","HSChapters","Year","Value")
  
  countryImportData[, Element := "Import Quantity [t]"]
  countryImportData[, ElementCode := 5610]
  
  setcolorder(countryImportData,c("HSChapters","HS6","ElementCode","Element","Year","Value"))
  

  
  
  # countryImportData$Year = as.numeric(levels(countryImportData$Year)[countryImportData$Year])
  
  names(hs6fclmapALL)[2] = "HS6"
  
  
  
  tradeDataFCL = merge(countryImportData,hs6fclmapALL,
                       by = c("HS6","Year"),
                       all.x = TRUE)
  
  
  tradeDataFCL = tradeDataFCL[!duplicated(tradeDataFCL), ]
  
  
  # standardCode <- read_excel("Trade/Data/standard_codes.xlsx")
  # 
  # setDT(standardCode)
  # 
  # standardCode <- standardCode[, c("hs2012_code","faostat_code")]
  # 
  # standardCode[, hs2012_code := as.character(hs2012_code)]
  
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
  
  
  ###merging with HSC codes
  
  hscCode=read_excel("Reference Files/Reference File.xlsx",sheet = "Trade_Commodities")
  hscCode = data.table(hscCode)
  
  
  tradeDataCPCM49=merge(tradeDataCPCM49,hscCode, by="CPCCode", all.x = T)
  
  
  setcolorder(tradeDataCPCM49,c("CountryM49","Country","HS6 standard code","CPCCode","Commodity","ElementCode","Element",
                                "Year","Value","Flag"))
  
  setnames(tradeDataCPCM49,"HS6 standard code","HS6 Standard Code")
  
  
  write.csv(tradeDataCPCM49, "Trade/Data/finalTradeData.csv", row.names = FALSE)
  
  # test
  # filter(tradeDataCPCM49, CPCCode == "01620")
  # filter(tradeDataCPCM49, CPCCode == "01930.04")
  
  
}


