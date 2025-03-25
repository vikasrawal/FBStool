imputeTourist=function(input,output,session){
  
  # rm(list = ls())

  
  # library(data.table)
  # library(readxl)
  # library(imputeTS)
  # 
  
  # upload food data
  # food = fread("Food/Data/finalFoodData.csv")
  food=subset(countryData, ElementCode == "5141")
  
  
  
  t=as.numeric(input$endyear)
  # t=2016
 country = as.character(input$Country)
 
 country= sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1)
 
 country=gsub(" ", "", country, fixed = TRUE)
 
 
 
 # country=sapply(strsplit(country, "\\|"), `[[`, 1)
 
  
  
  # country= "686"
  
  # sapply(strsplit(pp, "\\|"), `[[`, 1)
  
 if(is.null(country)){
   
   stop("Please enter the Country")
   
   
 } 
  
#  if (max(food$Year) != t){
#    
#     stop(paste("Please enter food for the year",t))
# } 
#   
  
food=food[ElementCode == "5141"]
food[,c("Commodity", "Flag", "ElementCode", "Element"):=NULL]
 
food[,Type:=NULL]
setnames(food,"Value", "Food [t]")
  
  

  # #create initial Tourist Data
  # initialTouristData = fread("Tourist Food/Data/initialTouristData.csv")
 

# upload calories Data
  # caloriesData=fread("Tourist Food/Data/caloriesData.csv")
caloriesData=read_excel("Data/caloriesData.xlsx")
setDT(caloriesData)


yearcols <- grep("^[[:digit:]]{4}$", names(caloriesData), value = TRUE)

caloriesData[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]

caloriesData= melt.data.table(caloriesData, id.vars = c("CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =yearcols,
                     value.name= "Value" )


setnames(caloriesData,"variable", "Year")

caloriesData[,Year:= as.character(Year)]

# 
# if (!(t %in% unique(caloriesData$Year))){
#   
#   stop(paste("Please enter Calories for the year",t))
# } 

caloriesData[,c("Commodity", "Element","ElementCode"):=NULL]
setnames(caloriesData, "Value", "Calories")
  
  
# uplaod population data
  popData=read_excel("Data/populationData.xlsx")
  
  setDT(popData)
  
  # if (length(popData$`Population [1000]`[popData$Year == t] )== 0){
  #   
  #   stop(paste("Please Enter the Popoulation for year",t))
  # }  
  
  
  
#Evaluate the DES for Sri Lanka

  popData[, Year :=as.character(Year)]
  
  
# we need to calculated the related country DES  
  
  CountryDES <- merge(food, popData, by = c("Year"),
                      all.x = TRUE )
  
  CountryDES<-merge(CountryDES, caloriesData, by=c("CPCCode","Year"),all.x = T)
  
  CountryDES[, `Population [1000]` :=as.numeric(`Population [1000]`)]
  
  CountryDES[, CaloriesPerCapitaperDay := (`Food [t]` * Calories * 10000) / 365 / (`Population [1000]` * 1000)]
  
  
  totDESCountry <- CountryDES[, list(totDESCountry = sum(CaloriesPerCapitaperDay, na.rm=T)),
                              by=list(Year)]
  
  
  totDESCountry[,CountryM49 := country]
  
  #load DES for other countries and rbind the Sri Lanka DES
  otherCountriestotDES=read_excel("Data/otherCountriestotDES.xlsx")
  setDT(otherCountriestotDES)
  
 otherCountriestotDES=otherCountriestotDES[!CountryM49 == country, ]
  
  
  
  yearcols_othercountries<- grep("^[[:digit:]]{4}$", names(otherCountriestotDES), value = TRUE)
  
  otherCountriestotDES[, (yearcols_othercountries) := lapply(.SD, as.numeric), .SDcols = yearcols_othercountries]
  
  otherCountriestotDES= melt.data.table(otherCountriestotDES, id.vars = c("CountryM49","Country"), 
                                        measure.vars =yearcols_othercountries,
                                value.name= "Value" )
  
  
  setnames(otherCountriestotDES,c("variable","Value"), c("Year","DES"))
  otherCountriestotDES[,DES:=as.numeric(DES)]
  
  
  # dd1=data.table("CountryM49" = "144", "Country"="Sri Lanka", "Year"="2010", "DES"="2447")
  # dd2=data.table("CountryM49" = "144", "Country"="Sri Lanka", "Year"="2011", "DES"="2462")
  # dd3=data.table("CountryM49" = "144", "Country"="Sri Lanka", "Year"="2012", "DES"="2473")
  # dd4=data.table("CountryM49" = "144", "Country"="Sri Lanka", "Year"="2013", "DES"="2475")
  # dd5=data.table("CountryM49" = "144", "Country"="Sri Lanka", "Year"="2014", "DES"="2441")
  # dd6=data.table("CountryM49" = "144", "Country"="Sri Lanka", "Year"="2015", "DES"="2507")
  # 
  # 
  # otherCountriestotDES=rbind(otherCountriestotDES,dd1,dd2,dd3,dd4,dd5,dd6)
  # 
  # otherCountriestotDES=otherCountriestotDES[CountryM49 != "686"]
  
  # write.csv(otherCountriestotDES,"Tourist Food/Data/otherCountriestotDES.csv",row.names=F)
  
  otherCountriestotDES[, Country:=NULL]
  setnames(totDESCountry, "totDESCountry","DES")
  
  DES=rbind(otherCountriestotDES,totDESCountry )
  
  DES[, baselineCalDay:= DES/2500]
  
  #Tourist Flow Data
  touristOvenightVisitors =read_excel("Data/touristOvenightVisitorsData.xlsx")
  setDT(touristOvenightVisitors)
  
  
  # 
  # overNight_otherYears = fread("tempory_tourist/touristFlowData.csv")
  # 
  # overNight_otherYears= overNight_otherYears[destinationCountryM49 =="1248" | originCountryM49 == "1248"]
  # 
  # overNight_otherYears= overNight_otherYears[timePointYears %in% c(2000:t)]
  # 
  # 
  # overNight_otherYears=merge(overNight_otherYears,countryName, by.x = "destinationCountryM49", by.y = "CountryM49" , all.x = TRUE)
  # 
  # setnames(overNight_otherYears, "Country","Destination Country")
  # 
  # overNight_otherYears=merge(overNight_otherYears,countryName, by.x = "originCountryM49", by.y = "CountryM49" , all.x = TRUE)
  # setnames(overNight_otherYears, "Country","Origin Country")
  # 
  # 
  # overNight_otherYears=merge(overNight_otherYears,elementName, by.x = "tourismElement", by.y = "ElementCode" , all.x = TRUE)
  # 
  # 
  # setnames(overNight_otherYears, c("timePointYears", "tourismElement", "originCountryM49","destinationCountryM49", "Value"),
  #          
  #          c("Year" , "ElementCode", "Origin CountryM49","Destination CountryM49", "Overnight Visitors [#]"))
  # 
  # touristOvenightVisitors=rbind(touristOvenightVisitors,overNight_otherYears)
  
  # touristOvenightVisitors= touristOvenightVisitors[!duplicated(touristOvenightVisitors),]
  
  
  touristOvenightVisitors[, `Origin CountryM49` :=as.character(`Origin CountryM49`)]
  touristOvenightVisitors[, `Destination CountryM49` :=as.character(`Destination CountryM49`)]
  touristOvenightVisitors[,ElementCode :=as.character(ElementCode)]
  
  
 
  
  
  # 
  # if (!(t %in% unique(touristOvenightVisitors$Year))){
  #   
  #   stop(paste("Please Enter information about tourist overnight visitors for year",t))
  #   
  # }
  
 
  
  touristOvenightVisitors[, c("Origin Country","Destination Country","Element", "ElementCode") := NULL]
  
  touristOvenightVisitors[!is.na(`Origin CountryM49`) & !is.na(`Destination CountryM49`), 
                          destOrigin := as.character(paste(`Destination CountryM49`, `Origin CountryM49`, sep = ","))]
  
  ## Filling the full time series
  timeSeriesOverNight<- as.data.table(expand.grid(destOrigin = unique(touristOvenightVisitors$destOrigin)[!is.na(unique(touristOvenightVisitors$destOrigin))],
                                                  Year = as.character(c(2010:t))))
  timeSeriesOverNight <- merge(timeSeriesOverNight,
                               touristOvenightVisitors[, c("destOrigin", "Destination CountryM49", "Origin CountryM49", "Year", "Overnight Visitors [#]"),
                                                       with = F], by = c("destOrigin", "Year"), all.x = T)
  
  timeSeriesOverNight <- timeSeriesOverNight[order(destOrigin, -as.numeric(Year))]
  
  timeSeriesOverNight[is.na(`Destination CountryM49`), `Destination CountryM49` := gsub(",.*$", "", destOrigin)]
  timeSeriesOverNight[is.na(`Origin CountryM49`), `Origin CountryM49` := sub('.*,\\s*', '', destOrigin)]
  
  
  ## impute missing overnigths data 
  tab <- timeSeriesOverNight[is.na(`Overnight Visitors [#]`), .N, destOrigin]
  setnames(tab, "N", "numbMissing")
  tab = tab[order(-numbMissing)]
  
  
  timeSeriesOverNight <- merge(timeSeriesOverNight,tab,by = "destOrigin",all.x = TRUE)
  
  timeSeriesOverNight[is.na(numbMissing), numbMissing := 0]
  
  timeSeriesOverNight[numbMissing < max(numbMissing),
                      interpolationSpline := na.ma(
                        `Overnight Visitors [#]`,weighting = "linear" ),by = list(destOrigin) ]
  
  
  
  
  
  
  # timeSeriesOverNight[numbMissing < max(numbMissing),
  #                     interpolationSpline := na.interpolation(
  #                       `OvernighVisitors [#]`, option ="spline"), by = list(destOrigin)]  
  # 
  # 
  # 
  # 
  
  
  
  
  
  timeSeriesOverNight[is.na(`Overnight Visitors [#]`), `Overnight Visitors [#]` := interpolationSpline]
  
  
  timeSeriesOverNight[, c("interpolationSpline",
                          "numbMissing", "destOrigin") := NULL]
  
  
  
  
  
  
  # upload statistics on tourism flows
  touristSummary=read_excel("Tourist Food/Data/touristSummaryData.xlsx")
  setDT(touristSummary)
  
  touristSummary[,CountryM49 :=as.character(CountryM49)]
  touristSummary[,ElementCode := as.character(ElementCode)]
  
  
  yearcols_tourist_summary <- grep("^[[:digit:]]{4}$", names(touristSummary), value = TRUE)
  
  touristSummary[, (yearcols_tourist_summary) := lapply(.SD, as.numeric), .SDcols = yearcols_tourist_summary]
  
  touristSummary= melt.data.table(touristSummary, id.vars = c("CountryM49", "Country","ElementCode" ,"Element"), measure.vars =yearcols_tourist_summary,
                                value.name= "Value" )
  
  
  setnames(touristSummary,"variable", "Year")
  
  
  touristSummary[,c("Country", "Element"):=NULL]
  setnames(touristSummary, "CountryM49", "Destination CountryM49")
  
  
  touristSummary <- as.data.table(dcast(touristSummary, `Destination CountryM49` + Year ~ ElementCode,value.var = "Value"))
  
  
  setnames(touristSummary, old = c("20", "30", "40"),
           new = c("averageNights", "sameDayVistNum", "averageDays"))
  
  touristSummary[!is.na(averageNights), averageNightsDays := averageNights]
  touristSummary[is.na(averageNights) & !is.na(averageDays), averageNightsDays := averageDays]
  touristSummary[, c("averageNights", "averageDays") := NULL]
  
  
  
  timeSerieSummaryData <- as.data.table(expand.grid(`Destination CountryM49` = unique(touristSummary$`Destination CountryM49`),
                                                    Year = as.character(c(2010:t))))
  
  timeSerieSummaryData <- merge(timeSerieSummaryData, touristSummary,
                                by = c("Destination CountryM49", "Year"), all.x = T)
  
  timeSerieSummaryData <- timeSerieSummaryData[order(`Destination CountryM49`, -as.numeric(Year))]
  
  
  
  
  #same day estimation
  auxTabSameDay <- timeSerieSummaryData[is.na(sameDayVistNum), .N, `Destination CountryM49`]
  
  
  setnames(auxTabSameDay, "N", "numbMissing")
  auxTabSameDay = auxTabSameDay[order(-numbMissing)]
  
  timeSerieSummaryData <- merge(timeSerieSummaryData, auxTabSameDay, by = "Destination CountryM49",all.x = TRUE)
  
  timeSerieSummaryData[numbMissing < as.numeric(length(2010:t)) ,
                       interpSplineSameDayVistNum := na.interpolation(
                         sameDayVistNum, option ="spline"),
                      ]
  
  
  #average days estiamtion
  auxTabAverageNightsDays <- timeSerieSummaryData[is.na(averageNightsDays), .N, `Destination CountryM49`]
  
  
  
  setnames(auxTabAverageNightsDays, "N", "numbMissing")
  auxTabAverageNightsDays = auxTabAverageNightsDays[order(-numbMissing)]
  
  timeSerieSummaryData <- merge(timeSerieSummaryData, auxTabAverageNightsDays, by = "Destination CountryM49",all.x = TRUE)
  
  timeSerieSummaryData[numbMissing.y < as.numeric(length(2010:t)),
                       interpSplineAverageNightsDays := na.interpolation(
                         averageNightsDays, option ="spline"), ]
  
  timeSerieSummaryData[, sameDayVistNum := ifelse(is.na(sameDayVistNum),interpSplineSameDayVistNum,sameDayVistNum )]
  timeSerieSummaryData[, averageNightsDays := ifelse(is.na(averageNightsDays),interpSplineAverageNightsDays,averageNightsDays )]
  
  
  timeSerieSummaryData[, c("interpSplineSameDayVistNum", "interpSplineAverageNightsDays",
                           "numbMissing.x", "numbMissing.y") := NULL]
  
  # setnames(timeSerieSummaryData, "interpSplineSameDayVistNum", "sameDayVistNum")
  # setnames(timeSerieSummaryData, "interpSplineAverageNightsDays", "averageNightsDays")
  
  
  ## Replace missing day visitor numbers (NA) with zero, because it won't effect
  ## end calculations, but NA's cause equations to fail
  timeSerieSummaryData$sameDayVistNum[is.na(timeSerieSummaryData$sameDayVistNum)] <- 0
  
  ## Merge the two data sets, one containing overnight visitor numbers and number
  ## of days they visited, the other data set the number of tourists travelling to
  ## and from each country
  touristData <- merge(timeSeriesOverNight, timeSerieSummaryData,
                       by=c("Destination CountryM49", "Year"), all.x = TRUE)
  
  ## rearrange the column order to make it easier to view
  touristData <- setcolorder(touristData,
                             neworder = c("Year", "Origin CountryM49", "Destination CountryM49", "Overnight Visitors [#]",
                                          "averageNightsDays", "sameDayVistNum"))
  
  touristData$sameDayVistNum[is.na(touristData$sameDayVistNum)] <- 0
  
  ## A small number of countries are missing values for "averageNightsDays" and
  ## this affects the bi-directional calculations for them, but also all of the
  ## other countries as well, so this imputes the missing number of days,
  ## by taking the mean of all day numbers present, grouped by year.
  touristData[, averageNightsDays := ifelse(
    is.na(averageNightsDays), mean(averageNightsDays, na.rm=TRUE),
    averageNightsDays), by = Year]
  
  ## Calculate the total number of tourist visitor days, the product of overnight
  ## visitor number and days per visit and adding the same day visitor numbers
  touristData[, `totVisitDays [#]` := `Overnight Visitors [#]` * averageNightsDays + sameDayVistNum]
  
  ## We can rid of these variables as we need the totVisitDays
  touristData[, c("Overnight Visitors [#]", "averageNightsDays", "sameDayVistNum") := NULL]
  
  ## We need to merge touristData with calorieCountryDay to calculate
  ## the consumption in each country. In our approach,
  ## the person comes from country A
  ## to country B eats the same food types as people in country B but eats the same
  ## calorie amounts as in their home country. In case we don't have totalCalDay for
  ## the country A, we will assume the same calories amount in country B.
  
  # Merge by orig country
  setnames(DES, "CountryM49", "Origin CountryM49")
  setkeyv(DES, c("Origin CountryM49", "Year"))
  
  setkeyv(touristData, c("Origin CountryM49", "Destination CountryM49", "Year"))
  
  keys = c("Origin CountryM49", "Year")
  touristData <- merge(touristData, DES, by = keys, all.x = T)
  
  setnames(touristData, old="baselineCalDay", new="baselineCalDayOrigCountry")
  touristData[, DES := NULL]
  
  # Merge by dest country
  setnames(DES, "Origin CountryM49", "Destination CountryM49")
  
  setkeyv(DES, c("Destination CountryM49", "Year"))
  
  setkeyv(touristData, c("Origin CountryM49", "Destination CountryM49", "Year"))
  keys = c("Destination CountryM49", "Year")
  touristData <- merge(touristData, DES, by = keys, all.x = T)
  
  setnames(touristData, old = "baselineCalDay", new = "baselineCalDayDestCountry")
  touristData[, DES := NULL]
  
  touristData[, baselineCalDayOrigCountry := ifelse(
    is.na(baselineCalDayOrigCountry), baselineCalDayDestCountry, baselineCalDayOrigCountry)]
  
  ## Now, we'll use only the baselineCalDayOrigCountry
  touristData[, baselineCalDayDestCountry := NULL]
  
  ## There are combinations ofcountries (origin-destination that we don't have the
  ## amount of calories consumed, so we are going to compute the average for those cases.
  
  
  averageCalTab <- touristData[, list(averageCal = mean(
    baselineCalDayOrigCountry, na.rm = T)),
    by = list(`Origin CountryM49`, Year)]
  
  keys = c("Origin CountryM49", "Year")
  touristData <- merge(touristData, averageCalTab, by = keys, all.x = T)
  touristData[is.na(baselineCalDayOrigCountry), baselineCalDayOrigCountry := averageCal]
  touristData[, "averageCal" := NULL]
  
  ## Calculate total visitor days per year who leaves your country using the baseline
  daysOut <- touristData[, list(daysOut = sum(`totVisitDays [#]` * baselineCalDayOrigCountry, na.rm=T)),
                         by = c('Origin CountryM49', 'Year')]
  
  ## Calculate total visitor days per year who comes to the country
  daysIn <- touristData[, list(daysIn = sum(`totVisitDays [#]` * baselineCalDayOrigCountry, na.rm=T)),
                        by = c('Destination CountryM49', 'Year')]
  
  ## change column name from "orig" to "country"
  setnames(daysOut, 'Origin CountryM49', 'CountryM49')
  
  ## change column name from "dest" to "country"
  setnames(daysIn, 'Destination CountryM49', 'CountryM49')
  
  ##-------------
  ## tourist data
  ##-------------
  
  ## merge daysOut and daysIn to allow calculation of days net per country and year
  touristDaysData <- merge(daysOut, daysIn, by = c("CountryM49", "Year"), all.x = TRUE)
  touristDaysData[, daysNet := daysIn - daysOut]
  
  
  
  
  
  touristDaysData=touristDaysData[CountryM49 == country]
  
  touristDaysData[,CountryM49 :=NULL]
  ## Merge tourismDays with the food consumption data
  setkey(touristDaysData, "Year")
  
  setkey(CountryDES, "Year")
  
  finaltouristData <- merge(CountryDES, touristDaysData,
                            by = c("Year"), all.x = T)
  
  finaltouristData[, calOutCountry := daysOut * CaloriesPerCapitaperDay]
  finaltouristData[, calInCountry := daysIn * CaloriesPerCapitaperDay]
  finaltouristData[, calNetCountry := daysNet * CaloriesPerCapitaperDay]
  finaltouristData[, netFood := calNetCountry/(Calories * 10000)]
  
  
  ## Get rid of some of the columns that we don't need anymore:
  finaltouristData[, c("Food [t]", "Population [1000]", "Calories", "CaloriesPerCapitaperDay",
                       "daysOut", "daysIn", "daysNet", "calOutCountry",
                       "calInCountry", "calNetCountry") := NULL]
  
  ## Net calories
  finaltouristData[, ElementCode:= "5164"]
  finaltouristData[, Flag:= "I"]
  setnames(finaltouristData,"netFood", "Value")
  finaltouristData <- finaltouristData[!is.na(Value)]
  
  
  commodityName = read_excel("Data/Reference File.xlsx",sheet = "SUA_Commodities") 
  commodityName = data.table(commodityName)
  
  elementName = read_excel("Data/Reference File.xlsx",sheet = "Elements")
  elementName = data.table(elementName)
  
  
  # countrycode= read_excel("Reference Files/Reference File.xlsx",sheet = "Country") 
  # countrycode = data.table(countrycode)
  # countrycode[,CountryM49 := as.character(CountryM49)]
  
  finaltouristData=merge(finaltouristData,commodityName, by="CPCCode", all.x = T)
  
  finaltouristData=merge(finaltouristData, elementName, by="ElementCode", all.x=T)
  
  
  # finaltouristData=merge(finaltouristData, countrycode, by="CountryM49", all.x=T)
  
  finaltouristData[,c("CountryM49","Country") := NULL]
  
  setcolorder(finaltouristData, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
  finaltouristData = finaltouristData[order(CPCCode,Year)]
  
  write.csv(finaltouristData,"Tourist Food/Data/finalTouristData.csv", row.names = F)
  
  finaltouristData[, Year :=as.character(Year)]
  finaltouristData[, Year :=as.numeric(Year)]
  
  finaltouristData=finaltouristData [Year > 2013]
  finaltouristData

  
  

return(finaltouristData)
  

  
}