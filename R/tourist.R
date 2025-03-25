tourist=function(input,output,session){
  
  # List of food commodities
  
  output$touristCommodities=renderRHandsontable({
    
    if (is.null(input$insertTourist)){
      
      DATA=itemFood
    }
    else {
      
      DATA= itemRow()
      
      
    }
    
    
    
    
    rhandsontable(DATA)
  })
  
  
  
  observeEvent(input$fileTourist,{
    
    inFile <- input$fileTourist
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df_tourist$data_tourist=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    
  })
  
  
  output$downloadTourist<- downloadHandler(
    
    
    
    
    filename = function() {
      
      "table.xlsx"
    },
    
    
    content = function(file) {
      
      write.xlsx(data.table(hot_to_r(input$touristTable)) ,file,row.names = FALSE)
    }
    
  )
  
  
  
  
  
  
  
  
  
  #Calory Conversion Table
 
CaloryTable=reactive({
  
  END_YEAR=input$endyear
  
  data=read_excel("Data/caloriesData.xlsx")
  
  setDT(data)
  
  yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
  
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  
  if(minyear > END_YEAR){
    stop("End year cannot be before all years in data")
  }
  
  
  if(END_YEAR > maxyear +1){
    
    stop("End year should be one year more than the data")
    
  }
  
  desired_yearset <- minyear:END_YEAR
  
  final_data <- if(maxyear == END_YEAR){
    
    data[]
    
  } else if(maxyear > END_YEAR){
    
    tempdata <- copy(data)
    delyears <- as.character((as.numeric(END_YEAR) + 1):maxyear)
     
    
    tempdata[, c(delyears) := NULL]
    tempdata[]
    
  } else if(maxyear < END_YEAR){
    
    tempdata <- copy(data)
    # Get names for column ordering later
    prevnames <- names(data)
    
    addyears <- as.character(END_YEAR)
    
    
    #order columns year then flag
    addorder <- as.vector(rbind(addyears))
    
    tempdata[, (addyears) := NA_real_]
    
    
    setcolorder(tempdata, c(prevnames, addorder))
    
    tempdata[]
    
  }
  
  final_data=final_data[order(CPCCode)]

  
}) 
  
   
  
  output$touristcalory=renderRHandsontable({
    
    
    rhandsontable(CaloryTable(),
                  undo = T, redo = T, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,
                  selectCallback = TRUE, fontweight= "bold",search=TRUE)%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
      
      hot_col("CPCCode", halign = "htRight", width= 100)%>%
      hot_col("ElementCode", halign = "htRight", width= 100)%>%
      hot_col("Element", halign = "htLeft", width= 200)%>%
      hot_cols(format = '0,0')%>%
      hot_col("Commodity", width = 250)
  })
  
  #Population data
  
  
  output$touristpop=renderRHandsontable({
    rhandsontable(populationData)%>%
      hot_cols(format = '0,0')%>%
      hot_col("CountryM49", halign = "htRight")
  })
  
  
overnightV=reactive({
  
  # touristovernightData=read_excel("Data/touristOvenightVisitorsData.xlsx")
  # 
  # setDT(touristovernightData)
  # 
  
  
 
  
  END_YEAR=input$endyear
  
  
  touristovernightData=read_excel("Data/overnight_all_countries.xlsx")
  
  setDT(touristovernightData)
  
 
 setnames(touristovernightData,c("tourismElement" ,"originCountryM49" ,"destinationCountryM49" ,"timePointYears", "Value", "Destination", "Origin" ,              
 "Tourism Element"), c("ElementCode","Origin CountryM49","Destination CountryM49","Year","Overnight Visitors [#]","Destination Country", "Origin Country"
                       ,"Element") ) 
  
  
  # names(touristovernightData)=c("Destination CountryM49","Destination Country","Origin CountryM49","Origin Country","ElementCode", "Element", "Year", "Overnight Visitors [#]")


  touristCode= sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1)
  
  touristCode=gsub(" ", "", touristCode, fixed = TRUE)

  touristovernightData=touristovernightData[`Destination CountryM49` == touristCode |`Origin CountryM49` == touristCode]
  
  
  
  
  
  # touristovernightData=touristovernightData[`Destination CountryM49` == 204|`Origin CountryM49` == 204]
  
  setcolorder(touristovernightData,c("Origin CountryM49","Origin Country","Destination CountryM49","Destination Country","ElementCode","Element","Year","Overnight Visitors [#]"))
  
  
  # write.xlsx(touristovernightData,"Data/touristOvenightVisitorsData.xlsx",row.names = FALSE)
  
  
  
  
  
  touristovernightData$`Origin CountryM49` = as.character(touristovernightData$`Origin CountryM49`)
  touristovernightData$`Origin Country` = as.character(touristovernightData$`Origin Country`)
  
  
  touristovernightData$`Destination CountryM49` = as.character(touristovernightData$`Destination CountryM49`)
  touristovernightData$`Destination Country` = as.character(touristovernightData$`Destination Country`)
  
  setDT(touristovernightData)
  
  touristovernightData[, ElementCode := as.character(ElementCode)]
  touristovernightData[, Element := as.character(Element)]
  touristovernightData[, Year := as.character(Year)]
  touristovernightData[, `Overnight Visitors [#]` := as.character(`Overnight Visitors [#]`)]
    
data= touristovernightData 


data[, Year := as.character(Year)]


data=dcast.data.table(data, `Origin CountryM49`+`Origin Country`
                      +`Destination CountryM49`+`Destination Country` + `ElementCode` + `Element` ~ Year, value.var = c("Overnight Visitors [#]"))

yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)

data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    
yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)

minyear <- min(as.numeric(yearcols))
maxyear <- max(as.numeric(yearcols))

# Convert types

data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]

yearErrors(END_YEAR, minyear, maxyear)
desired_yearset <- minyear:END_YEAR

final_data <- if(maxyear == END_YEAR){
  
  data[]
  
} else if(maxyear > END_YEAR){
  
  tempdata <- copy(data)
  delyears <- as.character((as.numeric(END_YEAR) + 1):maxyear)
  # delflags <- paste("Flag", delyears)  
  
  tempdata[, c(delyears) := NULL]
  tempdata[]
  
} else if(maxyear < END_YEAR){
  
  tempdata <- copy(data)
  # Get names for column ordering later
  prevnames <- names(data)
  
  addyears <- as.character(END_YEAR)
  # addflags <- paste("Flag", addyears)  
  
  #order columns year then flag
  addorder <- as.vector(rbind(addyears))
  
  tempdata[, (addyears) := NA_real_]
  # tempdata[, (addflags) := NA_character_]
  
  setcolorder(tempdata, c(prevnames, addorder))
  
  tempdata[]
  
}


final_data





})   
  
  

# observe(
#   
#   if (input$endyear %in% c(2010:2013)){
#     
#     sendSweetAlert(
#       session = session,
#       title = "No data available for tourist food !!",
#       text = paste("There is no data availabale for ", input$endyear),
#       type = "warning"
#     )
#     
#   }
#   
# )



  
  
  #Tourist Overnight Visits
  
  
  output$touristovernight=renderRHandsontable({
    
    
    rhandsontable(overnightV(),
                  height = 400,undo = T, redo = T,
                  useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
                  , selectCallback = TRUE, fontweight= "bold",search= TRUE 
                   )%>%
      
      hot_col("Origin CountryM49", width=150,halign = "htRight")%>%
      hot_col("Destination CountryM49", width=100,halign = "htRight")%>%
      hot_col("ElementCode", halign = "htRight")%>%
      hot_col("Origin Country",width=150)%>%
      hot_col("Destination Country",width=150, halign="htLeft")%>%
      hot_col("Element",width=200, halign="htLeft")%>%
      hot_col("ElementCode",width=100, halign="htRight")%>%
      hot_cols(format = '0,0')%>%
      hot_cols(fixedColumnsLeft = 6)%>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE)
    
      
    
  })
  
  
  #Tourist Summary
touristsummary=reactive({
  
  END_YEAR=input$endyear
  
  data = read_excel("Data/touristSummaryData.xlsx")
  setDT(data)
  
  data[,CountryM49 := as.character(CountryM49)]

  
  yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
  
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  
  if(minyear > END_YEAR){
    stop("End year cannot be before all years in data")
  }
  
  
  if(END_YEAR > maxyear +1){
    
    stop("End year should be one year more than the data")
    
  }
  
  desired_yearset <- minyear:END_YEAR
  
  final_data <- if(maxyear == END_YEAR){
    
    data[]
    
  } else if(maxyear > END_YEAR){
    
    tempdata <- copy(data)
    delyears <- as.character((as.numeric(END_YEAR) + 1):maxyear)
    
    
    tempdata[, c(delyears) := NULL]
    tempdata[]
    
  } else if(maxyear < END_YEAR){
    
    tempdata <- copy(data)
    # Get names for column ordering later
    prevnames <- names(data)
    
    addyears <- as.character(END_YEAR)
    
    
    #order columns year then flag
    addorder <- as.vector(rbind(addyears))
    
    tempdata[, (addyears) := NA_real_]
    
    
    setcolorder(tempdata, c(prevnames, addorder))
    
    tempdata[]
    
  }
  
  final_data
  
  
})   



  

output$touristsummary=renderRHandsontable({
    
    
    rhandsontable(touristsummary(),
                  undo = T, redo = T,height = 400, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,
                  selectCallback = TRUE, fontweight= "bold",search=TRUE)%>%
      hot_cols(format = '0,0')%>%
      hot_col("CountryM49", halign = "htRight", width=100)%>%
      hot_col("Country", halign = "htLeft", width= 300)%>%
      hot_col("Element", halign = "htLeft", width= 350)%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
      hot_col("ElementCode", halign = "htRight", width =100)
    
    
  })
  
  #Tourist DES
  
touristDESDataReactive=reactive({
  
  END_YEAR=input$endyear
  
  data=read_excel("Data/otherCountriestotDES.xlsx")
  setDT(data)
    
  
  
  
  yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
  
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  
  if(minyear > END_YEAR){
    stop("End year cannot be before all years in data")
  }
  
  
  if(END_YEAR > maxyear +1){
    
    stop("End year should be one year more than the data")
    
  }
  
  desired_yearset <- minyear:END_YEAR
  
  final_data <- if(maxyear == END_YEAR){
    
    data[]
    
  } else if(maxyear > END_YEAR){
    
    tempdata <- copy(data)
    delyears <- as.character((as.numeric(END_YEAR) + 1):maxyear)
    
    
    tempdata[, c(delyears) := NULL]
    tempdata[]
    
  } else if(maxyear < END_YEAR){
    
    tempdata <- copy(data)
    # Get names for column ordering later
    prevnames <- names(data)
    
    addyears <- as.character(END_YEAR)
    
    
    #order columns year then flag
    addorder <- as.vector(rbind(addyears))
    
    tempdata[, (addyears) := NA_real_]
    
    
    setcolorder(tempdata, c(prevnames, addorder))
    
    tempdata[]
    
  }
  
  final_data
  
    
    
    
  })       
  
  
  
  
  
  
  
  output$touristDES=renderRHandsontable({
    
    
    rhandsontable(touristDESDataReactive(), 
                  undo = T, redo = T, height = 400,useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,
                  selectCallback = TRUE, fontweight= "bold",search=TRUE)%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
      hot_cols(format = '0,0')%>%
      hot_col("CountryM49",width= 150, halign = "htRight")%>%
      hot_col("Country",width= 150, halign = "htLeft")
    
    
  })
  
  
  


  
  
  commodityTouristReactive=reactive({
    
    data=hot_to_r(input$touristCommodities) 
    data
  })
  
  
  
  #save modifications to food commodities
  
  observeEvent(input$touristcommoditySave,{
    
  write.csv(commodityTouristReactive(),"Food/Data/itemFood.csv", row.names=F)
    
  })
  
  
  
  populationDataReactive=reactive({
    
    data=hot_to_r(input$touristpop) 
    data
  })
  
  
  
  #save modifications to food commodities
  
  # observeEvent(input$touristpopSave,{
  #   
  #   write.csv(populationDataReactive(),"Food/Data/populationData.csv", row.names=F)
  #   
  # })
  
  
  #long format calory
  
  longformat_calory=reactive({
    data= data.table(hot_to_r(input$touristcalory))
    
    yyy= melt.data.table(data, id.vars = c("CountryM49","Country", "CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =c( "2010","2011","2012","2013",  "2014", "2015"),
                         value.name= "Value" )
    setnames(yyy,"variable", "Year")
    
    yyy[,Year:= as.character(Year)]
    # xxx= melt.data.table(data, id.vars = c("CountryM49","Country", "CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =c( "Flag 2010","Flag 2011", "Flag 2012" ,"Flag 2013", "Flag 2014","Flag 2015"),
    #                      value.name= "Flag" )
    
    # setnames(xxx,"variable", "Year")
    # xxx[,Year:=substring(Year,6)]
    # xxx[,Year:=as.character(Year)]
    # 
    # kk=merge(yyy, xxx, by = intersect(names(yyy), names(xxx)))  
    
    yyy
    
  })  
  
  
  

  
  #save modifications to calory data
  
  observeEvent(input$touristcalorySave,{
    
    
    data=hot_to_r(input$touristcalory)
    
    setDT(data)
    
    flagcols <- grep("^Flag", names(data), value = TRUE)
    yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    
    
    
    data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    
    write.xlsx(data,"Data/caloriesData.xlsx", row.names=F)
    
  })
  
  
  #overnight table
  
  longformat_overnight=reactive({
    data= data.table(hot_to_r(input$touristovernight))
    
    yyy= melt.data.table(data, id.vars = c("Origin CountryM49","Origin Country", "Destination CountryM49","Destination Country",
                                           
                                           "ElementCode" ,"Element"), measure.vars =c( "2010","2011","2012","2013",  "2014"),
                         value.name= "Value" )
    setnames(yyy,"variable", "Year")
    setnames(yyy,"Value","OvernighVisitors [#]")
    
    yyy[,Year:= as.character(Year)]
    # xxx= melt.data.table(data, id.vars = c("CountryM49","Country", "CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =c( "Flag 2010","Flag 2011", "Flag 2012" ,"Flag 2013", "Flag 2014","Flag 2015"),
    #                      value.name= "Flag" )
    
    # setnames(xxx,"variable", "Year")
    # xxx[,Year:=substring(Year,6)]
    # xxx[,Year:=as.character(Year)]
    # 
    # kk=merge(yyy, xxx, by = intersect(names(yyy), names(xxx)))  
    
    yyy
    
  })  
  
  
  
  
  
  #save modifications to overnight data
  
  observeEvent(input$touristovernightSave,{
    
    # write.csv(hot_to_r(input$touristovernight), "overNight.csv", row.names = FALSE)
    
    
    data=hot_to_r(input$touristovernight)

   
    
    
    yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)



    data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]


    hh=melt.data.table(data, id.vars = c("Origin CountryM49","Origin Country", "Destination CountryM49","Destination Country",

                                         "ElementCode" ,"Element"), measure.vars =yearcols,
                       value.name= "Value" )



    setnames(hh, "variable", "Year")
    setnames(hh, "Value","Overnight Visitors [#]" )


    hh[, Year := as.character(Year)]

    data2Add = copy(hh)

    touristovernightDataAll=read_excel("Data/overnight_all_countries.xlsx")
    setDT(touristovernightDataAll)

    setnames(touristovernightDataAll, "timePointYears", "Year")
    
    
    timeSeriesOverNight<- as.data.table(expand.grid(destinationCountryM49 = unique(touristovernightDataAll$destinationCountryM49),originCountryM49
                                                    = unique(touristovernightDataAll$originCountryM49),

                                                    Year = as.character(c(2010:input$endyear))))

    timeSeriesOverNight <- merge(timeSeriesOverNight,
                                 touristovernightDataAll, by = c("destinationCountryM49", "originCountryM49","Year"), all.x = T)

    country = as.character(input$Country)

    country= sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1)

    country=gsub(" ", "", country, fixed = TRUE)


    setnames(data2Add, c("Origin CountryM49","Origin Country","Destination CountryM49","Destination Country","ElementCode","Element","Year","Overnight Visitors [#]"),
             c("originCountryM49","Origin","destinationCountryM49","Destination","tourismElement","Tourism Element","Year","Value"))




    data2Remove = timeSeriesOverNight[destinationCountryM49 == country | originCountryM49 == country]


    timeSeriesOverNight = timeSeriesOverNight[!data2Remove]

    timeSeriesOverNight=timeSeriesOverNight[,c("originCountryM49", "destinationCountryM49", "Year"
                                               , "Value" )]



    data2Add=data2Add[,c("originCountryM49", "destinationCountryM49", "Year","Value" )]

    # data2Add[, `[flagObservationStatus] Status` := NA]
    # 
    # data2Add[, `[flagMethod] Method` := "h"]



    timeSeriesOverNight=timeSeriesOverNight[originCountryM49 != destinationCountryM49]

    timeSeriesOverNight=timeSeriesOverNight[!is.na(Value)]


    timeSeriesOverNight=rbind(timeSeriesOverNight, data2Add)


    # timeSeriesOverNight[, timePointYears := Year]
    timeSeriesOverNight[, tourismElement := "60"]
    timeSeriesOverNight[, `Tourism Element` := "Arrivals of non-resident tourists at national borders [ppl]"]

    timeSeriesOverNight=merge(timeSeriesOverNight, countryName, by.x = "originCountryM49", by.y = "CountryM49", all.x = TRUE)
    setnames(timeSeriesOverNight, "Country","Origin")

    timeSeriesOverNight=merge(timeSeriesOverNight, countryName, by.x = "destinationCountryM49", by.y = "CountryM49", all.x = TRUE)
    setnames(timeSeriesOverNight, "Country","Destination")


    setcolorder(timeSeriesOverNight, c("destinationCountryM49", "Destination","originCountryM49", "Origin","tourismElement","Tourism Element","Year",
                                       "Value"))

    setnames(timeSeriesOverNight, "Year","timePointYears")


    write.xlsx(hh,"Data/touristOvenightVisitorsData.xlsx", row.names=F)


    write.xlsx(timeSeriesOverNight,"Data/overnight_all_countries.xlsx", row.names=F)
    # 
    # 
    # 
    
    
    
    
  })  
  
  

#longformat of summary tourist  

  longformat_summaryTourist=reactive({
    data= data.table(hot_to_r(input$touristsummary))
    
    yyy= melt.data.table(data, id.vars = c("CountryM49","Country","ElementCode" ,"Element"), measure.vars =c( "2010","2011","2012","2013",  "2014", "2015"),
                         value.name= "Value" )
    setnames(yyy,"variable", "Year")
    
    
    yyy[,Year:= as.character(Year)]
    # xxx= melt.data.table(data, id.vars = c("CountryM49","Country", "CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =c( "Flag 2010","Flag 2011", "Flag 2012" ,"Flag 2013", "Flag 2014","Flag 2015"),
    #                      value.name= "Flag" )
    
    # setnames(xxx,"variable", "Year")
    # xxx[,Year:=substring(Year,6)]
    # xxx[,Year:=as.character(Year)]
    # 
    # kk=merge(yyy, xxx, by = intersect(names(yyy), names(xxx)))  
    
    yyy
    
  })  
  
  
  
  
  
  #save modifications to overnight data
  
  observeEvent(input$touristsummarySave,{
    
    
    data=hot_to_r(input$touristsummary)
    
    setDT(data)
    
   yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    
    data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    
    write.xlsx(data,"Tourist Food/Data/touristSummaryData.xlsx", row.names=F)
    
  })    
  
  
  
  longformat_DESother=reactive({
    data= data.table(hot_to_r(input$touristDES))
    
    yyy= melt.data.table(data, id.vars = c("CountryM49","Country"), measure.vars =c( "2010","2011","2012","2013",  "2014", "2015"),
                         value.name= "Value" )
    setnames(yyy,"variable", "Year")
    setnames(yyy,"Value", "DES")
    
    
    yyy[,Year:= as.character(Year)]
    
    # xxx= melt.data.table(data, id.vars = c("CountryM49","Country", "CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =c( "Flag 2010","Flag 2011", "Flag 2012" ,"Flag 2013", "Flag 2014","Flag 2015"),
    #                      value.name= "Flag" )
    
    # setnames(xxx,"variable", "Year")
    # xxx[,Year:=substring(Year,6)]
    # xxx[,Year:=as.character(Year)]
    # 
    # kk=merge(yyy, xxx, by = intersect(names(yyy), names(xxx)))  
    
    yyy
    
  })  
  
  
  
  
  
  #save modifications to overnight data
  
  observeEvent(input$touristDESSave,{
    
    data=hot_to_r(input$touristDES)
    
    setDT(data)
    
 
    yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    
    
    
    data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    
    write.xlsx(data,"Tourist Food/Data/otherCountriestotDES.xlsx", row.names=F)
    
  })    
  
  

  
  finalTouristLongformat= reactive({
    data= data.table(hot_to_r(input$touristTable))
    
    setDT(data)
    
    flagcols <- grep("^Flag", names(data), value = TRUE)
    yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    
    data[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
    data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    
    yyy= melt.data.table(data, id.vars = c("CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =yearcols,
                         value.name= "Value" )
    
    
    setnames(yyy,"variable", "Year")
    
    yyy[,Year:= as.character(Year)]
    xxx= melt.data.table(data, id.vars = c("CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars = flagcols  ,
                         value.name= "Flag" )
    
    setnames(xxx,"variable", "Year")
    xxx[,Year:=substring(Year,6)]
    xxx[,Year:=as.character(Year)]
    
    kk=merge(yyy, xxx, by = intersect(names(yyy), names(xxx)))  
    
    kk
    
    
  }) 
  
  
   
  
  observeEvent(input$touristSave,{
    
    
    touristNew=data.table(hot_to_r(input$touristTable))
    
    touristNew=subset(touristNew, ElementCode == 5164)
    touristNew=long_format(touristNew)
    touristNew[,c("Commodity","Element") := NULL]
    
    
    
    touristOld=subset(countryData, ElementCode == 5164 & Year %in% c(2014:as.numeric(input$endyear)))
    touristOld[,c("CountryM49","Country"):= NULL]
    
    # cropOld=copy(cropData)
    # stockOld=long_format(stockOld)
    touristOld[,c("Commodity","Element") := NULL]
    
    originalData =copy(countryData)
    
    originalData[,c("CountryM49","Country","Commodity","Element") := NULL]
    
    
    originalData= originalData[ !(CPCCode %in% touristOld[, CPCCode] & Year %in% touristOld[,Year]  & ElementCode %in% touristOld[, ElementCode])]
    
    originalData=rbind(originalData,touristNew)
    
    
    originalData=merge(originalData,elementName, by = "ElementCode" , all.x = TRUE)
    
    originalData=merge(originalData,commodityName, by = "CPCCode" , all.x = TRUE)
    
    originalData[, CountryM49 := gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1))]
    
    originalData = merge(originalData,countryName, by = "CountryM49", all.x = TRUE)
    
    setcolorder(originalData,c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
    
    save(originalData, file = "Data/countrySUA.RData")
    
    # write.xlsx(originalData, "Data/ggg.xlsx",row.names = FALSE)
    
    countryData <<- originalData 
    
    
    
    
    
  })
  

  
  
 tourist_data=reactive({
    
   data=subset(countryData, ElementCode == 5164 )
   # 
   
   setDT(data)
   
   data[, c("CountryM49","Country"):=NULL]
   
   data=wide_format(data)
   
   END_YEAR=input$endyear
   data=visualize_data(data,END_YEAR)
   
   
   
   
   

    
    
  })  
  
  
  
 
 df_tourist<- reactiveValues(data_tourist=NULL)
 
 
 
 

 

 
 

observeEvent(input$impute_tourist,{
  # 
  caloriesData=read_excel("Data/caloriesData.xlsx")
  setDT(caloriesData)

  

  yearcols <- grep("^[[:digit:]]{4}$", names(caloriesData), value = TRUE)

  caloriesData[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]

  caloriesData= melt.data.table(caloriesData, id.vars = c("CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =yearcols,
                                value.name= "Value" )


  setnames(caloriesData,"variable", "Year")

  caloriesData[,Year:= as.character(Year)]
  
######################################################
  
  touristOvenightVisitors =read_excel("Data/touristOvenightVisitorsData.xlsx")
  setDT(touristOvenightVisitors)

  touristOvenightVisitors[, `Origin CountryM49` :=as.character(`Origin CountryM49`)]
  touristOvenightVisitors[, `Destination CountryM49` :=as.character(`Destination CountryM49`)]
  touristOvenightVisitors[,ElementCode :=as.character(ElementCode)]
  

  
  
  if (all(is.na(caloriesData$Value[caloriesData$Year == input$endyear]))){
  
    
    
    sendSweetAlert(
      session = session,
      title = "Warning !!",
      text = paste( "Please enter Calories for the year", input$endyear),
      type = "info"
    )
    
    
    
  }
  
  else   if (all(is.na(touristOvenightVisitors$`Overnight Visitors [#]`[touristOvenightVisitors$Year == input$endyear]))){


    sendSweetAlert(
      session = session,
      title = "Warning !!",
      text = paste("Please Enter information about tourist overnight visitors for year", input$endyear),
      type = "info"
    )


    # stop(paste("Please Enter information about tourist overnight visitors for year",input$endyear))

  }
  
  
  
  
   else{
  
     df_tourist$data_tourist <- imputeTourist(input,output,session)
     
     
     if ( dim(df_tourist$data_tourist)[1] == 0){
       
       
       df_tourist$data_tourist <- subset(countryData, ElementCode %in% "5164")
       
       df_tourist$data_tourist[, c("CountryM49","Country"):= NULL]
       
       
       sendSweetAlert(
         session = session,
         title = "Missing Data !!",
         text = "Tourist Overnight Data are missing either as origin country or destination country! ",
         type = "warning"
       )
       
       
       
       
     }else {
       
       
       sendSweetAlert(
         session = session,
         title = "Imputed !!",
         text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
         type = "success"
       )
       
       
       
     }
     
     
     
     
     df_tourist$data_tourist=wide_format(df_tourist$data_tourist)
     END_YEAR=input$endyear
     df_tourist$data_tourist=visualize_data(df_tourist$data_tourist,END_YEAR)
    
  
     
     
  }
  

  
  
  
  
  
})
  
  
  
  
output$touristTable=renderRHandsontable({
    

 
 
 if (is.null(df_tourist$data_tourist)){  
   
   df_tourist$data_tourist <- tourist_data()
 }

 
 

 
 # flagcols <- grep("^Flag", names(df_tourist$data_tourist), value = TRUE)
 # yearcols <- grep("^[[:digit:]]{4}$", names(df_tourist$data_tourist), value = TRUE)
 # 
 # minyear <- min(as.numeric(yearcols))
 # maxyear <- max(as.numeric(yearcols))
 
 # # Convert types
 # df_tourist$data_tourist[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
 # df_tourist$data_tourist[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
 # 
 # 


  
print(str(df_tourist$data_tourist))
    
  rhandsontable(df_tourist$data_tourist,
               height = 450,undo = T, redo = T,
                useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
                , selectCallback = TRUE, fontweight= "bold",search= TRUE)%>%
    hot_cols(fixedColumnsLeft = 4)%>%
    hot_context_menu(allowRowEdit = TRUE)%>%
    hot_col(c("CPCCode"), width = 100, halign= "htRight")%>%
    hot_cols(format = '0,0')%>%
    hot_col("Element", halign= "htLEft", width = 180)%>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
    hot_col("ElementCode", halign= "htRight", width = 100)%>%
    hot_col("Commodity", halign= "htLeft", width =300)
    
    
  })  
 
#################################################



touristCommoditiesModal <- function(failed = FALSE) {
  
  
  modalDialog(
    
    easyClose = TRUE, size= "l",
    dataTableOutput("touristItem")
    ,
    
    footer = tagList(
      
      actionButton("insertTourist", "Insert")
    )
  )
  
}


observeEvent(input$touristinsertcommodities, {
  showModal(touristCommoditiesModal())
})


output$touristItem= renderDataTable({
  
  commodity=fread("Food/Data/foodCommodityList.csv")
  DT=commodity
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
  
  datatable(DT,
            escape=F)
  
  
})






observeEvent(input$insertTourist, {
  
  removeModal()
  
})



itemRow=reactive({
  # values=reactiveValues()
  # values$df=wideformat_crop()
  
  if (input$insertTourist >0 )  {
    s=input$touristItem_rows_selected
    if (!is.null(s)){
      
      itemList=fread("Food/Data/foodCommodityList.csv")
      yy=itemList[s,]
      # yy[,Type := "Food Estimate"]
      
      
      
      data=isolate(hot_to_r(input$touristCommodities))
      if (!(yy$CPCCode %in% data$CPCCode)){
        
        data=rbind(yy,data,fill=T)
      }
      
      
      # data[order(CPCCode)]
    } else {
      
      data=hot_to_r(input$touristCommodities)
      
    }
  }
  
  
  
  else{
    
    data=hot_to_r(input$touristCommodities)
    
  }
  
  # data=data[order(CPCCode)]
  
  data
})



####################################################
  
  
  
  
  
  
}