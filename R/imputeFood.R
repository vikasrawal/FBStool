imputeFood=function(input,output,session){
  
  show_modal_spinner(
    
    spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
  )

  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  
  data_Session <- data.table(df_sua_unbalanced$data_sua_unbalanced )
  
  data_Session <- long_format(data_Session)
  
  
  
  # data_Session <- copy(countryData)
  # data_Session[,c("CountryM49","Country","Commodity","Element"):=NULL]

  
  
  
  files = dir("Food/R",full.names = TRUE)
  sapply(files, source)
  
  
  # upload item Food #includes only food commodities for Sri Lanka. Filter SriLanka Data with ElementCode 5141. Extracted only those commodties
  # itemFood = fread(file = "Food/Data/itemFood.csv")
  
  
  Utilization_Table <- fread("SUA-FBS Balancing/Data/utilization_table_2018.csv")
  
  itemFood <- unique(Utilization_Table[food_item == "X"]$cpc_code)
  
  itemFood_session <- unique(data_Session[ElementCode == "5141"]$CPCCode)
  
  itemFood <- c(itemFood,itemFood_session)
  
  itemFood <- itemFood[!duplicated(itemFood)]
  
  
  # itemFood=subset(countryData, ElementCode == "5141")
  # 
  # itemFood=unique(itemFood[,c("CPCCode")])
  # 
  # 
  
  
  
  
  
  # upload production data
  
  productionData=subset(data_Session, ElementCode == "5510" & Year %in% c(2010:t[length(t)]))
  productionData[, c("ElementCode","Element","Commodity") := NULL] 
  productionData = subset(productionData, (CPCCode %in% itemFood))
  setnames(productionData,"Value","[5510] Production [t]")
  setnames(productionData,"Flag","[5510] Flag")
  
  
  productionData$`[5510] Flag` =ifelse(!is.na(productionData$`[5510] Production [t]`) & is.na(productionData$`[5510] Flag`), "", productionData$`[5510] Flag`)
  
  
  
  # upload trade data
  # tradeData = fread(file = "Trade/Data/finalTradeData.csv")
  
  tradeData=subset(data_Session, ElementCode %in% c("5610", "5910"))
  tradeData=tradeData[Year %in% c(2010:t[length(t)])]
  
 
  
  importData = subset(tradeData, ElementCode == 5610 & CPCCode %in% itemFood)
  importData[, c("ElementCode","Element", "Commodity") := NULL]
  setnames(importData,"Value","[5610] Import Quantity [t]")
  setnames(importData,"Flag","[5610] Flag")
  importData$`[5610] Flag` =ifelse(!is.na(importData$`[5610] Import Quantity [t]`) & is.na(importData$`[5610] Flag`), "", importData$`[5610] Flag`)
  
  
  exportData = subset(tradeData, ElementCode == 5910 & CPCCode %in% itemFood)
  exportData[, c("ElementCode","Element", "Commodity") := NULL]
  setnames(exportData,"Value","[5910] Export Quantity [t]")
  setnames(exportData,"Flag","[5910] Flag")
  
  exportData$`[5910] Flag` =ifelse(!is.na(exportData$`[5910] Export Quantity [t]`) & is.na(exportData$`[5910] Flag`), "", exportData$`[5910] Flag`)
  
  
  
  # merge data
  data = merge(importData,exportData,by = c("CPCCode","Year"), all = TRUE)
  data = merge(data,productionData,by = c("CPCCode","Year"),all = TRUE)
  

  
#Stock 
  stockChangesData=subset(data_Session, ElementCode == 5071 & CPCCode %in% itemFood & Year %in% c(2010:t[length(t)]))
  stockChangesData[, c("ElementCode","Element", "Commodity") := NULL]
  setnames(stockChangesData,"Value","[5071] Stock Variation [t]")
  setnames(stockChangesData,"Flag","[5071] Flag")
  
  
#merge data with Stock
  
data = merge(data,stockChangesData, 
               by = c("CPCCode","Year"),
               all = TRUE)
  
  
  
  
#Food residual is computed as p+I-(X+stock+Loss+Feed + Feed +Seed+Indu+Residual)
  
#Loss data

lossData=subset(data_Session, ElementCode == 5016 & CPCCode %in% itemFood & Year %in% c(2010:t[length(t)]))
lossData[, c("ElementCode","Element", "Commodity") := NULL]
setnames(lossData,"Value","[5016] Loss [t]")
setnames(lossData,"Flag","[5016] Flag")
  
data = merge(data,lossData, 
             by = c("CPCCode","Year"),
             all = TRUE)  
  
  

#feed data

feedData=subset(data_Session, ElementCode == 5520 & CPCCode %in% itemFood & Year %in% c(2010:t[length(t)]))
feedData[, c("ElementCode","Element", "Commodity") := NULL]
setnames(feedData,"Value","[5520] Feed [t]")
setnames(feedData,"Flag","[5520] Flag")

data = merge(data,feedData, 
             by = c("CPCCode","Year"),
             all = TRUE)  

#seed 
seedData=subset(data_Session, ElementCode == 5525 & CPCCode %in% itemFood & Year %in% c(2010:t[length(t)]))
seedData[, c("ElementCode","Element", "Commodity") := NULL]
setnames(seedData,"Value","[5525] Seed [t]")
setnames(seedData,"Flag","[5525] Flag")

data = merge(data,seedData, 
             by = c("CPCCode","Year"),
             all = TRUE)  

#industrial 
industrialData=subset(data_Session, ElementCode == 5165 & CPCCode %in% itemFood & Year %in% c(2010:t[length(t)]))
industrialData[, c("ElementCode","Element", "Commodity") := NULL]
setnames(industrialData,"Value","[5165] Industrial [t]")
setnames(industrialData,"Flag","[5165] Flag")

data = merge(data,industrialData, 
             by = c("CPCCode","Year"),
             all = TRUE)  


#create the Supply element
  

data[, `Supply [t]` := sum(`[5510] Production [t]`, 
                           `[5610] Import Quantity [t]`,(-1)* `[5910] Export Quantity [t]`, -
                             `[5071] Stock Variation [t]`,-
                             `[5016] Loss [t]`,-
                             `[5520] Feed [t]`,-
                             `[5525] Seed [t]` ,- 
                             `[5165] Industrial [t]` 
                             
                           ,na.rm = TRUE), by = c("CPCCode","Year")]

data$`[Supply] Flag` = "I"
  
  
  
  
  
foodData=subset(data_Session, ElementCode == "5141")
foodData[is.na(Value), Flag := NA ]
  
  if (!( t %in% unique(foodData$Year))){
    
    
   data_to_add = foodData[Year == (t-1)] 
    
      data_to_add[Year == as.character((t-1)), Year:= as.character(t)]
      data_to_add[ , c("Value", "Flag") := NULL]
      data_to_add[, Value := NA_real_] 
      data_to_add[, Flag := NA_character_] 
      
      
      foodData=rbind(foodData,data_to_add)
  }
  
  
  

  foodData[,c("ElementCode","Element", "Commodity"):=NULL]
  
  
  #Assign flag NA for vlaues NA
  
  foodData$Flag =ifelse(is.na(foodData$Value) & foodData$Flag == "", NA, foodData$Flag)
  
  # foodData=foodData[Year %in% c(2010:t)]
  
  #Merge the food Type
  
  
  foodType <- fread("Data/foodCommodityList.csv")
  foodType <-  foodType[, Commodity := NULL]
  
  foodType <- foodType[!is.na(Type)]
  
  
  data = merge(data,foodData, 
               by = c("CPCCode","Year"),
               all = TRUE)
  
  data = merge(data, foodType, by = "CPCCode", all.x = TRUE)
  
  
  
  data = subset(data, CPCCode %in% itemFood)
  
  setnames(data,"Value","[5141] Food [t]")
  setnames(data,"Flag","[5141] Flag")
  
  # foodData$`[5141] Flag` =ifelse(!is.na(foodData$`[5141] Food [t]`) & is.na(foodData$`[5141] Flag`), "", foodData$`[5141] Flag`)
  
  
  
  #create a columun Protected. We know that data of 2010 to t-1 are protected. 


  
  # foodData[,Protected := ifelse(foodData$Year %in% c(2010:(t-1)) | foodData$`[5141] Flag` %in% c("","T", "E"), TRUE, FALSE)]
  

  
 
  
  
  
  
  # upload fdmData
  fdmData = fread("Data/fdmData.csv",head=T)
  # setnames(fdmData,c("foodDemand", "foodFunction", "elasticity"),c("Food Demand","Food Function","Elasticity"))
  fdmData[, Commodity := NULL]
  
  # merge data
  # str(data)

  data <- merge(data, fdmData, by = c("CPCCode"), all.x = TRUE)
  
  
  #data=data[!duplicated(data[,c("CPCCode","Year")])]
  
  
  # check
  # sum(data[Year == 2013, `[5141] Food [t]`])
  
  # aa = unique(data[is.na(Elasticity),list(Commodity,CPCCode)])
  
  
  # data =  data[!((is.na(Elasticity) & Type == "Food Estimate"))]
  
  
  
  # upload gdp data
  # gdpData = fread("Food/Data/GDPpcData.csv")
  gdpData=read_excel("Data/gdpData.xlsx")
  
  setDT(gdpData)
  
  
  # merge data
  
  data[, Year := as.character(Year)]

  gdpData[, Year := as.character(Year)]
  
  
  
  
  data = merge(data,gdpData, 
               by = c("Year"),
               all.x = TRUE)
  
  # upload population data
  # popData = fread("Food/Data/populationData.csv")
  
  popData <- fread("SUA-FBS Balancing/Data/popSWS.csv")
  popData <- popData[,c("timePointYears","Value")]
  setnames(popData, names(popData),c("Year", "Population [1000]"))
  
  popData <- popData[Year %in% c(2010:t[length(t)])]
  popData <- popData[order(Year)]
  
  popData[,Year:=as.character(Year)]
  
  data <- merge(data, popData, 
                by = c("Year"),
                all.x = TRUE)
  
  
 
  # dataA = data[Year %in% c(2010:(t-1)),]
  # dataB = data[Year %in% t & !(is.na(`Supply [t]`) )]
  # data = rbind(dataA,dataB)
  

  
  ## The user should update the elasticity value in fdmData
  
  data[, `Food Function` := ifelse(`Food Function` == 4, 3, `Food Function`)]
  data[, Elasticity := as.numeric(Elasticity)]  
  
  
  
  data[,Protected := ifelse(Year %in% c(2010: (as.numeric(t[1])-1))| `[5141] Flag` %in% c("","T", "E"), TRUE, FALSE)]
  
  
  
  data = data[!(is.na(Type) & Protected == F)]
  
  # NA  type of some protected figures will not eliminate. Assign a temporary type
  
  data[Protected == TRUE & is.na(Type), Type := "Food Residual"]
  
  
  data =  data[!((is.na(Elasticity) & Type == "Food Estimate" & Protected == F))]
 
  # Food Module
  
  items = data[Year %in% t[length(t)], unique(CPCCode)]
  
 
  time = as.character(c((t-1):t[length(t)]))
  
  data[is.na(`Supply [t]`), `Supply [t]` := 0]
  
  for(j in 1:(length(time)-1)){
   # print(j)
    
    for(i in 1:length(items)){
      # print(i)
      db_t1 = dplyr::filter(data, CPCCode == items[i] & Year == time[j+1])
      db_t0 = dplyr::filter(data, CPCCode == items[i] & Year == time[j])
      
      
      if(nrow(db_t1) == 0 | nrow(db_t0) == 0) {
        next
      } else if (db_t1$`Supply [t]` == 0 & db_t1$Type != "Food Residual"){
        db_t1$`[5141] Food [t]` = 0
      } 
 ############# This has been commented due to the change of the calculation of food residuals     
      # else if (db_t1$Protected == FALSE & db_t1$Type == "Food Residual" & db_t1$`Supply [t]` > 0){
      #   db_t1$`[5141] Food [t]` = db_t1$`Supply [t]`} 
        
        
      #   else if (db_t1$Protected == FALSE & db_t1$Type == "Food Residual" & db_t1$`Supply [t]` <= 0){
      #   db_t1$`[5141] Food [t]` = 0
      # } 
    
    
    else if (db_t1$Protected == FALSE & db_t1$Type != "Food Residual"){
        food_t0 = data[CPCCode == items[i] & Year == time[j],`[5141] Food [t]`]
        pop_t0 =  data[CPCCode == items[i] & Year == time[j],`Population [1000]`]
        pop_t1 =  data[CPCCode == items[i] & Year == time[j+1],`Population [1000]`]
        supply_t0 = data[CPCCode == items[i] & Year == time[j],`Supply [t]`]
        supply_t1 = data[CPCCode == items[i] & Year == time[j+1],`Supply [t]`]
        elas = as.numeric(data[CPCCode == items[i] & Year == time[j+1],Elasticity])
        gdp_t0 = data[CPCCode == items[i] & Year == time[j],`GDP per capita [constant 2015 US$]`]
        gdp_t1 = data[CPCCode == items[i] & Year == time[j+1],`GDP per capita [constant 2015 US$]`]
        form = data[CPCCode == items[i] & Year == time[j+1],`Food Function`]
        
        db_t1$`[5141] Food [t]` = calculateFood(food_t0,as.numeric(pop_t0),as.numeric(pop_t1),elas,as.numeric(gdp_t0),as.numeric(gdp_t1),form)
        # db_t1$`[5141] Food [t]` = calculateFood(food_t0,pop_t0,pop_t1,supply_t0,supply_t1,elas,gdp_t0,gdp_t1,form)
    } 
      else  {
        db_t1$`[5141] Food [t]` = db_t1$`[5141] Food [t]`
      }
      data[CPCCode == items[i] & Year == time[j+1], `[5141] Food [t]` := db_t1$`[5141] Food [t]`]
      
      
    }
  }
  
  data[,`[5141] Flag` := as.character(`[5141] Flag`)]
  
  
  data[Type == "Food Residual" & Protected == FALSE, `[5141] Food [t]` := `Supply [t]`]
 
  
  
  data[Protected == FALSE & !is.na(`[5141] Food [t]`), `[5141] Flag` := "I"]
  
  
  # foodData = select(data,c(Year,CPCCode,`Supply [t]`,`[Supply] Flag`,Commodity,`[5141] Food [t]`,`[5141] Flag`,`Type`))
  
  foodData = data[,c("Year","CPCCode","Supply [t]", "[Supply] Flag","[5141] Food [t]","[5141] Flag","Type" , "Protected")]
  
  
  foodData[, `Supply [t]` := round(`Supply [t]`,0)]
  foodData[, `[5141] Food [t]` := round(`[5141] Food [t]`,0)]
  
  # foodData[`Supply [t]`<= 0 & Year %in% c(2014:2015),list(CPCCode,Year,`Supply [t]`,`[5141] Food [t]`)]
  # foodData[`Supply [t]`<= 0 & Year %in% c(2014:2015), `[5141] Food [t]` := 0]
  # foodData[`Supply [t]`<= 0 & Year %in% c(2014:2015), `[5141] Flag` := "I"]
  
  foodData[`Supply [t]`<= 0 & Year %in% c(t) & Type == "Food Residual" & Protected == FALSE, `[5141] Food [t]` := 0]
  foodData[`Supply [t]`<= 0 & Year %in% c(t) & Type == "Food Residual" & Protected == FALSE, `[5141] Flag` := "I"]
  
  
  
  # foodData[`[5141] Food [t]` > abs(`Supply [t]`) & Year %in% c(2014:2015),list(CPCCode,Year,`Supply [t]`,`[5141] Food [t]`)]
  
  # foodData[`[5141] Food [t]` > abs(`Supply [t]`) & Year %in% c(2014:2015), `[5141] Food [t]` := `Supply [t]`]
  # foodData[`[5141] Food [t]` > abs(`Supply [t]`) & Year %in% c(2014:2015),`[5141] Flag` := "I"]
  
  foodData[`[5141] Food [t]` > abs(`Supply [t]`) & Year %in% c(t) & Type == "Food Residual" & Protected == FALSE, `[5141] Food [t]` := `Supply [t]`]
  foodData[`[5141] Food [t]` > abs(`Supply [t]`) & Year %in% c(t)& Type == "Food Residual" & Protected == FALSE,`[5141] Flag` := "I"]
  
  foodData[, Protected := NULL]
  
  # foodData[`Supply [t]`<= 0 & Year %in% c(2014:2015), `[5141] Food [t]` := `Supply [t]`]
  # foodData[`Supply [t]`<= 0 & Year %in% c(2014:2015),`[5141] Flag` := "I"]
  
  # foodData[`Supply [t]`<= 0 & Year %in% c(t), `[5141] Food [t]` := `Supply [t]`]
  # foodData[`Supply [t]`<= 0 & Year %in% c(t),`[5141] Flag` := "I"]
  
  foodData <- merge(foodData,all_cpc,by = "CPCCode", all.x = TRUE)
  
 

  
  foodValueData = melt(foodData[,-c("[Supply] Flag","[5141] Flag")], id.vars = c("CPCCode","Commodity","Type","Year"),
                       variable.name = "Element", value.name = "Value")
  # unique(foodValueData$Element)
  
  foodValueData$Element = ifelse(foodValueData$Element == "[5141] Food [t]","Food [t]","Supply [t]")
  foodValueData$ElementCode = ifelse(foodValueData$Element == "Food [t]","5141","xxxx")
  
  
  foodFlagData = melt(foodData[,-c("Supply [t]","[5141] Food [t]")], id.vars = c("CPCCode","Commodity","Type","Year"),
                      variable.name = "Flag Description", value.name = "Flag")
  foodFlagData$`Flag Description` = NULL
  
  # write finalFoodData
  
  finalFoodData = cbind(foodValueData,foodFlagData[,"Flag"])
  setcolorder(finalFoodData, c("CPCCode","Commodity","Type","ElementCode","Element","Year","Value","Flag"))
  finalFoodData = finalFoodData[order(CPCCode,Year)]
  
  finalFoodData = finalFoodData[ElementCode ==  5141 & Value < 0, Value := 0]
  
  finalFoodData[, Value := ifelse(ElementCode == "xxxx" & Type == "Food Estimate", NA, Value) ]
  finalFoodData[, Flag := ifelse(ElementCode == "xxxx" & Type == "Food Estimate", NA, Flag) ]
  
  finalFoodData <- finalFoodData[ ! (is.na(Value) & ElementCode == "xxxx")]
  
  # In order to make sure that previous years food data are not overwritten...
  
  
  
  finalFoodData = subset(finalFoodData, Year %in% t)
  
  finalFoodData[, Type := NULL]
  
  
  countrdata_without_food_t <- subset(data_Session, !(Year %in% t & ElementCode %in% "5141"))
  
  
  # countrdata_without_food_t[, c("CountryM49","Country"):= NULL]
  
  countrdata_without_food_t <- subset(countrdata_without_food_t, ElementCode %in% "5141")
  
  finalFoodData<-rbind(finalFoodData,countrdata_without_food_t)
  
  finalFoodData <-  finalFoodData[!duplicated(finalFoodData[, c("CPCCode","ElementCode","Year")])]
  
  finalFoodData[, Flag := ifelse(is.na(Value), NA, Flag)]
  
 
  
  Sys.sleep(3)
  remove_modal_spinner()
  
  finalFoodData
  
}
