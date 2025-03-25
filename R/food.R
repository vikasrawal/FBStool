food=function(input,output,session){
  
  
  observeEvent(input$undoGDP, {
    
    # get last version
    new_version <- Pop_table_version("gdp")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    print(new_version)
    print(df_gdp$data_gdp)
    
    
    df_gdp$data_gdp <- new_version
    
  })
  

  
  observeEvent(input$undoPopulation, {
    
    # get last version
    new_version <- Pop_table_version("popultaion")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_pop$data_pop <- new_version
    
  })

  
  observeEvent(input$undoFoodclassific, {
    
    # get last version
    new_version <- Pop_table_version("food_classification")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_classification$data_classification <- new_version
    
  })
  
  
  observeEvent(input$undoFood, {
    
    # get last version
    new_version <- Pop_table_version("food_values")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_food$data_food  <- new_version
    
  })
  
  
  
  
  
  
  
  
  
##############   GDP Data
  
  
  
  
  observeEvent(input$startContinue,{
    
      data=read_excel("Data/gdpData.xlsx")
      
      data=data.table(data)
      
      data <- data[!duplicated(Year)]
      
      data[,Year:=as.character(Year)]
      
      
      
      if ( input$endyear > max(data$Year)){
        
        
        # if(as.numeric(max(data$Year))+1 != input$endyear){
        #   df_gdp$data_gdp <- NULL
        #   
        #   sendSweetAlert(
        #     session = session,
        #     title = "Error!!",
        #     text = paste("Please compile gdp data for ",paste(as.numeric(max(data$Year))+1,collapse = ", ") , " first.", sep = ""),
        #     type = "error"
        #   )
        #   
        #   } 
        # else {
        
        row <- data.table(Year = as.character(input$endyear), 'GDP per capita [constant 2015 US$]' = "")
        
        data=rbind(data,row)
        # }
      }
    else {
        data=data[Year %in% c(2010: as.numeric(input$endyear))]
      }
      
      
      data[, `GDP per capita [constant 2015 US$]` := round(as.numeric(`GDP per capita [constant 2015 US$]`),0)]
      
      data <- data[order(Year)]
      
      df_gdp$data_gdp <- data
      
      Add_table_version("gdp", copy( df_gdp$data_gdp))  
        
      
})
  

  observeEvent(input$gdp, {
    new <- hot_to_r(input$gdp)
    # if(any(new != df_gdp$data_gdp)) {
      Add_table_version("gdp", copy(hot_to_r(input$gdp)))
      df_gdp$data_gdp <- hot_to_r(input$gdp)
    # }
  })
  
  output$gdp=renderRHandsontable({
    
    DATA <-df_gdp$data_gdp
    
    
    t=as.numeric(input$fromyear)
    
    number_to_freeze=  DATA[!(Year %in% c(2010:as.numeric(t-1))), which =TRUE]
    
    numeric_columns  <- grep("^[[:digit:]]{4}$", names(DATA), value = TRUE)
    
    
    DATA[,(numeric_columns) := round(.SD,0), .SDcols=numeric_columns]
    
    DATA <- DATA[order(Year)]
    
  if (!is.null(DATA)){
    
    
    rhandsontable(DATA[order(Year)],undo = T, redo = T, rowHeaders = NULL,useTypes = T, trimWhitespace =FALSE , Strict = F,readOnly = TRUE,
                  selectCallback = TRUE, fontweight= "bold",search=TRUE,colHeaders = names(DATA))%>%
      
      hot_cols(format = '0,0')%>%
      hot_col(c("GDP per capita [constant 2015 US$]"),halign = "hLeft",width=190)%>%
      hot_col("Year",halign = "htRight", width = 50)%>%
      hot_row(number_to_freeze, readOnly = FALSE)%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
    
    
    
  }  
    
    
  
    
  })
  

  
  observeEvent(input$saveGDP,{
    
    END_YEAR=as.character(input$endyear)
    
    start_year <- as.character(input$fromyear)
    
   
    
    data=data.table(hot_to_r(input$gdp))
    
    data[,Year := as.character(Year)]
    
    # write.csv(data,"gdp.csv",row.names = F)
    
    # write.csv(data,"test.csv",row.names = FALSE)
    # 
    # data <- fread("test.csv")
    
    gdp_data <- data.table(read_excel("Data/gdpData.xlsx"))
    
    gdp_data[,Year := as.character(Year)]
    
    gdp_data <- gdp_data[!duplicated(Year)]
    
    gdp_data <- merge(gdp_data,data,by= "Year",all = TRUE)
    
    gdp_data[, `GDP per capita [constant 2015 US$].x` := ifelse(Year %in% c(start_year:END_YEAR), `GDP per capita [constant 2015 US$].y`,
                                                                `GDP per capita [constant 2015 US$].x`)]
    
    gdp_data[, `GDP per capita [constant 2015 US$].y` := NULL]
    
    setnames(gdp_data, "GDP per capita [constant 2015 US$].x","GDP per capita [constant 2015 US$]")
    
    write.xlsx(gdp_data,"Data/gdpData.xlsx", row.names=F)
    
  })
  
  
  
######### end of GDP data
  
  
  
###################  Population Data

  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Food') {
      
      END_YEAR=input$endyear
      # data=read_excel("Data/populationData.xlsx")
      
      data=fread("SUA-FBS Balancing/Data/popSWS.csv")
      
      data <- data[,c("timePointYears","Value"),with = F]
      
      data <- subset(data,timePointYears %in% c(2010:END_YEAR)) 
      
      setnames(data, c("timePointYears","Value"),c("Year","Population [1000]"))
      data[,Year:=as.character(Year)]
      
      data <- data[!duplicated(Year)]
      data <- data[order(Year)]
      
      
      if ( input$endyear > max(data$Year)){
        
        
        if(as.numeric(max(data$Year))+1 != input$endyear){
          df_pop$data_pop <- NULL
          
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile population data for ",paste(as.numeric(max(data$Year))+1,collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
          
          
          
          
        } else {
          
          row <- data.table(Year = as.character(input$endyear), 'Population [1000]' = "")
          
          data=rbind(data,row)
        }
      }
      else {
        data=data[Year %in% c(2010: as.numeric(input$endyear))]
      }
      
      
      data[, `Population [1000]` := round(as.numeric(`Population [1000]`),0)]
      
      df_pop$data_pop <- data
      
      Add_table_version("popultaion", copy(df_pop$data_pop))
      
    # } 
  })
  
  
  observeEvent(input$popultaion, {
    new <- hot_to_r(input$popultaion)
    
    # if(any(new != df_pop$data_pop)) {
      Add_table_version("popultaion", copy(hot_to_r(input$popultaion)))
      df_pop$data_pop <- hot_to_r(input$popultaion)
    # }
  })
  
  output$popultaion=renderRHandsontable({
    
    DATA <-df_pop$data_pop
    
    
    t=as.numeric(input$fromyear)
    
    number_to_freeze=  DATA[!(Year %in% c(2010:as.numeric(t-1))), which =TRUE]
    
    
    numeric_columns  <- grep("^[[:digit:]]{4}$", names(DATA), value = TRUE)
    
    
    DATA[,(numeric_columns) := round(.SD,0), .SDcols=numeric_columns]
    
    
    if (!is.null(DATA)){
      
      
      rhandsontable(DATA,undo = T, redo = T,rowHeaders = NULL, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,readOnly = TRUE,
                    selectCallback = TRUE, fontweight= "bold",search=TRUE,colHeaders = names(DATA))%>%
        
        hot_cols(format = '0,0')%>%
        hot_col(c("Population [1000]"),halign = "hLeft",width=190)%>%
        hot_col("Year",halign = "htRight", width = 50)%>%
        hot_row(number_to_freeze, readOnly = FALSE)%>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
      
      
      
    }  
    
    
    
  })
  
  

  observeEvent(input$savePopulation,{
    
    start_year <- as.character(input$fromyear)
    
    END_YEAR=as.character(input$endyear)
    
    data=data.table(hot_to_r(input$popultaion))
    
    # write.csv(data,"pop_test.csv",row.names = F)
    
    data[,Year := as.character(Year)]
    
    popData <- fread("SUA-FBS Balancing/Data/popSWS.csv")
    
    popData[, timePointYears := as.character(timePointYears)]
    
    popData <- popData[!duplicated(timePointYears)]
    
    data <- merge(popData,data, by.x = c("timePointYears"),by.y = c("Year"),all = TRUE)
   
    data[, Value := ifelse(timePointYears %in% c(start_year:END_YEAR), `Population [1000]`,Value )]
    
    data[, `Population [1000]` := NULL]
    
    data[is.na(geographicAreaM49), geographicAreaM49 := unique(countryData$CountryM49)]
    data[is.na(measuredElement), measuredElement := "511"]
    data[is.na(flagObservationStatus), flagObservationStatus := "X"]
    data[is.na(flagMethod), flagMethod := "h"]
    
    write.csv(data,"SUA-FBS Balancing/Data/popSWS.csv", row.names = FALSE)
    
  })
  
  
####################################  end Population Data #################################################
  
  
###################################  Food Demand Model ###################################################

  
  #delete "trimwhitespace" to continue the in the same cell but in the next line
 
  
  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Food') {
      
     
      data=foodfdmData
      
      data=data.table(data)
      
      df_fdm$data_fdm <- data
      
      
      
    # } 
  })  
  
  
  
  observeEvent(input$saveFDM,{
    
    data <- data.table(hot_to_r(input$food_fdm))
    
  write.csv(data, "Data/fdmData.csv", row.names=F)
    
  })
  
  output$food_fdm=renderRHandsontable({
    
    
    
  DATA <- df_fdm$data_fdm
    
    
    rhandsontable(df_fdm$data_fdm,height = 400,undo = T, redo = T,rowHeaders = NULL,readOnly =T,
                  useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
                  , selectCallback = TRUE, fontweight= "bold",search= TRUE,colHeaders = names(df_fdm$data_fdm))%>%
      hot_col("CPCCode", halign = "htRight", width = 100)%>%
      hot_col("Commodity", halign = "htLeft", width = 200)%>%
      hot_col("Elasticity", halign = "htRight", width = 100)%>%
      hot_col("FBSCode", halign = "htRight", width = 100)%>%
      hot_col("Food Demand", halign = "htRight", width = 100)%>%
      hot_col("Food Function", halign = "htRight", width = 100)%>%
      hot_col("Description", halign = "htLeft", width = 100)%>%
      hot_col("FBSCommodity", halign = "htLeft", width = 250)%>%
      
      #   # hot_cols(format = '0,0')%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
      hot_col("FBSCommodity", width= 250)%>%
    hot_cols(fixedColumnsLeft = 6) 
  })
  
  
  
############################  Food Classification Table #######################################################

observeEvent(input$startContinue,{
    # if (input$fao == 'Food') {

      data=data.table(data_food_classification)
      
      data <- data[!duplicated(CPCCode)]

      df_classification$data_classification <- data
      
      Add_table_version("food_classification", copy(df_classification$data_classification))


  })
  
  
  observeEvent(input$savefoodclassific,{
    
    DATA <- hot_to_r(input$food_classification)
    
    DATA <- data.table(DATA)
    
    write.csv(DATA,"Data/foodCommodityList.csv",row.names = FALSE)
  })
  
  observeEvent(input$food_classification, {
    new <- hot_to_r(input$food_classification)
    
    new %>% mutate(Type = as.character(Type))
    
    
    # convert type to character as comparison is not implemented for ordered factors
    if(any(new %>% mutate(Type = as.character(Type)) != 
       df_classification$data_classification %>% mutate(Type = as.character(Type)))) {
      Add_table_version("food_classification", copy(new))
      df_classification$data_classification <- new
    }
  })
  
  
  output$food_classification=renderRHandsontable({
    
    DATA <-df_classification$data_classification
    
    DATA[, Type := as.factor(Type)]
    
   
    if (!is.null(DATA)){
      
      
      output <- rhandsontable(DATA,undo = T, redo = T,rowHeaders = NULL, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,readOnly = FALSE,
                    selectCallback = TRUE, fontweight= "bold",search=TRUE,colHeaders = names(DATA))%>%
        
       
        # hot_col(c("Population [1000]"),halign = "hLeft",width=190)%>%
        # hot_col("Year",halign = "htRight", width = 50)%>%
        hot_col(c("CPCCode","Commodity"), readOnly = T)
        # hot_table(highlightCol = TRUE, highlightRow = TRUE)
      
      
      
     
      # if (!is.null(input$filter_cpc)) {
      #   
      #     output <- DATA[CPCCode %in% c(input$filter_cpc)]
      #  
      # }
      # 
      # output
  
    }  
    
    
    
  })
  

#################################################################  Food Data #######################################################################  
  
  
  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Loss') {
    
    END_YEAR=input$endyear
    
    foodData=subset(countryData, ElementCode %in% c("5141"))
    
    setDT(foodData)
    
    foodData <- foodData[!duplicated(foodData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
    
    foodData[, c("CountryM49","Country"):=NULL]
    
    foodData <- subset(foodData, Year %in% c(2010 : END_YEAR) )
    
    # foodData <- foodData[!is.na(Value)]
    
    
    foodData=wide_format(foodData)
    
    flagcols <- grep("^Flag", names(foodData), value = TRUE)
    yearcols <- grep("^[[:digit:]]{4}$", names(foodData), value = TRUE)
    
    minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))
    
    
    
    
    if(END_YEAR > maxyear +1){
      END_YEAR=as.numeric(END_YEAR)
      yearsToFill = (maxyear + 1):END_YEAR
      
      df_food$data_food <- NULL
      if(length(yearsToFill) > 0){
        # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
        
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = paste("Please compile Food data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
          type = "error"
        )
        
        
        
      }
      
    } else {
      
      foodData = visualize_data(foodData,END_YEAR, session)
      
      foodData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      
      df_food$data_food <- foodData
      
      Add_table_version("food_values", copy( df_food$data_food))  
    }
    
    
    
    
    # }
  })
  
  
  
  observeEvent(input$add_Food, {
    showModal(viewFoodTriplets())
  })
  
  
  viewFoodTriplets <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewFood")
      ,
      
      footer = tagList(
        
        actionButton("foodInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewFood= renderDataTable({
    
    
    
    
    
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_food$data_food$CPCCode) )
    values_foodCommodities$foodCommodities <- DT
    # }
    
    
    
    datatable(values_foodCommodities$foodCommodities,
              escape=F)
    
    
  })
  
  
  observeEvent(input$foodInsert, {
    
    removeModal()
    
  })
  
  
  proxy_food = dataTableProxy('food_values')
  
  observeEvent(input$food_values_cell_edit, {
    
    
    info = input$food_values_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_food$data_food[i,(j) := v]
    
    replaceData(proxy_food, df_food$data_food, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["food_values_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"food_values"))
    
    Add_table_version("food_values", copy( df_food$data_food))  
    
  })
  
  
  
  
  observeEvent(input$foodInsert, {
    
    
    s=as.numeric(input$viewFood_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_food$data_food)
      
      
      
      df_food$data_food <- data_current
      
      
    }
    
    else {
      
      
      foodlistTool <- copy(all_cpc)
      foodlistTool <- foodlistTool[order(CPCCode),]
      
      foodlistTool <- subset(foodlistTool,  !CPCCode %in% unique(isolate(df_food$data_food$CPCCode) ))
      
      yy=foodlistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := c("5141")]
      
      
      
      oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_food$data_food)
      data=data.table(data)
      # data[, hidden := NULL]
      
      if (!(unique(oo$CPCCode) %in% data$CPCCode)){
        
        data=rbind(oo,data,fill=T)
        
        
        data=data[!is.na(ElementCode)]
        data[is.na(data)] <- ""
        yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
        
        data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
        
        # data[is.na(data)] <- 0
        
      }
      
      data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      
      df_food$data_food <- data
      Add_table_version("food_values", copy( df_food$data_food))  
      
    }
    
    
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_food, {
    t = copy(df_food$data_food)
    final_data <- copy(df_food$data_food)
    
    if (!is.null(input$food_values_rows_selected)) {
      t <- t[as.numeric(input$food_values_rows_selected),]
      
      cpc_code_to_remove <- unique(t$CPCCode)
      ele_code_to_remove <- unique(t$ElementCode)
    }
    
    df_food$data_food <- final_data[!CPCCode %in% cpc_code_to_remove]
    
    #remove the cpc with the element from the database
    
    database <- copy(countryData)
    
    database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% ele_code_to_remove)]
    
    countryData <<- database
    
    Add_table_version("food_values", copy( df_food$data_food))  
    
    
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadFood<- downloadHandler(
    
    filename = function() {
      
      "food_availability_data.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_food <- data.table(df_food$data_food)
      
      data_download_food <- data_download_food[!is.na(CPCCode)]
      
      data_download_food[,hidden := NULL]
      
      write.xlsx(data_download_food ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileFooddenormalized,{
    
    
    inFile <- input$fileFooddenormalized
    
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
    
    
    
    END_YEAR=input$endyear
    # END_YEAR <- 2017
    
    # data_denormalized <- data.table(read_excel("normalized.xlsx"))
    
    data_denormalized <- copy(DATA)
    
    data_denormalized <- long_format(data_denormalized)
    
    data_denormalized <- data_denormalized[Year %in% c(input$fromyear : input$endyear)]
    
    data_denormalized[, c("Commodity","Element") := NULL]
    
    data_denormalized[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    
    # write.csv(data.table(df_food$data_food),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    food_Data <- data.table(df_food$data_food)
    
    
    food_Data <- long_format(food_Data)
    
    food_Data[, c("Commodity","Element") := NULL]
    
    food_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=food_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
    ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    food_Data <- food_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
    ]
    
    food_Data <- rbind(food_Data,xx)
    
    food_Data <- merge(food_Data, all_elements, by = "ElementCode", all.x = T)
    
    food_Data <- merge(food_Data, all_cpc, by= "CPCCode", all.x = T)
    
    food_Data <- food_Data[!is.na(Element)]
    
    food_Data <- subset(food_Data, Year %in% 2010:END_YEAR)
    
    food_Data <- wide_format(food_Data)
    
    food_Data = visualize_data(food_Data,END_YEAR)
    
    food_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_food$data_food <- food_Data
    
    Add_table_version("food_values", copy( df_food$data_food))  
    
  })
  
  
  
  observeEvent(input$uploadFoodModal, {
    showModal(uploadFood())
  })
  
  
  
  uploadFood <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileFood", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcFood", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_foodCountry$data_foodCountry)),multiple=F),
                    
                    selectizeInput("elementFood", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_foodCountry$data_foodCountry)) ,multiple=F),
                    selectizeInput("yearFood", "Year :",
                                   selected = NULL, choices = c("",colnames( df_foodCountry$data_foodCountry)),multiple=F),
                    
                    selectizeInput("valueFood", "Value :",
                                   selected = NULL, choices = c("",colnames( df_foodCountry$data_foodCountry)),multiple=F),
                    
                    selectizeInput("flagFood", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_foodCountry$data_foodCountry)),multiple=F),
                    
                    actionButton("uploadFood","Upload Food data")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    # dataTableOutput("importCountry")
                    div(style = 'overflow-x: scroll', dataTableOutput('foodCountry'))
                  )
                  
                )
    )
    
  }
  
  
  
  output$foodCountry <- renderDataTable({
    
    req(input$fileFood)
    
    
    
    inFile <- input$fileFood
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_foodCountry$data_foodCountry <- DATA
    
    
    datatable(df_foodCountry$data_foodCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcFood", choices = c("", colnames( df_foodCountry$data_foodCountry)))
    updateSelectInput(session, "elementFood", choices = c("",colnames( df_foodCountry$data_foodCountry)))
    updateSelectInput(session, "yearFood", choices = c("",colnames( df_foodCountry$data_foodCountry)))
    updateSelectInput(session, "valueFood", choices = c("",colnames( df_foodCountry$data_foodCountry)))
    updateSelectInput(session, "flagFood", choices = c("",colnames( df_foodCountry$data_foodCountry)))
    
  })
  
  
  #############################################
  
  observeEvent(input$uploadFood, {
    
    removeModal()
    
  })
  
  
  
  
  observeEvent(input$uploadFood,{
    
    
    data <- data.table(df_foodCountry$data_foodCountry)
    
    if(input$cpcFood == ""|input$elementFood == ""| input$yearFood == ""| input$valueFood == ""| input$flagFood == "" ){
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_food$data_food <- df_food$data_food
      
    }else{
      
    
    data <- data[, c(input$cpcFood, input$elementFood, input$yearFood, input$valueFood, input$flagFood), with= F]
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_foodCountry$data_foodCountry)
    }
    else{
    
    
    
    setnames(data,c(input$cpcFood, input$elementFood, input$yearFood, input$valueFood, input$flagFood),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    
    data <- subset(data, ElementCode %in% c("5141"))
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    food_values <- data.table(df_food$data_food)
    
    food_values <- long_format(food_values)
    
    food_values[,c("Commodity","Element") := NULL]
    
    food_values[, ElementCode := as.character(ElementCode)]
    
    xx <- food_values[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
    ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    food_values <- food_values[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
    ]
    
    food_values <- rbind(food_values,xx)
    
    food_values <- merge(food_values, all_elements, by = "ElementCode", all.x = T)
    
    food_values <- merge(food_values, all_cpc, by= "CPCCode", all.x = T)
    
    food_values <- food_values[!is.na(Element)]
    
    food_values <- wide_format(food_values)
    
    food_values[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_food$data_food <- food_values
    
    Add_table_version("food_values", copy( df_food$data_food))  
    }
    }
    
  })
  
  
  ############################################## 
  
  
  
  
  
  
  #save stock
  
  observeEvent(input$saveFood,{
    
    data_to_save <- df_food$data_food
    
    data_to_save <- subset(data_to_save, ElementCode == "5141")
    
    save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
    
    new_saved_data <- return_data_base(data_to_save)
    
    df_sua_unbalanced$data_sua_unbalanced <- new_saved_data
    
    
    
    
    
  })
  
  

  
  
  observeEvent(input$food_imputation,{
    
    # gdp_Data <- data.table(hot_to_r(input$gdp))  # this command gave an error when the user does not open the gdp tab
    
    gdp_Data <- data.table(read_excel("Data/gdpData.xlsx"))
    
   
    year_Range <- c(input$fromyear : input$endyear) 
    
    timeseries <-data.table(expand.grid(Year = as.character(year_Range)))
    
    timeseries <- merge(timeseries, gdp_Data, all.x = TRUE)
    
    empty_cell <- unique(timeseries[Year %in% year_Range]$`GDP per capita [constant 2015 US$]`)
    
    if (NA %in% empty_cell){
      
      sendSweetAlert(
        session = session,
        title = "Missing GDP Data !!",
        text = "Some GDP per capita are missing",
        type = "error"
      )
      
      
    } else {
    
    
    
    
    data <- imputeFood(input,output,session)

  
      sendSweetAlert(
        session = session,
        title = "Imputed !!",
        text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
        type = "success"
      )
      
  
    
    data <- wide_format(data)
    
    
    
    if ( is.null(data) ){
      
      sendSweetAlert(
        session = session,
        title = c("Imputation Error"),
        text = c("No time series data to impute"),
        type = "warning"
      )
      
    } else {
      
      
      # df_stock$data_stock=wide_format(df_stock$data_stock)
      END_YEAR=input$endyear
      data=visualize_data(data,END_YEAR)
      data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      
      df_food$data_food <- data
      
      Add_table_version("food_values", copy( df_food$data_food)) 
      
    }
    
    
    } 
    
  })
  
  
  
  
  
  output$food_values <- 
    renderDataTable(
      
      if (!is.null(df_food$data_food)){
        datatable (df_food$data_food, rownames= FALSE,class = 'cell-border stripe', 
                   
                   editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_food$data_food) == input$fromyear)-2)))),
                   extensions = c("FixedColumns","FixedHeader","Buttons"),
                   options = list(
                     pageLength = 25,
                     dom= 'Blfrtip', buttons = I('colvis'),
                     # dom='f', ordering=F,
                     # paging = TRUE, searching = TRUE, info = FALSE,
                     # sort = TRUE,
                     scrollX = TRUE,
                     scrollY = "500px" ,
                     autoWidth = T,
                     fixedColumns = list(leftColumns = 4),
                     columnDefs = list(
                       #list(width = '150px', targets = c(3))
                       # no hide column
                       list(visible = FALSE, targets = (ncol(df_food$data_food)-1))
                     )
                   ))  %>%
          
          formatStyle(0:ncol(df_food$data_food), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
      
    )
  
#####################################################################################################################################################################  
  
}
