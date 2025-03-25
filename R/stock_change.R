

stock_change <- function(input,output,session){

  
  observeEvent(input$undoStock, {
    
    # get last version
    new_version <- Pop_table_version("stock")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_stock$data_stock <- new_version
    
  })
  

  observeEvent(input$startContinue,{
    # if (input$fao == 'Stock') {
    
      END_YEAR=input$endyear
      
      stockData=subset(countryData, ElementCode %in% c( "5113","5071"))
      
      setDT(stockData)
      
      stockData <- stockData[!duplicated(stockData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
      
      stockData[, c("CountryM49","Country"):=NULL]
      
      stockData <- subset(stockData, Year %in% c(2010 : END_YEAR) )
      
      
 #updating opening stocks 
      
      
      x <- copy(stockData[Year %in% c(2014 : END_YEAR)])
      x[,c("Commodity","Element") := NULL]
      x <- dcast.data.table(x, CPCCode+ Year ~ ElementCode, value.var = c("Value","Flag"))
      
      setnames(x, c("Value_5071","Value_5113"),c("delta","new_opening"))
      
      x[is.na(delta), delta := 0]
      
      x[is.na(new_opening), new_opening := 0]
      
      x <- x[order(CPCCode, Year)]
      
      groups <- unique(x[, .(CPCCode)])
      
      res <- list()
      
      for (i in seq_len(nrow(groups))) {
        z <- x[groups[i], on = c( "CPCCode")]
        
        # print(i)
        if (nrow(z) > 1) {
          for (j in seq_len(nrow(z))[-1]) {
            
            # print(j)
            # negative delta cannot be more than opening
            if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
              z$delta[j-1] <- - z$new_opening[j-1]
            }
            z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
          }
          # negative delta cannot be more than opening
          if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
            z$delta[j] <- - z$new_opening[j]
          }
        }
        res[[i]] <- z
      }
      
      
      data_2013 <- stockData[Year %in% c(2010:2013)]
      
      updated_stock <- rbindlist(res)
      
      
      
      ValueData = melt(updated_stock[,-c("Flag_5071","Flag_5113")], 
                       id.vars = c("CPCCode","Year"),
                       variable.name = "ElementCode", value.name = "Value")
      
      ValueData[ElementCode == "delta", ElementCode := "5071"]
      ValueData[ElementCode == "new_opening", ElementCode := "5113"]
      
      FlagData = melt(updated_stock[,-c("delta","new_opening")], 
                      id.vars = c("CPCCode","Year"),
                      variable.name = "Flag_Des", value.name = "Flag")
      
      FlagData[, Flag_Des := NULL]
      
      
      
      update_Stock_final_data <- cbind(ValueData, FlagData[,"Flag"])
      
      
      update_Stock_final_data <- merge(update_Stock_final_data,all_cpc, by="CPCCode",all.x = T)
      
      update_Stock_final_data <- merge(update_Stock_final_data,all_elements, by="ElementCode",all.x = T)
      
      
      stockData <- rbind(data_2013,update_Stock_final_data)
      
      stockData = stockData[order(CPCCode,Year)]
    
      
      
      
      # stockData <- stockData[!is.na(Value)]
      
      
      stockData=wide_format(stockData)
      
      flagcols <- grep("^Flag", names(stockData), value = TRUE)
      yearcols <- grep("^[[:digit:]]{4}$", names(stockData), value = TRUE)
      
      minyear <- min(as.numeric(yearcols))
      maxyear <- max(as.numeric(yearcols))
      
      
      
      
      if(END_YEAR > maxyear +1){
        END_YEAR=as.numeric(END_YEAR)
        yearsToFill = (maxyear + 1):END_YEAR
        
        df_stock$data_stock <- NULL
        if(length(yearsToFill) > 0){
          # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
          
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile Stock Change data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
          
          
          
        }
        
      } else {
        
        stockData = visualize_data(stockData,END_YEAR, session)
        
        stockData <-stockData[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
        
        stockData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
        
        # write.csv(stockData,"stock_test.csv", row.names = FALSE)
        
        df_stock$data_stock <- stockData
        
        Add_table_version("stock", copy(df_stock$data_stock))
      }
      
      
      
      
   # }
  })


  
 #computation of opening stock button 
  
  
  observeEvent(input$open_stock_computation, {
    
    
    stockData <- data.table(df_stock$data_stock) 
 
      END_YEAR=input$endyear
      
      # stockData <- fread("stock_test.csv")
      
      stockData[, hidden := NULL]
      
      stockData <- long_format(stockData)
      
      x <- copy(stockData[Year %in% c(2014 : END_YEAR)])
      x[,c("Commodity","Element") := NULL]
      x <- dcast.data.table(x, CPCCode+ Year ~ ElementCode, value.var = c("Value","Flag"))
      
      setnames(x, c("Value_5071","Value_5113"),c("delta","new_opening"))
      
      x[is.na(delta), delta := 0]
      
      x[is.na(new_opening), new_opening := 0]
      
      x <- x[order(CPCCode, Year)]
      
      groups <- unique(x[, .(CPCCode)])
      
      res <- list()
      
      for (i in seq_len(nrow(groups))) {
        z <- x[groups[i], on = c( "CPCCode")]
        
        # print(i)
        if (nrow(z) > 1) {
          for (j in seq_len(nrow(z))[-1]) {
            
            # print(j)
            # negative delta cannot be more than opening
            if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
              z$delta[j-1] <- - z$new_opening[j-1]
            }
            z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
          }
          # negative delta cannot be more than opening
          if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
            z$delta[j] <- - z$new_opening[j]
          }
        }
        res[[i]] <- z
      }
      
      
      data_2013 <- stockData[Year %in% c(2010:2013)]
      
      updated_stock <- rbindlist(res)
      
      
      
      ValueData = melt(updated_stock[,-c("Flag_5071","Flag_5113")], 
                       id.vars = c("CPCCode","Year"),
                       variable.name = "ElementCode", value.name = "Value")
      
      ValueData[ElementCode == "delta", ElementCode := "5071"]
      ValueData[ElementCode == "new_opening", ElementCode := "5113"]
      
      FlagData = melt(updated_stock[,-c("delta","new_opening")], 
                      id.vars = c("CPCCode","Year"),
                      variable.name = "Flag_Des", value.name = "Flag")
      
      FlagData[, Flag_Des := NULL]
      
      
      
      update_Stock_final_data <- cbind(ValueData, FlagData[,"Flag"])
      
      
      update_Stock_final_data <- merge(update_Stock_final_data,all_cpc, by="CPCCode",all.x = T)
      
      update_Stock_final_data <- merge(update_Stock_final_data,all_elements, by="ElementCode",all.x = T)
      
      
      stockData <- rbind(data_2013,update_Stock_final_data)
      
      stockData = stockData[order(CPCCode,Year)]
 
      
      stockData=wide_format(stockData)
      
      
      
      # It is needed a validation for opening stock and stock variation. If stock var <0 , abs(stock) < opening stock.
      
      stock_validation <- copy(stockData)
      
      stock_validation <- long_format(stock_validation)
      
      yearval <- c(as.numeric(input$fromyear):as.numeric(input$endyear))
      # print(yearval)
      
      stock_validation[,validation := ifelse(Year %in% yearval & Value[ElementCode == "5071"]<0 &
                                               Value[ElementCode == "5113"]< abs(Value[ElementCode == "5071"]),1,0),
                       by = c("CPCCode","Year")]
      
      
      stock_validation <- stock_validation[validation == 1] 
      
      
      if (nrow(stock_validation)>0){
        
        CommoditiesTocheck <- unique(stock_validation$Commodity)
        
        sendSweetAlert(
          session = session,
          title = "Issue detected in Stock Variation!!",
          text = paste(c("Please check commodities:", CommoditiesTocheck),collapse= " "),
          type = "Error"
        )
        
      }  else {
      
      
      stockData = visualize_data(stockData,END_YEAR, session)
      
      stockData <-stockData[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
      
      stockData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      
      # write.csv(stockData,"stock_test.csv", row.names = FALSE)
      
      df_stock$data_stock <- stockData
      
      } 

    
    
})

  
  

observeEvent(input$add_Stock, {
  showModal(viewStockTriplets())
})


viewStockTriplets <- function(failed = FALSE) {
  
  
  modalDialog(
    
    easyClose = TRUE, size= "l",
    dataTableOutput("viewStock")
    ,
    
    footer = tagList(
      
      actionButton("stockInsert", "Insert")
    )
  )
  
}



output$viewStock= renderDataTable({
  

  
  
  # if (is.null(values_stockCommodities$stocksCommodities)) {
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_stock$data_stock[ElementCode == "5071"]$CPCCode) )
    values_stockCommodities$stocksCommodities <- DT
  # }
  
  # if (input$stockinsertcommodities >0 ){
  #   values_stockCommodities$stocksCommodities <- subset(values_stockCommodities$stocksCommodities,  !CPCCode %in% unique(data.table(df_stock$data_stock)$CPCCode) )
  # }
  # 
  
  datatable(values_stockCommodities$stocksCommodities,
            escape=F)

  
})


observeEvent(input$stockInsert, {
  
  removeModal()
  
})


proxy_stock = dataTableProxy('stock')

observeEvent(input$stock_cell_edit, {
  
  
  info = input$stock_cell_edit
  
  print(info)
  i = info$row
  j = (info$col + 1)
  v = info$value
  df_stock$data_stock[i,(j) := v]
  
  replaceData(proxy_stock, df_stock$data_stock, resetPaging = FALSE,rownames = FALSE)  # important
  
  info1 <- input[["stock_cell_edit"]]
  i <- info1[["row"]]
  j <- info1[["col"]]
  runjs(colorizeCell(i, j+1,"stock"))
  
  Add_table_version("stock", copy(df_stock$data_stock))
})





observeEvent(input$stockInsert, {
  
  
  s=as.numeric(input$viewStock_rows_selected)
  
  print(s)
  
  if (length(s) == 0){
    
    data_current <- data.table(df_stock$data_stock)
    
    
    
    df_stock$data_stock <- data_current
    
    
  }
  
  else {
    
   
    stocklistTool <- copy(all_cpc)
    stocklistTool <- stocklistTool[order(CPCCode),]
    
    stocklistTool <- subset(stocklistTool,  !CPCCode %in% 
                              unique(isolate(df_stock$data_stock[ElementCode == "5071"]$CPCCode) ))
    
   
    
    yy=stocklistTool[s,]
    
    # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
    ff[, ElementCode := c("5071")]
    
    ########## add also opening stocks ########################
    
    aa <- copy(ff)
    
    aa[, ElementCode := c("5113")]
    
    ff <- rbind(ff,aa)
    
    ##########################################################
    
    
    
    
    oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
    setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
    # oo=oo[order(CPCCode)]
    oo <-oo[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
    
    data=isolate(df_stock$data_stock[ElementCode %in% c("5071","5113")])
    data=data.table(data)
    data[, hidden := NULL]
    
    if (!(unique(oo$CPCCode) %in% data$CPCCode)){
      
      data=rbind(oo,data,fill=T)
      
      
      data=data[!is.na(ElementCode)]
      data[is.na(data)] <- ""
      yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
      
      data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
      
      # data[is.na(data)] <- 0
      
    }
    
    data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_stock$data_stock <- data
    
    
    
  }
  
  
  Add_table_version("stock", copy(df_stock$data_stock))
  
})


#delete rows in crop table

observeEvent(input$delete_btn_Stock, {
  t = copy(df_stock$data_stock)
  final_data <- copy(df_stock$data_stock)
  
  if (!is.null(input$stock_rows_selected)) {
    t <- t[as.numeric(input$stock_rows_selected),]
    
    
    cpc_code_to_remove <- unique(t$CPCCode)
    ele_code_to_remove <- unique(t$ElementCode)
    
  }
  
  
  df_stock$data_stock<- final_data[!CPCCode %in% cpc_code_to_remove]
  
  #remove the cpc with the element from the database
  
  database <- copy(countryData)
  
  database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% c( "5113","5071"))]
  
  # save(database, file = "Data/countrySUA.RData")
  
  countryData <<- database
  
  Add_table_version("stock", copy(df_stock$data_stock))
})


#download excle file (#downloadCrop)

output$downloadStock<- downloadHandler(
  
  filename = function() {
    
    "stock_change.xlsx"
  },
  
  
  content = function(file) {
    
    data_download_stock <- data.table(df_stock$data_stock)
    
    data_download_stock <- data_download_stock[!is.na(CPCCode)]
    
    data_download_stock[,hidden := NULL]
    
    write.xlsx(data_download_stock ,file,row.names = FALSE)
  }
  
)


# #upload crop denormalized data (#fileCrop)

observeEvent(input$fileStockdenormalized,{


  inFile <- input$fileStockdenormalized
 
  
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
  
  
  
  
  # write.csv(data.table(df_stock$data_stock),"crop_Data.csv",row.names = F)
  
  # crop_Data <- fread("crop_Data.csv")
  
  stock_Data <- data.table(df_stock$data_stock)
  
  
  stock_Data <- long_format(stock_Data)
  
  stock_Data[, c("Commodity","Element") := NULL]
  
  stock_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
  
  
  
  xx=stock_Data[!is.na(Value)][
    data_denormalized,
    on = c("CPCCode", "ElementCode", "Year")
    
    ]
  
  
  xx[, c("Value","Flag"):= NULL]
  
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  
  stock_Data <- stock_Data[
    !xx,
    on = c("CPCCode", "ElementCode", "Year")
    
    ]
  
  stock_Data <- rbind(stock_Data,xx)
  
  stock_Data <- merge(stock_Data, all_elements, by = "ElementCode", all.x = T)
  
  stock_Data <- merge(stock_Data, all_cpc, by= "CPCCode", all.x = T)
  
  stock_Data <- stock_Data[!is.na(Element)]
  
  stock_Data <- subset(stock_Data, Year %in% 2010:END_YEAR)
  
  stock_Data <- wide_format(stock_Data)
  
  stock_Data = visualize_data(stock_Data,END_YEAR)

  
  
  stock_Data <-stock_Data[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
  
  stock_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]

  df_stock$data_stock <- stock_Data
  
  
  Add_table_version("stock", copy(df_stock$data_stock))

})



observeEvent(input$uploadStockModal, {
  showModal(uploadStock())
})



uploadStock <- function(failed = FALSE) {
  
  
  modalDialog(size = "l",
              
              
              titlePanel("Upload File"),
              
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Select a file ----
                  fileInput("fileStock", "Choose Excel File",
                            multiple = TRUE,
                            accept = NULL),
                  # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                  
                  selectizeInput("cpcStock", "CPC Code",
                                 selected = NULL, choices = c("",colnames( df_stockCountry$data_stockCountry)),multiple=F),
                  
                  selectizeInput("elementStock", "Element Code",
                                 selected = NULL, choices = c("",colnames( df_stockCountry$data_stockCountry)) ,multiple=F),
                  selectizeInput("yearStock", "Year :",
                                 selected = NULL, choices = c("",colnames( df_stockCountry$data_stockCountry)),multiple=F),
                  
                  selectizeInput("valueStock", "Value :",
                                 selected = NULL, choices = c("",colnames( df_stockCountry$data_stockCountry)),multiple=F),
                  
                  selectizeInput("flagStock", "Flag :",
                                 selected = NULL, choices = c("",colnames( df_stockCountry$data_stockCountry)),multiple=F),
                  
                  actionButton("uploadStock","Upload Stock data")
                  
                ),
                
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Data file ----
                  # dataTableOutput("importCountry")
                  div(style = 'overflow-x: scroll', dataTableOutput('stockCountry'))
                )
                
              )
  )
  
}



output$stockCountry <- renderDataTable({
  
  req(input$fileStock)
  
  
  
  inFile <- input$fileStock
  
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  
  df_stockCountry$data_stockCountry <- DATA
  
  
  datatable(df_stockCountry$data_stockCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  
})


observe({
  
  updateSelectInput(session, "cpcStock", choices = c("", colnames( df_stockCountry$data_stockCountry)))
  updateSelectInput(session, "elementStock", choices = c("",colnames( df_stockCountry$data_stockCountry)))
  updateSelectInput(session, "yearStock", choices = c("",colnames( df_stockCountry$data_stockCountry)))
  updateSelectInput(session, "valueStock", choices = c("",colnames( df_stockCountry$data_stockCountry)))
  updateSelectInput(session, "flagStock", choices = c("",colnames( df_stockCountry$data_stockCountry)))
  
})


#############################################
observeEvent(input$uploadStock, {
  
  removeModal()
  
})

observeEvent(input$uploadStock,{
  
  
  data <- data.table(df_stockCountry$data_stockCountry)
  
  if(input$cpcStock == ""|input$elementStock == ""| input$yearStock == ""| input$valueStock == ""| input$flagStock == "" ){
    sendSweetAlert(
      session = session,
      title = "Warning !!",
      text = "Invalid data",
      type = "warning"
    )
    
    df_stock$data_stock <- df_stock$data_stock
    
  }else{
    
  
  data <- data[, c(input$cpcStock, input$elementStock, input$yearStock, input$valueStock, input$flagStock), with= F]
  
if (length(names(data)[duplicated(names(data))])>0){
 
    
    sendSweetAlert(
      session = session,
      title = "WARNING !!",
      text = "Please select the colums correctly",
      type = "warning"
    )
    
  data <- data.table(df_stockCountry$data_stockCountry)
}
  
else{
  
  # write.csv(data,"test_upload.csv",row.names = F)
  
  setnames(data,c(input$cpcStock, input$elementStock, input$yearStock, input$valueStock, input$flagStock),
           c("CPCCode","ElementCode","Year","Value","Flag"))
  
  
  data <- subset(data, ElementCode %in% c("5071", "5113"))
  
  data[, Year := as.character(Year)]
  data[, CPCCode := as.character(CPCCode)]
  data[, ElementCode := as.character(ElementCode)]
  
  data <- data[Year %in% c(input$fromyear : input$endyear)]
  
  # data[, CPCCode := paste0("0",CPCCode)]
  
  # write.csv(df_stock$data_stock, "cropTest.csv", row.names = FALSE)
  
  # stock <- fread("stockTest.csv")
  
  stock <- data.table(df_stock$data_stock)
  
  stock <- long_format(stock)
  
  stock[,c("Commodity","Element") := NULL]
  
  stock[, ElementCode := as.character(ElementCode)]
  
  xx <- stock[!is.na(Value)][
    data,
    on = c("CPCCode", "ElementCode", "Year")
    
  ]
  
  
  xx[, c("Value","Flag"):= NULL]
  
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  
  
  stock <- stock[!is.na(Value)][
    !xx,
    on = c("CPCCode", "ElementCode", "Year")
    
  ]
  
  stock <- rbind(stock,xx)
  
  stock <- merge(stock, all_elements, by = "ElementCode", all.x = T)
  
  stock <- merge(stock, all_cpc, by= "CPCCode", all.x = T)
  
  stock <- stock[!is.na(Element)]
  
  stock <- wide_format(stock)
  
  
  
  stock <-stock[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
  
  stock[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  
  df_stock$data_stock <- stock
  
  
  
}
  Add_table_version("stock", copy(df_stock$data_stock))
  }
  
})


############################################## 






#save stock

observeEvent(input$saveStock,{
  
  data_to_save <- copy(df_stock$data_stock)
  
  data_to_save <- subset(data_to_save, ElementCode %in% c("5071","5113"))
  
  # write.csv(data_to_save,"stock.csv", row.names = FALSE)
  
  # data_to_save <- fread("stock.csv")
  
  data_to_save[,hidden := NULL]
  
# It is needed a validation for opening stock and stock variation. If stock var <0 , abs(stock) < opening stock.
  
  stock_validation <- copy(data_to_save)
  
  stock_validation <- long_format(stock_validation)
  
  yearval <- c(as.numeric(input$fromyear):as.numeric(input$endyear))
  print(yearval)
  
  stock_validation[,validation := ifelse(Year %in% yearval & Value[ElementCode == "5071"]<0 &
                                           Value[ElementCode == "5113"]< abs(Value[ElementCode == "5071"]),1,0),
                   by = c("CPCCode","Year")]
  
  
  stock_validation <- stock_validation[validation == 1]
  
  
  if (nrow(stock_validation)>0){
    
    CommoditiesTocheck <- unique(stock_validation$Commodity)
    
    sendSweetAlert(
      session = session,
      title = "Issue detected in Stock Variation!!",
      text = paste(c("Please check commodities:", CommoditiesTocheck),collapse= " "),
      type = "Error"
    )
    
    
  }else if (nrow(stock_validation) == 0){
    
    
    
    save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
    
    new_saved_data_stock <- return_data_base(data_to_save)
    
    df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_stock
    
    
    sendSweetAlert(
      session = session,
      title = "Saved !!",
      text = "Your entries have been added to the Data.",
      type = "success"
    )
    
    # saveMessages(input, output, session, buttons= c("saveStock"))
    
  }
  

  
})



observeEvent(input$stock_imputation,{
  
data <- imputeStocksChanges(input,output,session)

 
data <- wide_format(data)


data <- data[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]

# write.csv(df_stock$data_stock, "stock_test.csv",row.names= FALSE)
# 
# data <- fread("stock_test.csv")

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
  
  df_stock$data_stock <- data
  
  Add_table_version("stock", copy(df_stock$data_stock))
  
}



 
  
})












output$stock <-
  

  
  renderDataTable(



    if (!is.null(df_stock$data_stock)){
    datatable (df_stock$data_stock,rownames= FALSE,class = 'cell-border stripe',

               editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_stock$data_stock) == input$fromyear)-2)))
                             ),
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 scrollX = TRUE,
                 scrollY = "500px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(
                                    #list(width = '150px', targets = c(3))
                                   # no hide column
                                   list(visible = FALSE, targets = (ncol(df_stock$data_stock)-1))
                 )
   


               )

               )  %>%

      formatStyle(0:ncol(df_stock$data_stock), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    }

  )

}