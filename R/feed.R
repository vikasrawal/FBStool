

feed <- function(input,output,session){
  
  observeEvent(input$undoFeedratio, {
    
    # get last version
    new_version <- Pop_table_version("feed_ratio")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_feed_ratio$data_feed_ratio <- new_version
    
  })
  
  
  observeEvent(input$undoFeed, {
    
    # get last version
    new_version <- Pop_table_version("feed_values")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_feed$data_feed <- new_version
    
  })
  
  
  
  
  
  
  
 observeEvent(input$startContinue,{

    
      
      
      END_YEAR=input$endyear
      
      feed_ratios<- data.table(read_excel("Data/feedRatio.xlsx"))
      
      feed_ratios <- visualize_data(feed_ratios,END_YEAR, session)
      
      df_feed_ratio$data_feed_ratio <- feed_ratios
      
      Add_table_version("feed_ratio", copy(df_feed_ratio$data_feed_ratio))  


  })
  
  
  
  observeEvent(input$add_Feed_ratio, {
    showModal(viewFeedRatio())
  })
  
  
  viewFeedRatio <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewFeedRatio")
      ,
      
      footer = tagList(
        
        actionButton("FeedRatioInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewFeedRatio= renderDataTable({
    
    
    
    
    # if (is.null(values_stockCommodities$stocksCommodities)) {
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_feed_ratio$data_feed_ratio$CPCCode) )
    values_feedratioCommodities$feedratioCommodities <- DT
    # }
    
    # if (input$stockinsertcommodities >0 ){
    #   values_stockCommodities$stocksCommodities <- subset(values_stockCommodities$stocksCommodities,  !CPCCode %in% unique(data.table(df_feed_ratio$data_feed_ratio)$CPCCode) )
    # }
    # 
    
    datatable(values_feedratioCommodities$feedratioCommodities ,
              escape=F)
    
    
  })
  
  
  observeEvent(input$FeedRatioInsert, {
    
    removeModal()
    
  })
  
  
  proxy_feed_ratio = dataTableProxy('feed_ratio')
  
  observeEvent(input$feed_ratio_cell_edit, {
    
    
    info = input$feed_ratio_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_feed_ratio$data_feed_ratio[i,(j) := v]
    
    replaceData(proxy_feed_ratio, df_feed_ratio$data_feed_ratio, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["feed_ratio_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"feed_ratio"))
    
    Add_table_version("feed_ratio", copy(df_feed_ratio$data_feed_ratio))  
    
  })
  
  
  
  
  observeEvent(input$FeedRatioInsert, {
    
    
    s=as.numeric(input$viewFeedRatio_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_feed_ratio$data_feed_ratio)
      
      
      
      df_feed_ratio$data_feed_ratio <- data_current
      
      
    }
    
    else {
      
      
      feedRatiolistTool <- copy(all_cpc)
      feedRatiolistTool <- feedRatiolistTool[order(CPCCode),]
      
      feedRatiolistTool <- subset(feedRatiolistTool,  !CPCCode %in% unique(isolate(df_feed_ratio$data_feed_ratio$CPCCode) ))
      
      yy=feedRatiolistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := "xxxx"]
      
      
      oo <- ff[,Element := "Feed Ratio [%]"]
      # oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_feed_ratio$data_feed_ratio)
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
      
      # data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      
      df_feed_ratio$data_feed_ratio <- data
      
      Add_table_version("feed_ratio", copy(df_feed_ratio$data_feed_ratio))  
      
    }
    
    
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_feed_ratio, {
    t = copy(df_feed_ratio$data_feed_ratio)
    
    
    if (!is.null(input$feed_ratio_rows_selected)) {
      t <- t[-as.numeric(input$feed_ratio_rows_selected),]
    }
    df_feed_ratio$data_feed_ratio<- t
    
    Add_table_version("feed_ratio", copy(df_feed_ratio$data_feed_ratio))  
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadFeedRatio<- downloadHandler(
    
    filename = function() {
      
      "feed_ratios.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_feed_ratio <- data.table(df_feed_ratio$data_feed_ratio)
      
      data_download_feed_ratio <- data_download_feed_ratio[!is.na(CPCCode)]
      
      # data_download_feed_ratio[,hidden := NULL]
      
      write.xlsx(data_download_feed_ratio ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileFeedRatiodenormalized,{
    
    
    inFile <- input$fileFeedRatiodenormalized
    
    
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
    
    
    
    
    # write.csv(data.table(df_feed_ratio$data_feed_ratio),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    feed_ratio_Data <- data.table(df_feed_ratio$data_feed_ratio)
    
    
    feed_ratio_Data <- long_format(feed_ratio_Data)
    
    feed_ratio_Data[, c("Commodity","Element") := NULL]
    
    feed_ratio_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=feed_ratio_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    feed_ratio_Data <- feed_ratio_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    feed_ratio_Data <- rbind(feed_ratio_Data,xx)
    
    feed_ratio_Data[, Element := "Feed Ratio [%]"]
    
    # feed_ratio_Data <- merge(feed_ratio_Data, all_elements, by = "ElementCode", all.x = T)
    
    feed_ratio_Data <- merge(feed_ratio_Data, all_cpc, by= "CPCCode", all.x = T)
    
    feed_ratio_Data <- feed_ratio_Data[!is.na(Element)]
    
    feed_ratio_Data <- subset(feed_ratio_Data, Year %in% 2010:END_YEAR)
    
    feed_ratio_Data <- wide_format(feed_ratio_Data)
    
    feed_ratio_Data = visualize_data(feed_ratio_Data,END_YEAR)
    
    # feed_ratio_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_feed_ratio$data_feed_ratio <- feed_ratio_Data
    
    Add_table_version("feed_ratio", copy(df_feed_ratio$data_feed_ratio))  
    
  })
  
  
  
  observeEvent(input$uploadFeedRatioModal, {
    showModal(uploadFeedRatio())
  })
  
  
  
  uploadFeedRatio <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileFeedRatio", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcFeedratio", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)),multiple=F),
                    
                    selectizeInput("elementFeedratio", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)) ,multiple=F),
                    selectizeInput("yearFeedratio", "Year :",
                                   selected = NULL, choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)),multiple=F),
                    
                    selectizeInput("valueFeedratio", "Value :",
                                   selected = NULL, choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)),multiple=F),
                    
                    selectizeInput("flagFeedratio", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)),multiple=F),
                    
                    actionButton("uploadFeedratio","Upload Feed ratio")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    # dataTableOutput("importCountry")
                    div(style = 'overflow-x: scroll', dataTableOutput('FeedratioCountry'))
                  )
                  
                )
    )
    
  }
  
  
  
  output$FeedratioCountry <- renderDataTable({
    
    req(input$fileFeedRatio)
    
    
    
    inFile <- input$fileFeedRatio
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_feed_ratioCountry$data_feed_ratioCountry <- DATA
    
    
    datatable(df_feed_ratioCountry$data_feed_ratioCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcFeedratio", choices = c("", colnames( df_feed_ratioCountry$data_feed_ratioCountry)))
    updateSelectInput(session, "elementFeedratio", choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)))
    updateSelectInput(session, "yearFeedratio", choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)))
    updateSelectInput(session, "valueFeedratio", choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)))
    updateSelectInput(session, "flagFeedratio", choices = c("",colnames( df_feed_ratioCountry$data_feed_ratioCountry)))
    
  })
  
  
  #############################################
  
  observeEvent(input$uploadFeedratio, {
    
    removeModal()
    
  })
  
  
  observeEvent(input$uploadFeedratio,{
    
    
    data <- data.table(df_feed_ratioCountry$data_feed_ratioCountry)
    
    if(input$cpcFeedratio == ""|input$elementFeedratio == ""| input$yearFeedratio == ""| input$valueFeedratio == ""| input$flagFeedratio == "" ){
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_feed_ratio$data_feed_ratio <- df_feed_ratio$data_feed_ratio
      
    }else{
      
    
    
    
    
    data <- data[, c(input$cpcFeedratio, input$elementFeedratio, input$yearFeedratio, input$valueFeedratio, input$flagFeedratio), with= F]
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_feed_ratioCountry$data_feed_ratioCountry)
    }else{
    
    
    setnames(data,c(input$cpcFeedratio, input$elementFeedratio, input$yearFeedratio, input$valueFeedratio, input$flagFeedratio),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    
    data <- subset(data, ElementCode %in% c("xxxx"))
    
    
    
    # data <- subset(data, ElementCode %in% c("5071"))
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    feed_ratio_ <- data.table(df_feed_ratio$data_feed_ratio)
    
    feed_ratio_ <- long_format(feed_ratio_)
    
    feed_ratio_[,c("Commodity","Element") := NULL]
    
    feed_ratio_[, ElementCode := as.character(ElementCode)]
    
    xx <- feed_ratio_[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    feed_ratio_ <- feed_ratio_[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    feed_ratio_ <- rbind(feed_ratio_,xx)
    
    feed_ratio_[, Element := "Feed  Ratio [%]"]
    
    # feed_ratio_ <- merge(feed_ratio_, all_elements, by = "ElementCode", all.x = T)
    
    feed_ratio_ <- merge(feed_ratio_, all_cpc, by= "CPCCode", all.x = T)
    
    feed_ratio_ <- feed_ratio_[!is.na(Element)]
    
    feed_ratio_ <- wide_format(feed_ratio_)
    
    # feed_ratio_[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_feed_ratio$data_feed_ratio <- feed_ratio_
    
    Add_table_version("feed_ratio", copy(df_feed_ratio$data_feed_ratio))  
    
    }
    }
    
  })
  
  
  ############################################## 

  
  observeEvent(input$saveFeedratio,{
    
   data_to_save <- df_feed_ratio$data_feed_ratio
    
    write.xlsx(data_to_save,"Data/feedRatio.xlsx",row.names = F)
    
  })
  
  
  


  output$feed_ratio <-
    renderDataTable({

      # x <- grep("^[[:digit:]]{4}$", names(df_feed_ratio$data_feed_ratio), value = TRUE)
      # col_width <- set_hot_colwidths(df_feed_ratio$data_feed_ratio)
     
      datatable (df_feed_ratio$data_feed_ratio, rownames= FALSE,class = 'cell-border stripe',

                 editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_feed_ratio$data_feed_ratio) 
                                                                                      == input$fromyear)-2)))),
                 extensions = c("FixedColumns","FixedHeader","Buttons"),
                 options = list(

                   # paging = TRUE, searching = TRUE, info = FALSE,
                   # sort = TRUE,
                   scrollX = TRUE,
                   scrollY = "500px" ,
                   dom= 'Blfrtip', buttons = I('colvis'),
                   autoWidth = T,
                   fixedColumns = list(leftColumns = 4),
                   columnDefs = list(list(width = '', targets =  c(6))
                                     # list(visible = FALSE, targets = (ncol(df_feed_ratio$data_feed_ratio)-1))
                                     
                                     )
                 ))%>%
        formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")

   } )
  
#################################################### feed data ###############################################################
  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Feed') {
      
      END_YEAR=input$endyear
      
      feedData=subset(countryData, ElementCode %in% c("5520"))
      
      setDT(feedData)
      
      feedData <- feedData[!duplicated(feedData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
      
      feedData[, c("CountryM49","Country"):=NULL]
      
      feedData <- subset(feedData, Year %in% c(2010 : END_YEAR) )
      
      # feedData <- feedData[!is.na(Value)]
      
      
      feedData=wide_format(feedData)
   
      flagcols <- grep("^Flag", names(feedData), value = TRUE)
      yearcols <- grep("^[[:digit:]]{4}$", names(feedData), value = TRUE)
      
      minyear <- min(as.numeric(yearcols))
      maxyear <- max(as.numeric(yearcols))
      
      
      
      
      if(END_YEAR > maxyear +1){
        END_YEAR=as.numeric(END_YEAR)
        yearsToFill = (maxyear + 1):END_YEAR
        
        df_feed$data_feed <- NULL
        if(length(yearsToFill) > 0){
          # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
          
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile Feed data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
          
          
          
        }
        
      } else {
        
        feedData = visualize_data(feedData,END_YEAR, session)
        
        feedData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
        
        df_feed$data_feed <- feedData
        
        Add_table_version("feed_values", copy(df_feed$data_feed)) 
      }
      
      

      
    # }
  })
  
  
  
  observeEvent(input$add_Feed, {
    showModal(viewFeedTriplets())
  })
  
  
  viewFeedTriplets <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewFeed")
      ,
      
      footer = tagList(
        
        actionButton("feedInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewFeed= renderDataTable({
    
    
    
    
    
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_feed$data_feed$CPCCode) )
    values_feedCommodities$feedCommodities <- DT
    # }
    
  
    
    datatable(values_feedCommodities$feedCommodities,
              escape=F)
    
    
  })
  
  
  observeEvent(input$feedInsert, {
    
    removeModal()
    
  })
  
  
  proxy_feed = dataTableProxy('feed_values')
  
  observeEvent(input$feed_values_cell_edit, {
    
    
    info = input$feed_values_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_feed$data_feed[i,(j) := v]
    
    replaceData(proxy_feed, df_feed$data_feed, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["feed_values_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"feed_values"))
    
    Add_table_version("feed_values", copy(df_feed$data_feed)) 
    
  })
  
  
  
  
  observeEvent(input$feedInsert, {
    
    
    s=as.numeric(input$viewFeed_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_feed$data_feed)
      
      
      
      df_feed$data_feed <- data_current
      
      
    }
    
    else {
      
      
      feedlistTool <- copy(all_cpc)
      feedlistTool <- feedlistTool[order(CPCCode),]
      
      feedlistTool <- subset(feedlistTool,  !CPCCode %in% unique(isolate(df_feed$data_feed$CPCCode) ))
      
      yy=feedlistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := c("5520")]
      
      
      
      oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_feed$data_feed)
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
      
      df_feed$data_feed <- data
      Add_table_version("feed_values", copy(df_feed$data_feed)) 
      
    }
    
    
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_feed, {
    t = copy(df_feed$data_feed)
    final_data <- copy(df_feed$data_feed)
    
    if (!is.null(input$feed_values_rows_selected)) {
      t <- t[as.numeric(input$feed_values_rows_selected),]
      
      cpc_code_to_remove <- unique(t$CPCCode)
      ele_code_to_remove <- unique(t$ElementCode)
    }
  
    
    df_feed$data_feed<- final_data[!CPCCode %in% cpc_code_to_remove]
    
    #remove the cpc with the element from the database
    
    database <- copy(countryData)
    
    database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% ele_code_to_remove)]
    
    # save(database, file = "Data/countrySUA.RData")
    
    countryData <<- database
    
    Add_table_version("feed_values", copy(df_feed$data_feed)) 
    
    
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadFeed<- downloadHandler(
    
    filename = function() {
      
      "feed_data.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_feed <- data.table(df_feed$data_feed)
      
      data_download_feed <- data_download_feed[!is.na(CPCCode)]
      
      data_download_feed[,hidden := NULL]
      
      write.xlsx(data_download_feed ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileFeeddenormalized,{
    
    
    inFile <- input$fileFeeddenormalized
    
    
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
    
    
    
    
    # write.csv(data.table(df_feed$data_feed),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    feed_Data <- data.table(df_feed$data_feed)
    
    
    feed_Data <- long_format(feed_Data)
    
    feed_Data[, c("Commodity","Element") := NULL]
    
    feed_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=feed_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    feed_Data <- feed_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    feed_Data <- rbind(feed_Data,xx)
    
    feed_Data <- merge(feed_Data, all_elements, by = "ElementCode", all.x = T)
    
    feed_Data <- merge(feed_Data, all_cpc, by= "CPCCode", all.x = T)
    
    feed_Data <- feed_Data[!is.na(Element)]
    
    feed_Data <- subset(feed_Data, Year %in% 2010:END_YEAR)
    
    feed_Data <- wide_format(feed_Data)
    
    feed_Data = visualize_data(feed_Data,END_YEAR)
    
    feed_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_feed$data_feed <- feed_Data
    
    Add_table_version("feed_values", copy(df_feed$data_feed)) 
    
  })
  
  
  
  observeEvent(input$uploadFeedModal, {
    showModal(uploadFeed())
  })
  
  
  
  uploadFeed <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileFeed", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcFeed", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_feedCountry$data_feedCountry)),multiple=F),
                    
                    selectizeInput("elementFeed", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_feedCountry$data_feedCountry)) ,multiple=F),
                    selectizeInput("yearFeed", "Year :",
                                   selected = NULL, choices = c("",colnames( df_feedCountry$data_feedCountry)),multiple=F),
                    
                    selectizeInput("valueFeed", "Value :",
                                   selected = NULL, choices = c("",colnames( df_feedCountry$data_feedCountry)),multiple=F),
                    
                    selectizeInput("flagFeed", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_feedCountry$data_feedCountry)),multiple=F),
                    
                    actionButton("uploadFeed","Upload Feed data")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                  
                    div(style = 'overflow-x: scroll', dataTableOutput('feedCountry'))
                  )
                  
                )
    )
    
  }
  
  
  
  output$feedCountry <- renderDataTable({
    
    req(input$fileFeed)
    
    
    
    inFile <- input$fileFeed
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_feedCountry$data_feedCountry <- DATA
    
    
    datatable(df_feedCountry$data_feedCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcFeed", choices = c("", colnames( df_feedCountry$data_feedCountry)))
    updateSelectInput(session, "elementFeed", choices = c("",colnames( df_feedCountry$data_feedCountry)))
    updateSelectInput(session, "yearFeed", choices = c("",colnames( df_feedCountry$data_feedCountry)))
    updateSelectInput(session, "valueFeed", choices = c("",colnames( df_feedCountry$data_feedCountry)))
    updateSelectInput(session, "flagFeed", choices = c("",colnames( df_feedCountry$data_feedCountry)))
    
  })
  
  
  #############################################
  
  observeEvent(input$uploadFeed, {
    
    removeModal()
    
  })
  
  
  
  
  
  
  observeEvent(input$uploadFeed,{
    
    
    data <- data.table(df_feedCountry$data_feedCountry)
    
    if(input$cpcFeed == ""|input$elementFeed == ""| input$yearFeed == ""| input$valueFeed == ""| input$flagFeed == "" ){
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_feed$data_feed <- df_feed$data_feed
      
    }else{
      
    
    data <- data[, c(input$cpcFeed, input$elementFeed, input$yearFeed, input$valueFeed, input$flagFeed), with= F]
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_feedCountry$data_feedCountry)
    }
    else{
    
    
    setnames(data,c(input$cpcFeed, input$elementFeed, input$yearFeed, input$valueFeed, input$flagFeed),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    data <- subset(data, ElementCode %in% c("5520"))
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    feed_values <- data.table(df_feed$data_feed)
    
    feed_values <- long_format(feed_values)
    
    feed_values[,c("Commodity","Element") := NULL]
    
    feed_values[, ElementCode := as.character(ElementCode)]
    
    xx <- feed_values[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    feed_values <- feed_values[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    feed_values <- rbind(feed_values,xx)
    
    feed_values <- merge(feed_values, all_elements, by = "ElementCode", all.x = T)
    
    feed_values <- merge(feed_values, all_cpc, by= "CPCCode", all.x = T)
    
    feed_values <- feed_values[!is.na(Element)]
    
    feed_values <- wide_format(feed_values)
    
    feed_values[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_feed$data_feed <- feed_values
    
    Add_table_version("feed_values", copy(df_feed$data_feed)) 
    
    }
    }
  })
  
  
  ############################################## 
  
  
  
  
  
  
  #save stock
  
  observeEvent(input$saveFeed,{
    
    data_to_save <- df_feed$data_feed
    
    data_to_save <- subset(data_to_save, ElementCode == "5520")
    
    save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
    
    new_saved_data <- return_data_base(data_to_save)
    
    df_sua_unbalanced$data_sua_unbalanced <- new_saved_data
    
    
})
  
  
  
  observeEvent(input$feed_imputation_ratio,{
    
    
    data <- imputationFeedRatio(input,output,session)
    
    df_feed_ratio$data_feed_ratio <- data
    
    Add_table_version("feed_ratio", copy(df_feed_ratio$data_feed_ratio))
    
  })
  
  
  observeEvent(input$feed_imputation,{
    
    
    data <- imputeFeed(input,output,session)
    
    
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
      
      df_feed$data_feed <- data
      
      Add_table_version("feed_values", copy(df_feed$data_feed))
      
    }
    
   
   
    
    
  })
  
  
  
  
  
  output$feed_values <- 
    renderDataTable(
      
      if (!is.null(df_feed$data_feed)){
        datatable (df_feed$data_feed, rownames= FALSE,class = 'cell-border stripe', 
                   
            editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_feed$data_feed) == input$fromyear)-2)))),
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
                       list(visible = FALSE, targets = (ncol(df_feed$data_feed)-1))
                     )
                   ))  %>%
          
          formatStyle(0:ncol(df_feed$data_feed), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
      
    )
  
  
}