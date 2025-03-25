

loss <- function(input,output,session){
  
  
  
  observeEvent(input$undoLossratio, {
    
    # get last version
    new_version <- Pop_table_version("loss_ratio")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_loss_ratio$data_loss_ratio <- new_version
    
  })
  
  
  observeEvent(input$undoLoss, {
    
    # get last version
    new_version <- Pop_table_version("loss_values")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_loss$data_loss <- new_version
    
  })
  
  
  
 observeEvent(input$startContinue,{

    # if(input$fao == 'Loss'){
      
      
      END_YEAR=input$endyear
      
      loss_ratios<- data.table(read_excel("Data/lossRatio.xlsx"))
      
      loss_ratios <- visualize_data(loss_ratios,END_YEAR, session)
      
      
     
      df_loss_ratio$data_loss_ratio <- loss_ratios
      
      
      Add_table_version("loss_ratio", copy(df_loss_ratio$data_loss_ratio)) 


  })
  
  
  
  observeEvent(input$add_Loss_ratio, {
    showModal(viewLossRatio())
  })
  
  
  viewLossRatio <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewLossRatio")
      ,
      
      footer = tagList(
        
        actionButton("LossRatioInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewLossRatio= renderDataTable({
    
    
    
    
    # if (is.null(values_stockCommodities$stocksCommodities)) {
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_loss_ratio$data_loss_ratio$CPCCode) )
    values_lossratioCommodities$lossratioCommodities <- DT
    # }
    
    # if (input$stockinsertcommodities >0 ){
    #   values_stockCommodities$stocksCommodities <- subset(values_stockCommodities$stocksCommodities,  !CPCCode %in% unique(data.table(df_loss_ratio$data_loss_ratio)$CPCCode) )
    # }
    # 
    
    datatable(values_lossratioCommodities$lossratioCommodities ,
              escape=F)
    
    
  })
  
  
  observeEvent(input$LossRatioInsert, {
    
    removeModal()
    
  })
  
  
  proxy_loss_ratio = dataTableProxy('loss_ratio')
  
  observeEvent(input$loss_ratio_cell_edit, {
    
    
    info = input$loss_ratio_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_loss_ratio$data_loss_ratio[i,(j) := v]
    
    replaceData(proxy_loss_ratio, df_loss_ratio$data_loss_ratio, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["loss_ratio_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"loss_ratio"))
    
    
    Add_table_version("loss_ratio", copy(df_loss_ratio$data_loss_ratio)) 
    
  })
  
  
  
  
  observeEvent(input$LossRatioInsert, {
    
    
    s=as.numeric(input$viewLossRatio_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_loss_ratio$data_loss_ratio)
      
      
      
      df_loss_ratio$data_loss_ratio <- data_current
      
      
    }
    
    else {
      
      
      lossRatiolistTool <- copy(all_cpc)
      lossRatiolistTool <- lossRatiolistTool[order(CPCCode),]
      
      lossRatiolistTool <- subset(lossRatiolistTool,  !CPCCode %in% unique(isolate(df_loss_ratio$data_loss_ratio$CPCCode) ))
      
      yy=lossRatiolistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := "xxxx"]
      
      
      oo <- ff[,Element := "Loss Ratio [%]"]
      # oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_loss_ratio$data_loss_ratio)
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
      
      # data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      
      df_loss_ratio$data_loss_ratio <- data
      
    }
    
    Add_table_version("loss_ratio", copy(df_loss_ratio$data_loss_ratio)) 
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_loss_ratio, {
    t = copy(df_loss_ratio$data_loss_ratio)
    
    
    if (!is.null(input$loss_ratio_rows_selected)) {
      t <- t[-as.numeric(input$loss_ratio_rows_selected),]
    }
    df_loss_ratio$data_loss_ratio<- t
    
    Add_table_version("loss_ratio", copy(df_loss_ratio$data_loss_ratio)) 
    
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadLossRatio<- downloadHandler(
    
    filename = function() {
      
      "loss_ratio.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_loss_ratio <- data.table(df_loss_ratio$data_loss_ratio)
      
      data_download_loss_ratio <- data_download_loss_ratio[!is.na(CPCCode)]
      
      # data_download_loss_ratio[,hidden := NULL]
      
      write.xlsx(data_download_loss_ratio ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileLossRatiodenormalized,{
    
    
    inFile <- input$fileLossRatiodenormalized
    
    
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
    
    
    
    
    # write.csv(data.table(df_loss_ratio$data_loss_ratio),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    loss_ratio_Data <- data.table(df_loss_ratio$data_loss_ratio)
    
    
    loss_ratio_Data <- long_format(loss_ratio_Data)
    
    loss_ratio_Data[, c("Commodity","Element") := NULL]
    
    loss_ratio_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=loss_ratio_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    loss_ratio_Data <- loss_ratio_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    loss_ratio_Data <- rbind(loss_ratio_Data,xx)
    
    loss_ratio_Data[, Element := "Loss Ratio [%]"]
    
    # loss_ratio_Data <- merge(loss_ratio_Data, all_elements, by = "ElementCode", all.x = T)
    
    loss_ratio_Data <- merge(loss_ratio_Data, all_cpc, by= "CPCCode", all.x = T)
    
    loss_ratio_Data <- loss_ratio_Data[!is.na(Element)]
    
    loss_ratio_Data <- subset(loss_ratio_Data, Year %in% 2010:END_YEAR)
    
    loss_ratio_Data <- wide_format(loss_ratio_Data)
    
    loss_ratio_Data = visualize_data(loss_ratio_Data,END_YEAR)
    
    # loss_ratio_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_loss_ratio$data_loss_ratio <- loss_ratio_Data
    
    Add_table_version("loss_ratio", copy(df_loss_ratio$data_loss_ratio)) 
    
  })
  
  
  
  observeEvent(input$uploadlossRatioModal, {
    showModal(uploadLossRatio())
  })
  
  
  
  uploadLossRatio <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileLossRatio", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcLossratio", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)),multiple=F),
                    
                    selectizeInput("elementLossratio", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)) ,multiple=F),
                    selectizeInput("yearLossratio", "Year :",
                                   selected = NULL, choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)),multiple=F),
                    
                    selectizeInput("valueLossratio", "Value :",
                                   selected = NULL, choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)),multiple=F),
                    
                    selectizeInput("flagLossratio", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)),multiple=F),
                    
                    actionButton("uploadLossratio","Upload Loss ratio")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    # dataTableOutput("importCountry")
                    div(style = 'overflow-x: scroll', dataTableOutput('LossratioCountry'))
                  )
                  
                )
    )
    
  }
  
  
  
  output$LossratioCountry <- renderDataTable({
    
    req(input$fileLossRatio)
    
    
    
    inFile <- input$fileLossRatio
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_loss_ratioCountry$data_loss_ratioCountry <- DATA
    
    
    datatable(df_loss_ratioCountry$data_loss_ratioCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcLossratio", choices = c("", colnames( df_loss_ratioCountry$data_loss_ratioCountry)))
    updateSelectInput(session, "elementLossratio", choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)))
    updateSelectInput(session, "yearLossratio", choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)))
    updateSelectInput(session, "valueLossratio", choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)))
    updateSelectInput(session, "flagLossratio", choices = c("",colnames( df_loss_ratioCountry$data_loss_ratioCountry)))
    
  })
  
  
  #############################################
  
  
  observeEvent(input$uploadLossratio,{
    
    
    data <- data.table(df_loss_ratioCountry$data_loss_ratioCountry)
    
    if(input$cpcLossratio == ""|input$elementLossratio == ""| input$yearLossratio == ""| input$valueLossratio == ""| 
       
       input$flagLossratio == "" ){
      
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_loss_ratio$data_loss_ratio <- df_loss_ratio$data_loss_ratio
      
    }else{
      
    
    
    data <- data[, c(input$cpcLossratio, input$elementLossratio, input$yearLossratio, input$valueLossratio, input$flagLossratio), with= F]
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_loss_ratioCountry$data_loss_ratioCountry)
    }else{
    
    
    
    setnames(data,c(input$cpcLossratio, input$elementLossratio, input$yearLossratio, input$valueLossratio, input$flagLossratio),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    
    data <- subset(data, ElementCode %in% c("xxxx"))
    
   
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    loss_ratio_ <- data.table(df_loss_ratio$data_loss_ratio)
    
    loss_ratio_ <- long_format(loss_ratio_)
    
    loss_ratio_[,c("Commodity","Element") := NULL]
    
    loss_ratio_[, ElementCode := as.character(ElementCode)]
    
    xx <- loss_ratio_[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    loss_ratio_ <- loss_ratio_[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    loss_ratio_ <- rbind(loss_ratio_,xx)
    
    loss_ratio_[, Element := "Loss Ratio [%]"]
    
     # loss_ratio_ <- merge(loss_ratio_, all_elements, by = "ElementCode", all.x = T)
    
    loss_ratio_ <- merge(loss_ratio_, all_cpc, by= "CPCCode", all.x = T)
    
    loss_ratio_ <- loss_ratio_[!is.na(Element)]
    
    loss_ratio_ <- wide_format(loss_ratio_)
    
    # loss_ratio_[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_loss_ratio$data_loss_ratio <- loss_ratio_
    }
    
    Add_table_version("loss_ratio", copy(df_loss_ratio$data_loss_ratio)) 
    }  
  })
  
  
  observeEvent(input$uploadLossratio, {
    
    removeModal()
    
  })
  
  
  
  
  ############################################## 

  
  observeEvent(input$saveLossratio,{
    
   data_to_save <- df_loss_ratio$data_loss_ratio
    
   write.xlsx(data_to_save,"Data/lossRatio.xlsx",row.names = F)
    
  })
  
  
  
  observeEvent(input$loss_imputation_ratio,{
    
  data <- imputationLossRatio(input,output,session)
    
  df_loss_ratio$data_loss_ratio <- data
  
  Add_table_version("loss_ratio", copy(df_loss_ratio$data_loss_ratio))
  
  })
  
  
  
  
  

  output$loss_ratio <-
    renderDataTable({

      # x <- grep("^[[:digit:]]{4}$", names(df_loss_ratio$data_loss_ratio), value = TRUE)
      # col_width <- set_hot_colwidths(df_loss_ratio$data_loss_ratio)
     
      datatable (df_loss_ratio$data_loss_ratio, rownames= FALSE,class = 'cell-border stripe',

                 editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_loss_ratio$data_loss_ratio) 
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
                                     # list(visible = FALSE, targets = (ncol(df_loss_ratio$data_loss_ratio)-1))
                                     
                                     
                                     )
                 ))%>%
        formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")

   } )
  
#################################################### loss data ###############################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
  
  
  
  
  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Loss') {
      
      END_YEAR=input$endyear
      
      lossData=subset(countryData, ElementCode %in% c("5016"))
      
      setDT(lossData)
      
      lossData <- lossData[!duplicated(lossData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
      
      lossData[, c("CountryM49","Country"):=NULL]
      
      lossData <- subset(lossData, Year %in% c(2010 : END_YEAR) )
      
      # lossData <- lossData[!is.na(Value)]
      
      
      lossData=wide_format(lossData)
   
      flagcols <- grep("^Flag", names(lossData), value = TRUE)
      yearcols <- grep("^[[:digit:]]{4}$", names(lossData), value = TRUE)
      
      minyear <- min(as.numeric(yearcols))
      maxyear <- max(as.numeric(yearcols))
      
      
      
      
      if(END_YEAR > maxyear +1){
        END_YEAR=as.numeric(END_YEAR)
        yearsToFill = (maxyear + 1):END_YEAR
        
        df_loss$data_loss <- NULL
        if(length(yearsToFill) > 0){
          # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
          
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile Loss data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
          
          
          
        }
        
      } else {
        
        lossData = visualize_data(lossData,END_YEAR, session)
        
        lossData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
        
        df_loss$data_loss <- lossData
        
        Add_table_version("loss_values", copy(df_loss$data_loss))  
        
      }
      
      

      
    # }
  })
  
  
  
  observeEvent(input$add_Loss, {
    showModal(viewLossTriplets())
  })
  
  
  viewLossTriplets <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewLoss")
      ,
      
      footer = tagList(
        
        actionButton("lossInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewLoss= renderDataTable({
    
    
    
    
    
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_loss$data_loss$CPCCode) )
    values_lossCommodities$lossCommodities <- DT
    # }
    
  
    
    datatable(values_lossCommodities$lossCommodities,
              escape=F)
    
    
  })
  
  
  observeEvent(input$lossInsert, {
    
    removeModal()
    
  })
  
  
  proxy_loss = dataTableProxy('loss_values')
  
  observeEvent(input$loss_values_cell_edit, {
    
    
    info = input$loss_values_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_loss$data_loss[i,(j) := v]
    
    replaceData(proxy_loss, df_loss$data_loss, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["loss_values_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"loss_values"))
    
    Add_table_version("loss_values", copy(df_loss$data_loss))
  })
  
  
  
  
  observeEvent(input$lossInsert, {
    
    
    s=as.numeric(input$viewLoss_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_loss$data_loss)
      
      
      
      df_loss$data_loss <- data_current
      
      
    }
    
    else {
      
      
      losslistTool <- copy(all_cpc)
      losslistTool <- losslistTool[order(CPCCode),]
      
      losslistTool <- subset(losslistTool,  !CPCCode %in% unique(isolate(df_loss$data_loss$CPCCode) ))
      
      yy=losslistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := c("5016")]
      
      
      
      oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_loss$data_loss)
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
      
      df_loss$data_loss <- data
      
    }
    
    Add_table_version("loss_values", copy(df_loss$data_loss))  
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_loss, {
    t = copy(df_loss$data_loss)
    final_data <- copy(df_loss$data_loss)
    
    if (!is.null(input$loss_values_rows_selected)) {
      t <- t[as.numeric(input$loss_values_rows_selected),]
      
      cpc_code_to_remove <- unique(t$CPCCode)
      ele_code_to_remove <- unique(t$ElementCode)
    }
  
    
    df_loss$data_loss<- final_data[!CPCCode %in% cpc_code_to_remove]
    
    #remove the cpc with the element from the database
    
    database <- copy(countryData)
    
    database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% ele_code_to_remove)]
    
    # save(database, file = "Data/countrySUA.RData")
    
    countryData <<- database
    Add_table_version("loss_values", copy(df_loss$data_loss))
    
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadLoss<- downloadHandler(
    
    filename = function() {
      
      "loss_data.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_loss <- data.table(df_loss$data_loss)
      
      data_download_loss <- data_download_loss[!is.na(CPCCode)]
      
      data_download_loss[,hidden := NULL]
      
      write.xlsx(data_download_loss ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileLossdenormalized,{
    
    
    inFile <- input$fileLossdenormalized
    
    
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
    
    
    
    
    # write.csv(data.table(df_loss$data_loss),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    loss_Data <- data.table(df_loss$data_loss)
    
    
    loss_Data <- long_format(loss_Data)
    
    loss_Data[, c("Commodity","Element") := NULL]
    
    loss_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=loss_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    loss_Data <- loss_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    loss_Data <- rbind(loss_Data,xx)
    
    loss_Data <- merge(loss_Data, all_elements, by = "ElementCode", all.x = T)
    
    loss_Data <- merge(loss_Data, all_cpc, by= "CPCCode", all.x = T)
    
    loss_Data <- loss_Data[!is.na(Element)]
    
    loss_Data <- subset(loss_Data, Year %in% 2010:END_YEAR)
    
    loss_Data <- wide_format(loss_Data)
    
    loss_Data = visualize_data(loss_Data,END_YEAR)
    
    loss_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_loss$data_loss <- loss_Data
    
    
    Add_table_version("loss_values", copy(df_loss$data_loss))  
    
  })
  
  
  
  observeEvent(input$uploadLossdModal, {
    showModal(uploadLoss())
  })
  
  
  
  uploadLoss <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileLoss", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcLoss", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_lossCountry$data_lossCountry)),multiple=F),
                    
                    selectizeInput("elementLoss", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_lossCountry$data_lossCountry)) ,multiple=F),
                    selectizeInput("yearLoss", "Year :",
                                   selected = NULL, choices = c("",colnames( df_lossCountry$data_lossCountry)),multiple=F),
                    
                    selectizeInput("valueLoss", "Value :",
                                   selected = NULL, choices = c("",colnames( df_lossCountry$data_lossCountry)),multiple=F),
                    
                    selectizeInput("flagLoss", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_lossCountry$data_lossCountry)),multiple=F),
                    
                    actionButton("uploadLoss","Upload Loss data")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    # dataTableOutput("importCountry")
                    div(style = 'overflow-x: scroll', dataTableOutput('lossCountry'))
                  )
                  
                )
    )
    
  }
  
  
  
  output$lossCountry <- renderDataTable({
    
    req(input$fileLoss)
    
    
    
    inFile <- input$fileLoss
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_lossCountry$data_lossCountry <- DATA
    
    
    datatable(df_lossCountry$data_lossCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcLoss", choices = c("", colnames( df_lossCountry$data_lossCountry)))
    updateSelectInput(session, "elementLoss", choices = c("",colnames( df_lossCountry$data_lossCountry)))
    updateSelectInput(session, "yearLoss", choices = c("",colnames( df_lossCountry$data_lossCountry)))
    updateSelectInput(session, "valueLoss", choices = c("",colnames( df_lossCountry$data_lossCountry)))
    updateSelectInput(session, "flagLoss", choices = c("",colnames( df_lossCountry$data_lossCountry)))
    
  })
  
  
  #############################################
  
  observeEvent(input$uploadLoss, {
    
    removeModal()
    
  })
  
  
  
  
  observeEvent(input$uploadLoss,{
    
    
    data <- data.table(df_lossCountry$data_lossCountry)
    
    if(input$cpcLoss == ""|input$elementLoss == ""| input$yearLoss == ""| input$valueLoss == ""| input$flagLoss == "" ){
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_loss$data_loss <- df_loss$data_loss
      
    }else{
      
    
    data <- data[, c(input$cpcLoss, input$elementLoss, input$yearLoss, input$valueLoss, input$flagLoss), with= F]
    
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_lossCountry$data_lossCountry)
    }else{
    
    
    
    setnames(data,c(input$cpcLoss, input$elementLoss, input$yearLoss, input$valueLoss, input$flagLoss),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    
    data <- subset(data, ElementCode %in% c("5016"))
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    loss_values <- data.table(df_loss$data_loss)
    
    loss_values <- long_format(loss_values)
    
    loss_values[,c("Commodity","Element") := NULL]
    
    loss_values[, ElementCode := as.character(ElementCode)]
    
    xx <- loss_values[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    loss_values <- loss_values[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    loss_values <- rbind(loss_values,xx)
    
    loss_values <- merge(loss_values, all_elements, by = "ElementCode", all.x = T)
    
    loss_values <- merge(loss_values, all_cpc, by= "CPCCode", all.x = T)
    
    loss_values <- loss_values[!is.na(Element)]
    
    loss_values <- wide_format(loss_values)
    
    loss_values[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_loss$data_loss <- loss_values
    
    Add_table_version("loss_values", copy(df_loss$data_loss))
    
    }
    
      
    
    }
  })
  
  
  ############################################## 
  
  
  
  
  
  
  #save stock
  
  observeEvent(input$saveLoss,{
    
    data_to_save <- df_loss$data_loss
    
    data_to_save <- subset(data_to_save, ElementCode == "5016")
    
    save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
    
    new_saved_data <- return_data_base(data_to_save)
    
    df_sua_unbalanced$data_sua_unbalanced <- new_saved_data
    
    
    
    
    
  })
  
  
  
  observeEvent(input$loss_imputation,{
    
    
    
    
    data <- imputeLoss(input,output,session)
    
    
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
      
      df_loss$data_loss <- data
      
      Add_table_version("loss_values", copy(df_loss$data_loss))
      
    }
    
   
    
    
  })
  
  
  
  
  
  output$loss_values <- 
    renderDataTable(
      
      if (!is.null(df_loss$data_loss)){
        datatable (df_loss$data_loss, rownames= FALSE,class = 'cell-border stripe', 
                   
            editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_loss$data_loss) == input$fromyear)-2)))),
                   extensions = c("FixedColumns","FixedHeader","Buttons"),
                   options = list(
                     pageLength = 25,
                     
                     # dom='f', ordering=F,
                     # paging = TRUE, searching = TRUE, info = FALSE,
                     # sort = TRUE,
                     scrollX = TRUE,
                     scrollY = "500px" ,
                     dom= 'Blfrtip', buttons = I('colvis'),
                     autoWidth = T,
                     fixedColumns = list(leftColumns = 4),
                     columnDefs = list(
                       #list(width = '150px', targets = c(3))
                       # no hide column
                       list(visible = FALSE, targets = (ncol(df_loss$data_loss)-1))
                     )
                   ))  %>%
          
          formatStyle(0:ncol(df_loss$data_loss), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
      
    )
  
  
}