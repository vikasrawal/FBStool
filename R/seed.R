

seed <- function(input,output,session){
  
  
  
  observeEvent(input$undoSeedratio, {
    
    # get last version
    new_version <- Pop_table_version("seed_ratio")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_seed_ratio$data_seed_ratio <- new_version
    
  })
  
  
  
  
  observeEvent(input$undoSeed, {
    
    # get last version
    new_version <- Pop_table_version("seed_values")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_seed$data_seed <- new_version
    
  })
  
  
  
  
 observeEvent(input$startContinue,{

    # if(input$fao == 'Seed'){
      
      
      END_YEAR=input$endyear
      
      seed_ratios<- data.table(read_excel("Data/seedRate.xlsx"))
      
      seed_ratios <- visualize_data(seed_ratios,END_YEAR, session)
      
      
  
      df_seed_ratio$data_seed_ratio <- seed_ratios
      
      Add_table_version("seed_ratio", copy( df_seed_ratio$data_seed_ratio))  
    


  })
  
  
  
  observeEvent(input$add_Seed_ratio, {
    showModal(viewSeedRatio())
  })
  
  
  viewSeedRatio <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewSeedRatio")
      ,
      
      footer = tagList(
        
        actionButton("SeedRatioInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewSeedRatio= renderDataTable({
    
    
    
    
    # if (is.null(values_stockCommodities$stocksCommodities)) {
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_seed_ratio$data_seed_ratio$CPCCode) )
    values_seedratioCommodities$seedratioCommodities <- DT
    # }
    
    # if (input$stockinsertcommodities >0 ){
    #   values_stockCommodities$stocksCommodities <- subset(values_stockCommodities$stocksCommodities,  !CPCCode %in% unique(data.table(df_seed_ratio$data_seed_ratio)$CPCCode) )
    # }
    # 
    
    datatable(values_seedratioCommodities$seedratioCommodities ,
              escape=F)
    
    
  })
  
  
  observeEvent(input$SeedRatioInsert, {
    
    removeModal()
    
  })
  
  
  proxy_seed_ratio = dataTableProxy('seed_ratio')
  
  observeEvent(input$seed_ratio_cell_edit, {
    
    
    info = input$seed_ratio_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_seed_ratio$data_seed_ratio[i,(j) := v]
    
    replaceData(proxy_seed_ratio, df_seed_ratio$data_seed_ratio, resetPaging = FALSE,rownames = FALSE)  # important
    
    
    info1 <- input[["seed_ratio_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"seed_ratio"))
    
    Add_table_version("seed_ratio", copy( df_seed_ratio$data_seed_ratio))
    
  })
  
  
  
  
  observeEvent(input$SeedRatioInsert, {
    
    
    s=as.numeric(input$viewSeedRatio_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_seed_ratio$data_seed_ratio)
      
      
      
      df_seed_ratio$data_seed_ratio <- data_current
      
      
    }
    
    else {
      
      
      seedRatiolistTool <- copy(all_cpc)
      seedRatiolistTool <- seedRatiolistTool[order(CPCCode),]
      
      seedRatiolistTool <- subset(seedRatiolistTool,  !CPCCode %in% unique(isolate(df_seed_ratio$data_seed_ratio$CPCCode) ))
      
      yy=seedRatiolistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := "xxxx"]
      
      
      oo <- ff[,Element := "Seed Rate [Kg/ha]"]
      # oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_seed_ratio$data_seed_ratio)
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
      
      df_seed_ratio$data_seed_ratio <- data
      
      Add_table_version("seed_ratio", copy( df_seed_ratio$data_seed_ratio))  
      
    }
    
    
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_seed_ratio, {
    t = copy(df_seed_ratio$data_seed_ratio)
    
    
    if (!is.null(input$seed_ratio_rows_selected)) {
      t <- t[-as.numeric(input$seed_ratio_rows_selected),]
    }
    df_seed_ratio$data_seed_ratio<- t
    
    Add_table_version("seed_ratio", copy( df_seed_ratio$data_seed_ratio))
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadSeedRatio<- downloadHandler(
    
    filename = function() {
      
      "seed_rates.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_seed_ratio <- data.table(df_seed_ratio$data_seed_ratio)
      
      data_download_seed_ratio <- data_download_seed_ratio[!is.na(CPCCode)]
      
      # data_download_stock[,hidden := NULL]
      
      write.xlsx(data_download_seed_ratio ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileSeedRatiodenormalized,{
    
    
    inFile <- input$fileSeedRatiodenormalized
    
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
    
    print(DATA)
    
    END_YEAR=input$endyear
    # END_YEAR <- 2017
    
    # data_denormalized <- data.table(read_excel("normalized.xlsx"))
    
    data_denormalized <- copy(DATA)
    
    data_denormalized <- long_format(data_denormalized)
    
    data_denormalized <- data_denormalized[Year %in% c(input$fromyear : input$endyear)]
    
    data_denormalized[, c("Commodity","Element") := NULL]
    
    data_denormalized[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    
    # write.csv(data.table(df_seed_ratio$data_seed_ratio),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    seed_ratio_Data <- data.table(df_seed_ratio$data_seed_ratio)
    
    
    seed_ratio_Data <- long_format(seed_ratio_Data)
    
    seed_ratio_Data[, c("Commodity","Element") := NULL]
    
    seed_ratio_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=seed_ratio_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    seed_ratio_Data <- seed_ratio_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    seed_ratio_Data <- rbind(seed_ratio_Data,xx)
    
    seed_ratio_Data[, Element := "Seed Rate [Kg/ha]"]
    
    # seed_ratio_Data <- merge(seed_ratio_Data, all_elements, by = "ElementCode", all.x = T)
    
    seed_ratio_Data <- merge(seed_ratio_Data, all_cpc, by= "CPCCode", all.x = T)
    
    seed_ratio_Data <- seed_ratio_Data[!is.na(Element)]
    
    seed_ratio_Data <- subset(seed_ratio_Data, Year %in% 2010:END_YEAR)
    
    seed_ratio_Data <- wide_format(seed_ratio_Data)
    
    seed_ratio_Data = visualize_data(seed_ratio_Data,END_YEAR)
    
    # seed_ratio_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_seed_ratio$data_seed_ratio <- seed_ratio_Data
    
    Add_table_version("seed_ratio", copy( df_seed_ratio$data_seed_ratio))
    
  })
  
  
  
  observeEvent(input$uploadSeedRatioModal, {
    showModal(uploadSeedRatio())
  })
  
  
  
  uploadSeedRatio <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileSeedRatio", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcSeedratio", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)),multiple=F),
                    
                    selectizeInput("elementSeedratio", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)) ,multiple=F),
                    selectizeInput("yearSeedratio", "Year :",
                                   selected = NULL, choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)),multiple=F),
                    
                    selectizeInput("valueSeedratio", "Value :",
                                   selected = NULL, choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)),multiple=F),
                    
                    selectizeInput("flagSeedratio", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)),multiple=F),
                    
                    actionButton("uploadSeedratio","Upload Seed ratio")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    # dataTableOutput("importCountry")
                    div(style = 'overflow-x: scroll', dataTableOutput('SeedratioCountry'))
                  )
                  
                )
    )
    
  }
  
  
  
  output$SeedratioCountry <- renderDataTable({
    
    req(input$fileSeedRatio)
    
    
    
    inFile <- input$fileSeedRatio
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_seed_ratioCountry$data_seed_ratioCountry <- DATA
    
    
    datatable(df_seed_ratioCountry$data_seed_ratioCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcSeedratio", choices = c("", colnames( df_seed_ratioCountry$data_seed_ratioCountry)))
    updateSelectInput(session, "elementSeedratio", choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)))
    updateSelectInput(session, "yearSeedratio", choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)))
    updateSelectInput(session, "valueSeedratio", choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)))
    updateSelectInput(session, "flagSeedratio", choices = c("",colnames( df_seed_ratioCountry$data_seed_ratioCountry)))
    
  })
  
  
  #############################################
  
  
  observeEvent(input$uploadSeedratio, {
    
    removeModal()
    
  })
  
  
  
  observeEvent(input$uploadSeedratio,{
    
    
    data <- data.table(df_seed_ratioCountry$data_seed_ratioCountry)
    
    
    if(input$cpcSeedratio == ""|input$elementSeedratio == ""| input$yearSeedratio == ""| input$valueSeedratio == ""| 
       input$flagSeedratio == "" ){
      
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_seed_ratio$data_seed_ratio <- df_seed_ratio$data_seed_ratio
      
    }else{
      
    
    
    
    
    data <- data[, c(input$cpcSeedratio, input$elementSeedratio, input$yearSeedratio, input$valueSeedratio, input$flagSeedratio), with= F]
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_seed_ratioCountry$data_seed_ratioCountry)
    }else{
    
    
    
    setnames(data,c(input$cpcSeedratio, input$elementSeedratio, input$yearSeedratio, input$valueSeedratio, input$flagSeedratio),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    
    
  
    
    
    data <- subset(data, ElementCode %in% c("xxxx"))
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    seed_ratio_ <- data.table(df_seed_ratio$data_seed_ratio)
    
    seed_ratio_ <- long_format(seed_ratio_)
    
    seed_ratio_[,c("Commodity","Element") := NULL]
    
    seed_ratio_[, ElementCode := as.character(ElementCode)]
    
    xx <- seed_ratio_[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    seed_ratio_ <- seed_ratio_[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    seed_ratio_ <- rbind(seed_ratio_,xx)
    
    seed_ratio_[, Element := "Seed Rate [Kg/ha]"]
    
    # seed_ratio_ <- merge(seed_ratio_, all_elements, by = "ElementCode", all.x = T)
    
    seed_ratio_ <- merge(seed_ratio_, all_cpc, by= "CPCCode", all.x = T)
    
    seed_ratio_ <- seed_ratio_[!is.na(Element)]
    
    seed_ratio_ <- wide_format(seed_ratio_)
    
    # seed_ratio_[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_seed_ratio$data_seed_ratio <- seed_ratio_
    
    Add_table_version("seed_ratio", copy( df_seed_ratio$data_seed_ratio))
    
    }
    }
  })
  
  
  ############################################## 

  
  observeEvent(input$saveSeedratio,{
    
   data_to_save <- df_seed_ratio$data_seed_ratio
    
    write.xlsx(data_to_save,"Data/seedRate.xlsx",row.names = F)
    
  })
  
  
  
  observeEvent(input$seed_imputation_ratio,{
    
    
    data <- imputationSeedRates(input,output,session)
    
    df_seed_ratio$data_seed_ratio <- data
    
    
    if ( !is.null(data) ){
      
      sendSweetAlert(
        session = session,
        title = c("Imputation Error"),
        text = c("No time series data to impute"),
        type = "success"
      )
      
    }
    
    Add_table_version("seed_ratio", copy( df_seed_ratio$data_seed_ratio))  
    
  })
  
  
  
  
  

  output$seed_ratio <-
    renderDataTable({

      # x <- grep("^[[:digit:]]{4}$", names(df_seed_ratio$data_seed_ratio), value = TRUE)
      # col_width <- set_hot_colwidths(df_seed_ratio$data_seed_ratio)
     
      datatable (df_seed_ratio$data_seed_ratio, rownames= FALSE,class = 'cell-border stripe',

                 editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_seed_ratio$data_seed_ratio) 
                                                                                      == input$fromyear)-2)))),
                 extensions = c("FixedColumns","FixedHeader","Buttons"),
                 options = list(

                   # paging = TRUE, searching = TRUE, info = FALSE,
                   # sort = TRUE,
                   scrollX = TRUE,
                   dom= 'Blfrtip', buttons = I('colvis'),
                   scrollY = "500px" ,
                   autoWidth = T,
                   fixedColumns = list(leftColumns = 4),
                   columnDefs = list(list(width = '', targets =  c(6))
                                     # list(visible = FALSE, targets = (ncol(df_seed_ratio$data_seed_ratio)-1))
                                     
                                     )
                 ))%>%
        formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")

   } )
  
#################################################### seed data ###############################################################
  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Seed') {
      
      END_YEAR=input$endyear
      
      seedData=subset(countryData, ElementCode %in% c("5525","5025"))
      
      setDT(seedData)
      
      seedData <- seedData[!duplicated(seedData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
      
      seedData[, c("CountryM49","Country"):=NULL]
      
      seedData <- subset(seedData, Year %in% c(2010 : END_YEAR) )
      
      # seedData <- seedData[!is.na(Value)]
      
      
      seedData=wide_format(seedData)
   
      flagcols <- grep("^Flag", names(seedData), value = TRUE)
      yearcols <- grep("^[[:digit:]]{4}$", names(seedData), value = TRUE)
      
      minyear <- min(as.numeric(yearcols))
      maxyear <- max(as.numeric(yearcols))
      
      
      
      
      if(END_YEAR > maxyear +1){
        END_YEAR=as.numeric(END_YEAR)
        yearsToFill = (maxyear + 1):END_YEAR
        
        df_seed$data_seed <- NULL
        if(length(yearsToFill) > 0){
          # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
          
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile Seed data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
          
          
          
        }
        
      } else {
        
        seedData = visualize_data(seedData,END_YEAR, session)
        
        seedData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
        
        df_seed$data_seed <- seedData
        
        Add_table_version("seed_values", copy(df_seed$data_seed)) 
      }
      
      

      
    # }
  })
  
  
  
  observeEvent(input$add_Seed, {
    showModal(viewSeedTriplets())
  })
  
  
  viewSeedTriplets <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewSeed")
      ,
      
      footer = tagList(
        
        actionButton("seedInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewSeed= renderDataTable({
    
    
    
    
    
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_seed$data_seed$CPCCode) )
    values_seedCommodities$seedCommodities <- DT
    # }
    
  
    
    datatable(values_seedCommodities$seedCommodities,
              escape=F)
    
    
  })
  
  
  observeEvent(input$seedInsert, {
    
    removeModal()
    
  })
  
  
  proxy_seed = dataTableProxy('seed_values')
  
  observeEvent(input$seed_values_cell_edit, {
    
    
    info = input$seed_values_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_seed$data_seed[i,(j) := v]
    
    replaceData(proxy_seed, df_seed$data_seed, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["seed_values_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"seed_values"))
    
    Add_table_version("seed_values", copy(df_seed$data_seed)) 
  })
  
  
  
  
  observeEvent(input$seedInsert, {
    
    
    s=as.numeric(input$viewSeed_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_seed$data_seed)
      
      
      
      df_seed$data_seed <- data_current
      
      
    }
    
    else {
      
      
      seedlistTool <- copy(all_cpc)
      seedlistTool <- seedlistTool[order(CPCCode),]
      
      seedlistTool <- subset(seedlistTool,  !CPCCode %in% unique(isolate(df_seed$data_seed$CPCCode) ))
      
      yy=seedlistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := c("5525")]
      
      ########## add also area sown ########################
      
      aa <- copy(ff)
      
      aa[, ElementCode := c("5025")]
      
      ff <- rbind(ff,aa)
      
      #####################################################
      
      
      oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_seed$data_seed)
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
      
      df_seed$data_seed <- data
      
      Add_table_version("seed_values", copy(df_seed$data_seed)) 
      
    }
    
    
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_seed, {
    t = copy(df_seed$data_seed)
    final_data <- copy(df_seed$data_seed)
    
    if (!is.null(input$seed_values_rows_selected)) {
      t <- t[as.numeric(input$seed_values_rows_selected),]
      cpc_code_to_remove <- unique(t$CPCCode)
      ele_code_to_remove <- unique(t$ElementCode)
      
    }
  
    
    df_seed$data_seed<- final_data[!CPCCode %in% cpc_code_to_remove]
    
    #remove the cpc with the element from the database
    
    database <- copy(countryData)
    
    database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% ele_code_to_remove)]
    
    # save(database, file = "Data/countrySUA.RData")
    
    countryData <<- database
    
    Add_table_version("seed_values", copy(df_seed$data_seed)) 
    
    
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadSeed<- downloadHandler(
    
    filename = function() {
      
      "seed_data.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_seed <- data.table(df_seed$data_seed)
      
      data_download_seed <- data_download_seed[!is.na(CPCCode)]
      
      data_download_seed[,hidden := NULL]
      
      write.xlsx(data_download_seed ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileSeeddenormalized,{
    
    
    inFile <- input$fileSeeddenormalized
    
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
    
    print(DATA)
    
    END_YEAR=input$endyear
    # END_YEAR <- 2017
    
    # data_denormalized <- data.table(read_excel("normalized.xlsx"))
    
    data_denormalized <- copy(DATA)
    
    data_denormalized <- long_format(data_denormalized)
    
    data_denormalized <- data_denormalized[Year %in% c(input$fromyear : input$endyear)]
    
    data_denormalized[, c("Commodity","Element") := NULL]
    
    data_denormalized[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    
    # write.csv(data.table(df_seed$data_seed),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    seed_Data <- data.table(df_seed$data_seed)
    
    
    seed_Data <- long_format(seed_Data)
    
    seed_Data[, c("Commodity","Element") := NULL]
    
    seed_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=seed_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    seed_Data <- seed_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    seed_Data <- rbind(seed_Data,xx)
    
    seed_Data <- merge(seed_Data, all_elements, by = "ElementCode", all.x = T)
    
    seed_Data <- merge(seed_Data, all_cpc, by= "CPCCode", all.x = T)
    
    seed_Data <- seed_Data[!is.na(Element)]
    
    seed_Data <- subset(seed_Data, Year %in% 2010:END_YEAR)
    
    seed_Data <- wide_format(seed_Data)
    
    seed_Data = visualize_data(seed_Data,END_YEAR)
    
    seed_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_seed$data_seed <- seed_Data
    
    Add_table_version("seed_values", copy(df_seed$data_seed)) 
    
  })
  
  
  
  observeEvent(input$uploadSeedModal, {
    showModal(uploadSeed())
  })
  
  
  
  uploadSeed <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileSeed", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcSeed", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_seedCountry$data_seedCountry)),multiple=F),
                    
                    selectizeInput("elementSeed", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_seedCountry$data_seedCountry)) ,multiple=F),
                    selectizeInput("yearSeed", "Year :",
                                   selected = NULL, choices = c("",colnames( df_seedCountry$data_seedCountry)),multiple=F),
                    
                    selectizeInput("valueSeed", "Value :",
                                   selected = NULL, choices = c("",colnames( df_seedCountry$data_seedCountry)),multiple=F),
                    
                    selectizeInput("flagSeed", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_seedCountry$data_seedCountry)),multiple=F),
                    
                    actionButton("uploadSeed","Upload Seed data")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                  
                    div(style = 'overflow-x: scroll', dataTableOutput('seedCountry'))
                  )
                  
                )
    )
    
  }
  
  
  observeEvent(input$uploadSeed, {
    
    removeModal()
    
  })
  
  
  
  
  output$seedCountry <- renderDataTable({
    
    req(input$fileSeed)
    
    
    
    inFile <- input$fileSeed
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_seedCountry$data_seedCountry <- DATA
    
    
    datatable(df_seedCountry$data_seedCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcSeed", choices = c("", colnames( df_seedCountry$data_seedCountry)))
    updateSelectInput(session, "elementSeed", choices = c("",colnames( df_seedCountry$data_seedCountry)))
    updateSelectInput(session, "yearSeed", choices = c("",colnames( df_seedCountry$data_seedCountry)))
    updateSelectInput(session, "valueSeed", choices = c("",colnames( df_seedCountry$data_seedCountry)))
    updateSelectInput(session, "flagSeed", choices = c("",colnames( df_seedCountry$data_seedCountry)))
    
  })
  
  
  #############################################
  
  
  observeEvent(input$uploadSeed,{
    
    
    data <- data.table(df_seedCountry$data_seedCountry)
    
    if(input$cpcSeed == ""|input$elementSeed == ""| input$yearSeed == ""| input$valueSeed == ""| input$flagSeed == "" ){
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_seed$data_seed <- df_seed$data_seed
      
    }else{
      
    
  
    data <- data[, c(input$cpcSeed, input$elementSeed, input$yearSeed, input$valueSeed, input$flagSeed), with= F]
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_seedCountry$data_seedCountry)
    }else{
    
    
    setnames(data,c(input$cpcSeed, input$elementSeed, input$yearSeed, input$valueSeed, input$flagSeed),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    
    data <- subset(data, ElementCode %in% c("5525"))
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    seed_values <- data.table(df_seed$data_seed)
    
    seed_values <- long_format(seed_values)
    
    seed_values[,c("Commodity","Element") := NULL]
    
    seed_values[, ElementCode := as.character(ElementCode)]
    
    xx <- seed_values[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    seed_values <- seed_values[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    seed_values <- rbind(seed_values,xx)
    
    seed_values <- merge(seed_values, all_elements, by = "ElementCode", all.x = T)
    
    seed_values <- merge(seed_values, all_cpc, by= "CPCCode", all.x = T)
    
    seed_values <- seed_values[!is.na(Element)]
    
    seed_values <- wide_format(seed_values)
    
    seed_values[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_seed$data_seed <- seed_values
    
    Add_table_version("seed_values", copy(df_seed$data_seed)) 
    
    }
    }  
  })
  
  
  ############################################## 
  
  
  
  
  
  
  #save stock
  
  observeEvent(input$saveSeed,{
    
    data_to_save <- df_seed$data_seed
    
    data_to_save <- subset(data_to_save, ElementCode %in% c("5525","5025"))
    
    # data_to_save[,hidden := NULL]
    
    save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
    
    new_saved_data <- return_data_base(data_to_save)
    
    df_sua_unbalanced$data_sua_unbalanced <- new_saved_data
    
    
    
    
    
  })
  
  
  
  observeEvent(input$seed_imputation,{
    
    
    data <- imputeSeed(input,output,session)
    
    
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
      
      df_seed$data_seed <- data
      
      Add_table_version("seed_values", copy(df_seed$data_seed))
      
    }
    
    
    
   
    
    
  })
  
  
  
  
  
  output$seed_values <- 
    renderDataTable(
      
      if (!is.null(df_seed$data_seed)){
        datatable (df_seed$data_seed, rownames= FALSE,class = 'cell-border stripe', 
                   
            editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_seed$data_seed) == input$fromyear)-2)))),
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
                       list(visible = FALSE, targets = (ncol(df_seed$data_seed)-1))
                     )
                   ))  %>%
          
          formatStyle(0:ncol(df_seed$data_seed), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
      
    )
  
  
}