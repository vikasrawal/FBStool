

industry <- function(input,output,session){
  
  
  observeEvent(input$undoIndustry, {
    
    # get last version
    new_version <- Pop_table_version("industry_values")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_industry$data_industry <- new_version
    
  })
  
  
  
  

#################################################### Industry data ###############################################################
  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Industry') {
      
      END_YEAR=input$endyear
      
      industryData=subset(countryData, ElementCode %in% c("5165"))
      
      setDT(industryData)
      
      industryData <- industryData[!duplicated(industryData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
      
      industryData[, c("CountryM49","Country"):=NULL]
      
      industryData <- subset(industryData, Year %in% c(2010 : END_YEAR) )
      
      # industryData <- industryData[!is.na(Value)]
      
      
      industryData=wide_format(industryData)
   
      flagcols <- grep("^Flag", names(industryData), value = TRUE)
      yearcols <- grep("^[[:digit:]]{4}$", names(industryData), value = TRUE)
      
      minyear <- min(as.numeric(yearcols))
      maxyear <- max(as.numeric(yearcols))
      
      
      
      
      if(END_YEAR > maxyear +1){
        END_YEAR=as.numeric(END_YEAR)
        yearsToFill = (maxyear + 1):END_YEAR
        
        df_industry$data_industry <- NULL
        if(length(yearsToFill) > 0){
          # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
          
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile Industrial data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
          
          
          
        }
        
      } else {
        
        industryData = visualize_data(industryData,END_YEAR, session)
        
        # industryData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
        
        df_industry$data_industry <- industryData
        
        Add_table_version("industry_values", copy( df_industry$data_industry)) 
      }
      
      

      
    # }
  })
  
  
  
  observeEvent(input$add_Industry, {
    showModal(viewIndustryTriplets())
  })
  
  
  viewIndustryTriplets <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewIndustry")
      ,
      
      footer = tagList(
        
        actionButton("industryInsert", "Insert")
      )
    )
    
  }
  
  
  
  output$viewIndustry= renderDataTable({
    
    
    
    
    
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    
    
    DT <- subset(DT,  !CPCCode %in% unique(df_industry$data_industry$CPCCode) )
    values_industryCommodities$industryCommodities <- DT
    # }
    
  
    
    datatable(values_industryCommodities$industryCommodities,
              escape=F)
    
    
  })
  
  
  observeEvent(input$industryInsert, {
    
    removeModal()
    
  })
  
  
  proxy_industry = dataTableProxy('industry_values')
  
  observeEvent(input$industry_values_cell_edit, {
    
    
    info = input$industry_values_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_industry$data_industry[i,(j) := v]
    
    replaceData(proxy_industry, df_industry$data_industry, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["industry_values_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"industry_values"))
    
    Add_table_version("industry_values", copy( df_industry$data_industry)) 
    
  })
  
  
  
  
  observeEvent(input$industryInsert, {
    
    
    s=as.numeric(input$viewIndustry_rows_selected)
    
    if (length(s) == 0){
      
      data_current <- data.table(df_industry$data_industry)
      
      
      
      df_industry$data_industry <- data_current
      
      
    }
    
    else {
      
      
      industrylistTool <- copy(all_cpc)
      industrylistTool <- industrylistTool[order(CPCCode),]
      
      industrylistTool <- subset(industrylistTool,  !CPCCode %in% unique(isolate(df_industry$data_industry$CPCCode) ))
      
      yy=industrylistTool[s,]
      
      # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
      ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
      ff[, ElementCode := c("5165")]
      
      
      
      oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
      setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
      oo=oo[order(CPCCode)]
      
      data=isolate(df_industry$data_industry)
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
      
      df_industry$data_industry <- data
      
      Add_table_version("industry_values", copy( df_industry$data_industry)) 
      
    }
    
    
    
    
  })
  
  
  #delete rows in crop table
  
  observeEvent(input$delete_btn_industry, {
    t = copy(df_industry$data_industry)
    final_data <- copy(df_industry$data_industry)
    
    
    if (!is.null(input$industry_values_rows_selected)) {
      t <- t[as.numeric(input$industry_values_rows_selected),]
      
      cpc_code_to_remove <- unique(t$CPCCode)
      ele_code_to_remove <- unique(t$ElementCode)
    }

    df_industry$data_industry<- final_data[!CPCCode %in% cpc_code_to_remove]
    
    #remove the cpc with the element from the database
    
    database <- copy(countryData)
    
    database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% ele_code_to_remove)]
    
    # save(database, file = "Data/countrySUA.RData")
    
    countryData <<- database
    
    Add_table_version("industry_values", copy( df_industry$data_industry)) 
  })
  
  
  #download excle file (#downloadCrop)
  
  output$downloadIndustry<- downloadHandler(
    
    filename = function() {
      
      "non_food_industrial_data.xlsx"
    },
    
    
    content = function(file) {
      
      data_download_industry <- data.table(df_industry$data_industry)
      
      data_download_industry <- data_download_industry[!is.na(CPCCode)]
      
      # data_download_stock[,hidden := NULL]
      
      write.xlsx(data_download_industry ,file,row.names = FALSE)
    }
    
  )
  
  
  # #upload crop denormalized data (#fileCrop)
  
  observeEvent(input$fileIndustrydenormalized,{
    
    
    inFile <- input$fileIndustrydenormalized
    
    
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
    
    
    
    
    # write.csv(data.table(df_industry$data_industry),"crop_Data.csv",row.names = F)
    
    # crop_Data <- fread("crop_Data.csv")
    
    industry_Data <- data.table(df_industry$data_industry)
    
    
    industry_Data <- long_format(industry_Data)
    
    industry_Data[, c("Commodity","Element") := NULL]
    
    industry_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    
    
    xx=industry_Data[!is.na(Value)][
      data_denormalized,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    industry_Data <- industry_Data[
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    industry_Data <- rbind(industry_Data,xx)
    
    industry_Data <- merge(industry_Data, all_elements, by = "ElementCode", all.x = T)
    
    industry_Data <- merge(industry_Data, all_cpc, by= "CPCCode", all.x = T)
    
    industry_Data <- industry_Data[!is.na(Element)]
    
    industry_Data <- subset(industry_Data, Year %in% 2010:END_YEAR)
    
    industry_Data <- wide_format(industry_Data)
    
    industry_Data = visualize_data(industry_Data,END_YEAR)
    
    # industry_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_industry$data_industry <- industry_Data
    
    Add_table_version("industry_values", copy( df_industry$data_industry)) 
    
  })
  
  
  
  observeEvent(input$uploadIndustryModal, {
    showModal(uploadIndustry())
  })
  
  observeEvent(input$uploadIndustry, {
    
    removeModal()
    
  })
  
  
  uploadIndustry <- function(failed = FALSE) {
    
    
    modalDialog(size = "l",
                
                
                titlePanel("Upload File"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("fileIndustry", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                    # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                    
                    selectizeInput("cpcIndustry", "CPC Code",
                                   selected = NULL, choices = c("",colnames( df_industryCountry$data_industryCountry)),multiple=F),
                    
                    selectizeInput("elementIndustry", "Element Code",
                                   selected = NULL, choices = c("",colnames( df_industryCountry$data_industryCountry)) ,multiple=F),
                    selectizeInput("yearIndustry", "Year :",
                                   selected = NULL, choices = c("",colnames( df_industryCountry$data_industryCountry)),multiple=F),
                    
                    selectizeInput("valueIndustry", "Value :",
                                   selected = NULL, choices = c("",colnames( df_industryCountry$data_industryCountry)),multiple=F),
                    
                    selectizeInput("flagIndustry", "Flag :",
                                   selected = NULL, choices = c("",colnames( df_industryCountry$data_industryCountry)),multiple=F),
                    
                    actionButton("uploadIndustry","Upload Industry data")
                    
                  ),
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                  
                    div(style = 'overflow-x: scroll', dataTableOutput('industryCountry'))
                  )
                  
                )
    )
    
  }
  
  
  
  output$industryCountry <- renderDataTable({
    
    req(input$fileIndustry)
    
    
    
    inFile <- input$fileIndustry
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
    df_industryCountry$data_industryCountry <- DATA
    
    
    datatable(df_industryCountry$data_industryCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
  })
  
  
  observe({
    
    updateSelectInput(session, "cpcIndustry", choices = c("", colnames( df_industryCountry$data_industryCountry)))
    updateSelectInput(session, "elementIndustry", choices = c("",colnames( df_industryCountry$data_industryCountry)))
    updateSelectInput(session, "yearIndustry", choices = c("",colnames( df_industryCountry$data_industryCountry)))
    updateSelectInput(session, "valueIndustry", choices = c("",colnames( df_industryCountry$data_industryCountry)))
    updateSelectInput(session, "flagIndustry", choices = c("",colnames( df_industryCountry$data_industryCountry)))
    
  })
  
  
  #############################################
  
  
  observeEvent(input$uploadIndustry,{
    
    
    data <- data.table(df_industryCountry$data_industryCountry)
    
    if(input$cpcIndustry == ""|input$elementIndustry == ""| input$yearIndustry == ""| input$valueIndustry == ""| input$flagIndustry == "" ){
      sendSweetAlert(
        session = session,
        title = "Warning !!",
        text = "Invalid data",
        type = "warning"
      )
      
      df_industry$data_industry <- df_industry$data_industry
      
    }else{
      
    
    
    data <- data[, c(input$cpcIndustry, input$elementIndustry, input$yearIndustry, input$valueIndustry, input$flagIndustry), with= F]
    
    if (length(names(data)[duplicated(names(data))])>0){
      
      
      sendSweetAlert(
        session = session,
        title = "WARNING !!",
        text = "Please select the colums correctly",
        type = "warning"
      )
      
      data <- data.table(df_industryCountry$data_industryCountry)
    }
    else{
    
    setnames(data,c(input$cpcIndustry, input$elementIndustry, input$yearIndustry, input$valueIndustry, input$flagIndustry),
             c("CPCCode","ElementCode","Year","Value","Flag"))
    
    data <- subset(data, ElementCode %in% c("5165"))
    
    data[, Year := as.character(Year)]
    data[, CPCCode := as.character(CPCCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data <- data[Year %in% c(input$fromyear : input$endyear)]
    
    industry_values <- data.table(df_industry$data_industry)
    
    industry_values <- long_format(industry_values)
    
    industry_values[,c("Commodity","Element") := NULL]
    
    industry_values[, ElementCode := as.character(ElementCode)]
    
    xx <- industry_values[!is.na(Value)][
      data,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    
    xx[, c("Value","Flag"):= NULL]
    
    setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
    
    
    industry_values <- industry_values[!is.na(Value)][
      !xx,
      on = c("CPCCode", "ElementCode", "Year")
      
      ]
    
    industry_values <- rbind(industry_values,xx)
    
    industry_values <- merge(industry_values, all_elements, by = "ElementCode", all.x = T)
    
    industry_values <- merge(industry_values, all_cpc, by= "CPCCode", all.x = T)
    
    industry_values <- industry_values[!is.na(Element)]
    
    industry_values <- wide_format(industry_values)
    
    # industry_values[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_industry$data_industry <- industry_values
    
    Add_table_version("industry_values", copy( df_industry$data_industry)) 
    
    }
    }
  })
  
  
  ############################################## 
  
  
  
  
  
  
  #save Industry
  
  observeEvent(input$saveIndustry,{
    
    data_to_save <- df_industry$data_industry
    
    data_to_save <- subset(data_to_save, ElementCode %in% c("5165"))
    
    save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
    
    new_saved_data <- return_data_base(data_to_save)
    
    df_sua_unbalanced$data_sua_unbalanced <- new_saved_data
    
    
    
    
    
  })
  
  
  
  observeEvent(input$industry_imputation,{
    
    
   
    
    
  })
  
  
  
  
  
  output$industry_values <- 
    renderDataTable(
      
      if (!is.null(df_industry$data_industry)){
        datatable (df_industry$data_industry, rownames= FALSE,class = 'cell-border stripe', 
                   
            editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_industry$data_industry) == input$fromyear)-2)))),
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
                       # list(visible = FALSE, targets = (ncol(df_industry$data_industry)-1))
                     )
                   ))  %>%
          
          # formatStyle(0:ncol(df_industry$data_industry), valueColumns = "hidden",
          #             `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
      
    )
  
  
}