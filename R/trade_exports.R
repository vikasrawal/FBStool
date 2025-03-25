

trade_exports <- function(input,output,session){

  #### undo Exports ####
  
  observeEvent(input$undoExports, {
    
    # get last version
    new_version <- Pop_table_version("Exports")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_exports$data_exports <- new_version
    
  })


observeEvent(input$startContinue,{
  
  
  END_YEAR=input$endyear
  
  exportData=subset(countryData, ElementCode %in% c("5910") )
  
  setDT(exportData)
  
  exportData <- exportData[!duplicated(exportData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
  
  exportData[, c("CountryM49","Country"):=NULL]
  
  exportData <- subset(exportData, Year %in% c(2010 : END_YEAR) )
  
  exportData <- exportData[!is.na(Value)]
  
  exportData=wide_format(exportData)
  
  
  flagcols <- grep("^Flag", names(exportData), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(exportData), value = TRUE)
  
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  if(END_YEAR > maxyear +1){
    END_YEAR=as.numeric(END_YEAR)
    yearsToFill = (maxyear + 1):END_YEAR
    
    df_exports$data_exports <- NULL
    if(length(yearsToFill) > 0){
      # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
      
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste("Please compile Exports data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
        type = "error"
      )
      
      
      
    }

  } else {
    
    exportData = visualize_data(exportData,END_YEAR, session)
    
    exportData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_exports$data_exports <- exportData
    
    Add_table_version("Exports", copy(df_exports$data_exports)) 
  }
  
})



observeEvent(input$add_Exports, {
  showModal(viewExportsTriplets())
})


viewExportsTriplets <- function(failed = FALSE) {
  
  
  modalDialog(
    
    easyClose = TRUE, size= "l",
    dataTableOutput("viewExports")
    ,
    
    footer = tagList(
      
      actionButton("exportsInsert", "Insert")
    )
  )
  
}



output$viewExports= renderDataTable({
  
  
 
  commodity=copy(all_cpc)
  commodity <- commodity[order(CPCCode),]
  DT=commodity
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
  
  
  DT <- subset(DT,  !CPCCode %in% unique(df_exports$data_exports$CPCCode) )
  values_exportsCommodities$exportsCommodities <- DT
  # }
  
  # if (input$stockinsertcommodities >0 ){
  #   values_stockCommodities$stocksCommodities <- subset(values_stockCommodities$stocksCommodities,  !CPCCode %in% unique(data.table(df_stock$data_stock)$CPCCode) )
  # }
  # 
  
  datatable(values_exportsCommodities$exportsCommodities,
            escape=F)
  
  
  
})


observeEvent(input$exportsInsert, {
  
  removeModal()
  
})


proxy_exports = dataTableProxy('Exports')

observeEvent(input$Exports_cell_edit, {
  
  
  info = input$Exports_cell_edit
  
  print(info)
  i = info$row
  j = (info$col + 1)
  v = info$value
  df_exports$data_exports[i,(j) := v]
  
  replaceData(proxy_exports, df_exports$data_exports, resetPaging = FALSE,rownames = FALSE)  # important
  
  info1 <- input[["Exports_cell_edit"]]
  i <- info1[["row"]]
  j <- info1[["col"]]
  runjs(colorizeCell(i, j+1,"Exports"))
  
  Add_table_version("Exports", copy(df_exports$data_exports)) 
  
  
})




observeEvent(input$exportsInsert, {
  
  
  s=as.numeric(input$viewExports_rows_selected)
  
  if (length(s) == 0){
    
    data_current <- data.table(df_exports$data_exports)
    
    
    
    df_exports$data_exports <- data_current
    
    
  }
  
  else {
    
    
    exportslistTool <- copy(all_cpc)
    exportslistTool <- exportslistTool[order(CPCCode),]
    
    exportslistTool <- subset(exportslistTool,  !CPCCode %in% unique(isolate(df_exports$data_exports$CPCCode) ))
    
    yy=exportslistTool[s,]
    
    # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
    ff[, ElementCode := c("5910")]
    
    
    
    oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
    setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
    oo=oo[order(CPCCode)]
    
    data=isolate(df_exports$data_exports)
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
    
    df_exports$data_exports <- data
    
    Add_table_version("Exports", copy(df_exports$data_exports)) 
    
  }
  
  
  
  
})





#delete rows 

observeEvent(input$delete_btn_exports, {
  t = copy(df_exports$data_exports)
  final_data <- copy(df_exports$data_exports)
  
  if (!is.null(input$Exports_rows_selected)) {
    t <- t[as.numeric(input$Exports_rows_selected),]
    
    cpc_code_to_remove <- unique(t$CPCCode)
    ele_code_to_remove <- unique(t$ElementCode)
  }
  
  
  df_exports$data_exports <- final_data[!CodeCPC %in% cpc_code_to_remove]
  database <- copy(countryData)
  
  database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% c("5910"))]
  
  countryData <<- database
  
  Add_table_version("Exports", copy(df_exports$data_exports))
})


#download excle file (#downloadCrop)

output$downloadExports <- downloadHandler(
  
  filename = function() {
    
    "export_data.xlsx"
  },
  
  
  content = function(file) {
    
    data_download_exports <- data.table(df_exports$data_exports)
    
    data_download_exports <- data_download_exports[!is.na(CPCCode)]
    
    data_download_exports[,hidden := NULL]
    
    write.xlsx(data_download_exports ,file,row.names = FALSE)
  }
  
)


# #upload crop denormalized data (#fileCrop)

observeEvent(input$fileExportsdenormalized,{


  inFile <- input$fileExportsdenormalized
 
  
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
  
  
  
  
  # write.csv(data.table(df_exports$data_exports),"exports_Data.csv",row.names = F)
  
  # exports_Data <- fread("exports_Data.csv")
  
  exports_Data <- data.table(df_exports$data_exports)
  
  
  exports_Data <- long_format(exports_Data)
  
  exports_Data[, c("Commodity","Element") := NULL]
  
  exports_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
  
  
  
  xx=exports_Data[!is.na(Value)][
    data_denormalized,
    on = c("CPCCode", "ElementCode", "Year")
    
    ]
  
  
  xx[, c("Value","Flag"):= NULL]
  
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  
  exports_Data <- exports_Data[
    !xx,
    on = c("CPCCode", "ElementCode", "Year")
    
    ]
  
  exports_Data <- rbind(exports_Data,xx)
  
  exports_Data <- merge(exports_Data, all_elements, by = "ElementCode", all.x = T)
  
  exports_Data <- merge(exports_Data, all_cpc, by= "CPCCode", all.x = T)
  
  exports_Data <- exports_Data[!is.na(Element)]
  
  exports_Data <- subset(exports_Data, Year %in% 2010:END_YEAR)
  
  exports_Data <- wide_format(exports_Data)
  
  exports_Data = visualize_data(exports_Data,END_YEAR)

  exports_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]

  df_exports$data_exports <- exports_Data
  
  Add_table_version("Exports", copy(df_exports$data_exports))

})



observeEvent(input$uploadExportsModal, {
  showModal(uploadExports())
})
# 


uploadExports <- function(failed = FALSE) {

  
  modalDialog(size = "l",
              
              
              titlePanel("Upload File"),
              
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Select a file ----
                  fileInput("fileExports", "Choose Country Export Excel File (.xlsx)",
                            multiple = TRUE,
                            accept = NULL),
                  # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                  
                  selectizeInput("exportHS", "HS Code (Please make sure that HS Code should contain at least 6 digits) :",
                                 selected = NULL, choices = c("", colnames(df_exportsCountry$data_exportsCountry)),multiple=F),
                  
                  selectizeInput("exportCommodity", "Commodity :",
                                 selected = NULL, choices = c("", colnames(df_importsCountry$data_importsCountry)),multiple=F),
                  
                  selectizeInput("exportQuantity", "Quantity (Please make sure that the quantity is in tons) :",
                                 selected = NULL, choices = c("", colnames(df_exportsCountry$data_exportsCountry)) ,multiple=F),
                  selectizeInput("exportYear", "Year :",
                                 selected = NULL, choices = c("", colnames(df_exportsCountry$data_exportsCountry)),multiple=F),
                  actionButton("exportMap","Map Export Data with CPCCode")
                  
                ),
                
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Data file ----
                  # dataTableOutput("importCountry")
                  div(style = 'overflow-x: scroll', dataTableOutput('exportCountry'))
                )
                
              )
  )
  
  
  
  
}

 
 
# output$cropCountry <- renderDataTable({
#   
#   req(input$fileCrop)
#   
#   
#   
#   inFile <- input$fileCrop
#   
#   file.rename(inFile$datapath,
#               paste(inFile$datapath, ".xlsx", sep=""))
#   DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
#   
#   df_cropCountry$data_cropCountry <- DATA
#   
#   
#   datatable(df_cropCountry$data_cropCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
#   
#   
# })

observe({
  
  updateSelectInput(session, "exportHS", choices = c("", colnames(df_exportsCountry$data_exportsCountry)))
  updateSelectInput(session, "exportCommodity", choices = c("", colnames(df_exportsCountry$data_exportsCountry)))
  updateSelectInput(session, "exportQuantity", choices = c("",colnames(df_exportsCountry$data_exportsCountry)))
  updateSelectInput(session, "exportYear", choices = c("",colnames(df_exportsCountry$data_exportsCountry)))
})




observeEvent(input$exportMap, {
  
  removeModal()
  
})




output$exportCountry <- renderDataTable({
  
  req(input$fileExports)
  
  
  
  inFile <- input$fileExports
  
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  
  df_exportsCountry$data_exportsCountry <- DATA
  
  
  datatable(df_exportsCountry$data_exportsCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  
})




observeEvent(input$exportMap,{
  
  data <- df_exports$data_exports
  
  
  if (input$exportYear== ""| input$exportHS == ""| input$exportQuantity == ""){
    
    
    sendSweetAlert(
      session = session,
      title = "Warning !!",
      text = "Invalid data",
      type = "warning"
    )
    
    df_exports$data_exports <- data
    
  }else if (!is.null(input$fileExports)){
  
    sendSweetAlert(
      session = session,
      title = "Warning !!",
      text = paste("You are deleting existing export trade data of years", input$fromyear, "to", input$endyear),
      type = "warning"
    )
  
  data <-tradeExportMapping_old(input,output,session)
  
  data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  
  df_exports$data_exports <- data
  
  Add_table_version("Exports", copy(df_exports$data_exports))
  }
  
  
  
  ###newly added for not mapped data
  
  viewnotmappedExports <- function(failed = FALSE) {
    
    
    modalDialog(
      
      title = "The following HS codes were not mapped for Exports Trade Data",
      easyClose = TRUE, size= "l",
      dataTableOutput("notmapped_exports"),
      
      
      footer = tagList(
        
        downloadButton("notmappedexports_download", "Download Excel File"),
        modalButton("Dismiss")
      )
    )
    
  }
  
  
  showModal(viewnotmappedExports())
  
  
  output$notmapped_exports= renderDataTable({
    
    
    
    datatable(df_exports_not_mapped$data_exports_not_mapped,
              escape=F)
    
    
  })
  
  
  
  
})

output$notmappedexports_download <- downloadHandler(
  
  filename = function() {
    
    "export_data_not_mapped.xlsx"
  },
  
  
  content = function(file) {
    
    
    
    write.xlsx(df_exports_not_mapped$data_exports_not_mapped,file,row.names = FALSE)
  }
  
)


# 
# observeEvent(input$uploadCrop,{
#   
#   
#   data <- data.table(df_cropCountry$data_cropCountry)
#   
#   
#   
#   # write.csv(data,"test.csv",row.names = FALSE)
#   
#   # data <- fread("test.csv")
#   
#   # data <- data[, c("CPCCode","ElementCode","Value","Flag","Year")]
#   
#   data <- data[, c(input$cpcCrop, input$elementCrop, input$yearCrop, input$valueCrop, input$flagCrop), with= F]
#   
#   setnames(data,c(input$cpcCrop, input$elementCrop, input$yearCrop, input$valueCrop, input$flagCrop),
#            c("CPCCode","ElementCode","Year","Value","Flag"))
#   
#   
#   
#   data <- subset(data, ElementCode %in% c("5312","5510"))
#   
#   data[, Year := as.character(Year)]
#   data[, CPCCode := as.character(CPCCode)]
#   data[, ElementCode := as.character(ElementCode)]
#   
#   # data[, CPCCode := paste0("0",CPCCode)]
#   
#   # write.csv(df_exports$data_exports, "cropTest.csv", row.names = FALSE)
#   
#   # crop <- fread("cropTest.csv")
#   
#   crop <- data.table(df_exports$data_exports)
#   
#   crop <- long_format(crop)
#   
#   crop[,c("Commodity","Element") := NULL]
#   
#   crop[, ElementCode := as.character(ElementCode)]
#   
#   xx <- crop[!is.na(Value)][
#     data,
#     on = c("CPCCode", "ElementCode", "Year")
#     
#     ]
#   
#   
#   xx[, c("Value","Flag"):= NULL]
#   
#   setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
#   
#   
#   crop <- crop[!is.na(Value)][
#     !xx,
#     on = c("CPCCode", "ElementCode", "Year")
#     
#     ]
#   
#   crop <- rbind(crop,xx)
#   
#   crop <- merge(crop, all_elements, by = "ElementCode", all.x = T)
#   
#   crop <- merge(crop, all_cpc, by= "CPCCode", all.x = T)
#   
#   crop <- crop[!is.na(Element)]
#   
#   crop <- wide_format(crop)
#   
#   crop[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
#   
#   df_exports$data_exports <- crop
#   
#   
# })
# 

############################################## 


#save crop

observeEvent(input$saveExports,{
  
  data_to_save <- copy(df_exports$data_exports)
  
  data_to_save[,hidden := NULL]
  
  save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
  
  new_saved_data_exports <- return_data_base(data_to_save)

  df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_exports
  
  

  
  
  
})



output$Exports <- 
  
 
  renderDataTable(
    if(!is.null(df_exports$data_exports)){
    
    datatable (df_exports$data_exports, rownames= FALSE,class = 'cell-border stripe', 
               
               editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_exports$data_exports) == input$fromyear)-2)))), 
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
                                   list(visible = FALSE, targets = (ncol(df_exports$data_exports)-1))
                 ) 
               ))  %>%
      
      formatStyle(0:ncol(df_exports$data_exports), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
   
    }
  )
    
}