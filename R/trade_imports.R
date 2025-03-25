

trade_imports <- function(input,output,session){

  #### undo imports ####
  
  observeEvent(input$undoImports, {
    
    # get last version
    new_version <- Pop_table_version("imports")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_imports$data_imports <- new_version
    
  })


observeEvent(input$startContinue,{
  
  
  END_YEAR=input$endyear
  
  # cropData=subset(countryData, CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode])
  #                 & ElementCode %in% c("5510","5312","5421","5327","5423") )
  
  
  importData=subset(countryData, ElementCode %in% c("5610") )
  
  setDT(importData)
  
  importData <- importData[!duplicated(importData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
  
  importData[, c("CountryM49","Country"):=NULL]
  
  importData <- subset(importData, Year %in% c(2010 : END_YEAR) )
  
  importData <- importData[!is.na(Value)]
  
  importData=wide_format(importData)
  
  
  flagcols <- grep("^Flag", names(importData), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(importData), value = TRUE)
  
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  if(END_YEAR > maxyear +1){
    END_YEAR=as.numeric(END_YEAR)
    yearsToFill = (maxyear + 1):END_YEAR
    
    df_imports$data_imports <- NULL
    if(length(yearsToFill) > 0){
      # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
      
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste("Please compile Imports data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
        type = "error"
      )
      
      
      
    }

  } else {
    
    importData = visualize_data(importData,END_YEAR, session)
    
    importData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_imports$data_imports <- importData
    
    Add_table_version("imports", copy(df_imports$data_imports))  
  }
  
})



observeEvent(input$add_Imports, {
  showModal(viewImportsTriplets())
})


viewImportsTriplets <- function(failed = FALSE) {
  
  
  modalDialog(
    
    easyClose = TRUE, size= "l",
    dataTableOutput("viewImports")
    ,
    
    footer = tagList(
      
      actionButton("importsInsert", "Insert")
    )
  )
  
}



output$viewImports= renderDataTable({
  
  
 
  commodity=copy(all_cpc)
  commodity <- commodity[order(CPCCode),]
  DT=commodity
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
  
  
  DT <- subset(DT,  !CPCCode %in% unique(df_imports$data_imports$CPCCode) )
  values_importsCommodities$importsCommodities <- DT
  # }
  
  # if (input$stockinsertcommodities >0 ){
  #   values_stockCommodities$stocksCommodities <- subset(values_stockCommodities$stocksCommodities,  !CPCCode %in% unique(data.table(df_stock$data_stock)$CPCCode) )
  # }
  # 
  
  datatable(values_importsCommodities$importsCommodities,
            escape=F)
  
  
  
})


observeEvent(input$importsInsert, {
  
  removeModal()
  
})


proxy_imports = dataTableProxy('imports')

observeEvent(input$imports_cell_edit, {
  
  
  info = input$imports_cell_edit
  
  print(info)
  i = info$row
  j = (info$col + 1)
  v = info$value
  df_imports$data_imports[i,(j) := v]
  
  replaceData(proxy_imports, df_imports$data_imports, resetPaging = FALSE,rownames = FALSE)  # important
  
  info1 <- input[["imports_cell_edit"]]
  i <- info1[["row"]]
  j <- info1[["col"]]
  runjs(colorizeCell(i, j+1,"imports"))
  
  Add_table_version("imports", copy(df_imports$data_imports)) 
})




observeEvent(input$importsInsert, {
  
  
  s=as.numeric(input$viewImports_rows_selected)
  
  if (length(s) == 0){
    
    data_current <- data.table(df_imports$data_imports)
    
    
    
    df_imports$data_imports <- data_current
    
    
  }
  
  else {
    
    
    importslistTool <- copy(all_cpc)
    importslistTool <- importslistTool[order(CPCCode),]
    
    importslistTool <- subset(importslistTool,  !CPCCode %in% unique(isolate(df_imports$data_imports$CPCCode) ))
    
    yy=importslistTool[s,]
    
    # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff=melt.data.table(yy[,c("CPCCode", "Commodity")], id.vars = c("CPCCode", "Commodity"))
    ff[, ElementCode := c("5610")]
    
    
    
    oo=merge(ff,all_elements, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
    setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
    oo=oo[order(CPCCode)]
    
    data=isolate(df_imports$data_imports)
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
    
    df_imports$data_imports <- data
    
    
    Add_table_version("imports", copy(df_imports$data_imports)) 
    
  }
  
  
  
  
})





#delete rows 

observeEvent(input$delete_btn_imports, {
  t = copy(df_imports$data_imports)
  final_data <- copy(df_imports$data_imports)
  
  if (!is.null(input$imports_rows_selected)) {
    t <- t[as.numeric(input$imports_rows_selected),]
    
    cpc_code_to_remove <- unique(t$CPCCode)
    ele_code_to_remove <- unique(t$ElementCode)
  }
  
  
  df_imports$data_imports<- final_data[!CPCCode %in% cpc_code_to_remove]
  database <- copy(countryData)
  
  database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% c("5610"))]
  
  countryData <<- database
  
  Add_table_version("imports", copy(df_imports$data_imports))
})


#download excle file (#downloadCrop)

output$downloadImports <- downloadHandler(
  
  filename = function() {
    
    "import_data.xlsx"
  },
  
  
  content = function(file) {
    
    data_download_imports <- data.table(df_imports$data_imports)
    
    data_download_imports <- data_download_imports[!is.na(CPCCode)]
    
    data_download_imports[,hidden := NULL]
    
    write.xlsx(data_download_imports ,file,row.names = FALSE)
  }
  
)


# #upload crop denormalized data (#fileCrop)

observeEvent(input$fileImportsdenormalized,{


  inFile <- input$fileImportsdenormalized
 
  
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
  
  
  
  
  # write.csv(data.table(df_imports$data_imports),"imports_Data.csv",row.names = F)
  
  # imports_Data <- fread("imports_Data.csv")
  
  imports_Data <- data.table(df_imports$data_imports)
  
  
  imports_Data <- long_format(imports_Data)
  
  imports_Data[, c("Commodity","Element") := NULL]
  
  imports_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
  
  
  
  xx=imports_Data[!is.na(Value)][
    data_denormalized,
    on = c("CPCCode", "ElementCode", "Year")
    
    ]
  
  
  xx[, c("Value","Flag"):= NULL]
  
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  
  imports_Data <- imports_Data[
    !xx,
    on = c("CPCCode", "ElementCode", "Year")
    
    ]
  
  imports_Data <- rbind(imports_Data,xx)
  
  imports_Data <- merge(imports_Data, all_elements, by = "ElementCode", all.x = T)
  
  imports_Data <- merge(imports_Data, all_cpc, by= "CPCCode", all.x = T)
  
  imports_Data <- imports_Data[!is.na(Element)]
  
  imports_Data <- subset(imports_Data, Year %in% 2010:END_YEAR)
  
  imports_Data <- wide_format(imports_Data)
  
  imports_Data = visualize_data(imports_Data,END_YEAR)

  imports_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]

  df_imports$data_imports <- imports_Data
  
  Add_table_version("imports", copy(df_imports$data_imports)) 

})



observeEvent(input$uploadImportsModal, {
  showModal(uploadImports())
})
# 


uploadImports <- function(failed = FALSE) {

  
  modalDialog(size = "l",
              
              
              titlePanel("Upload File"),
              
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Select a file ----
                  fileInput("fileImports", "Choose Country Import Excel File (.xlsx)",
                            multiple = TRUE,
                            accept = NULL),
                  # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                  
                  selectizeInput("importHS", "HS Code (Please make sure that HS Code should contain at least 6 digits) :",
                                 selected = NULL, choices = c("", colnames(df_importsCountry$data_importsCountry)),multiple=F),
                  selectizeInput("importCommodity", "Commodity :",
                                 selected = NULL, choices = c("", colnames(df_importsCountry$data_importsCountry)),multiple=F),
                  
                  selectizeInput("importQuantity", "Quantity (Please make sure that the quantity is in tons) :",
                                 selected = NULL, choices = c("", colnames(df_importsCountry$data_importsCountry)) ,multiple=F),
                  selectizeInput("importYear", "Year :",
                                 selected = NULL, choices = c("", colnames(df_importsCountry$data_importsCountry)),multiple=F),
                  actionButton("importMap","Map Import Data with CPCCode")
                  
                ),
                
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Data file ----
                  # dataTableOutput("importCountry")
                  div(style = 'overflow-x: scroll', dataTableOutput('importCountry'))
                )
                
              )
  )
  
  
  
  
}

 
 


observe({
  
  updateSelectInput(session, "importHS", choices = c("", colnames(df_importsCountry$data_importsCountry)))
  updateSelectInput(session, "importCommodity", choices = c("", colnames(df_importsCountry$data_importsCountry)))
  updateSelectInput(session, "importQuantity", choices = c("",colnames(df_importsCountry$data_importsCountry)))
  updateSelectInput(session, "importYear", choices = c("",colnames(df_importsCountry$data_importsCountry)))
})




observeEvent(input$importMap, {
  
  removeModal()
  
})




output$importCountry <- renderDataTable({
  
  req(input$fileImports)
  
  
  
  inFile <- input$fileImports
  
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  
  df_importsCountry$data_importsCountry <- DATA
  
  
  datatable(df_importsCountry$data_importsCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  
})


observeEvent(input$importMap,{

  data <- df_imports$data_imports
  
 
  
if (input$importYear== ""| input$importHS == ""| input$importQuantity == ""){
  
  
  sendSweetAlert(
    session = session,
    title = "Warning !!",
    text = "Invalid data",
    type = "warning"
  )
  
  df_imports$data_imports <- data
}else if (!is.null(input$fileImports)){

  sendSweetAlert(
    session = session,
    title = "Warning !!",
    text = paste("You are deleting existing import trade data of years", input$fromyear, "to", input$endyear),
    type = "warning"
  )
  
  
  
  
  data <-tradeImportMapping_old(input,output,session)
  
  data <- data[!is.na(CPCCode)]

  data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]

  df_imports$data_imports <- data
  
  Add_table_version("imports", copy(df_imports$data_imports)) 

}
  
  
###newly added for not mapped data
  
  viewnotmappedImports <- function(failed = FALSE) {
    
    
    modalDialog(
      
      title = "The following HS codes were not mapped for Imports Trade Data",
      easyClose = TRUE, size= "l",
      dataTableOutput("notmapped_imports"),
    
      
      footer = tagList(

        downloadButton("notmappedimports_download", "Download Excel File"),
        modalButton("Dismiss")
      )
    )
    
  }
  
  
  showModal(viewnotmappedImports())
  
  
  output$notmapped_imports= renderDataTable({
    
    
    
    datatable(df_imports_not_mapped$data_imports_not_mapped,
              escape=F)
    
    
  })
  
  

  
})



output$notmappedimports_download <- downloadHandler(
  
  filename = function() {
    
    "import_data_not_mapped.xlsx"
  },
  
  
  content = function(file) {
    
    
    
    write.xlsx(df_imports_not_mapped$data_imports_not_mapped,file,row.names = FALSE)
  }
  
)

############################################## 


#save crop

observeEvent(input$saveImports,{
  
  data_to_save <- copy(df_imports$data_imports)
  
  data_to_save[,hidden := NULL]
  
  save_to_database(data = data_to_save,countryData,year_range = c(input$fromyear:input$endyear))
  
  new_saved_data_imports <- return_data_base(data_to_save)

  df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_imports
  
  

  
  
  
})



output$imports <- 
  
 
  renderDataTable(
    if(!is.null(df_imports$data_imports)){
    
    datatable (df_imports$data_imports, rownames= FALSE,class = 'cell-border stripe', 
               
               editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_imports$data_imports) == input$fromyear)-2)))), 
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
                                   list(visible = FALSE, targets = (ncol(df_imports$data_imports)-1))
                 ) 
               ))  %>%
      
      formatStyle(0:ncol(df_imports$data_imports), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
   
    }
  )
    
}