residual=function(input,output,session){
  
  
  values_residualCommodities <- reactiveValues(residualCommodities = NULL)

  
  
  residual_data= reactive({
    
    residualData=subset(countryData, ElementCode == 5166)
    # 
    
    setDT(residualData)
    
    residualData <- residualData[!duplicated(residualData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
    
    # residualData[ElementCode == 5153, `:=` (ElementCode = "5166")]
 
   
    residualData[, c("CountryM49","Country"):=NULL]
    
    residualData=wide_format(residualData)
    
    END_YEAR=input$endyear
    residualData=visualize_data(residualData,END_YEAR)
    
    residualData   
  
    
}) 
  
  
  # 
  # finalResidualLongformat= reactive({
  #   data= data.table(hot_to_r(input$residualTable))
  #   
  #   yyy= melt.data.table(data, id.vars = c("CountryM49","Country", "CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =c( "2010","2011","2012","2013",  "2014", "2015"),
  #                        value.name= "Value" )
  #   setnames(yyy,"variable", "Year")
  #   
  #   yyy[,Year:= as.character(Year)]
  #   xxx= melt.data.table(data, id.vars = c("CountryM49","Country", "CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =c( "Flag 2010","Flag 2011", "Flag 2012" ,"Flag 2013", "Flag 2014","Flag 2015"),
  #                        value.name= "Flag" )
  #   
  #   setnames(xxx,"variable", "Year")
  #   xxx[,Year:=substring(Year,6)]
  #   xxx[,Year:=as.character(Year)]
  #   
  #   kk=merge(yyy, xxx, by = intersect(names(yyy), names(xxx)))  
  #   
  #   kk
  #   
  #   
  #   
  # })
  
  observeEvent(input$residualSave, {
    
    residualNew=data.table(hot_to_r(input$residualTable))
    residualNew=long_format(residualNew)
    residualNew[,c("Commodity","Element") := NULL]
    
    
    
    residualOld=subset(countryData, ElementCode == 5166 & Year %in% c(2010:as.numeric(input$endyear)))
    
    # cropOld=copy(cropData)
    # residualOld=long_format(residualOld)
    residualOld[,c("Commodity","Element") := NULL]
    
    originalData =copy(countryData)
    
    originalData[,c("CountryM49","Country","Commodity","Element") := NULL]
    
    
    originalData= originalData[ !(CPCCode %in% residualOld[, CPCCode] & Year %in% residualOld[,Year]  & ElementCode %in% residualOld[, ElementCode])]
    
    originalData=rbind(originalData,residualNew)
    
    
    originalData=merge(originalData,elementName, by = "ElementCode" , all.x = TRUE)
    
    originalData=merge(originalData,commodityName, by = "CPCCode" , all.x = TRUE)
    
    originalData[, CountryM49 := gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1))]
    
    originalData = merge(originalData,countryName, by = "CountryM49", all.x = TRUE)
    
    setcolorder(originalData,c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
    
    originalData<-originalData[!duplicated(originalData)]
    
    save(originalData, file = "Data/countrySUA.RData")
    
    # write.xlsx(data.table(hot_to_r(input$crop)), "Data/ggg.xlsx",row.names = FALSE)
    
    countryData <<- originalData  
    
  }) 
  
  
  addRow=reactive({
    
    
    if (input$insertResidual >0  )  {
      s=as.numeric(input$residualItem_rows_selected)
     
      
  if (length(s) == 0){
    
    data=hot_to_r(input$residualTable)
    
  } else {
    
    s=as.numeric(input$residualItem_rows_selected)
    table <- values_residualCommodities$residualCommodities[s,][,Select :=NULL]
    
    
    
    
    
    table=data.table(table,"ElementCode" = "5166", "Element" = "Residual other uses [t]")
    
    oo=unique(table$CPCCode)
    
    
    data=isolate(hot_to_r(input$residualTable))
    data=data.table(data)
    
    
    if (!(oo %in%  data$CPCCode)){
      
      data=rbind(table, data,fill=T)
      
      data=data[!is.na(ElementCode)]
      data[is.na(data)] <- ""
    } 
    
    
  }   
      
 }
    else {
      
      data=hot_to_r(input$residualTable)
      
      
    }
    
    
    yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    
    data
    
    
})
  
  
  
  output$downloadResidual <- downloadHandler(
    
    
    
    
    filename = function() {
      
      "table.xlsx"
    },
    
    
    content = function(file) {
      
      
      
      write.xlsx(data.table(hot_to_r(input$residualTable)) ,file,row.names = FALSE)
    }
    
  )
  
  
  
  
  
  
  
  
  
  
  valuesResidual <- reactiveValues(Residual = 'initial')
  
  observeEvent(input$fileResidual,  {valuesResidual$Residual = 'upload'})
  observeEvent(input$insertResidual,  {valuesResidual$Residual = 'add'})
  
  
  
  
  
  
  
  
  
  output$residualTable=renderRHandsontable({
    
  
    DATA=residual_data()
    
    
    if ( valuesResidual$Residual == 'upload' ){
      
      inFile <- input$fileResidual
      
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      DATA=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
      
      END_YEAR=input$endyear
      DATA = visualize_data(DATA,END_YEAR)
      
      
      # reset("fileStockchanges")
      
      # print(input$fileStockchanges$name)
      
      
      
    }
    
    if (valuesResidual$Residual == 'add') {
      
      
      DATA=addRow()
      
    }
       
    t=as.numeric(input$endyear)
    
    number_to_freeze = which( colnames(DATA) ==  paste("Flag",(t-1)))
    
    

    
    rhandsontable(DATA,
                  
                  height = 450,undo = T, redo = T,
                  useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
                  , selectCallback = TRUE, fontweight= "bold",search= TRUE )%>%
      hot_cols(fixedColumnsLeft = 6)%>%
      hot_context_menu(allowRowEdit = TRUE)%>%
      hot_col("Commodity", width= 250)   %>%
      hot_col("ElementCode", halign = "htRight", width = 120)%>%
      hot_col("Element", halign = "hLeft", width = 150)%>%
      hot_col("CPCCode", halign = "htRight", width= 100)%>%
      hot_cols(format = '0,0')%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
    
    hot_col(1:number_to_freeze, readOnly = TRUE)
    
    
    
    
    
    
    
    
    
    
#     rhandsontable(wideformat_residual(), undo = T, redo = T, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,
#                   selectCallback = TRUE, fontweight= "bold",search=TRUE)%>%
#       
#       hot_col("Commodity", width=380)%>%
#       # hot_col("Element", width=255)%>%
#       hot_col("ElementCode", halign = "htRight")%>%
#       hot_col("CPCCode", halign = "htRight")%>%
#       hot_col("CountryM49", halign = "htRight")%>%
#       hot_col(c("2010","2011","2012", "2013", "2014", "2015"), halign = "htRight")%>%
#       hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
#     hot_cols(fixedColumnsLeft = 6)%>%
#     hot_context_menu(
#       customOpts = list(
#         search = list(name = "Search",
#                       callback = htmlwidgets::JS(
#                         "function (key, options) {
#                       var srch = prompt('Search criteria');
#                       
#                       this.search.query(srch);
#                       this.render();
# }"))))
#     
    
    
  })
  
  
  
#####################################  
  observeEvent(input$residualinsertcommodities, {
    showModal(residualCommoditiesModal())
  })  
  
  
  residualCommoditiesModal <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("residualItem")
      ,
      
      footer = tagList(
        
        actionButton("insertResidual", "Insert")
      )
    )
    
  }
  
  
  output$residualItem= renderDataTable({
    
    if (is.null(values_residualCommodities$residualCommodities)) {
      commodity=fread("Data/commodityList.csv")
      DT=commodity
      DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
      values_residualCommodities$residualCommodities <- DT
    }
    
    if (input$residualinsertcommodities >0 ){
      values_residualCommodities$residualCommodities <- subset(values_residualCommodities$residualCommodities,  
                                                               !CPCCode %in% unique(isolate(hot_to_r(input$residualTable))$CPCCode) )
    }
    
    
    datatable(values_residualCommodities$residualCommodities,
              escape=F)
    
    
    
  })
  
  observeEvent(input$insertResidual, {
    
    removeModal()
    
  })  

    
  residualRow=reactive({
    # values=reactiveValues()
    # values$df=wideformat_crop()
    
    if (input$insertResidual >0 )  {
      s=input$residualItem_rows_selected
      if (!is.null(s)){
        
        itemList=fread("Reference Files/commodityList.csv")
        yy=itemList[s,]
        
        
        # ff[,variable:=NULL]
        # setnames(ff,"value", "ElementCode")
       
        yy[,ElementCode := "5166"]
        yy[,Element := "Residual other uses [t]"]

        
        setcolorder(yy,c("CPCCode", "Commodity", "ElementCode", "Element"))
        
        data=isolate(hot_to_r(input$residualTable))
        
        if (!(yy$CPCCode %in% data$CPCCode)){
          data=rbind(yy,data,fill=T)
        }
        # data[order(CPCCode)]
      } else {
        
        data=hot_to_r(input$residualTable)
        
      }
    }
    
    
    
    else{
      
      data=hot_to_r(input$residualTable)
      
    }
    
    # data=data[order(CPCCode)]
    
    data 
    
    
  })
  
  
  
  
   
  
}