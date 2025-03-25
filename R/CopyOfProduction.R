Production=function(input,output,session){

  values_livestockCommodities <- reactiveValues(livestockCommodities = NULL)
  
  valuesxxx <- reactiveValues(test = 'initial')
  valuesxxxx <-reactiveValues(livestock = 'initial3')
  
  observeEvent(input$fileCrop,  {valuesxxx$test = 'upload'})
  observeEvent(input$cropInsert,  {valuesxxx$test = 'add'})
  
  
  
observeEvent(input$crop$changes$changes, {valuesxxx$test = 'automatic'})





  
 crops=reactive({
   
   # cropData=createCropdata(countryData,initial_year, end_year,session)
   
  
  
   END_YEAR=input$endyear
   
   cropData=subset(countryData, CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode])
   & ElementCode %in% c("5510","5312","5421","5327","5423") )
    
   
   #take production that do not appear in classification table
   
   crop_production_data <- subset(countryData, CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode])
                                   )
   
   
   
    setDT(cropData)
    
    cropData <- cropData[!duplicated(cropData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
    
    cropData[, c("CountryM49","Country"):=NULL]
    
    cropData <- subset(cropData, Year %in% c(2010 : END_YEAR) )
    
    cropData <- cropData[!is.na(Value)]
    
    
    cropData=wide_format(cropData)
   
  
    cropData=visualize_data_production(cropData,END_YEAR, session)
    
    
    # remove NA rows for all years columns 
    
    

     
     
    cropData
   
   
 })

 
 automatic_calculation_Crop = reactive({
   
   
   if (!is.null(input$crop$changes$changes)){
   
   datacopy=data.table(hot_to_r(input$crop))

           row.no <- as.numeric(unlist(input$crop$changes$changes)[1])
           
           col.no <- as.numeric(unlist(input$crop$changes$changes)[2])

           flagcols <- grep("^Flag", names(datacopy), value = TRUE)
           yearcols <- grep("^Value", names(datacopy), value = TRUE) 
           
           
         
          col.name <- colnames(datacopy)[as.numeric(col.no) +1]
       
          element = datacopy[[(row.no + 1), "ElementCode"]]
          
 
          # year=colnames(datacopy)[col.no+1]
         
          cpccode= datacopy[[(row.no + 1), "CPCCode"]]

       
   
   
#   if(element == "5421" & !(col.name %in% flagcols)){#if Yield is changed, Production should be updated
# 
#    y=datacopy[ElementCode == "5312" & CPCCode==cpccode, col.no+1, with=F]
#     p=datacopy[ElementCode == "5421" & CPCCode==cpccode, col.no+1 , with=F]
# 
#   
# if (nrow(y) != 0 & nrow(p) != 0){
# 
# 
#  datacopy[ElementCode == "5510" & CPCCode== cpccode, (names(datacopy)[col.no+1]):= y*p]
# 
# 
# 
# }
#  
#   }
          

 # else if (element == "5510" & !(col.name %in% flagcols)){#if Production is changed, Area Harvested should be updated

          
          if (element == "5510" & !(col.name %in% flagcols)){#if Production is changed, Area Harvested should be updated

    p=datacopy[ElementCode == "5510" & CPCCode==cpccode, col.no+1, with=F]
    a=datacopy[ElementCode == "5312" & CPCCode==cpccode, col.no+1, with=F]

    if (nrow(a) != 0 & nrow(p) != 0){

    datacopy[ElementCode == "5421" & CPCCode== cpccode, (names(datacopy)[col.no+1]):= round(p/a,2)]
    }

  }
          
          
  else if (element == "5312" & !(col.name %in% flagcols)){#if Area Harvested is chnaged, Production should be updated


    a=datacopy[ElementCode == "5312" & CPCCode==cpccode ,  col.no+1, with=F]
    p=datacopy[ElementCode == "5510" & CPCCode==cpccode, col.no+1, with=F]
    if (nrow(a) != 0 & nrow(p) != 0){
            datacopy[ElementCode == "5421" & CPCCode== cpccode, (names(datacopy)[col.no+1]):= round(p/a,2)]
    }

      }
        

          
          
   } else {
     
     
     datacopy= data.table(hot_to_r(input$crop))
     
   }
          
datacopy
  
        
   
 })


 
 
 automatic_calculation_Stock = reactive({
   
   
   if (!is.null(input$livestock$changes$changes)){
     
     datacopy_livestock=data.table(hot_to_r(input$livestock))
     
     row.no <- as.numeric(unlist(input$livestock$changes$changes)[1])
     
    
     
     
     element = datacopy_livestock[[(row.no + 1), "ElementCode"]]
     
     
     col.no <- as.numeric(unlist(input$livestock$changes$changes)[2])
     
     
     flagcols <- grep("^Flag", names(datacopy_livestock), value = TRUE)
     yearcols <- grep("^Value", names(datacopy_livestock), value = TRUE) 
     
     
     
     col.name <- colnames(datacopy_livestock)[as.numeric(col.no) +1]
     
     # year=colnames(datacopy_livestock)[col.no+1]
    
     
     # year= datacopy[[(row.no + 1), Year]]
     # print(year)
     
     cpccode= datacopy_livestock[[(row.no + 1), "CPCCode"]]
     
     # stocklistTool=fread("Data/livestockListTool.csv")
     
     stocklistTool <- subset(classification, classification %in% c("LD","L","LA","LP"))
     stocklistTool[,classification := NULL]
     
     productivityCodes <- unique(stocklistTool$`Productivity Code`)
     inputCodes <- unique(stocklistTool$`Input Code`)
     outputCodes <- unique(stocklistTool$`Output Code`)
     
     # if(element %in%  productivityCodes & !is.na(element) & !(col.name %in% flagcols)){#if Yield is changed, Production should be updated
     # 
     #           inp=stocklistTool$`Input Code`[stocklistTool$CPCCode == cpccode]
     #           productivity=stocklistTool$`Productivity Code`[stocklistTool$CPCCode == cpccode]
     #           outp=stocklistTool$`Output Code`[stocklistTool$CPCCode == cpccode]
     # 
     #           y=datacopy_livestock[ElementCode == inp & CPCCode==cpccode, col.no+1, with=F]
     #           p=datacopy_livestock[ElementCode ==  productivity & CPCCode==cpccode, col.no+1 , with=F]
     #           
     #         
     # 
     #           if (nrow(y) != 0 & nrow(p) != 0){
     # 
     #           datacopy_livestock[ElementCode == outp & CPCCode== cpccode, (names(datacopy_livestock)[col.no+1]):= y*10^(-3)*p]
     # 
     #           }
     # 
     # }
     # 
     # else if(element %in%  outputCodes & !is.na(element) & !(col.name %in% flagcols)){#if Production is changed, Area Harvested should be updated
       
       
       if(element %in%  outputCodes & !is.na(element) & !(col.name %in% flagcols)){#if Production is changed, Area Harvested should be updated

               inp=stocklistTool$`Input Code`[stocklistTool$CPCCode == cpccode]
               productivity=stocklistTool$`Productivity Code`[stocklistTool$CPCCode == cpccode]
               outp=stocklistTool$`Output Code`[stocklistTool$CPCCode == cpccode]

               p=datacopy_livestock[ElementCode == outp & CPCCode==cpccode, col.no+1, with=F]
               a=datacopy_livestock[ElementCode == inp & CPCCode==cpccode, col.no+1, with=F]
               
       #
               if (nrow(a) != 0 & nrow(p) != 0){
               datacopy_livestock[ElementCode == productivity & CPCCode== cpccode, (names(datacopy_livestock)[col.no+1]):= (p/a)*10^3]

               }

             }
     else if(element %in%  inputCodes & !is.na(element) & !(col.name %in% flagcols)){#if Area Harvested is chnaged, Production should be updated

               inp=stocklistTool$`Input Code`[stocklistTool$CPCCode == cpccode]
               productivity=stocklistTool$`Productivity Code`[stocklistTool$CPCCode == cpccode]
               outp=stocklistTool$`Output Code`[stocklistTool$CPCCode == cpccode]


               a=datacopy_livestock[ElementCode == inp & CPCCode==cpccode ,  col.no+1, with=F]
               p=datacopy_livestock[ElementCode == outp & CPCCode==cpccode, col.no+1, with=F]

               if (nrow(a) != 0 & nrow(p) != 0){

                 datacopy_livestock[ElementCode == productivity & CPCCode== cpccode, (names(datacopy_livestock)[col.no+1]):= (p/a)*10^3]
               }
  
             }
     
     
     
     
     
   }
   
   else {
     
     
     datacopy_livestock= data.table(hot_to_r(input$livestock))
     
   }
   
   datacopy_livestock
   
   
   
 })
 
 
 

 
 

 

 output$crop=renderRHandsontable({
   
   
   DATA=NULL
   
 if (is.null(DATA)){
DATA=crops()
}
#   
if (valuesxxx$test == 'automatic'){

  DATA=automatic_calculation_Crop()

 

}
 


   if ( valuesxxx$test == 'upload' ){

     inFile <- input$fileCrop

     file.rename(inFile$datapath,
                 paste(inFile$datapath, ".xlsx", sep=""))
     DATA=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
     END_YEAR=input$endyear
     DATA = visualize_data(DATA,END_YEAR)
 

   }
   
   if (valuesxxx$test == 'add') {


     DATA=addingRowsCropReactive()

   }

   

  numeric_columns  <- grep("^[[:digit:]]{4}$", names(DATA), value = TRUE)
  
  flag_columns <- grep("^Flag}$", names(DATA), value = TRUE)

  t=as.numeric(input$endyear)
  
  number_to_freeze = which( colnames(DATA) ==  paste("Flag",(t-1)))
  
  DATA[ElementCode %in% c("5312","5510"),(numeric_columns) := round(.SD,0), .SDcols=numeric_columns]
 
  
  DATA[ElementCode %in% c("5421"),(numeric_columns) := round(.SD,2), .SDcols=numeric_columns]
  
   # rhandsontable(DATA, undo = T, redo = T, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T,
   #              width = 10, selectCallback = TRUE, fontweight= "bold",search= TRUE)%>%
  
  
  
  
  
  rhandsontable(DATA,undo = T, redo = T,stretchH = "all",height = 450,
                useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
                , selectCallback = TRUE, fontweight= "bold",search= TRUE,colHeaders = names(DATA))%>%
    
    
       hot_validate_numeric(cols = as.character(2010:input$endyear), exclude = "" )%>%
     
       hot_validate_character(cols = grep("^Flag", names(DATA), value = TRUE), choices= c("", "T", "E", "I", "M") )%>%
    
    hot_cols(fixedColumnsLeft = 4)%>%
    hot_context_menu(allowRowEdit = TRUE)%>%
    hot_cols(format = '0,0')%>%
    hot_col(c("Commodity"),width=250)%>%
    hot_col("ElementCode", halign = "htRight", width = 120)%>%
    hot_col("Element", halign = "hLeft", width = 150)%>%
    hot_col("CPCCode", halign = "htRight", width= 100)%>%
    hot_cols(manualColumnResize=TRUE) %>%
    hot_col(numeric_columns,width = 90)%>%
    hot_col(1:number_to_freeze, readOnly = TRUE)%>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE)

  
  # rhandsontable(DATA, undo = T, redo = T,stretchH = "all",height = 450,
  #               useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
  #                            , selectCallback = TRUE, fontweight= "bold",search= TRUE )%>%
  # 
  #    hot_validate_numeric(cols = as.character(2010:input$endyear), exclude = "" )%>%
  # 
  #    hot_validate_character(cols = grep("^Flag", names(DATA), value = TRUE), choices= c("", "T", "E", "I", "M") )%>%
  # 
  # 
  #    # hot_cols(grep("^Flag", names(DATA), value = TRUE), width= 80) %>%
  #    hot_col("Commodity",width = 250)   %>%
  #    hot_col("ElementCode", halign = "htRight")%>%
  #    hot_col("Element", halign = "hLeft")%>%
  #    hot_col("CPCCode", halign = "htRight")%>%
  #     hot_cols(format = '0.0')%>%
  #    hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
  #    hot_col(numeric_columns, format = "0")%>%
  #   #  hot_cols(fixedColumnsLeft = 0)%>%
  #   hot_cols(fixedColumnsLeft = 4)%>%
  #    hot_col(numeric_columns,width = 105)%>%
  #   hot_col(1:number_to_freeze, readOnly = TRUE)

 })
 

 
 observeEvent(input$livestockpage, {
  
   cropNew=data.table(hot_to_r(input$crop))
   
   # cropNew <- read_excel("Bhutan/test.xlsx")
   # 
   # cropNew <- data.table(cropNew)
   
   # write.xlsx(cropNew,"Bhutan/test.xlsx", row.names=FALSE)
   
   cropNew=long_format(cropNew)
   cropNew[,c("Commodity","Element") := NULL]
   
   cropNew[, Flag := as.character(Flag)]
   
   cropOld=automatic_calculation_Crop()
   
   # cropOld=copy(cropData)
   cropOld=long_format(cropOld)
   cropOld[,c("Commodity","Element") := NULL]
   
   
   originalData= copy(countryData)
   originalData[,c("CountryM49","Country","Commodity","Element") := NULL]
   
   
   originalData= originalData[ !(CPCCode %in% cropOld[, CPCCode] & Year %in% cropOld[,Year]  & ElementCode %in% cropOld[, ElementCode])]
   
   originalData=rbind(originalData,cropNew)
   
   
   
   
   
   originalData=merge(originalData,elementName, by = "ElementCode" , all.x = TRUE)
   
   originalData=merge(originalData,commodityName, by = "CPCCode" , all.x = TRUE)
   
   originalData[, CountryM49 := gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1))]
   
   originalData = merge(originalData,countryName, by = "CountryM49", all.x = TRUE)
   
   setcolorder(originalData,c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
   
   originalData<-originalData[!duplicated(originalData)]
   
   save(originalData, file = "Data/countrySUA.RData")
   
   countryData <<- originalData
   
   # write.xlsx(data.table(hot_to_r(input$crop)), "Data/ggg.xlsx",row.names = FALSE)
   # print(gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1)))

})
 
 
livestock= reactive({
  
  
  END_YEAR=input$endyear
  
  # elementCodes_livestock= fread("Data/livestockListTool.csv")
  
  elementCodes_livestock <- subset(classification, classification %in% c("LP","LD","L"))
  
  elementCodes_livestock[,classification := NULL]
  

  elementCodes_livestock=unique(elementCodes_livestock[,c("Input Code","Productivity Code","Output Code")])

  elementCodes_livestock=subset(elementCodes_livestock, `Output Code` == 5510)


  elementCodes_livestock = unique(c(elementCodes_livestock$`Input Code`, elementCodes_livestock$`Productivity Code`,
                                    elementCodes_livestock$`Output Code`))
  
  elementCodes_livestock <- elementCodes_livestock[!is.na(elementCodes_livestock)]


  data=subset(countryData, CPCCode %in% unique(subset(classification, classification %in% c("LP","LD","L"))[,CPCCode])
              & ElementCode %in% elementCodes_livestock)

  
  data <- data[!duplicated(data,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]

  data[, c("CountryM49","Country") :=NULL]
  
  data <- subset(data, Year %in% c(2010 : END_YEAR) )
  
  data <- data[!is.na(Value)]
  

  data=wide_format(data)

  setDT(data)

 

  data=visualize_data_production(data,END_YEAR)

data  


})



observeEvent(input$cropInsert, {

  removeModal()

})


observeEvent(input$stockInsert, {

  removeModal()

})





cropDerivedModal <- function(failed = FALSE) {
  
  
  modalDialog(
    
    easyClose = TRUE, size= "l",
    dataTableOutput("cropDerived")
    ,
    
    footer = tagList(
      
      actionButton("cropDerivedInsert", "Insert")
    )
  )
  
}


observeEvent(input$cropderived, {
  showModal(cropDerivedModal())
})



output$cropDerived= renderDataTable({
  
  croplistTool=fread("Data/commodityList.csv")
  DT=croplistTool
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
  
  datatable(DT,
            escape=F)
  
  
})


observeEvent(input$cropDerivedInsert, {
  
  removeModal()
  
})





addingRowsStockReactive=reactive({

  
  # data=hot_to_r(input$stock)

  if (input$stockInsert >0 )  {
    s=input$viewStock_rows_selected
    
    
    
  if (length(s) == 0){
    
    
    data=hot_to_r(input$livestock)
    
    
  }
    
    else {
    
    table <- values_livestockCommodities$livestockCommodities[s,][,Select :=NULL]

    yy=copy(table)
    
    ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff[,variable:=NULL]
    setnames(ff,"value", "ElementCode")
    
    elementName = read_excel("Data/Reference File.xlsx",sheet = "Elements")
    elementName = data.table(elementName)
    
    oo=merge(ff,elementName, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
    setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
    oo=oo[order(CPCCode)]
    
    data=isolate(hot_to_r(input$livestock))
    data=data.table(data)
    
    
    
    if (!(unique(oo$CPCCode) %in% data$CPCCode)){
      
      data=rbind(oo,data,fill=T)
      data=data[!is.na(ElementCode)]
      data[is.na(data)] <- ""
      
      
      yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
      
      data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
      
      data[is.na(data)] <- 0
    }
    
    
    
    
    
    }
  }
  
  else {
    
    data=hot_to_r(input$livestock)
    
    
  }
    
    

 data   
   


})





valuesyyy <- reactiveValues(test2 = 'initial2')

observeEvent(input$fileLivestock,  {valuesyyy$test2 = 'upload2'})
observeEvent(input$stockInsert,  {valuesyyy$test2 = 'add2'})

observeEvent(input$livestock$changes$changes, {valuesyyy$test2 = 'automaticlivestock'})



output$livestock=renderRHandsontable({
  
  DATA=NULL
  
  
if (is.null(DATA)){
  DATA=livestock()
}
  
  
  
  if (valuesyyy$test2 =='automaticlivestock'){

    DATA=automatic_calculation_Stock()

  }

  
  # observeEvent(input$stock$changes$changes, {valuesxxx$livestock = 'automaticlivestock'})
  
  
  
  

  if ( valuesyyy$test2 == 'upload2' ){
    
    inFile <- input$fileLivestock
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
    END_YEAR=input$endyear
    DATA = visualize_data(DATA,END_YEAR)
    
    # reset("fileStockchanges")
    
    # print(input$fileStockchanges$name)
    
    
    
  }
  
  if (valuesyyy$test2 == 'add2') {
    
    
    DATA=addingRowsStockReactive()
    
  }

  
  numeric_columns  <- grep("^[[:digit:]]{4}$", names(DATA), value = TRUE)
  

  t=as.numeric(input$endyear)
  
  number_to_freeze = which( colnames(DATA) ==  paste("Flag",(t-1)))
  
  DATA[ElementCode %in% c("5312","5510","5318","5314","5319","5320"),(numeric_columns) := round(.SD,0), .SDcols=numeric_columns]
  DATA[!ElementCode %in% c("5312","5510","5318","5314","5319","5320"),(numeric_columns) := round(.SD,2), .SDcols=numeric_columns]
  

  rhandsontable(DATA,height = 450,undo = T, redo = T, stretchH = "all", 
                useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
                , selectCallback = TRUE, fontweight= "bold",search= TRUE ,colHeaders = names(DATA))  %>%
    
    hot_validate_numeric(cols = as.character(2010:input$endyear), exclude = "" )%>%
    
    hot_validate_character(cols = grep("^Flag", names(DATA), value = TRUE), choices= c("", "T", "E", "I", "M") )%>%

    hot_col("Commodity", width= 250)   %>%
    hot_col("Element", halign = "hLeft", width = 250)%>%
    hot_cols(format = '0,0')%>%
    hot_col("ElementCode", halign = "htRight", width = 100)%>%
    hot_col("CPCCode", halign = "htRight", width= 100)%>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE,stretchH = "all")%>%
    hot_cols(fixedColumnsLeft = 4) %>%
    hot_col(numeric_columns,width = 105)%>%
    hot_col(1:number_to_freeze, readOnly = TRUE)%>%
    hot_cols(manualColumnResize=TRUE)%>%
    hot_context_menu(
    customOpts = list(
      search = list(name = "Search",
                    callback = htmlwidgets::JS(
                      "function (key, options) {
                      var srch = prompt('Search criteria');
                      
                      this.search.query(srch);
                      this.render();
}"))))
  
  
})





observeEvent(input$tradepage, {

  
  liveNew=data.table(hot_to_r(input$livestock))
  
  liveNew=long_format(liveNew)
  liveNew[,c("Commodity","Element") := NULL]
  

  liveOld=automatic_calculation_Stock()
  
  # cropOld=copy(cropData)
  liveOld=long_format(liveOld)
  liveOld[,c("Commodity","Element") := NULL]
  
  
  originalData <- copy(countryData)
  originalData[,c("CountryM49","Country","Commodity","Element") := NULL]
  
  
  originalData= originalData[ !(CPCCode %in% liveOld[, CPCCode] & Year %in% liveOld[,Year]  & ElementCode %in% liveOld[, ElementCode])]
  
  originalData=rbind(originalData,liveNew)
  
  
  
  
  
  originalData=merge(originalData,elementName, by = "ElementCode" , all.x = TRUE)
  
  originalData=merge(originalData,commodityName, by = "CPCCode" , all.x = TRUE)
  
  originalData[, CountryM49 := gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1))]
  
  originalData = merge(originalData,countryName, by = "CountryM49", all.x = TRUE)
  
  setcolorder(originalData,c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
  originalData<-originalData[!duplicated(originalData)]
  
  save(originalData, file = "Data/countrySUA.RData")
  
  countryData <<- originalData
  

})



#triplet list appearing in the front for crops
output$croplist=renderDataTable({
  # croplistTool=fread("Data/cropListTool.csv")
  
  croplistTool <- subset(classification, classification %in% c("CP","C","CD"))
  
  croplistTool[, classification := NULL]

})


#set the button "View triplet" to allow the user to add triplets in the table




viewCropTriplets <- function(failed = FALSE) {


  modalDialog(

    easyClose = TRUE, size= "l",
    dataTableOutput("viewCrop")
   ,

    footer = tagList(

      actionButton("cropInsert", "Insert")
    )
    )

}


observeEvent(input$cropsTriplet, {
  showModal(viewCropTriplets())
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


observeEvent(input$stockTriplet, {
  showModal(viewStockTriplets())
})




observeEvent(input$saveprodDerivedmodi, {
 
  # cropNew=read_excel("Data/ggg.xlsx")
  # setDT(cropNew)
  

  
  
  cropNew=data.table(hot_to_r(input$crop))
  cropNew=long_format(cropNew)
  cropNew[,c("Commodity","Element") := NULL]
  
  
  
  cropOld=automatic_calculation_Crop()
  
  # cropOld=copy(cropData)
  cropOld=long_format(cropOld)
  cropOld[,c("Commodity","Element") := NULL]
  
  originalData =copy(countryData)
  
  originalData[,c("CountryM49","Country","Commodity","Element") := NULL]
  
  
  originalData= originalData[ !(CPCCode %in% cropOld[, CPCCode] & Year %in% cropOld[,Year]  & ElementCode %in% cropOld[, ElementCode])]
  
  originalData=rbind(originalData,cropNew)


  originalData=merge(originalData,elementName, by = "ElementCode" , all.x = TRUE)
 
  originalData=merge(originalData,commodityName, by = "CPCCode" , all.x = TRUE)
 
  # touristCode= sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1)
  # 
  # touristCode=gsub(" ", "", touristCode, fixed = TRUE)
  
  originalData[, CountryM49 :=gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1))]
  
  originalData = merge(originalData,countryName, by = "CountryM49", all.x = TRUE)
 
  setcolorder(originalData,c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
  
  originalData<-originalData[!duplicated(originalData)]
  
  save(originalData, file = "Data/countrySUA.RData")
  
  # write.xlsx(data.table(hot_to_r(input$crop)), "Data/ggg.xlsx",row.names = FALSE)
  
  countryData <<- originalData
  
  })




observeEvent(input$saveliveDerivedmodi, {

  liveNew=data.table(hot_to_r(input$livestock))
  liveNew=long_format(liveNew)
  liveNew[,c("Commodity","Element") := NULL]
  
  
  
  liveOld=automatic_calculation_Stock()
  
  # cropOld=copy(cropData)
  liveOld=long_format(liveOld)
  liveOld[,c("Commodity","Element") := NULL]
  
  
  originalData <- copy(countryData)
  originalData[,c("CountryM49","Country","Commodity","Element") := NULL]
  
  
  originalData= originalData[ !(CPCCode %in% liveOld[, CPCCode] & Year %in% liveOld[,Year]  & ElementCode %in% liveOld[, ElementCode])]
  
  originalData=rbind(originalData,liveNew)
  
  
  
  
  
  originalData=merge(originalData,elementName, by = "ElementCode" , all.x = TRUE)
  
  originalData=merge(originalData,commodityName, by = "CPCCode" , all.x = TRUE)
  
  originalData[, CountryM49 := gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1))]
  
  originalData = merge(originalData,countryName, by = "CountryM49", all.x = TRUE)
  
  setcolorder(originalData,c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
  originalData<-originalData[!duplicated(originalData)]
  
  save(originalData, file = "Data/countrySUA.RData")
  
  countryData <<- originalData
  
  # write.xlsx(data.table(hot_to_r(input$crop)), "Data/ggg.xlsx",row.names = FALSE)
  
})




output$downloadCrop <- downloadHandler(
  
  

  
  filename = function() {
   
   "table.xlsx"
  },
  
 
  content = function(file) {
    
    
    
    write.xlsx(data.table(hot_to_r(input$crop)) ,file,row.names = FALSE)
  }

)



output$downloadLivestock <- downloadHandler(
  
  
  
  
  filename = function() {
    
    "table.xlsx"
  },
  
  
  content = function(file) {
    
    
    
    write.xlsx(data.table(hot_to_r(input$livestock)) ,file,row.names = FALSE)
  }
  
)









observeEvent(input$saveprodmodi, {
  shinyjs::js$refresh()
  
})



observeEvent(input$savelivestockmodi, {
  shinyjs::js$refresh()
})



output$viewCrop= renderDataTable({

  # croplistTool=fread("Data/cropListTool.csv")
  
  croplistTool <-subset(classification, classification %in% c("C", "CD", "CP"))
  
  croplistTool[,classification := NULL]
  
  non_triplet= croplistTool[is.na(`Input Code`)] 
  
  #Requested by China to add 01421 anf 01422 to the crop list. 
  
  groundnuts <- data.table(CPCCode = c("01422","01421"), Commodity = c("Groundnuts in shell","Groundnuts in shell, seed for planting"), `Output Code` = "5510",Output = "Production [t]")
  
  non_triplet <- rbind(non_triplet,groundnuts, fill=T)
  
  
  triplet= croplistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
  
  
  # classification= read_excel("Data/crop_livestock_classification.xlsx")
  # classification = data.table(classification)
  # classification[, names(classification) := lapply(.SD, trimws)]
  # setDT(classification)
  
  classification_crop =classification[classification %in% c("C", "CD", "CP")]
  
  cpc2keep= unique(classification_crop$CPCCode)
  
  
  ##Requested by China to add 01421 anf 01422 to the crop list.
  cpc2keep <- c(cpc2keep, c("01421","01422"))
  
  
  non_triplet=subset(non_triplet, CPCCode %in% cpc2keep)
  
  
  fbscodes=fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  fbscodes=c(unique(fbscodes$id1),unique(fbscodes$id2),unique(fbscodes$id3),unique(fbscodes$id4))
  
  non_triplet=subset(non_triplet, !(CPCCode %in% fbscodes))
 
  
  croplistTool=rbind(triplet,non_triplet)
  
  
  DT=croplistTool
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')

  DT <- subset(DT,  !CPCCode %in% unique(hot_to_r(input$crop)$CPCCode) )
  
  
 datatable(DT,
            escape=F)


})



output$viewStock= renderDataTable({
  
  # values_livestockCommodities <- reactiveValues(livestockCommodities = NULL)
  
  if (is.null(values_livestockCommodities$livestockCommodities)){

  # stocklistTool=fread("Data/livestockListTool.csv")
  
    stocklistTool <- subset(classification, classification %in% c("LP","LD","L","LA"))
  
    stocklistTool[, classification := NULL]
  
  
  itemList=fread("Data/commodityList.csv")
  
  
  
  ff=itemList[!(CPCCode %in% unique(stocklistTool$CPCCode))]
  
  
  
# #here total CPC list should be filtered 
#   
#   classification= read_excel("Data/crop_livestock_classification.xlsx")
#   classification = data.table(classification)
#   # setnames(classification, "CPC 2.1 Exp. code","CPCCode")
#   classification[, names(classification) := lapply(.SD, trimws)]
#   
#   
#   setDT(classification)
  
  classification_livestock = classification[classification %in% c("LA", "L", "LP", "LD")]
  
  cpc2keepLive= unique(classification_livestock$CPCCode)
  
  
  ff=subset(ff, CPCCode %in%  cpc2keepLive)
  
  
  fbsCodes=fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  
  fbscode=c(unique(fbsCodes$id1),unique(fbsCodes$id2),unique(fbsCodes$id3),unique(fbsCodes$id4))
  
  
  ff=subset(ff, !(CPCCode %in% fbsCodes))
  
  
  stocklistTool=rbind(stocklistTool,ff,fill=T)
  
  
  stocklistTool[is.na(Output), `:=` (Output= "Production [t]", `Output Code`= "5510") ]
  
  
 
  

  DT=stocklistTool
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
  
  values_livestockCommodities$livestockCommodities <-DT
  }
  
  
if (input$stockTriplet >0){
  
  values_livestockCommodities$livestockCommodities <- subset(values_livestockCommodities$livestockCommodities, 
                                                             ! CPCCode %in% unique(isolate(hot_to_r(input$livestock))$CPCCode) )
  
  
}
   
datatable(values_livestockCommodities$livestockCommodities,
            escape=F)


})



#triplet list appearing in the front for livestock
output$livestocklist = renderDataTable({

  # livestocklistTool=fread("Data/livestockListTool.csv")
  
  livestocklistTool <- subset(classification, classification %in% c("LP","L","LD","LA"))
  
  livestocklistTool[,classification := NULL]
  
})




#derived adding for livestock


viewCropTriplets <- function(failed = FALSE) {
  
  
  modalDialog(
    
    easyClose = TRUE, size= "l",
    dataTableOutput("viewCrop")
    ,
    
    footer = tagList(
      
      actionButton("cropInsert", "Insert")
    )
  )
  
}


observeEvent(input$cropsTriplet, {
  showModal(viewCropTriplets())
})






observeEvent(input$cropInsert, {
  
  removeModal()
  
})





addingRowsCropReactive=reactive({
  
if (input$cropInsert > 0)  {
  s=as.numeric(input$viewCrop_rows_selected)
  
  
  
 if (length(s) == 0){
   
   data=hot_to_r(input$crop)
   
   
 }
  
  
  
else {
  
 
  # croplistTool=fread("Data/cropListTool.csv")
  
  croplistTool <-subset(classification, classification %in% c("CP","C","CD"))
  
  croplistTool[,classification := NULL]
  
  
  non_triplet= croplistTool[is.na(`Input Code`)] 
  
  #Requested by China to add 01421 anf 01422 to the crop list. 
  
  groundnuts <- data.table(CPCCode = c("01422","01421"), Commodity = c("Groundnuts in shell","Groundnuts in shell, seed for planting"), `Output Code` = "5510",Output = "Production [t]")
  
  non_triplet <- rbind(non_triplet,groundnuts, fill=T)
  
  triplet= croplistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
  
  
  # classification= read_excel("Data/crop_livestock_classification.xlsx")
  # classification = data.table(classification)
  # classification[, names(classification) := lapply(.SD, trimws)]
  # 
  # 
  # setDT(classification)
  
  classification_crop=classification[classification %in% c("C", "CD", "CP")]
  
  cpc2keep= unique(classification$CPCCode)
  
  
  cpc2keep <- c(cpc2keep,c("01421","01422"))
  
  non_triplet=subset(non_triplet, CPCCode %in% cpc2keep)
  
  
  fbscodes=fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  fbscodes=c(unique(fbscodes$id1),unique(fbscodes$id2),unique(fbscodes$id3),unique(fbscodes$id4))
  
  non_triplet=subset(non_triplet, !(CPCCode %in% fbscodes))
  
  
  croplistTool=rbind(triplet,non_triplet)
  
  
  croplistTool <- subset(croplistTool,  !CPCCode %in% unique(isolate(hot_to_r(input$crop))$CPCCode) )

    yy=croplistTool[s,]

    ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff[,variable:=NULL]
    setnames(ff,"value", "ElementCode")

    elementName = read_excel("Data/Reference File.xlsx",sheet = "Elements")
    elementName = data.table(elementName)

    oo=merge(ff,elementName, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
    setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
    oo=oo[order(CPCCode)]

    data=isolate(hot_to_r(input$crop))
    data=data.table(data)

  
      if (!(unique(oo$CPCCode) %in% data$CPCCode)){

                data=rbind(oo,data,fill=T)


                data=data[!is.na(ElementCode)]
                data[is.na(data)] <- ""
                yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)

                data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]

                data[is.na(data)] <- 0

    }
  
  
  
}  
  


}
  
  else {
    
    data=hot_to_r(input$crop)
    
  }
  
  data


})




}





