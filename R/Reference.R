Reference = function(input,output,session) {
  
  # commodityName = reactive({
  #   
  #   data = read_excel("Data/Reference File.xlsx",sheet = "SUA_Commodities") 
  #   data = data.table(commodityName) 
  #   data
  #   
  # })
  
  df_tree<- reactiveValues(data_commodityTree=NULL)
  df_nutrient<- reactiveValues(data_nutrient=NULL)
  
  
  
  flags_reactive <-reactive({
    
    flags= read_excel("Data/Reference File.xlsx",sheet = "Flags")
    # 
    flags=data.table(flags)
    flags
    
  })
  
  
  output$flags=renderDataTable(
    
   
    
    
    datatable(
      
      flags_reactive(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
          
          #colvis have the option to visualize columns
          list('pageLength', list(
            extend = 'excel',
            
            pageSize = 'A4',
            orientation = 'landscape',
            filename = 'Flags',
            title = "Flags"
            
          ))
        
      )),
    
    
    server = FALSE
    
    
  )
  
  suaCommodities_reactive <- reactive({
    
    
    suacommodityName = read_excel("Data/Reference File.xlsx",sheet = "SUA_Commodities") 
    # suacommodityName$CPCCode =as.double(suacommodityName$CPCCode)
    suacommodityName = data.table(suacommodityName) 
    
    
    suacommodityName=suacommodityName[order(CPCCode)]
    suacommodityName
    
  })
  
  
  output$suaCommodities = renderDataTable(
    
    datatable(
      suaCommodities_reactive(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
          
          #colvis have the option to visualize columns
          list('pageLength', list(
            extend = 'excel',
            
            pageSize = 'A4',
            orientation = 'landscape',
            filename = 'SUA Commodities',
            title = "SUA Commodities"
            
          ))
        
      )),
    
    
    server = FALSE
   
  

    
  )  
  
  
  fbsCommo_reactive <- reactive({
    
    
    fbscommodityName = read_excel("Data/Reference File.xlsx",sheet = "FBS_Commodities")
    fbscommodityName = data.table(fbscommodityName)
    
    
    fbscommodityName=fbscommodityName[order(FBSCode)]
    
    fbscommodityName
    
  })
  
  
  
  
  
  output$fbsCommodities = renderDataTable(
    
    datatable(
      fbsCommo_reactive(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
          
          #colvis have the option to visualize columns
          list('pageLength', list(
            extend = 'excel',
            
            pageSize = 'A4',
            orientation = 'landscape',
            filename = 'FBS Commodities',
            title = "FBS Commodities"
            
          ))
        
      )),
    
    
    server = FALSE
   
  )  
  
  
  
  tradeCommodities_reacitve <- reactive({
    
    tradecommodityName = read_excel("Data/Reference File.xlsx",sheet = "Trade_Commodities")
    tradecommodityName = data.table(tradecommodityName)
    
    
    commodityName = read_excel("Reference Files/Reference File.xlsx",sheet = "SUA_Commodities") 
    commodityName = data.table(commodityName)
    
    
    tradecommodityName=merge(tradecommodityName,commodityName, by=c("CPCCode","Commodity"),all.x = T)
    
    tradecommodityName=tradecommodityName[order(CPCCode)]
    # setnames(tradecommodityName,"HS6 standard code","HS6")
    
    setcolorder(tradecommodityName,c("HS6","CPCCode","Commodity"))
    
    tradecommodityName
    
    
  })
  
  
  
  
  
  
  
  
  
  output$tradeCommodities = renderDataTable( 
    
    datatable(
      tradeCommodities_reacitve(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
          
          #colvis have the option to visualize columns
          list('pageLength', list(
            extend = 'excel',
            
            pageSize = 'A4',
            orientation = 'landscape',
            filename = 'Trade Commodities',
            title = "Trade Commodities"
            
          ))
        
      )),
    
    
    server = FALSE
  )   
  
  
  
element_reactive <- reactive({
  
  elementName = read_excel("Data/Reference File.xlsx",sheet = "Elements")
  elementName = data.table(elementName)
  
  
  elementName=elementName[order(ElementCode)]
  elementName
  
  
})  
  
  
  
  
 output$elements=renderDataTable(
   
   datatable(
     element_reactive(),
     rownames = FALSE,
     extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
       pageLength = 20,fixedHeader = FALSE,
       orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
       lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
       dom = 'Bfrtip',
       buttons =
         
         #colvis have the option to visualize columns
         list('pageLength', list(
           extend = 'excel',
           
           pageSize = 'A4',
           orientation = 'landscape',
           filename = 'Element Names',
           title = "Element Names"
           
         ))
       
     )),
   
   
   server = FALSE
  
   
 ) 
 
 
 tree_reactive=reactive({
   
   
   tree = fread("SUA-FBS Balancing/Data/tree.csv")
   
   tree[, c("geographicAreaM49","flagMethod"):= NULL]
  
   setnames(tree, c("measuredElementSuaFbs", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus"),
            c("Element", "CPCCode Parent", "CPCCode Child", "Year", "Value", "Flag"))
   
   
   tree=merge(tree,commodityName, by.x ="CPCCode Parent" ,by.y = "CPCCode", all.x = TRUE)
   
   
   setnames(tree, "Commodity", "Commodity Parent")
   
 
   tree=merge(tree,commodityName, by.x = "CPCCode Child" ,by.y = "CPCCode", all.x = TRUE)
   
   
   setnames(tree,"Commodity","Commodity Child")
   
   setcolorder(tree, c("CPCCode Parent", "Commodity Parent", "CPCCode Child", "Commodity Child" , "Year", "Element", "Value", "Flag"))
   
   tree=subset(tree, Year %in% as.character(input$endyear))
 
   

   
   
   })
 
 output$tree=renderDataTable(
   

   datatable(
     tree_reactive(),
     rownames = FALSE,
     extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
       pageLength = 20,fixedHeader = FALSE,
       orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
       lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
       dom = 'Bfrtip',
       buttons =

         #colvis have the option to visualize columns
         list('pageLength', list(
           extend = 'excel',
           
           pageSize = 'A4',
           orientation = 'landscape',
           filename = 'Tree',
           title = "Commodity Tree"

         ))

     )),


  server = FALSE
 
) 
 
 
 
 
 
 
 
fbsTree_reactive=reactive({
  
  data=fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  fbsName= read_excel("Data/Reference File.xlsx", "FBS_Commodities")
  
  setDT(fbsName)
  
  
  data=merge(data,fbsName, by.x = "id1",by.y = "FBSCode",all.x = T)
  
  setnames(data,"Commodity","Grand Total")
  
  data=merge(data,fbsName, by.x = "id2",by.y = "FBSCode",all.x = T)
  
  setnames(data,"Commodity","FBS Catergory")
  
  data=merge(data,fbsName, by.x = "id3",by.y = "FBSCode",all.x = T)
  
  setnames(data,"Commodity","FBS Macro Aggregate")
  
  data=merge(data,fbsName, by.x = "id4",by.y = "FBSCode",all.x = T)
  
  setnames(data,"Commodity","FBS Group")
  
  setnames(data,c("id1","id2","id3","id4","item_sua_fbs"),c("Grand Total Code","FBS Catergory Code","FBS Macro Aggrgate Code","FBS Group Code","CPCCode"))
 
  data=merge(data,commodityName,by="CPCCode",all.x = TRUE)
  
  
  setcolorder(data,c("Grand Total Code","Grand Total",
                     "FBS Catergory Code","FBS Catergory","FBS Macro Aggrgate Code","FBS Macro Aggregate",
                     "FBS Group Code","FBS Group","CPCCode","Commodity"))
  
  data
  
  })
 
 
 output$fbsTree=renderDataTable(
   
   
   datatable(
     fbsTree_reactive(),
     rownames = FALSE,
     extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
       pageLength = 20,fixedHeader = FALSE,
       orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
       lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
       dom = 'Bfrtip',
       buttons =
         
         #colvis have the option to visualize columns
         list('pageLength', list(
           extend = 'excel',
           
           pageSize = 'A4',
           orientation = 'landscape',
           filename = 'Tree',
           title = "Commodity Tree"
           
         ))
       
     )),
   
   
   server = FALSE
   # 
   
 ) 
   
 
 
 commodityTree = reactive({
   
   data=fread("SUA-FBS Balancing/Data/tree.csv")
   
   
   t=as.character(input$endyear)
   
   
   data=subset(data, timePointYears == t )
   
   
   
   
   tree=copy(data)
   
   tree[, c("geographicAreaM49","flagMethod") := NULL]
   
   
   
   tree=subset(tree, measuredElementSuaFbs %in% c("extractionRate") & timePointYears %in% as.character(input$endyear))
   
   
   tree[, c("measuredElementSuaFbs", "timePointYears") := NULL ]
   
   setnames(tree, c("measuredItemParentCPC", "measuredItemChildCPC", "Value", "flagObservationStatus"), c("CPCCode Parent","CPCCode Child",
                                                                                                          "Extraction Rate","Flag"))
   
   tree=merge(tree,commodityName,by.x = "CPCCode Parent" , by.y = "CPCCode", all.x = TRUE)
   
   setnames(tree,"Commodity","Parent Commodity")
   
   tree=merge(tree,commodityName,by.x = "CPCCode Child" , by.y = "CPCCode", all.x = TRUE)
   
   setnames(tree,"Commodity","Child Commodity")
   
   
   setcolorder(tree,c("CPCCode Parent","Parent Commodity","CPCCode Child","Child Commodity","Extraction Rate","Flag"))
   
   # tree[, `CPCCode Parent` := as.factor(`CPCCode Parent`)]
   # tree[, `Parent Commodity` := as.factor(`Parent Commodity`)]
   # tree[, `CPCCode Child` := as.factor(`CPCCode Child`)]
   # tree[, `Child Commodity` := as.factor(`Child Commodity`)]
   # 
   
   
   # setattr(tree[["CPCCode Parent"]],"levels",unique(commodityName$CPCCode))
   # setattr(tree[["CPCCode Child"]],"levels",unique(commodityName$CPCCode))
   # setattr(tree[["Parent Commodity"]],"levels",unique(commodityName$Commodity))
   # setattr(tree[["Child Commodity"]],"levels",unique(commodityName$Commodity))
   
   tree[is.na(`Extraction Rate`),`Extraction Rate`:=0]
   
   tree=tree[order(`CPCCode Parent`)]
   tree
   
 })  
 
 
 output$commoditytree=renderRHandsontable({
   
   # df_tree<- reactiveValues(data_commodityTree=NULL)
   
   
   if (is.null(df_tree$data_commodityTree)){  
     
     
     df_tree$data_commodityTree= commodityTree()
     
   }
   rhandsontable(df_tree$data_commodityTree,columnSorting = TRUE, height = 450)%>%
     
     
     
     # rhandsontable(df_tree$data_commodityTree, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T,
     #               selectCallback = TRUE, fontweight= "bold",search= TRUE)%>%
     hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>%
     
     # hot_col("CPCCode Parent", halign = "htRight", width = 80)%>%
     # 
     # hot_col("CPCCode Child", halign = "htRight", width = 80)%>%
     # 
     # 
     # 
     hot_col("Child Commodity", width = 400,readOnly = TRUE)%>%
     hot_col("Parent Commodity",  width = 400,readOnly = TRUE)%>%
     hot_col("CPCCode Parent",allowInvalid = TRUE,readOnly = TRUE)%>%
     # hot_col("Parent Commodity", allowInvalid = TRUE,readOnly = TRUE)%>%
     hot_col("CPCCode Child", allowInvalid = TRUE,readOnly = TRUE)
   # hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
   # hot_col("Child Commodity", allowInvalid = TRUE)
   # 
   
 })
 
 
 observeEvent(input$treeSave,{
   
   
   tree= data.table(hot_to_r(input$commoditytree))
   
   # write.csv(tree,"xx.csv",row.names = F)
   

   
   # # print(tree)
   # write.csv(tree,"treeFromTool.csv", row.names = FALSE)
   # # 
   # # 
   # tree=fread("treeFromTool.csv")
   # # 
   
   
   tree[, c("Parent Commodity", "Child Commodity") := NULL]
   
   
   originalTree=fread("SUA-FBS Balancing/Data/tree.csv")
   
   
   countryCode= unique(originalTree$geographicAreaM49)
   
   originalTree2Bind = subset(originalTree,!timePointYears %in% as.character(input$endyear)) ###rbind
   
   
   originalTree2Save =subset(originalTree,timePointYears %in% as.character(input$endyear))
   
   originalTree2Bind2=subset(originalTree2Save, !measuredElementSuaFbs %in% "extractionRate") ##rbind  #share
   
   
   originalTree2Save=subset(originalTree2Save, measuredElementSuaFbs %in% "extractionRate")
   
   ttt=merge(tree,originalTree2Save, by.x =c("CPCCode Parent",  "CPCCode Child") ,by.y =c("measuredItemParentCPC", "measuredItemChildCPC") , all.x = TRUE)
   
   
   ttt[, c("Value","flagObservationStatus") := NULL]
   
   
   setnames(ttt, c("Extraction Rate", "Flag"),c("Value","flagObservationStatus"))
   setnames(ttt, c("CPCCode Parent","CPCCode Child"),c("measuredItemParentCPC","measuredItemChildCPC"))
   
   
   
   
   ttt[, flagMethod := ifelse(is.na(flagMethod),"t" ,flagMethod)]
   ttt[, geographicAreaM49 := ifelse(is.na(geographicAreaM49),countryCode ,geographicAreaM49)]
   ttt[, measuredElementSuaFbs := ifelse(is.na(measuredElementSuaFbs),"extractionRate" ,measuredElementSuaFbs)]
   ttt[, timePointYears := ifelse(is.na(timePointYears),as.character(input$endyear) ,timePointYears)]
   
   
   
   
   finalTree=rbind(originalTree2Bind,originalTree2Bind2,ttt)
   
   
   finalTree[measuredElementSuaFbs == "extractionRate" & Value == 0, Value :=NA]
   
   write.csv(finalTree,"SUA-FBS Balancing/Data/tree.csv" , row.names = FALSE)
   
   
 })
   
   
   
 
 nutrient_data= reactive({
   
   data=fread("SUA-FBS Balancing/Data/nutrientData.csv")
   
   t=as.character(input$endyear)
   
   
   data=subset(data, timePointYearsSP %in% t)
   print(t)
   
   data[,c("timePointYearsSP","flagRatio", "geographicAreaM49"):=NULL]
   
   data1001=dcast.data.table(data[measuredElement %in% 1001], measuredItemCPC ~ measuredElement, value.var = c("Value"))
   # data1003=dcast.data.table(data[measuredElement %in% 1003], measuredItemCPC ~ measuredElement, value.var = c("Value"))
   # data1005=dcast.data.table(data[measuredElement %in% 1005], measuredItemCPC ~ measuredElement, value.var = c("Value"))
   
   
   # data=merge(data1001, data1003, by="measuredItemCPC", all = TRUE)
   
   # data=merge(data, data1005, by="measuredItemCPC", all = TRUE)
   
   setnames(data,c("measuredItemCPC","Value"),c("CPCCode","Calories [/100g]"))
   
   data=merge(data,commodityName,by="CPCCode", all.x = TRUE)
   
   data[, measuredElement := NULL]
   
   setcolorder(data,c("CPCCode","Commodity","Calories [/100g]"))
   
   data
   # write.csv(data,"nutr.csv",row.names = FALSE)
   
   
 })
 
 
 observeEvent(input$nutrientSave,{
   
   
   data=data.table(hot_to_r(input$nutrientFactors))
   t=as.character(input$endyear)
   
  
   
   
   nutrient_database=fread("SUA-FBS Balancing/Data/nutrientData.csv")
   
   nutrient_database=subset(nutrient_database, !(timePointYearsSP %in% t)) # the endyear is dropped
   
   data[, Commodity := NULL]
   setnames(data,c("Calories [/100g]"),c("1001"))
   
   dataCalories=melt(data[,c("CPCCode","1001")],id.vars ="CPCCode")
   
   # dataProtein=melt(data[,c("CPCCode","1003")],id.vars ="CPCCode")
   # 
   # dataFats=melt(data[,c("CPCCode","1005")],id.vars ="CPCCode")
   
   
   
   data=rbind(dataCalories)
   
   data[, geographicAreaM49 := unique(nutrient_database$geographicAreaM49)]
   
   data[, timePointYearsSP := as.character(t)]
   
   data[,flagRatio := "N"]
   
   setnames(data,c("CPCCode","variable","value"),c("measuredItemCPC","measuredElement","Value"))
   
   
   data=rbind(nutrient_database,data)
   
   data[, measuredElement := as.character(measuredElement)]
   
   write.csv(data,"SUA-FBS Balancing/Data/nutrientData.csv",row.names = FALSE)
   
   
   
 })
 
 
 
 
 output$nutrientFactors =renderRHandsontable({
   
   
   df_nutrient$data_nutrient =nutrient_data()
   
   rhandsontable( df_nutrient$data_nutrient)
   
   
   rhandsontable(df_nutrient$data_nutrient,undo = T, redo = T, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,
                 selectCallback = TRUE, fontweight= "bold",search=TRUE) %>%
     hot_col("Commodity", width=380)   %>%
     hot_col("CPCCode", halign = "htRight", width= 100)%>%
     hot_table(highlightCol = TRUE, highlightRow = TRUE)
   
 })   
   
   
   

 output$downloadTree <- downloadHandler(
   
   
   filename = function() {
     
     "table.xlsx"
   },
   
   
   content = function(file) {
     
     
     
     write.xlsx(data.table(hot_to_r(input$commoditytree)) ,file,row.names = FALSE)
   }
   
   
 )
 
 
 output$downloadNutrientFac <- downloadHandler(
   
   
   filename = function() {
     
     "table.xlsx"
   },
   
   
   content = function(file) {
     
     
     
     write.xlsx(data.table(hot_to_r(input$nutrientFactors)) ,file,row.names = FALSE)
   }
   
   
 )
 
 

  
  
}