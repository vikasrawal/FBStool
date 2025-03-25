compilesuafbs=function(input,output,session){
  
  
  
  values_1 <- reactiveValues(sua_unbalanced = NULL)
  values_2 <- reactiveValues(sua_balanced = NULL)
  values_3 <- reactiveValues(fbs_stand = NULL)
  values_4<- reactiveValues(fbs = NULL)
  values_5<- reactiveValues(des = NULL)
  
  values_6<- reactiveValues(desCommodity = NULL)
  
  
  values_7<- reactiveValues(desCommodityYear = NULL)
  values_8<- reactiveValues(desTimeseries = NULL)
  
  
  values_parent <-reactiveValues(parentCommodity = NULL)
  # values_child <-reactiveValues(childCommodity = NULL)
  
  
 
  
  
  
  
  observeEvent(input$addPrimaryChild, {
    showModal(viewParentChildModal())
  })  
  
  
  
  
  viewParentChildModal <- function(failed = FALSE) {
    
    
    modalDialog(
      
      easyClose = TRUE, size= "l",
      dataTableOutput("viewParent")
      # dataTableOutput("viewChild")
      ,
      
      footer = tagList(
        
        actionButton("parentchildInsert", "Insert")
      )
    )
    
  }
  
  
  
  
  
  observeEvent(input$parentchildInsert, {
    
    removeModal()
    
  })
  
  
  
  
  
  output$viewParent= renderDataTable({
    
    
    
    
     if (is.null(values_parent$parentCommodity)){
       
       tree=fread("SUA-FBS Balancing/Data/tree.csv")
       
       tree <- tree[,c("measuredItemParentCPC","measuredItemChildCPC")]
       
       tree <- unique(tree)
       
       setnames(tree,c("measuredItemParentCPC","measuredItemChildCPC"),c("CPCCode Parent","CPCCode Child"))
       
       tree <- merge(tree,commodityName, by.x="CPCCode Parent",by.y = "CPCCode", all.x = TRUE)
       
       setnames(tree,"Commodity","Parent Commodity")
       
       tree <- merge(tree,commodityName, by.x="CPCCode Child",by.y = "CPCCode", all.x = TRUE)
       
       setnames(tree,"Commodity","Child Commodity")
       
       setcolorder(tree, c("CPCCode Parent","Parent Commodity","CPCCode Child", "Child Commodity"))
       
       DT <- copy(tree)
       
       DT <- DT[order(`CPCCode Parent`)]
       
       DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
       
       values_parent$parentCommodity <- DT
       
       
       write.csv(data.table(input$commoditytree),"tree_test.csv",row.names = FALSE)
       
       
      #  # tree_active <- data.table(input$commoditytree)
      #  
      #  tree_active <- fread("tree_test.csv")
      #  
      #  
      # #  if (input$addPrimaryChild >0 ){
      #   
      #    pair_list = vector(mode = "list", length = nrow(unique(DT[,c("CPCCode Parent","CPCCode Child")])))
      # 
      # 
      #    for (j in seq_len(nrow(unique(tree_active[,c("CPCCode Parent","CPCCode Child")])))){
      # 
      # 
      #      pair_list[[j]] <- as.character(tree_active[j,c("CPCCode Parent","CPCCode Child")])
      # 
      # 
      #    }
      #    
      #    
      #    
      #   vv= lapply(pair_list, FUN=function(x){any(DT$`CPCCode Parent`==x[[1]] & DT$`CPCCode Child`==x[[2]])})       
      #    
      #    
      #    
      #    
         
      #    
      #    
      #    
      #    
      #    
      #    values_parent$parentCommodity<- subset( values_parent$parentCommodity,
      # 
      # !`CPCCode Parent` %in% unique(isolate(hot_to_r(input$commoditytree))$`CPCCode Parent`)
      # &
      #   !`CPCCode Child` %in% unique(isolate(hot_to_r(input$commoditytree))$`CPCCode Child`)
      # 
      # 
      # 
      # )
      # #    
      # #    
      # #    
      # #    
      #  }
      #  
       }
      # 
    
    
 datatable(values_parent$parentCommodity,
              escape=F)
   
   
    
  })  
  
   
  
  
#   output$viewChild= renderDataTable({
#     
#     if (is.null(values_child$childCommodity)){
#       
#       
#       
#       tree=fread("SUA-FBS Balancing/Data/tree.csv")
#       
#       parents= unique(tree[,measuredItemParentCPC ])
#       
#       
#       
#       child=read_excel("Data/Reference File.xlsx", "SUA_Commodities")
#       setDT(child)
#       
#       child=child[!(CPCCode %in% parents)]
#       
#       
#       child=unique(child[,CPCCode])
#       
#       
#       
#       child=data.table(child)
#       
#       setnames(child,"child", "CPCCode Child")
#       
#       
#       child=merge(child,commodityName,by.x ="CPCCode Child" ,by.y ="CPCCode" ,all.x = TRUE)
#       
#       setnames(child, "Commodity", "Child Commodity")
#       
#       
#       
#       DT=copy(child)
#       
#     
#       DT[["Select Child Commodity"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
#    
#       values_child$childCommodity <- DT
#       
#       write.csv(values_child$childCommodity, "child.csv", row.names = FALSE)
# }
#     
#     
# datatable(values_child$childCommodity,
#               escape=F)
#     
#     
#     
# })  
#   
#   
  
  
  
  
  
  
 




output$sua_unbalance_title=renderText({
  
  
  paste("SUA Unbalanced", input$endyear, "of", countrycodes$Country)
  
})






output$SUA_Balanced=renderText({
  
  
  paste("SUA Balanced", input$endyear, "of", countrycodes$Country)
  
})





output$Primary_Equivalent_Unbalanced_title=renderText({
  
  
  paste("Primary Equilvalent Unbalanced", input$endyear, "of", countrycodes$Country)
  
})







output$fbs_title=renderText({
  
  
  paste("Food Balance Sheet (FBS)", input$endyear, "of", countrycodes$Country)
  
})







addRowTree=reactive({
  
  
  if (input$parentchildInsert > 0)  {
    s1=as.numeric(input$viewParent_rows_selected)
    
    
  
    
    if (length(s1) == 0){

      data=hot_to_r(input$commoditytree)
      
    } else {
       
      s1=as.numeric(input$viewParent_rows_selected)
      # s1=c(2,3)
      
      
      
      
      parentRow <- values_parent$parentCommodity[s1,][,"Select" :=NULL]
      
      
      
      # parentRow <- fread("parent.csv")[s1,][,"Select Parent Commodity" :=NULL]
      # childRow <- fread("child.csv")[s2,][,"Select Child Commodity" :=NULL]
     
      parentCPC <- parentRow$`CPCCode Parent`
      childCPC <- parentRow$`CPCCode Child`
      
      
      table= expand.grid(parentCPC,childCPC)
      setDT(table)
      
      setnames(table, c("Var1","Var2"),c("CPCCode Parent","CPCCode Child"))
      
      table<-merge(table,commodityName, by.x = "CPCCode Parent", by.y = "CPCCode", all.x = TRUE)
      
      setnames(table, "Commodity","Parent Commodity")
      
  
      table<-merge(table,commodityName, by.x = "CPCCode Child", by.y = "CPCCode", all.x = TRUE)
      
      setnames(table, "Commodity","Child Commodity")
      
      setcolorder(table, c("CPCCode Parent", "Parent Commodity", "CPCCode Child", "Child Commodity"))
      
 
      
      data=isolate(hot_to_r(input$commoditytree))
      data=data.table(data)
     
      data=rbind(table,data,fill=T)
    
    }
   
  }

  else {

    data= hot_to_r(input$commoditytree)

  }



 
data


  
  
})





observeEvent(input$parentchildInsert,{
  
  
  
  df_tree$data_commodityTree=addRowTree()
  
  
})





  




  
   
  

suaUnbalancedReactive=eventReactive(input$startStandardization, {

  
    t= as.character(input$endyear)

    data=get(load("Data/countrySUA.RData"))
    
    data=data[Year %in% t]
    
    
    # elemKeys=c("5510", "5610", "5071",, "5023", "5910", "5016", "5165", "5520","5525","5164","5166","5141")
    
    elemKeys=c("5510", "5610", "5071","5113", "5023", "5910", "5016", "5165", "5520","5525","5166","5141")
    
    data=data[ElementCode %in% elemKeys]
    
    
    
    
    # if (length(setdiff(elemKeys,unique(data$ElementCode)))){
      
    #   elementMissingCode= setdiff(elemKeys,unique(data$ElementCode))
    #   
    #   
    #   # elementNameMissing= subset(elementName, ElementCode %in% elementMissingCode)$Element
    #   
    #   ifesle (!(elementMissingCode %in% elemKeys)){
    #     
    #     print('POPUP: la colonna non esiste')
    #     
    #   } 
    #    
    #    
    # 
    #   
    # # }
    # 
    
    data[, c("CountryM49","Country","Flag", "Year", "ElementCode") := NULL]
    
    data=dcast.data.table(data, CPCCode + Commodity  ~ Element, value.var = "Value")
    
    
    data[,`Production [t]`:= as.numeric(`Production [t]`)]

    data[,`Import Quantity [t]` := as.numeric(`Import Quantity [t]`)]

    data[,`Stock Variation [t]`:= as.numeric(`Stock Variation [t]`)]


    data[,`Feed [t]`:= as.numeric(`Feed [t]`)]
    data[,`Food [t]`:= as.numeric(`Food [t]`)]
    data[,`Industrial uses [t]`:= as.numeric(`Industrial uses [t]`)]
    data[,`Loss [t]`:= as.numeric(`Loss [t]`)]
    data[,`Seed [t]`:= as.numeric(`Seed [t]`)]
    # data[,`Tourist consumption [t]`:= as.numeric(`Tourist consumption [t]`)]
    data[,`Export Quantity [t]`:= as.numeric(`Export Quantity [t]`)]
    # data[,`Residual other uses [t]`:= as.numeric(`Residual other uses [t]`)]

    

   data[ , `supply`:= rowSums(.SD, na.rm=T), .SDcols=c("Production [t]","Import Quantity [t]" )]




    # data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
    #                                                          ,"Industrial uses [t]","Loss [t]","Seed [t]","Tourist consumption [t]","Export Quantity [t]","Residual other uses [t]")]
   
   
   
   #removed tourist
   data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
   ,"Industrial uses [t]","Loss [t]","Seed [t]","Export Quantity [t]")]

    data[, `Imbalance [t]` := supply - utilization]

    data[, c("supply","utilization"):=NULL]

    
    
    
    data[, `Imbalance [t]` := round(`Imbalance [t]`,0)]

    # setcolorder(data,c("CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Stock Variation [t]",
    #                    "Food [t]","Tourist consumption [t]", "Loss [t]", "Feed [t]", "Seed [t]", "Industrial uses [t]", "Residual other uses [t]","Imbalance [t]"))


    
    #removed tourist
    # setcolorder(data,c("CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Stock Variation [t]",
    #                    "Food [t]", "Loss [t]", "Feed [t]", "Seed [t]", "Industrial uses [t]", "Residual other uses [t]","Imbalance [t]"))
    
    setcolorder(data,c("CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]","Opening Stocks [t]", "Stock Variation [t]",
                   "Food [t]", "Loss [t]", "Feed [t]", "Seed [t]", "Industrial uses [t]", "Imbalance [t]"))

    

    fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")

  
    fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
    
    setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
    
    fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
    
    fbsTree=merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)

    fbsTree[, fbsCode_S := NULL]
    
    setnames(fbsTree,"Commodity","FBS Commodity")
    
    # fbsTree <- fbsTree[,c("CPCCode" ,"FBSCode Lev4" ,"FBS Commodity Lev4")]

    # setnames(fbsTree, c("FBSCode Lev4", "FBS Commodity Lev4"),c("FBSCode","FBS Commodity"))

    data=merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)

    # setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" ,
    #   "Tourist consumption [t]","Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Residual other uses [t]","Imbalance [t]" ))

    
    #removed tourist 
    setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]","Opening Stocks [t]","Stock Variation [t]" ,"Food [t]" 
                        ,"Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Imbalance [t]"))
    
    
    
    
    data[,5:15]=round(data[,5:15],0)
    
    # data[is.na(data)] <- ""
    
    
    values_1$sua_unbalanced <- data
    
    
    
    numericcol= names(data[,sapply(data,is.numeric),with=FALSE])

    datatable(data) %>%
      
      formatCurrency(numericcol,currency = "", digits = 0,interval = 3, mark = ",")%>%
      formatStyle(columns = 1:2, color = "grey")%>%
      formatStyle(columns = 3:4, color = "blue")  
    
   
   
    
    
})



suaUnbalancedReactive_G=reactive({
  
  
  t= as.character(input$endyear)
  
  data=get(load("Data/countrySUA.RData"))
  
  data=data[Year %in% t]
  
  
  # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5166","5141")
  
  elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5166","5141")
  
  data=data[ElementCode %in% elemKeys]
  
  
  
  
  # if (length(setdiff(elemKeys,unique(data$ElementCode)))){
  
  #   elementMissingCode= setdiff(elemKeys,unique(data$ElementCode))
  #   
  #   
  #   # elementNameMissing= subset(elementName, ElementCode %in% elementMissingCode)$Element
  #   
  #   ifesle (!(elementMissingCode %in% elemKeys)){
  #     
  #     print('POPUP: la colonna non esiste')
  #     
  #   } 
  #    
  #    
  # 
  #   
  # # }
  # 
  
  data[, c("CountryM49","Country","Flag", "Year", "ElementCode") := NULL]
  
  data=dcast.data.table(data, CPCCode + Commodity  ~ Element, value.var = "Value")
  
  # data[, `Processed [t]` := NULL]
  
  
 
  
  
  data[,`Production [t]`:= as.numeric(`Production [t]`)]
  
  data[,`Import Quantity [t]` := as.numeric(`Import Quantity [t]`)]
  
  data[,`Stock Variation [t]`:= as.numeric(`Stock Variation [t]`)]
  
  
  data[,`Feed [t]`:= as.numeric(`Feed [t]`)]
  data[,`Food [t]`:= as.numeric(`Food [t]`)]
  data[,`Industrial uses [t]`:= as.numeric(`Industrial uses [t]`)]
  data[,`Loss [t]`:= as.numeric(`Loss [t]`)]
  data[,`Seed [t]`:= as.numeric(`Seed [t]`)]
  # data[,`Tourist consumption [t]`:= as.numeric(`Tourist consumption [t]`)]
  data[,`Export Quantity [t]`:= as.numeric(`Export Quantity [t]`)]
  data[,`Residual other uses [t]`:= as.numeric(`Residual other uses [t]`)]
  
  
  
  data[ , `supply`:= rowSums(.SD, na.rm=T), .SDcols=c("Production [t]","Import Quantity [t]" )]
  
  
  
  
  # data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
  #                                                          ,"Industrial uses [t]","Loss [t]","Seed [t]","Tourist consumption [t]","Export Quantity [t]","Residual other uses [t]")]
  
  
  
  #removed tourist
  data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
                                                            ,"Industrial uses [t]","Loss [t]","Seed [t]","Export Quantity [t]","Residual other uses [t]")]
  
  data[, `Imbalance [t]` := supply - utilization]
  
  data[, c("supply","utilization"):=NULL]
  
  
  
  
  data[, `Imbalance [t]` := round(`Imbalance [t]`,0)]
  
  # setcolorder(data,c("CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Stock Variation [t]",
  #                    "Food [t]","Tourist consumption [t]", "Loss [t]", "Feed [t]", "Seed [t]", "Industrial uses [t]", "Residual other uses [t]","Imbalance [t]"))
  
  
  
  #removed tourist
  setcolorder(data,c("CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Stock Variation [t]",
                     "Food [t]", "Loss [t]", "Feed [t]", "Seed [t]", "Industrial uses [t]", "Residual other uses [t]","Imbalance [t]"))
  
  
  fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  
  fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
  
  setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
  
  fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
  
  fbsTree=merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)
  
  fbsTree[, fbsCode_S := NULL]
  
  setnames(fbsTree,"Commodity","FBS Commodity")
  
  # fbsTree <- fbsTree[,c("CPCCode" ,"FBSCode Lev4" ,"FBS Commodity Lev4")]
  
  # setnames(fbsTree, c("FBSCode Lev4", "FBS Commodity Lev4"),c("FBSCode","FBS Commodity"))
  
  data=merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)
  
  # setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" ,
  #   "Tourist consumption [t]","Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Residual other uses [t]","Imbalance [t]" ))
  
  
  #removed tourist 
  setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" 
                      ,"Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Residual other uses [t]","Imbalance [t]" ))
  
  
  
  
  data[,5:15]=round(data[,5:15],0)
  
  data[is.na(data)] <- ""
  
  
  values_1$sua_unbalanced <- data
  
  
  
  numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
  
  datatable(data) %>%
    
    formatCurrency(numericcol,currency = "", digits = 0,interval = 3, mark = ",")%>%
    formatStyle(columns = 1:2, color = "grey")%>%
    formatStyle(columns = 3:4, color = "blue")  
  
  
  
  
  
})





suaProcessedReactive=eventReactive(input$balancedProcessed, {

  t=as.character(input$endyear)
  
  files_sua_balanced=list.files(path = "SUA-FBS Balancing/Data",pattern ="^sua_balanced_")
  
  
  if (paste0("sua_balanced_",t,".csv") %in% files_sua_balanced){
  
  data=fread(paste0("SUA-FBS Balancing/Data/sua_balanced_",t,".csv"))
  
  data <- subset(data, timePointYears %in% t)
  
  #residual given by country will not appear in sua_balanced
  
  # data=subset(data, !measuredElementSuaFbs %in% 5166)
  
  
  #These data does not consist tourist if the year is before or equal to 2013
  
  data[, geographicAreaM49 := NULL]
  
  setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
           
           c("ElementCode","CPCCode","Year","Value","Flag"))
  
  
  
  data=merge(data, commodityName, by= "CPCCode", all.x = TRUE)
  
  data=merge(data,elementName, by="ElementCode", all.x = TRUE)
  
  # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
  #tourist remove
  
  
  elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5141","664", "5166","5113")
  
  data=subset(data, ElementCode %in% elemKeys)
  
  
  data[,c("ElementCode", "Year","Flag" ) :=NULL]
  
  
  data=dcast.data.table(data, CPCCode + Commodity   ~ Element, value.var = "Value")
  
  
  if (!'Processed [t]' %in% names(data)) {
    
    
    data[, `Processed [t]` := NA_real_]
    
  }
  
  
  if (!'Opening Stocks [t]' %in% names(data)) {
    
    
    data[, `Opening Stocks [t]` := NA_real_]
    
  }
  
  if (!'Stock Variation [t]' %in% names(data)) {
    
    
    data[, `Stock Variation [t]` := NA_real_]
    
  }
  
  if (!'Industrial uses [t]' %in% names(data)) {
    
    
    data[, `Industrial uses [t]` := NA_real_]
    
  }
  
  
  if (!'Seed [t]' %in% names(data)) {
    
    
    data[, `Seed [t]` := NA_real_]
    
  }
  
  if (!'Feed [t]' %in% names(data)) {
    
    
    data[, `Feed [t]` := NA_real_]
    
  }
  
  
  if (!'Loss [t]' %in% names(data)) {
    
    
    data[, `Loss [t]` := NA_real_]
    
  }
  
  
  
  #These data does not consist tourist if the year is before or equal to 2013
  
  
  # if (!("Tourist consumption [t]" %in% colnames(data))){
  #   
  #   data[, `Tourist consumption [t]` := NA_real_]
  #   
  # }
  
  
  data[,`Export Quantity [t]`:=as.numeric(`Export Quantity [t]`)]
  
  
  
  # data[ , `supply`:= rowSums(.SD, na.rm=T), .SDcols=c("Production [t]","Import Quantity [t]" )]
  
  # data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
  #                                                           ,"Industrial uses [t]","Loss [t]","Seed [t]","Tourist consumption [t]","Export Quantity [t]",
  #                                                           "Processed [t]")]
  
  
  # data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
  # ,"Industrial uses [t]","Loss [t]","Seed [t]","Export Quantity [t]",
  # "Processed [t]")]
  
  # data[, `Imbalance [t]` := supply - utilization]
  # 
  # data[, `Imbalance [t]` := round(`Imbalance [t]`,0)]
  # 
  # 
  # data[, c("supply","utilization"):=NULL]
  
  
  
  
  # setcolorder(data,c("CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Stock Variation [t]",
  #                    "Food [t]","Processed [t]","Tourist consumption [t]", "Loss [t]", "Feed [t]", "Seed [t]",
  #                    "Industrial uses [t]","Imbalance [t]"))
  
  setcolorder(data,c("CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Opening Stocks [t]","Stock Variation [t]",
                     "Food [t]","Processed [t]", "Loss [t]", "Feed [t]", "Seed [t]",
                     "Industrial uses [t]","Residual other uses [t]","Food supply (/capita/day) [kcal]"))
  
  
  
  fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  
  fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
  
  setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
  
  fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
  
  fbsTree=merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)
  
  fbsTree[, fbsCode_S := NULL]
  
  setnames(fbsTree,"Commodity","FBS Commodity")
  
  # fbsTree <- fbsTree[,c("CPCCode" ,"FBSCode Lev4" ,"FBS Commodity Lev4")]
  
  # setnames(fbsTree, c("FBSCode Lev4", "FBS Commodity Lev4"),c("FBSCode","FBS Commodity"))
  
  data=merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)
  
  
  
  
  # 
  # setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" ,"Processed [t]",
  #                     "Tourist consumption [t]","Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Imbalance [t]" ))
  # 
  # 
  
  
  #removed tourist
  setcolorder(data, c("FBS Code","FBS Commodity","CPCCode","Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Opening Stocks [t]","Stock Variation [t]",
                      "Food [t]","Processed [t]", "Loss [t]", "Feed [t]", "Seed [t]",
                      "Industrial uses [t]","Residual other uses [t]","Food supply (/capita/day) [kcal]"))
  
  data[,5:17]=round(data[,5:17],0)
  
  numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
  
  # data[is.na(data)] <- ""
  values_2$sua_balanced <- data
  
  datatable(data) %>%
    
    formatCurrency(numericcol,currency = "", digits = 0,interval = 3, mark = ",") %>%
    formatStyle(columns = 1:2, color = "grey")%>%
    formatStyle(columns = 3:4, color = "blue") 
  
  }else {
    
    sendSweetAlert(
      session = session,
      title = paste("Please Run the Balancing Plugin to create SUA Balanced of", t),
      # text = paste(elementNameMissing, collapse = ' , '),
      type = "warning"
    )
    
    
    
  }
 
})



# primaryEquivalentReactive=eventReactive(input$standardizeAggrigate, {
# 
# 
#   data=fread("SUA-FBS Balancing/StandardizedData/_03_AfterST_BeforeFBSbal.csv")
# 
#   
#   data[, geographicAreaM49 := NULL]
#   
#   setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
#            
#            c("ElementCode","CPCCode","Year","Value","Flag"))
#   
#   
#   
#   data=merge(data, commodityName, by= "CPCCode", all.x = TRUE)
#   
#   data=merge(data,elementName, by="ElementCode", all.x = TRUE)
#   
#   # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141", "5166", "261","271","281")
#   
#   #tourist
#   elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5141", "5166", "261","271","281")
#   
#   data=subset(data, ElementCode %in% elemKeys)
#   
#   
#   
#   
#   
#   data[,c("ElementCode", "Year","Flag" ) :=NULL]
#   
#   data=dcast.data.table(data, CPCCode + Commodity   ~ Element, value.var = "Value")
#   
# 
# 
#  
#   data[ , `supply`:= rowSums(.SD, na.rm=T), .SDcols=c("Production [t]","Import Quantity [t]" )]
# 
# 
# 
#   # data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
#   #                                                           ,"Industrial uses [t]","Loss [t]","Seed [t]","Tourist consumption [t]","Export Quantity [t]",
#   #                                                           "Processed [t]", "Residual other uses [t]")]
# 
#   
#   #tourist
#   data[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
#                                                             ,"Industrial uses [t]","Loss [t]","Seed [t]","Export Quantity [t]",
#                                                             "Processed [t]", "Residual other uses [t]")]
#   
#   
#   
#   data[, `Imbalance [t]` := supply - utilization]
# 
#   data[, `Imbalance [t]` := round(`Imbalance [t]`,0)]
# 
# 
#   data[, c("supply","utilization"):=NULL]
#   
# 
#   
#   fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
#   
#   
#   fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
#   
#   setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
#   
#   fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
#   
#   fbsTree=merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)
#   
#   fbsTree[, fbsCode_S := NULL]
#   
#   setnames(fbsTree,"Commodity","FBS Commodity")
#   
#   # fbsTree <- fbsTree[,c("CPCCode" ,"FBSCode Lev4" ,"FBS Commodity Lev4")]
#   
#   # setnames(fbsTree, c("FBSCode Lev4", "FBS Commodity Lev4"),c("FBSCode","FBS Commodity"))
#   
#   data=merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)
#   
#   
#   
#   
#   # setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" ,"Processed [t]",
#   #                     "Tourist consumption [t]","Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Residual other uses [t]","Imbalance [t]",
#   #                     "Calories/Year [Mln kcal]" ,"Fats/Year [t]","Proteins/Year [t]"))
#   # 
#   
#   #tourist
#   setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" ,"Processed [t]",
#                       "Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Residual other uses [t]","Imbalance [t]",
#                       "Calories/Year [Mln kcal]" ,"Fats/Year [t]","Proteins/Year [t]"))
#   
#   
#   data[,5:19]=round(data[,5:19],0)
#   
#   numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
#   
#   # data[is.na(data)] <- ""
#   
#  values_3$fbs_stand <- data
#   
#   datatable(data) %>%
#     
#     formatCurrency(numericcol,currency = "", digits = 0,interval = 3, mark = ",")%>%
#     formatStyle(columns = 1:2, color = "grey")%>%
#     formatStyle(columns = 3:4, color = "blue") 
# })





# fbsReactive=eventReactive(input$balanceFBS, {
# 
# 
#   data=fread("SUA-FBS Balancing/StandardizedData/FBS_balanced.csv")
#   
#   
#   data[,c("geographicAreaM49", "timePointYears","flagObservationStatus","flagMethod") := NULL]
#   
#   setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "Value", "imbalance"),
#            
#            c("ElementCode","FBS Code","Value","Imbalance [t]"))
#   
#   
# 
#  
# 
#  data=merge(data, commodityName, by.x = "FBS Code",by.y = "CPCCode", all.x = TRUE)
#  
#  data=merge(data,elementName, by="ElementCode", all.x = TRUE)
#  
#  data[, ElementCode := NULL]
#  
#  data=dcast.data.table(data, `FBS Code` + Commodity +`Imbalance [t]`  ~ Element, value.var = "Value")
# 
#  
#  #tourist 
#  
#  data[, `Tourist consumption [t]` := NULL]
# 
#   DESvalue= fread("SUA-FBS Balancing/StandardizedData/caloryData.csv")
# 
#   DESvalue[,c("geographicAreaM49", "timePointYears","flagObservationStatus","flagMethod") := NULL]
# 
#   setnames(DESvalue,c("measuredElementSuaFbs", "measuredItemFbsSua", "Value"),
# 
#            c("ElementCode","FBS Code","Value"))
# 
# 
#  DESvalue=subset(DESvalue, ElementCode %in% as.character(664))
#  
#  DESvalue[, ElementCode := NULL]
#  
#  setnames(DESvalue, "Value","DES")
#  
#  data=merge(data, DESvalue, by = "FBS Code", all =TRUE)
#  
#  setnames(data, "Commodity","FBS Commodity")
#  
#  # setcolorder(data,c("FBS Code","FBS Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Stock Variation [t]",
#  #                    "Food [t]","Processed [t]","Tourist consumption [t]", "Loss [t]", "Feed [t]", "Seed [t]", "Industrial uses [t]","Residual other uses [t]","Imbalance [t]"
#  #                    ,"DES"))
#  
#  
#  #tourist
#  
#  setcolorder(data,c("FBS Code","FBS Commodity","Production [t]", "Import Quantity [t]", "Export Quantity [t]", "Stock Variation [t]",
#                     "Food [t]","Processed [t]", "Loss [t]", "Feed [t]", "Seed [t]", "Industrial uses [t]","Residual other uses [t]","Imbalance [t]"
#                     ,"DES"))
#  
#  data[,3:15]=round(data[,3:15],0)
#  
#  
#  numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
#  data=subset(data, !`FBS Code` %in% "S2901")
#  
#  data[, `FBS Code` := substring(`FBS Code`,2)]
#  
#  
#  data[is.na(data)] <- ""
#  
#  
#  values_4$fbs <- data
#  
#  
#  datatable(data) %>%
#    
#    formatCurrency(numericcol,currency = "", digits = 0,interval = 3, mark = ",")%>%
#    
#    formatStyle('Imbalance [t]', backgroundColor =  styleEqual(0, c( 'yellow')))%>%
#  
#  formatStyle('DES','Imbalance [t]', backgroundColor = styleEqual(0,'lightblue'))
#  
#  
#  })



desreactive=eventReactive(input$standardizeAggrigate, {

 
  t=as.character(input$endyear)
  
  files_sua_balanced=list.files(path = "SUA-FBS Balancing/Data",pattern ="^sua_balanced_")
  
  
  if (paste0("sua_balanced_",t,".csv") %in% files_sua_balanced){
  
  
  
  data=fread(paste0("SUA-FBS Balancing/Data/sua_balanced_",t,".csv"))
  
  data <- subset(data, timePointYears %in% t)
  
  
  
  data[, geographicAreaM49 := NULL]
  
  setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
           
           c("ElementCode","CPCCode","Year","Value","Flag"))
  
  
  
  data=merge(data, commodityName, by= "CPCCode", all.x = TRUE)
  
  data=merge(data,elementName, by="ElementCode", all.x = TRUE)
  
  # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
  #tourist remove
  
  
  elemKeys=c("664")
  
  data=subset(data, ElementCode %in% elemKeys)
  
  
  data[,c("Flag" ) :=NULL]
  
 
 
  
  fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  
  fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
  
  setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
  
  fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
  
  fbsTree=merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)
  
  fbsTree[, fbsCode_S := NULL]
  
  setnames(fbsTree,"Commodity","FBS Commodity")
  
  # fbsTree <- fbsTree[,c("CPCCode" ,"FBSCode Lev4" ,"FBS Commodity Lev4")]
  
  # setnames(fbsTree, c("FBSCode Lev4", "FBS Commodity Lev4"),c("FBSCode","FBS Commodity"))
  
  data=merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)
  
  
  data[, tot := sum(Value), by = list(`FBS Code`,Year)]
  
  
  data <- data[,c('FBS Code', 'FBS Commodity', "ElementCode", "Element", "Year", "tot"), with= F]
  
  data <- unique(data)
  
  data <- data[order(`FBS Code`)]
  
  tot_DES <- data.table('FBS Code' = "2901", 'FBS Commodity' = "GRAND TOTAL - DEMAND", ElementCode = "664", Element= "Food supply (/capita/day) [kcal]"
                        , Year = unique(data$Year), tot= sum(data$tot))
  
  
  data<- rbind(tot_DES,data)
  
  
  setnames(data,"tot","Value")
  
  # 
  # setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" ,"Processed [t]",
  #                     "Tourist consumption [t]","Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Imbalance [t]" ))
  # 
  # 
  
  
  #removed tourist
  setcolorder(data, c("FBS Code","FBS Commodity","ElementCode", "Element", "Year", "Value" ))
  
  data[, Value := round(Value, 0)]
  
  # numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
  
  # data[is.na(data)] <- ""
  values_5$des <- data
  
  datatable(data) 
  
  # %>%
  #   
  #   formatCurrency(numericcol,currency = "", digits = 0,interval = 3, mark = ",") 
  #   # formatStyle(columns = 1:2, color = "grey")%>%
  #   # formatStyle(columns = 3:4, color = "blue") 
  
  }else {
    
    sendSweetAlert(
      session = session,
      title = paste("Please Run the Balancing Plugin to create Total Calories of", t),
      # text = paste(elementNameMissing, collapse = ' , '),
      type = "warning"
    )
    
    
    
    
  }
})



desreactive_time_series=reactive({
  
  
  t=as.character(input$endyear)
  
 
  
  
  # if (paste0("sua_balanced_",t,".csv") %in% files_sua_balanced){
    
    files_sua_balanced=list.files(path = "SUA-FBS Balancing/Data",pattern ="^sua_balanced_")
    
    
    DES_Years = vector(mode = "list", length = length(files_sua_balanced))
    
    
    
    for (i in seq_len(length(files_sua_balanced))){
      
      
      DES_Years[[i]]=fread(paste0("SUA-FBS Balancing/Data/", files_sua_balanced[i]))
      
     
      # DES_Years[[i]] <- rbindlist(DES_Years[[i]])
      
      
    }
    
    
    data<- rbindlist(DES_Years)
    
    # data <- subset(data, timePointYears %in% t)
    
    data <- subset(data,timePointYears %in% c(2014:t) )
    
    data[, geographicAreaM49 := NULL]
    
    setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
             
             c("ElementCode","CPCCode","Year","Value","Flag"))
    
    data[,ElementCode := as.character(ElementCode)]
    
    data=merge(data, commodityName, by= "CPCCode", all.x = TRUE)
    
    data=merge(data,elementName, by="ElementCode", all.x = TRUE)
    
    # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
    #tourist remove
    
    
    elemKeys=c("664")
    
    data=subset(data, ElementCode %in% elemKeys)
    
    
    data[,c("Flag" ) :=NULL]
    
    
    
    
    fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
    
    
    fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
    
    setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
    
    fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
    
    fbsTree=merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)
    
    fbsTree[, fbsCode_S := NULL]
    
    setnames(fbsTree,"Commodity","FBS Commodity")
    
    # fbsTree <- fbsTree[,c("CPCCode" ,"FBSCode Lev4" ,"FBS Commodity Lev4")]
    
    # setnames(fbsTree, c("FBSCode Lev4", "FBS Commodity Lev4"),c("FBSCode","FBS Commodity"))
    
    data=merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)
    
    
    data[, tot := sum(Value), by = list(`FBS Code`,Year)]
    
    
    data <- data[,c('FBS Code', 'FBS Commodity', "ElementCode", "Element", "Year", "tot"), with= F]
    
    data <- unique(data)
    
    data <- data[order(`FBS Code`)]
    
    
    tot_DES <- copy(data)
    
    tot_DES <- tot_DES[,c("Year","tot")]
    
    # tot_DES <- data.table('FBS Code' = "2901", 'FBS Commodity' = "GRAND TOTAL - DEMAND", ElementCode = "664", Element= "Food supply (/capita/day) [kcal]"
    #                       , Year = NA_character_, tot= NA_real_ )
    
    tot_DES <- tot_DES[, tot_sum := sum(tot), by = "Year"]
    
    tot_DES[,tot := NULL]
    
    tot_DES <- unique(tot_DES)
    
    tot_DES <- tot_DES[, `:=` ('FBS Code' = "2901",
                               'FBS Commodity' = "GRAND TOTAL - DEMAND",
                               ElementCode = "664",
                               Element= "Food supply (/capita/day) [kcal]")]
    
    setnames(tot_DES,"tot_sum","tot")
    
    data<- rbind(tot_DES,data)
    
    
    setnames(data,"tot","Value")
    
    # 
    # setcolorder(data, c("FBS Code","FBS Commodity",  "CPCCode", "Commodity", "Production [t]", "Import Quantity [t]", "Export Quantity [t]" ,"Stock Variation [t]" ,"Food [t]" ,"Processed [t]",
    #                     "Tourist consumption [t]","Loss [t]" ,"Feed [t]","Seed [t]" ,"Industrial uses [t]","Imbalance [t]" ))
    # 
    # 
    
    
    #removed tourist
    setcolorder(data, c("FBS Code","FBS Commodity","ElementCode", "Element", "Year", "Value" ))
    
    data[, Value := round(Value, 0)]
    
    data <- data[order(Value),]
    
    data_timeseries <- dcast.data.table(data, `FBS Code` + `FBS Commodity`+
                               ElementCode+Element ~ Year, value.var = c("Value"))
    
    data_timeseries <- data_timeseries[!is.na(`FBS Code`)]
    
    t=paste(t)
    
    data_timeseries=data_timeseries[order(data_timeseries[,t,with=F], decreasing = T),]
    
    data_timeseries
    
  
})
















desByCPC=reactive({
  
  
  
  # fbs_sua_conversion2 <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins","DESfoodSupply_kCd","proteinSupplyQt_gCd","fatSupplyQt_gCd", "exports", "feed", "food",
  #                                                           "foodManufacturing", "imports", "loss", "production",
  #                                                           "seed", "stockChange", "residual","industrial", "tourist"),
  #                                   code=c("261", "281", "271","664","674","684","5910", "5520", "5141",
  #                                          "5023", "5610", "5016", "5510",
  #                                          "5525", "5071", "5166","5165", "5164"))
  
  
  
  #tourist
  
  fbs_sua_conversion2 <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins","DESfoodSupply_kCd","proteinSupplyQt_gCd","fatSupplyQt_gCd", "exports", "feed", "food",
                                                            "foodManufacturing", "imports", "loss", "production",
                                                            "seed", "stockChange", "residual","industrial"),
                                    code=c("261", "281", "271","664","674","684","5910", "5520", "5141",
                                           "5023", "5610", "5016", "5510",
                                           "5525", "5071", "5166","5165"))
  
  DESCPClevel=fread("SUA-FBS Balancing/StandardizedData/DESCPClevel.csv") #after the balancing
  
  DESCPClevel = merge(DESCPClevel, fbs_sua_conversion2, by = "measuredElementSuaFbs")
  
  DESCPClevel[,`:=`(measuredElementSuaFbs = NULL)]
  
  
  DESCPClevel=DESCPClevel[,c("measuredItemSuaFbs","code","Value")]
  
  
  
  DESCPClevel=merge(DESCPClevel,elementName, by.x = "code", by.y = "ElementCode", all.x = TRUE)
  
  
  DESCPClevel[,code := NULL]
  
  DESCPClevel <-  DESCPClevel[, lapply(.SD,sum), by=list(measuredItemSuaFbs,Element)]
  
  DESCPClevel= dcast.data.table(DESCPClevel, measuredItemSuaFbs ~ Element, value.var = c("Value"))
  

  
  DESCPClevel[ , `supply`:= rowSums(.SD, na.rm=T), .SDcols=c("Production [t]","Import Quantity [t]" )]
  
  
  
  # DESCPClevel[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
  #                                                                  ,"Industrial uses [t]","Loss [t]","Seed [t]","Tourist consumption [t]","Export Quantity [t]",
  #                                                                  "Processed [t]", "Residual other uses [t]")]
  
  
  DESCPClevel[ , `utilization`:= rowSums(.SD, na.rm=T), .SDcols=c( "Feed [t]","Food [t]","Stock Variation [t]"
                                                                   ,"Industrial uses [t]","Loss [t]","Seed [t]","Export Quantity [t]",
                                                                   "Processed [t]", "Residual other uses [t]")]
  
  
  DESCPClevel[, `Imbalance [t]` := supply - utilization]
  
  DESCPClevel[, `Imbalance [t]` := round(`Imbalance [t]`,0)]
  
  
  DESCPClevel[, c("supply","utilization"):=NULL]
  
  
  print(unique(DESCPClevel$`Imbalance [t]`)) #zero confirms that the table is after balancing
  
  
  
  DESCPClevel=DESCPClevel[,c("measuredItemSuaFbs","Calories/Year [Mln kcal]", "Fats/Year [t]","Proteins/Year [t]")]
  
  setnames(DESCPClevel, "Calories/Year [Mln kcal]","calories")
  
  
  
  fbsTree= fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  
  
  
  DESCPClevel = merge(DESCPClevel, fbsTree, by.x = "measuredItemSuaFbs",by.y = "item_sua_fbs",all.x = TRUE)
  
  
  
  
  DESCPClevel=DESCPClevel[, c("id1","id2","id3","id4","calories", "Fats/Year [t]","Proteins/Year [t]")]
  
  
  
  
  table1 = aggregate( .~ id1,data = DESCPClevel[,-c("id2","id3", "id4")],sum)
  setDT(table1)
  table1[, c("id2", "id3", "id4") := NA_character_]
  
  
  
  table2 = aggregate( .~ id2,data = DESCPClevel[,-c("id1","id3", "id4")],sum)
  setDT(table2)
  table2[, c("id1", "id3", "id4") := NA_character_]
  
  
  
  table3 = aggregate( .~ id3,data = DESCPClevel[,-c("id2","id1", "id4")],sum)
  setDT(table3)
  table3[, c("id1", "id2", "id4") := NA_character_]
  
  
  
  table4 = aggregate( .~ id4,data = DESCPClevel[,-c("id2","id3", "id1")],sum)
  setDT(table4)
  table4[, c("id1", "id2", "id3") := NA_character_]
  
  DESCPC=rbind(table1,table2,table3,table4)
  
  
  # setcolorder(DESCPC, c("id1","id2","id3","id4","calories"))
  
  
  
  # population <- fread("SUA-FBS Balancing/Data/pop_all_countries.csv")
  
  
  population=read_excel("Data/populationData.xlsx")
  
  population <- data.table(population)
  
  areaKeys=fread("SUA-FBS Balancing/Data/geo.csv")
  
  population[, geographicAreaM49 := as.character(unique(areaKeys$geographicAreaM49))]
  
  population[, measuredElement := as.character(511)]
  
  population[, flagObservationStatus := as.character("X")]
  population[, flagMethod := as.character("h")]
  
  setnames(population,c("Year","Population [1000]"),c("timePointYears","Value"))
  
  setcolorder(population,c("geographicAreaM49", "measuredElement" ,
                    "timePointYears" ,"Value" ,"flagObservationStatus", "flagMethod") )
  
  population[geographicAreaM49=="156",geographicAreaM49:="1248"] 
  
  
  
  population <- population[geographicAreaM49 %in% unique(areaKeys$geographicAreaM49)]
  
  population <- subset(population, timePointYears %in% as.character(input$endyear))
  
  
  
  DESCPC[, population := population$Value]
  
  
  DESCPC[, `DES (kcal/capita/day)` := (calories/365)/(population*1000)]
  
  DESCPC[,proteinSupplyQt_gCd:=(`Proteins/Year [t]`/365)/(population*1000)]
  DESCPC[,fatSupplyQt_gCd:=(`Fats/Year [t]`/365)/(population*1000)]
  
  DESCPC=DESCPC[,-c("Proteins/Year [t]","Fats/Year [t]")]
  
  
  
  fbsName =read_excel("Data/Reference File.xlsx", "FBS_Commodities")
  
  setDT(fbsName)
  
  setnames(fbsName, "Commodity","FBS Commodity")
  
  DESCPC=merge(DESCPC,fbsName, by.x =c("id1") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 1")
  
  DESCPC=merge(DESCPC,fbsName, by.x =c("id2") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 2")
  DESCPC=merge(DESCPC,fbsName, by.x =c("id3") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 3")
  DESCPC=merge(DESCPC,fbsName, by.x =c("id4") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 4")
  
  
  
  sua_variables= fread("SUA-FBS Balancing/StandardizedData/DESCPClevel.csv") #after the balancing
  
 
  sua_variables <- sua_variables[, c("measuredItemSuaFbs","measuredElementSuaFbs", "Value")]
  
  sua_variables <- sua_variables[, lapply(.SD,sum), by=list(measuredItemSuaFbs,measuredElementSuaFbs)]
  
  
  setDT(sua_variables)
  sua_variables= dcast.data.table(sua_variables,measuredItemSuaFbs ~ measuredElementSuaFbs, value.var = c("Value"))
  
  sua_variables <- sua_variables[, -c ("Calories","Fats","Proteins")]
  
  sua_variables<- merge(sua_variables,fbsTree[,c("id4","item_sua_fbs")], by.x="measuredItemSuaFbs",by.y = "item_sua_fbs", all.x = TRUE)
  
  sua_variables[, "measuredItemSuaFbs" := NULL]
  
  
  
  
  
  sua_variables = aggregate( .~ id4,data = sua_variables,sum)
  setDT(sua_variables)
  
  
  
  
  
  DESCPC=merge(DESCPC,sua_variables, by= "id4" ,all.x = TRUE)
  

  
  
  #spliiting the rows 
  
  rbind1= DESCPC[id1== "2901"]
  
  
  
  #Vegetable products
  
  rbind2=DESCPC[id2== "2903"]
  
  DT_vegetables <- data.table(
    id4 = character(),
    id3 = character(),
    id2 = character(),
    id1=character(),
    calories=numeric(),
    fatSupplyQt_gCd=numeric(),
    proteinSupplyQt_gCd=numeric(),
    exports=numeric(),
    feed=numeric(), 
    food =numeric(),
    foodManufacturing=numeric(),
    imports=numeric(),
    industrial=numeric(),
    loss=numeric(),
    production=numeric(),
    residual=numeric(),
    seed=numeric(),
    stockChange=numeric(),
    tourist=numeric(),
    population=numeric(),
    `DES (kcal/capita/day)` =numeric(),
    `Level 1`= character(),
    `Level 2` =character(),
    `Level 3` =character(),
    `Level 4` = character()
  )
  
  
  
  
  for (i in sort(unique(fbsTree[id2== "2903"]$id3),decreasing = FALSE)){
    
    
    level_3 = DESCPC[id3== i]
    
    
    level_4= DESCPC[id4 %in%  unique( fbsTree[id3 == i ]$id4)]
    
    
    
    DT_vegetables=rbind(DT_vegetables,level_3,level_4)
    
    
  }
  
  
  
  #Animal Products
  
  rbind3=DESCPC[id2== "2941"]
  
  
  DT_animalproducts <- data.table(
    id4 = character(),
    id3 = character(),
    id2 = character(),
    id1=character(),
    calories=numeric(),
    fatSupplyQt_gCd=numeric(),
    proteinSupplyQt_gCd=numeric(),
    population=numeric(),
    exports=numeric(),
    feed=numeric(), 
    food =numeric(),
    foodManufacturing=numeric(),
    imports=numeric(),
    industrial=numeric(),
    loss=numeric(),
    production=numeric(),
    residual=numeric(),
    seed=numeric(),
    stockChange=numeric(),
    tourist=numeric(),
    `DES (kcal/capita/day)` =numeric(),
    `Level 1`= character(),
    `Level 2` =character(),
    `Level 3` =character(),
    `Level 4` = character()
  )
  
  
  
  
  for (i in sort(unique(fbsTree[id2== "2941"]$id3),decreasing = FALSE)){
    
    
    level_3_animal = DESCPC[id3== i]
    
    
    level_4_animal= DESCPC[id4 %in%  unique( fbsTree[id3 == i ]$id4)]
    
    
    
    DT_animalproducts=rbind(DT_animalproducts,level_3_animal,level_4_animal)
  }
  
  
  DESCPC_final=rbind(rbind1,rbind2,DT_vegetables,rbind3, DT_animalproducts)
  
  
  
  


 

  DESCPC_final=DESCPC_final[,- c("id1","id2","id3","id4","population", "calories")]
  
  
  
  DESCPC_final[, `DES (kcal/capita/day)` := round(`DES (kcal/capita/day)`,1)]
  
  
  #tourist
  
  DESCPC_final[, tourist := NULL]
  # setcolorder(DESCPC_final, c("Level 1", "Level 2","Level 3","Level 4", "production", "imports","exports",
  #                             "stockChange", "food", "foodManufacturing", "tourist", "feed","seed", "loss", "residual", "industrial", "DES (kcal/capita/day)"
  #                             , "fatSupplyQt_gCd","proteinSupplyQt_gCd"))
  # 
 
  setcolorder(DESCPC_final, c("Level 1", "Level 2","Level 3","Level 4", "production", "imports","exports",
                              "stockChange", "food", "foodManufacturing", "feed","seed", "loss", "residual", "industrial", "DES (kcal/capita/day)"
                              , "fatSupplyQt_gCd","proteinSupplyQt_gCd"))
  
  
  
  # setnames(DESCPC_final,c("production","imports","exports","stockChange","food","foodManufacturing","tourist","feed","seed","loss","residual", "industrial")
  #          ,c("Production","Imports","Exports","Stock Changes","Food","Food Processing","Tourist Food","Feed","Seed","Loss","Residual and Other Uses","Industrial Use"))
  # 
  
  #tourist
  
  setnames(DESCPC_final,c("production","imports","exports","stockChange","food","foodManufacturing","feed","seed","loss","residual", "industrial")
           ,c("Production","Imports","Exports","Stock Changes","Food","Food Processing","Feed","Seed","Loss","Residual and Other Uses","Industrial Use"))
  
  
  cols2round<- names(DESCPC_final[,-c("Level 1","Level 2","Level 3","Level 4")])
  DESCPC_final[,(cols2round) := round(.SD,1), .SDcols=cols2round]
  
  DESCPC_final[is.na(DESCPC_final)] <- ""
  
  # numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
  values_6$desCommodity <- DESCPC_final
  
  datatable(DESCPC_final)  %>%
  
 
 formatCurrency(cols2round,currency = "", digits = 0,interval = 3, mark = ",")
    

  # DESCPC_final
  
})






DESbyCPCYear=reactive({
  
  

  
  
 files_DES=list.files(path = "SUA-FBS Balancing/StandardizedData",pattern ="^DESCPClevel_")
  

 DES_Years = vector(mode = "list", length = length(files_DES))
 
 
 
 for (i in seq_len(length(files_DES))){
   
   
   DES_Years[[i]]=fread(paste0("SUA-FBS Balancing/StandardizedData/", files_DES[i]))
   
   DES_Years[[i]]=subset(DES_Years[[i]], measuredElementSuaFbs == "Calories")
   
   DES_Years[[i]]=DES_Years[[i]][, c("timePointYears","measuredItemSuaFbs","measuredElementSuaFbs","Value")]
   
   # DES_Years[[i]] <- rbindlist(DES_Years[[i]])
   
   
   }
 
 
 DES_Years <- rbindlist(DES_Years)
 

 
 
 
  
 DES_Years[, measuredElementSuaFbs := NULL]
 
 
######################################################################
DES_2010_2013 <- fread("SUA-FBS Balancing/StandardizedData/sua_balanced_data_2010_2013.csv")
 
DES_2010_2013 <- DES_2010_2013[measuredElementSuaFbs %in% "664"]

DES_2010_2013[, c("geographicAreaM49","measuredElementSuaFbs","flagObservationStatus","flagMethod") := NULL]

setnames(DES_2010_2013,"measuredItemFbsSua","measuredItemSuaFbs")
 
##########################################################
 
 
 
DES_Years <- rbind(DES_Years,DES_2010_2013)
 
 
DESCPClevel= dcast.data.table(DES_Years, measuredItemSuaFbs ~ timePointYears, value.var = c("Value"))
  

  
fbsTree= fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  
  
  
DESCPClevel = merge(DESCPClevel, fbsTree, by.x = "measuredItemSuaFbs",by.y = "item_sua_fbs",all.x = TRUE)
  
  
  
  
DESCPClevel=DESCPClevel[, measuredItemSuaFbs := NULL]
  
  # DESCPClevel[is.na(id1), id1 := "2901"]
  
  
  table1 = aggregate( .~ id1,data = DESCPClevel[,-c("id2","id3", "id4")],FUN=sum,na.rm=TRUE, na.action=NULL)
  setDT(table1)
  table1[, c("id2", "id3", "id4") := NA_character_]
  
  
  
  table2 = aggregate( .~ id2,data = DESCPClevel[,-c("id1","id3", "id4")],sum)
  setDT(table2)
  table2[, c("id1", "id3", "id4") := NA_character_]
  
  
  
  table3 = aggregate( .~ id3,data = DESCPClevel[,-c("id2","id1", "id4")],sum)
  setDT(table3)
  table3[, c("id1", "id2", "id4") := NA_character_]
  
  
  
  table4 = aggregate( .~ id4,data = DESCPClevel[,-c("id2","id3", "id1")],sum)
  setDT(table4)
  table4[, c("id1", "id2", "id3") := NA_character_]
  
  DESCPC=rbind(table1,table2,table3,table4)
  
  
  # setcolorder(DESCPC, c("id1","id2","id3","id4","calories"))
  geo=fread("SUA-FBS Balancing/Data/geo.csv") 
  
#################################################################################33  
  # population <- fread("SUA-FBS Balancing/Data/pop_all_countries.csv")
  
  population=read_excel("Data/populationData.xlsx")
  
  population <- data.table(population)
  
  population[, geographicAreaM49 := as.character(geo$geographicAreaM49)]
  
  population[, measuredElement := as.character(511)]
  
  population[, flagObservationStatus := as.character("X")]
  population[, flagMethod := as.character("h")]
  
  setnames(population,c("Year","Population [1000]"),c("timePointYears","Value"))
  
  setcolorder(population,c("geographicAreaM49", "measuredElement" ,
                       "timePointYears" ,"Value" ,"flagObservationStatus", "flagMethod") )
  
  population[geographicAreaM49=="156",geographicAreaM49:="1248"]
  
  
##############################################################  
 
  
  
  population <- population[geographicAreaM49 %in% unique(geo$geographicAreaM49)]
  population <- subset(population, timePointYears %in% as.character(2014:input$endyear))
  
  population <- population[, c("measuredElement","timePointYears","Value")]
  
  setnames(population, "Value","Population")
  
  population <- data.table(dcast(population, measuredElement~ timePointYears,value.var = "Population"))
  
  
  population[,measuredElement := NULL]
  
  
  pop_columns <- grep("^[[:digit:]]{4}$", names(population), value = TRUE)
  
  
  pop_columns_new <- paste0("Pop_", pop_columns)
  
  setnames(population, pop_columns, pop_columns_new)
  
  DESCPC <-  cbind(DESCPC,population )
  
  
  
  for (i in grep("^[[:digit:]]{4}$", names(DESCPC), value = TRUE)
       [!grep("^[[:digit:]]{4}$", names(DESCPC), value = TRUE) %in% c(2010:2013)]){
    
    pop<-as.symbol(paste0("Pop_", i))
    yearColumn <- as.symbol(i)
    new <- paste0("DES_",i) 
    DESCPC[,(new) := eval(pop)]
    
    
    DESCPC[,(new) :=  (eval(yearColumn)/365)/(eval(pop)*1000)]
    
  
    
  }
  
  
  setnames(DESCPC, c("2010","2011","2012","2013"), c("DES_2010","DES_2011","DES_2012","DES_2013"))
  
  
  
  

  
  
#################################################################
  
  
  # DESCPC[, `DES (kcal/capita/day)` := (calories/365)/(population*1000)]
  
  
  delete_column1 <- grep("^[[:digit:]]{4}$", names(DESCPC), value = TRUE)
  
  
  delete_column2 <- grep("^Pop", names(DESCPC), value = TRUE)
  
  
  DESCPC[,c(delete_column1,delete_column2):= NULL]
 

  
  
  fbsName =read_excel("Data/Reference File.xlsx", "FBS_Commodities")
  
  setDT(fbsName)
  
  setnames(fbsName, "Commodity","FBS Commodity")
  
  DESCPC=merge(DESCPC,fbsName, by.x =c("id1") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 1")
  
  DESCPC=merge(DESCPC,fbsName, by.x =c("id2") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 2")
  DESCPC=merge(DESCPC,fbsName, by.x =c("id3") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 3")
  DESCPC=merge(DESCPC,fbsName, by.x =c("id4") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 4")
  
  

  

  
  #spliiting the rows 
  
  rbind1= DESCPC[id1== "2901"]
  
  
  
  #Vegetable products
  
  rbind2=DESCPC[id2== "2903"]
  

  
  
  
  DT_vegetables <- copy(DESCPC)
  
  DT_vegetables<-DT_vegetables[0,]
  
  
  
  
  for (i in sort(unique(fbsTree[id2== "2903"]$id3),decreasing = FALSE)){
    
    
    level_3 = DESCPC[id3== i]
    
    
    level_4= DESCPC[id4 %in%  unique( fbsTree[id3 == i ]$id4)]
    
    
    
    DT_vegetables=rbind(DT_vegetables,level_3,level_4)
    
    
  }
  
  
  
  #Animal Products
  
  rbind3=DESCPC[id2== "2941"]
  
  DT_animalproducts<- copy(DESCPC)
  
  DT_animalproducts <- DT_animalproducts[0,]
  
  
  for (i in sort(unique(fbsTree[id2== "2941"]$id3),decreasing = FALSE)){
    
    
    level_3_animal = DESCPC[id3== i]
    
    
    level_4_animal= DESCPC[id4 %in%  unique( fbsTree[id3 == i ]$id4)]
    
    
    
    DT_animalproducts=rbind(DT_animalproducts,level_3_animal,level_4_animal)
  }
  
  
  DESCPC_final=rbind(rbind1,rbind2,DT_vegetables,rbind3, DT_animalproducts)
  DESCPC_final=DESCPC_final[,- c("id1","id2","id3","id4")]
  
  
  
  roundColumns <-  grep("^DES", names(DESCPC_final), value = TRUE)
  
  
  DESCPC_final[,(roundColumns) := round(.SD,0), .SDcols=roundColumns]
  
  setcolorder(DESCPC_final, c("Level 1", "Level 2","Level 3","Level 4",roundColumns ))
  
  DESCPC_final[is.na(DESCPC_final)] <- ""
  
  
  # 
  # 
  # cols2round<- names(DESCPC_final[,-c("Level 1","Level 2","Level 3","Level 4")])
  # DESCPC_final[,(cols2round) := round(.SD,1), .SDcols=cols2round]
  # 
  # numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
  values_7$desCommodityYear <- DESCPC_final
  # 
  # datatable(DESCPC_final)  %>%
  #   
  #   
  #   formatCurrency(cols2round,currency = "", digits = 0,interval = 3, mark = ",")
  # 
  # 
  # DESCPC_final
  
})



DESbyCPCYear_G=reactive({
  
  
  
  
  
  files_DES=list.files(path = "SUA-FBS Balancing/StandardizedData",pattern ="^DESCPClevel_")
  
  
  DES_Years = vector(mode = "list", length = length(files_DES))
  
  
  
  for (i in seq_len(length(files_DES))){
    
    
    DES_Years[[i]]=fread(paste0("SUA-FBS Balancing/StandardizedData/", files_DES[i]))
    
    DES_Years[[i]]=subset(DES_Years[[i]], measuredElementSuaFbs == "Calories")
    
    DES_Years[[i]]=DES_Years[[i]][, c("timePointYears","measuredItemSuaFbs","measuredElementSuaFbs","Value")]
    
    # DES_Years[[i]] <- rbindlist(DES_Years[[i]])
    
    
  }
  
  
  DES_Years <- rbindlist(DES_Years)
  
  
  
  
  
  
  DES_Years[, measuredElementSuaFbs := NULL]
  
  
  ######################################################################
  DES_2010_2013 <- fread("SUA-FBS Balancing/StandardizedData/sua_balanced_data_2010_2013.csv")
  
  DES_2010_2013 <- DES_2010_2013[measuredElementSuaFbs %in% "664"]
  
  DES_2010_2013[, c("geographicAreaM49","measuredElementSuaFbs","flagObservationStatus","flagMethod") := NULL]
  
  setnames(DES_2010_2013,"measuredItemFbsSua","measuredItemSuaFbs")
  
  ##########################################################
  
  
  
  DES_Years <- rbind(DES_Years,DES_2010_2013)
  
  
  DESCPClevel= dcast.data.table(DES_Years, measuredItemSuaFbs ~ timePointYears, value.var = c("Value"))
  
  
  
  fbsTree= fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  
  
  
  DESCPClevel = merge(DESCPClevel, fbsTree, by.x = "measuredItemSuaFbs",by.y = "item_sua_fbs",all.x = TRUE)
  
  
  
  
  DESCPClevel=DESCPClevel[, measuredItemSuaFbs := NULL]
  
  # DESCPClevel[is.na(id1), id1 := "2901"]
  
  
  table1 = aggregate( .~ id1,data = DESCPClevel[,-c("id2","id3", "id4")],FUN=sum,na.rm=TRUE, na.action=NULL)
  setDT(table1)
  table1[, c("id2", "id3", "id4") := NA_character_]
  
  
  
  table2 = aggregate( .~ id2,data = DESCPClevel[,-c("id1","id3", "id4")],sum)
  setDT(table2)
  table2[, c("id1", "id3", "id4") := NA_character_]
  
  
  
  table3 = aggregate( .~ id3,data = DESCPClevel[,-c("id2","id1", "id4")],sum)
  setDT(table3)
  table3[, c("id1", "id2", "id4") := NA_character_]
  
  
  
  table4 = aggregate( .~ id4,data = DESCPClevel[,-c("id2","id3", "id1")],sum)
  setDT(table4)
  table4[, c("id1", "id2", "id3") := NA_character_]
  
  DESCPC=rbind(table1,table2,table3,table4)
  
  
  # setcolorder(DESCPC, c("id1","id2","id3","id4","calories"))
  geo=fread("SUA-FBS Balancing/Data/geo.csv") 
  
  
  population <- fread("SUA-FBS Balancing/Data/pop_all_countries.csv")
  
  population <- population[geographicAreaM49 %in% unique(geo$geographicAreaM49)]
  population <- subset(population, timePointYears %in% as.character(2014:input$endyear))
  
  population <- population[, c("measuredElement","timePointYears","Value")]
  
  setnames(population, "Value","Population")
  
  population <- data.table(dcast(population, measuredElement~ timePointYears,value.var = "Population"))
  
  
  population[,measuredElement := NULL]
  
  
  pop_columns <- grep("^[[:digit:]]{4}$", names(population), value = TRUE)
  
  
  pop_columns_new <- paste0("Pop_", pop_columns)
  
  setnames(population, pop_columns, pop_columns_new)
  
  DESCPC <-  cbind(DESCPC,population )
  
  
  
  for (i in grep("^[[:digit:]]{4}$", names(DESCPC), value = TRUE)
       [!grep("^[[:digit:]]{4}$", names(DESCPC), value = TRUE) %in% c(2010:2013)]){
    
    pop<-as.symbol(paste0("Pop_", i))
    yearColumn <- as.symbol(i)
    new <- paste0("DES_",i) 
    DESCPC[,(new) := eval(pop)]
    
    
    DESCPC[,(new) :=  (eval(yearColumn)/365)/(eval(pop)*1000)]
    
    
    
  }
  
  
  setnames(DESCPC, c("2010","2011","2012","2013"), c("DES_2010","DES_2011","DES_2012","DES_2013"))
  
  
  
  
  
  
  
  #################################################################
  
  
  # DESCPC[, `DES (kcal/capita/day)` := (calories/365)/(population*1000)]
  
  
  delete_column1 <- grep("^[[:digit:]]{4}$", names(DESCPC), value = TRUE)
  
  
  delete_column2 <- grep("^Pop", names(DESCPC), value = TRUE)
  
  
  DESCPC[,c(delete_column1,delete_column2):= NULL]
  
  
  
  
  fbsName =read_excel("Data/Reference File.xlsx", "FBS_Commodities")
  
  setDT(fbsName)
  
  setnames(fbsName, "Commodity","FBS Commodity")
  
  DESCPC=merge(DESCPC,fbsName, by.x =c("id1") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 1")
  
  DESCPC=merge(DESCPC,fbsName, by.x =c("id2") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 2")
  DESCPC=merge(DESCPC,fbsName, by.x =c("id3") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 3")
  DESCPC=merge(DESCPC,fbsName, by.x =c("id4") ,by.y ="FBSCode" ,all.x = TRUE)
  
  setnames(DESCPC, "FBS Commodity","Level 4")
  
  
  
  
  
  
  #spliiting the rows 
  
  rbind1= DESCPC[id1== "2901"]
  
  
  
  #Vegetable products
  
  rbind2=DESCPC[id2== "2903"]
  
  
  
  
  
  DT_vegetables <- copy(DESCPC)
  
  DT_vegetables<-DT_vegetables[0,]
  
  
  
  
  for (i in sort(unique(fbsTree[id2== "2903"]$id3),decreasing = FALSE)){
    
    
    level_3 = DESCPC[id3== i]
    
    
    level_4= DESCPC[id4 %in%  unique( fbsTree[id3 == i ]$id4)]
    
    
    
    DT_vegetables=rbind(DT_vegetables,level_3,level_4)
    
    
  }
  
  
  
  #Animal Products
  
  rbind3=DESCPC[id2== "2941"]
  
  DT_animalproducts<- copy(DESCPC)
  
  DT_animalproducts <- DT_animalproducts[0,]
  
  
  for (i in sort(unique(fbsTree[id2== "2941"]$id3),decreasing = FALSE)){
    
    
    level_3_animal = DESCPC[id3== i]
    
    
    level_4_animal= DESCPC[id4 %in%  unique( fbsTree[id3 == i ]$id4)]
    
    
    
    DT_animalproducts=rbind(DT_animalproducts,level_3_animal,level_4_animal)
  }
  
  
  DESCPC_final=rbind(rbind1,rbind2,DT_vegetables,rbind3, DT_animalproducts)
  DESCPC_final=DESCPC_final[,- c("id1","id2","id3","id4")]
  
  
  
  roundColumns <-  grep("^DES", names(DESCPC_final), value = TRUE)
  
  
  DESCPC_final[,(roundColumns) := round(.SD,0), .SDcols=roundColumns]
  
  setcolorder(DESCPC_final, c("Level 1", "Level 2","Level 3","Level 4",roundColumns ))
  
  DESCPC_final[is.na(DESCPC_final)] <- ""
  
  
  # 
  # 
  # cols2round<- names(DESCPC_final[,-c("Level 1","Level 2","Level 3","Level 4")])
  # DESCPC_final[,(cols2round) := round(.SD,1), .SDcols=cols2round]
  # 
  # numericcol= names(data[,sapply(data,is.numeric),with=FALSE])
  values_7$desCommodityYear <- DESCPC_final
  # 
  # datatable(DESCPC_final)  %>%
  #   
  #   
  #   formatCurrency(cols2round,currency = "", digits = 0,interval = 3, mark = ",")
  # 
  # 
  # DESCPC_final
  
})




















suaAgg <- reactive({
  
  data<- suaAggregate(input,output,session)
  
  colmunsNumeric <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
  
  data[, (colmunsNumeric) := lapply(.SD, as.numeric), .SDcols = colmunsNumeric]
  
})

suaAgg_fbs<- reactive(
  
  data<- suaAggregate_fbs(input,output,session)
  
  
  
)


suaAgg_all<- reactive(
  
  data<- sua_balanced_all_elements(input,output,session)
  
  
  
)


output$downloadSUA_Aggregate <- downloadHandler(

  filename = function() { paste("suaAggregate",input$endyear, '.xlsx', sep='') },
  content = function(file) {
   # write.xlsx(suaAgg(), file, row.names = FALSE,sheetName="SUA Balanced Aggregation by CPC",append=FALSE)
   #  write.xlsx(suaAgg_fbs(), file, row.names = FALSE,sheetName="SUA Balanced Aggregation by FBS",append =TRUE)
   #  write.xlsx(suaAgg_all(), file, row.names = FALSE,sheetName="SUA Balanced Aggregation All  Elements",append =TRUE)
   #  
    # list_of_datasets <- list("SUA KCal by CPC" = suaAgg(), "SUA All  Elements"
    #                          = suaAgg_all())
    
    list_of_datasets <- list("SUA KCal by CPC" = suaAgg(), "SUA All  Elements"
                             = suaAgg_all(), "x" = suaAgg_fbs())
    
    write.xlsx(list_of_datasets, file)
    
    
    
    }

)





  
output$downloadSuaUnbalanced <- downloadHandler(
  filename = function() { paste("suaUnbalanced",input$endyear, '.xlsx', sep='') },
  content = function(file) {
    write.xlsx(values_1$sua_unbalanced, file, row.names = FALSE)
  }

)



output$downloadstand <- downloadHandler(
  filename = function() { paste("suaBalanced",input$endyear, '.xlsx', sep='') },
  content = function(file) {
    write.xlsx(values_2$sua_balanced, file)
  }
)



output$downloadPrimary <- downloadHandler(
  filename = function() { paste("suaCPCPrimaryEquivalentTable",input$endyear, '.csv', sep='') },
  content = function(file) {
    write.csv(values_3$fbs_stand, file)
  }
)





output$downloadfbs <- downloadHandler(
  filename = function() { paste("FBS",input$endyear, '.csv', sep='') },
  content = function(file) {
    write.csv(values_4$fbs, file)
  }
)




output$downloadDES <- downloadHandler(
  filename = function() { paste("DES",input$endyear, '.csv', sep='') },
  content = function(file) {
    write.csv(values_5$des, file)
  }
)

output$downloadDES_timeseries <- downloadHandler(
  filename = function() { paste("DES_timeseries",input$endyear, '.csv', sep='') },
  content = function(file) {
    write.csv(desreactive_time_series(), file)
  }
)



output$downloadDESbyCPC <- downloadHandler(
  filename = function() { paste("DES by Commodity Group",input$endyear, '.csv', sep='') },
  content = function(file) {
    write.csv(values_6$desCommodity, file)
  }
)




output$downloadDESbyCPCYear <- downloadHandler(
  filename = function() { paste("DES by Commodity by year",input$endyear, '.csv', sep='') },
  content = function(file) {
    write.csv(values_7$desCommodityYear, file)
  }
)



  
output$suaUnbalanced=renderDataTable({

   # if (input$startStandardization > 0 ) {

      suaUnbalancedReactive()
     
    
     
   # } 
  
#   else{
#   
#      suaUnbalancedReactive_G()
#      
# }

})
  





output$suaProcessed=renderDataTable({

  if (input$balancedProcessed > 0 ) {

    suaProcessedReactive()

  }



})




# output$primary=renderDataTable({
# 
#   if (input$standardizeAggrigate > 0 ) {
# 
#     primaryEquivalentReactive()
# 
#   }
# 
# 
# 
# })



# 
# output$fbs=renderDataTable ({
# 
#   nn=nrow(fbsReactive())
# 
#   if (input$balanceFBS > 0 ) {
# 
# 
#   fbsReactive()
# 
# }
# 
# })




output$DES=renderDataTable({

  if (input$standardizeAggrigate > 0 ) {

 desreactive()

  }
  
})




# output$DESbyCPC=renderDataTable({
#   
#   if (input$DESButtonCPC > 0 ) {
#     
#     desByCPC()
#     
#   }
#   
# })




# output$DESbyCPCYear=renderDataTable({
#   
#   if (input$DESButtonCPCYear > 0 ) {
#     
#     DESbyCPCYear()
#     
#   }
#   
#   else{
#   #   
#     DESbyCPCYear_G()
#   #   
#   }
#   
# })
#   
  
}