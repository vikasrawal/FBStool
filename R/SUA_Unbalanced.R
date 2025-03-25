

SUA_Unbalanced <- function(input,output,session){

  
  

  
  observeEvent(input$startContinue, {
  
  if (input$endyear == ""){
    
    data <- NULL
    
    validate(
      need(!is.null(data), "No data")
    )
    
  }

  
  else {
    
    
    data <- copy(countryData)
    
    data[, c("CountryM49","Country") := NULL]
    
    
    data <- subset(data, Year %in% 2010:input$endyear)
    
    
##################food processing for Zanzibar commented#########################################################################
    
    
    food_processing_data <- Calculate_Food_Processing(input,output,session)
    
   
   
    data <-rbind(data,food_processing_data)
    
#################################################################################################################    
    
    
   sua_elements <- c("5510","5610","5910","5071", "5141", "5525","5520","5165","5166", "5016","5113", "5023")
   
   data <- subset(data,ElementCode %in% sua_elements)
  
  data[, Value := round(Value,0)]
  
  data_residual <- copy(data)
  
  data_residual[,c("Commodity","Element") := NULL]
  
  
  
  #####compute imbalance
  
  
  data_residual[,
       `:=`(
         supply =
           sum(Value[ElementCode %chin% c("5510","5610")],
               - Value[ElementCode %chin% c("5910","5071")],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(ElementCode %chin% c("5510","5610","5910","5071","5113"))],
               na.rm = TRUE)
       ),
       by = c("CPCCode","Year")
       ][,
         imbalance := supply - utilizations
         ][ , c("supply","utilizations") := NULL
         ]
  
  
  
  data_residual[, c("ElementCode","Value", "Flag") := NULL]
  
  data_residual[, ElementCode := c("5166")]
  setnames(data_residual, "imbalance","Value")
  data_residual[, Flag := c("I")]
  
  
  data_residual <- unique(data_residual)
  
  data_residual <- merge(data_residual,all_cpc, by= "CPCCode",all.x = TRUE)
  data_residual <- merge(data_residual,all_elements, by= "ElementCode",all.x = TRUE)
  
  data <- rbind(data, data_residual)
  
  
  
###########################################################################################################################################################
  
  data <- wide_format(data)
  
  # write.csv(data, "Ubalanced.csv", row.names = F)
  
  # data <- fread("Ubalanced.csv")
  
 #sort it by fbs tree 
  fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
  fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
  
  setnames(fbsTree, c("item_sua_fbs"),c("CPCCode"))
  
  fbsTree[, CPCCode := as.character(CPCCode)]
  
  data <- merge(data, fbsTree, by= c("CPCCode"), all.x = TRUE)
  
  
  
  
  
  data <-data[order(CPCCode, factor(ElementCode, levels = c("5113", "5510","5610","5910","5071", "5141","5023", "5525","5520","5016","5165","5166")))]
  
  data <-data[order(id4)]
  
  data[, id4 := NULL]
  
  }
  
  validate(
    need(!is.null(data), "No data")
  )
  

data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
  
df_sua_unbalanced$data_sua_unbalanced <- data


  
})
  

  # also food processing must be updated 
update_residual <- function(){
  
  
  
  data <- copy(df_sua_unbalanced$data_sua_unbalanced)
  
  # write.csv(data,"test_sua.csv",row.names = T)
  
  data <- long_format(data)
  
  
  food_processing_data <- Calculate_Food_Processing(input,output,session)
 
  data <- data[!ElementCode %in% "5023"]
   
  data <-rbind(data,food_processing_data)
  
  
  # write.csv(data,"sua_unbalanced_test.csv",row.names = F)
  
  data <- subset(data, Year %in% 2010:input$endyear)
  
  #we do not take 5166, because we are about to update 5166
  sua_elements <- c("5510","5610","5910","5071", "5141", "5525","5520","5165", "5016","5113", "5023")
  
  data <- subset(data,ElementCode %in% sua_elements)
  
  data[, Value := round(Value,0)]
  
  data_residual <- copy(data)
  
  data_residual[,c("Commodity","Element") := NULL]
  
  data_residual[, Year := as.character(Year)]
  data_residual[, ElementCode := as.character(ElementCode)]
  
  #####compute imbalance
  
  
  data_residual[,
                `:=`(
                  supply =
                    sum(Value[ElementCode %chin% c("5510","5610")],
                        - Value[ElementCode %chin% c("5910","5071")],
                        na.rm = TRUE),
                  # All elements that are NOT supply elements
                  utilizations =
                    sum(Value[!(ElementCode %chin% c("5510","5610","5910","5071","5113"))],
                        na.rm = TRUE)
                ),
                by = c("CPCCode","Year")
  ][,
    imbalance := supply - utilizations
  ][ , c("supply","utilizations") := NULL
  ]
  
  
  
  data_residual[, c("ElementCode","Value", "Flag") := NULL]
  
  data_residual[, ElementCode := c("5166")]
  setnames(data_residual, "imbalance","Value")
  data_residual[, Flag := c("I")]
  
  
  data_residual <- unique(data_residual)
  
  data_residual <- merge(data_residual,all_cpc, by= "CPCCode",all.x = TRUE)
  data_residual <- merge(data_residual,all_elements, by= "ElementCode",all.x = TRUE)
  
  data <- rbind(data, data_residual)
  
  
  
  ###########################################################################################################################################################
  
  data <- wide_format(data)
  
  
  #sort it by fbs tree 
  fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
  fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
  
  setnames(fbsTree, c("item_sua_fbs"),c("CPCCode"))
  
  fbsTree[, CPCCode := as.character(CPCCode)]
  
  data <- merge(data, fbsTree, by= c("CPCCode"), all.x = TRUE)
  
  data <-data[order(CPCCode, factor(ElementCode, levels = c("5113", "5510","5610","5910","5071", "5141","5023", "5525","5520","5016","5165","5166")))]
  
  
  data <-data[order(id4)]
  
  data[, id4 := NULL]
  
  
  validate(
    need(!is.null(data), "No data")
  )
  
  
  data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
  
  df_sua_unbalanced$data_sua_unbalanced <- data
  
  
  
  
}
  
  
observeEvent(input$saveCrop,{
  
  update_residual()
  
})

observeEvent(input$saveLivestock,{
  
  update_residual()
  
})


observeEvent( input$saveStock,{
  
  update_residual()
  
})


observeEvent( input$saveImports,{
  
  update_residual()
  
})


observeEvent( input$saveExports,{
  
  update_residual()
  
})




observeEvent( input$saveLoss,{
  
  update_residual()
  
})
observeEvent(input$saveFeed,{
  
  update_residual()
  
})

observeEvent( input$saveSeed,{
  
  update_residual()
  
})

observeEvent( input$saveIndustry,{
  
  update_residual()
  
})


observeEvent( input$saveFood,{
  
  update_residual()
  
})




output$sua_unbalanced <- 
  
  # my_vals = unique(df_sua_unbalanced$data_sua_unbalanced$ElementCode)
  # my_colors = ifelse(my_vals=='5166','orange','grey')
  
  renderDataTable(
    
  
    
    datatable (df_sua_unbalanced$data_sua_unbalanced, rownames= FALSE,class = 'cell-border stripe',
               
               # editable = list(target = "cell", disable = list(columns = c(0,1,2,3))), 
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 # paging = TRUE, searching = TRUE, info = FALSE,
                 # sort = TRUE,
                 scrollX = TRUE,
                 scrollY = "500px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(
                   # list(width = '200px', targets = c(3)),
                                   list(visible = FALSE, targets = (ncol(df_sua_unbalanced$data_sua_unbalanced)-1))) 
               ))  %>%
      
     
      formatStyle(0:ncol(df_sua_unbalanced$data_sua_unbalanced), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatStyle('ElementCode', target = "row", color = styleEqual(unique(df_sua_unbalanced$data_sua_unbalanced$ElementCode),
                                                                              ifelse(unique(df_sua_unbalanced$data_sua_unbalanced$ElementCode)=='5166','blue','black'))) %>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
  
    
    )  

output$download_sua_balanced<- downloadHandler(
  
  filename = function() {
    
    "SUA_Balanced_data.xlsx"
  },
  
  
  content = function(file) {
    
    data_download <- data.table(df_sua_balanced$data_sua_balanced)
    
    data_download <- data_download[!is.na(CPCCode)]
    
    data_download[,hidden := NULL]
    
    write.xlsx(data_download ,file,row.names = FALSE)
  }
  
)

output$download_nutri_table<- downloadHandler(
  
  filename = function() {
    
    "Nutrient Factors.xlsx"
  },
  
  
  content = function(file) {
    
    data_download <- data.table(df_nutrient$data_nutrient)
    

    
    write.xlsx(data_download ,file,row.names = FALSE)
  }
  
)





output$download_sua_unbalanced<- downloadHandler(
  
  filename = function() {
    
    "SUA_Unbalanced_data.xlsx"
  },
  
  
  content = function(file) {
    
    data_download <- data.table(df_sua_unbalanced$data_sua_unbalanced)
    
    data_download <- data_download[!is.na(CPCCode)]
    
    data_download[,hidden := NULL]
    
    write.xlsx(data_download ,file,row.names = FALSE)
  }
  
)

get_download_total_des <- function(by = 'level') {
  fbs_all_levels <- data.table(df_des$data_des)
  
  fbs_all_levels[,hidden := NULL]
  
  fbs_all_levels[,`FBS Code` := as.character(`FBS Code`)]
  fbs_all_levels[,ElementCode := as.character(ElementCode)]
  
  fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  # fbsTree[, `id1` := paste0("S",`id1`)]
  # fbsTree[, `id2` := paste0("S",`id2`)]
  # fbsTree[, `id3` := paste0("S",`id3`)]
  # fbsTree[, `id4` := paste0("S",`id4`)]
  
  fbs_last_level <- copy(fbs_all_levels)
  
  fbs_1_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id1)]
  fbs_2_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id2)]
  fbs_3_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id3)]
  fbs_4_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id4)]
  fbs_fish    <- fbs_last_level[`FBS Code` %in% c("2960")]
  
  des_commodity <- fread("SUA-FBS Balancing/Data/sua_balanced.csv")
  #des_commodity <- des_commodity[measuredElementSuaFbs %in% c("664","674","684","261","271","281","665")]
  # des_commodity <- des_commodity[timePointYears %in% c(as.numeric(input$startYear):as.numeric(input$endYear))]
  des_commodity[, c("geographicAreaM49","flagObservationStatus") := NULL]
  des_commodity <- data.table(des_commodity)
  setnames(des_commodity, names(des_commodity),c("ElementCode","CPCCode","Year","Value"))
  des_commodity <- merge(des_commodity,all_cpc, by = "CPCCode", all.x = TRUE)
  des_commodity[, ElementCode := as.character(ElementCode)]
  des_commodity <- merge(des_commodity,all_elements, by = "ElementCode", all.x = TRUE)
  
  setcolorder(des_commodity, c("CPCCode","Commodity","ElementCode","Element", "Year","Value"))
  
  
  
  # des_commodity[,Value := round(Value,0)]
  des_commodity =  dcast.data.table(des_commodity, CPCCode+Commodity+ElementCode+Element ~ Year, value.var = c("Value"))
  des_commodity <- des_commodity %>%
    filter(ElementCode %in% fbs_4_level$ElementCode) %>%
    arrange(CPCCode, Commodity, ElementCode)
  
 
  
  des_commodity <- des_commodity[order(`CPCCode`,factor(ElementCode, 
    levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
  
  require(openxlsx)
  wb <- createWorkbook()
  
  if(by == 'level') {
    df_list <- list(fbs_1_level=fbs_1_level, fbs_2_level=fbs_2_level,fbs_3_level=fbs_3_level,fbs_4_level=fbs_4_level,des_commodity=des_commodity, fish_seafood = fbs_fish)
    tab_names <- c('Level 1_Grand Total', 'Level 2_Anim.Veget.', 'Level 3_FBS Aggreg.', 'Level 4_FBS', 'Level 5_SUA', 'FBS Fish')
    
    for(i in 1:length(df_list)) {
      
      data <- df_list[[i]]
      name <- tab_names[i]
      addWorksheet(wb, name)
      
      # lowercase descriptions
      data[[2]] <- tolower(data[[2]])
      
      writeData(wb, name,data,
                startCol = 1,
                startRow = 1,
                colNames = TRUE)
      
      # comma separator for thousands
      year_columns <- 2:ncol(data)
      addStyle(wb,
               sheet = name,
               style = createStyle(
                 numFmt = '#,##0.00'
               ),
               rows = rep(1:nrow(data), length(year_columns)) + 1,
               cols = rep(year_columns, each = nrow(data))
      )
      
      # diving lines between groups
      # not for grand total
      if(i != 1) {
        new_element <- which(data[[2]] != c('', data[[2]][1:(nrow(data) - 1)]))
      
        addStyle(wb,
                 sheet = name,
                 style = createStyle(
                   borderStyle = 'thin',
                   border = 'Bottom'
                 ),
                 rows = rep(new_element, ncol(data)),
                 cols = rep(1:ncol(data), each = length(new_element)),
                 stack = TRUE
        )
      }
      
      
      # first four columns bold
      addStyle(wb,
               sheet = name,
               style = createStyle(
                 textDecoration = "bold"
               ),
               rows = rep(0:nrow(data), 4) + 1,
               cols = rep(1:4, each = nrow(data)+1),
               stack = TRUE
      )
      
      # highlight top row
      addStyle(wb,
               sheet = name,
               style = createStyle(
                 fgFill = "#99CCFF"
               ),
               rows = 1,
               cols = 1:ncol(data),
               stack = TRUE
      )
    }
  } else {
    for(year in as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))) {
      data_sheet = Prepare_data_des_report(
        year = year,
        fbs1 = fbs_1_level,
        fbs2 = fbs_2_level,
        fbs3 = fbs_3_level,
        fbs_tree = fbsTree,
        sua_balanced = df_sua_balanced_with_Nut$data_sua_balanced_with_nut,
        fbs_balanced = df_fbs_balanced$data_fbs_balanced,
        population_data = fread("SUA-FBS Balancing/Data/popSWS.csv"),
        elements = readxl::read_excel('data/elements.xlsx'),
        country = countryData$Country[1]
      )
      
      Add_FBS_year_sheet_formatted(wb, as.character(year), data_sheet)
    }
  }
  
  return(wb)
}


output$download_total_des_level <- downloadHandler(
  filename = function() {
    "Nutrients_by_level.xlsx"
  },
  content = function(filename) {
    
    show_modal_spinner(
      
      spin = "cube-grid",
      color = "firebrick",
      text = "Preparing data, please wait...",
      session = shiny::getDefaultReactiveDomain()
    )
    
    wb <- get_download_total_des(by = 'level')
    saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    
    remove_modal_spinner()
  }
)

output$download_total_des_year <- downloadHandler(
  filename = function() {
    "Nutrients_SUA_by_year.xlsx"
  },
  content = function(filename) {
    
    show_modal_spinner(
      
      spin = "cube-grid",
      color = "firebrick",
      text = "Preparing data, please wait...",
      session = shiny::getDefaultReactiveDomain()
    )
    
    wb <- get_download_total_des(by = 'year')
    saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    
    remove_modal_spinner()
  }
)







############################################ Tree Domain ##########################################################################################

observeEvent(input$startContinue, {
  
  
  data=fread("SUA-FBS Balancing/Data/tree.csv")
  
  
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  
  
  data=subset(data, timePointYears %in% t )
  
  
  
  
  tree=copy(data)
  
  tree[, c("geographicAreaM49","flagMethod") := NULL]
  
  
  
  tree=subset(tree, measuredElementSuaFbs %in% c("extractionRate") & timePointYears %in% as.character(t))
  
  
  tree[, c("measuredElementSuaFbs") := NULL ]
  
  setnames(tree, c("measuredItemParentCPC", "measuredItemChildCPC", "Value", "flagObservationStatus","timePointYears"), c("CPCCode Parent","CPCCode Child",
                                                                                                         "Extraction Rate","Flag","Year"))
  
  tree=merge(tree,all_cpc,by.x = "CPCCode Parent" , by.y = "CPCCode", all.x = TRUE)
  
  setnames(tree,"Commodity","Parent Commodity")
  
  tree=merge(tree,all_cpc,by.x = "CPCCode Child" , by.y = "CPCCode", all.x = TRUE)
  
  setnames(tree,"Commodity","Child Commodity")
  
  
  setcolorder(tree,c("CPCCode Parent","Parent Commodity","CPCCode Child","Child Commodity","Year","Extraction Rate","Flag"))
  
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
  
  
  df_tree$data_tree <- tree
 
})



proxy_tree= dataTableProxy('commodity_tree')

observeEvent(input$commodity_tree_cell_edit, {
  
  
  info = input$commodity_tree_cell_edit
  
  print(info)
  i = info$row
  j = (info$col + 1)
  v = info$value
  df_tree$data_tree[i,(j) := v]
  
  replaceData(proxy_tree, df_tree$data_tree, resetPaging = FALSE,rownames = FALSE)  # important
  
  
  
  
})


output$download_tree<- downloadHandler(
  
  filename = function() {
    
    "Commodity_tree.xlsx"
  },
  
  
  content = function(file) {
    
    data_download_tree <- data.table(df_tree$data_tree)
    
    
    write.xlsx(data_download_tree ,file,row.names = FALSE)
  }
  
)




observeEvent(input$treeSave,{
  
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  
  treeOriginal <- fread("SUA-FBS Balancing/Data/tree.csv")
  
  tree <- data.table(df_tree$data_tree)
  
  # write.csv(tree,"tree_test.csv" , row.names = F)
  
  tree[,c("Parent Commodity","Child Commodity") := NULL]
  
  setnames(tree, c("CPCCode Parent","CPCCode Child","Year"),c("measuredItemParentCPC","measuredItemChildCPC",
                                                              "timePointYears"))
  tree_to_save <- merge(treeOriginal,tree, by=c("measuredItemParentCPC","measuredItemChildCPC","timePointYears"), 
              all.x = T )
  
  tree_to_save[measuredElementSuaFbs == "extractionRate" & timePointYears %in% t,
      `:=` (Value = `Extraction Rate`,
            flagObservationStatus= Flag )]
  tree_to_save <- tree_to_save[,c("Extraction Rate","Flag") := NULL]
  
  write.csv(tree_to_save,"SUA-FBS Balancing/Data/tree.csv",row.names = F)
  
})

observeEvent(input$nutrientSave,{
  
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  
  nutrientOriginal <- fread("SUA-FBS Balancing/Data/nutrientData.csv")
  
  nutrient <- data.table(df_nutrient$data_nutrient)
  
  # write.csv(nutrient,"nutrient_test.csv" , row.names = F)
  
  nutrient[,c("Commodity","Element") := NULL]
  
  setnames(nutrient, c("CPCCode","ElementCode","Year", "Value"),c("measuredItemCPC","measuredElement",
                                                              "timePointYearsSP", "new_value"))
  
  nutrient[,measuredElement := as.character(measuredElement)]
  nutrientOriginal[,measuredElement := as.character(measuredElement)]
  
  nutrient_to_save <- merge(nutrientOriginal,nutrient, by=c("measuredItemCPC","measuredElement","timePointYearsSP"), 
                        all.x = T )
  
  nutrient_to_save[ timePointYearsSP  %in% t,
               `:=` (Value = new_value
                     )]
  nutrient_to_save <- nutrient_to_save[,c("new_value") := NULL]
  
  write.csv(nutrient_to_save,"SUA-FBS Balancing/Data/nutrientData.csv",row.names = F)
  
})

 
observeEvent(input$startContinue, {

  data=fread("SUA-FBS Balancing/Data/nutrientData.csv")

  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))


  data <- data[!duplicated(data)]


  # write.csv(data,"SUA-FBS Balancing/Data/nutrientData.csv",row.names = FALSE)



  data=subset(data, timePointYearsSP %in% t)
  

  data[,c("flagRatio", "geographicAreaM49"):=NULL]

  # data1001=dcast.data.table(data[measuredElement %in% 1001], measuredItemCPC ~ measuredElement, value.var = c("Value"))
  # data1003=dcast.data.table(data[measuredElement %in% 1003], measuredItemCPC ~ measuredElement, value.var = c("Value"))
  # data1005=dcast.data.table(data[measuredElement %in% 1005], measuredItemCPC ~ measuredElement, value.var = c("Value"))


  # data=merge(data1001, data1003, by="measuredItemCPC", all = TRUE)

  # data=merge(data, data1005, by="measuredItemCPC", all = TRUE)

  setnames(data,c("measuredItemCPC", "measuredElement"),c("CPCCode","ElementCode"))
  
  data[, ElementCode := as.character(ElementCode)]

  data=merge(data,all_cpc,by="CPCCode", all.x = TRUE)
  data=merge(data,all_elements,by="ElementCode", all.x = TRUE)

  setnames(data,"timePointYearsSP","Year")

  setcolorder(data,c("CPCCode","Commodity","ElementCode","Element","Year","Value"))

 df_nutrient$data_nutrient <- data
  



})





proxy_nutrient_factors = dataTableProxy('nutrient_factors')

observeEvent(input$nutrient_factors_cell_edit, {
  
  
  info = input$nutrient_factors_cell_edit
  
  print(info)
  i = info$row
  j = (info$col + 1)
  v = info$value
  df_nutrient$data_nutrient[i,(j) := v]
  
  replaceData(proxy_nutrient_factors, df_nutrient$data_nutrient, resetPaging = FALSE,rownames = FALSE)  # important
  
  
  
  
})








output$commodity_tree <- 
  renderDataTable(
    
    datatable (df_tree$data_tree, rownames= FALSE,class = 'cell-border stripe', 
               editable = list(target = "cell", disable = list(columns = c(1:4))),
               
               extensions = c("FixedHeader"),
               options = list(
                 pageLength = 25,
                 fixedHeader= T,
                 # dom='f', ordering=F,
                 # paging = TRUE, searching = TRUE, info = FALSE,
                 # sort = TRUE,
                 scrollX = TRUE,
                 scrollY = "500px" 
                 # autoWidth = T
                 # fixedColumns = list(leftColumns = 4),
                 # columnDefs = list(list(width = '400px', targets = c(3)),
                 #                   list(visible = FALSE, targets = (ncol(df_crop$data_crop)-1))
                 # ) 
               ))%>%
    
    
     DT::formatStyle(columns = names(data), color="red")
     
    
    # %>%
    #   
    #   formatStyle(0:ncol(df_crop$data_crop), valueColumns = "hidden",
    #               `border-bottom` = styleEqual(1, "solid 3px")) %>%
    #   formatCurrency(columns = format_years,currency = "", digits = 0,interval = 3, mark = ",")
    # }
    
  )



output$nutrient_factors <- 
  renderDataTable(
    
    
    
    datatable(df_nutrient$data_nutrient, rownames= FALSE,class = 'cell-border stripe',
              editable = list(target = "cell", disable = list(columns = c(1:4))), 
              options = list(
                
                columnDefs = list(list(width = '40px', targets = c(0,1,2,3,4,5)))
                                 
                
              ))
    
  

    
  )






#################################################################################################################################################


############################################# SUA Balanced Domain  ################################################################################




#Running the balancingplugin 

observeEvent(input$runPlugin,{ style <- isolate(input$style)


# withProgress(message = 'Compiling data...',detail = 'This may take a while...', style = style, value =0, {
#   
#   SUAFBS_t(input,output,session)
#   
# })


show_modal_spinner(
  
  spin = "cube-grid",
  color = "firebrick",
  text = "Please wait...",
  session = shiny::getDefaultReactiveDomain()
)
SUAFBS_t(input,output,session)
Sys.sleep(3)
remove_modal_spinner()

sendSweetAlert(
  session = session,
  title = c("The Balancing Plugin"),
  text = c("Completed"),
  type = "success"
)

})





suaBal_NU <- eventReactive(c(input$checkbox,input$gotosuaBalanced),{
  
  
  
  t=as.numeric(input$endyear)
  
  files_sua_balanced=list.files(path = "SUA-FBS Balancing/Data",pattern ="^sua_balanced")
  
  
  if (paste0("sua_balanced.csv") %in% files_sua_balanced){
    
    data <- fread("SUA-FBS Balancing/Data/sua_balanced.csv")
    
    data <- subset(data, timePointYears %in% c(2014:t))
    
    
    #Attach 2010-2013 SUA BAlanced data
    
    
    sua_bal_2010_2013 <- fread("SUA-FBS Balancing/Data/sua_bal_2010_2013.csv")
    
    sua_bal_2010_2013 <- sua_bal_2010_2013[timePointYears %in% c(2010:2013)]
    
    sua_bal_2010_2013[, flagMethod := NULL]
    
    data <- rbind(data,sua_bal_2010_2013)
    
    #remove variables where there exists only 5166 for a particular CPC
    
    remove_row <- unique(data[,c("measuredItemFbsSua","measuredElementSuaFbs"),with = F])
    
    duplicated_Row <- remove_row[duplicated(measuredItemFbsSua)]
    
    remove_row <- remove_row[!measuredItemFbsSua %in% unique(duplicated_Row$measuredItemFbsSua)]
    
    data <- subset(data,! measuredItemFbsSua %in% unique(remove_row$measuredItemFbsSua))
    
    #residual given by country will not appear in sua_balanced
    
    # data=subset(data, !measuredElementSuaFbs %in% 5166)
    
    
    #These data does not consist tourist if the year is before or equal to 2013
    
    data[, geographicAreaM49 := NULL]
    
    setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
             
             c("ElementCode","CPCCode","Year","Value","Flag"))
    
    data[, ElementCode := as.character(ElementCode)]
    data[, ElementCode := as.character(ElementCode)]
    
    data=merge(data, all_cpc, by= "CPCCode", all.x = TRUE)
    
    data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
    
    # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
    #tourist remove
    
    data <- data[!is.na(Commodity)]
    
    elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5141","664","665", "674","684","5166","5113"
               ,new_nutrient_element)
    
    data=subset(data, ElementCode %in% elemKeys)
    
    # data[,Value := round(Value,0)]
    
    data <- wide_format(data)
    
    fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")
    
    
    fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
    
    setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
    
    fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
    
    
    commodityName <- data.table(read_excel("Data/Reference File.xlsx", sheet = "SUA_Commodities"))
    
    fbsTree=merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)
    
    fbsTree[, fbsCode_S := NULL]
    
    setnames(fbsTree,"Commodity","FBS Commodity")
    
    # fbsTree <- fbsTree[,c("CPCCode" ,"FBSCode Lev4" ,"FBS Commodity Lev4")]
    
    # setnames(fbsTree, c("FBSCode Lev4", "FBS Commodity Lev4"),c("FBSCode","FBS Commodity"))
    
    data=merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)
    
    
    
    year_col <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    
    flag_col <- grep("^Flag", names(data), value = TRUE)
    
    addorder <- as.vector(rbind(year_col,flag_col))
    
    setcolorder(data,c("FBS Code", "FBS Commodity", "CPCCode","Commodity","ElementCode","Element",addorder))
    
    data[, `FBS Code` := as.character(`FBS Code`)]
    
    
    xx <- data[order(c(`FBS Code`), factor(ElementCode, levels = c("5113", "5510","5610","5910"
                                                                   ,"5071", "5141", "5525","5520","5016","5023","5165","5166","664","665","674","684",new_nutrient_element)))]
    
    
    data <- data[with(data, order(`FBS Code`, CPCCode,factor(ElementCode, levels = c("5113", "5510","5610","5910"
                                                                                     ,"5071", "5141", "5525","5520","5016","5023","5165","5166","664","665","674","684",new_nutrient_element)))), ]
    
    # data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
    
    df_sua_balanced_with_Nut$data_sua_balanced_with_nut <- data[!ElementCode %in% c("4030","4031","4023","4014","4008")] #remove ash ,water et
    
    
    if (input$checkbox == FALSE ){
      
      data <- data[!ElementCode %in% c("674","684",new_nutrient_element)]
      
      data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
    
      df_sua_balanced$data_sua_balanced <- data
    
    }else if (input$checkbox == TRUE) {
      
      df_sua_balanced$data_sua_balanced <- data
      
      data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
      
    }
    
  }
  
  
  else {
    
    sendSweetAlert(
      session = session,
      title = paste("Please Run the Balancing Plugin to create SUA Balanced of", t),
      # text = paste(elementNameMissing, collapse = ' , '),
      type = "warning"
    )
    
    
    
  }
    
    
    
  
  
  
  
  
})









output$sua_balanced <-
  
  
  
  renderDataTable({
    
    if (is.null(suaBal_NU())){
      
      validate(
        need(nrow(suaBal_NU())>0, "Please run the balancing Plugin")
      )
    }
    datatable (suaBal_NU(), rownames= FALSE,class = 'cell-border stripe',
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 # fixedHeader= TRUE,
                 scrollX = TRUE,
                 scrollY = "400px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 6),
                 columnDefs = list(
                   list(visible = FALSE, targets = (ncol(suaBal_NU())-1)))
               ))  %>%
      formatStyle(0:ncol(suaBal_NU()), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px"))%>%
      
      
      formatStyle('ElementCode', target = "row", color = styleEqual(unique(suaBal_NU()$ElementCode),
                                                                    ifelse(unique(suaBal_NU()$ElementCode)%in% c('5166','664','665'),'blue','black')))%>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    
    
  })







observeEvent(input$all_imp,{
  
  
  show_modal_spinner(
    
    spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
  )
  
  Sys.sleep(3)
  remove_modal_spinner()
  
#stock
  
  print("Start Stock Imputation")
  
  data_stock <- imputeStocksChanges(input,output,session)
  
  
  data_stock <- wide_format(data_stock)
  
  END_YEAR=input$endyear
  data_stock=visualize_data(data_stock,END_YEAR)
  data_stock[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  
  
  data_stock <- data_stock[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
  
  df_stock$data_stock <- data_stock
  
  
  
  data_to_save_stock <- copy(df_stock$data_stock)
  
  data_to_save_stock <- subset(data_to_save_stock, ElementCode %in% c("5071","5113"))
  
  
  data_to_save_stock[,hidden := NULL]
  
  save_to_database(data = data_to_save_stock,countryData,year_range = c(input$fromyear:input$endyear))
  
  new_saved_data_stock <- return_data_base(data_to_save_stock)
  
  df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_stock
  
  update_residual()

  print("End Stock Imputation")
  
#loss
  
  print("Start Loss Imputation")
  
  data_loss <- imputeLoss(input,output,session)
  
  
  data_loss <- wide_format(data_loss)
  
  END_YEAR=input$endyear
  data_loss=visualize_data(data_loss,END_YEAR)
  data_loss[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  
  df_loss$data_loss <- data_loss
  
  data_to_save_loss <- copy(df_loss$data_loss)
  
  data_to_save_loss <- df_loss$data_loss
  
  data_to_save_loss <- subset(data_to_save_loss, ElementCode == "5016")
  
  save_to_database(data = data_to_save_loss,countryData,year_range = c(input$fromyear:input$endyear))
  
  new_saved_data_loss <- return_data_base(data_to_save_loss)
  
  df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_loss
  
  update_residual()
  
  print("End Loss Imputation")
  
#feed
  
  print("Start Feed Imputation")
  
  data_feed <- imputeFeed(input,output,session)
  
  
  data_feed <- wide_format(data_feed)
  
  
  if ( is.null(data_feed) ){
    
    sendSweetAlert(
      session = session,
      title = c("Imputation Error"),
      text = c("No time series data to impute"),
      type = "warning"
    )
    
  } else {
    
    
    # df_stock$data_stock=wide_format(df_stock$data_stock)
    END_YEAR=input$endyear
    data_feed=visualize_data(data_feed,END_YEAR)
    data_feed[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_feed$data_feed <- data_feed
    
  }
  
  data_to_save_feed <- df_feed$data_feed
  
  data_to_save_feed <- subset(data_to_save_feed, ElementCode == "5520")
  
  save_to_database(data = data_to_save_feed,countryData,year_range = c(input$fromyear:input$endyear))
  
  new_saved_data_feed <- return_data_base(data_to_save_feed)
  
  df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_feed
  
  update_residual()
  
  print("End Feed Imputation")
  
#seed
  
  print("Start Seed Imputation")
  
  data_seed <- imputeSeed(input,output,session)
  
  
  data_seed <- wide_format(data_seed)
  
  
  
  if ( is.null(data_seed) ){
    
    sendSweetAlert(
      session = session,
      title = c("Imputation Error"),
      text = c("No time series data to impute"),
      type = "warning"
    )
    
  } else {
    
    
    # df_stock$data_stock=wide_format(df_stock$data_stock)
    END_YEAR=input$endyear
    data_seed=visualize_data(data_seed,END_YEAR)
    data_seed[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_seed$data_seed <- data_seed
    
  }
  
  data_to_save_seed <- df_seed$data_seed
  
  data_to_save_seed <- subset(data_to_save_seed, ElementCode == "5525")
  
  # data_to_save[,hidden := NULL]
  
  save_to_database(data = data_to_save_seed,countryData,year_range = c(input$fromyear:input$endyear))
  
  new_saved_data_seed <- return_data_base(data_to_save_seed)
  
  df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_seed
  
  update_residual()
  
  print("End Seed Imputation")
  
  
#food
  
  print("Start Food Imputation")
  
  data_food <- imputeFood(input,output,session)
  
  # 
  # sendSweetAlert(
  #   session = session,
  #   title = "Imputed !!",
  #   text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
  #   type = "success"
  # )
  # 
  
  
  data_food <- wide_format(data_food)
  
  
  
  if ( is.null(data_food) ){
    
    sendSweetAlert(
      session = session,
      title = c("Imputation Error"),
      text = c("No time series data to impute"),
      type = "warning"
    )
    
  } else {
    
    
    # df_stock$data_stock=wide_format(df_stock$data_stock)
    END_YEAR=input$endyear
    data_food=visualize_data(data_food,END_YEAR)
    data_food[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    df_food$data_food <- data_food
    
  }
  
  
  data_to_save_food <- df_food$data_food
  
  data_to_save_food <- subset(data_to_save_food, ElementCode == "5141")
  
  save_to_database(data = data_to_save_food,countryData,year_range = c(input$fromyear:input$endyear))
  
  new_saved_data_food <- return_data_base(data_to_save_food)
  
  df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_food
  
  update_residual()
  
  print("End Food Imputation")
  
  sendSweetAlert(
    session = session,
    title = c("Imputed!!"),
    text = c("Missing values were successfully imputed. Please refer to the manual for the applied methodology"),
    type = "success"
  )
  
 
  
})


}

Prepare_data_des_report <- function(year, fbs1, fbs2, fbs3, fbs_tree, 
                                    sua_balanced, fbs_balanced, population_data,
                                    elements, country) {
  
  year <- as.character(year)
  
  ### Data preparation for level 1, 2 and 3 of the Food balance sheet
  
  # List ID4 for each FBS group
  fbs_child <- rbind(
    fbs_tree %>%
      dplyr::select(id1, id4) %>%
      rename(child = id4, parent = id1) %>%
      distinct,
    fbs_tree %>%
      dplyr::select(id2, id4) %>%
      rename(child = id4, parent = id2) %>%
      distinct,
    fbs_tree %>%
      dplyr::select(id3, id4) %>%
      rename(child = id4, parent = id3) %>%
      distinct
  ) %>%
    mutate(child = as.character(child)) %>%
    filter(!is.na(child))
  
  # compute supply and utilization statistics for each FBSCode
  # by aggregating the child (ID4) elements
  # sua_by_fbs_code <- sua_balanced %>%
  #   rename(child = `FBS Code`) %>%
  #   left_join(fbs_child, by = 'child') %>%
  #   rename(value = sym(year)) %>%
  #   dplyr::select(parent, ElementCode, value) %>%
  #   mutate(value = replace_na(value, 0)) %>%
  #   group_by(parent, ElementCode) %>%
  #   summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") %>%
  #   ungroup() %>%
  #   rename(FBSCode = parent)
  
  sua_by_fbs_code <- df_fbs_balanced$data_fbs_balanced %>%
    mutate(FBSCode = as.numeric(str_replace(`FBS Code`, 'S', ''))) %>%
    rename(value = sym(year)) %>%
    filter(FBSCode %in% fbs_child$parent) %>%
    dplyr::select(FBSCode, ElementCode, value) %>%
    complete(FBSCode = unique(fbs_child$parent), 
             ElementCode = unique(fbs_balanced$ElementCode),
             fill = list(value = 0))
  
  # Extract the nutritional value of each FBSCode using the precalculated
  # fbs1, fbs2 and fbs4
  des_by_fbs_code <- rbind(
    fbs1 %>% 
      rename(FBSCode = `FBS Code`,
             value = sym(year)) %>%
      mutate(name = "Grand total",
             level = 1) %>%
      dplyr::select(FBSCode, name, level, ElementCode, value),
    fbs2 %>% 
      rename(FBSCode = `FBS Code`,
             value = sym(year),
             name = `FBS Group`) %>%
      mutate(level = 2) %>%
      dplyr::select(FBSCode,  name, level, ElementCode,value),
    fbs3 %>% 
      rename(FBSCode = `FBS Code`,
             value = sym(year),
             name = `FBS Group`) %>%
      mutate(level = 3) %>%
      dplyr::select(FBSCode, name, level, ElementCode, value)
  )
  
  name_fbs_code <- des_by_fbs_code %>%
    dplyr::select(FBSCode, name, level) %>%
    mutate(CPCCode = 0) %>%
    distinct
  
  des_by_fbs_code <- des_by_fbs_code %>% dplyr::select(-name, -level)
  
  
  ### Data preparation for the individual commodities 
  
  data_commodity <- sua_balanced %>% 
    rename(value = sym(year),
           child = `FBS Code`) %>%
    left_join(fbs_tree %>% 
                dplyr::select(id3, id4) %>%
                rename(child = id4,
                       FBSCode = id3) %>%
                mutate(child = as.character(child)),
              by = 'child') %>%
    left_join(all_cpc %>% dplyr::select(-Commodity), by = "CPCCode")  %>%
    dplyr::select(CPCCode, FBSCode, ElementCode, value, Commodity) %>%
    mutate(value = replace_na(value, 0))
  
  name_commodity <- data_commodity %>%
    dplyr::select(CPCCode, FBSCode, Commodity) %>%
    mutate(level = 5) %>%
    rename(name = Commodity) %>%
    distinct
  
  data_commodity <- data_commodity %>% dplyr::select(-Commodity)
  
  ## combine data
  elements$ElementCode = as.character(elements$ElementCode)
  combined <- rbind(sua_by_fbs_code %>%
                      mutate(CPCCode = 0),
                    des_by_fbs_code %>%
                      mutate(CPCCode = 0),
                    data_commodity) %>%
    group_by(FBSCode, ElementCode, CPCCode) %>%
    slice(1) %>%
    ungroup() %>%
    complete(nesting(FBSCode, CPCCode), ElementCode, fill = list(value = 0)) %>%
    left_join(elements, by = 'ElementCode') %>%
    filter(!(ElementType %in% c('other', 'des total'))) %>%
    left_join(rbind(
      name_commodity %>% 
        mutate(name = as.character(name),
               CPCCode = as.character(CPCCode)) %>%
        rename(commodity = name),
      name_fbs_code %>% 
        mutate(name = as.character(name)) %>%
        rename(commodity = name)
    ), by = c('CPCCode', 'FBSCode'))
  
  if(!all(combined$ElementCode %in% elements$ElementCode)) {
    unrecognized <- combined$ElementCode[!(combined$ElementCode %in% elements$ElementCode)] %>%
      unique
    cat("Warning! Unrecognized ElementCode(s): ", unrecognized, '\n')
    combined <- combined %>% filter(!(ElementCode %in% unrecognized))
  }
  
  # long to wide format
  data_table = combined %>%
    dplyr::select(CPCCode, FBSCode, level, commodity, value, ElementCode) %>%
    mutate(ElementCode = factor(ElementCode, elements$ElementCode)) %>%
    spread(key = "ElementCode", value = "value")
  
  # column names to be used in Excel
  index_element <- match(data_table %>% 
                           dplyr::select(commodity:last_col()) %>% 
                           dplyr::select(-commodity) %>% colnames,
                         elements$ElementCode)
  element_name <- elements$Name[index_element]
  element_unit <- elements$Unit[index_element]
  
  # number of cols used for supply, utilization and nutritional value
  used_elements <- elements %>% filter(ElementCode %in% colnames(data_table))
  
  num_cols <- c(sum(used_elements$ElementType == 'supply'),
                sum(used_elements$ElementType == 'utilization'),
                sum(used_elements$ElementType == 'des')) 
  
  # don't show supply & utilization numbers at total and animal/veget level
  data_table[data_table[['level']] %in% c(1, 2), 
             colnames(data_table) %in% (
               elements %>% filter(ElementType %in% c('supply', 'utilization')) %>% pull(ElementCode)
             )] <- NA
  
  population <- population_data %>%
    filter(timePointYears == !!year) %>%
    pull(Value) * 1000
  
  return(
    list(
      data_table = data_table,
      element_name = element_name,
      element_unit = element_unit,
      num_cols = num_cols,
      population = population,
      country = country
    )
    
  )
}
