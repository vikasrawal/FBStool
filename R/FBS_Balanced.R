

FBS_Balanced <- function(input,output,session){

  
############################################# FBS Balanced Domain  ################################################################################


  
observeEvent(input$fbs_balanced_button,{
  
  
  show_modal_spinner(
    
    spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
  )
  fbs_balanced_plugin(input,output,session)
  # fbs_balanced_plugin_new(input,output,session)
  Sys.sleep(6)
  remove_modal_spinner()
  
  
  
  
  
  
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  
  files_fbs_balanced=list.files(path = "SUA-FBS Balancing/FBS_Balanced/Data",pattern ="^fbs_balanced_final.csv")
  
  
  if (paste0("fbs_balanced_final.csv") %in% files_fbs_balanced){
    
    data=fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.csv")
    
    data <- subset(data, timePointYears %in% t)
    
    #binding data of 2010 to 2013 
    
    # data_2010 <- fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_2010_2013.csv")[,flagMethod := NULL]
    
    # data <- rbind(data,data_2010)
    
    data[, geographicAreaM49 := NULL]
    
    setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
             
             c("ElementCode","CPCCode","Year","Value","Flag"))
    
    data[, ElementCode := as.character(ElementCode)]
    data[, Year := as.character(Year)]
    
    data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
    
    commodityName <- data.table(read_excel("Data/Reference File.xlsx", sheet = "SUA_Commodities"))
    
    data <-merge(data,commodityName,by = "CPCCode",all.x = TRUE)
    
    # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
    #tourist remove
    
    data <- data[!is.na(Commodity)]
    
    
    elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5141","5166")
    
    data=subset(data, ElementCode %in% elemKeys)
    
    data[,Value := round(Value,0)]
    
    data <- data[!CPCCode %in% c("2910")]
    
    data <- wide_format(data)
    
    setnames(data,c("CPCCode","Commodity"), c("FBS Code","FBS Group"))
    
    data[, `FBS Code` := as.character(`FBS Code`)]
    
    data <-data[order(`FBS Code`, factor(ElementCode, levels = c("5510","5610","5910","5071", "5141", "5525","5520","5016","5023","5165","5166",
                                                              "664","665","674","684","261","271","281")))]
    
    data[, hidden := ifelse(`FBS Code` != shift(`FBS Code`, type = "lead"), 1, 0)] 
    
    df_fbs_balanced$data_fbs_balanced <- data
    
 
    
    newtab <- switch(input$fao,
                     "sua_balanced" = "fbs"
                     
    )
    updateTabItems(session, "fao", newtab)
    
  }else {
    
    sendSweetAlert(
      session = session,
      title = paste("Please Run the Balancing Plugin to create FBS Balanced of", t),
      # text = paste(elementNameMissing, collapse = ' , '),
      type = "warning"
    )
    
    
    
  }
  

  
})

  # output$download_fbs_balanced<- downloadHandler(
  #   
  #   filename = function() {
  #     
  #     "FBS_Balanced_data.xlsx"
  #   },
  #   
  #   
  #   content = function(file) {
  #     
  #     current_data <- data.table(df_fbs_balanced$data_fbs_balanced)
  #     
  #     # write.csv(current_data, "fbs_test.csv", row.names = FALSE)
  #     current_data[,hidden := NULL]
  #     
  #     flagcols <- grep("^Flag", names(current_data), value = TRUE)
  #     yearcols<- grep("^[[:digit:]]{4}$", names(current_data), value = TRUE)
  #     current_data[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
  #     current_data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  #     
  #     
  #     current_data_val <- melt.data.table(current_data, id.vars = c("FBS Code", "FBS Group","ElementCode" ,"Element"), measure.vars =grep("^[[:digit:]]{4}$", names(current_data), value = TRUE),
  #                                     value.name= "Value")
  #     setnames(current_data_val, "variable", "Year")
  #     current_data_val[,Year:= as.character(Year)]
  #     
  #     
  #     current_data_Flag= melt.data.table(current_data, id.vars = c( "FBS Code", "FBS Group","ElementCode" ,"Element"),
  #                                   measure.vars =grep("^Flag", names(current_data), value = TRUE),
  #                                   value.name= "Flag" )
  #     
  #     setnames(current_data_Flag,"variable", "Year")
  #     current_data_Flag[,Year:=substring(Year,6)]
  #     current_data_Flag[,Year:=as.character(Year)]
  #     
  #     current_data=merge(current_data_val, current_data_Flag, by = intersect(names(current_data_val), names(current_data_Flag)))
  #     
  #     
  #     
  #     
  #     data=fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.csv")
  #     
  #     frozen_year <- as.numeric(input$fromyear)-1
  #     
  #     data <- subset(data, timePointYears %in% 2014:frozen_year)
  #     
  #     data[, geographicAreaM49 := NULL]
  #     
  #     
  #     data_2010 <- fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_2010_2013.csv")[,flagMethod := NULL]
  #     
  #     data_2010[, geographicAreaM49 := NULL]
  #     data <- rbind(data,data_2010)
  #     
  #     data <- data[!duplicated(data[,c("measuredElementSuaFbs","timePointYears","measuredItemFbsSua")])]
  #     
  #     setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
  #              
  #              c("ElementCode","CPCCode","Year","Value","Flag"))
  #     
  #     setnames(current_data, c("FBS Code", "FBS Group"),c("CPCCode","Commodity"))
  #     
  #     current_data[,c("Commodity","Element"):=NULL]
  #     
  #     data <- rbind(data,current_data)
  #     
  #    
  #     data[, ElementCode := as.character(ElementCode)]
  #     data[, Year := as.character(Year)]
  #     
  #     data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
  #     
  #     commodityName <- data.table(read_excel("Data/Reference File.xlsx", sheet = "SUA_Commodities"))
  #     
  #     data <-merge(data,commodityName,by = "CPCCode",all.x = TRUE)
  #     
  #     # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
  #     #tourist remove
  #     
  #     data <- data[!is.na(Commodity)]
  #     
  #     
  #     elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5141","5166")
  #     
  #     data=subset(data, ElementCode %in% elemKeys)
  #     
  #     data[,Value := round(Value,0)]
  #     
  #     data <- data[!CPCCode %in% c("2910")]
  #     
  #     data <- data[!duplicated(data[,c("CPCCode","Year","ElementCode")])]
  #     
  #     data <- wide_format(data)
  #     
  #     setnames(data,c("CPCCode","Commodity"), c("FBS Code","FBS Group"))
  #     
  #     data[, `FBS Code` := as.character(`FBS Code`)]
  #     
  #     data <-data[order(`FBS Code`, factor(ElementCode, levels = c("5510","5610","5910","5071", "5141", "5525","5520","5016","5023","5165","5166",
  #                                                                  "664","665","674","684","261","271","281")))]
  #     
  #     write.xlsx(data ,file,row.names = FALSE)
  #   }
  #   
  # )
  
  


  
  output$download_fbs_balanced<- downloadHandler(
    
    # show_modal_spinner(
    #   
    #   spin = "cube-grid",
    #   color = "firebrick",
    #   text = "Please wait...",
    #   session = shiny::getDefaultReactiveDomain()
    # ),

    filename = function() {
      "FBS_Balanced_by_year.xlsx"
    },
    content = function(filename) {
      
      data_full <- fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.csv")
      replacements = data.frame(
        ele1  = unique(nutrientEle$ElementCode), 
        ele2 = as.numeric(unique(nutrientEle$measuredElement))
      )
      data_full <- merge(data_full,
                         replacements, by.x = "measuredElementSuaFbs", by.y = "ele2", all.x = TRUE)
      
      nutrient_data <- data_full %>%
        filter(!is.na(ele1)) %>%
        rename(`year` = timePointYears,
               `FBS Code` = measuredItemFbsSua,
               `ElementCode` = ele1,
               value = Value) %>%
        dplyr::select(`FBS Code`, ElementCode, year, value)
      
   
      
      require(openxlsx)
      wb <- createWorkbook()
      
      for(year in as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))) {
        
        data_sheet = Prepare_data_fbs_report(
          year = year,
          fbs_tree = fread("SUA-FBS Balancing/Data/fbsTree.csv"),
          fbs_balanced = df_fbs_balanced$data_fbs_balanced,
          fbs_balanced_final = fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.csv"),
          commodity_names = data.table(read_excel("Data/Reference File.xlsx", sheet = "SUA_Commodities")),
          nutrient_data = nutrient_data,
          population_data = fread("SUA-FBS Balancing/Data/popSWS.csv"),
          elements = readxl::read_excel('data/elements.xlsx'),
          country = countryData$Country[1]
        )
        
       
        
        Add_FBS_year_sheet_formatted2(wb, as.character(year), data_sheet)

      }
      
      saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    }
  )
  
  Prepare_data_fbs_report <- function(year, fbs_tree, fbs_balanced, fbs_balanced_final,
                                      commodity_names, nutrient_data, population_data,
                                      elements, country) {
    
    year <- as.character(year)
    
    # Add proteins and fat manually
    # to be removed once these elements are added to nutrients
    protein_fat <- fbs_balanced_final %>%
      filter(timePointYears == as.numeric(year),
             measuredElementSuaFbs %in% c(664, 674,684),
             measuredItemFbsSua %in% fbs_balanced$`FBS Code`) %>%
      rename(`FBS Code` = measuredItemFbsSua,
             ElementCode = measuredElementSuaFbs,
             value = Value) %>%
    dplyr::select(`FBS Code`, ElementCode, value) 
      
    
    ### Data preparation 
    elements$ElementCode = as.character(elements$ElementCode)
    
    data <- rbind(
      fbs_balanced %>% 
        rename(value = sym(year)) %>%
        dplyr::select(`FBS Code`, ElementCode, value),
      nutrient_data %>% 
        filter(year == !!year) %>%
        dplyr::select(-year),
      protein_fat
    ) %>%
      left_join(
        fbs_tree %>%
          mutate(id3 = paste0('S', id3),
                 id4 = paste0('S', id4)) %>%
          dplyr::select(id3, id4) %>%
          rename(`FBS Code` = id4,
                 parent = id3) %>%
          distinct,
        by = "FBS Code"
      ) %>%
      mutate(level = ifelse(is.na(parent), 3, 4)) %>%
      complete(nesting(`FBS Code`, `parent`, `level`), ElementCode, fill = list(value = 0)) %>%
      left_join(
        commodity_names %>%
          rename(`FBS Code` = CPCCode,
                 `FBSGroup` = Commodity),
        by = 'FBS Code'
      ) %>%
      left_join(elements, by = 'ElementCode') %>%
      filter(!(ElementType %in% c('other', 'des total'))) %>%
      filter(include_fbs == 1) %>%
      dplyr::select(-include_fbs)
    
    data$level[data$`FBS Code` %in% paste0('S', fbs_tree$id2)] <- 2
    
    total <- data %>%
      filter(level == 2) %>%
      group_by(ElementCode) %>%
      summarise(`FBS Code` = 2901,
                parent = NA,
                level = 1,
                value = sum(value),
                FBSGroup = "Grand Total",
                ElementType = ElementType[1],
                Unit = Unit[1],
                Name = Name[1])
    
    data <- rbind(data,
                  total)
    
    ## Add element names
    if(!all(data$ElementCode %in% elements$ElementCode)) {
      unrecognized <- data$ElementCode[!(data$ElementCode %in% elements$ElementCode)] %>%
        unique
      cat("Warning! Unrecognized ElementCode(s): ", unrecognized, '\n')
      data <- data %>% filter(!(ElementCode %in% unrecognized))
    }
    
    # long to wide format
    data_table = data %>%
      dplyr::select(`FBS Code`, parent, level, FBSGroup, value, ElementCode) %>%
      mutate(ElementCode = factor(ElementCode, elements$ElementCode)) %>%
      spread(key = "ElementCode", value = "value")
    
    # column names to be used in Excel
    index_element <- match(data_table %>% 
                             dplyr::select(FBSGroup :last_col()) %>% 
                             dplyr::select(-FBSGroup ) %>% colnames,
                           elements$ElementCode)
    element_name <- elements$Name[index_element]
    element_unit <- elements$Unit[index_element]
    
    # number of cols used for supply, utilization and nutritional value
    used_elements <- elements %>% filter(ElementCode %in% colnames(data_table))
    
    num_cols <- c(sum(used_elements$ElementType == 'supply'),
                  sum(used_elements$ElementType == 'utilization'),
                  sum(used_elements$ElementType == 'des')) 
    
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

  
output$fbs_balanced <-


  renderDataTable({
    
    if (is.null(df_fbs_balanced$data_fbs_balanced)){
      
      validate(
        need(nrow(df_fbs_balanced$data_fbs_balanced)>0, "Please run the FBS standardization plugin")
      )
    }
    

    datatable (df_fbs_balanced$data_fbs_balanced, rownames= FALSE,class = 'cell-border stripe',
               extensions = c("FixedColumns","FixedHeader", "Buttons"),
               options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 # fixedHeader= TRUE,
                 scrollX = TRUE,
                 scrollY = "500px" ,
                 # autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(
                   list(visible = FALSE, targets = (ncol(df_fbs_balanced$data_fbs_balanced)-1)))
               ))  %>%
      formatStyle(0:ncol(df_fbs_balanced$data_fbs_balanced), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
    
      
      formatStyle('ElementCode', target = "row", color = styleEqual(unique(df_fbs_balanced$data_fbs_balanced$ElementCode),
                                                                    ifelse(unique(df_fbs_balanced$data_fbs_balanced$ElementCode)%in% c('5166','664','665','674','684',
                                                                                                                                       '261','271','281'),'blue','black')))%>%
      formatCurrency(columns = as.character(c(input$fromyear:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")


  })



observeEvent(input$total_DES,{
  
  
  
  t= as.numeric(input$endyear)
  
  
  files_fbs_balanced=list.files(path = "SUA-FBS Balancing/FBS_Balanced/Data",pattern ="^fbs_balanced_final.csv")
  
  if (paste0("fbs_balanced_final.csv") %in% files_fbs_balanced){
    
    
    
    data=fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.csv")
    
    
#####fishery calories must be binned here      ##new
    
  fish <- data.table(read_excel("Data/fish.xlsx"))
  
  setnames(fish,c("Item Code (CPC)","Element Code","Year","Flag"),c("measuredItemFbsSua", "measuredElementSuaFbs","timePointYears", "flagObservationStatus"))
    
  fish[,geographicAreaM49 := unique(data$geographicAreaM49)]  
  
  data <- rbind(data,fish)
  
#########################################################   ## new 
    data <- subset(data, timePointYears %in% c(2014:t)) #new
    
    data[, geographicAreaM49 := NULL]
    
    setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
             
             c("ElementCode","CPCCode","Year","Value","Flag"))
    
    data[, ElementCode := as.character(ElementCode)]
    data[, Year := as.character(Year)]
    
    ### before merging nutrient factors  
    
    replacements = data.table(
      ele1  = unique(nutrientEle$ElementCode), 
      ele2 = unique(nutrientEle$measuredElement)
    )
    
  
    data <- merge(data,replacements, by.x = "ElementCode", by.y = "ele2", all.x = TRUE)
    
    
    data[, ElementCode := ifelse(!is.na(ele1),  ele1, ElementCode)]
    
    data[, ele1 := NULL]
    
    #request by Rachele to remove Ash, water , alcohol and Nianacin, food quantity
    data <- data[!ElementCode %in% c("4031","4030","4008","4023", "4014", "4001", "665")]
    ########################################
    data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
    
    commodityName <- data.table(read_excel("Data/Reference File.xlsx", sheet = "SUA_Commodities"))
    
    data <-merge(data,commodityName,by = "CPCCode",all.x = TRUE)
    
    # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
    #tourist remove
    
    data <- data[!is.na(Commodity)]
    
    
    # elemKeys=c("664", "665","674","684","261","271","281",new_nutrient_element)
    
    elemKeys=c("664","674","684",new_nutrient_element) # removed 261,271, 281,665
    
    data=subset(data, ElementCode %in% elemKeys)
    
##################### new ############### fishery inclusion ####################### ## new 
    
##2901 grand Total (Including Fishery)
##2902 grand total (excluding Fishery)
    
tempdata <- data[CPCCode %in% c("S2901", "S2960") & ElementCode %in% c("664","674","684",new_nutrient_element)][,c("CPCCode","ElementCode","Year","Value"), with = F] 

tempdata[, new := sum(Value[CPCCode %in% "S2901"], Value[CPCCode %in% "S2960"],na.rm = TRUE),.(Year,ElementCode)]

tempdata <- unique(tempdata[,c("ElementCode","Year","new"),with =F])

tempdata[, CPCCode := "S2901"]

setnames(tempdata,"new","Value")

tempdata[,Flag := "I"]

tempdata[, Commodity := "Grand Total (incl. Fish, Seafood)"]

tempdata <- merge(tempdata,all_elements, by = "ElementCode", all.x = TRUE)

## 2901 must be converted to 2902 as grand total (excluding fishery) but only for calories , fats and proteins 664, 674, 684 
#(2901 code will be there for 261,271,281)

data[CPCCode == "S2901" & ElementCode %in% c("664","674","684"),  ':=' (CPCCode ="S2902", Commodity = "Grand Total (excl. Fish, Seafood)")]
    
###################################################################################    
    
    data <- rbind(data,tempdata)


###add new nutrient at total level

NutriTotData <- copy(data)[CPCCode %in% c("S2903","S2941") & ElementCode %in% new_nutrient_element]

NutriTotData <- aggregate(
  Value ~ ElementCode+Year, NutriTotData,
   sum, na.rm = TRUE)

NutriTotData <- data.table(NutriTotData)

NutriTotData[, Flag := "I"]

NutriTotData[, CPCCode := "S2901"]

NutriTotData[,Commodity := "GRAND TOTAL - DEMAND"]

NutriTotData <- merge(NutriTotData, all_elements, by = "ElementCode", all.x = TRUE)

data <- rbind(data,NutriTotData)

    # data[,Value := round(Value,0)]
    
    
    
    # data <- wide_format(data)
    
    setnames(data,c("CPCCode","Commodity"), c("FBS Code","FBS Group"))
    
    data[, `FBS Code` := as.character(`FBS Code`)]
    
    data[, Flag := NULL]
    
    data[,`FBS Code` := sub('.', '', `FBS Code`)]
    
    
    data =  dcast.data.table(data, `FBS Code`+`FBS Group`+ElementCode+Element ~ Year, value.var = c("Value"))
    
    # data <- data[ElementCode %in% "664"]
    
    
    
    fbsTree= fread("SUA-FBS Balancing/Data/fbsTree.csv")
    
    
    
    ##new
    
    #spliiting the rows  ## this is including fish 
    
    rbind1= data[`FBS Code`== "2901"]
    
    rbind1 <- rbind1[order(`FBS Code`, factor(ElementCode, levels = c("664","665", "674","684","261","271","281",new_nutrient_element)))]
    
    
    ##new total excluding fish
    
    rbind_ex_fish= data[`FBS Code`== "2902"]
    
    rbind_ex_fish <- rbind_ex_fish[order(`FBS Code`, factor(ElementCode, levels = c("664", "674","684",new_nutrient_element)))]
    
    #fish  #new
    
    rbind_fish = data[`FBS Code`== "2960"]
    
    rbind_fish <- rbind_fish[order(`FBS Code`, factor(ElementCode, levels = c("664", "674","684")))]
    
    
    
##########################new###########################    
    
    #Vegetable products
    
    rbind2=data[`FBS Code`== "2903"]
    
    rbind2 <- rbind2[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
    
    
    
    DT_vegetables <- copy(data)
    
    DT_vegetables<-DT_vegetables[0,]
    
    
    
    
    for (i in sort(unique(fbsTree[id2== "2903"]$id3),decreasing = FALSE)){
      
      
      level_3 = data[`FBS Code` == i]
      level_3 <- level_3[order(`FBS Code`, factor(ElementCode, levels = c("664","665", "674","684","261","271","281",new_nutrient_element)))]
      
      level_4= data[`FBS Code` %in%  unique( fbsTree[id3 == i ]$id4)]
      level_4 <- level_4[order(`FBS Code`, factor(ElementCode, levels = c("664", "665","674","684","261","271","281",new_nutrient_element)))]
      
      
      DT_vegetables=rbind(DT_vegetables,level_3,level_4)
      
      
    }
    
    
    
    #Animal Products
    
    rbind3=data[`FBS Code`== "2941"]
    
    rbind3 <- rbind3[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
    
    DT_animalproducts<- copy(data)
    
    DT_animalproducts <- DT_animalproducts[0,]
    
    
    for (i in sort(unique(fbsTree[id2== "2941"]$id3),decreasing = FALSE)){
      
      
      level_3_animal = data[`FBS Code`== i]
      level_3_animal <- level_3_animal[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
      
      level_4_animal= data[`FBS Code` %in%  unique( fbsTree[id3 == i ]$id4)]
      level_4_animal <- level_4_animal[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
      
      
      DT_animalproducts=rbind(DT_animalproducts,level_3_animal,level_4_animal)
    }
    
    #new
    DESCPC_final=rbind(rbind1,rbind_ex_fish,rbind_fish,rbind2,DT_vegetables,rbind3, DT_animalproducts)
    
    
    DESCPC_final[, hidden := ifelse(`FBS Code` != shift(`FBS Code`, type = "lead"), 1, 0)] 
    
    df_des$data_des <- DESCPC_final
    
    
    
  }else {
    
    sendSweetAlert(
      session = session,
      title = paste("Please Run the Balancing Plugin to create Total Calories of", t),
      # text = paste(elementNameMissing, collapse = ' , '),
      type = "warning"
    )
    
  }
})






# observeEvent(input$total_DES,{
# 
# 
# 
#   t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
# 
# 
#   files_fbs_balanced=list.files(path = "SUA-FBS Balancing/FBS_Balanced/Data",pattern ="^fbs_balanced_final.csv")
# 
#   if (paste0("fbs_balanced_final.csv") %in% files_fbs_balanced){
# 
# 
# 
#     data=fread("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.csv")
# 
#     data <- subset(data, timePointYears %in% t)
# 
#     data[, geographicAreaM49 := NULL]
# 
#     setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
# 
#              c("ElementCode","CPCCode","Year","Value","Flag"))
# 
#     data[, ElementCode := as.character(ElementCode)]
#     data[, Year := as.character(Year)]
# 
#     data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
# 
#     commodityName <- data.table(read_excel("Data/Reference File.xlsx", sheet = "SUA_Commodities"))
# 
#     data <-merge(data,commodityName,by = "CPCCode",all.x = TRUE)
# 
#     # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
#     #tourist remove
# 
#     data <- data[!is.na(Commodity)]
# 
# 
#     elemKeys=c("664", "665","674","684","261","271","281")
# 
#     data=subset(data, ElementCode %in% elemKeys)
# 
#     data[,Value := round(Value,0)]
# 
# 
# 
#     # data <- wide_format(data)
# 
#     setnames(data,c("CPCCode","Commodity"), c("FBS Code","FBS Group"))
# 
#     data[, `FBS Code` := as.character(`FBS Code`)]
# 
#     total <- data[`FBS Code` %in% "S2901"]
#     total_664 <- total[ ElementCode == "664"]
#     total <- total[ !ElementCode == "664"]
# 
#     total <- rbind(total_664,total)
# 
#     data <- data[!`FBS Code` %in% "S2901"]
# 
#     data <- rbind(total,data)
# 
#     # data <-data[order(`FBS Code`, factor(ElementCode, levels = c(
#     #                                                              "664","665","674","684","261","271","281")))]
# 
#     setcolorder(data, c("FBS Code","FBS Group","ElementCode", "Element", "Year", "Value" ))
# 
#     data[, hidden := ifelse(ElementCode != shift(ElementCode, type = "lead"), 1, 0)]
# 
#     df_des$data_des <- data
# 
# 
# 
#   }else {
# 
#     sendSweetAlert(
#       session = session,
#       title = paste("Please Run the Balancing Plugin to create Total Calories of", t),
#       # text = paste(elementNameMissing, collapse = ' , '),
#       type = "warning"
#     )
# 
# }
# })


# hide/show filters table des
observe({
  
  des_filter <- input$select_des_data_filter 
  
  if(des_filter == 'Year') {
    
    hide(id = 'select_des_data_nutrient', time = 0)
    show(id = 'select_des_data_year', time = 0)
    
  } else { # Nutrient
    
    show(id = 'select_des_data_nutrient', time = 0)
    hide(id = 'select_des_data_year', time = 0)
    
  }
  
})

# update select year/nutrient when data des is loaded
observe({
  
  data <- df_des$data_des
  
  if(is.null(data)) {
    return()
  }
  
  # set available years
  cols <- colnames(data)
  year_col <- !grepl("\\D", cols)
  years <- cols[year_col]
  updateSelectInput(session, 'select_des_data_year', choices = years)
  
  # set available nutrients
  nutrients <- unique(data$Element)
  updateSelectInput(session, 'select_des_data_nutrient', choices = nutrients)
})


des_table_data <- reactive({
  
  data <- df_des$data_des
  
  if(is.null(data)) {
    return(NULL)
  }
  
  des_filter <- input$select_des_data_filter 
  
  
  if(des_filter == 'Year') {
    filter_year <- input$select_des_data_year
    
    if(filter_year == "") {
      return(NULL)
    }
    
    elements <- readxl::read_excel('data/elements.xlsx')
    
    
    return(
      data %>%
        left_join(elements %>% 
                    mutate(label = paste0(Name, " (", Unit, ")")) %>%
                    dplyr::select(ElementCode, label) %>%
                    mutate(ElementCode = as.character(ElementCode)),
                  by = 'ElementCode') %>%
        dplyr::select("FBS Code", "FBS Group", "label", as.character(filter_year)) %>%
        spread(
          value = as.character(filter_year),
          key = 'label'
        )
    )
  } else {
    filter_nutrient <- input$select_des_data_nutrient
    return(
      data %>%
        filter(Element == filter_nutrient) %>%
        dplyr::select(-c(Element, ElementCode, hidden))
    )
  }
  
})

output$total_des <-
  
  
  renderDataTable({
    
    data = des_table_data()
    
    if (is.null(data)){
      
      validate(
        need(nrow(data)>0, "Please run the balancing plugin")
      )
    }
    
    dt <- datatable (data, rownames= FALSE,
               class = 'cell-border stripe',
               extensions = c("FixedColumns","FixedHeader"),
               options = list(
                 pageLength = 25,
                 fixedHeader= FALSE,
                 scrollX = TRUE,
                 scrollY = "500px"
               )) 
    
    nutrient_cols <- colnames(data)
    nutrient_cols <- nutrient_cols[!(nutrient_cols %in% c('FBS Code', 'FBS Group', 'ElementCode', 'Element', 'hidden'))]
    
    
    dt <- dt %>% 
      formatCurrency(columns = nutrient_cols, currency = "", digits = 2, interval = 3, mark = ",")
    
    
  })





}
