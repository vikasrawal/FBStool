
shinyServer(function(input, output, session) {
  
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  reactive({
    
    updateSelectInput(session, input$countrym49, c(country_selc)) 
  })
  

  
  ###################################################### Tree and Nutrient #######################################################################
  ################################################################################################################################################
  
  observeEvent(input$startContinue,{
    
    t=as.character(input$endyear)
    
    
    
    #after validating the tree write to the folder
    
    tree <- fread("SUA-FBS Balancing/Data/tree.csv")
    
    years_missing = c()
    
    treeToadd = list()
    
    for (i in c(2010:t)){
      
      
      condition <- i %in% unique(tree$timePointYears)
      
      if (condition == FALSE){
        
        years_missing<- c(years_missing,i)
        
      }
      
    }
    
    
    
    if (length(years_missing) != 0){
      
      for (i in 1:length(years_missing)){
        
        
        tree_new <- tree[timePointYears == as.numeric(input$endyear) - 1]
        
        treeToadd_ <- copy(tree_new)[,timePointYears := years_missing[i]]
        
        treeToadd[[i]] <- treeToadd_
        
      }
      
      
    }
    
    treeToadd <- rbindlist(treeToadd)
    
    
    tree <- rbind(tree,treeToadd)
    
    
    tree[!duplicated(tree)]
    
    
    write.csv(tree,"SUA-FBS Balancing/Data/tree.csv",row.names = FALSE)
    
    
    ##################Nutrient Data ################################
    
    
    
    nutrientData <- fread("SUA-FBS Balancing/Data/nutrientData.csv")
    
    years_missing_nutrient = c()
    
    nutrientToadd = list()
    
    for (i in c(2010:t)){
      
      
      condition_nutrient <- i %in% unique(nutrientData$timePointYears)
      
      if (condition_nutrient == FALSE){
        
        years_missing_nutrient<- c(years_missing_nutrient,i)
        
      }
      
    }
    
    
    
    if (length(years_missing_nutrient) != 0){
      
      for (i in 1:length(years_missing_nutrient)){
        
        
        nutrient_new<- nutrientData[timePointYearsSP == as.numeric(input$endyear)-1]
        
        nutrientToadd_ <- copy(nutrient_new)[,timePointYearsSP := years_missing_nutrient[i]]
        
        nutrientToadd[[i]] <- nutrientToadd_
        
      }
      
      
    }
    
    nutrientToadd <- rbindlist(nutrientToadd)
    
    
    nutrientData <- rbind(nutrientData,nutrientToadd)
    
    
    nutrientData[!duplicated(nutrientData)]
    
    
    write.csv(nutrientData,"SUA-FBS Balancing/Data/nutrientData.csv",row.names = FALSE)
    
    
    
    
  })
  
  
  ###############################################################################################################################################
  ################################################################################################################################################
  
  
  
  
  
  
  
#sua unbalanced
  
  df_sua_unbalanced <<- reactiveValues(data_sua_unbalanced =NULL)
  
  df_sua_balanced <<- reactiveValues(data_sua_balanced =NULL)
  
  df_sua_balanced_with_Nut <<- reactiveValues(data_sua_balanced_with_nut =NULL)
  
  df_des <<- reactiveValues(data_des =NULL)
  
 
#fbs balanced  
  
  df_fbs_stand <<- reactiveValues(data_fbs_stand =NULL)
  
  df_fbs_balanced <<- reactiveValues(data_fbs_balanced =NULL)
  

  #fbs balanced  
  
  df_fbs_stand <<- reactiveValues(data_fbs_stand =NULL)
  
  df_fbs_balanced <<- reactiveValues(data_fbs_balanced =NULL)
  
  
  #fbs report 
  
  df_fbs_report <<- reactiveValues(data_fbs_report =NULL)
  
  
  

  
  
#crop reactive values   
  
  value <<- reactiveValues(data_crop =NULL)
  df_cropCountry <<- reactiveValues(data_cropCountry =NULL)
  valuesxxx <<-  reactiveValues(test = 'initial')
  observeEvent(input$cropInsert,  {valuesxxx$test = 'add'})
  
  #########
  value <<- reactiveValues(countrym49 =NULL)
  
  
  
#livestock reactive values   
  
  value <<- reactiveValues(data_livestock =NULL)
  df_livestockCountry <<- reactiveValues(data_livestockCountry =NULL)
  valuesxxx <<-  reactiveValues(livestock_button = 'initial')
  # observeEvent(input$livestockInsert,  {valuesxxx$livestock_button = 'add'})
  
  
  
#Imports
  df_imports <<- reactiveValues(data_imports =NULL)
  df_importsCountry <<- reactiveValues(data_importsCountry =NULL)
  valuesxxx <<-  reactiveValues(imports_button = 'initial')
  values_importsCommodities <<- reactiveValues(importsCommodities = NULL)

  df_imports_not_mapped <<- reactiveValues(data_imports_not_mapped = NULL)
  df_exports_not_mapped <<- reactiveValues(data_exports_not_mapped = NULL)  
    
#Exports
  df_exports <<- reactiveValues(data_exports =NULL)
  df_exportsCountry <<- reactiveValues(data_exportsCountry =NULL)
  valuesxxx <<-  reactiveValues(exports_button = 'initial')
  values_exportsCommodities <<- reactiveValues(exportsCommodities = NULL)
  
  
  
#stock data 
  
  
df_stock <<- reactiveValues(data_stock =NULL)
df_stockCountry <<- reactiveValues(data_stockCountry =NULL)
valuesxxx <<-  reactiveValues(stock_button = 'initial')
values_stockCommodities <<- reactiveValues(stocksCommodities = NULL)




#loss data


df_loss_ratio <<- reactiveValues(data_loss_ratio = NULL)
values_lossratioCommodities <<- reactiveValues(lossratioCommodities = NULL)
df_loss_ratioCountry <<- reactiveValues(data_loss_ratioCountry =NULL)


df_loss <<- reactiveValues(data_loss =NULL)
df_lossCountry <<- reactiveValues(data_lossCountry =NULL)
valuesxxx <<-  reactiveValues(loss_button = 'initial')
values_lossCommodities <<- reactiveValues(lossCommodities = NULL)




#feed data

df_feed_ratio <<- reactiveValues(data_feed_ratio = NULL)
values_feedratioCommodities <<- reactiveValues(feedratioCommodities = NULL)
df_feed_ratioCountry <<- reactiveValues(data_feed_ratioCountry =NULL)


df_feed <<- reactiveValues(data_feed =NULL)
df_feedCountry <<- reactiveValues(data_feedCountry =NULL)
valuesxxx <<-  reactiveValues(feed_button = 'initial')
values_feedCommodities <<- reactiveValues(feedCommodities = NULL)
  
  
#seed data


df_seed_ratio <<- reactiveValues(data_seed_ratio = NULL)
values_seedratioCommodities <<- reactiveValues(seedratioCommodities = NULL)
df_seed_ratioCountry <<- reactiveValues(data_seed_ratioCountry =NULL)


df_seed <<- reactiveValues(data_seed =NULL)
df_seedCountry <<- reactiveValues(data_seedCountry =NULL)
valuesxxx <<-  reactiveValues(seed_button = 'initial')
values_seedCommodities <<- reactiveValues(seedCommodities = NULL)


# Industry data

df_industry <<- reactiveValues(data_industry =NULL)
df_industryCountry <<- reactiveValues(data_industryCountry =NULL)
valuesxxx <<-  reactiveValues(industry_button = 'initial')
values_industryCommodities <<- reactiveValues(industryCommodities = NULL)


#Food data

df_gdp <<- reactiveValues(data_gdp =NULL)
df_pop <<- reactiveValues(data_pop =NULL)
df_fdm <<- reactiveValues(data_fdm =NULL)
df_classification <<- reactiveValues(data_classification =NULL)


df_food <<- reactiveValues(data_food =NULL)
df_foodCountry <<- reactiveValues(data_foodCountry =NULL)
valuesxxx <<-  reactiveValues(food_button = 'initial')
values_foodCommodities <<- reactiveValues(foodCommodities = NULL)


#fish data 

df_fish <<- reactiveValues(data_fish =NULL) #new

df_gift <<- reactiveValues(data_gift =NULL) #new

############################################

  df_tree <<- reactiveValues(data_tree =NULL)
  
  df_nutrient <<-  reactiveValues(data_nutrient =NULL)
  

  
 

#domain scripts 
  

  crop_production <- crop_production(input,output,session)
  
  livestock_production <- livestock_production(input,output,session)
  # 
  trade_imports <- trade_imports(input,output,session)

  trade_exports <- trade_exports(input,output,session)
  
  stock_change <- stock_change(input,output,session)

  loss <- loss(input,output,session)
  
  feed <- feed(input,output,session)
  
  seed <- seed(input,output,session)
  
  industry <- industry(input,output,session)
 
  food <- food(input,output,session)
  
  fish<- fish(input,output,session) #new
  
  SUA_Unbalanced <- SUA_Unbalanced(input,output,session)
  
  FBS_Balanced <- FBS_Balanced(input,output,session)
  
  fbs_report <- fbs_report(input,output,session)
  
  reference_tables <- reference_tables(input,output,session)
  
  generate_self_sufficiency_tab(input,output,session)
  
  GIFT <- GIFT(input,output,session)
  
  
  
 
  observeEvent(input$startContinue,{
    
    
    newtab <- switch(input$fao,
                     "Start" = "production"
                    
                     
    )
    updateTabItems(session, "fao", newtab)
    
    
    
  })
  
  
  observeEvent(input$gotosuaBalanced,{


    newtab <- switch(input$fao,
                     "sua_unbalanced" = "sua_balanced"

    )
    updateTabItems(session, "fao", newtab)



  })
  
  
  observeEvent(input$fbs_balanced_button,{


    #newtab <- switch(input$fao,
    #                  "fbs_tab"="fbs"
    #
    #)
    #updateTabItems(session, "fao", newtab)
    session$sendCustomMessage("set_active_tab", c(11, "#shiny-tab-fbs"))


  })
  
  observeEvent(input$testswitch, {
    session$sendCustomMessage("set_active_tab", c(9, "#shiny-tab-sua_unbalanced"))
  })
  
  
  
  observeEvent(input$total_DES,{
    
    
    # newtab <- switch(input$fao,
    #                  "fbs" = "total_des"
    #                  
    # )
    # updateTabItems(session, "fao", newtab)
    session$sendCustomMessage("set_active_tab", c(12, "#shiny-tab-total_des"))
    
    
    
  })
  
  

  
  
  observeEvent(input$fbs_report_button,{


    newtab <- switch(input$fao,
                     "fbs" = "fbs_report"

    )
    updateTabItems(session, "fao", newtab)



  })
  

  
  
  

  
  

  
observeEvent(input$fromyear,{
  
  if (input$fromyear != "" & nchar(input$fromyear) == 4){
  
  if(input$fromyear <= 2013){
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = "Please Select a year greater than 2013",
        type = "error"
      )
    }
  
  }
})
  
  
  
  observeEvent(input$fbs_report_yr,{
    
    if (input$fbs_report_yr != "" & nchar(input$fbs_report_yr) == 4){
      
      if(input$fbs_report_yr < input$fromyear){
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = paste("Please Select a year greater than", input$fromyear),
          type = "error"
        )
      }
      
    }
  })
  

observeEvent(input$endyear,{
  
  if (input$endyear != "" & nchar(input$endyear) == 4){
    
    if(input$endyear <= 2013){
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Please Select a year greater than 2013",
        type = "error"
      )
    }
    
  }
})


observeEvent(c(input$fromyear, input$endyear),{
  
  if (input$fromyear != "" & nchar(input$fromyear) == 4 & nchar(input$endyear) == 4 ){
    
    if(input$endyear < input$fromyear){
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = paste("Please Select a year greater than",input$fromyear),
        type = "error"
      )
    }
    
  }
})


saveMessages(input, output, session, buttons= c("saveCrop", "saveLivestock","saveImports","saveExports",
                                                "saveLossratio","saveLoss","saveFeedratio","saveFeed","saveSeedratio",
                                                "saveSeed","saveIndustry","saveGDP","savePopulation","savefoodclassific",
                                                "saveFood", "saveFish","nutrientSave","treeSave"))





session$onSessionEnded(function() {
  # # 
  # # session$reload()
  # stopApp()
  # # 
  # # 
})

Sys.sleep(1)

  
})






