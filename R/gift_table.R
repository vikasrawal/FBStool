GIFT<- function(input,output,session){
  
  
  
observeEvent(input$total_DES, {
  
  t=as.numeric(input$endyear)
  
  sua_balanced <- fread("SUA-FBS Balancing/Data/sua_balanced.csv")
  
  sua_balanced <-  sua_balanced[timePointYears %in% c(2014:t)]
  
  sua_balanced <- sua_balanced[!measuredElementSuaFbs %in% c("261","271","281","4031","4030","4008","4023","4014", "4001")]
  
  #filter only the 
  
  sua_balanced <- subset(sua_balanced, measuredElementSuaFbs %in%  
                           c("664",  "674",  "684", new_nutrient_element)) #removing 665
  
  
  sua_balanced[, c("geographicAreaM49","flagObservationStatus") := NULL]
  
  
  grouping_data <- data.table(read_excel("Data/Food Grouping_SUA to GIFT_for FBS tool.xlsx"))
  grouping_data[, c("SUA description (CPC)") := NULL]
  
  setnames(grouping_data,"SUA code (CPC)","CPCCode")
  
  grouping_data <- grouping_data[!duplicated(grouping_data[,c("Food group code (GIFT)","CPCCode"),with= F])]
  
  setnames(sua_balanced,"measuredItemFbsSua","CPCCode")
  
  sua_balanced <- merge(sua_balanced, grouping_data, by = c("CPCCode"), all.x = TRUE)
  
  sua_balanced <- sua_balanced[!is.na(`Food groups (GIFT)`)]
  
  sua_balanced[, CPCCode := NULL]
  
  sua_balanced <-sua_balanced[,.(Aggregate_Val=sum(Value )
        ), by=c("Food group code (GIFT)","Food groups (GIFT)","timePointYears","measuredElementSuaFbs")]   
  
  sua_balanced <- dcast.data.table(sua_balanced, `Food group code (GIFT)`+`Food groups (GIFT)` + measuredElementSuaFbs~ timePointYears, value.var = c("Aggregate_Val"))
  
  
  
  
  setnames(sua_balanced, "measuredElementSuaFbs","ElementCode")
  
  sua_balanced[,ElementCode := as.character(ElementCode)]
  
  sua_balanced <- merge(sua_balanced, all_elements, by= "ElementCode", all.x = TRUE)
  
  yearcols <- grep("^Value", names(sua_balanced), value = TRUE) 
  
  setcolorder(sua_balanced, c("Food group code (GIFT)","Food groups (GIFT)",
                              "ElementCode","Element",yearcols))
  
  df_gift$data_gift <- sua_balanced
  
})
  
  
  
  
  
  output$download_gift<- downloadHandler(
    
    filename = function() {
      
      "GIFT.xlsx"
    },
    
    
    content = function(file) {
      
      data_download <- data.table(df_gift$data_gift)
     write.xlsx(data_download ,file,row.names = FALSE)
    }
    
  )
  
  
  
  
  
  output$gift_tab <-
    
    
    renderDataTable({
      
      if (is.null(df_gift$data_gift)){
        
        validate(
          need(nrow(df_gift$data_gift)>0, "Please run the balancing plugin")
        )
      }
      
      datatable (df_gift$data_gift, rownames= FALSE,class = 'cell-border stripe',
                 extensions = c("FixedColumns","FixedHeader"),
                 options = list(
                   pageLength = 25,
                   fixedHeader= TRUE,
                   scrollX = TRUE,
                   scrollY = "500px"
                   # autoWidth = T,
                   # fixedColumns = list(leftColumns = 6)
                   # columnDefs = list(
                   #   list(visible = FALSE, targets = (ncol(df_gift$data_gift))))
                 ))   %>%
        
       
        
        formatCurrency(columns = as.character(c(2014:input$endyear)),currency = "", digits = 2,interval = 3, mark = ",")
      
      
    })
  
  
  
  
   
}