fish <- function(input,output,session){
  
  
  observeEvent(input$undoFish, {
    
    # get last version
    new_version <- Pop_table_version("fish")  
    
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    
    df_fish$data_fish <- new_version
    
  })
  
  
  observeEvent(input$startContinue,{
    # if (input$fao == 'Feed') {
    
    END_YEAR=input$endyear
    
    fishData=data.table(read_excel("Data/fish.xlsx"))
    
    
    fishData <- subset(fishData, Year %in% c(2010 : END_YEAR) )
    
    setnames(fishData,c("Element Code", "Item Code (CPC)"),c("ElementCode", "CPCCode"))
    
    fishData[,ElementCode := as.character(ElementCode)]
    
    fishData <- merge(fishData,all_elements, by = "ElementCode",all.x = TRUE)
    
    fishData[,Commodity:= "Fish, Seafood"]
    
    fishData=wide_format(fishData)
    
    flagcols <- grep("^Flag", names(fishData), value = TRUE)
    yearcols <- grep("^[[:digit:]]{4}$", names(fishData), value = TRUE)
    
    minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))
    
    
    
    
    if(END_YEAR > maxyear +1){
      END_YEAR=as.numeric(END_YEAR)
      yearsToFill = (maxyear + 1):END_YEAR
      
      df_fish$data_fish <- NULL
      if(length(yearsToFill) > 0){
        # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
        
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = paste("Please compile Fish data for the year(s)",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
          type = "error"
        )
        
        
        
      }
      
    } else {
      
      fishData = visualize_data(fishData,END_YEAR, session)
      
      fishData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      
      df_fish$data_fish <- fishData
      
      
      Add_table_version("fish", copy(df_fish$data_fish))
      
    }
    
    
    
    
    # }
  })
  
  
  proxy_fish = dataTableProxy('fish')
  
  observeEvent(input$fish_cell_edit, {
    
    
    info = input$fish_cell_edit
    
    print(info)
    i = info$row
    j = (info$col + 1)
    v = info$value
    df_fish$data_fish[i,(j) := v]
    
    replaceData(proxy_fish, df_fish$data_fish, resetPaging = FALSE,rownames = FALSE)  # important
    
    info1 <- input[["fish_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"fish"))
    
    Add_table_version("fish", copy(df_fish$data_fish))
    
  })
  
  
  
  observeEvent(input$saveFish,{
    
    data_to_save <- df_fish$data_fish
    
    data_to_save <- long_format(data_to_save)
    
    data_to_save[, c("Commodity", "Element") := NULL]
    
    setnames(data_to_save,c("ElementCode", "CPCCode"),c("Element Code", "Item Code (CPC)"))
    
    write.xlsx(data_to_save,"Data/fish.xlsx",row.names = F)
    
  })
  
  
  
  
  
  
  
  output$fish <- 
    renderDataTable(
      
      if (!is.null(df_fish$data_fish)){
        datatable (df_fish$data_fish, rownames= FALSE,class = 'cell-border stripe', 
                   
                   editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(df_fish$data_fish) == input$fromyear)-2)))),
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
                       #list(width = '150px', targets = c(3))
                       # no hide column
                       list(visible = FALSE, targets = (ncol(df_fish$data_fish)-1))
                     )
                   ))  %>%
          # 
          formatStyle(0:ncol(df_fish$data_fish), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
      
    )
  
  
}