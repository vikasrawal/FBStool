# This function observes the 'save and continue' buttons and jumps to the next page

observeSequentiallyActive = function(input, output, session){
  
  observe({


    if (input$startContinue > 0) {
      session$sendCustomMessage('activeNavs', 'Production')
      updateTabsetPanel(session, "suaNavlist", selected = "Production")
    }
  })
  

  
  
  
  # 
  observe({
    if (input$productionSave > 0) {
      session$sendCustomMessage('activeNavs', 'Trade')
      updateTabsetPanel(session, "suaNavlist", selected = "Trade")
    }
  })

  fname = tempfile(fileext = ".csv")

  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$productionSave
    tableProduction = isolate(input$tableProduction)
    if (!is.null(tableProduction)) {
      write.csv(hot_to_r(input$tableProduction), fname)
      print(fname)
    }
  })
  
  
  output$tableProduction = renderRHandsontable({
    if (!is.null(input$tableProduction)) {
      DF = hot_to_r(input$tableProduction)
    } 
    
    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  }) 
  
  

  
  
  observe({
    if (input$tradeSave > 0) {
      session$sendCustomMessage('activeNavs', 'Stocks')
      updateTabsetPanel(session, "suaNavlist", selected = "Stocks")
    }
  })
  
  observe({
    if (input$stocksSave > 0) {
      session$sendCustomMessage('activeNavs', 'Food')
      updateTabsetPanel(session, "suaNavlist", selected = "Food")
    }
  })
  
  observe({
    if (input$foodSave > 0) {
      session$sendCustomMessage('activeNavs', 'Feed')
      updateTabsetPanel(session, "suaNavlist", selected = "Feed")
    }
  })
  
  observe({
    if (input$feedSave > 0) {
      session$sendCustomMessage('activeNavs', 'Seed')
      updateTabsetPanel(session, "suaNavlist", selected = "Seed")
    }
  })
  
  observe({
    if (input$seedSave > 0) {
      session$sendCustomMessage('activeNavs', 'Food Losses & Waste')
      updateTabsetPanel(session, "suaNavlist", selected = "Food Losses & Waste")
    }
  })
  
  observe({
    if (input$flwSave > 0) {
      session$sendCustomMessage('activeNavs', 'Industrial Use')
      updateTabsetPanel(session, "suaNavlist", selected = "Industrial Use")
    }
  })
  
  observe({
    if (input$industrialSave > 0) {
      session$sendCustomMessage('activeNavs', 'Tourist Consumption')
      updateTabsetPanel(session, "suaNavlist", selected = "Tourist Consumption")
    }
  })
  
  observe({
    if (input$touristSave > 0) {
      session$sendCustomMessage('activeNavs', 'Residual Other Uses')
      updateTabsetPanel(session, "suaNavlist", selected = "Residual Other Uses")
    }
  })
  
  observe({
    if (input$residualSave > 0) {
      session$sendCustomMessage('activeNavs', 'Save All')
      updateTabsetPanel(session, "suaNavlist", selected = "Save All")
    }
  })
  
  
}