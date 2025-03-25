Auto_compile <- function(session, input) {
  
  cat("Development option active - Auto_compile()\nDisable this option in production\n")
  
  shiny::updateTextInput(session, "fromyear", value = "2018")
  shiny::updateTextInput(session, "endyear", value = "2020")
  
  year_range <- reactive({
    list(input$fromyear,input$endyear)
  })
  
  observeEvent(year_range(), {
    
    if(input$fromyear != "" && input$endyear != "") {
      shinyjs::click("startContinue")
    }
    
  })
  
}

Activate_tab <- function(session, tab) {
  updateTabItems(session, "fao", selected =tab)
}