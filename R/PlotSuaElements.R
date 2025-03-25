


PlotSuaElements = function(input, output, session){
  
  element = c("crop", "livestock", "tradeI", "tradeE", "Stocks", "foodTable", "lossTable", "feedTable", 
              "seedTable", "industryTable", "residualTable")
  
  lapply(element, function(element){ 
  
    output[[paste("BarChart", element, sep = "")]] = renderPlotly({local({
    
    PlotData = data.table(hot_to_r(input[[element]]))
    
    
    
    PlotData=long_format(PlotData)
    
    PlotData = subset(PlotData, Element == input[[paste(element, "PlotElement", sep ="")]])
    
    print(unique(PlotData$Element))
    PlotData = subset(PlotData, Commodity %in% input[[paste(element, "PlotX", sep ="")]])
    PlotData =subset(PlotData, Year %in% input[[paste(element, "Plotyear", sep ="")]])
    plot_ly(x = ~PlotData[, Commodity], y = ~PlotData[,Value], color = ~PlotData[, Year]) %>%
      layout(xaxis = list(title= "Commodity"), yaxis = list(title= "Value"))
    
    })
    })
    
    
    output[[paste("PieChart", element, sep = "")]] = renderPlotly({local({ 
      PlotData = data.table(hot_to_r(input[[element]]))
      

      PlotData=long_format(PlotData)
      
      PlotData = subset(PlotData, Element == input[[paste(element, "PlotPieElement", sep ="")]])
      PlotData = subset(PlotData, Commodity %in% input[[paste(element, "PlotPieX", sep ="")]])
      PlotData =subset(PlotData, Year %in% input[[paste(element, "PlotPieyear", sep ="")]])  
    plot_ly(PlotData, labels = ~PlotData[, Commodity], values = ~PlotData[,Value], type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% layout(showlegend = T)
    
    })
    })   
    
    output[[paste("LinesChart", element, sep = "")]] = renderPlotly({local({ 
      PlotData = data.table(hot_to_r(input[[element]]))
      
      
      PlotData=long_format(PlotData)
      
      PlotData = subset(PlotData, Element == input[[paste(element, "PlotLinesElement", sep ="")]])
      PlotData = subset(PlotData, Commodity %in% input[[paste(element, "PlotLinesX", sep ="")]])
      PlotYears = seq(input[[paste(element, "PlotLinesyear", sep ="")]][1], input[[paste(element, "PlotLinesyear", sep ="")]][2], by = 1)
      PlotData = subset(PlotData, Year %in% PlotYears)  
    #  p = plot_ly(PlotData, x = ~PlotData[Commodity == input[[paste(element, "PlotLinesX", sep ="")]][1], Year],
    #              y = ~PlotData[Commodity == input[[paste(element, "PlotLinesX", sep ="")]][1],Value], type = 'scatter', mode = 'lines')
    # for(i in 2:length(input[[paste(element, "PlotLinesX", sep ="")]])){
    #   p <- add_trace(p, y = PlotData[Commodity == input[[paste(element, "PlotLinesX", sep ="")]][i], Value],
    #                  mode = "lines")
    # }
    # 
    #  p
      # p <- plot_ly()
      # 
      # for(i in 1:length(input[[paste(element, "PlotLinesX", sep ="")]])){
      #   p <- add_trace(p, x = PlotData[Commodity == input[[paste(element, "PlotLinesX", sep ="")]][1], Year],
      #                  y = PlotData[Commodity == input[[paste(element, "PlotLinesX", sep ="")]][i], Value],
      #                  mode = "lines")
      # }
      # 
      # p
      # 
      # p <- plot_ly()
      # 
      # for(i in 1:5){
      #   p <- add_trace(p, x = 1:10, y = rnorm(10), mode = "lines")
      # }
      # 
      # p
     p <- plot_ly()%>% layout(xaxis = list(title= "Year", dtick = 1), yaxis = list(title= "Tonnes"))
     for(i in 1: length(input[[paste(element, "PlotLinesX", sep ="")]])){
       p <- add_lines(p, x = PlotYears,
                      y = PlotData[Commodity == input[[paste(element, "PlotLinesX", sep ="")]][i], Value], 
                      mode = "lines+markers",
                      name = paste(input[[paste(element, "PlotLinesX", sep ="")]][i]))
     }
     
     p
     # ggideal_point <- ggplot(PlotData) +
     #   geom_line(aes_(x = Year, 
     #                 y = Value, by = Commodity, color = Commodity), size =1) +
     #   labs(x = "Year", y = "Ideology", title = "graph_title") +
     #   scale_colour_hue("Commodity", l = 70, c = 150) + ggthemes::theme_few() 
     #   theme(legend.direction = "horizontal", legend.position = "bottom")
     # 
     # # Convert ggplot object to plotly
     # gg <- plotly_build(ggideal_point)
     # gg
      
    
  })
    })
    
  })  

   observe({
     
     year = as.numeric(input$endyear)
     if(is.na(year)){
       year = 2013}
     
     updateSelectInput(session, paste(element, "Plotyear", sep =""),choices =  2010:year)
     # 
     
     
     
   })
 
   
  
}
