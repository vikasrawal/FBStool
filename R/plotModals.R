plotModals = function(SUAelement = "crop", label = "Crop", title = "Crop Production", selectedEle = "Production [t]"){

fluidPage(
bsModal(paste(label, "PlotBar", sep = ""), title, trigger = paste("barChart", label, sep = ""), size = "large",
        fluidPage(dropdownButton(
          selectizeInput(paste(SUAelement, "PlotX", sep =""), label = "Commodities", choices = get(paste(SUAelement, "Commodities", sep ="")), selected = get(paste(SUAelement, "Commodities", sep =""))[1], multiple = TRUE),
          selectizeInput(paste(SUAelement, "PlotElement", sep =""), label = "Element", choices = get(paste(SUAelement, "Elements", sep ="")), selected = selectedEle, multiple = F),
          selectInput(paste(SUAelement,  "Plotyear", sep = ""), label = "Years", choices =as.numeric(min(countryData[,Year])):as.numeric(max(countryData[,Year])) , multiple = T, selected = 2013), icon=icon("gear"), status = "danger"
        ),
        plotlyOutput(paste("BarChart", SUAelement, sep = "")))),

bsModal(paste(label, "PlotPie", sep = ""), title, trigger = paste("pieChart", label, sep = ""), size = "large",
        fluidPage(dropdownButton(
          selectizeInput(paste(SUAelement, "PlotPieX", sep =""), label = "Commodities", choices = get(paste(SUAelement, "Commodities", sep ="")), selected = get(paste(SUAelement, "Commodities", sep =""))[1], multiple = TRUE),
          selectizeInput(paste(SUAelement, "PlotPieElement", sep =""), label = "Element", choices = get(paste(SUAelement, "Elements", sep ="")), selected = selectedEle, multiple = F),
          selectInput(paste(SUAelement,  "PlotPieyear", sep = ""),label = "Years", choices =as.numeric(min(countryData[,Year])):as.numeric(max(countryData[,Year])) , multiple = F, selected = 2013), icon=icon("gear"), status = "danger"
        ),
        plotlyOutput(paste("PieChart" , SUAelement, sep = "")))),
bsModal(paste(label, "PlotLines", sep = ""), title, trigger = paste("lineChart", label, sep = ""), size = "large",
        fluidPage(dropdownButton(
          selectizeInput(paste(SUAelement, "PlotLinesX", sep =""), label = "Commodities", get(paste(SUAelement, "Commodities", sep ="")), selected = get(paste(SUAelement, "Commodities", sep =""))[1], multiple = TRUE),
          selectizeInput(paste(SUAelement, "PlotLinesElement", sep =""), label = "Element", choices = get(paste(SUAelement, "Elements", sep ="")), selected = selectedEle, multiple = F),
          #selectInput("cropPlotLinesyear",label = "Years", choices =2010:2015 , multiple = T, selected = c(2010, 2011, 2012, 2013, 2014)), icon=icon("gear"), status = "danger"
          sliderInput(paste(SUAelement,  "PlotLinesyear", sep = ""), "Years", min = as.numeric(min(countryData[,Year])), max = as.numeric(max(countryData[,Year])), 
                      value = c(as.numeric(min(countryData[,Year])),2014), sep = "", step = 1), icon=icon("gear"), status = "danger"
          
        ),
        plotlyOutput(paste("LinesChart" , SUAelement, sep = ""))))
)
  
}