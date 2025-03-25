SUAFBS=function(){
  
  
 tagList(
  fluidPage(
    tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
    
#     tags$style(HTML("
# .tabbable > .nav > li > a                  {background-color: white;  color:blue}
#                   #  .tabbable > .nav > li > a[data-value='Start'] {background-color: red;   color:white}
#                   # .tabbable > .nav > li > a[data-value='Crops Data'] {background-color: red;   color:black}
#                   # .tabbable > .nav > li > a[data-value='Food'] {background-color: blue;  color:white}
#                   # # .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
#                   # # .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
#                   ")),
    
tabsetPanel("SUABalancing", id = "suaBalance", tabPanel("Start", value = "FBS_domain",
                                                        
br(),column(9,h2("Welcome!"), br(), br(),
  h4("On the following pages, the full supply and
    utilization account (SUA) as well as the
     Food Balance Sheets (FBS) are constructed. In the initial tabs you will be shown additional parameters and you are free to change them. Please
     choose a  year on the right side 
     of this page for which the FBS construction 
     procedure should be applied. Also, please make
     sure that you have read the Handbook for FBS. 
     ")
    
     ),
  sidebarPanel(width = 3, position = 'right',
                                              
                                            
       #         textInput("FBSSUAyear", 
       # "Please select a year"
       #   
       #               ),
                                              
                                              
                    actionButton("startStandardization", "Start Standardization and Balancing!", align = 'center', styleclass = "success", block =T),
       
       tags$head(
         tags$style(
           HTML(".shiny-notification {
                height: 100px;
                width: 800px;
                position:fixed;
                top: calc(50% - 50px);;
                left: calc(50% - 400px);;
}
"
           )
           )
           )
       

       #radioButtons('style', 'Progress bar style')
                                 )
                                  ),


# 
# tabPanel("Commodity Tree", br(),br(), 
#          
#          
#          fluidRow(column(width =5 , a("Commodity Tree",
#                                       style = "font-size: 170%;color: blue;",href="tree.pdf#page=1")),br(),
#         
#          
#          column(5, 
#                 
#                 div(style="display:inline-block"
#                     # dropdownButton(label = "Add parent and child commodities", icon = icon("chevron-circle-right"), circle = FALSE, #fullwidth=TRUE,
#                     #                status = "primary", 
#                     #                tooltip = tooltipOptions(title = "Add parent and child commodities option", placement = "top")
#                     #                
#                     #                # actionGroupButtons(
#                     #                #   inputIds = c("addPrimaryChild"),
#                     #                #   labels = list(tags$span(icon("plus"), "Add parent and child commodities")),
#                     #                #   status = "primary", direction = "vertical"
#                     #                # )
#                     #                
#                     #                )
#                 ),
#                 
#                 
#                 
#                 div(style="display:inline-block",
#                        dropdownButton(label = "Save", icon = icon("chevron-circle-right"), circle = FALSE, #fullwidth=TRUE,
#                                       status = "warning", 
#                                       tooltip = tooltipOptions(title = "Save and continue options", placement = "top"),
#                                       
#                                       actionGroupButtons(
#                                         inputIds = c("treeSave"),
#                                         labels = list(tags$span(icon("save"), "Save Modifications to the tree")),
#                                         status = "warning", direction = "vertical"
#                                       ))
#          ),
#          
#          div(style="display:inline-block",
#              
#              
#              actionGroupButtons(
#                inputIds = c("nutrient"),
#                labels = list(tags$span(icon("magic"),"Continue")),
#                status = "info", direction = "vertical"
#              )
#          )
#          )    
#          
#          
#          ), br(), br(),
#          
#          rHandsontableOutput("commoditytree"),
#          tags$style(type="text/css", "#commoditytree th {font-weight:bold;}"),
#          tags$head(
#            tags$style(HTML("
#                            .handsontable {
#                            overflow: visible;
#                            }
#                            ")))
#          
#          
#          
#          
#          
#          ),


# tabPanel("Nutrient Factors", br(),br(), 
#          
#          
#          fluidRow(column(width =5 , h2("Nutrient Factors")),br(),
#                   
#                   
#                   column(5, 
#                          #nutrient add commodity
#                          
#                          # div(style="display:inline-block",
#                          #     # dropdownButton(label = "Add a Commodity", icon = icon("chevron-circle-right"), circle = FALSE, #fullwidth=TRUE,
#                          #     #                status = "primary", 
#                          #     #                tooltip = tooltipOptions(title = "Add commodity option", placement = "top"),
#                          #     #                
#                          #     #                actionGroupButtons(
#                          #     #                  inputIds = c("addNutrient"),
#                          #     #                  labels = list(tags$span(icon("plus"), "Add a Commodity")),
#                          #     #                  status = "primary", direction = "vertical"
#                          #     #                ))
#                          # ),
#                          # 
#                          
#                          
#                          div(style="display:inline-block",
#                              dropdownButton(label = "Save", icon = icon("chevron-circle-right"), circle = FALSE, #fullwidth=TRUE,
#                                             status = "warning", 
#                                             tooltip = tooltipOptions(title = "Save and continue options", placement = "top"),
#                                             
#                                             actionGroupButtons(
#                                               inputIds = c("nutrientSave"),
#                                               labels = list(tags$span(icon("save"), "Save Modifications to nutrient factors")),
#                                               status = "warning", direction = "vertical"
#                                             ))
#                          ),
#                          
#                          div(style="display:inline-block",
#                              
#                              
#                              actionGroupButtons(
#                                inputIds = c("startSUA"),
#                                labels = list(tags$span(icon("magic"),"Start compiling SUA-FBS!")),
#                                status = "info", direction = "vertical"
#                              )
#                          )
#                   )    
#                   
#                   
#          ), br(), br(),
#          
#          rHandsontableOutput("nutrientFactors"),
#          
#          tags$style(type="text/css", "#nutrientFactors th {font-weight:bold;}"),
#          tags$head(
#            tags$style(HTML("
#                            .handsontable {
#                            overflow: visible;
#                            }
#                            ")))
#          
#          
#          
# ),








tabPanel("SUA Unbalanced", br(),br(), fluidRow(column(width =5 , h2(textOutput("sua_unbalance_title")))),br(),
       fluidRow(column(width = 4,downloadButton("downloadSuaUnbalanced", "Download", class = "butt1")), 
                # column(width = 2,actionButton("balancedProcessed", "Balance Processed", align = 'center', status = "info", direction = "vertical", icon = "magic"))
              
                div(style="display:inline-block",
               
                    actionGroupButtons(
                      inputIds = c("runPlugin"),
                      labels = list(tags$span(icon("magic"),"Run Balancing Plugin")),
                      status = "danger", direction = "vertical"
                    ),
                    
                    
                                   
                                   actionGroupButtons(
                                     inputIds = c("balancedProcessed"),
                                     labels = list(tags$span(icon("magic"),"SUA Balanced")),
                                     status = "info", direction = "vertical"
                                   )
                )
       
       )
       
       
       ,br(),br(),
       dashboardBody(
         div(style = 'overflow-x: scroll', DT::dataTableOutput('suaUnbalanced'))
       ),
       
       
       tags$head( 
         tags$style(type='text/css', '.fixedHeader table.table thead 
                    .sorting { background-color: #eee; }'), 
         tags$script(src='dataTables.fixedHeader.js') 
         ) ,
         
       # dataTableOutput("suaUnbalanced"),
         tags$style(type="text/css", "#suaUnbalanced th {font-weight:bold;}")   ),


tabPanel("SUA Balanced", br(),br(), fluidRow(column(width =5 , h2(textOutput("SUA_Balanced")))),br(),
         fluidRow(column(width = 4,downloadButton("downloadstand", "Download", class= "butt1")), 
                  # column(width = 2,actionButton("standardizeAggrigate", "Standardize & Aggregate", align = 'center', styleclass = "success", block =T))
         
                  div(style="display:inline-block",
                      
                      
                      actionGroupButtons(
                        inputIds = c("standardizeAggrigate"),
                        labels = list(tags$span(icon("magic"),"Total DES")),
                        status = "info", direction = "vertical"
                      )
                      # downloadButton("downloadSUA_Aggregate", "Download Time Series", class= "butt1")
                  ) 
                  
                  
                  
                  
                  
                  
                  )
         
         
         ,br(),br(),
         
         
         dataTableOutput("suaProcessed"),
         tags$style(type="text/css", "#suaProcessed th {font-weight:bold;}")   ),




# tabPanel("Primary Equivalent Unbalanced", br(),br(), fluidRow(column(width =5 , h2(textOutput("Primary_Equivalent_Unbalanced_title")))),br(),
#          fluidRow(column(width = 4,downloadButton("downloadPrimary", "Download", class = "butt1")), 
#                   # column(width = 2,actionButton("balanceFBS", "Balance FBS", align = 'center', styleclass = "success", block =T))
#          
#                   
#                   div(style="display:inline-block",
#                       
#                       
#                       actionGroupButtons(
#                         inputIds = c("balanceFBS"),
#                         labels = list(tags$span(icon("magic"),"Balance FBS")),
#                         status = "info", direction = "vertical"
#                       )
#                   ) 
#                   
#                   
#                   
#                   
#                   
#                   
#                   )
#          
#          
#          ,br(),br(),
#          
#          
#          dataTableOutput("primary"),
#          tags$style(type="text/css", "#primary th {font-weight:bold;}")   ),
# 
# 
# 
# 







# 
# tabPanel("SUA Balanced ", br(),br(), fluidRow(column(width =8 , h2("SUA Balanced Table")),
#                                                      column(width = 3,downloadButton("downloadSuaBalanced", "Download SUA Balanced Table"))),br(),br(),
#          
#          
#          rHandsontableOutput("suaBalanced"),
#          tags$style(type="text/css", "#suaBalanced th {font-weight:bold;}")),


# tabPanel("FBS", br(),br(), fluidRow(column(width =8 , h2(textOutput("fbs_title")))
#                                               ),br(),br(),
#          
#          
#          
#          fluidRow(column(width = 2,downloadButton("downloadfbs", "Download", class = "butt1")), 
#                   # column(width = 2,actionButton("DESButton", "DES by FBS Aggregate", align = 'center', styleclass = "success", block =T))
#          
#                   column(width=10,
#                   div(style="display:inline-block",
#                       
#                       
#                       actionGroupButtons(
#                         inputIds = c("DESButton"),
#                         labels = list(tags$span(icon("magic"),"DES by Aggregate")),
#                         status = "info", direction = "vertical"
#                       ))
#                   # div(style="display:inline-block",
#                   #     dropdownButton(label = "Get Report!", #fullwidth=TRUE,
#                   #                    downloadButton("PDFreport", "Generate report"),
#                   #                    circle = FALSE, status = "danger", icon = icon("file-alt"),
#                   #                    tooltip = tooltipOptions(title = "Get a PDF report", placement = "top")
#                   #     )
#                   #     #                actionGroupButtons(inputIds = c("PDFreport", "HTMLreport", "WordReport"),
#                   #     #                                   labels = list(tags$span(icon("file-pdf"), "PDF"),
#                   #     #                                                 tags$span(icon("file-code"), "HTML"),
#                   #     #                                                 tags$span(icon("file-word"), "Word")),
#                   #     #                                   status = "danger", direction = "vertical"),
#                   #     #                circle = FALSE, status = "danger", icon = icon("file-alt"),
#                   #     #                tooltip = tooltipOptions(title = "Plot data", placement = "top")
#                   #     # )
#                   #     
#                   # )
#                   
#                   )
#                   
#                   
#                   
#                   ), br(),br(),
#          
#          dataTableOutput("fbs"),
#          tags$style(type="text/css", "#fbs th {font-weight:bold;}")),

tabPanel("Total DES", br(),br(), fluidRow(column(width =5 , h2("Total Dietary Energy Supply (DES)"))), br(),br(),
                                                           fluidRow(column(width = 4,downloadButton("downloadDES", "Download", class = "butt1"),
                                                                           downloadButton("downloadDES_timeseries", "Download Timeseries", class = "butt1")  )
#           column(width=4,
#            div(style="display:inline-block",
#           actionGroupButtons(
#            inputIds = c("DESButtonCPC"),
#             labels = list(tags$span(icon("magic"),"DES by Commodity")),
#            status = "info", direction = "vertical"
# ))
# )                
#                                                                     
                                                                    
                                                                    
                                                                    
                                                                    
                                                                    
                                                                    ),
         
         tags$head(tags$style(".butt1{background-color:grey;} .butt1{color: black;} .butt1{font-family: New Times Roman}
                              .butt1{font-size: 18}")), 
         br(),br(),
         
         
         dataTableOutput("DES"),
         tags$style(type="text/css", "#DES th {font-weight:bold;}"))


# tabPanel("DES by Commodity Group", br(),br(), fluidRow(column(width =5 , h2("Dietary Energy Supply (DES) by Commodity Group"))), br(),br(),
#          fluidRow(column(width = 4,downloadButton("downloadDESbyCPC", "Download", class = "butt1")),
#                   
#                   column(width=4,
#                          div(style="display:inline-block",
#                              actionGroupButtons(
#                                inputIds = c("DESButtonCPCYear"),
#                                labels = list(tags$span(icon("magic"),"DES Time Series")),
#                                status = "info", direction = "vertical"
#                              ))
#                   )             
#                   
#    ),
#    
#    
#    
#          
#          tags$head(tags$style(".butt1{background-color:grey;} .butt1{color: black;} .butt1{font-family: New Times Roman}
#                               .butt1{font-size: 18}")), 
#          br(),br(),
#          
#          
#          dataTableOutput("DESbyCPC"),
#          tags$style(type="text/css", "#DESbyCPC th {font-weight:bold;}")),



# 
#         tabPanel("DES by Year", br(),br(), fluidRow(column(width =8 , h2("Dietary Energy Supply (DES) by  Year"))), br(),br(),
#          fluidRow(column(width = 3,downloadButton("downloadDESbyCPCYear", "Download", class = "butt1"))
#                   
#                   
#                   
#          ),
#          
#          tags$head(tags$style(".butt1{background-color:grey;} .butt1{color: black;} .butt1{font-family: New Times Roman}
#                               .butt1{font-size: 18}")), 
#          br(),br(),
#          
#          
#          dataTableOutput("DESbyCPCYear"),
#          tags$style(type="text/css", "#DESbyCPCYear th {font-weight:bold;}"))

            
            
            
            )    
    
  ) 

 ) 

}