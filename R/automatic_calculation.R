# 
# automatic_calculation_crop=reactive({
# 
#   datacopy <- NULL
# 
#   #For initial data upload
#   if(is.null(input$crop)){
#     datacopy=wideformat_crop()
# 
#   }else {
# 
#  # datacopy = hot_to_r(input$crop)
# 
#     datacopy =data.table(hot_to_r(input$crop))
# 
# 
#     # #If there is change in data
#     if(!is.null(input$crop$changes$changes) & !is.na(datacopy) ){
#     #
#       row.no <- unlist(input$crop$changes$changes)[1]
#       element = datacopy[[(row.no + 1), "ElementCode"]]
#       year= datacopy[[(row.no + 1), "Year"]]
#       cpccode= datacopy[[(row.no + 1), "CPCCode"]]
#       col.no <- unlist(input$crop$changes$changes)[2]
# 
# 
#     #  col.no = 7
# 
#       # w=auto[ElementCode == "5312" & CPCCode == "0112" , col.no , with=F ]
#       # v=auto[ElementCode == "5421" & CPCCode == "0112" , col.no , with=F ]
#       #
#       # auto[ElementCode == "5510" & CPCCode == "0112", (names(auto)[col.no]):= w*v]
# 
# 
# 
# 
#       if(element == "5421" ){#if Yield is changed, Production should be updated
# 
#         y=datacopy[ElementCode == "5312" & CPCCode==cpccode, col.no+1, with=F]
#         p=datacopy[ElementCode == "5421" & CPCCode==cpccode, col.no+1 , with=F]
# 
#         datacopy[ElementCode == "5510" & CPCCode== cpccode, (names(datacopy)[col.no+1]):= y*p]
# 
# 
#     #
#     #
#     #
#     #     datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                      datacopy$ElementCode == "5510"] =
#     #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                        datacopy$ElementCode == "5312"] *
#     #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                        datacopy$ElementCode == "5421"]
#     #
#       }
#       else if(element == "5510"& !is.na(element)){#if Production is changed, Area Harvested should be updated
#     #
#         p=datacopy[ElementCode == "5510" & CPCCode==cpccode, col.no+1, with=F]
#           a=datacopy[ElementCode == "5421" & CPCCode==cpccode, col.no+1, with=F]
# 
#         datacopy[ElementCode == "5312" & CPCCode== cpccode, (names(datacopy)[col.no+1]):= p/a]
#     #     #
#     #     #
#     #
#     #
#     #
#     #     datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                      datacopy$ElementCode == "5312"] =
#     #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                        datacopy$ElementCode == "5510"] /
#     #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                        datacopy$ElementCode == "5421"]
#     #
#     #
#     #
#     #
#       }else if(element == "5312"& !is.na(element)){#if Area Harvested is chnaged, Production should be updated
#     #
#         a=datacopy[ElementCode == "5312" & CPCCode==cpccode ,  col.no+1, with=F]
#           p=datacopy[ElementCode == "5421" & CPCCode==cpccode, col.no+1, with=F]
# 
#         datacopy[ElementCode == "5510" & CPCCode== cpccode, (names(datacopy)[col.no+1]):= a*p]
#     #
#     #
#     #
#     #
#     #     datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                      datacopy$ElementCode == "5510"] =
#     #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                        datacopy$ElementCode == "5312"] *
#     #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#     #                        datacopy$ElementCode == "5421"]
#     #
#     #
#       }
# 
# 
#     }
# 
#   }
#   datacopy=datacopy[order(CPCCode)]
# 
#   datacopy
# 
# })




# 
# automatic_calculation_stock=reactive({
# 
#   datacopy <- NULL
# 
#   #For initial data upload
#   if(is.null(input$stock)){
#     datacopy=wideformat_stock()
# 
#   }else {
# 
#     # datacopy = hot_to_r(input$crop)
# 
#     datacopy =data.table(hot_to_r(input$stock))
# 
# 
#     # #If there is change in data
#     if(!is.null(input$stock$changes$changes) & !is.na(datacopy) ){
#       #
#       row.no <- unlist(input$stock$changes$changes)[1]
#       element = datacopy[[(row.no + 1), "ElementCode"]]
#       year= datacopy[[(row.no + 1), "Year"]]
#       cpccode= datacopy[[(row.no + 1), "CPCCode"]]
#       col.no <- unlist(input$stock$changes$changes)[2]
#       
#       
# 
#       #  col.no = 7
# 
#       # w=auto[ElementCode == "5312" & CPCCode == "0112" , col.no , with=F ]
#       # v=auto[ElementCode == "5421" & CPCCode == "0112" , col.no , with=F ]
#       #
#       # auto[ElementCode == "5510" & CPCCode == "0112", (names(auto)[col.no]):= w*v]
#       
#       stocklistTool=fread("Production/Data/livestockListTool.csv")
#       
#       
#       productivityCodes <- unique(stocklistTool$`Productivity Code`)
#       inputCodes <- unique(stocklistTool$`Input Code`)
#       outputCodes <- unique(stocklistTool$`Output Code`)
#       
# 
# 
#       if(element %in%  productivityCodes & !is.na(element) ){#if Yield is changed, Production should be updated
#         
#         
#         inp=stocklistTool$`Input Code`[stocklistTool$CPCCode == cpccode]
#         productivity=stocklistTool$`Productivity Code`[stocklistTool$CPCCode == cpccode]
#         outp=stocklistTool$`Output Code`[stocklistTool$CPCCode == cpccode]
# 
#         y=datacopy[ElementCode == inp & CPCCode==cpccode, col.no+1, with=F]
#         p=datacopy[ElementCode ==  productivity & CPCCode==cpccode, col.no+1 , with=F]
# 
#         datacopy[ElementCode == outp & CPCCode== cpccode, (names(datacopy)[col.no+1]):= y*p]
# 
# 
#         #
#         #
#         #
#         #     datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                      datacopy$ElementCode == "5510"] =
#         #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                        datacopy$ElementCode == "5312"] *
#         #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                        datacopy$ElementCode == "5421"]
#         #
#       }
#       else if(element %in%  outputCodes  & !is.na(element)){#if Production is changed, Area Harvested should be updated
#         
#         
#         inp=stocklistTool$`Input Code`[stocklistTool$CPCCode == cpccode]
#         productivity=stocklistTool$`Productivity Code`[stocklistTool$CPCCode == cpccode]
#         outp=stocklistTool$`Output Code`[stocklistTool$CPCCode == cpccode]
#         
#         
#         #
#         p=datacopy[ElementCode == outp & CPCCode==cpccode, col.no+1, with=F]
#         a=datacopy[ElementCode == productivity & CPCCode==cpccode, col.no+1, with=F]
# 
#         datacopy[ElementCode == inp & CPCCode== cpccode, (names(datacopy)[col.no+1]):= p/a]
#         #     #
#         #     #
#         #
#         #
#         #
#         #     datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                      datacopy$ElementCode == "5312"] =
#         #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                        datacopy$ElementCode == "5510"] /
#         #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                        datacopy$ElementCode == "5421"]
#         #
#         #
#         #
#         #
#       }else if(element %in%  inputCodes & !is.na(element)){#if Area Harvested is chnaged, Production should be updated
#         
#         inp=stocklistTool$`Input Code`[stocklistTool$CPCCode == cpccode]
#         productivity=stocklistTool$`Productivity Code`[stocklistTool$CPCCode == cpccode]
#         outp=stocklistTool$`Output Code`[stocklistTool$CPCCode == cpccode]
#         
#         
#         #
#         a=datacopy[ElementCode == inp & CPCCode==cpccode ,  col.no+1, with=F]
#         p=datacopy[ElementCode == productivity & CPCCode==cpccode, col.no+1, with=F]
# 
#         datacopy[ElementCode == outp & CPCCode== cpccode, (names(datacopy)[col.no+1]):= a*p]
#         #
#         #
#         #
#         #
#         #     datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                      datacopy$ElementCode == "5510"] =
#         #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                        datacopy$ElementCode == "5312"] *
#         #       datacopy$Value[datacopy$CPCCode == cpccode & datacopy$Year == year &
#         #                        datacopy$ElementCode == "5421"]
#         #
#         #
#       }
# 
# 
#     }
# 
#   }
#   datacopy=datacopy[order(CPCCode)]
#   datacopy
# 
# })