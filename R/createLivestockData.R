

#This function reads the suadata and subset production element by taking into account the triplets and save it in the Data folder. 
#suadata contains only two elements out of the triplets. In the case of livestock, it contains only productions and area harvested (input and output). 
#Yield/Carcass Weigth [kg/head] (productivity) is missing. Therefore, this function computes the corresponding productivity quantity by mapping it with the triplet 
#list of the  commodities in the livestock domain. 



createLivestockData=function(data,initial_year, end_year,session){
  
  tempdata=copy(data)
  
  # tempdata=countryData
  
  tempdata[,c("CountryM49","Country"):=NULL]
  
  livestockCPCCodes=read_excel("C:/Users/Siriwardenas/Documents/FBSTool-Generic/Data/crop_livestock_classification.xlsx")
  setDT(livestockCPCCodes)
  
  setnames(livestockCPCCodes, "CPC 2.1 Exp. code","CPCCode")
  
  liveItems_Derived=unique(livestockCPCCodes$CPCCode[livestockCPCCodes$`Primary or derived` == c("LD")])
  liveItems_Primary=unique(livestockCPCCodes$CPCCode[livestockCPCCodes$`Primary or derived` == c("LP")])
  # liveItems=unique(livestockCPCCodes$CPCCode[livestockCPCCodes$`Primary or derived` == c("L")])
  
  # liveData_Derived=tempdata[CPCCode %in% liveItems_Derived]
  # liveData_Derived=liveData_Derived[ElementCode %in% 5510]

  livePrimarydata=tempdata[CPCCode %in% liveItems_Primary]
  
  
  livestocklist=fread("C:/Users/Siriwardenas/Documents/FBSTool-Generic/Data/livestockListTool.csv")
  livestocklist=livestocklist[`Input Code` != "53200",]
  livestocklist=livestocklist[`Input Code` != "53201",]
  
  yy=c(unique(livestocklist$`Input Code`), unique(livestocklist$`Productivity Code`), unique(livestocklist$`Output Code`))
  
  livePrimarydata = livePrimarydata[ElementCode %in% yy]
  livePrimarydata$CPCCode=as.character(livePrimarydata$CPCCode)
  
  
  
  livePrimarydata=livePrimarydata[, c("CPCCode", "ElementCode", "Year", "Value", "Flag")]
  
  
  livePrimarydata_wide=dcast.data.table(livePrimarydata, CPCCode + Year ~ ElementCode, value.var = c("Value","Flag"))
  
  
  
  
  setnames(livePrimarydata_wide, grep("^Value", names(livePrimarydata_wide), value = TRUE),sub("Value","",sub('_', '', grep("^Value", names(livePrimarydata_wide), value = TRUE))))
  
  
  
  gg=merge(livePrimarydata_wide,livestocklist, by="CPCCode", all=TRUE)
  
  gg=gg[!is.na(Year)]

  
  gg[, input_value := .SD[[.BY[[1]]]], by=`Input Code`]
  gg[, output_value := .SD[[.BY[[1]]]], by=`Output Code`]
  
  gg[, productivity_value := output_value/input_value]
  
  gg[, productivity_flag:= "I"]
  gg[,productivity_value := ifelse(is.nan(productivity_value), 0, productivity_value)]
  
  
  
  prodcutivity_tab= gg[,c("CPCCode", "Year" , "Productivity Code","productivity_value","productivity_flag")]
  
  
  setnames(prodcutivity_tab, c("Productivity Code" , "productivity_value" ,"productivity_flag"), c("ElementCode","Value","Flag"))
  
  prodcutivity_tab=prodcutivity_tab[!is.na(Value)]
  
  
  prodcutivity_tab= merge(prodcutivity_tab,elementName,
                          by.x = "ElementCode",
                          by.y = "ElementCode",
                          all.x = TRUE)
  
  prodcutivity_tab= merge(prodcutivity_tab,commodityName,
                          by.x = "CPCCode",
                          by.y = "CPCCode",
                          all.x = TRUE)
  
  # 
  setcolorder(prodcutivity_tab, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
  # 
  
  # livePrimarydata=rbind(livePrimarydata,prodcutivity_tab)
  
 
 
 
  
  
  # livePrimarydata= merge(livePrimarydata,elementName,
  #                        by.x = "ElementCode",
  #                        by.y = "ElementCode",
  #                        all.x = TRUE)
  # 
  # livePrimarydata= merge(livePrimarydata,commodityName,
  #                        by.x = "CPCCode",
  #                        by.y = "CPCCode",
  #                        all.x = TRUE)
  # 
  # 
  # setcolorder(livePrimarydata, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
  # 
  # livestockData=rbind(livePrimarydata,liveData_Derived)
  # 
  # livestockData=wide_format(livestockData)
  # 
  # 
  # 
  # write.xlsx(livestockData,"Data/livestockData.xlsx",row.names=FALSE)
  # 
  return(prodcutivity_tab)

  
}