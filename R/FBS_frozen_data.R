FBS_frozen_data=function(input,output,session){
  
  
#This function saves the frozen tables used in the FBS domain according to the country selected by the user at the very begining. 
  # country=51
  
  country = as.character(input$Country)
  country=gsub(" ","", sapply(strsplit(as.character(input$Country), "\\|"), `[[`, 1))
  
  
  
  t = as.numeric(input$endyear)
  
  
  # t=2014
  
  tree_all=get(load("SUA-FBS-Balancing/Data/tree_all.RData"))
  
  tree_all = subset(tree_all, timePointYearsSP >=  2010)
  
  tree=subset(tree_all, geographicAreaM49 == country & timePointYearsSP == t )
  
# If country does not have the tree available for the corresponding selected year. Then, use the most recent year available tree. 
  
  if (nrow(tree) == 0){
    
    tree=subset(tree_all,geographicAreaM49 == country )
    
    year=max(tree$timePointYearsSP) 
    
    
    tree=subset(tree_all, geographicAreaM49 == country & timePointYearsSP == year)
        
} 
  
tree[,timePointYearsSP := as.character(t) ]
  
  
names(tree)=c("CPCCodeParent", "CountryM49", "Year", "CPCCodeChild", "extractionRate", "share", "target", "type")
  
  
  countryCode= read_excel("Reference Files/Reference File.xlsx", "Country")
  
  setDT(countryCode)
  
  
  commodity = read_excel("Reference Files/Reference File.xlsx", "SUA_Commodities")
  setDT(commodity)
  
  
  tree=merge(tree,countryCode, by = "CountryM49", all.x=TRUE)
  
  
  tree=merge(tree,commodity, by.x= "CPCCodeParent", by.y = "CPCCode", all.x = TRUE)
  
  
  setnames(tree,"Commodity","CommodityParent")
  
  
  tree=merge(tree,commodity, by.x= "CPCCodeChild", by.y = "CPCCode", all.x = TRUE)
  
  
  setnames(tree,"Commodity","CommodityChild")
  
  
  tree[, "type" :=NULL]
  
  
  setcolorder(tree, c("CountryM49", "Country", "CPCCodeParent", "CommodityParent","CPCCodeChild","CommodityChild","Year","extractionRate","share", "target" ))
  
  

  
  write.csv(tree,"SUA-FBS-Balancing/Data/tree.csv", row.names = FALSE)
  
  
##Utilization Table
  
  utilization_all=get(load("SUA-FBS-Balancing/Data/utilizationTable_all.RData"))
  
  utilizationTable=subset(utilization_all, geographicAreaM49 == country)
  
  write.csv(utilizationTable,"SUA-FBS-Balancing/Data/utilizationTable.csv", row.names = FALSE)
  
  
 
  
  
##Nutrient Data
  

  
  nutrientData=get(load("SUA-FBS-Balancing/Data/nutrientData_all.RData"))
  
  
  
  nutrientData=nutrientData[geographicAreaM49 == country]
  
  
  names(nutrientData)= c("CountryM49","ElementCode","CPCCode","Year","Value","FlagRatio")
  
  
  
  countryCode= read_excel("Reference Files/Reference File.xlsx", "Country")
  
  setDT(countryCode)
  
  
  commodity = read_excel("Reference Files/Reference File.xlsx", "SUA_Commodities")
  setDT(commodity)
  
  
  
  elementCode = read_excel("Reference Files/Reference File.xlsx", "Elements")
  setDT(elementCode)
  
  
  nutrientData=merge(nutrientData,countryCode, by = "CountryM49", all.x=TRUE)
  
  
  nutrientData=merge(nutrientData,commodity, by= "CPCCode", all.x = TRUE)
  
  nutrientData=merge(nutrientData, elementCode, by= "ElementCode",all.x = TRUE)
  
  
  setcolorder(nutrientData,c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","FlagRatio"))
  
  write.csv(nutrientData,"SUA-FBS-Balancing/Data/nutrientData.csv",row.names = FALSE)
  

 #fish DES Data 
  
  fishDES=fread("SUA-FBS-Balancing/Data/fish_DES_all.csv")
  
  fishDES=fishDES[CountryM49 == country]
  
  
  write.csv(fishDES,"SUA-FBS-Balancing/Data/FBS_Fishery.csv",row.names = FALSE)
  
  
}