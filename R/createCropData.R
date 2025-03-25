

#This function reads the suadata and subset production element by taking into account the triplets and save it in the Data folder. 
#suadata contains only two elements out of the triplets. In the case of crop, it contains only productions and area harvested (input and output). 
#Yield (productivity) is missing. Therefore, this function computes the corresponding productivity quantity by mapping it with the triplet 
#list of the  commodities in the crop domain. 

createCropdata=function(data,initial_year, end_year,session){

  tempdata=copy(data) 
  

  
tempdata[,c("CountryM49","Country"):=NULL]

cropCPCCodes=read_excel("C:/Users/Siriwardenas/Documents/FBSTool-Generic/Data/crop_livestock_classification.xlsx")
setDT(cropCPCCodes)

setnames(cropCPCCodes, "CPC 2.1 Exp. code","CPCCode")

# cropItems_Derived=unique(cropCPCCodes$CPCCode[cropCPCCodes$`Primary or derived` == c("CD")])
cropItems_Primary=unique(cropCPCCodes$CPCCode[cropCPCCodes$`Primary or derived` == c("CP")])
# 
# cropData_Derived=tempdata[CPCCode %in% cropItems_Derived]
# cropData_Derived=cropData_Derived[ElementCode %in% 5510]


cropPrimarydata=tempdata[CPCCode %in% cropItems_Primary]

cropslist=fread("C:/Users/Siriwardenas/Documents/FBSTool-Generic/Data/cropListTool.csv")[1:168,]
yy=c(unique(cropslist$`Input Code`), unique(cropslist$`Productivity Code`), unique(cropslist$`Output Code`))

cropslist=cropslist[,c("CPCCode", "Input Code", "Productivity Code", "Output Code")]


cropPrimarydata = cropPrimarydata[ElementCode %in% yy]
cropPrimarydata$CPCCode=as.character(cropPrimarydata$CPCCode)



cropPrimarydata=cropPrimarydata[, c("CPCCode", "ElementCode", "Year", "Value", "Flag")]


cropPrimarydata_wide=dcast.data.table(cropPrimarydata, CPCCode + Year ~ ElementCode, value.var = c("Value","Flag"))




setnames(cropPrimarydata_wide, grep("^Value", names(cropPrimarydata_wide), value = TRUE),sub("Value","",sub('_', '', grep("^Value", names(cropPrimarydata_wide), value = TRUE))))



gg=merge(cropPrimarydata_wide,cropslist, by="CPCCode", all=TRUE)

gg=gg[!is.na(Year)]


gg[, input_value := .SD[[.BY[[1]]]], by=`Input Code`]
gg[, output_value := .SD[[.BY[[1]]]], by=`Output Code`]

gg[, productivity_value := output_value/input_value]

gg[, productivity_flag:= "I"]
gg[,productivity_value := ifelse(is.nan(productivity_value), 0, productivity_value)]



prodcutivity_tab= gg[,c("CPCCode", "Year" , "Productivity Code","productivity_value","productivity_flag")]


setnames(prodcutivity_tab, c("Productivity Code" , "productivity_value" ,"productivity_flag"), c("ElementCode","Value","Flag"))


prodcutivity_tab=prodcutivity_tab[!is.na(Value)]

# 
# cropPrimarydata=rbind(cropPrimarydata,prodcutivity_tab)
# 
# 
# 
# 
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
# cropData=rbind(cropPrimarydata,cropData_Derived)
# 
# 
# cropData=wide_format(cropData)
# 
# 
# 
# write.xlsx(cropData,"Data/cropData.xlsx",row.names=FALSE)
# 
return(prodcutivity_tab)

}