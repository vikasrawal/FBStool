




#Please enter the country Name in which you want to create the FBS Tool


# countryName= "Cambodia"
# suadata=readRDS("SUAData/SUA_data.rds")
#initial_year=2010
#end_year = 2015



# data=create_FBS_Tool(suadata=readRDS("SUAData/SUA_data.rds"),countryName = "Cambodia")
#data is the SUA of all Countries

create_FBS_Tool = function(suadata,countryName, initial_year, end_year){


tempData=copy(suadata)  
initial_year=as.numeric(initial_year)
end_year=as.numeric(end_year)
  
setnames(tempData,c("geographicAreaM49" ,"geographicAreaM49_description", "measuredElement" , "measuredElement_description","measuredItemCPC",              
"measuredItemCPC_description" ,"timePointYears" ,"Value" ,"translatedFlag"),
c("CountryM49","Country", "ElementCode","Element", "CPCCode", "Commodity", "Year", "Value", "Flag"))

  
setcolorder(tempData, c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))

countryData=lapply(unique(tempData[,CountryM49]), function(x){subset(tempData, CountryM49 == x)}) 



for (i in 1:length(countryData)){
  
  
suaData=  countryData[[i]]

save(suaData,file= paste0("SUAData/", unique(suaData$Country), ".RData"))

}


suaData= get(load(paste0("SUAData/",countryName, ".RData")))
save(suaData, file=paste0("Data/", unique(suaData$Country), ".RData"))
suaData=get(load(paste0("Data/",countryName, ".RData")))


#creating crop data 

cropData=createCropdata(data,initial_year, end_year)


#creating livestock data

livestockData=createLivestockData(data,initial_year,end_year)








}