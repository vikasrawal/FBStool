#Source functions

css <- HTML(
  "table.dataTable tr.selected td.yellow {
  background-color: yellow !important
  }
  td.yellow {
  background-color: yellow !important
  }"
)

colorizeCell <- function(i, j,id){
  sprintf("colorizeCell(%d, %d, %s)", i, j, id)
}





sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)

tourist_activate =TRUE

colorizeCell <- function(i, j,id){
  sprintf("colorizeCell(%d, %d, %s)", i, j, id)
}





## Require packages or install if missing
packageRequirevInstall()




# all cpc codes 

all_cpc <- data.table(read_excel("Data/cpc2.1.xlsx"))



all_cpc[,c("CONVERSION FACTORS\r\n(FCL-CPC)", "notes" ) :=NULL]

setnames(all_cpc, c("SWS CODE","SWS DESCRIPTOR"), c("CPCCode","Commodity"))

all_cpc[CPCCode == "21439.040000000001",CPCCode := "21439.04"]

add_cpc <- data.table(CPCCode = c("21439.9","01199") , Commodity = c("Juice of fruits n.e.", "Other cereals") )

all_cpc <- rbind(all_cpc,add_cpc)


all_cpc <- all_cpc[!duplicated(all_cpc[,"CPCCode"])]

all_cpc <- all_cpc[CPCCode == "39141", CPCCode := "39140.02"]

all_cpc_codes <- unique(all_cpc$CPCCode)

#reading country data


countryData <- get(load("Data/countrySUA.RData"))
countryData[, Value := as.numeric(Value)] 
countryData[, Flag := as.character(Flag)]


#opening stocks 2014 is protected 
countryData[, Flag := ifelse(ElementCode == "5113" & Year == "2014", "E", Flag)]

countryData[, Flag := ifelse(ElementCode == "5113" & Year %in% c(2015:2018), "I", Flag)]


#some countries do not have Industrial use in 2010-2013. In that case create, empty cells for those cases
#Example Guinea

if (as.character(unique(countryData$CountryM49)) == "204"){

if (length(unique(countryData[ElementCode == "5165"]$Year)) != length(unique(countryData$Year))){
  
  indsutrialTobind<- data.table(expand.grid(CountryM49 = as.character(unique(countryData$CountryM49)),
                                            Country = unique(countryData$Country), 
                                            CPCCode = unique(countryData[ElementCode == "5165"]$CPCCode),
                                            ElementCode = "5165",
                                            Element = "Industrial uses [t]",
                                            Year = as.character(2012:2013),
                                            Value = NA_real_,
                                            Flag = NA_character_))
  
  
  indsutrialTobind <- merge(indsutrialTobind,all_cpc, by= "CPCCode", all.x = TRUE)
  
  countryData <- rbind(countryData,indsutrialTobind)
  
  cols_to_format <- c("CountryM49","Country","ElementCode","Element","Flag","Year")
  
  countryData[, (cols_to_format) := lapply(.SD, as.character), .SDcols = cols_to_format]
  
}
}


# As per requested, live animals must be eliminated. Codes were provided by Giulia.


live_animals <- c("02151", "02154", "02153", "02194", "02192.01","02191","02152",
"02132", "02112","02121.01","02111","02123","02131","02133","02121.02","02192","02122",
  "02140")

countryData <- countryData[!CPCCode %in% live_animals]



countryName <- unique(countryData$Country)

existing_codes <- unique(countryData[, c("CPCCode", "ElementCode")])

#formating years 







#countrySUA data cpc codes must be changed with all_cpc

countryData[, Commodity := NULL]

countryData <- merge(countryData, all_cpc, by = "CPCCode", all.x = TRUE)

setcolorder(countryData, c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value"))


#check for NA commdoties 

countryData[CPCCode == "21439.9", Commodity  := "Juice of fruits n.e."]


# reference 

all_elements <- data.table(read_excel("Data/Reference File.xlsx",sheet= "Elements"))

all_elements_to_merge <- copy(all_elements)


all_elements <- subset(all_elements, ElementCode %in% c("5510","5610","5910","5071","5520","5016","5525","5141","5318","5319","5320","5314","5327","5313","5321",
                                                        
                      "5165","5113","5166","5312","5025","5023", "665","664","674","684","1001","1003","1005","261","271","281"))

all_element_codes <- unique(all_elements$ElementCode)




# remove existing codes from all codes 


allCodes <- data.table(expand.grid(CPCCode = all_cpc_codes, ElementCode = all_element_codes))



#missiong codes 

missingCodes <- allCodes[!existing_codes, on=.(CPCCode, ElementCode)]


missingCodes <- merge(missingCodes, all_cpc, by= "CPCCode", all.x = TRUE)

missingCodes <- merge(missingCodes, all_elements, by= "ElementCode", all.x = TRUE)
#cpc vector 

cpc_vector <- within(missingCodes[,c("CPCCode","Commodity")], x <- paste(CPCCode,Commodity,sep='  |  '))$x


######################################################################################################################################################################

#production domain 


classification <- data.table(read_excel("Data/production_list_cpc.xlsx"))

classification <- classification[CPCCode == "39141", CPCCode := "39140.02"]

#There 22 commodities in the classification table that are missing in 2.1 cpc expanded table provided by Tomasz.
#The names are not exactly same as in 2.1 expanded. So merging names

# classification[, Commodity := NULL]

setnames(classification, "Commodity", "Commodity_name")
classification <- merge(classification,all_cpc , by = "CPCCode" , all.x = TRUE)

classification[is.na(Commodity), Commodity := Commodity_name]

classification <- classification[!is.na(Commodity)]
classification[, Commodity_name := NULL]





set_hot_colwidths <- function(data) {
  ncols <- ncol(data)
  widths <- rep(100, ncols + 1)
  widths[grep("^CPCCode", colnames(data))] <- 0.1
  widths[grep("Commodity", colnames(data))] <- 0.1

  


    widths[grep("^Flag", colnames(data))] <- 0.1


  return(widths)
}






#loss domain


#loss ration cpc vector 

cpc_loss <- unique(data.table(read_excel("Data/lossRatio.xlsx"))$CPCCode)
cpc_vector_loss <- all_cpc_codes[! all_cpc_codes %in% cpc_loss]

cpc_vector_loss <- merge(data.table(CPCCode = cpc_vector_loss),all_cpc, by= "CPCCode", all.x = TRUE)


cpc_vector_loss<- within(cpc_vector_loss[,c("CPCCode","Commodity")], x <- paste(CPCCode,Commodity,sep='  |  '))$x

#element vector 

element_vector <-  within(missingCodes[,c("ElementCode","Element")], x <- paste(ElementCode,Element,sep='  |  '))$x

element_vector_loss <- paste("xxxx | Loss Ratio [%]")


# Food domain 


foodfdmData=fread("Data/fdmData.csv")
foodfdmData=foodfdmData[order(CPCCode)]



data_food_classification <- fread("Data/foodCommodityList.csv")


# Trade 

#trade mapping 

fao_code_country<- fread("SUA-FBS Balancing/Trade/Data/country_codes.csv")
fcl_codes <-as.vector(fread("SUA-FBS Balancing/Trade/Data/fcl_codes.csv")$x)


##nutrient elemtns 

nutrientEle <- data.table(read_excel("Data/Reference File.xlsx", sheet = "Nutrient Elements"))

nutrientEle[, new := paste(Element, Unit)]

nutrientEle[, Element := NULL]

setnames(nutrientEle, "new","Element")

# elements to bind

new_nutrient_element <- c(4008,4031, 4009,4006,4007,4001,4010,4011,4023,4012,4013,4022,4014,4021,4017,
                          4018,4029,4030,4015
)


nutri2bind <- nutrientEle[,c("measuredElement","Measured Element"),with= F]

setnames(nutri2bind, c("measuredElement","Measured Element"),c("ElementCode","Element"))

all_elements <- rbind(all_elements,nutri2bind, nutrientEle[,c("ElementCode","Element"),with = F])


### countrym49 selection
 


countries <- data.table(read_excel("Data/Reference File.xlsx", sheet = "Country"))

country_selc <- paste0(countries$CountryM49 , "|" , countries$Country)
