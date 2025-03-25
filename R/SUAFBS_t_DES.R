
SUAFBS_t_DES=function(input,output,session){


files = dir("SUA-FBS Balancing/R",full.names = TRUE)

for(i in files){
  source(i, local = TRUE)
}

basedir="SUA-FBS Balancing/StandardizedData"

t=as.character(input$endyear)
print(t)


batchnumber = 1 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   


message("Getting parameters/datasets...")

# start and end year for standardization come from user parameters
startYear = as.character(2014)
endYear = as.character(t)

# geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = as.character(startYear:endYear)




source("SUA-FBS Balancing/R/suaFilling.R")
source("SUA-FBS Balancing/R/standardizationWrapper.R")
source("SUA-FBS Balancing/R/addMissingElements.R")
source("SUA-FBS Balancing/R/finalStandardizationToPrimary.R")
source("SUA-FBS Balancing/R/calculateAvailability.R")
source("SUA-FBS Balancing/R/collapseEdges.R")
source("SUA-FBS Balancing/R/standardizeTree.R")
source("SUA-FBS Balancing/R/computeFbsAggregate.R")
##############################################################
############ DOWNLOAD AND VALIDATE TREE ######################
##############################################################




#after validating the tree write to the folder

tree <- fread("SUA-FBS Balancing/Data/tree.csv")





##############################################################
############ Set parameters for specific dataset #############
##############################################################

params = defaultStandardizationParameters()
params$itemVar = "measuredItemSuaFbs"
params$mergeKey[params$mergeKey == "measuredItemCPC"] = "measuredItemSuaFbs"
params$elementVar = "measuredElementSuaFbs"
params$childVar = "measuredItemChildCPC"
params$parentVar = "measuredItemParentCPC"
params$productionCode = "production"
params$importCode = "imports"
params$exportCode = "exports"
params$stockCode = "stockChange"
params$foodCode = "food"
params$feedCode = "feed"
params$seedCode = "seed"
params$wasteCode = "loss"
params$industrialCode = "industrial"
params$touristCode = "tourist"
params$foodProcCode = "foodManufacturing"
params$residualCode = "residual"
params$createIntermetiateFile= "TRUE"
params$protected = "Protected"
params$official = "Official"



##############################################################
#################### SET KEYS FOR DATA #######################
##############################################################

elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", 
           "5165", "5520","5525","5164","5166","5141")

# 5510 Production[t]
# 5610 Import Quantity [t]
# 5071 Stock Variation [t]
# 5023 Export Quantity [t]
# 5910 Loss [t]
# 5016 Industrial uses [t]
# 5165 Feed [t]
# 5520 Seed [t]
# 5525 Tourist Consumption [t]
# 5164 Residual other uses [t]
# 5166 Food [t]
# 5141 Food Supply (/capita/day) [Kcal]

desKeys = c("664","674","684")




message("Reading SUA data...")

data=get(load("Data/countrySUA.RData"))

data[, Value := as.numeric(Value)]

data[, c("Country", "Commodity","Element") := NULL]

setnames(data,c("CountryM49","CPCCode","ElementCode","Year","Value","Flag"),
         c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","Value","flagObservationStatus"))

data=subset(data, measuredElementSuaFbs %in% elemKeys)

data = elementCodesToNames(data, itemCol = "measuredItemFbsSua",
                           elementCol = "measuredElementSuaFbs")


#################################################################################################SUMEDA MODIFICATION- Adding the element stock change for stockcommodities where is not presented in sua unbalanced. 


Utilization_Table = fread("SUA-FBS Balancing/Data/Utilization_Table.csv")



Stock_Items = dplyr::filter(Utilization_Table,!is.na(stock))
Stock_Items$classification_stock = "Stock_Item"
Stock_Items = dplyr::select_(Stock_Items,"cpc_code","classification_stock")
Stock_Items <- data.table(Stock_Items)
Stock_Items <- unique(Stock_Items$cpc_code)



dataStock <- data[measuredElementSuaFbs == "stock_change" & measuredItemFbsSua %in% Stock_Items]

Stock_Items_Nonex<-Stock_Items[!Stock_Items %in% (unique(dataStock$measuredItemFbsSua))]




data_with_stock <- expand.grid(measuredElementSuaFbs = "stock_change", geographicAreaM49 = unique(data$geographicAreaM49),
                               measuredItemFbsSua = Stock_Items_Nonex, timePointYears = c(2014:2016), flagObservationStatus = "I",
                               flagMethod = "e" )

data_with_stock=data.table(data_with_stock)

data_with_stock[, measuredElementSuaFbs := as.character(measuredElementSuaFbs)]
data_with_stock[, geographicAreaM49 := as.character(geographicAreaM49)]
data_with_stock[, measuredItemFbsSua := as.character(measuredItemFbsSua)]
data_with_stock[, timePointYears := as.character(timePointYears)]

data_with_stock[, Value := NA]
data_with_stock[,flagMethod :=NULL]
data <- rbind(data,data_with_stock)




##################################################################################################### Sumeda's modification ends


data2 =  fread("SUA-FBS Balancing/Data/data2.csv")
                            


message("convert element codes into element names")

data[measuredElementSuaFbs=="foodmanufacturing",measuredElementSuaFbs:="foodManufacturing"]
setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
data[measuredElementSuaFbs=="stock_change",measuredElementSuaFbs:="stockChange"]
data[measuredElementSuaFbs=="stock",measuredElementSuaFbs:="stockChange"]

message("delete null elements")

data=data[!is.na(measuredElementSuaFbs)]

##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data=convertSugarCodes(data)

##############################################################
############### CREATE THE COLUMN "OFFICIAL" #################
##############################################################

#flag method
# flagValidTable = ReadDatatable("valid_flags")

flagValidTable=fread("SUA-FBS Balancing/Data/flagValidTable.csv")

flagValidTable[, flagObservationStatus := as.factor(flagObservationStatus)]

Feed_Items = dplyr::filter(Utilization_Table,feed_desc%in%c("Potential Feed","FeedOnly"))
Feed_Items = dplyr::rename(Feed_Items,classification_feed= feed_desc)
Feed_Items = dplyr::select_(Feed_Items,"cpc_code","classification_feed")
Stock_Items = dplyr::filter(Utilization_Table,!is.na(stock))
Stock_Items$classification_stock = "Stock_Item"
Stock_Items = dplyr::select_(Stock_Items,"cpc_code","classification_stock")
Feed_Items$classification_feed = "feedOnly"
Fruit_Veg_Food = fread("SUA-FBS Balancing/Data/Fruit_Veg_Food.csv")






# Feed_Items = dplyr::filter(Feed_Items,classification=="feedOnly",!is.na(classification))

#flagmethod
# data=left_join(data,flagValidTable,by=c("flagObservationStatus","flagMethod"))%>%
#   data.table

data=left_join(data,flagValidTable,by=c("flagObservationStatus"))%>%
  data.table

data[flagObservationStatus%in%c("","T"),Official:=TRUE]
data[is.na(Official),Official:=FALSE]
data[flagObservationStatus%in%c("","T"),Protected:=TRUE]

#######################################
# The following copy is needed for saving back some of the intermediate
# files. These intermediate steps will come without flag and the flag
# will be merged with this original data object
dataFlags = copy(data)

##############################################################
# For DERIVED select only the protected and the estimation 
# (coming from the submodule of derived and Livestock)
# I have to select Protected and Estimation (I,e) and (I,i)
# For all the others delete the production value
# this will leave the Sua Filling creting prodcution, where needed

level = findProcessingLevel(tree, from = params$parentVar,
                            to = params$childVar, aupusParam = params)
primaryEl = level[processingLevel == 0, get(params$itemVar)]


#flamethod
# data[!(get(params$protected)=="TRUE"|(flagObservationStatus=="I"&flagMethod%in%c("i","e")))
#      &get(params$elementVar)==params$productionCode
#      &!(get(params$itemVar) %in% primaryEl),Value:=NA]




data[!(get(params$protected)=="TRUE"|(flagObservationStatus=="I"))
     &get(params$elementVar)==params$productionCode
     &!(get(params$itemVar) %in% primaryEl),Value:=NA]

##############################################################
##################   RECALCULATE SHARE   #####################
##########   FOR COMMODITIES MANUALLY ENTERED   ##############
##############################################################

p=params

### Compute availability and SHARE  

data[, availability := sum(ifelse(is.na(Value), 0, Value) *
                             ifelse(get(p$elementVar) == p$productionCode, 1,
                                    ifelse(get(p$elementVar) == p$importCode, 1,
                                           ifelse(get(p$elementVar) == p$exportCode, -1,
                                                  ifelse(get(p$elementVar) == p$stockCode, 0,
                                                         ifelse(get(p$elementVar) == p$foodCode, 0,
                                                                ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                       ifelse(get(p$elementVar) == p$feedCode, 0,
                                                                              ifelse(get(p$elementVar) == p$wasteCode, 0,
                                                                                     ifelse(get(p$elementVar) == p$seedCode, 0,
                                                                                            ifelse(get(p$elementVar) == p$industrialCode, 0,
                                                                                                   ifelse(get(p$elementVar) == p$touristCode, 0,
                                                                                                          ifelse(get(p$elementVar) == p$residualCode, 0, 0))))))))))))),
     by = c(p$mergeKey)]



mergeToTree = data[, list(availability = mean(availability)),
                   by = c(p$itemVar)]
setnames(mergeToTree, p$itemVar, p$parentVar)
plotTree = copy(tree)

tree2=copy(plotTree)

tree2shares=tree[measuredElementSuaFbs=="share"]
tree2shares[,share:=Value]
tree2shares[,c("measuredElementSuaFbs","Value"):=NULL]

tree2exRa=tree[measuredElementSuaFbs=="extractionRate"]
tree2exRa[,extractionRate:=Value]
tree2exRa[,c("measuredElementSuaFbs","Value"):=NULL]


tree2=data.table(left_join(tree2shares,tree2exRa[,c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC",
                                                    "timePointYears","extractionRate"),with=FALSE],
                           by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears")))

tree2=tree2[!is.na(extractionRate)]
tree2 = merge(tree2, mergeToTree, by = p$parentVar, all.x = TRUE)


#### SHAREs 1 
# share are the proportion of availability of each parent
# on the total availability by child
# Function checkShareValue() checks the validity of the shares,
# change wrong values
# return a severity table 
# and the values to be saved back in the session of the TRee

tree2[,checkFlags:=paste0("(",flagObservationStatus,",",flagMethod,")")]

tree2[,availability.child:=availability*get(p$extractVar)]

tree2[,shareSum:=sum(share),by=c("measuredItemChildCPC", "timePointYears")]

# Create eventual Errors HEre for testing the checkshares function 

uniqueShares2change = tree2[checkFlags=="(E,f)"&(round(shareSum,3)!=1|is.na(shareSum)), .N, by = c("measuredItemChildCPC", "timePointYears")]
uniqueShares2change[, N := NULL]


tree2[,newShare:=NA]
tree2[,severity:=NA]
tree2[,message:=NA]
tree2[,newShare:=as.numeric(newShare)]
tree2[,severity:=as.integer(severity)]
tree2[,message:=as.character(message)]

tree2change = vector(mode = "list", length = nrow(uniqueShares2change))

for (i in seq_len(nrow(uniqueShares2change))) {
  filter = uniqueShares2change[i, ]
  tree2Subset = tree2[filter, , on = c("measuredItemChildCPC", "timePointYears")]
  
  tree2change[[i]] = checkShareValue(tree2Subset)
  
}

tree2change = rbindlist(tree2change)

tree2merge=copy(tree2change)

# Before sending it via email, change flags

if(dim(tree2merge)[1]>0){
  tree2merge[checkFlags!="(E,f)",flagObservationStatus:="I"]
  tree2merge[checkFlags!="(E,f)",flagMethod:="i"]
  tree2merge[,c("checkFlags","availability.child","shareSum","availability","extractionRate"):=NULL]
}

# sendMail4shares(tree2merge)

# IF SHARES ARE VALID ( and Tree2change does not exists), The " tree" is the one to use 
# onwards
# nothing has to be done in this case
# IF THERE IS A tree2change, after it has been sent, it has to be integrated in the tree
# before going on

if(dim(tree2merge)[1]>0){
  setnames(tree2merge,"share","Value")
  tree2merge[,c("severity","message"):=NULL]
  tree2merge[,measuredElementSuaFbs:="share"]
  setcolorder(tree2merge,c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                           "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", 
                           "flagMethod"))
  uniquecomb = tree2merge[, .N, by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                                       "measuredItemChildCPC", "timePointYears")]
  uniquecomb[,N := NULL]
  
  tree=rbind(tree[!uniquecomb, ,on=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                                     "measuredItemChildCPC", "timePointYears")],tree2merge)
  
}


##############################################################
##############  LAST MANIPULATIONS ON TREE   #################
##############################################################

tree[,c("flagObservationStatus","flagMethod"):=NULL]
tree=data.table(dcast(tree,geographicAreaM49 + measuredItemParentCPC + measuredItemChildCPC + timePointYears
                      ~ measuredElementSuaFbs,value.var = "Value"))

tree=tree[!is.na(extractionRate)]
tree=tree[!is.na(measuredItemChildCPC)]

## Update tree by setting some edges to "F"
FPCommodities <- c( "01499.06", "01921.01")
# These commodities are forwards processed instead of backwards processed:
#        code             description type
# 3: 01499.06      Kapokseed in shell CRNP
# 4: 01921.01   Seed cotton, unginned CRPR  

tree[, target := ifelse(measuredItemParentCPC %in% FPCommodities,
                        "F", "B")]

# MERGE the TREE with the item Map fpr future manipulation

itemMap = fread("SUA-FBS Balancing/Data/itemMap.csv")



itemMap = itemMap[, c("code", "type"), with = FALSE]
setnames(itemMap, "code", "measuredItemSuaFbs")
data = merge(data, itemMap, by = "measuredItemSuaFbs")
setnames(itemMap, "measuredItemSuaFbs", "measuredItemParentCPC")
tree = merge(tree, itemMap, by = "measuredItemParentCPC")

## Remove missing elements
data = data[!is.na(measuredElementSuaFbs), ]

#flagmethod
# data=data[,c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49", 
#              "timePointYears", "Value", "flagObservationStatus", "flagMethod", 
#              "Valid", "Protected", "Official", "type"),with=FALSE]

data=data[,c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49", 
             "timePointYears", "Value", "flagObservationStatus", 
             "Valid", "Protected", "Official", "type"),with=FALSE]




# #######################################################
# # save the initial data locally for future reports
# if(CheckDebug()){
#   dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
# }
# if(CheckDebug()){
#   initialSua = data
#   save(initialSua,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_01_InitialSua_BeforeCB.RData"))
# }
#######################################################

#flagmethod
# data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Official","Protected","type","flagObservationStatus","flagMethod"))]

data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Official","Protected","type","flagObservationStatus"))]


#############################################################
##########    LOAD NUTRIENT DATA AND CORRECT    #############
#############################################################
message("Loading nutrient data...")

# itemKeys = GetCodeList("agriculture", "aupus_ratio", "measuredItemCPC")[, code]

# Nutrients are:
# 1001 Calories
# 1003 Proteins
# 1005 Fats
nutrientCodes = c("1001", "1003", "1005")

nutrientData =  fread("SUA-FBS Balancing/Data/nutrientData.csv")


setnames(nutrientData, c("measuredItemCPC", "timePointYearsSP"),
         c("measuredItemSuaFbs", "timePointYears"))

# It has been found that some Nutrient Values are wrong in the Nutrient Data Dataset

######### CREAM SWEDEN 

nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1001",Value:=195]
nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1003",Value:=3]
nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1005",Value:=19]

### MILK SWEDEN
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1001",Value:=387]
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1003",Value:=26]
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1005",Value:=30]

nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1001",Value:=310]
nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1003",Value:=23]
nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1005",Value:=23]

#################################################################
#################################################################
#################################################################
message("Download Utilization Table from SWS...")

# utilizationTable=ReadDatatable("utilization_table")
utilizationTable=fread("SUA-FBS Balancing/Data/utilization_table_percent.csv")



# setnames(utilizationTable,colnames(utilizationTable),c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs", 
#                                                        "rank", "rankInv"))

setnames(utilizationTable,colnames(utilizationTable),c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs", 
                                                       "percent","rank", "rankInv"))

message("Download zero Weight from SWS...")

zeroWeight=fread("SUA-FBS Balancing/Data/zeroWeight.csv")

zeroWeight<- unique(zeroWeight$x)


message("Download cutItems from SWS...")

cutItems=fread("SUA-FBS Balancing/Data/cutItems.csv")

cutItems<- unique(cutItems$x)

message("Download fbsTree from SWS...")
fbsTree=fread("SUA-FBS Balancing/Data/fbsTree.csv")



setnames(fbsTree,colnames(fbsTree),c( "fbsID1", "fbsID2", "fbsID3","fbsID4", "measuredItemSuaFbs"))
setcolorder(fbsTree,c("fbsID4", "measuredItemSuaFbs", "fbsID1", "fbsID2", "fbsID3"))

message("Defining vectorized standardization function...")

standardizationVectorized_DES = function(data, tree, nutrientData,batchnumber,
                                     utilizationTable,cutItems,fbsTree){
  
  # record if output is being sunk and at what level
  sinkNumber <- sink.number()
  # Prevent sink staying open if function is terminated prematurely (such as
  # in debugging of functions in standardization)
  on.exit(while (sink.number() > sinkNumber) sink())
  
  if (nrow(data) == 0) {
    message("No rows in data, nothing to do")
    return(data)
  }
  
  # If printCodes is length 0, neither the .md files nor plots are created
  # If it has a non-zero value, those are the codes which will have file outputs
  
  printCodes = character()
  
  # printCodes = c("01701")
  # printCodes = getChildren(commodityTree = tree,
  # parentColname = params$parentVar,
  # childColname = params$childVar,
  # topNodes = printCodes)
  
out = standardizationWrapper_DES(data = data, tree = tree, fbsTree = fbsTree, 
                                  standParams = params, printCodes = printCodes,
                                  nutrientData = nutrientData,
                                  debugFile = params$createIntermetiateFile
                                  ,batchnumber = batchnumber,
                                  utilizationTable = utilizationTable,
                                  cutItems=cutItems)
  return(out)
}

## Split data based on the two factors we need to loop over

data <- subset(data,timePointYears %in% yearVals)
uniqueLevels = data[, .N, by = c("geographicAreaM49", "timePointYears")]
uniqueLevels[, N := NULL]
parentNodes = fread("SUA-FBS Balancing/Data/parentNodes.csv")



parentNodes = parentNodes[level == 0, node] 

aggFun = function(x) {
  if (length(x) > 1)
    stop("x should only be one value!")
  return(sum(x))
}

standData = vector(mode = "list", length = nrow(uniqueLevels))

# Create Local Temporary File for Intermediate Savings






if(params$createIntermetiateFile){
  # if(file.exists(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))){
  #   file.remove(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))
  # }
  # if(file.exists(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))){
  #   file.remove(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))
  # }
  if(file.exists(paste0(basedir,"/SUA-FBS Balancing/StandardizedData/_02_AfterSuaFilling_BeforeST.csv"))){
    file.remove(paste0(basedir,"/SUA-FBS Balancing/StandardizedData/_02_AfterSuaFilling_BeforeST.csv"))
  }
  if(file.exists(paste0(basedir,"/SUA-FBS Balancing/StandardizedData/_03_AfterST_BeforeFBSbal.csv"))){
    file.remove(paste0(basedir,"/SUA-FBS Balancing/StandardizedData/_03_AfterST_BeforeFBSbal.csv"))
  }
  # if(file.exists(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_04_NotBalancedDerived.csv"))){
  #   file.remove(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_04_NotBalancedDerived.csv"))
  # }  
  # if(file.exists(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))){
  #   file.remove(paste0(basedir,"/debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))
  # }
}


message("Beginning actual standardization process...")

data2 = as.data.frame(data2)
data2 = dplyr::rename(data2,measuredItemSuaFbs=measuredItemFbsSua)
data2$Value[data2$Value==0] = NA
data2_Stock = data2 %>%
  dplyr::filter(measuredElementSuaFbs=="stock_change") %>%
  group_by(geographicAreaM49,measuredElementSuaFbs,measuredItemSuaFbs)%>%
  dplyr::summarise(Median_Value_Stock = median(abs(Value),na.rm=TRUE)) %>%
  ungroup()

data2_Stock = as.data.frame(data2_Stock)
data2_Stock = dplyr::select_(data2_Stock,"geographicAreaM49","measuredItemSuaFbs","Median_Value_Stock")

data2_Industrial = data2 %>%
  dplyr::filter(measuredElementSuaFbs=="industrial") %>%
  group_by(geographicAreaM49,measuredElementSuaFbs,measuredItemSuaFbs)%>%
  dplyr::summarise(Median_Value_Industrial = median(abs(Value),na.rm=TRUE)) %>%
  ungroup() 

data2_Industrial = as.data.frame(data2_Industrial)
data2_Industrial = dplyr::select_(data2_Industrial,"geographicAreaM49","measuredItemSuaFbs","Median_Value_Industrial")
rm(data2)
data = as.data.frame(data)
data = left_join(data,Feed_Items,by=c("measuredItemSuaFbs"="cpc_code"))
data = left_join(data,Fruit_Veg_Food,by=c("measuredItemSuaFbs"="cpc"))
data = left_join(data,Stock_Items,by=c("measuredItemSuaFbs"="cpc_code"))
data = left_join(data,data2_Stock,by=c("geographicAreaM49","measuredItemSuaFbs"))
data = left_join(data,data2_Industrial,by=c("geographicAreaM49","measuredItemSuaFbs"))
data$food_classification[is.na(data$food_classification)] = "NonVegFruitFood"
data$classification_feed[is.na(data$classification_feed)] = "NonFeedOnly"
data$classification_stock[is.na(data$classification_stock)] = "NonStock"
data=ungroup(data)
data = as.data.table(data)




files_DES_Remove=list.files(path = "SUA-FBS Balancing/StandardizedData",pattern ="^DESCPClevel_")


for (i in seq_len(length(files_DES_Remove))){
  
  
  if(file.exists(paste0(basedir,"/", files_DES_Remove[i]))){
    file.remove(paste0(basedir,"/", files_DES_Remove[i]))
  }
}




##  Run all the standardization and balancig for combination of country/year
ptm <- proc.time()
for (i in seq_len(nrow(uniqueLevels))) {
  filter = uniqueLevels[i, ]
  dataSubset = data[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset = tree[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset[, c("geographicAreaM49", "timePointYears") := NULL]
  subNutrientData = nutrientData[filter, , on = c("geographicAreaM49",
                                                  "timePointYears")]
  subNutrientData = dcast(measuredItemSuaFbs ~ measuredElement,
                          data = subNutrientData, value.var = "Value",
                          fun.agg = aggFun)
  setnames(subNutrientData, nutrientCodes,
           c("Calories", "Proteins", "Fats"))
  utilizationTableSubset = utilizationTable[get(params$geoVar)==as.character(uniqueLevels[i,1,with=FALSE])]
  
  standData[[i]] = standardizationVectorized_DES(data = dataSubset,
                                             tree = treeSubset,
                                             nutrientData = subNutrientData,
                                             batchnumber = batchnumber,
                                             utilizationTable = utilizationTableSubset,
                                             fbsTree=fbsTree
  )
  
 
}


}

