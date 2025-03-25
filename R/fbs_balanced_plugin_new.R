
fbs_balanced_plugin_new <- function(input,output,session){


  
  files = dir("SUA-FBS Balancing/R",full.names = TRUE)
  
  for(i in files){
    source(i, local = TRUE)
  }
  
  message("Getting parameters/datasets...")
  
COUNTRY <-as.character(unique(countryData$CountryM49))

COUNTRY_NAME <-as.character(unique(countryData$Country))
 

basedir <- getwd()
#tool_year <- c("2022")

# start and end year for standardization come from user parameters
startYear = as.numeric(input$fromyear)
endYear = as.numeric(input$endyear)
#geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = as.character(2014:endYear)
#outlierMail = swsContext.computationParams$checks

##  Get data configuration and session
#sessionKey_fbsBal = swsContext.datasets[[1]]
#sessionKey_suabal = swsContext.datasets[[2]]
#sessionKey_fbsStand = swsContext.datasets[[3]]


#sessionCountries =
 # getQueryKey("geographicAreaM49", sessionKey_fbsBal)

# geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
#                       dimension = "geographicAreaM49")[type == "country", code]

# Select the countries based on the user input parameter
# selectedGEOCode =
#   switch(geoM49,
#          "session" = sessionCountries,
#          "all" = geoKeys)

#selectedGEOCode = sessionCountries

#areaKeys = selectedGEOCode


##############################################################
############ DOWNLOAD AND VALIDATE TREE ######################
##############################################################

ptm <- proc.time()
message("Downloading tree...")
# giulia . nella funzioen get commodity tree new method dovremmo elimimnare il pull delle shares, nomn le usiamo piu
#tree=getCommodityTreeNewMethod(areaKeys,as.character(2000:endYear))

tree <- fread("SUA-FBS Balancing/Data/tree.csv")

tree <- subset(tree, timePointYears %in% yearVals )


if (COUNTRY == "835"){
  
  tree[, geographicAreaM49 := as.character(835)]
  
}

tree[Value==0,Value:=NA]
##################################################################
##########################FUNCTIONS###############################
#################################################################



calculateImbalance <- function(data,
                               supply_add = c("production", "imports"),
                               supply_subtract = c("exports", "stockChange"),
                               supply_all = union(supply_add, supply_subtract),
                               item_name = "measuredItemSuaFbs",
                               bygroup = c("geographicAreaM49", "timePointYears", item_name))
{
  
  stopifnot(is.data.table(data))
  
  data[measuredElementSuaFbs %!in% c("residual","TotalCalories","TotalProteins",
                                     "TotalFats","calories","proteins","fats"),
       `:=`(
         supply =
           sum(Value[measuredElementSuaFbs %chin% supply_add],
               - Value[measuredElementSuaFbs %chin% supply_subtract],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(measuredElementSuaFbs %chin% supply_all)],
               na.rm = TRUE)
       ),
       by = bygroup
  ][,
    imbalance := supply - utilizations
  ]
}

calculateError <- function(data,
                           supply_add = c("production", "imports"),
                           supply_subtract = c("exports", "stockChange"),
                           supply_all = union(supply_add, supply_subtract),
                           item_name = "measuredItemSuaFbs",
                           bygroup = c("geographicAreaM49", "timePointYears", item_name))
{
  
  stopifnot(is.data.table(data))
  
  data[measuredElementSuaFbs %!in% c("TotalCalories","TotalProteins",
                                     "TotalFats","calories","proteins","fats"),
       `:=`(
         supply =
           sum(Value[measuredElementSuaFbs %chin% supply_add],
               - Value[measuredElementSuaFbs %chin% supply_subtract],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(measuredElementSuaFbs %chin% c(supply_all, "residual"))],
               na.rm = TRUE)
       ),
       by = bygroup
  ][,
    error := round(supply - utilizations, digits=2)
  ]
}





# Replacement for merge(x, y, by = VARS, all.x = TRUE) that do not set keys
# By default it behaves as dplyr::left_join(). If nomatch = 0, non-matching
# rows will not be returned

dt_left_join <- function(x, y, by = NA, allow.cartesian = FALSE,
                         nomatch = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- y[x, on = by, allow.cartesian = allow.cartesian, nomatch = nomatch]
  
  setcolorder(res, c(names(x), setdiff(names(y), names(x))))
  
  res
}




computeFbsAggregate = function(data, fbsTree, standParams){
  ## Data Quality Checks
  stopifnot(standParams$itemVar %in% colnames(data))
  stopifnot(standParams$itemVar %in% colnames(fbsTree))
  stopifnot(paste0("fbsID", 1:4) %in% colnames(fbsTree))
  
  data = merge(data, fbsTree, by = standParams$itemVar, all.x=TRUE)
  data = data[is.na(fbsID1) & measuredItemSuaFbs%in% FoodItems, fbsID1:= "S2901"]
  data = data[is.na(fbsID4) & measuredItemSuaFbs%in% FoodItems, fbsID4:= measuredItemSuaFbs]
  
  
  out = list()
  
  out[[1]] = data[,list(Value = sum(Value, na.rm = TRUE)),
                  by = c(standParams$elementVar, standParams$yearVar,
                         standParams$geoVar, "fbsID4")]
  
  out[[2]] = data[, list(Value = sum(Value, na.rm = TRUE)),
                  by = c(standParams$elementVar, standParams$yearVar,
                         standParams$geoVar, "fbsID3")]
  
  out[[3]] = data[, list(Value = sum(Value, na.rm = TRUE)),
                  by = c(standParams$elementVar, standParams$yearVar,
                         standParams$geoVar, "fbsID2")]
  
  out[[4]] = data[get(standParams$elementVar) %in% c("261", "271", "281","664","674", "684"), 
                  list(Value = sum(Value, na.rm = TRUE)),
                  by = c(standParams$elementVar, standParams$yearVar,
                         standParams$geoVar, "fbsID1")]
  
  # out[[5]] = data[is.na(fbsID4) & standParams$itemVar %in% FoodItems ,
  #                 list(Value = sum(Value, na.rm = TRUE)),
  #                 by = c(standParams$elementVar, standParams$yearVar,
  #                        standParams$geoVar, "measuredItemSuaFbs")]
  
  return(out)
}

# giulia new function to get shares down up from sws
getShareDown = function(geographicAreaM49 = NULL, timePointYears = NULL){
  
  shareselemKeys = c("5432")
  
  sharesitemPKeys = GetCodeList(domain = "suafbs", dataset = "down_up_share", "measuredItemParentCPC_tree")
  sharesitemPKeys = sharesitemPKeys[, code]
  
  sharesitemCKeys = GetCodeList(domain = "suafbs", dataset = "down_up_share", "measuredItemChildCPC_tree")
  sharesitemCKeys = sharesitemCKeys[, code]
  
  shareskey = faosws::DatasetKey(domain = "suafbs", dataset = "down_up_share", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = shareselemKeys),
    measuredItemParentCPC = Dimension(name = "measuredItemParentCPC_tree", keys = sharesitemPKeys),
    measuredItemChildCPC = Dimension(name = "measuredItemChildCPC_tree", keys = sharesitemCKeys),
    timePointYears = Dimension(name = "timePointYears", keys = yearVals)
  ))
  
  
  
  SharesDown= faosws::GetData(shareskey)
  
  
  SharesDown[measuredElementSuaFbs=="5432",measuredElementSuaFbs:="share"]
  
  message("Sharedown correctly downloaded")
  
  setnames(SharesDown,c("measuredItemParentCPC_tree","measuredItemChildCPC_tree"),
           c("measuredItemParentCPC","measuredItemChildCPC"))
  
  return(SharesDown)  
  
}


# function to get shares up down from SWS

getShareUp = function(geographicAreaM49 = NULL, timePointYears = NULL){
  
  shareselemKeys = c("5431")
  
  sharesitemPKeys = GetCodeList(domain = "suafbs", dataset = "up_down_share", "measuredItemParentCPC_tree")
  sharesitemPKeys = sharesitemPKeys[, code]
  
  sharesitemCKeys = GetCodeList(domain = "suafbs", dataset = "up_down_share", "measuredItemChildCPC_tree")
  sharesitemCKeys = sharesitemCKeys[, code]
  
  shareskey = faosws::DatasetKey(domain = "suafbs", dataset = "up_down_share", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = shareselemKeys),
    measuredItemParentCPC = Dimension(name = "measuredItemParentCPC_tree", keys = sharesitemPKeys),
    measuredItemChildCPC = Dimension(name = "measuredItemChildCPC_tree", keys = sharesitemCKeys),
    timePointYears = Dimension(name = "timePointYears", keys = yearVals)
  ))
  
  
  
  SharesUp= faosws::GetData(shareskey)
  
  
  SharesUp[measuredElementSuaFbs=="5431",measuredElementSuaFbs:="share_Up"]
  
  message("ShareUp correctly downloaded")
  
  setnames(SharesUp,c("measuredItemParentCPC_tree","measuredItemChildCPC_tree"),
           c("measuredItemParentCPC","measuredItemChildCPC"))
  
  return(SharesUp)  
  
}


`%!in%` = Negate(`%in%`)


###########END FUNCTION--------------------------------------------

#LoadShareDownUp and UPdown
#shares=getShareDown(areaKeys,yearVals)

#if(file.exists(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/shares.csv"))){
  #file.remove(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/shares.csv"))
 # write.csv(shares,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/shares.csv"),row.names = FALSE)
#}else{
  #write.csv(shares,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/shares.csv"),row.names = FALSE)
  
#}

shares <- fread("/SUA-FBS Balancing/FBS_Balanced/Data/shares.csv")


#sharesUP=getShareUp(areaKeys, yearVals)

#if(file.exists(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/sharesUP.csv"))){
 # file.remove(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/sharesUP.csv"))
 # write.csv(sharesUP,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/sharesUP.csv"),row.names = FALSE)
#}else{
  #write.csv(sharesUP,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/sharesUP.csv"),row.names = FALSE)
  
#}

sharesUP <- fread("/SUA-FBS Balancing/FBS_Balanced/Data/sharesUP.csv")

################################################################
message("Download Utilization Table from SWS...")

Utilization_Table <- fread("SUA-FBS Balancing/Data/utilization_table_2018.csv")

DerivedItem <- Utilization_Table[derived == 'X', get("cpc_code")]

ProxyPrimary <- Utilization_Table[proxy_primary == 'X', get("cpc_code")]

Primary <- Utilization_Table[primary_item == 'X', get("cpc_code")]

Orphan <- Utilization_Table[orphan== 'X', get("cpc_code")]

FoodItems<- Utilization_Table[food_item=="X",get("cpc_code")]


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
params$foodProcCode = "foodmanufacturing"
params$residualCode = "residual"
params$createIntermetiateFile= "TRUE"
params$protected = "Protected"
params$official = "Official"
params$calories = "calories"
params$proteins = "proteins"
params$fats = "fats"


##############################################################
######## CLEAN ALL SESSION TO BE USED IN THE PROCESS #########
##############################################################
## CLEAN fbs_standardized
message("wipe fbs_standardized session")

# CONFIG <- GetDatasetConfig(sessionKey_fbsStand@domain, sessionKey_fbsStand@dataset)
# 
# fbs_standardized=GetData(sessionKey_fbsStand)
# fbs_standardized=fbs_standardized[timePointYears%in%yearVals]
# 
# fbs_standardized[, Value := NA_real_]
# fbs_standardized[, CONFIG$flags := NA_character_]
# 
# if(file.exists(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_standardized_wipe.csv"))){
#   file.remove(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_standardized_wipe.csv"))
#   write.csv(fbs_standardized,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_standardized_wipe.csv"),row.names = FALSE)
# }else{
#   
#   write.csv(fbs_standardized,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_standardized_wipe.csv"),row.names = FALSE)
# }

fbs_standardized <- fread(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_standardized_wipe.csv"))

if (COUNTRY == "835"){
  
  fbs_standardized[, geographicAreaM49 := as.character(835)]
  
}


## CLEAN fbs_balanced
message("wipe fbs_balanced session")

# CONFIG <- GetDatasetConfig(sessionKey_fbsBal@domain, sessionKey_fbsBal@dataset)
# 
# fbs_balancedData=GetData(sessionKey_fbsBal)
# fbs_balancedData=fbs_balancedData[timePointYears%in%yearVals]
# 
# fbs_balancedData[, Value := NA_real_]
# fbs_balancedData[, CONFIG$flags := NA_character_]
# 
# if(file.exists(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balancedData_wipe.csv"))){
#   file.remove(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balancedData_wipe.csv"))
#   write.csv(fbs_balancedData,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balancedData_wipe.csv"),row.names = FALSE)
# }else{
#   
#   write.csv(fbs_balancedData,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balancedData_wipe.csv"),row.names = FALSE)
# }

fbs_balancedData <- fread(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balancedData_wipe.csv"))

if (COUNTRY == "835"){
  
  fbs_balancedData[, geographicAreaM49 := as.character(835)]
  
}



#SaveData(CONFIG$domain, CONFIG$dataset , data = fbs_balancedData, waitTimeout = Inf)

##############################################################
#################### SET KEYS FOR importing SUA BAL DATA #######################
##############################################################



# desKeys = c("664")
# elemKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredElementSuaFbs")
# elemKeys = elemKeys[, code]
# 
# itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredItemFbsSua")
# itemKeys = itemKeys[, code]
# 
# 
# key = DatasetKey(domain = "suafbs", dataset = "sua_balanced", dimensions = list(
#   geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
#   measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
#   measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
#   timePointYears = Dimension(name = "timePointYears", keys = as.character(2000:tool_year))))


##############################################################
####################### DOWNLOAD  DATA #######################
##############################################################

# message("Reading SUA data...")
#get sua bal Data
# CONFIG <- GetDatasetConfig(sessionKey_suabal@domain, sessionKey_suabal@dataset)

SuabalData=fread("SUA-FBS Balancing/Data/sua_balanced.csv")

SuabalData <- subset(SuabalData, measuredElementSuaFbs %in% 
                       c("5113", "5510", "5610", "5071", "5910", "5520", "5525", "5016", "5023", "5165",
                         "5166", "5141",  "664",  "665",  "511", new_nutrient_element))

SuabalData=SuabalData[timePointYears%in%yearVals]

setnames(SuabalData,"measuredItemFbsSua",params$itemVar)



# sua_bal_2010_2013 <- SuabalData[timePointYears %in% c(2010:2013) ]
# sua_bal_2010_2013 <- sua_bal_2010_2013[,flagMethod := NULL]
# 
# if(file.exists(paste0(basedir,"/SUA-FBS Balancing/Data/sua_bal_2010_2013.csv"))){
#   file.remove(paste0(basedir,"/SUA-FBS Balancing/Data/sua_bal_2010_2013.csv"))
#   write.csv(sua_bal_2010_2013,paste0(basedir,"/SUA-FBS Balancing/Data/sua_bal_2010_2013.csv"),row.names = FALSE)
# }else{
#   
#   write.csv(sua_bal_2010_2013,paste0(basedir,"/SUA-FBS Balancing/Data/sua_bal_2010_2013.csv"),row.names = FALSE)
# }

sua_bal_2010_2013 <- fread("/SUA-FBS Balancing/Data/sua_bal_2010_2013.csv")

#Sua balanced only quantities
elemKeysq=c("5510", "5610", "5071", "5023", "5910", "5016", 
            "5165", "5520","5525","5164","5141", "5166")

data=SuabalData[get(params$elementVar) %in% elemKeysq,]

data=data[timePointYears%in%yearVals]
# data= data[Value!=0]
# giulia, la funzione non funziona, ci sono missing value nelle tablelle che usa, fai un'attribuzione manuale dei n omi
# data=elementCodesToNames(data,standParams = params)
codes <- tibble::tribble(
  ~measuredElementSuaFbs,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodmanufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5071", "stockChange",
  "664", "calories",
  "674", "proteins",
  "684", "fats",
  "5166", "residual",
  "5164", "tourist",
  "261", "TotalCalories",
  "271", "TotalProteins",
  "281", "TotalFats"
)

setDT(codes)
data <- dt_left_join(data, codes, by = "measuredElementSuaFbs")

data[, measuredElementSuaFbs := name]

data[, name := NULL]


#Sua balanced DES
# dataDes<-SuabalData[get(params$elementVar) %in% desKeys]
# dataDes[get(params$elementVar)=="664",params$elementVar:=params$calories]

#Using the whole tree not by level

ExtrRate <-
  tree[
    !is.na(Value) &
      measuredElementSuaFbs == 'extractionRate'
  ][,
    .(
      measuredItemParentCPC,
      geographicAreaM49,
      measuredItemChildCPC,
      timePointYears,
      extractionRate = Value
    )
  ]
ExtrRate<-ExtrRate[timePointYears %in% yearVals]

ExtrRate [,geographicAreaM49 := as.character(geographicAreaM49)]
ExtrRate [,timePointYears := as.character(timePointYears)]
# merge sua data with the tree

data_tree <-copy(data)


setnames(data_tree, "measuredItemSuaFbs", "measuredItemParentCPC")

data_tree <-
  merge(
    data_tree,
    ExtrRate,
    by = c(params$parentVar, params$geoVar, params$yearVar),
    allow.cartesian = TRUE,
    all.y =    TRUE
  )

data_tree <- as.data.table(data_tree)

emptychild<- data_tree[ ,c(2,3,8), with=FALSE]
setnames(emptychild, "measuredItemChildCPC", "measuredItemSuaFbs")
emptychild<- unique(emptychild)
emptychild<-merge(emptychild, data, by=c("geographicAreaM49", "timePointYears","measuredItemSuaFbs"), all.x=TRUE)
emptychild<- emptychild[ , emptychild:=ifelse(is.na(Value), TRUE, FALSE)]
emptychild<- emptychild[ , c(1,2,3,8), with=FALSE]
emptychild<- unique(emptychild)
setnames(emptychild, "measuredItemSuaFbs", "measuredItemChildCPC")

# remove from datatree connections where the child does not exist

data_tree<-merge(data_tree, emptychild, by= c("geographicAreaM49", "timePointYears", "measuredItemChildCPC"), all.x=TRUE)
data_tree<- data_tree[ emptychild==FALSE]
data_tree<- data_tree[ , emptychild:=NULL]

#country orphans: item with extraction rate in commodity tree but no values for parent
countryOrphans <- unique(data_tree[is.na(Value)])
countryOrphans<- countryOrphans[ ,c(1,2,3,8,9), with= FALSE]
setnames(countryOrphans, "measuredItemChildCPC", "measuredItemSuaFbs")
countryOrphans<- merge(countryOrphans,data, by=c("measuredItemSuaFbs", "geographicAreaM49", "timePointYears"))

countryOrphans<- countryOrphans[ , c(1,3), with =FALSE]
setnames(countryOrphans, "measuredItemSuaFbs", "measuredItemChildCPC")
countryOrphans<- unique(countryOrphans)


data_tree<- data_tree[!is.na(Value)]

# subset derived data for which we do not have production and the connection in the tree is missing. 
# This data needs to be standardized using global extraction rates and availability based on supply

connectedChild<-unique(data_tree$measuredItemChildCPC)
data_derived<-unique(data[measuredItemSuaFbs %in% DerivedItem]$measuredItemSuaFbs)
Notconnected<- setdiff(data_derived,connectedChild)
Notconnected<- setdiff(Notconnected, Orphan)


# Global_ER<-ReadDatatable("global_er")
# 
# 
# if(file.exists(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/Global_ER.csv"))){
#   file.remove(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/Global_ER.csv"))
#   write.csv(Global_ER,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/Global_ER.csv"),row.names = FALSE)
# }else{
#   
#   write.csv(Global_ER,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/Global_ER.csv"),row.names = FALSE)
# }

Global_ER <- fread(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/Global_ER.csv"))


colnames(Global_ER)<- c("measuredItemParentCPC", "measuredItemChildCPC", "global_er")
Global_ER$global_er<- as.numeric(Global_ER$global_er)

# items for which we don't have global ER and they are not orphans. To be sent by email
missingER<- setdiff(Notconnected, unique(Global_ER$measuredItemChildCPC))

# converting the list of items missing ER to a table with description to be sent by email to users

missingERTable <- data.table(
  geographicAreaM49 = COUNTRY,
  measuredItemFbsSua = c(missingER)
)
#convert to data table in case the missingER list is empty
missingERTable <- as.data.table(missingERTable)

# missingERTable <- nameData("suafbs", "sua_balanced", missingERTable)

#if(length(selectedGEOCode)==1){
#tmp_file_missingER<- tempfile(pattern = paste0("missingER_", COUNTRY_NAME, "_"), fileext = '.csv')
#}else{
#tmp_file_missingER<- tempfile(pattern = paste0("missingER_"), fileext = '.csv')
#}
#write.csv(missingERTable, tmp_file_missingER,row.names = FALSE)



# items to be standardized with global ER
Notconnected<- Global_ER[measuredItemChildCPC %in% Notconnected]
Notconnected<- Notconnected[ , geographicAreaM49:= COUNTRY]


yearsTable <-
  CJ(
    geographicAreaM49 = unique(data$geographicAreaM49),
    timePointYears = as.character(startYear:endYear),
    measuredItemParentCPC=unique(Notconnected$measuredItemParentCPC),
    measuredItemChildCPC=unique(Notconnected$measuredItemChildCPC)
  )
Notconnected<-merge(yearsTable, Notconnected, by= c("geographicAreaM49",
                                                    "measuredItemParentCPC", "measuredItemChildCPC"), all=TRUE)

Notconnected<-Notconnected[!is.na(global_er)]
# merge the not connected children with data

Notconnected_data <-copy(data)

setnames(Notconnected_data, "measuredItemSuaFbs", "measuredItemParentCPC")

Notconnected_data <-
  merge(
    Notconnected_data,
    Notconnected,
    by = c(params$parentVar, "timePointYears", "geographicAreaM49"),
    allow.cartesian = TRUE,
    all.y =    TRUE
  )

# add to country orphans the children with a global ER but parent has no value

countryOrphansTOT<-copy(Notconnected_data)
countryOrphansTOT<- countryOrphansTOT[ , NoParent:=all(is.na(Value)) , by= c("timePointYears", "geographicAreaM49", "measuredItemChildCPC")]
countryOrphansTOT<-countryOrphansTOT[NoParent==TRUE]
countryOrphansTOT<-countryOrphansTOT[ , c(2,8), with= FALSE]
countryOrphansTOT<- rbind(countryOrphans, countryOrphansTOT)
countryOrphansTOT<- countryOrphansTOT[ , geographicAreaM49:= COUNTRY]

Notconnected_data<- Notconnected_data[ !is.na(Value)]

# merge the tree connections data with the global connections data

data_tree<- merge(data_tree, Notconnected_data, by= c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC",
                                                      "timePointYears", "Value","measuredElementSuaFbs", "flagObservationStatus", 
                                                      "flagMethod"), all = TRUE)

data_tree[,
          availability :=
            sum(
              Value[measuredElementSuaFbs %in% c('production', 'imports')],
              - Value[measuredElementSuaFbs %in% c('exports', "stockChange")],
              na.rm = TRUE
            ),
          by = c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears")
]

data_tree<- data_tree[availability<0 , availability:=0]

importShares<-data[ measuredItemSuaFbs%in% unique(data_tree$measuredItemChildCPC)]
importShares<- importShares[ , ImportShare:= (Value[measuredElementSuaFbs== "imports"])/
                               sum(
                                 Value[measuredElementSuaFbs=="production"],
                                 Value[measuredElementSuaFbs=="imports"],
                                 na.rm = TRUE
                               ),
                             by = c("geographicAreaM49","measuredItemSuaFbs","timePointYears")]

importShares<- importShares[ ,c(1,3,4,8), with=FALSE]
importShares<- unique(importShares)

importShares<- importShares[is.na(ImportShare), ImportShare:= 0]
setnames(importShares, "measuredItemSuaFbs", "measuredItemChildCPC")

data_tree<- merge(data_tree, importShares, by=c("measuredItemChildCPC", "geographicAreaM49", "timePointYears"), all.x = TRUE)

# take shares down up imported from the dataset in sws
setnames(shares, "Value", "share")
shares<-shares[ , c(2,7,8):=NULL]
data_tree<- dt_left_join(data_tree, shares,by= c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears"))


# last modification dec 23 - recalculate real down up shares for imputed derived production (they can be different from shares down saved in sws)
Protected_prod<- copy(data)
Protected_prod<-Protected_prod[ , protectedProd:=
                                  ifelse(flagObservationStatus[measuredElementSuaFbs=="production"] %in% c('',"T","E"),
                                         TRUE,FALSE), by= c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")]

Protected_prod<-Protected_prod[is.na(protectedProd), protectedProd:=FALSE]
Protected_prod<-unique(Protected_prod[ , c(1,3,4,8), with=FALSE])

setnames(Protected_prod, "measuredItemSuaFbs", "measuredItemChildCPC")
data_shares<- copy(data_tree)

data_shares<- merge(data_shares, Protected_prod, by= c("geographicAreaM49", "measuredItemChildCPC", "timePointYears"), all.x = TRUE )

# merge with shares up down 
setnames(sharesUP, "Value", "shareUp")
sharesUP<-sharesUP[ , c(2,7,8):=NULL]

data_shares<- dt_left_join(data_shares, sharesUP, by= c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears"))

# zero weight need to be treated separately

# coproduct_table <- ReadDatatable('zeroweight_coproducts')

coproduct_table <- fread(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/coproduct_table.csv"))


coproduct_table<-coproduct_table[!is.nan(branch)]
coproduct_table <- coproduct_table[, .(measured_item_child_cpc, branch)]

# dataset containing informations of zeroweight commodities
# data_zeroweight <- data_tree[measuredItemChildCPC %chin% zeroWeight]

# import data for coproduct relation
coproduct_table <-
  coproduct_table[,
                  .(zeroweight = measured_item_child_cpc, measuredItemChildCPC = branch)
  ]

coproduct_table <- unique(coproduct_table, by = c("measuredItemChildCPC", "zeroweight"))

# We subset the zeroweight coproduct reference table by taking only zeroweights and their coproduct
# that are childcommodities in the tree of the country
coproduct_table <-
  coproduct_table[
    measuredItemChildCPC %chin% data_tree$measuredItemChildCPC &
      zeroweight %chin% data_tree$measuredItemChildCPC
  ]
# zeroWeight=ReadDatatable("zero_weight")[,item_code]

zeroweight <- fread("SUA-FBS Balancing/Data/zeroWeight.csv")

# Computing real shares for non-zeroweight commodities
data_shares <- data_shares[measuredItemChildCPC %!in% zeroWeight]

# compute processed for each parent x to a child y (called processed to child)
data_shares<-data_shares[ , processed_tochild:= 
                            Value[measuredElementSuaFbs=="foodmanufacturing"]*shareUp*extractionRate,
                          by=c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC","timePointYears")]

data_shares<-data_shares[measuredElementSuaFbs=="foodmanufacturing"]

data_shares<-data_shares[ ,share_real:= processed_tochild / sum(processed_tochild, na.rm = TRUE),
                          by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")]

# data_shares<- data_shares[, anyProtected:=ifelse(any(protectedProd==TRUE), TRUE, FALSE),
#                           by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")]    

data_shares<- data_shares[protectedProd==FALSE & !is.na(share_real), share:=share_real]

# computing real shares for zero weight
zw_shares<-merge(data_shares,coproduct_table, all.y  =TRUE, by=c("measuredItemChildCPC"))
zw_shares<-zw_shares[!is.na(measuredItemParentCPC)]

zw_shares[ , measuredItemChildCPC:=zeroweight]
zw_shares[ , processed_back:=processed_tochild/extractionRate]
zw_shares<- zw_shares[ , processed_back:=sum(processed_back, na.rm = TRUE), by=c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "measuredItemChildCPC")]

zw_shares<-unique(zw_shares[ , c(1,2,3,4,18,19), with=FALSE])

zw_shares<-merge(zw_shares, data_tree,all.x = TRUE, by=c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "measuredItemChildCPC"))

zw_shares<-zw_shares[ measuredElementSuaFbs=="foodmanufacturing"]
zw_shares<-zw_shares[ , processed_back:=processed_back*extractionRate]

zw_shares<-zw_shares[ ,share_real:= processed_back/ sum(processed_back, na.rm = TRUE),
                      by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")]

# zw_shares<- zw_shares[Protected_prod==FALSE & !is.na(share_real), share:=share_real]
zw_shares<- zw_shares[!is.na(share_real), share:=share_real]

shares_corrected<- rbind(zw_shares[ ,c(1,2,3,4,15), with=FALSE], data_shares[ ,c(1,2,3,4,13), with=FALSE])
setnames(shares_corrected, "share", "share_corrected")


# compute shares based on availability for not connected children

AvailabilityShares = data_tree[, c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "measuredItemChildCPC", "availability"), with= FALSE]
AvailabilityShares<- unique(AvailabilityShares,by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears"))
AvailabilityShares[, AvailabilityShare := availability / sum(availability, na.rm = T), by =c("timePointYears", "measuredItemChildCPC","geographicAreaM49")]
AvailabilityShares[ , number_of_parent := .N,
                    by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
]

# if supply=0, 0/0 gives NA, we therefore standardize the child proportionally to all parent
AvailabilityShares[is.na(AvailabilityShare),
                   AvailabilityShare:= 1/number_of_parent, by =c("timePointYears", "measuredItemChildCPC", "geographicAreaM49") ]
AvailabilityShares<- AvailabilityShares[ , c(1,2,3,4,6), with= FALSE]

data_tree<- merge(data_tree, AvailabilityShares, by= c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears"), all.x=TRUE)



data_tree<- data_tree[ is.na(extractionRate), extractionRate:= global_er]
data_tree<- data_tree[ , c(1,2,3,4,9,12,13,14), with=FALSE]
data_tree<- unique(data_tree)

data_tree<- merge(data_tree, shares_corrected, all.x=TRUE, by= c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears"))

data_tree<-data_tree[ !is.na(share_corrected), share:= share_corrected]
data_tree<-data_tree[ , share_corrected:=NULL
]
data_tree<- data_tree[ ,sumShares:=sum(share, na.rm = TRUE), by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")]

# adjust the shares to have sum=1
# 
data_tree<- data_tree[sumShares!=1 , share:= share/sumShares]
# data_tree<- data_tree[ ,sumShares:=sum(share, na.rm = TRUE), by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")]

data_tree<- data_tree[ sumShares==0, share:=AvailabilityShare]

data_tree<- data_tree[ , c("sumShares", "AvailabilityShare"):=NULL]
#######################################

p=params

tree<-data_tree

message("Download fbs groups structure from SWS...")

# fbsTree=ReadDatatable("fbs_tree")
fbsTree <- fread("SUA-FBS Balancing/Data/fbsTree.csv")

fbsTree=data.table(fbsTree)
setnames(fbsTree,colnames(fbsTree),c( "fbsID1", "fbsID2", "fbsID3","fbsID4", "measuredItemSuaFbs"))
setcolorder(fbsTree,c("fbsID4", "measuredItemSuaFbs", "fbsID1", "fbsID2", "fbsID3"))

fbsTree[ ,  `:=`
         (fbsID4 = paste0("S", get("fbsID4")),
           fbsID3 = paste0("S", get("fbsID3")),
           fbsID2 = paste0("S", get("fbsID2")),
           fbsID1 = paste0("S", get("fbsID1")))]

#############################################################
##########    LOAD NUTRIENT DATA    #############
#############################################################
# giulia, prendi dataDES da sua balanced

desKeys = c("664","674","684", "261", "271", "281")
dataDes<-SuabalData[measuredElementSuaFbs %in% desKeys]
# #################################################################

tree[,weight:=1]
tree[measuredItemChildCPC %in% zeroWeight , weight:=0]



tree <- 
  left_join(
    tree,
    
    fbsTree[,
            list(
              measuredItemChildCPC = measuredItemSuaFbs,
              fbsID4
            )
    ],
    by = c(  'measuredItemChildCPC')
  )
setnames(tree, "fbsID4", "groupChild")

tree <- 
  left_join(
    tree,
    
    fbsTree[,
            list(
              measuredItemParentCPC = measuredItemSuaFbs,
              fbsID4
            )
    ],
    by = c(  'measuredItemParentCPC')
  )

setnames(tree, "fbsID4", "groupParent")

# standardize only children under the same FBS group, children with parent in different group should be 
# treated as proxy primary(merged to the following data batch)
AggregateOnly<-as.data.table(copy(tree))
AggregateOnly<-AggregateOnly[ ,sameGroup:=ifelse(groupChild==groupParent, TRUE,FALSE)]
AggregateOnly<-AggregateOnly[ ,Not_toAggregate:=ifelse(any(sameGroup)==TRUE,TRUE,FALSE),
                              by=c("geographicAreaM49","measuredItemChildCPC","timePointYears" )]

AggregateOnly<-AggregateOnly[Not_toAggregate==FALSE]
AggregateOnly<-AggregateOnly[ ,c(1,2,3,4,5,6,7,8), with=FALSE]
Aggregate_only<-unique(AggregateOnly$measuredItemChildCPC)

tree<- as.data.table(tree)
tree<- tree[ , c("groupChild", "groupParent"):=NULL]

# if the parent is a zero weight the child becomes a zero weight, otherwise the production is standardized and we have a double counting

tree[,weight_parent:=1]
tree[measuredItemParentCPC %in% zeroWeight & measuredItemParentCPC %!in% Aggregate_only 
     , weight_parent:=0]
tree[ weight_parent==0, weight:=0]

# also, if the parent is zero weight, the child need to be standardized using the import share of the parent
setnames(importShares, "measuredItemChildCPC", "measuredItemParentCPC")
setnames(importShares, "ImportShare", "ImpShare_parent")

tree<- as.data.table(merge(tree, importShares, by= c("geographicAreaM49", "timePointYears","measuredItemParentCPC"), all.x = TRUE))
tree[ weight_parent==0 & !is.na(ImpShare_parent), ImportShare:=ImportShare+(1-ImportShare)*ImpShare_parent]
tree[weight_parent==0 & !is.na(ImpShare_parent)& measuredItemChildCPC%in%Aggregate_only, ImportShare:=ImpShare_parent]
# tree[ , weight_parent:=NULL]
tree[ , ImpShare_parent:=NULL]


message("Defining vectorized standardization function...")

## Split data based on the two factors we need to loop over
uniqueLevels = data[, .N, by = c("geographicAreaM49", "timePointYears")]
uniqueLevels[, N := NULL]




standData = vector(mode = "list", length = nrow(uniqueLevels))
standData0 = vector(mode = "list", length = nrow(uniqueLevels))
edgeData = vector(mode = "list", length = nrow(uniqueLevels))



#########STANDARDIZATION AND AGGREGATION-----------------------------------
message("Beginning actual standardization process...")

# giulia, figli di zero weight zero weight. quando calcolo il rpocessed dei proxy uso importshare

for (i in seq_len(nrow(uniqueLevels))) {
  
  # i=1
  
  message(paste("Standardizing ",uniqueLevels$geographicAreaM49[i]," for the year ",uniqueLevels$timePointYears[i]))
  
  filter = uniqueLevels[i, ]
  dataSubset = data[filter, , on = c("geographicAreaM49", "timePointYears")]
  dataDesSubset = dataDes[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset = tree[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset[, c("geographicAreaM49", "timePointYears") := NULL]
  countryOrphansYear = countryOrphansTOT[ filter, ,on = c("geographicAreaM49", "timePointYears")]
  
  standParams<-p
  
  keyCols = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
  
  treeSubset[, c(standParams$yearVar) := dataSubset[, get(standParams$yearVar)][1]]
  treeSubset[, c(standParams$geoVar) := dataSubset[, get(standParams$geoVar)][1]]
  
  targetNodes = Utilization_Table[proxy_primary == 'X' | primary_item == 'X'| orphan == 'X',
                                  get("cpc_code")]
  
  targetNodes = c(targetNodes,countryOrphansYear$measuredItemChildCPC, missingER, Aggregate_only)
  
  edges = treeSubset
  parentName = standParams$parentVar
  childName = standParams$childVar
  extractionName = standParams$extractVar
  
  
  targetNodes_data<- edges[ measuredItemParentCPC %in% targetNodes]
  
  tobe_stand<- edges[ !measuredItemParentCPC %in% targetNodes]
  
  level<- findProcessingLevel(edgeData = treeSubset, from = parentName,
                              to = childName)
  setnames(level, "temp", "measuredItemParentCPC")
  
  treeLevels <- dt_left_join(tobe_stand, level, by = "measuredItemParentCPC")
  treeLevels<- treeLevels[processingLevel!=0]
  treeLevels<-treeLevels[ , pseudoProxy:=FALSE]
  
  for (lev in sort(unique(treeLevels$processingLevel), decreasing = TRUE)) {
    #testing purpose
    # lev=1
    
    treeCurrentLevel <-
      treeLevels[
        processingLevel == lev
      ]
    
    
    edgesCopy = copy(edges[, c(parentName, childName, extractionName,p$shareVar,"weight",
                               keyCols), with = FALSE])
    
    
    edgesCopy<-edgesCopy[!measuredItemChildCPC%in%targetNodes]
    
    setnames(edgesCopy, c(parentName, childName, extractionName,p$shareVar,"weight"),
             c("newParent", parentName, "extractionMult","share.parent","weight.Parent"))
    
    treeCurrentLevel = merge(treeCurrentLevel, edgesCopy, by = c(parentName, keyCols),
                             all.x = TRUE, allow.cartesian = TRUE)
    # new correct method
    treeCurrentLevel[measuredItemParentCPC%in% targetNodes, `:=`
                     (newParent=measuredItemParentCPC,
                       extractionMult= 1,
                       share.parent = 1, 
                       weight.Parent = 1)]
    
    treeCurrentLevel[is.na(share.parent), share.parent:=0]
    
    
    treeCurrentLevel[, c(p$shareVar) := ifelse(!is.na(share.parent), get(p$shareVar) *(share.parent), share),
                     by=c(p$geoVar,p$yearVar,childName,parentName)
    ]
    # old amsata method (give sum of shares > than 1 in specific cases)
    # treeCurrentLevel[, c(p$shareVar) := ifelse(!is.na(share.parent),get(p$shareVar) *(share.parent/sum(share.parent,na.rm = TRUE)),share),
    #                  by=c(p$geoVar,p$yearVar,childName,parentName)
    # ]
    
    # tag item that go out from the group but his parent goes put of the group as well:
    # those items should not be accounted in the processed at fbs level
    # (ex. if malt and beer are coming from wheat, but beer is a child of malt, only malt should be accounted in the processed of WHEAT & PROD)
    treeCurrentLevel <- 
      left_join(
        treeCurrentLevel,
        
        fbsTree[,
                list(
                  measuredItemChildCPC = measuredItemSuaFbs,
                  fbsID4
                )
        ],
        by = c(  'measuredItemChildCPC')
      )
    setnames(treeCurrentLevel, "fbsID4", "groupChild")
    
    treeCurrentLevel <- 
      left_join(
        treeCurrentLevel,
        
        fbsTree[,
                list(
                  measuredItemParentCPC = measuredItemSuaFbs,
                  fbsID4
                )
        ],
        by = c(  'measuredItemParentCPC')
      )
    
    setnames(treeCurrentLevel, "fbsID4", "groupParent1")
    
    treeCurrentLevel <- 
      left_join(
        treeCurrentLevel,
        
        fbsTree[,
                list(
                  newParent = measuredItemSuaFbs,
                  fbsID4
                )
        ],
        by = c(  'newParent')
      )
    
    setnames(treeCurrentLevel, "fbsID4", "groupParent2")  
    
    treeCurrentLevel<-as.data.table(treeCurrentLevel)
    treeCurrentLevel<- treeCurrentLevel[ , pseudoProxy:= ifelse(groupChild!=groupParent2 & groupParent1!=groupParent2, TRUE, FALSE)]
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    treeCurrentLevel[, c(parentName) := ifelse(is.na(newParent), get(parentName),
                                               newParent)]
    treeCurrentLevel[, c(extractionName) := get(extractionName) *
                       ifelse(is.na(extractionMult), 1, extractionMult)]
    
    treeCurrentLevel[, c("newParent", "extractionMult","share.parent","weight.Parent","groupChild","groupParent1", "groupParent2") := NULL]
    
    
    treeCurrentLevel<- treeCurrentLevel[ , processingLevel:= (processingLevel-1)]
    
    treeLevels<- treeLevels[ !processingLevel==lev]
    
    treeCurrentLevel<-treeCurrentLevel[order(measuredItemParentCPC,measuredItemChildCPC, extractionRate,share,weight,
                                             timePointYears, geographicAreaM49)]
    treeLevels<-rbind(treeLevels, treeCurrentLevel)
    
  }
  
  treeLevels<-treeLevels[,processingLevel:=NULL]
  pseudoProxy<-treeLevels[ , c(1,2,8,9,10), with=FALSE]
  pseudoProxy<-unique(pseudoProxy)
  treeLevels<-treeLevels[ ,pseudoProxy:=NULL]
  
  finalEdges = rbind(targetNodes_data, treeLevels)
  
  edgeData[[i]] <- finalEdges         
  
  
  
  #standardize quantities and take target nodes data
  
  dataQTY=dataSubset
  dataDES=dataDesSubset
  standTree=finalEdges
  params=standParams
  
  
  ## Merge the tree with the children data to be standardized
  
  setnames(dataQTY, params$itemVar, params$childVar)
  
  dataQTY <- dataQTY[ , c("timePointYears", "geographicAreaM49","measuredElementSuaFbs", "measuredItemChildCPC",
                          "Value"), with = FALSE ]
  
  dataQTY <-merge(
    finalEdges, 
    dataQTY,
    by = c(params$yearVar, params$geoVar, params$childVar),
    all.x = TRUE, 
    allow.cartesian = TRUE
  )
  
  #  to be stardardized: all the derived items except Primary, Proxy primary, orphans 
  # and except all the derived in a different fbs group (Zero weight have a partial standardization)
  
  
  standQTY<- copy(dataQTY)
  
  NoStandardized<- unique(c(Primary, ProxyPrimary, Orphan))
  
  standQTY<- standQTY[ ,tobestand:= ifelse( !measuredItemChildCPC %in% NoStandardized , TRUE, FALSE),
                       by = c(params$yearVar, params$geoVar, params$childVar)]
  standQTY<-standQTY[tobestand == TRUE]
  
  standQTY <- 
    left_join(
      standQTY,
      
      fbsTree[,
              list(
                measuredItemChildCPC = measuredItemSuaFbs,
                fbsID4
              )
      ],
      by = c(  'measuredItemChildCPC')
    )
  setnames(standQTY, "fbsID4", "groupChild")
  
  standQTY <- 
    left_join(
      standQTY,
      
      fbsTree[,
              list(
                measuredItemParentCPC = measuredItemSuaFbs,
                fbsID4
              )
      ],
      by = c(  'measuredItemParentCPC')
    )
  
  setnames(standQTY, "fbsID4", "groupParent")
  
  # standardize only children under the same FBS group, children with parent in different group should be 
  # just aggregated, as they were proxy primary(merged to the following data batch)
  
  # AggregateOnly<-standQTY%>%filter(groupChild!=groupParent)
  # AggregateOnly<- as.data.table(AggregateOnly)
  # 
  
  standQTY <- standQTY%>%filter(groupChild==groupParent)
  standQTY<- as.data.table(standQTY)
  standQTY<-standQTY[ , c("groupChild", "groupParent"):=NULL]
  
  
  standQTY<-standQTY[ , standQty:= Value*share/extractionRate]
  
  standQTY<- standQTY[weight==0 & measuredElementSuaFbs== 'imports', 
                      standQty:= Value*share/extractionRate ]
  
  standQTY<- standQTY[weight==0 & measuredElementSuaFbs!= 'imports', 
                      standQty:= Value*ImportShare*share/extractionRate ]
  
  standQTY<- standQTY[!measuredElementSuaFbs %in% c("foodmanufacturing") ]
  standQTY<- standQTY[!(measuredElementSuaFbs=="production")]
  
  standQTY<-standQTY[ , c(1,2,4,10,13), with=FALSE]
  
  standQTY<- standQTY[!is.na(standQty)]
  
  setnames(standQTY, "measuredItemParentCPC", "measuredItemSuaFbs")
  setnames(standQTY, "standQty", "Value")
  setcolorder(standQTY, c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs","timePointYears", "Value"))
  
  
  # primary, proxy primary, all orphans data and missing ER have to be added as they are
  # The derived in the group "aggregateOnly" have to be added for the portion coming from parents in different FBSgroups
  # Example: Malt need to be aggregated for the share coming from wheat and/or rice(different fbs group), 
  # while the share coming from barley (same fbs group) is standardized in the previous batch. 
  
  # AggregateOnly <- AggregateOnly[ , qty:=sum(Value*share, na.rm = TRUE),
  #                                 by=c("measuredItemChildCPC", "timePointYears", "geographicAreaM49", "measuredElementSuaFbs")]
  # AggregateOnly<- AggregateOnly[ , c(1,2,3,9,14), with= FALSE]
  # setnames(AggregateOnly, "qty", "Value")
  # AggregateOnly<-unique(AggregateOnly)
  # 
  Orphan<-c(Orphan, countryOrphansYear$measuredItemChildCPC)
  
  primarydata<-copy(dataSubset)
  primarydata<-primarydata[measuredItemChildCPC %in% c(Primary, Orphan, ProxyPrimary, missingER, Aggregate_only)]
  primarydata<-primarydata[ , c(1,2,3,4,5), with=FALSE]
  
  # primarydata<-rbind(primarydata, Aggregate_only)
  
  primarydata<-primarydata[!measuredElementSuaFbs=="foodmanufacturing"]
  setnames(primarydata, "measuredItemChildCPC", "measuredItemSuaFbs")
  
  
  # we compute the processed that need to stay (the processed related to item outside the tree)
  
  
  dataQTY_processed<-copy(dataQTY)
  dataQTY_processed<-dataQTY_processed[ measuredElementSuaFbs=="production"]
  
  dataQTY_processed <- 
    left_join(
      dataQTY_processed,
      
      fbsTree[,
              list(
                measuredItemChildCPC = measuredItemSuaFbs,
                fbsID4
              )
      ],
      by = c(  'measuredItemChildCPC')
    )
  setnames(dataQTY_processed, "fbsID4", "groupChild")
  
  dataQTY_processed <- 
    left_join(
      dataQTY_processed,
      
      fbsTree[,
              list(
                measuredItemParentCPC = measuredItemSuaFbs,
                fbsID4
              )
      ],
      by = c(  'measuredItemParentCPC')
    )
  
  setnames(dataQTY_processed, "fbsID4", "groupParent")
  
  # standardize under processed children with different fbs group
  
  dataQTY_processed <- dataQTY_processed%>%filter(groupChild!=groupParent)
  dataQTY_processed<- as.data.table(dataQTY_processed)
  
  dataQTY_processed<- dataQTY_processed[!is.na(share) & share!=0]
  
  dataQTY_processed<-merge(dataQTY_processed, pseudoProxy, by= c(params$yearVar, params$geoVar,params$parentVar, params$childVar), all.x = TRUE)
  dataQTY_processed<- dataQTY_processed[is.na(pseudoProxy), pseudoProxy:=FALSE]
  dataQTY_processed<- dataQTY_processed[pseudoProxy!=TRUE]
  dataQTY_processed<-dataQTY_processed[ , pseudoProxy:=NULL]
  
  dataQTY_processed<- dataQTY_processed[ , processed_parent:= 
                                           (Value*weight*share/extractionRate),
                                         by = c(params$yearVar, params$geoVar,params$parentVar)]
  
  # addition 14/12 If the parent is zero weight, the child that goes out from the group 
  # should be assigned to processed for the component coming from import, which is the import share of the parent
  
  
  dataQTY_processed<-dataQTY_processed[weight_parent==0,processed_parent:= 
                                         (Value*share*ImportShare/extractionRate),
                                       by = c(params$yearVar, params$geoVar,params$parentVar) ]
  
  
  dataQTY_processed<- dataQTY_processed[ , processed:= sum(processed_parent, na.rm = TRUE), by=c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")]
  
  
  dataQTY_processed<-dataQTY_processed[,
                                       c(params$geoVar,params$parentVar,
                                         params$elementVar,params$yearVar,"processed"),
                                       with=FALSE
  ] 
  
  setnames(dataQTY_processed, params$parentVar,params$itemVar)
  setnames(dataQTY_processed,"processed", "Value")
  dataQTY_processed<- dataQTY_processed[ , measuredElementSuaFbs:= "foodmanufacturing"]
  data_QTY_processed<- unique(dataQTY_processed, by=c("geographicAreaM49", "timePointYears", "measuredElementSuaFbs", "measuredItemSuaFbs"))  
  
  # merge the 3 dataset created for quantities and aggregate standardized data by parent
  
  out = rbind(standQTY, data_QTY_processed, primarydata)
  
  out = out[, list(
    Value = sum(Value, na.rm = TRUE)),
    by = c(params$yearVar, params$geoVar, params$itemVar,params$elementVar)]
  
  # compute error, the not explained imbalance was sent to processed. now deleted this part but keep it commented for debugging
  # fbsstand_error<-copy(out)
  # 
  # calculateError(data=fbsstand_error)
  # 
  # fbsstand_error<- fbsstand_error[measuredElementSuaFbs=="residual" , adjError:= error - Value ,
  #                                 by=c(params$yearVar, params$geoVar, params$itemVar)]
  # fbsstand_error<- fbsstand_error[,
  #                                 newproc:= sum(Value[measuredElementSuaFbs=="foodmanufacturing"],adjError, na.rm=TRUE),
  #                                 by=c(params$yearVar, params$geoVar, params$itemVar)]
  # 
  # fbsstand_error<- fbsstand_error[measuredElementSuaFbs=="residual"& newproc>100]
  # fbsstand_error[,`:=`(Value=newproc,
  #                      error=NULL,
  #                      supply=NULL,
  #                      utilizations=NULL,
  #                      newproc=NULL,
  #                      measuredElementSuaFbs = "foodmanufacturing",
  #                      adjError=NULL
  #                      )]
  # 
  # fbsstand_error<-unique(fbsstand_error, by=colnames(fbsstand_error))
  # setnames(fbsstand_error, "Value", "correction")
  # 
  # out<-merge(out,fbsstand_error,by=c(params$yearVar, params$geoVar, params$itemVar,params$elementVar), all=TRUE)
  # out<- out[!is.na(correction), Value:= correction]
  # out<- out[ , correction:=NULL]
  
  # #
  # # re-calculate imbalance
  # 
  fbsstand_imbalance<-copy(out[measuredElementSuaFbs %!in% c("residual")])
  
  calculateImbalance(data=fbsstand_imbalance)
  
  fbsstand_imbalance[,`:=`(Value=imbalance,
                           measuredElementSuaFbs="residual",
                           imbalance=NULL,
                           supply=NULL,
                           utilizations=NULL)]
  
  fbsstand_imbalance<-unique(fbsstand_imbalance, by=colnames(fbsstand_imbalance))
  
  out<-rbind(out[measuredElementSuaFbs %!in% c("residual")],fbsstand_imbalance)
  
  
  setnames(out,"measuredElementSuaFbs", "name")
  out <- out[codes, on = "name"]
  out[,name:=NULL]
  
  
  
  #DES AGGREGATION
  
  ## Merge the tree with the node data
  StandtreeDES<-copy(standTree)
  
  StandtreeDES[, c(params$parentVar, params$childVar, params$yearVar, params$geoVar) :=
                 list(as.character(get(params$parentVar)), as.character(get(params$childVar)),
                      as.character(get(params$yearVar)), as.character(get(params$geoVar)))]
  
  setnames(dataDES, params$itemVar, params$childVar)
  
  
  
  dataDES<-merge(dataDES, StandtreeDES,
                 by = c(params$yearVar, params$geoVar, params$childVar),
                 all.x = TRUE, allow.cartesian = TRUE)
  
  # i primary proxy primary, orphans e aggregate only rimangono cosi
  dataDES<-dataDES[measuredItemChildCPC %in% c(Primary,ProxyPrimary, Orphan, missingER, Aggregate_only),
                   c(params$parentVar, params$extractVar, params$shareVar) :=
                     list(get(params$childVar), 1, 1)]
  
  
  dataDES[,missedDES:=(mean(Value,na.rm = TRUE)>0) & 
            (sum(share,na.rm = TRUE)>=0) & (sum(share,na.rm = TRUE) < 1),
          by = c(params$yearVar, params$geoVar, params$childVar,params$elementVar)
  ]
  
  
  # check to see if some calories are missing
  dataDES[missedDES==TRUE,
          `:=`(measuredItemParentCPC=measuredItemChildCPC,share=1)]
  
  dataDES[,weight:=1]
  dataDES[missedDES==TRUE,params$extractVar:=1]
  dataDES<- dataDES[ , ImportShare:=NULL]
  dataDES<- dataDES[ , weight_parent:=NULL]
  
  dataDES<-unique(
    dataDES, by=names(dataDES)
  )
  
  # cases where the shares are identical and one of them disappears becasue of using unique 
  
  dataDES[,missedDES:=(mean(Value,na.rm = TRUE)>0) & 
            (sum(share,na.rm = TRUE)>=0) & (sum(share,na.rm = TRUE) < 1),
          by = c(params$yearVar, params$geoVar, params$childVar,params$elementVar)
  ]
  
  dataDES<-dataDES[, share:= ifelse(missedDES==TRUE, 1, share)]
  
  outDataDes = dataDES[, list(
    Value = sum( Value*get(params$shareVar), na.rm = TRUE)),
    by = c(params$yearVar, params$geoVar, params$parentVar,params$elementVar)]
  
  setnames(outDataDes, "measuredItemParentCPC", "measuredItemSuaFbs")
  setcolorder(outDataDes, c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs","timePointYears", "Value"))
  
  
  out = rbind(out,outDataDes)
  
  standData0[[i]] <- out
  
  # aggregate at FBS level to have fbs balanced
  
  if(is.null(fbsTree)){
    # If no FBS tree, just return SUA-level results
    outOut=dataSubset
  } else {
    outOut = computeFbsAggregate(data =out , fbsTree = fbsTree,
                                 standParams = p)
  }
  standData[[i]] <- rbindlist(outOut)
  
}

#####sSAVING EDGES DOC#######

#Livia Saving final edges 
finalEdgesTable <- rbindlist(edgeData)
finalEdgesTable<- finalEdgesTable[ , proxy:=
                                     ifelse(measuredItemChildCPC %in% ProxyPrimary, TRUE, FALSE)]

setnames(finalEdgesTable, "measuredItemParentCPC","measuredItemParentCPC_tree" )
setnames(finalEdgesTable, "measuredItemChildCPC","measuredItemChildCPC_tree" )

finalEdgesTable<- nameData(domain="suafbs", dataset = "ess_fbs_commodity_tree2", finalEdgesTable)
finalEdgesTable<- finalEdgesTable[ , `:=`
                                   (measuredItemParentCPC_tree = paste0("'", get("measuredItemParentCPC_tree")),
                                     measuredItemChildCPC_tree = paste0("'", get("measuredItemChildCPC_tree")) )]                             

if(length(selectedGEOCode)==1){
  tmp_file_finalEdges<- tempfile(pattern = paste0("finalEdges_", COUNTRY_NAME, "_"), fileext = '.csv')
}else{
  tmp_file_finalEdges<- tempfile(pattern = paste0("finalEdges_"), fileext = '.csv')
}

# write.csv(finalEdgesTable, tmp_file_finalEdges, row.names = FALSE)


#####sSAVING FBS STANDARDIZED#########################################
fbs_standardized<-rbindlist(standData0)
fbs_standardized[,`:=`(flagObservationStatus="I",
                       flagMethod="s")]

setnames(fbs_standardized,"measuredItemSuaFbs","measuredItemFbsSua")
setDT(fbs_standardized)

fbs_standardized<-fbs_standardized[!is.na(Value)]
fbs_standardized<-fbs_standardized[measuredElementSuaFbs %!in% c("261","271","281")]

#Food grams

############################ POPULATION #####################################

# key <-
#   DatasetKey(
#     domain = "population",
#     dataset = "population_unpd",
#     dimensions =
#       list(
#         geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
#         measuredElementSuaFbs = Dimension(name = "measuredElement", keys = "511"), # 511 = Total population
#         timePointYears = Dimension(name = "timePointYears", keys = as.character(2000:endYear))
#       )
#   )


popSWS <- fread("SUA-FBS Balancing/Data/popSWS.csv")

stopifnot(nrow(popSWS) > 0)

popSWS[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

# Fix for missing regional official data in the country total
# Source: DEMOGRAPHIC SURVEY, Kurdistan Region of Iraq, July 2018, IOM UN Migration
# ("the KRI population at 5,122,747 individuals and the overall Iraqi
# population at 36,004,552 individuals", pag.14; it implies 14.22805%)
# https://iraq.unfpa.org/sites/default/files/pub-pdf/KRSO%20IOM%20UNFPA%20Demographic%20Survey%20Kurdistan%20Region%20of%20Iraq_0.pdf
popSWS[geographicAreaM49 == "368" & timePointYears %in% yearVals, Value := Value * 0.8577195]

# Fix for Moldova Population. We need to exclude Transnistria population, 
# since production and trade data exclude this area.We use an approximate value of 500,000, 
# following indication of 2014 census data provided by UN Population Division

popSWS[geographicAreaM49 == "498" & timePointYears %in% startYear:endYear, Value := Value - 500]



#ADD food supply (Grams/capita/day) in FBS standardized
foodGram_data <-
  dt_left_join(
    # Food
    fbs_standardized[
      measuredElementSuaFbs == '5141',
      list(
        geographicAreaM49,
        measuredItemFbsSua,
        measuredElementSuaFbs,
        timePointYears,
        food = Value,
        flagObservationStatus = "T",
        flagMethod = "i"
      )
    ],
    # Population
    popSWS[, list(geographicAreaM49, timePointYears, population = Value)],
    by = c('geographicAreaM49', 'timePointYears')
  )

foodGram_data[,Value:=(food*1000000)/(365*population*1000)]
foodGram_data[,measuredElementSuaFbs:="665"]
foodGram_data[, c("food", "population") := NULL]

foodGram_data<-foodGram_data[,list(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears,Value,flagObservationStatus,flagMethod)]

fbs_standardized<-rbind(fbs_standardized,foodGram_data)

write.csv(fbs_standardized,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_standardized_final.csv") ,row.names=FALSE)



message("saving FBS standardized...")
#SaveData(domain = "suafbs", dataset = "fbs_standardized", data = fbs_standardized, waitTimeout = 20000)

#end fns standardized-------------------------------------------------------------

########SAVING FBS BALANCED#################################################-------------------

fbs_balanced<-rbindlist(standData)
fbs_balanced[,`:=`(flagObservationStatus="I",
                   flagMethod="s")]
setnames(fbs_balanced,"fbsID4","measuredItemFbsSua")



fbs_balanced<-fbs_balanced[!is.na(Value)]
fbs_balanced<-fbs_balanced[!is.na(measuredItemFbsSua)]



#ADD food supply (Grams/capita/day) in FBS balanced
foodGram_data_fb <-
  dt_left_join(
    # Food
    fbs_balanced[
      measuredElementSuaFbs == '5141',
      list(
        geographicAreaM49,
        measuredItemFbsSua,
        measuredElementSuaFbs,
        timePointYears,
        food = Value,
        flagObservationStatus = "T",
        flagMethod = "i"
      )
    ],
    # Population
    popSWS[, list(geographicAreaM49, timePointYears, population = Value)],
    by = c('geographicAreaM49', 'timePointYears')
  )

foodGram_data_fb[,Value:=(food*1000000)/(365*population*1000)]
foodGram_data_fb[,measuredElementSuaFbs:="665"]
foodGram_data_fb[, c("food", "population") := NULL]

foodGram_data_fb<-foodGram_data_fb[,list(geographicAreaM49,measuredItemFbsSua,measuredElementSuaFbs,timePointYears,Value,flagObservationStatus,flagMethod)]

fbs_balanced<-rbind(fbs_balanced,foodGram_data_fb)


write.csv(fbs_balanced,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.csv") ,row.names=FALSE)

message("saving FBS balanced...")
#SaveData(domain = "suafbs", dataset = "fbs_balanced_", data = fbs_balanced, waitTimeout = 20000)


#extracting FBS table for 2010-2013


# CONFIG <- GetDatasetConfig(sessionKey_fbsBal@domain, sessionKey_fbsBal@dataset)
# 
# fbs_balancedData_2010_2013=GetData(sessionKey_fbsBal)
# 
# fbs_balancedData_2010_2013 <- fbs_balancedData_2010_2013[timePointYears %in% c(2010:2013)]
# 
# write.csv(fbs_balancedData_2010_2013,paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_2010_2013.csv") ,row.names=FALSE)
# 

fbs_balancedData_2010_2013 <- fread(paste0(basedir,"/SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_2010_2013.csv"))

setnames(popSWS, "measuredElement", "measuredElementSuaFbs")
popSWS<- popSWS[ timePointYears%in% c(startYear:endYear)]
popSWS[,measuredItemFbsSua:="S2501"]

#SaveData(domain = "suafbs", dataset = "fbs_balanced_", data = popSWS, waitTimeout = 20000)


#popSWS[,measuredItemFbsSua:="S2901"]
#SaveData(domain = "suafbs", dataset = "fbs_balanced_", data = popSWS, waitTimeout = 20000)



#end fbs balanced---------------------------------------


### File containing imbalances greater that 1 MT----------------------

fbs_residual_to_send<-copy(fbs_balanced[measuredElementSuaFbs=="5166" & abs(Value)>1000])


# if (nrow(fbs_residual_to_send) > 0) {
#   fbs_residual_to_send<-nameData("suafbs", "fbs_balanced_", fbs_residual_to_send)
# }



#if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
if(length(selectedGEOCode)==1){
  tmp_file_residual<- tempfile(pattern = paste0("FBS_IMBALANCES_", COUNTRY_NAME, "_"), fileext = '.csv')
  
}else{
  tmp_file_residual<- tempfile(pattern = paste0("FBS_IMBALANCES_"), fileext = '.csv')
  
}

fbs_residual_to_send<-unique(fbs_residual_to_send, by=c(names(fbs_residual_to_send)))
# write.csv(fbs_residual_to_send, tmp_file_residual, row.names = FALSE)

### End File containing imbalances greater that 1 MT----------------------

#File containing aggregated DES from FBS-----------------------
fbs_des_to_send<-fbs_balanced[measuredElementSuaFbs=="664"]
fbs_des_to_send<-nameData("suafbs", "fbs_balanced_", fbs_des_to_send)
fbs_des_to_send <-
  dcast(
    fbs_des_to_send,
    geographicAreaM49_description + measuredElementSuaFbs_description+measuredItemFbsSua_description ~ timePointYears,
    fun.aggregate = sum,
    value.var = "Value"
  )

#if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
if(length(selectedGEOCode)==1){
  tmp_file_des<- tempfile(pattern = paste0("FBS_DES_", COUNTRY_NAME, "_"), fileext = '.csv')
}else{
  tmp_file_des<- tempfile(pattern = paste0("FBS_DES_"), fileext = '.csv')
}
# write.csv(fbs_des_to_send, tmp_file_des, row.names = FALSE)
#End File containing aggregated DES from FBS-----------------------

#DES comparison: SUA and FBS-----------------
# DEssua<-dataDes[measuredElementSuaFbs=="664",]
# DEssua<-DEssua[,
               # list(Value=sum(Value,na.rm = TRUE)),
               # by=c("geographicAreaM49","measuredElementSuaFbs","timePointYears")
# ]

# DEssua<-nameData("suafbs", "sua_balanced", DEssua)
# DEssua[,measuredItemFbsSua_description:="DES from SUA_bal"]

# # DEssua <-
#   dcast(
#     DEssua,
#     geographicAreaM49_description + measuredElementSuaFbs_description+measuredItemFbsSua_description ~ timePointYears,
#     fun.aggregate = sum,
#     value.var = "Value"
#   )

# setDT(DEssua)
# DEssua[,`measuredElementSuaFbs_description`:="Food supply (/capita/day) [kcal]"]
# 
# setDT(fbs_des_to_send)
# DesFBS<-fbs_des_to_send[measuredItemFbsSua_description=="GRAND TOTAL - DEMAND"]
# DesFBS[,measuredItemFbsSua_description:="DES from FBS"]
# ComparativeDES<-rbind(DEssua,DesFBS)


# #if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
# if(length(selectedGEOCode)==1){
#   tmp_file_desSuaFbs<- tempfile(pattern = paste0("DES_SUA_vs_FBS_", COUNTRY_NAME, "_"), fileext = '.csv')
# }else{
#   tmp_file_desSuaFbs<- tempfile(pattern = paste0("DES_SUA_vs_FBS_"), fileext = '.csv')
# }
# write.csv(ComparativeDES, tmp_file_desSuaFbs, row.names = FALSE)
#End DES comparison: SUA and FBS-----------------

#Items with DES and without FBS group----------------------
# DESItems_noFBSGroup<-dataDes[measuredItemSuaFbs %!in% fbsTree[,get(p$itemVar)] & Value>0,]
# DESItems_noFBSGroup[,measuredElementSuaFbs:="664"]
# DESItems_noFBSGroup<-nameData("suafbs", "sua_balanced", DESItems_noFBSGroup)

# if(nrow(DESItems_noFBSGroup)>0){
#   
#   DESItems_noFBSGroup <-
#     dcast(
#       DESItems_noFBSGroup,
#       geographicAreaM49_description + measuredElementSuaFbs_description+measuredItemSuaFbs ~ timePointYears,
#       fun.aggregate = sum,
#       value.var = "Value"
#     )
# }
# 
# #if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
# if(length(selectedGEOCode)==1){
#   tmp_file_noFbsGroup<- tempfile(pattern = paste0("ITEMS_NO_FBSGROUP_", COUNTRY_NAME, "_"), fileext = '.csv')
# }else{
#   tmp_file_noFbsGroup<- tempfile(pattern = paste0("ITEMS_NO_FBSGROUP_"), fileext = '.csv')
# }
# 
# write.csv(DESItems_noFBSGroup, tmp_file_noFbsGroup, row.names = FALSE)
#End Items with DES and without FBS group----------------------

# fin<-Sys.time()

# duree<-fin-commence

# if (!CheckDebug()) {
#   send_mail(
#     from = "do-not-reply@fao.org",
#     to = swsContext.userEmail,
#     subject = "Results from newBalancing plugin",
#     body = c("If all commodities of a tree (FBS item) are balanced at SUA level, then the 
#              FBS item is balanced by moving eventual residual to process.",
#              tmp_file_des,
#              tmp_file_residual,
#              tmp_file_desSuaFbs,
#              tmp_file_missingER,
#              tmp_file_finalEdges
#              
#     )
#   )
# }

}