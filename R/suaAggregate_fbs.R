suaAggregate_fbs <- function(input,output,session){
  
  t=as.character(input$endyear)
  
  sua_balanced_data = fread(paste0("SUA-FBS Balancing/StandardizedData/_02_AfterSuaFilling_BeforeST",t,".csv"))
  
  
  sua_balanced_data <- subset(sua_balanced_data, timePointYears == input$endyear)
  
  
  if (!(t %in% c(2010:2013))){
    
    #sua balanced is fixed for 2010-2013
    sua_balanced_data_2010_2013 <- fread("SUA-FBS Balancing/StandardizedData/sua_balanced_data_2010_2013.csv")
    
    sua_balanced_data_2010_2013[, flagMethod := NULL]
    
    sua_balanced_data <- rbind(sua_balanced_data,sua_balanced_data_2010_2013)
    
  }
  
  
  if (t == 2013){
    
    sua_balanced_data_2010_2013 <- fread("SUA-FBS Balancing/StandardizedData/sua_balanced_data_2010_2013.csv")
    
    sua_balanced_data_2010_2013[, flagMethod := NULL]
    
    sua_balanced_data_2010_2013 <- sua_balanced_data_2010_2013[timePointYears %in% c(2010,2011)]
    
    sua_balanced_data <- rbind(sua_balanced_data,sua_balanced_data_2010_2013)
    
  }
  
  
  sua_balanced_data[!duplicated(sua_balanced_data)]
  
  # 
  kcalData <- subset(sua_balanced_data, measuredElementSuaFbs %in% c("664"))
  
  kcalData[,"flagObservationStatus" := NULL]
  
  kcalData<-kcalData %>% group_by(geographicAreaM49,timePointYears) %>%  mutate(totDES=sum(Value,na.rm = T))
  

  
  
  
  # 
  # ##############################################################################################
  # 
  fbsTree<-fread("SUA-FBS Balancing/Data/fbsTree.csv")
  
  fbsCodes <- fread("SUA-FBS Balancing/Data/fbsCodes.csv")
  fbsCodes=subset(fbsCodes,grepl("^S",code))
  fbsCodes=subset(fbsCodes,grepl("[[:upper:]]\\s",description))
  fbsCodes$code<-gsub("^S","",fbsCodes$code)
  
  fbsTree <- merge(fbsTree,fbsCodes,by.x = "id1",by.y = "code")
  
  
  kcalData <- subset(sua_balanced_data, measuredElementSuaFbs %in% c("664"))
  
  kcalData[,"flagObservationStatus" := NULL]
  
  
  kcalData <- merge(kcalData,fbsTree,by.x = "measuredItemFbsSua",by.y = "item_sua_fbs")
  
  # totalDes=kcalData_id[,totDES:=sum(Value, na.rm = T), by=c("geographicAreaM49,timePointYears,id1")]
  
  #kcalData<-kcalData[,totDES:=sum(Value, na.rm = T), by=c("geographicAreaM49,timePointYears,id1")]
  
  agg4=kcalData[,desAgg4:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id4")]
  agg3=kcalData[,desAgg3:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id3")]
  agg2=kcalData[,desAgg2:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id2")]
  agg1=kcalData[,desAgg1:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id1")]
  # 
  # 
  agg4=agg4[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id4","desAgg4"),with=F]
  agg3=agg3[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id3","desAgg3"),with=F]
  agg2=agg2[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id2","desAgg2"),with=F]
  agg1=agg1[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id1","desAgg1"),with=F]
  
  agg4[,measuredItemFbsSua:=id4]
  agg4[,id4:=NULL]
  setnames(agg4,"desAgg4","Value")
  agg4=unique(agg4)
  
  agg3[,measuredItemFbsSua:=id3]
  agg3[,id3:=NULL]
  setnames(agg3,"desAgg3","Value")
  agg3=unique(agg3)
  
  agg2[,measuredItemFbsSua:=id2]
  agg2[,id2:=NULL]
  setnames(agg2,"desAgg2","Value")
  agg2=unique(agg2)
  
  
  agg1[,measuredItemFbsSua:=id1]
  agg1[,id1:=NULL]
  setnames(agg1,"desAgg1","Value")
  
  agg1=unique(agg1)
  
  aggData=rbind(agg1,agg2,agg3,agg4)
  aggData[,Value:=round(Value,0)]
  
  aggDataWide = dcast.data.table(setDT(aggData),geographicAreaM49+
                                   measuredItemFbsSua ~timePointYears , fun.aggregate = NULL,value.var = "Value")
  
  fbsName <- read_excel("Data/Reference File.xlsx", "FBS_Commodities")
  
  fbsName <- data.table(fbsName)
  
  aggDataWide <- merge(aggDataWide,fbsName, by.x="measuredItemFbsSua", by.y = "FBSCode", all.x = TRUE)
  
  columnNumeric <- grep("^[[:digit:]]{4}$", names(aggDataWide), value = TRUE)
  
  setcolorder(aggDataWide, c("geographicAreaM49","measuredItemFbsSua","Commodity",columnNumeric))
  
  
  # aggDataWide$measuredItemFbsSua<-gsub("^2","S2",aggDataWide$measuredItemFbsSua)
  # aggDataWide<-nameData("suafbs","fbs_balanced_",aggDataWide)
  
  
  return(aggDataWide)
  
  
  
}

