
return_data_base <- function(data_to_save){


data_to_save <- data_to_save[!is.na(CPCCode)]

data_to_save <- long_format(data_to_save)

data_to_save[, ElementCode := as.character(ElementCode)]

sua_Data <- data.table(df_sua_unbalanced$data_sua_unbalanced)

# write.csv(sua_Data,"sua_Data.csv",row.names = F)

# sua_Data <- fread("sua_Data.csv")

sua_Data <- long_format(sua_Data)

sua_Data[, ElementCode := as.character(ElementCode)]

data_to_save[, c("Commodity","Element") := NULL]

sua_Data[, c("Commodity","Element") := NULL]



# xx <- data_to_save[!is.na(Value)][
#   sua_Data,
#   on = c("CPCCode", "ElementCode", "Year")
# 
#   ]


xx <- data_to_save[
  sua_Data,
  on = c("CPCCode", "ElementCode", "Year")

]


# xx[ElementCode %in% unique(data_to_save$ElementCode) & Year == "2017" , `:=` (i.Value = Value, i.Flag= Flag)]



xx[ElementCode %in% unique(data_to_save$ElementCode), `:=` (i.Value = Value, i.Flag= Flag)]



xx[, c("Value","Flag"):= NULL]

setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))


# yy <- data_to_save[!is.na(Value)][
#   !sua_Data,
#   on = c("CPCCode", "ElementCode", "Year")
# 
#   ]

yy <- data_to_save[
  !sua_Data,
  on = c("CPCCode", "ElementCode", "Year")

]

xx <- rbind(xx,yy)




xx <- merge(xx,all_cpc, by = "CPCCode", all.x = TRUE)
xx <- merge(xx,all_elements_to_merge, by = "ElementCode", all.x = TRUE)

xx <- wide_format(xx)
xx <-xx[order(CPCCode, factor(ElementCode, 
                              levels = c("5113", "5510","5610","5910","5071", "5141","5023", "5525","5520","5016","5165","5166")))]


xx[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 

return(xx)
}