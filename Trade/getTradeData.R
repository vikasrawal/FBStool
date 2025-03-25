rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(readr)
library(foreign)
library(haven)

# Read the generic conversion table HS > FCL
# hsfclmap3 <- tbl_df(ReadDatatable("hsfclmap3"))
files = dir("Trade/R",full.names = TRUE)
#sapply(files, source)
for(i in files){
  source(i, local = TRUE)
}


# This mapping refers to ALL country
hsfclmap3 = read_csv("Trade/Data/hsfclmap3.csv",
                     col_types = cols(area = col_integer(),
                                      flow = col_integer(),
                                      fromcode = col_character(),
                                      tocode = col_character(),
                                      fcl = col_integer(),
                                      startyear = col_integer(),
                                      endyear = col_integer(),
                                      recordnumb = col_integer()))
# filter Mongolia country
hsfclmap3 = filter(hsfclmap3, area == 141)
hsfclmap3$fcl = addHeadingsFCL(hsfclmap3$fcl)

# The code written works year by year
# Select year

timePeriod = seq(2000,2017,1)
hs6fclmapALL = data.frame()

for(t in 1:length(timePeriod)){
  
  year = timePeriod[t]
  hsfclmap <- hsfclmap3 %>%
    filter_(~startyear <= year &
              endyear >= year)
  
  # Workaround issue #123
  hsfclmap <- hsfclmap %>%
    mutate_at(vars(ends_with("code")),
              funs(num = as.numeric)) %>%
    mutate_(fromgtto = ~fromcode_num > tocode_num) %>%
    select(-ends_with("code_num"))
  from_gt_to <- hsfclmap$recordnumb[hsfclmap$fromgtto]
  
  hsfclmap <- hsfclmap %>%
    filter_(~!fromgtto) %>%
    select_(~-fromgtto)
  
  stopifnot(nrow(hsfclmap) > 0)
  
  # extract full and year list of hs6 code
  hs6fclmap_full <- extract_hs6fclmap(hsfclmap3, parallel = FALSE)
  hs6fclmap_year <- extract_hs6fclmap(hsfclmap, parallel = FALSE)
  
  hs6fclmap <- bind_rows(hs6fclmap_full, hs6fclmap_year) %>%
    filter_(~fcl_links == 1L) %>%
    distinct() 
  # %>% tbl_df()
  
  hs6fclmap$Year = timePeriod[t]
  
  hs6fclmapALL = rbind(hs6fclmapALL,hs6fclmap)
}

# test
# distinct(hs6fclmapALL,Year) 
# head(hs6fclmapALL)
# dim(hs6fclmapALL)
# str(hs6fclmapALL)  

hs6fclmapALL$hs6 = addHeadingsHS6(hs6fclmapALL$hs6)

# hs6fclmapALL$flow = NULL
hs6fclmapALL$fcl_links = NULL


write.csv(hs6fclmapALL, file = "Trade/Data/hs6fclmapALL.csv", row.names = FALSE)


##


