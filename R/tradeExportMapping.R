
tradeExportMapping=function(input,output,session, values_exportCountryData){
  
  
  t=as.character(input$endyear)
  
  
  basedir <-getwd()
  
  
  files = dir("SUA-FBS Balancing/Trade/R",full.names = TRUE)
  for(i in files){
    source(i, local = TRUE)
  }
  
  
  
  
  exportData=data.table(values_exportCountryData$exportCountry)
  # exportData=data.table(read_excel("Zanzibar/import.xlsx"))
  
  
  exportData = exportData[,c(input$exportYear, input$exportHS,input$exportQuantity),with=F]
  
  # exportData = exportData[,c("year_2014", "HS code","2014"),with=F]
  
  
  
  exportData=data.table(exportData)
  
  
  setnames(exportData,c(input$exportYear,input$exportHS,input$exportQuantity), c("Year","HS6","Value"))
  
  # setnames(exportData,c("year_2014","HS code","2014"), c("Year","HS6","Value"))
  
  
  exportData <- exportData[Year %in% t]
  
  
  
  if (nrow(exportData)== 0){
    
    sendSweetAlert(
      session = session,
      title = "No data to map !!",
      text = "No data to map",
      type = "warning"
    )
    
    
    exportDataFinal <- hot_to_r(input$tradeE)
    
    
  } 
  
  else{
    
    
    exportData <- exportData[, lapply(.SD,sum), by=list(Year,HS6)]
    
    
    countrycode <- fread("SUA-FBS Balancing/Trade/Data/country_codes.csv")
    
    M49_code <- unique(countryData$CountryM49)
    
    
    exportData[, reporter := countrycode[`M49 Code` == M49_code]$`Country Code`]
    
    setnames(exportData,"HS6","hs")
    
    
    # importData= melt.data.table(importData, id.vars = c("Year", "hs","reporter"), measure.vars = c("Value"), value = TRUE,
    #                               value.name= "QTY")
    
    
    exportData[,flow := 2]
    
    
    exportData[,hs6 := str_sub(hs,1L,6)]
    
    exportData[,HSLength := nchar(hs)]
    
    setnames(exportData, c("Value"),c("QTY"))
    
    
    tldata_export <- copy(exportData)
    
    
    generate_hs6mapping <- TRUE
    
    # Parallel backend will be used only if required packages are installed.
    # It will be switched to FALSE if packages are not available.
    multicore <- TRUE
    
    
    
    # The main trademap (i.e., the "historical" one) is:
    
    # hsfclmap3 <- ReadDatatable('hsfclmap5')
    # 
    # write.csv(hsfclmap3,"SUA-FBS Balancing/Trade/hsfclmap3.csv",row.names = F)
    
    hsfclmap3 <- fread("SUA-FBS Balancing/Trade/Data/hsfclmap3.csv")
    
    hsfclmap3 <- hsfclmap3[area %in% fao_code_country[`M49 Code` == M49_code]$`Country Code`]
    
    # this one is used to "patch" the previous one (i.e., to apply corrections) for missing links:
    
    # add_map <- ReadDatatable('hsfclmap4')
    # write.csv(add_map,"SUA-FBS Balancing/Trade/add_map.csv",row.names = F)
    
    
    add_map <- fread("SUA-FBS Balancing/Trade/Data/add_map.csv")
    
    add_map <- add_map[reporter_fao %in% fao_code_country[`M49 Code` == M49_code]$`Country Code`]
    
    # There are some corrections that need to be applied to the main trademap
    
    
    
    
    #data("hsfclmap3", package = "hsfclmap", envir = environment())
    # NOTE: it is pulling now v5
    # FCL, startyear, endyear codes can be overwritten by corrections
    
    hsfclmap3[!is.na(correction_fcl), fcl := correction_fcl]
    
    hsfclmap3[!is.na(correction_startyear), startyear := correction_startyear]
    
    hsfclmap3[!is.na(correction_endyear), endyear := correction_endyear]
    
    hsfclmap3[, grep('correction', names(hsfclmap3)) := NULL]
    
    ##' 1. Extend the `endyear` for those combinations of `area` / `flow` /
    ##' `fromcode` / `tocode` for which `endyear` < `year`.
    
    # Extend endyear to 2050
    hsfclmap3[, maxy := max(endyear), .(area, flow, fromcode, tocode)]
    
    hsfclmap3[, extend := ifelse(maxy < 2050, TRUE, FALSE), .(area, flow, fromcode, tocode)]
    
    hsfclmap3[endyear == maxy & extend, endyear := 2050]
    
    hsfclmap3[, c("maxy", "extend") := NULL]
    # / Extend endyear to 2050
    
    
    # Unmapped codes are added here:
    
    
    # ADD UNMAPPED CODES
    
    
    # Check that all FCL codes are valid
    
    # fcl_codes <- as.numeric(ReadDatatable('fcl_2_cpc')$fcl)
    # 
    # write.csv(fcl_codes,"SUA-FBS Balancing/Trade/fcl_codes.csv",row.names = F)
    
    fcl_codes <-as.vector(fread("SUA-FBS Balancing/Trade/Data/fcl_codes.csv")$x)
    
    
    stopifnot(length(fcl_codes) > 0)
    
    
    
    fcl_diff <- setdiff(unique(add_map$fcl), fcl_codes)
    
    
    fcl_diff <- fcl_diff[!is.na(fcl_diff)]
    
    
    fcl_diff <- setdiff(fcl_diff, 0)
    
    
    if (length(fcl_diff) > 0) {
      warning(paste('Invalid FCL codes:', paste(fcl_diff, collapse = ', ')))
    }
    
    
    # Check that years are in a valid range
    
    
    if (min(add_map$year) < 2000) {
      warning('The minimum year should not be lower than 2000.')
    }
    
    
    if (max(add_map$year) > as.numeric(format(Sys.Date(), '%Y'))) {
      warning('The maximum year should not be greater than the current year.')
    }
    
    
    # Check that there are no duplicate codes
    
    
    if (nrow(add_map[, .N, .(reporter_fao, year, flow, hs)][N > 1]) > 0) {
      warning('Removing duplicate HS codes by reporter/year/flow.')
      
      add_map[, `:=`(n = .N, hs_ext_perc = sum(!is.na(hs_extend))/.N), .(reporter_fao, year, flow, hs)]
      
      
      # Prefer cases where hs_extend is available
      add_map <- add_map[hs_ext_perc == 0 | (hs_ext_perc > 0 & !is.na(hs_extend) & n == 1)]
      
      
      add_map[, c("n", "hs_ext_perc") := NULL]
    }
    
    
    # Raise warning if countries were NOT in mapping.
    
    
    if (length(setdiff(unique(add_map$reporter_fao), hsfclmap3$area)) > 0) {
      warning('Some countries were not in the original mapping.')
    }
    
    
    add_map <-
      add_map[,
              .(area = reporter_fao,
                flow,
                fromcode = gsub(' ', '', hs),
                tocode = gsub(' ', '', hs),
                fcl = as.numeric(fcl),
                startyear = year,
                endyear = 2050L,
                recordnumb = NA_integer_,
                area_name = reporter_name
              )
              ]
    
    
    max_record <- max(hsfclmap3$recordnumb)
    
    
    add_map$recordnumb <- (max_record + 1):(max_record + nrow(add_map))
    
    
    ##' 1. Add additional codes that were not present in the HS-FCL
    ##' original mapping file.
    
    
    hsfclmap3 <- rbind(add_map, hsfclmap3)
    
    
    hsfclmap3[, `:=`(startyear = as.integer(startyear), endyear = as.integer(endyear))]
    
    
    # / ADD UNMAPPED CODES
    
    # Keep only codes for which `startyear` is less than current year and for which `endyear` is greater than current year:
    
    
    
    
    ##' 1. Keep HS-FCL links for which `startyear` <= `year` & `endyear` >= `year`
    year <- as.integer(t)
    
    hsfclmap <- hsfclmap3[startyear <= year & endyear >= year]
    
    
    hsfclmap3 <- tbl_df(hsfclmap3)
    
    
    hsfclmap[, c("startyear", "endyear") := NULL]
    
    
    # Workaround issue #123
    hsfclmap[, fromgtto := as.numeric(fromcode) > as.numeric(tocode)]
    
    
    from_gt_to <- hsfclmap$recordnumb[hsfclmap$fromgtto]
    
    
    # if (length(from_gt_to) > 0)
    #   flog.warn(paste0("In following records of hsfclmap fromcode greater than tocode: ",
    #                    paste0(from_gt_to, collapse = ", ")))
    
    
    hsfclmap <- hsfclmap[fromgtto == FALSE][, fromgtto := NULL]
    
    
    stopifnot(nrow(hsfclmap) > 0)
    
    
    hsfclmap <- tbl_df(hsfclmap)
    
    
    # Generate an HS6 trademap, useful if no specific links are available:
    
    if (generate_hs6mapping) {
      
      
      # hs6fclmap ####
      
      
      # flog.trace("[%s] Extraction of HS6 mapping table", PID, name = "dev")
      
      
      ##'     1. Universal (all years) HS6 mapping table.
      
      
      # flog.trace("[%s] Universal (all years) HS6 mapping table", PID, name = "dev")
      
      
      hs6fclmap_full <- extract_hs6fclmap(hsfclmap3, parallel = multicore)
      
      
      ##'     1. Current year specific HS6 mapping table.
      
      
      # flog.trace("[%s] Current year specific HS6 mapping table", PID, name = "dev")
      
      
      hs6fclmap_year <- extract_hs6fclmap(hsfclmap, parallel = multicore)
      
      
      hs6fclmap <- bind_rows(hs6fclmap_full, hs6fclmap_year) %>%
        filter_(~fcl_links == 1L) %>%
        distinct()
      
      
    } else {
      
      
      # A dummy zero-row dataframe needs to be created
      hs6fclmap <-
        data_frame(
          reporter  = integer(),
          flow      = integer(),
          hs6       = integer(),
          fcl       = double(),
          fcl_links = integer()
        )
      
      
    }
    
    # trest
    reportdir = "C:/Users/Siriwardenas/Documents/FBSTool-Updated-Current/SUA-FBS Balancing/Trade/"
    
    
    #country Data
    
    # tldata <- fread("SUA-FBS Balancing/Trade/Data/country_trade.csv")
    # 
    # tldata <- tldata[flow == 1]
    
    
    # Extract HS6-FCL mapping table:
    
    
    ##'     1. Extract HS6-FCL mapping table.
    
    
    tldatahs6links <- mapHS6toFCL(tldata_export, hs6fclmap)
    
    
    # Extract specific HS-FCL mapping table:
    
    ##'     1. Extract specific HS-FCL mapping table.
    
    
    tldata_export[,hs6 := as.integer(hs6)]
    
    tldatalinks <- mapHS2FCL(tradedata   = tldata_export,
                             maptable    = hsfclmap3,
                             hs6maptable = hs6fclmap,
                             year        = year,
                             parallel    = multicore)
    
    # Add FCL codes:
    
    
    ##'     1. Use HS6-FCL or HS-FCL mapping table.
    
    
    tldata_export$map_src <- NA_character_
    
    
    tldata_export <- add_fcls_from_links(tldata_export,
                                  hs6links = tldatahs6links,
                                  links    = tldatalinks)
    
    setDT(tldata_export)
    
    # If HS6 standard exists, use it:
    
    ##'     1. Use HS6 starndard for unmapped codes.
    
    
    ##' - `hs6standard`: HS6standard will be used as last resort for mapping.
    
    # flog.trace("[%s] Reading in 'standard_hs12_6digit' datatable", PID, name = "dev")
    
    
    # hs6standard <- ReadDatatable('standard_hs12_6digit')
    # 
    # 
    # write.csv(hs6standard,"SUA-FBS Balancing/Trade/Data/hs6standard.csv",row.names = F)
    
    hs6standard <- fread("SUA-FBS Balancing/Trade/Data/hs6standard.csv")
    
    
    stopifnot(nrow(hs6standard) > 0)
    
    hs6standard <-
      hs6standard[
        !duplicated(hs2012_code)
        ][,
          .(hs6 = as.integer(hs2012_code), hs2012_code, faostat_code)
          ]
    
    
    
    
    tldata_export <- tldata_export %>%
      left_join(
        hs6standard %>% dplyr::select(-hs2012_code),
        by = 'hs6'
      ) %>%
      dplyr::mutate(
        fcl     = ifelse(is.na(fcl) & !is.na(faostat_code), faostat_code, fcl),
        map_src = ifelse(is.na(fcl) & !is.na(faostat_code), 'standard', map_src)
      ) %>%
      dplyr::select(-faostat_code)
    
    
    setDT(tldata_export)
    
    
    tldata_export[,fcl := addHeadingsFCL(fcl)]
    
    
    hs_chapters <- c(sprintf("%02d", 01:24), 33, 35, 38, 40:41, 43, 50:53)
    
    
    
    tldata_export$HSChapters = substring(tldata_export$hs,1,2)
    tldata_export = tldata_export[HSChapters %in% hs_chapters,]
    
    #hs codes that did not map 
    
    codes_unmapped <- tldata_export[is.na(fcl)]
    
    tldata_export <- tldata_export[!is.na(fcl)]
    
    
    
    
    
    # The use fcl2cpc to convert FCL codes to CPC
    
    
    
    
    fcl2cpc <- fread("Trade/Data/fcl_2_cpc.csv")
    
    
    fcl2cpc[, fcl := addHeadingsFCL(fcl2cpc$fcl)]
    
    tldata_export=tldata_export[!is.na(fcl),]
    
    
    
    tldata_export = merge(tldata_export,fcl2cpc,
                   by = "fcl",
                   all.x = TRUE)
    
    missing_map_export  <- tldata_export[is.na(cpc)]
    
    
    tldata_export <-tldata_export[,c("Year","QTY","cpc"),with = F]
    
    
    tldata_export <- aggregate(`QTY` ~ Year + cpc,data = tldata_export,sum)
    
    
    tldata_export <- data.table(tldata_export)
    
    tldata_export[, ElementCode := "5910"]
    tldata_export[, Element := "Export Quantity [t]"]
    
    
    setnames(tldata_export,  c("QTY","cpc"),c("Value","CPCCode"))
    
    
    tldata_export <- tldata_export[!is.na(CPCCode)]
    
    tldata_export[,Flag := ""]
    
    tldata_ex <- merge(tldata_export,commodityName, by= "CPCCode", all.x = TRUE)
    
    
    ### bind trade other years data
    trade_export_years <-subset(countryData, ElementCode == "5910" & Year %in% c(2010:(as.numeric(t)-1)))
    
    trade_export_years[,c("CountryM49","Country") := NULL]
    
    exportDataCPC<-rbind(tldata_ex,trade_export_years)
    
    
    exportDataFinal =  dcast.data.table(exportDataCPC, CPCCode+Commodity+ElementCode+Element ~ Year, value.var = c("Value","Flag"))
    
    flagcols <- grep("^Flag", names(exportDataFinal), value = TRUE)
    yearcols <- grep("^Value", names(exportDataFinal), value = TRUE)
    
    
    
    flagcols_new=gsub("_", " ", flagcols, fixed=TRUE)
    
    yearcols_new=gsub("^.*?_","",yearcols)
    
    exportDataFinal=data.table(exportDataFinal)
    
    setnames(exportDataFinal,flagcols, flagcols_new)
    setnames(exportDataFinal,yearcols, yearcols_new)
    
    addorder <- as.vector(rbind(yearcols_new, flagcols_new))
    
    
    
    setcolorder(exportDataFinal,c("CPCCode","Commodity","ElementCode","Element",addorder))
    
    
    exportDataFinal[, (flagcols_new) := lapply(.SD, as.character), .SDcols = flagcols_new]
    exportDataFinal[, (yearcols_new) := lapply(.SD, as.numeric), .SDcols = yearcols_new]
    
    exportDataFinal[order(CPCCode)]
    
    exportDataFinal
    
    
    
    missing_map_export<- missing_map_export[,c("Year","hs","QTY"),with =F]
    
    # write.xlsx(missing_map_import ,file,row.names = FALSE)
    
    write.csv(missing_map_export,"SUA-FBS Balancing/Trade/Data/missing_export.csv" ,row.names = F)
    
  }
  return(exportDataFinal)
  


}
