#' Converts area codes, stored in UNSD Comtrade M49, into FAO area codes
#'
#' @param Numeric or character vector of UNSC M49 area codes
#'
#' @return Interger vector of FAO area codes
#'
#' @import dplyr
#' @export

convertComtradeM49ToFAO <- function(reporters) {
  
  if(!is.integer(reporters)) reporters <- as.integer(reporters)
  
  reporters <- data.frame(reporter = reporters,
                          stringsAsFactors = F)
  
  # data("m49faomap", package = "faoswsTrade", envir = environment())
  
  m49faomap = read.csv("~/FBS Shiny Tool/Sumeda's code/my-food_balance_sheet_tool/Trade/Data/m49faomap.csv")[,-1]
  
  reporters <- reporters %>%
    left_join(m49faomap, by = c("reporter" = "m49"))
  
  if(any(is.na(reporters$fao))) warning("Some area codes were not recognized")
  
  reporters$fao
  
}