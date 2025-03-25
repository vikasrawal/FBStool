##' addHeadingsHS6
##'
##' Function to add headings to HS6 codes in order to have 6 digits HS6 codes
##' 
##' @export
##' 


addHeadingsHS6 = function(measuredItemHS6){ 
  ifelse(nchar(measuredItemHS6) == 5, 
         paste0("0",measuredItemHS6, sep = ""),
         measuredItemHS6)
}
