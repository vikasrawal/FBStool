# # This function checks wether the packages required for the shiny app are installed and apllies either require or 
# # install.packages from both Cran and GitHub
# 



#   
#   install.packages("devtools")
#   require(devtools)
#     
# 
#   
#   install_version("shiny", version = "1.0.1",repos = "http://cran.us.r-project.org")
#   
#   
#   install_version("data.table", version = "1.10.4")
#   install_version("shinyBS", version = "0.61",repos = "https://cran.ma.imperial.ac.uk/")
#   install_version("ggplot2", version = "2.2.1")
#   install_version("xts", version = "0.10-0")
#   install_version("dygraphs", version = "1.1.1.4")
#   install_version("rhandsontable", version = "0.3.4")
#   install_version("shinyFiles", version = "0.6.2")
#   install_version("readxl", version = "1.0.0")
#   install_version("stringr", version = "1.2.0")
#   install_version("dplyr", version = "0.7.2")
#   install_version("reshape2", version = "1.4.2")
#   install_version("readr", version = "1.1.1")
#   install_version("openxlsx", version = "4.0.17")
#   install_version("shinydashboard", version = "0.6.1")
#   install_version("DT", version = "0.2")
#   install_version("imputeTS", version = "2.5")
#  
#   install_version("igraph", version = "1.1.2")
#   install_version("zoo", version = "1.8-0")
#   install_version("shinyjs", version = "0.9.1")
#   install_version("V8", version = "1.5")
#   install_version("reshape", version = "0.8.7")
#   install_version("shinythemes", version = "1.1.1")
#   install_version("plyr", version = "1.8.4")
#   install_version("RJSONIO", version = "1.3-0")
#   install_version("shinysky", version = "0.1.2")
#   




packageRequirevInstall = function(){
  
  # # remove.packages("shiny", lib="C:/Users/Golini/Documents/R/R-3.4.2/library~/R/R-3.4.2/library")
  #   install.packages("devtools")
  #   require(devtools)
  # 
  #   install_version("shiny", version = "1.0.1",repos = "http://cran.us.r-project.org")
  #   require(shiny)
  # 
  packagesCran = c("shiny", "data.table", "shinyBS", "devtools", "ggplot2", "xts", "dygraphs","rhandsontable","datasets",
                    "xtable","shinyFiles", "readxl", "stringr","dplyr","reshape2","readr","openxlsx","shinydashboard","DT","imputeTS", "shinyBS",
                   "igraph","zoo","shinyjs", "shinybusy","V8","reshape","shinythemes", "shinyWidgets", "plotly", "MASS", "lattice","tidyr")
  
  
  
  if (length(setdiff(packagesCran, rownames(installed.packages()))) > 0) {
    
install.packages(setdiff(packagesCran, rownames(installed.packages()))) 
    
  }
  lapply(packagesCran, require, character.only = T)
  
  # packagesGitHub = data.table(package = c("shinysky", "rhandsontable"), 
  #                             path = c("AnalytixWare/ShinySky", "jrowen/rhandsontable"))
  
  # temporary bug fix. there's probably a more elegant way to do this
  # if (length(setdiff(packagesGitHub[, package], rownames(installed.packages()))) > 0) {
  # devtools::install_github(packagesGitHub[!package  %in% rownames(installed.packages()), path])  
  #}
  
  # lapply(packagesGitHub[, package], require, character.only = T)
  
  
}













