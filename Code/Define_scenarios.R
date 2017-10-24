#'========================================================================================================================================
#' Project:  AGCLIM50II
#' Subject:  Code to define scenarios
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================







### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### Define scenarios, periods, path, project, sourcefile and 
scenarios<-c("GDPEndoSSP2", "GDPEndoSSP2_C250", "GDPEndoSSP2_C500", "GDPEndoSSP2_C750", "GDPEndoSSP2_C1000",
             "GDPEndoSSP2_C1250", "GDPEndoSSP2_C2500")

periods<-c("2011-2015", "2015-2020", "2020-2030", "2030-2040", "2040-2050", "2050-2060", "2060-2070")


### Source script that creates file names
source("Code\\Load_Magnet.r")

### SET DATAPATH
source(file.path(root, "code/get_dataPath.r"))

dataPath <- "D:\\Tabeau\\AgCLim50_2"
dataResultPath <- "D:\\Tabeau\\AgCLim50_2\\Results"


### SOURCE BASIC MAGNET OUTPUT
# Emissions
source("code/Emissions.r")

#



### SOURCE ADDITIONAL OUTPUT