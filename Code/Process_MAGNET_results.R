#'========================================================================================================================================
#' Project:  AGCLIM50II
#' Subject:  Code to process MAGNET RESULTS
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("plyr", "tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "lazyeval")
#p_load("plyr", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("gdxrrw")


### SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)

### SET DATAPATH
#source(file.path(root, "code/Support/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### load required GAMS libraries (folder user specific)
GAMSPath <- "C:\\24.4"
#GAMSPath <- "C:\\Program Files\\GAMS\\win64\\24.6"
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.


### Set working folder
wdPath <- "D:\\R\\agCLIM50II"
setwd(wdPath)  

dataPath <- "D:\\Tabeau\\AgCLim50_2"
dataResultPath <- "D:\\Tabeau\\AgCLim50_2\\Results"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### Define scenarios, periods, path, project, sourcefile and 
scenarios<-c("GDPEndoSSP2", "GDPEndoSSP2_C250", "GDPEndoSSP2_C500", "GDPEndoSSP2_C750", "GDPEndoSSP2_C1000",
             "GDPEndoSSP2_C1250", "GDPEndoSSP2_C2500")

periods<-c("2011-2015", "2015-2020", "2020-2030", "2030-2040", "2040-2050", "2050-2060", "2060-2070")

### Source script that creates file names
source("Code\\Load_Magnet.r")
