# Code to prepare indicators for FSMIP based on MAGNET OUTPUT and import into R
# Can be improved by:
#             - Extracting values for level variables of POP and GDP to compute GDP per capita
#             - Adding standard functions to plot drivers
#             - Adding code to convert update files to GDX and extract information
#             - Add functions to save in csv format
#             - Add functions to compute growth rates over periods and total period
#             - Include code as standard in MAGNET
#             - Compute data for total world
#             - Adding user defined region concordance to make user defined aggregations

# PACKAGES
BasePackages <-c("readr", "readxl", "stringr", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "haven")
lapply(BasePackages,library,character.only=TRUE)
AdditionalPackages <-c("gdxrrw")
lapply(AdditionalPackages,library, character.only=TRUE)

# load required GAMS libraries (folder user specific)
GAMSPath <- "C:\\24.4"
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.

# Set working folder
wdPath <- "D:/Tabeau/AgCLim50_2_v3"
setwd(wdPath)  

# FUNCTIONS
# Function to convert all har files to gdx (in the same folder)
har2gdx.f<-function(har.file, gdx.file=""){
  system(paste("HAR2GDX.exe", har.file, gdx.file, sep=" "))
}

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# Upload data for all scenarios and combine into one file
# Upload update and view files which contain most variables
# Perhaps also view data should be uploaded.

# Create new folder for relevant files
dataBasePath <- "./Basedata"
dataUpdatesPath <- "./Updates"
dataSolPath <- "./Solutions"
dataAddPath <- "./Additional"
#dataShocksPath <- "./4_MAGNET/Shocks"
dataResultPath <- "./Results"
if (!file.exists(dataResultPath)) dir.create(dataResultPath) 

# Define scenarios, periods and project
scenarios<-c("GDPEndoSSP2", "GDPEndoSSP2_C250", "GDPEndoSSP2_C500", "GDPEndoSSP2_C750", "GDPEndoSSP2_C1000",
             "GDPEndoSSP2_C1250", "GDPEndoSSP2_C2500", 
             "GDPEndoSSP2D", "GDPEndoSSP2DT", "GDPEndoSSP2_C250D", "GDPEndoSSP2_C500D", "GDPEndoSSP2_C750D", "GDPEndoSSP2_C1000D",
             "GDPEndoSSP2_C1250D", "GDPEndoSSP2_C2500D")
periods<-c("2011-2015", "2015-2020", "2020-2030", "2030-2040", "2040-2050", "2050-2060", "2060-2070")

# Create lookup table for update files
sourcefile<-c("update")
lookup_upd <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_upd$Var2, "-", 2)))
lookup_upd <- cbind(lookup_upd, TMP) ; rm(TMP)
names(lookup_upd) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_upd$harSourceFiles <- paste(with(lookup_upd, paste(scenario, period, sourcefile, sep="_")), ".har", sep="")
lookup_upd$gdxResultFiles <- paste(with(lookup_upd, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create lookup table for update_view files
sourcefile<-c("update_view")
lookup_upd_view <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_upd_view$Var2, "-", 2)))
lookup_upd_view <- cbind(lookup_upd_view, TMP); rm(TMP)
names(lookup_upd_view) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_upd_view$harSourceFiles <- paste(with(lookup_upd_view, paste(scenario, period, sourcefile, sep="_")), ".har", sep="")
lookup_upd_view$gdxResultFiles <- paste(with(lookup_upd_view, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create lookup table for solution files files
sourcefile<-c("Solution")
lookup_sol <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_sol$Var2, "-", 2)))
lookup_sol <- cbind(lookup_sol, TMP); rm(TMP)
names(lookup_sol) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_sol$harSourceFiles <- paste(with(lookup_sol, paste(scenario, period, sourcefile, sep="_")), ".sol", sep="")
lookup_sol$gdxResultFiles <- paste(with(lookup_sol, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create list of base files
BaseDataFiles <- data.frame(harSourceFiles = c("BaseData_b.har", "BaseData_b_view.har"),
                            gdxResultFiles = c("BaseData_b.gdx", "BaseData_b_view.gdx"))


# Create lookup table for slc files
sourcefile<-c("solution")
destinationfile <- c("solution_slc")
lookup_slc <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_slc$Var2, "-", 2)))
lookup_slc <- cbind(lookup_slc, TMP) ; rm(TMP)
names(lookup_slc) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_slc$harSourceFiles <- paste(with(lookup_slc, paste(scenario, period, sourcefile, sep="_")), ".slc", sep="")
lookup_slc$gdxResultFiles <- paste(with(lookup_slc, paste(scenario, period, destinationfile, sep="_")), ".gdx", sep="")


### Create lookup table for additional files
# Read files
sourceFile <- list.files("Additional")
destinationfile <- "Emissions_Reduct_Tax.gdx"
lookup_add <- data.frame(harSourceFiles = sourceFile, gdxSourceFiles = destinationfile)
apply(lookup_add, 1, function(x) har2gdx.f(file.path(dataAddPath, x[1]), file.path(dataResultPath, x[2])))

######## CONVERT FILES

# Add base period GDX files
apply(BaseDataFiles, 1, function(x) har2gdx.f(file.path(dataBasePath, x[1]), file.path(dataResultPath, x[2])))

# Convert update files to GDX
apply(lookup_upd, 1, function(x) har2gdx.f(file.path(dataUpdatesPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))

# Convert update view files to GDX
apply(lookup_upd_view, 1, function(x) har2gdx.f(file.path(dataUpdatesPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))

# One warning: Check
# Convert sol files to GDX # warning about sets! CHECK
apply(lookup_sol, 1, function(x) har2gdx.f(file.path(dataSolPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))

# Convert slc files to GDX
#apply(lookup_slc, 1, function(x) har2gdx.f(file.path(dataSolPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))

# Convert additional files
