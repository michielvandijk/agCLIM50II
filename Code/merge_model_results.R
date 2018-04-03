#'========================================================================================================================================
#' Project:  AGCLIM50II
#' Subject:  Code to combine model results
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("plyr", "tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "lazyeval", "skimr")
#p_load("plyr", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("gdxrrw")


### SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### SET DATAPATH
source(file.path(root, "code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### load required GAMS libraries (folder user specific)
igdx(GAMSPath)

### SOURCE
source(file.path(root, "code/R2GDX.r"))

### LOAD DATA
# Template
temp <- read_excel(file.path(dataPath, "Reporting/Reporting_template_AGMIP_Jan2017_v3.1_Jul17_AGCLIM52.xlsx"), sheet = "Listing_template")


### GLOBIOM
GLOBIOM_raw <- read_csv(file.path(dataPath, "Results\\agclim50_GLOBIOM_mtg_agmip_12022018_v2.csv"), col_names = F) %>%
  setNames(c("model", "scenario", "region", "item", "variable", "unit", "year", "value")) %>%
  mutate_all(funs(gsub("\'", "", .))) %>%
  mutate(value = as.numeric(value),
         year = as.integer(year),
         variable = toupper(variable)) %>%
  filter(region %in% temp$Region[!is.na(temp$Region)])

# Check if there are variables with missing information for 2010
# Missing values are in fact zero but these are filtered out by GAMS
check2010 <- GLOBIOM_raw %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
GLOBIOM <- GLOBIOM_raw %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, unit, variable) %>%
  filter(any(year==2010)) %>%
  ungroup()

# Check
summary(GLOBIOM)
str(GLOBIOM)

lapply(GLOBIOM[sapply(GLOBIOM, class) == "character"], unique)

xtabs(~item + variable, data = GLOBIOM)
xtabs(~variable + unit, data = GLOBIOM)
rm(check2010)


### IMAGE
# Process
IMAGE_raw <- read_csv(file.path(dataPath, "Results/AGCLIM50-II_IMAGE_AGMIP_01032018.csv")) %>%
  dplyr::rename(model = Model, scenario = Scenario, region = Region, item = Item, unit = Unit, variable = Variable, year = Year, value = Value) %>%
  mutate(year = as.numeric(year))

# Check if there are variables with missing information for 2010
check2010 <- IMAGE_raw %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
IMAGE <- IMAGE_raw %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, unit, variable) %>%
  filter(any(year==2010)) %>%
  ungroup()

# Check
summary(IMAGE)
str(IMAGE)

lapply(IMAGE[sapply(IMAGE, class) == "character"], unique)

xtabs(~item + variable, data = IMAGE)
xtabs(~variable + unit, data = IMAGE)
rm(check2010)


### MAGNET
MAGNET_raw <- read_csv(file.path(dataPath, "Results/agCLIM50II_MAGNET_2018-03-30.csv"))

### REPLACE 2011 with 2010 values 
MAGNET_raw <- MAGNET_raw %>%
  mutate(year = ifelse(year == 2011, 2010, year)) 

# Check if there are variables with missing information for 2010
check2010 <- MAGNET_raw %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
MAGNET <- MAGNET_raw %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, unit, variable) %>%
  filter(any(year==2010)) %>%
  ungroup()

# Check
summary(MAGNET)
str(MAGNET)

lapply(MAGNET[sapply(MAGNET, class) == "character"], unique)

xtabs(~item + variable, data = MAGNET)
xtabs(~variable + unit, data = MAGNET)
rm(check2010)


### CAPRI
# Scenario list
capri_scenario_list <- read_csv(file.path(dataPath, "Results/capri_scenario_list.csv"))

CAPRI_raw <- read_delim(file.path(dataPath, "Results/AgMip_CAPRI_results_20180305.csv"), delim = ";") %>%
  setNames(c("model", "scenario_capri", "region", "item", "variable", "year", "unit", "value")) %>%
  filter(region %in% temp$Region[!is.na(temp$Region)]) %>%
  left_join(capri_scenario_list) %>%
  dplyr::select(-scenario_capri)

# Check if there are variables with missing information for 2010
check2010 <- CAPRI_raw %>%
  arrange(model, scenario, region, item, variable, year) %>%
  group_by(model, scenario, region, item, variable) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
CAPRI <- CAPRI_raw %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, unit, variable) %>%
  filter(any(year==2010)) %>% 
  ungroup

# Check
summary(CAPRI)
str(CAPRI)

lapply(CAPRI[lapply(CAPRI, class) == "character"], unique)

xtabs(~item + variable, data = CAPRI)
xtabs(~variable + unit, data = CAPRI)
rm(check2010)


### RAW FILE FOR GAMS PROCESSING
total_gams <- bind_rows(GLOBIOM_raw, MAGNET_raw, IMAGE_raw, CAPRI_raw) %>%
  ungroup()

total_gams_gdx <- para_gdx(total_gams, c("model", "scenario", "region", "item",
                                         "variable", "year", "unit"), "agclim50II", "data")

wgdx(paste0("I:/frank/agclim50II_total_gams_format_", Sys.Date(), ".gdx"), total_gams_gdx)


### CLEANED FILE FOR PLOTTING
# Bind in one file
total <- bind_rows(MAGNET, GLOBIOM, IMAGE, CAPRI) %>% 
              filter(year>=2010) %>% 
  ungroup()

# Calculate index
total <- total %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  mutate(index = (value/value[year==2010]*1)) %>%
  arrange(model, scenario, variable, region, item, unit, year)

# Check
summary(total)

# Remove NAN values for IMAGE because 2010 values are 0
inf.nan.na.clean_f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  return(x)
}

total <- inf.nan.na.clean_f(total) %>% 
  filter(!is.na(index))

# Checks on missing values
check_miss <- total %>%
  group_by(model, variable, item) %>%
  summarize(
    n=n()
    #miss = sum(is.na(Value)),
    #mean = mean(value, na.rm=TRUE)
  ) %>%
  spread(model, n)

xtabs(~unit + model, data = total)
xtabs(~variable + model, data = total)
xtabs(~region + model, data = total)
xtabs(~scenario + model, data = total)


### SAVE DATA
# csv
write_csv(total, file.path(dataPath, paste0("Results\\agclim50II_total_", Sys.Date(), ".csv")))
