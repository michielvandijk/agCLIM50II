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


### LOAD REPORTING TEMPLATE
temp <- read_excel(file.path(dataPath, "Reporting/Reporting_template_AGMIP_AgCLIM_Jul2016_v3.xlsx"), sheet = "Listing_template")


### GLOBIOM
# Process
GLOBIOM <- read_csv(file.path(dataPath, "Results\\agclim50_GLOBIOM_mtg_agmip_07072017_v2.csv"), col_names = F) %>%
  setNames(c("model", "scenario", "region", "item", "variable", "unit", "year", "value")) %>%
  mutate_all(funs(gsub("\'", "", .))) %>%
  mutate(value = as.numeric(value),
         year = as.integer(year),
         variable = toupper(variable)) %>%
  filter(region %in% temp$Region[!is.na(temp$Region)])

# Check if there are variables with missing information for 2010
# Missing values are in fact zero but these are filtered out by GAMS
check2010 <- GLOBIOM %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
GLOBIOM <- GLOBIOM %>%
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
IMAGE <- read_csv(file.path(dataPath, "Results/AGCLIM50-II_IMAGE_AGMIP_02102017.csv")) %>%
  rename(model = Model, scenario = Scenario, region = Region, item = Item, unit = Unit, variable = Variable, year = Year, value = Value) %>%
  mutate(year = as.numeric(year))

# Check if there are variables with missing information for 2010
check2010 <- IMAGE %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Check
summary(IMAGE)
str(IMAGE)

lapply(IMAGE[sapply(IMAGE, class) == "character"], unique)

xtabs(~item + variable, data = IMAGE)
xtabs(~variable + unit, data = IMAGE)
rm(check2010)


### MAGNET
MAGNET <- read_csv(file.path(dataPath, "Results/agCLIM50II_MAGNET_2017-11-04.csv"))

### REPLACE 2011 with 2010 values?
MAGNET <- MAGNET %>%
  mutate(year = ifelse(year == 2011, 2010, year))

# Check if there are variables with missing information for 2010
check2010 <- MAGNET %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
MAGNET <- MAGNET %>%
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
CAPRI <- read_csv(file.path(dataPath, "Results/171025_AGCLIM50-2_CAPRI_results.csv")) %>%
  setNames(c("region", "variable", "item", "year", "scenario", "value")) %>%
  mutate(model = "CAPRI",
         unit = NA) %>%
  filter(region %in% temp$Region[!is.na(temp$Region)])

# Remove CAPRI baseline
CAPRI <- filter(CAPRI, scenario != "BASELINE")

# Check if there are variables with missing information for 2010
check2010 <- CAPRI %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
CAPRI <- CAPRI %>%
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


# Bind in one file
total <- bind_rows(MAGNET, GLOBIOM, IMAGE, CAPRI) %>% 
              filter(year>=2010)

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

# Save data
write_csv(total, file.path(dataPath, paste0("Results\\agclim50II_total_", Sys.Date(), ".csv")))
