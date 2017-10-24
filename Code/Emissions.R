#'========================================================================================================================================
#' Project:  AGCLIM50II
#' Subject:  Code to Create emissions variables
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("plyr", "tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "lazyeval")
# Additional packages
p_load("gdxrrw")


### SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### load required GAMS libraries (folder user specific)
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.


### ECO2
ECO2 <- current.f("ECO2", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "REG")) %>%
  filter(GHG == "CO2") %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)

### EN2O
EN2O <- current.f("EN2O", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "REG")) %>%
  filter(GHG == "N2O") %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)

### ECO2
ECH4 <- current.f("ECH4", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "REG")) %>%
  filter(GHG == "CH4") %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)

### EMIS
EMIS <- bind_rows(EN2O, ECO2, ECH4) %>%
  group_by(REG, scenario, year, unit) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(variable = "EMIS")


### COMBINE
emissions <- bind_rows(ECH4, ECO2, EN2O, EMIS)

## CLEAN UP
rm(ECH4, ECO2, EN2O, EMIS)
