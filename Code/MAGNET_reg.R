#'========================================================================================================================================
#' Project:  AGCLIM50II
#' Subject:  Code to create regional level variables
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


############################################
### Variables with only region dimension ###
############################################

# GDP volume
GDPT <-  constant2.f("GDPT","BaseData_b_view.gdx", "GDPSRC", c("REG", "GDPSOURCE"), "REG", "qgdp", "REG") %>%
  mutate(value = value/1000, unit = "bn USD 2007 MER")

# POP total population
POPT <- constant2.f("POPT", "BaseData_b.gdx", "POP", c("REG"), c("REG"), "pop", c("REG")) %>%
  mutate(unit = "mn pers")

# GDP value = GDPSRC(SREG,SUM) AND GDPSRC(SREG,SUM)  (NOT certified)
GDPval <- current.f("GDPval", "BaseData_b_view.gdx", "GDPSRC", lookup_upd_view, "GDPSRC", c("REG", "GDPSOURCE"), c("REG")) %>%
  mutate(value = value/1000, unit = "bn USD MER")

# Calories
NQT <- current.f("NQT", "BaseData_b_view.gdx",  "NQT", lookup_upd_view, "NQT", c("NUTRIENTS", "REG"), c("NUTRIENTS", "REG")) %>%
  rename(unit = NUTRIENTS) %>%
  filter(unit == "CAL")

# ECO2
ECO2 <- current.f("ECO2", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "REG")) %>%
  filter(GHG == "CO2") %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)

# EN2O
EN2O <- current.f("EN2O", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "REG")) %>%
  filter(GHG == "N2O") %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)

# ECO2
ECH4 <- current.f("ECH4", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "REG")) %>%
  filter(GHG == "CH4") %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)

# EMIS
EMIS <- bind_rows(EN2O, ECO2, ECH4) %>%
  group_by(REG, scenario, year, unit) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(variable = "EMIS")


### COMBINE
MAGNET_reg <- bind_rows(GDPT, POPT, GDPval, NQT, ECH4, ECO2, EN2O, EMIS)

  
## CLEAN UP
rm(GDPT, POPT, GDPval, NQT, ECH4, ECO2, EN2O, EMIS)
