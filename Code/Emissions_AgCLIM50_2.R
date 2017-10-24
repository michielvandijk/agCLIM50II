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


### SECTORS
sec1 <- c("pdr", "wht", "grain", "oils", "sug", "hort", "crops", "cattle", "pigpoul", "milk")

### CH4
GCH4 <- current.f("GCH4", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "FUELX", "FUELUSER", "REG")) %>%
  filter(GHG == "CH4", FUELX == "Act", FUELUSER %in% sec1) %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)

### N2O
GN2O <- current.f("GN2O", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "FUELX", "FUELUSER", "REG")) %>%
  filter(GHG == "N2O", FUELX == "Act", FUELUSER %in% sec1) %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG)




### ADDITIONAL EMISSIONS 
# PROT
QPROD <- current.f("QPROD", "BaseData_b.gdx",  "QPROD", lookup_upd, "QPROD", c("PROD_SECT", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  filter(TRAD_COMM %in% sec1) %>%
  mutate(value = value/1000, unit = "1000 tons")

# CONT
QCONS <- current.f("QCONS", "BaseData_b_view.gdx",  "NQSECT", lookup_upd_view, "NQSECT", c("NUTRIENTS", "PRIM_AGRI", "REG"), c("NUTRIENTS", "PRIM_AGRI","REG"))  %>%
  rename(TRAD_COMM = PRIM_AGRI) %>%
  filter(NUTRIENTS == "QUANT", TRAD_COMM %in% sec1) %>%
  mutate(value = value/1000, unit = "1000 tons") %>%
  dplyr::select(-NUTRIENTS)
