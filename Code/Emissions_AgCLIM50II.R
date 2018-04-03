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

### load fm2dm
fm2dm <- read_xlsx(file.path(root, "Mappings/MAGNET_fm2dm.xlsx"), sheet = "fm2dm") %>%
  dplyr::select(TRAD_COMM, fm2dm)


### SECTORS
sec1 <- c("pdr", "wht", "grain", "oils", "sug", "hort", "crops", "oagr", "cattle", "pigpoul", "milk")
sec2 <- c("pdr", "wht", "grain", "oils", "sug", "hort", "oagr", "crops")
sec3 <- c("cattle", "pigpoul", "milk")
sec4 <- c("pdr", "cattle", "pigpoul", "milk")

### CH4
GCH4 <- current.f("GCH4", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "FUELX", "FUELUSER", "REG")) %>%
  filter(GHG == "CH4", FUELX == "Act", FUELUSER %in% sec4) %>%
  mutate(unit = "MtCO2e") %>%
  dplyr::select(-GHG, -FUELX) %>%
  rename(TRAD_COMM = FUELUSER)

### N2O
GN2O <- bind_rows(
  current.f("GN2O", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "FUELX", "FUELUSER", "REG")) %>%
    filter(GHG == "N2O", FUELX == "fert", FUELUSER %in% sec2) %>%
    mutate(unit = "MtCO2e") %>%
    dplyr::select(-GHG),
  current.f("GN2O", "BaseData_b.gdx",  "QGHGX_GAS", lookup_upd, "DQGHGX", c("GHG", "FUELX", "FUELUSER", "REG"), c("GHG", "FUELX", "FUELUSER", "REG")) %>%
    filter(GHG == "N2O", FUELX == "Act", FUELUSER %in% sec3) %>%
    mutate(unit = "MtCO2e")) %>%
    dplyr::select(-GHG, -FUELX) %>%
  rename(TRAD_COMM = FUELUSER)


### ADDITIONAL EMISSIONS 
# PROT
PROD <- current.f("PROD", "BaseData_b.gdx",  "QPROD", lookup_upd, "QPROD", c("PROD_SECT", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  filter(TRAD_COMM %in% sec1) %>%
  left_join(fm2dm) %>%
  mutate(value = value/1000*fm2dm, unit = "1000 t") %>%
  dplyr::select(-fm2dm)
  

# FOOD
FOOD <- current.f("FOOD", "BaseData_b_view.gdx",  "NQSECT", lookup_upd_view, "NQSECT", c("NUTRIENTS", "PRIM_AGRI", "REG"), c("NUTRIENTS", "PRIM_AGRI","REG"))  %>%
  rename(TRAD_COMM = PRIM_AGRI) %>%
  filter(NUTRIENTS == "QUANT", TRAD_COMM %in% sec1) %>%
  mutate(value = value/1000, unit = "1000 t") %>%
  dplyr::select(-NUTRIENTS)


### CN2O
CN2O <- bind_rows(GN2O, PROD) %>%
  filter(scenario == "GDPEndoSSP2") %>%
  group_by(year, REG, TRAD_COMM) %>%
  summarize(CN2O = value[variable == "GN2O"]/value[variable == "PROD"]) %>%
  mutate(CN2O = ifelse(is.infinite(CN2O), 0, CN2O),
         CN2O = ifelse(is.na(CN2O), 0, CN2O))
  

### CCH4  
CCH4 <- bind_rows(GCH4, PROD) %>%
  filter(scenario == "GDPEndoSSP2", TRAD_COMM %in% sec4) %>%
  group_by(year, REG, TRAD_COMM) %>%
  summarize(CCH4 = value[variable == "GCH4"]/value[variable == "PROD"])  %>%
  mutate(CCH4 = ifelse(is.infinite(CCH4), 0, CCH4),
         CCH4 = ifelse(is.na(CCH4), 0, CCH4))


  
### EMRF_CH4
EMRF_CH4 <- bind_rows(
  rgdx.param(file.path(dataResultPath, "Emissions_Reduct_Tax.gdx"), "OLDEMRD", 
                   names = c("CtaxTar", "year", "GHG", "FUELX", "TRAD_COMM", "REG", "EMRF"), compres = T) %>%
  filter((GHG =="CH4" & FUELX == "Act" & TRAD_COMM %in% sec1)) %>%
  mutate(year = as.character(gsub("Y", "", year)),
         scenario = paste0("GDPEndoSSP2_", CtaxTar),
         scenario = gsub("tax", "", scenario)) %>%
  dplyr::select(-CtaxTar, -FUELX, -GHG),
  rgdx.param(file.path(dataResultPath, "Emissions_Reduct_Tax.gdx"), "OLDEMRD", 
             names = c("CtaxTar", "year", "GHG", "FUELX", "TRAD_COMM", "REG", "EMRF"), compres = T) %>%
    filter((GHG =="CH4" & FUELX == "Act" & TRAD_COMM %in% sec1)) %>%
    mutate(year = as.character(gsub("Y", "", year)),
           scenario = paste0("GDPEndoSSP2_", CtaxTar, "D"),
           scenario = gsub("tax", "", scenario)) %>%
    dplyr::select(-CtaxTar, -FUELX, -GHG)
)


# EMRF_N2O
EMRF_N2O <- bind_rows(
  rgdx.param(file.path(dataResultPath, "Emissions_Reduct_Tax.gdx"), "OLDEMRD", 
                       names = c("CtaxTar", "year", "GHG", "FUELX", "TRAD_COMM", "REG", "EMRF"), compres = T) %>%
  filter((GHG == "N2O" & FUELX == "Act"& TRAD_COMM %in% sec3) |
           (GHG == "N2O" & FUELX == "fert" & TRAD_COMM %in% sec2)) %>%
  mutate(year = as.character(gsub("Y", "", year)),
         scenario = paste0("GDPEndoSSP2_", CtaxTar),
         scenario = gsub("tax", "", scenario)) %>%
  dplyr::select(-CtaxTar, -FUELX, -GHG),
  rgdx.param(file.path(dataResultPath, "Emissions_Reduct_Tax.gdx"), "OLDEMRD", 
             names = c("CtaxTar", "year", "GHG", "FUELX", "TRAD_COMM", "REG", "EMRF"), compres = T) %>%
    filter((GHG == "N2O" & FUELX == "Act"& TRAD_COMM %in% sec3) |
             (GHG == "N2O" & FUELX == "fert" & TRAD_COMM %in% sec2)) %>%
    mutate(year = as.character(gsub("Y", "", year)),
           scenario = paste0("GDPEndoSSP2_", CtaxTar, "D"),
           scenario = gsub("tax", "", scenario)) %>%
    dplyr::select(-CtaxTar, -FUELX, -GHG)
)


### TCH4 
##### SET INF AND NAN TO 0
TCH4 <- full_join(PROD, CCH4) %>%
  left_join(EMRF_CH4) %>%
  filter(scenario != "GDPEndoSSP2", year !=2011, 
         TRAD_COMM %in% sec4) %>%
  mutate(EMRF = ifelse(is.na(EMRF), 0, EMRF),
         value = value * CCH4 * EMRF,
         variable = "TCH4", 
         unit = "MtCO2e") %>%
  dplyr::select(-CCH4, -EMRF)
summary(TCH4)


### TN2O 
##### SET INF AND NAN TO 0
TN2O <- full_join(PROD, CN2O) %>%
  left_join(EMRF_N2O) %>%
  filter(scenario != "GDPEndoSSP2", year !=2011, 
         TRAD_COMM %in% sec1) %>%
  mutate(EMRF = ifelse(is.na(EMRF), 0, EMRF),
         value = value * CN2O * EMRF,
         variable = "TN2O",
         unit = "MtCO2e") %>%
  dplyr::select(-CN2O, -EMRF)
summary(TN2O)

### COMBINE
emis_agclim50II <- bind_rows(PROD, FOOD, GCH4, GN2O, TCH4, TN2O)


### CTAX


### CLEAN UP
rm(EMRF_CH4, EMRF_N2O, CCH4, CN2O, PROD, FOOD, GCH4, GN2O, TCH4, TN2O)
