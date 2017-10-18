MAGNET1_raw[["AREA"]] <- current.f("AREA", "BaseData_b.gdx", "LTYPEDEM", lookup_upd, "LDEM", c("PROD_SECT", "ENDWL_COMM", "REG"), c("PROD_SECT", "ENDWL_COMM", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  mutate(value = value/10, # MAGNET AREA is in km2
         unit = "1000 ha")
input <- varname <- "AREA"
base_file <- "Basedata_b.gdx"
path <- dataResultPath
var <- varbase<- "LTYPEDEM"
group.var <- set.names <- set_names <- c("PROD_SECT", "ENDWL_COMM", "REG")
scenariofile <- lookup_upd
varsen <- "LDEM"



MAGNET1_raw[["AREA"]] <- 
  
  x <- current.f("AREA", "BaseData_b.gdx", "LTYPEDEM", lookup_upd, "LDEM", c("PROD_SECT", "ENDWL_COMM", "REG"), c("PROD_SECT", "ENDWL_COMM", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  mutate(value = value/10, # MAGNET AREA is in km2
         unit = "1000 ha")

# function to create current series
current.f <- function(varname, basefile, varbase, scenariofile, varsen, set.names, group.var){
  baseValue <- load_gdx2(basefile, dataResultPath, varbase, set.names) %>%
    group_by_(.dots = group.var) %>%
    summarize(value = sum(value, na.rm=T))
  
  group.var2 <- c(group.var, "scenario", "year")
  
  scenfile <- dplyr::select_(scenariofile, .dots = c("gdxResultFiles", "year", "scenario"))
  
  scenValue <- adply(scenfile, 1, load_gdx, dataResultPath, varsen, set.names) %>%
    dplyr::select(-gdxResultFiles) %>%
    group_by_(.dots = group.var2) %>%
    summarize(value = sum(value, na.rm=T)) %>%
    mutate(variable = varname)
  
  scen <- unique(scenValue$scenario)
  
  base.scenario.f <- function(scen, base, varname) {
    base$scenario <- scen
    base$year <- "2007"
    base$variable <- varname
    return(base)
  } 
  
  baseValue2 <- ldply(scen, function(x,y,z) base.scenario.f(x, baseValue, varname))
  
  currValue <-   baseValue2 %>%
    bind_rows(., scenValue) %>%
    arrange_(.dots = group.var2) %>%
    ungroup()
  
  return(currValue)
}
