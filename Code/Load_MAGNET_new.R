
### UPDATED GDX READ FILES

# Function to remove factors
unfactorize <- function(df){
  for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
  return(df)
}

# Function load gdx file
load_gdx <- function(input, path, var, set_names){
  file <- file.path(path, base_file)
  gdx <- rgdx.param(file, var, compress=T, names=set_names, squeeze = FALSE) 
  
  # Select factors
  factors <- gdx[sapply(gdx, class) =="factor"]
  factor_levels <- lapply(factors, levels)
  factor_combinations <- expand.grid(factor_levels)
  names(factor_combinations) <- set_names
  output <- left_join(factor_combinations, gdx)
  output[is.na(output)] <- 0
  names(output) <- c(set_names, "value")
  output <- unfactorize(output)
  return(output)
}


load_gdx2 <- function(input, path, var, set_names){
  file <- file.path(path, input["gdxResultFiles"])
  gdx <- rgdx.param(file, var, compress=T, names=set_names, squeeze = FALSE) 
  
  # Select factors
  factors <- gdx[sapply(gdx, class) =="factor"]
  factor_levels <- lapply(factors, levels)
  factor_combinations <- expand.grid(factor_levels)
  names(factor_combinations) <- set_names
  output <- left_join(factor_combinations, gdx)
  output[is.na(output)] <- 0
  names(output) <- c(set_names, "value")
  output <- unfactorize(output)
  return(output)
}


var.extract.f<-function(input, path, var, set.names){
  file <- file.path(path, input["gdxResultFiles"])
  #TMP<-rgdx.param(file, var, compress=FALSE, names=set.names, squeeze = FALSE)
  TMP <- rgdx(file,list(name = var, compress = T, form = 'full'))
  TMP <-TMP$val
  ncol <- ncol(TMP)
  if (ncol < 2){
    TMP <- data.frame(V1 = row.names(TMP), V2 = TMP[,1])
    row.names(TMP) <- NULL
  } else {
    TMP <- as.data.frame(as.table(TMP))
  }
  names(TMP) <- c(set.names, "value")
  TMP <-unfactorize(TMP)
  return(TMP)
}




  

current.f <- function(varname, basefile, varbase, scenariofile, varsen, set.names, group.var){
  
  baseValue <- load_gdx(basefile, dataResultPath, varbase, set.names) %>%
    group_by_(.dots = group.var) %>%
    dplyr::summarize(value = sum(value, na.rm=T))
  
  group.var2 <- c(group.var, "scenario", "year")
  
  scenfile <- dplyr::select_(scenariofile, .dots = c("gdxResultFiles", "year", "scenario"))
  
  scenValue <- adply(scenfile, 1, load_gdx2, dataResultPath, varsen, set.names) %>%
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
