#'========================================================================================================================================
#' Project:  AgCLIM50II
#' Subject:  Plots paper
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot","labelled", "formattable")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "leaflet", "mapview")
# Additional packages
p_load("grid")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)


### SET DATAPATH
source(file.path(root, "code/get_dataPath.r"))


### LOAD DATA
total_raw <- read.csv(file.path(dataPath, "Results/agclim50II_total_2018-03-01.csv")) 


### PROCESS DATA
# Remove DT scenario (only MAGNET and IMAGE), order scenarios etc.
total <- total_raw %>%
  filter(scenario != "SSP2_baseDT") %>%
  mutate(scenario = forcats::fct_relevel(scenario,
                                         c("SSP2_base", "SSP2_baseD", "SSP2_CP1000", "SSP2_CP1000D",
                                           "SSP2_CP1250",  "SSP2_CP1250D", "SSP2_CP250", "SSP2_CP2500",
                                           "SSP2_CP2500D", "SSP2_CP250D", "SSP2_CP500", "SSP2_CP500D",  
                                           "SSP2_CP750", "SSP2_CP750D"))) %>%
  separate(scenario, c("SSP", "c_price"), "_", remove = F) %>%
  mutate(scenario_check = str_sub(scenario, start= -1),
         diet = ifelse(scenario_check == "D", "diet", "no_diet"),
         id = paste(item, variable, sep ="_"),
         diff = (index - 1)*100,
         c_price = factor(gsub("D", "", c_price), levels = c("base", "CP250", "CP500", "CP750", "CP1000", "CP1250", "CP2500"))) %>%
         dplyr::select(-scenario_check)

# Create df with only wld 



### CREATE WLD DATABASE FOR BAR PLOT
# Create database for plotting
sel <- c("AGR_PROD", "CRP_AREA", "AGR_XPRP", "LSP_XPRP", "AGR_ECH4", "AGR_EN2O", "TOT_GDPT", "TOT_POPT", "LSP_AREA", "AGR_EMIS")
total_wld <- filter(total, id %in% sel, region == "WLD", year == 2050)

# Check units x variables
xtabs(~ unit + variable + model, data = total_wld)

# filter out mn USD  which is only presented in MAGNET
total_wld <- filter(total_wld, (!(unit == "mn USD" & model == "MAGNET") &
                                  !(unit == "1000 t" & model == "GLOBIOM")))


### BARPLOTS WITH GROWTH
# bar plot with models on x-axis
barplot_f <- function(df, id_sel){
  df <- filter(df, id == id_sel)
  title <- paste(df$variable, df$item, sep = "_")
  p = ggplot(data = df, aes(x = model, y = diff, shape = model, colour = diet)) +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_point() + 
    facet_grid(~c_price) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("2010-2050 growth (%)")
  p
}
lapply(sel, function(x) barplot_f(total_wld, x))

# Barplot with diet on the x-axis
barplot2_f <- function(df, id_sel){
  df <- filter(df, id == id_sel)
  title <- paste(df$variable, df$item, sep = "_")
  p = ggplot(data = df, aes(x = diet, y = diff, shape = model, colour = model)) +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_point() + 
    facet_grid(~c_price) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("2010-2050 growth (%)")
  p
}
lapply(sel, function(x) barplot2_f(total_wld, x))


# Barplot with scenario on the x-axis
barplot3_f <- function(df, id_sel){
  df <- filter(df, id == id_sel)
  title <- paste(df$variable, df$item, sep = "_")
  p = ggplot(data = df, aes(x = c_price, y = diff, shape = model, colour = model)) +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_point() + 
    facet_grid(~diet) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("2010-2050 growth (%)")
  p
}
lapply(sel, function(x) barplot3_f(total_wld, x))

### CREATE WLD DATABASE FOR BAR PLOT
# Create database for plotting
sel <- c("AGR_PROD", "CRP_AREA", "AGR_XPRP", "LSP_XPRP", "AGR_ECH4", "AGR_EN2O", "TOT_GDPT", "TOT_POPT", "LSP_AREA", "AGR_EMIS")
total_wld2 <- filter(total, id %in% sel, region == "WLD")

# Check units x variables
xtabs(~ unit + variable + model, data = total_wld2)

# filter out duplicate unit-variable combinations
total_wld2 <- filter(total_wld2, (!(unit == "mn USD" & model == "MAGNET") &
                       !(unit == "1000 t" & model == "GLOBIOM")))

unique(total_wld2$unit)

# levelplot by diet
levelplot_f <- function(df, id_sel){
  df <- filter(df, id == id_sel)
  title <- paste(df$variable, df$item, sep = "_")
  p = ggplot() +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = c_price)) + 
    geom_point(data = df, aes(x = year, y = value, shape = model, colour = c_price)) +
    facet_grid(~diet) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = paste(unique(df$unit), collapse = "___")) +
    theme_bw()
  p
}

lapply(sel, function(x) levelplot_f(total_wld2, x))


# levelplot by diet
levelplot2_f <- function(df, id_sel){
  df <- filter(df, id == id_sel)
  title <- paste(df$variable, df$item, sep = "_")
  p = ggplot() +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_line(data = df, aes(x = year, y = value, linetype = diet, colour = model)) + 
    geom_point(data = df, aes(x = year, y = value, shape = model, colour = model)) +
    facet_grid(~c_price) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = paste(unique(df$unit), collapse = "___")) +
    theme_bw()
  p
}

lapply(sel, function(x) levelplot2_f(total_wld2, x))


levelplot3_f <- function(df, id_sel){
  df <- filter(df, id == id_sel)
  title <- paste(df$variable, df$item, sep = "_")
  p = ggplot() +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = model)) + 
    geom_point(data = df, aes(x = year, y = value, shape = model, colour = model)) +
    facet_grid(diet~c_price) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = paste(unique(df$unit), collapse = "___")) +
    theme_bw()
  p
}

lapply(sel, function(x) levelplot3_f(total_wld2, x))


### GDP AND POP PLOTS
# gdp
gdp <- total %>%
  filter(variable %in% c("GDPT"),
         diet == "no_diet",
         year == 2050,
         model == "MAGNET",
         c_price == "base") 

# Plots
fig_gdp = ggplot(data = gdp, aes(x = SSP, y = index, fill = SSP)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "Index (2010=1)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
 theme(legend.position = "bottom") +
 theme(panel.spacing = unit(0, "lines")) +
 theme(panel.border = element_blank()) +
 theme(strip.background = element_rect(fill = NA, colour = "black")) +
 theme(axis.line.y = element_line(color="black"))

fig_gdp

fig_gdp2 = ggplot(data = gdp, aes(x = SSP, y = diff, fill = SSP)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  #coord_cartesian(ylim = c(0,12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_gdp2

# pop
pop <- total %>%
  filter(variable %in% c("POPT"),
         diet == "no_diet",
         year == 2050,
         model == "MAGNET",
         c_price == "base") 

fig_pop = ggplot(data = pop, aes(x = SSP, y = index, fill = SSP)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "Index (2010=1)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,2.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_pop

fig_pop2 = ggplot(data = pop, aes(x = SSP, y = diff, fill = SSP)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(-10,150)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_pop2