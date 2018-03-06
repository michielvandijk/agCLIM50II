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
total_raw <- read.csv(file.path(dataPath, "Results/agclim50II_total_2018-03-06.csv")) 


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
         id = paste(variable, item, sep ="_"),
         diff = (index - 1)*100,
         c_price = factor(gsub("D", "", c_price), levels = c("base", "CP250", "CP500", "CP750", "CP1000", "CP1250", "CP2500"))) %>%
  dplyr::select(-scenario_check)


### CREATE WLD DATABASE FOR RELEVANT VARIABLES
# Create database for plotting
sel <- c("PROD_AGR", "PROD_CRP", "PROD_LSP", "AREA_CRP", "AREA_LSP", "XPRP_AGR", "XPRP_CRP", "XPRP_LSP",
         "ECH4_AGR", "EN2O_AGR", "EMIS_AGR", "EMIS_CRP", "EMIS_LSP", "GDPT_TOT", "POPT_TOT", "YEXO_CRP", 
         "LYXO_LSP")

# Check units x variables
xtabs(~ unit + variable + model, data = filter(total, model == "GLOBIOM"))
xtabs(~ variable + model, data = total)

# filter out some duplicate units
total <- filter(total, (!(unit == "mn USD" & model == "MAGNET") &
                                  !(unit == "1000 t" & model == "GLOBIOM") &
                                  !(unit == "fm t/ha" & variable == "YEXO" & model == "GLOBIOM")))


### BARPLOTS WITH GROWTH
# bar plot with models on x-axis
barplot_f <- function(df, id_sel, reg = "WLD", yr = 2050){
  df <- filter(df, id == id_sel, region == reg, year == yr)
  title <- paste(df$variable, df$item, df$region, sep = "_")
  p = ggplot(data = df, aes(x = model, y = diff, shape = model, colour = diet)) +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_point() + 
    facet_grid(~c_price) +
    ggtitle(title) +
    ylab("2010-2050 growth (%)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
}

regions <- unique(total$region)

#lapply(sel, function(x) barplot_f(total, x))


# Barplot with diet on the x-axis
barplot2_f <- function(df, id_sel, reg = "WLD", yr = 2050){
  df <- filter(df, id == id_sel, region == reg, year == yr)
  title <- paste(df$variable, df$item, df$region, sep = "_")
  p = ggplot(data = df, aes(x = diet, y = diff, shape = model, colour = model)) +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_point() + 
    facet_grid(~c_price) +
    ggtitle(title) +
    ylab("2010-2050 growth (%)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
}

#lapply(sel, function(x) barplot2_f(total, x))


# Barplot with scenario on the x-axis
barplot3_f <- function(df, id_sel, reg = "WLD", yr = 2050){
  df <- filter(df, id == id_sel, region == reg, year == yr)
  title <- paste(df$variable, df$item, df$region, sep = "_")
  p = ggplot(data = df, aes(x = c_price, y = diff, shape = model, colour = model)) +
    #scale_fill_manual(values = colour) +
    #geom_bar(stat="identity", colour = "black") + 
    geom_point() + 
    facet_grid(~diet) +
    ggtitle(title) +
    ylab("2010-2050 growth (%)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
}

#lapply(sel, function(x) barplot3_f(total, x))



### LEVEL PLOT
# levelplot by diet
levelplot_f <- function(df, id_sel, reg = "WLD"){
  df <- filter(df, id == id_sel, region == reg)
  title <- paste(df$variable, df$item, df$region, sep = "_")
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
#lapply(sel, function(x) levelplot_f(total, x))
#map2(c("CALO_AGR"), regions, function(a,b) levelplot_f(total, a, b))
#map2(c("CALO_LSP"), regions, function(a,b) levelplot_f(total, a, b))


# levelplot by diet
levelplot2_f <- function(df, id_sel, reg = "WLD"){
  df <- filter(df, id == id_sel, region == reg)
  title <- paste(df$variable, df$item, df$region, sep = "_")
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

#lapply(sel, function(x) levelplot2_f(total, x))


levelplot3_f <- function(df, id_sel, reg = "WLD"){
  df <- filter(df, id == id_sel, region == reg)
  title <- paste(df$variable, df$item, df$region, sep = "_")
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
#lapply(sel, function(x) levelplot3_f(total, x))
#map2(c("CALO_AGR"), regions, function(a,b) levelplot3_f(total, a, b))
#map2(c("CALO_LSP"), regions, function(a,b) barplot3_f(total, a, b))


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