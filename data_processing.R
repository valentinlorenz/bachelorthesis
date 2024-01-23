library(rgdal)
library(knitr)
library(sf)
library(scales)
library(maptools)
library(ggplot2)
library(plyr)

setwd("c:/Users/swenj/academiccloudsync/MoveNsense/GIS")
setwd("D:/RStudio")

ogrListLayers("data.gdb")

# IMPORT

pop_dens <- readOGR("data.gdb", layer="population")
pop_old <- readOGR("data.gdb", layer="popold")
pop_young <- readOGR("data.gdb", layer="popyoung")
quiet <- readOGR("data.gdb", layer="quietness")
build_count <- readOGR("data.gdb", layer="buildingDensity")
build_perc <- readOGR("data.gdb", layer="buildingDensity_percent")
single_fam <- readOGR("data.gdb", layer="singleFamilyHousing")
prot_areas <- readOGR("data.gdb", layer="protectedareas_grid")
open_veg <- readOGR("data.gdb", layer="openveg")
closed_veg <- readOGR("data.gdb", layer="closedvegetation")
rec_green <- readOGR("data.gdb", layer="recreationalgreenspace")
unsealed <- readOGR("data.gdb", layer="unsealedsurface")
roads <- readOGR("data.gdb", layer="streetlen")
bus <- readOGR("data.gdb", layer="publictransport")
culture <- readOGR("data.gdb", layer="culture")
supply <- readOGR("data.gdb", layer="supply")
public <- readOGR("data.gdb", layer="public")
ph <- readOGR("data.gdb", layer="soilph")
carbon <- readOGR("data.gdb", layer="organiccarbon")
rightwing <- readOGR("data.gdb", layer="rightwing_votes")
leftwing <- readOGR("data.gdb", layer="leftwing_votes")
participation <- readOGR("data.gdb", layer="participation")
realestate <- readOGR("data.gdb", layer="realestate")
elevation <- readOGR("data.gdb", layer="elevation")
slope <- readOGR("data.gdb", layer="slope")
river_dist <- readOGR("data.gdb", layer="distanceoder")
ndvi_mean <- readOGR("data.gdb", layer="ndvi")
ndvi_sd <- readOGR("data.gdb", layer="ndvi_sd")
ndvi_max <- readOGR("data.gdb", layer="ndvi_max")
biodiversity <- readOGR("data.gdb", layer="biodiversity_observationpoints")
shapes <- readOGR("data.gdb", layer="ID")

ID <- seq(from=1, to=33783)

# CALCULATE DATA

pop_old_perc <- as.data.frame(ID)
pop_old_perc$pop_old <- (pop_old$grid_code / pop_dens$grid_code)
pop_old_perc[is.na(pop_old_perc)] <- 0

pop_young_perc <- as.data.frame(ID)
pop_young_perc$pop_young <- (pop_young$grid_code / pop_dens$grid_code)
pop_young_perc[is.na(pop_young_perc)] <- 0 
# PROCESSING

vars <- list(pop_young_perc, pop_old_perc, pop_dens, quiet, build_count, build_perc, single_fam, prot_areas, open_veg, closed_veg, rec_green, unsealed, roads, bus, culture, supply, public, ph, carbon, rightwing, leftwing, participation, realestate, elevation, slope, river_dist, ndvi_mean, ndvi_sd, ndvi_max, biodiversity)
vars_names <- list('pop_young', 'pop_old', 'pop_dens', 'quiet', 'build_count', 'build_perc', 'single_fam', 'prot_areas', 'open_veg', 'closed_veg', 'rec_green', 'unsealed', 'roads', 'bus', 'culture', 'supply', 'public', 'ph', 'carbon', 'rightwing', 'leftwing', 'participation', 'realestate', 'elevation', 'slope', 'river_dist', 'ndvi_mean', 'ndvi_sd', 'ndvi_max', 'biodiversity')

# Reduce columns
for (i in seq(from=3, to=30)){
  vars[[i]]$ID <- ID
  vars[[i]] <- vars[[i]][c('grid_code', 'ID')]
}

#Rename columns

for (k in seq(from=3, to=30)){
  names(vars[[k]]@data) <- c(vars_names[[k]], 'ID')
}

# merge dataframes 

variables_df <- as.data.frame(ID)

for (j in seq(from=2, to=30)){
  vars[[j]] <- as.data.frame(vars[[j]])
}

for (var in vars){
  variables_df <- merge(x = variables_df, y = var)
}

# RESCALE DATA

for (i in 2:ncol(variables_df)){
  variables_df[,i] <- rescale(variables_df[,i], to=c(0,1))
}





