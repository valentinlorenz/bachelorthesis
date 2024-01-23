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


#### DATA PRE-PROCESSING ####

# ph: replace value 0 with N/A
ph$grid_code[which(ph$grid_code == 0)] <- NA

# carbon: replace value 0 with N/A
carbon$grid_code[which(carbon$grid_code == 0)] <- NA

# old population: calculate ratio of overall population to people > 64

pop_old_perc <- as.data.frame(ID)
pop_old_perc$grid_code <- (pop_old$grid_code / pop_dens$grid_code)

# young population: - " - 

pop_young_perc <- as.data.frame(ID)
pop_young_perc$grid_code <- (pop_young$grid_code / pop_dens$grid_code)


#### Combine data into same dataframe 

vars <- list(pop_young_perc, pop_old_perc, pop_dens, quiet, build_count, build_perc, single_fam, prot_areas, open_veg, closed_veg, rec_green, unsealed, roads, bus, culture, supply, public, ph, carbon, rightwing, leftwing, participation, realestate, elevation, slope, river_dist, ndvi_mean, ndvi_sd, ndvi_max, biodiversity)
vars_names <- list('pop_yng', 'pop_old', 'pop_dens', 'quiet', 'buildings', 'build_per', 'single_fam', 'protected', 'open_veg', 'treecover', 'rec_green', 'unsealed', 'roads', 'bus_dist', 'culture', 'supply', 'public', 'ph', 'carbon', 'right', 'left', 'particip', 'realest', 'elevation', 'slope', 'river_dist', 'ndvi_mean', 'ndvi_sd', 'ndvi_max', 'biodiverse')

# Reduce columns to two: ID & grid_code
for (i in seq(from=1, to=30)){
  vars[[i]]$ID <- ID
  vars[[i]] <- vars[[i]][c('grid_code', 'ID')]
}

#Rename columns
for (k in seq(from=1, to=30)){
  names(vars[[k]]) <- c(vars_names[[k]], 'ID')
}

### merge dataframes 

variables_df <- as.data.frame(ID)

# convert all data to dataframes
for (j in seq(from=1, to=30)){
  vars[[j]] <- as.data.frame(vars[[j]])
}

# merge data
for (var in vars){
  variables_df <- merge(x = variables_df, y = var, by="ID")
}

# remove geometry columns

variables_df <- variables_df[, which(names(variables_df) %in% vars_names)]


#### MORE DATA PROCESSING ####

# single family housing: replace values in area with 0 houses with NA
variables_df$single_fam <- single_fam$grid_code
variables_df$single_fam[which(variables_df$buildings == 0 & variables_df$build_per == 0)] <- NA


#### PLOT DATA ####

par(mfrow = c(5, 6))
for (var in variables_df){
  hist(var)
}

#### EXPORT DATA AS SHAPEFILE ####

geometry_df <- as.data.frame(ID)
geometry_df$geometry <- biodiversity$geometry
combined <- cbind(geometry_df, variables_df, by="ID")
combined <- combined[1:32]

combined <- st_as_sf(combined)
plot(combined)

st_write(combined, dsn="D:/data/processed_data/", layer="alldata_nolog", driver="ESRI Shapefile")

#### Import cropped file ####

data <- st_read("D:/data/processed_data/finalextent_nolog.gpkg")

# plot

par(mfrow = c(5, 6))
data <- as.data.frame(data)
for (var in data[2:31]){
  hist(var)
}

# log transform 

data_log <- data

for (i in 2:ncol(data_log)-1){
  data_log[,i] <- data_log[, i] + 1
}

for (var in data_log[2:31]-1){
  hist(var)
}

# RESCALE DATA

for (i in 3:ncol(data_log)-1){
  data_log[,i] <- rescale(data_log[,i], to=c(0,1))
}



st_write(data_log, dsn="D:/data/processed_data/", layer="finalextent_log", driver="ESRI Shapefile")

# READ DATA TO REPLACE OSM POI VALUES

data <- st_read("D:/data/processed_data/finalextent_log.shp")
culture <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/culture_distance_clip.shp")
supply <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/supply_distance_clip.shp")
public <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/public_distance_clip.shp")

# replace old values

data$culture <- culture$grid_code
data$supply <- supply$grid_code
data$public <- public$grid_code

# log + scale new data
data <- as.data.frame(data)
for (i in 16:18){
  data[, i] <- data[, i] + 1
}
for (i in 16:18){
  data[,i] <- rescale(data[,i], to=c(0,1))
}

# remove data that is too homogeneous / unreliable

data <- data[, -which(names(data) %in% c("pop_dens", "pop_old", "pop_yng", "quiet", "biodiverse", "slope"))]


st_write(data, dsn="D:/data/processed_data/", layer="final_data", driver="ESRI Shapefile")


