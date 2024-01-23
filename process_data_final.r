library(knitr)
library(sf)
library(scales)
library(ggplot2)
library(plyr)

setwd("c:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data")

# IMPORT
# WITHOUT importing population density related variables, data on real estate prices, 
# slope, quietness and biodiversity values

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten", layer="final_data")


build_count <- st_read(dsn=".", layer="buildingcover")
build_perc <- st_read(dsn=".", layer="buildingperc")
single_fam <- st_read(dsn=".", layer="singlefam")
prot_areas <- st_read(dsn=".", layer="protectedareas")
open_veg <- st_read(dsn=".", layer="openveg")
closed_veg <- st_read(dsn=".", layer="closedveg")
rec_green <- st_read(dsn=".", layer="recgreen")
unsealed <- st_read(dsn=".", layer="unsealedsurface")
roads <- st_read(dsn=".", layer="streetlen")
bus <- st_read(dsn=".", layer="pubtrans")
culture <- st_read(dsn=".", layer="culture")
supply <- st_read(dsn=".", layer="supply")
public <- st_read(dsn=".", layer="public")
ph <- st_read(dsn=".", layer="ph")
carbon <- st_read(dsn=".", layer="carbon")
rightwing <- st_read(dsn=".", layer="rightwing")
leftwing <- st_read(dsn=".", layer="leftwing")
participation <- st_read(dsn=".", layer="participation")
elevation <- st_read(dsn=".", layer="elevation")
river_dist <- st_read(dsn=".", layer="distanceoder")
ndvi_mean <- st_read(dsn=".", layer="ndvi")
ndvi_sd <- st_read(dsn=".", layer="ndvi_sd")
ndvi_max <- st_read(dsn=".", layer="ndvi_max")
shapes <- st_read(dsn=".", layer="buildingcover")

ID <- seq(from=1, to=33783)


#### DATA PRE-PROCESSING ####

# ph: replace value 0 with N/A
ph$grid_code[which(ph$grid_code == 0)] <- NA

# carbon: replace value 0 with N/A
carbon$grid_code[which(carbon$grid_code == 0)] <- NA


#### Combine data into same dataframe 

vars <- list(build_count, build_perc, single_fam, prot_areas, open_veg, closed_veg, rec_green, unsealed, roads, bus, culture, supply, public, ph, carbon, rightwing, leftwing, participation, elevation, ndvi_mean, ndvi_sd, ndvi_max)
vars_names <- list('buildings', 'build_per', 'single_fam', 'protected', 'open_veg', 'treecover', 'rec_green', 'unsealed', 'roads', 'bus_dist', 'culture', 'supply', 'public', 'ph', 'carbon', 'right', 'left', 'particip', 'elevation', 'ndvi_mean', 'ndvi_sd', 'ndvi_max')

# Reduce columns to two: ID & grid_code
for (i in seq(from=1, to=22)){
  vars[[i]] <- as.data.frame(vars[[i]])
  vars[[i]]$ID <- ID
  vars[[i]] <- vars[[i]][c('grid_code', 'ID')]
}

#Rename columns
for (k in seq(from=1, to=22)){
  names(vars[[k]]) <- c(vars_names[[k]], 'ID')
}

### merge dataframes 

variables_df <- as.data.frame(ID)

# convert all data to dataframes
for (j in seq(from=1, to=22)){
  vars[[j]] <- as.data.frame(vars[[j]])
}

# merge data
for (var in vars){
  variables_df <- merge(x = variables_df, y = var, by="ID")
}


#### MORE DATA PROCESSING ####

# single family housing: replace values in area with 0 houses with NA
variables_df$single_fam[which(variables_df$buildings == 0 & variables_df$build_per == 0)] <- NA

# rescale data

for (i in 2:22){
  variables_df[,i] <- rescale(variables_df[,i], to=c(0,1))
}



st_write(variables_df, dsn="./processed_data", layer="finalextent_scaled", driver="ESRI Shapefile")

data <- 
  

st_write(data_log, dsn="D:/data/processed_data/", layer="finalextent_log", driver="ESRI Shapefile")


#### PLOT DATA ####

par(mfrow = c(5, 6))
for (var in variables_df){
  hist(var)
}

#### EXPORT DATA AS SHAPEFILE ####

geometry_df <- as.data.frame(ID)
geometry_df$geometry <- leftwing$geometry
combined <- cbind(geometry_df, variables_df, by="ID")
combined <- combined[1:25]

combined <- st_as_sf(combined)
plot(combined)

st_write(combined, dsn="./processed_data/", layer="fullextent_scaled", driver="ESRI Shapefile")

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

shapes$ID <- ID
ID_shp <- shapes[6]

# READ DATA TO REPLACE OSM POI VALUES

data <- st_read("D:/data/processed_data/finalextent_log.shp")
culture <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/culture_distance_clip.shp")
supply <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/supply_distance_clip.shp")
public <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/public_distance_clip.shp")

# replace old values

data$culture <- culture$grid_code
data$supply <- supply$grid_code
data$public <- public$grid_code


for (i in 16:18){
  data[,i] <- rescale(data[,i], to=c(0,1))
}

# remove data that is too homogeneous / unreliable

data <- data[, -which(names(data) %in% c("pop_dens", "pop_old", "pop_yng", "quiet", "biodiverse", "slope"))]


st_write(data, dsn="D:/data/processed_data/", layer="final_data", driver="ESRI Shapefile")


####### FINAL

data <- st_read("./final_data_scaled_final.shp")
culture <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/culture_distance_clip.shp")
supply <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/supply_distance_clip.shp")
public <- st_read("C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/public_distance_clip.shp")

data$culture <- culture$grid_code
data$supply <- supply$grid_code
data$public <- public$grid_code

data <- as.data.frame(data)

# scale new data to 1, 0 > so high values (high distance) equals low value

for (i in 13:15){
  data[,i] <- as.integer(data[,i])
  data[,i] <- rescale(data[,i], to=c(1,0))
}

st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data/", layer="final_data_scaled_POIdistance2", driver="ESRI Shapefile", append=FALSE)


culture['grid_code'] <- rescale(culture['grid_code'], to=c(1,0))

culture <- as.data.frame(culture)
culture$grid_code <- as.double(culture$grid_code)
culture_rescaled <- rescale(culture$grid_code, to=c(1,0))

# replace supply value

supply <- st_read(dsn=".", layer="supply_clip_number")
supply <- as.data.frame(supply)
supply$grid_code <- rescale(supply$grid_code)
#vec <- supply$grid_code
#supply$grid_code <- (vec - min(vec)) / (max(vec) - min(vec))
data <- st_read("C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data/final_data_scaled_final.shp")
data <- as.data.frame(data)
combined <- merge(data, supply, by=c("match1", "match2"))
combined <- combined[2:33]
#data$supply <- supply$grid_code
st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data/", layer="final_data_scaled_final", driver="ESRI Shapefile", append=FALSE)

data <- st_read("C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten//final_data_final.shp")
data$culture <- rescale(data$grid_code, to=c(1,0))
data$public <- rescale(data$grid_code_, to=c(1,0))
data$supply <- rescale(data$grid_cod_1, to=c(1,0))

st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data/", layer="final_data_scaled_final", driver="ESRI Shapefile", append=FALSE)

#### POI numbers

data <- st_read("C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data/final_data_POInumbers.shp")
data$culture <- rescale(data$grid_code, to=c(1,0))
data$public <- rescale(data$grid_code_, to=c(1,0))
data$supply <- rescale(data$grid_cod_1, to=c(1,0))

st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data/", layer="final_data_POInumbers.shp", driver="ESRI Shapefile", append=FALSE)


#### FINAL FINAL

data <- st_read("./final_data_scaled_final.shp")
data <- as.data.frame(data)

st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data_poiDistance_noNA.shp", driver="ESRI Shapefile", append=FALSE)

# replace single family housing values

data$single_fam[which(data$buildings == 0 & data$build_per == 0)] <- NA

st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data_poiDistance_NA.shp", driver="ESRI Shapefile", append=FALSE)

# poi point values

data <- st_read("C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten/data/data/processed_data/final_data_POInumbers.shp")
data$culture <- rescale(data$grid_code, to=c(0,1))
data$public <- rescale(data$grid_code_, to=c(0,1))
data$supply <- rescale(data$grid_cod_1, to=c(0,1))
st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data_poiNumber_noNA.shp", driver="ESRI Shapefile", append=FALSE)

data$single_fam[which(data$buildings == 0 & data$build_per == 0)] <- NA

st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data_poiNumber_NA.shp", driver="ESRI Shapefile", append=FALSE)

# remove elevation, change pH/carbon values
data <- st_read("C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/shapefile_data_poiNumber_NA.shp")

data <- data[, -which(names(data) == "elevation")] # remove elevation variable
data$ph[data$ph == 0] <- NA # correctly code NAs in ph variable
data$ph <- rescale(data$ph, to=c(0,1)) # rescale ph

st_write(data, dsn="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="data_poiNumber_NA_noelevation_phFixed.shp", driver="ESRI Shapefile", append=FALSE)
