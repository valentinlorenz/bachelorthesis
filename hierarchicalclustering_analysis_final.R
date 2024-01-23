######## FUNCTIONS ########

#### Load Data and Packages ####

install_packages = function(){
  install.packages("HH", dep=TRUE)
  install.packages("devtools", dep=TRUE)
  install.packages("corrplot")
  install.packages("vegan", dep=TRUE)
  install.packages("ade4", dep=TRUE)
  install.packages("gclus", dep=TRUE)
  install.packages("labdsv", dep=TRUE)
  install.packages("dismo", dep=TRUE)
  install.packages("plotmo", dep=TRUE)
  install.packages("randomForest", dep=TRUE)
  install.packages("party", dep=TRUE)
  install.packages("tree", dep=TRUE)
  install.packages("sf", dep=TRUE)
  install.packages("NbClust")
  install.packages("fmsb")
  install.packages("RColorBrewer")
}
load_packages = function(){
  library(HH)
  library(devtools)
  library(corrplot)
  library(vegan)
  library(ade4)
  library(gclus)
  library(cluster)
  library(RColorBrewer)
  library(labdsv)
  library(stats)
  library(dismo)
  library(plotmo)
  library(randomForest)
  library(party)
  library(tree)
  library(sf)
  library(NbClust)
  library(dplyr)
  library(fmsb)
  library(scales)
  
}

#### Correlation ####
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

plot_corr = function(variables_cor){
  
  variables.correlacion <- cor(variables_cor[,2:length(variables_cor)], method= "pearson", use= "complete.obs")
  str(variables.correlacion)
  
  # Correlatiom matrix
  p.mat <- cor.mtest(variables_cor[,2:length(variables_cor)])$p #to remove non-significant elements from matrix
  corrplot(variables.correlacion, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat = NULL, sig.level = 0.01, insig = "blank", main= "Correlation Matrix")
  # corrplot(variables.correlacion, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat = p.mat, sig.level = 0.01, insig = "blank") #Eliminate non-significant correlations
  
  return(variables.correlacion)
}

#### Hierarchical Clustering #####

#OPTION 1: CALCULATE DISTANCE MATRIX WITH THE "DIST" FUNCTION (with this we get the tree a little better, and the later cuts make more sense).
#OPTION 2: CALCULATE DISTANCES WITH "VEGDIST" -> vegdist(varTFSE.norm, binary=FALSE, na.rm=TRUE, method="euc") #I measure dissimilarity.

#https://www.statmethods.net/advstats/cluster.html

hierarchical_clusters = function(variables_hrc, method_dist, method_clust, cutoff){
  #Distance Matrix
  distances<-dist(variables_hrc[,2:length(variables_hrc)], method=method_dist)
  
  #Cluster based on distances (Minor distance = Major Correlation)
  cluster<-hclust(distances, method=method_clust) # An�lisis jer�rquico cluster a partir de matriz de disimilitud
  
  #Drawing the Cluster
  plot(cluster, xlab="", ylab="Height", hang=-1, lwd=2, main="") # title(clases_TFSE_scale, dist, hclus) #labels=variablesTFSE$Name
  
  #Make groups from cluster (try out different n of groups)
  #rect.hclust(cluster, 8, border= c("darkred", "darkgreen")) 
  #rect.hclust(cluster, 10, border= c("red","mistyrose2","darksalmon","darkred","goldenrod4","olivedrab","darkkhaki","darkgreen","darkolivegreen1","palegreen"))
  #rect.hclust(cluster, 15, border= c("palegreen","lemonchiffon1","khaki4","yellow4","green2","deepskyblue2","peru","pink","gold","deepskyblue4","tan4","sandybrown","darkgreen","darkorange","forestgreen"))
  #rect.hclust(cluster, 15, border="grey")
  rect.hclust(cluster, cutoff, border=c("darkred", "darkgreen"))
  
  #Cut the cluster at the selected level
  cut <- cutree(cluster, cutoff)
  cut1 <- cut
  
  grid<-rownames(variables_hrc)
  rownames(as.data.frame(cut))
  grid_bind <- cbind(grid, cut, cut1)
  
  return(list(cut, grid_bind, cluster, cut1))
  
  #dim(grid_bind)
  
  #write.table(grid_bind, "./hrc_table.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")
}
random_forest = function(variables_rf, cut){
  variables_cut <- data.frame(variables_rf, as.factor(cut)) #Nuevo elemento. Recordar que lo de as.factor es para que entienda a cut10 como categor�as en lugar de un valor num�rico m�s.
  #variables_cut$single_fam[which(is.na(variables_cut$single_fam))] <- 0 # remove single fam NA values for random forest to avoid NA error

  #Regression Model
  #formula.regresion<-as.formula(paste("presencia ~ ", paste(names(resultado.vif), collapse="+"), collapse=""))
  formula.regresion<-as.formula(paste("as.factor(cut) ~ ", paste(names(variables_rf[,2:length(variables_rf)]), collapse="+"), collapse=""))
  
  #RANDOM FORESTS
  
  #AJUSTE DEL MODELO randomForest CON LOS DISTINTOS TFSE
  
  rf<-randomForest(formula.regresion, data=variables_cut, nodesize=1, importance=TRUE, proximity= TRUE, ntree=1000, na.action=na.exclude)#Da error 
  
  #NOTA: Los par�metros nodesize y maxnodes
  
  #PLOTS DEL ERROR SEG�N N�MERO DE �RBOLES
  plot(rf)
  
  #DEVIANZA EXPLICADA
  print(rf)
  conf.matrix<-print(rf)
  
  #CURVAS DE RESPUESTA
  #con plotmo
  #plotmo(rf)
  
  #con partialPlot (variable a variable)
  # partialPlot(rf, x.var=SEC_10, pred.data=variables_cut)
  
  #IMPORTANCIA DE LAS VARIABLES
  importance(rf)
  rf_imp_var<-round(importance(rf), 2) #Mucho m�s factible para su interpretaci�n.
  
  #Gr�fico
  varImpPlot(rf, main="")
  
  return(rf)
}
remove_variable = function(variables_all, var_name, dist_meth, clust_meth, clust_n){
  
  variables_rmv <- variables_all[, -which(names(variables_all) %in% var_name)]
  cor <- plot_corr(variables_rmv)
  hrca_list_rmv <- hierarchical_clusters(variables_rmv, dist_meth, clust_meth, clust_n)
  rf_rmv <- random_forest(variables_rmv, hrca_list_rmv[[1]])
  
  return(list(variables_rmv, cor, hrca_list_rmv[[4]]))
  
}

#### Calculate Standard Deviation from Mean ####
means_table = function(var_data, clusters, n){
  # combine data with cluster numbers
  var_data$cluster <- clusters
  
  # create new dataframe with ncol(var_data) columns and as many rows as there are clusters
  means <- data.frame(matrix(ncol=length(var_data), nrow=n))
  colnames(means) <- colnames(var_data)
  
  # remove ID column
  means <- means[2:length(means)]
  
  # go through every cluster
  for (i in 1:n){
    to_calculate_mean <- var_data[var_data$cluster == i,] # subselect all values of a specific cluster
    to_calculate_mean <- to_calculate_mean[2:length(to_calculate_mean)] # remove ID column
    
    # calculate mean for every variable
    for (k in 1:length(to_calculate_mean)){
      means[i,k] <- mean(to_calculate_mean[[k]], na.rm = TRUE)
    }
  }
  
  # add row that has values for all clusters
  
  means[nrow(means)+1,] <- c(colMeans(means[1:length(means)-1]), 0)
  
  return(means)
}

sd_from_mean <- function(var_data, clusters, n){
  
  means <- means_table(var_data, clusters, n)
  # create new, empty table to write results into
  sd_table <- data.frame(matrix(nrow=nrow(means)-1, ncol=ncol(means)))
  colnames(sd_table) <- colnames(means)
  
  # for every cluster
  for (i in 1:(nrow(sd_table))){
    # for every variable
    for (k in 1:(ncol(sd_table))){
      # calculate deviation of cluster mean from mean of all clusters
      deviation <- means[i, k] - means[nrow(means), k]
      
      # calculate standard deviation of variable
      sd <- sd(means[1:nrow(sd_table),k], na.rm = TRUE)
      
      # how many standard deviations from the mean?
      
      sd_dev <- deviation/sd
      
      # if NA put in nothing
      
      if (is.na(sd_dev)){
        sd_table[i, k] <- ""
      }
      
      # filling with ++++ (> 2 sd), +++ (> 1.5 SD), ++ (> 1 SD), + (> 0.5 SD)
      
      else if (sd_dev >= 2){
        sd_table[i, k] <- "++++"
      }
      else if (sd_dev >= 1.5){
        sd_table[i, k] <- "+++"
      }
      else if (sd_dev >= 1){
        sd_table[i, k] <- "++"
      }
      else if (sd_dev >= 0.5){
        sd_table[i, k] <- "+"
      }
      
      # filling with ---- (> 2 sd), --- (> 1.5 SD), -- (> 1 SD), - (> 0.5 SD)
      
      else if (sd_dev <= -2){
        sd_table[i, k] <- "----"
      }
      else if (sd_dev <= -1.5){
        sd_table[i, k] <- "---"
      }
      else if (sd_dev <= -1){
        sd_table[i, k] <- "--"
      }
      else if (sd_dev <= -0.5){
        sd_table[i, k] <- "-"
      }
      
      # otherwise leave field empty
      
      else{
        sd_table[i, k] <- ""
      }
      
      
    }
  }
  
  sd_table$cluster <- 1:nrow(sd_table)
  
  return(sd_table)
  
}



#### Export Data ####
export_shp = function(hra_results, filename, shapefile, filepath){
  
  df <- as.data.frame(hra_results)
  df$ID_1 <- shapefile$ID_1
  
  shp <- cbind(shapefile, df, by="ID")
  shp <- shp[ , -which(names(shp) %in% c("by","ID_1.1"))]
  
  st_write(obj=shp, dsn=filepath, layer=filename, driver="ESRI Shapefile", append=FALSE)
  
  return(shp)
}






######## ANALYSIS 1: NA values, POI Distance values #########

#install_packages()
load_packages()

# Load Data

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data")
data <- as.data.frame(data_shp)
data <- data[1:24]
data <- data[-c(2)]

# clustering + mapping #

distance_method = "manhattan"
cluster_method = "ward.D"
cluster_n = 8

clusters8 <- hierarchical_clusters(data, distance_method, cluster_method, cluster_n)
rf8 <- random_forest(data, clusters8[[1]])
cor <- plot_corr(data)
shp8 <- export_shp(clusters8[[4]], "clusters8_wholearea_notremoved", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)

# remove highly correlated + irrelevant variables #

# correlation > |0.7| & MDA < 20
hca_rmv <- remove_variable(data, "ndvi_sd", distance_method, cluster_method, cluster_n)

shp8_rmv <- export_shp(hca_rmv[[3]], "clusters8_wholearea_removed", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8_rmv["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)


# recluster cluster 8 (urban center cluster)

data_urban_shp <- shp8_rmv[shp8_rmv$hra_results == 8,]
data_urban_shp <- data_urban_shp[1:24]
data_urban_shp <- data_urban_shp[-c(2)]
data_urban <- as.data.frame(data_urban_shp)

for (i in 2:23){
  data_urban[,i] <- rescale(data_urban[,i], to=c(0,1))
}

data_urban_shp <- st_as_sf(data_urban)

data_urban <- data_urban[1:23]




cluster_n <- 5

clusters5_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf5_urban <- random_forest(data_urban, clusters5_urban[[1]])

cor <- plot_corr(data_urban)
shp5_urban <- export_shp(clusters5_urban[[4]], "clusters5_urbancenter", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp5_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)

# test: 6 clusters

cluster_n <- 6
clusters6_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf6_urban <- random_forest(data_urban, clusters6_urban[[1]])

cor <- plot_corr(data_urban)
shp6_urban <- export_shp(clusters6_urban[[4]], "clusters6_urbancenter", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "lightgrey"), main="City Center Clusters: 6", key.pos = 1)

# BETTER RESULTS - CONTINUE W 6 CLUSTERS

# cor > |0.7|
hca_rmv_urban <- remove_variable(data_urban, "ndvi_sd", distance_method, cluster_method, cluster_n)
hca_rmv_urban <- remove_variable(hca_rmv_urban[[1]], "supply", distance_method, cluster_method, cluster_n)
hca_rmv_urban <- remove_variable(hca_rmv_urban[[1]], "public", distance_method, cluster_method, cluster_n)


# MDA < 10

hca_rmv_urban <- remove_variable(hca_rmv_urban[[1]], "protected", distance_method, cluster_method, cluster_n)
hca_rmv_urban <- remove_variable(hca_rmv_urban[[1]], "rec_green", distance_method, cluster_method, cluster_n)


# export + plot

shp6_urban_rmv <- export_shp(hca_rmv_urban[[3]], "clusters6_urbancenter_rmv", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban_rmv["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "blue"), main="City Center Clusters: 6", key.pos = 1)

### Means

table_sd8 <- sd_from_mean(data, clusters8[[1]], 8)
write.csv(table_sd8, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results/table8_wholearea.csv", row.names=FALSE)

table_sd6_urban <- sd_from_mean(data_urban, clusters6_urban[[1]], 6)
write.csv(table_sd6_urban, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results/table6_urban.csv", row.names=FALSE)






######## ANALYSIS 2: No NA values, POI Distance values #########

#install_packages()
load_packages()

# Load Data

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data_poiDistance_noNA")
data <- as.data.frame(data_shp)
data <- data[1:24]
data <- data[-c(2)]

# clustering + mapping #

distance_method = "manhattan"
cluster_method = "ward.D"
cluster_n = 8

clusters8 <- hierarchical_clusters(data, distance_method, cluster_method, cluster_n)
rf8 <- random_forest(data, clusters8[[1]])
cor <- plot_corr(data)
shp8 <- export_shp(clusters8[[4]], "clusters8_wholearea_notremoved_poiDistance_noNA", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)

# remove highly correlated + irrelevant variables #

# correlation > |0.7|
hca_rmv <- remove_variable(data, "ndvi_sd", distance_method, cluster_method, cluster_n)
hca_rmv <- remove_variable(hca_rmv[[1]], "culture", distance_method, cluster_method, cluster_n)
hca_rmv <- remove_variable(hca_rmv[[1]], "bus_dist", distance_method, cluster_method, cluster_n)
hca_rmv <- remove_variable(hca_rmv[[1]], "public", distance_method, cluster_method, cluster_n)
# MDA < 20
hca_rmv <- remove_variable(hca_rmv[[1]], "buildings", distance_method, cluster_method, cluster_n)


shp8_rmv <- export_shp(hca_rmv[[3]], "clusters8_wholearea_removed_poiDistance_noNA", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8_rmv["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)


# recluster cluster 1 (urban center cluster)

data_urban_shp <- shp8_rmv[shp8_rmv$hra_results == 1,]
data_urban_shp <- data_urban_shp[1:24]
data_urban_shp <- data_urban_shp[-c(2)]
data_urban <- as.data.frame(data_urban_shp)

# rescale data
for (i in 2:23){
  data_urban[,i] <- rescale(data_urban[,i], to=c(0,1))
}

data_urban_shp <- st_as_sf(data_urban)

data_urban <- data_urban[1:23]




cluster_n <- 5

clusters5_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf5_urban <- random_forest(data_urban, clusters5_urban[[1]])

cor <- plot_corr(data_urban)
shp5_urban <- export_shp(clusters5_urban[[4]], "clusters5_urbancenter_poiDistance_noNA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp5_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)

# test: 6 clusters

cluster_n <- 6
clusters6_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf6_urban <- random_forest(data_urban, clusters6_urban[[1]])

cor <- plot_corr(data_urban)
shp6_urban <- export_shp(clusters6_urban[[4]], "clusters6_urbancenter_poiDistance_noNA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "lightgrey"), main="City Center Clusters: 6", key.pos = 1)

# BETTER RESULTS - CONTINUE W 6 CLUSTERS

# cor > |0.7|
hca_rmv_urban <- remove_variable(data_urban, "ndvi_max", distance_method, cluster_method, cluster_n)
hca_rmv_urban <- remove_variable(hca_rmv_urban[[1]], "bus_dist", distance_method, cluster_method, cluster_n)
hca_rmv_urban <- remove_variable(hca_rmv_urban[[1]], "culture", distance_method, cluster_method, cluster_n)


# MDA < 10
# nothing to remove

# export + plot

shp6_urban_rmv <- export_shp(hca_rmv_urban[[3]], "clusters6_urbancenter_rmv_poiDistance_noNA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban_rmv["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "blue"), main="City Center Clusters: 6", key.pos = 1)







######## ANALYSIS 3: NA values, POI Point values #########

#install_packages()
load_packages()

# Load Data

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data_poiNumber_NA")
data <- as.data.frame(data_shp)
data <- data[1:24]
data <- data[-c(2)]

# clustering + mapping #

distance_method = "manhattan"
cluster_method = "ward.D"
cluster_n = 8

clusters8 <- hierarchical_clusters(data, distance_method, cluster_method, cluster_n)
rf8 <- random_forest(data, clusters8[[1]])
cor <- plot_corr(data)
shp8 <- export_shp(clusters8[[4]], "clusters8_wholearea_notremoved_poiNumbers_NA", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)

# remove highly correlated + irrelevant variables #

# correlation > |0.7|
hca_rmv <- remove_variable(data, "ndvi_sd", distance_method, cluster_method, cluster_n)
# MDA < 20
hca_rmv <- remove_variable(hca_rmv[[1]], "public", distance_method, cluster_method, cluster_n)
hca_rmv <- remove_variable(hca_rmv[[1]], "supply", distance_method, cluster_method, cluster_n)


shp8_rmv <- export_shp(hca_rmv[[3]], "clusters8_wholearea_removed_poiNumbers_NA", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8_rmv["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)


# recluster cluster 7 (urban center cluster)

data_urban_shp <- shp8_rmv[shp8_rmv$hra_results == 7,]
data_urban_shp <- data_urban_shp[1:24]
data_urban_shp <- data_urban_shp[-c(2)]
data_urban <- as.data.frame(data_urban_shp)

# rescale data
for (i in 2:23){
  data_urban[,i] <- rescale(data_urban[,i], to=c(0,1))
}

data_urban_shp <- st_as_sf(data_urban)

data_urban <- data_urban[1:23]




cluster_n <- 5

clusters5_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf5_urban <- random_forest(data_urban, clusters5_urban[[1]])

cor <- plot_corr(data_urban)
shp5_urban <- export_shp(clusters5_urban[[4]], "clusters5_urbancenter_poiNumbers_NA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp5_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)

# test: 6 clusters

cluster_n <- 6
clusters6_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf6_urban <- random_forest(data_urban, clusters6_urban[[1]])

cor <- plot_corr(data_urban)
shp6_urban <- export_shp(clusters6_urban[[4]], "clusters6_urbancenter_poiNumbers_NA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "lightgrey"), main="City Center Clusters: 6", key.pos = 1)

# BETTER RESULTS - CONTINUE W 6 CLUSTERS

# cor > |0.7|
hca_rmv_urban <- remove_variable(data_urban, "ndvi_sd", distance_method, cluster_method, cluster_n)

# MDA < 10
# nothing to remove

# export + plot

shp6_urban_rmv <- export_shp(hca_rmv_urban[[3]], "clusters6_urbancenter_rmv_poiNumbers_NA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban_rmv["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "blue"), main="City Center Clusters: 6", key.pos = 1)


######## ANALYSIS 4: No NA values, POI Point values #########

#install_packages()
load_packages()

# Load Data

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="shapefile_data_poiNumber_noNA")
data <- as.data.frame(data_shp)
data <- data[1:24]
data <- data[-c(2)]

# clustering + mapping #

distance_method = "manhattan"
cluster_method = "ward.D"
cluster_n = 8

clusters8 <- hierarchical_clusters(data, distance_method, cluster_method, cluster_n)
rf8 <- random_forest(data, clusters8[[1]])
cor <- plot_corr(data)
shp8 <- export_shp(clusters8[[4]], "clusters8_wholearea_notremoved_poiNumbers_noNA", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)

# remove highly correlated + irrelevant variables #

# correlation > |0.7|
hca_rmv <- remove_variable(data, "ndvi_sd", distance_method, cluster_method, cluster_n)
# MDA < 20
hca_rmv <- remove_variable(hca_rmv[[1]], "public", distance_method, cluster_method, cluster_n)
hca_rmv <- remove_variable(hca_rmv[[1]], "supply", distance_method, cluster_method, cluster_n)


shp8_rmv <- export_shp(hca_rmv[[3]], "clusters8_wholearea_removed_poiNumbers_noNA", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8_rmv["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)


# recluster cluster 7 (urban center cluster)

data_urban_shp <- shp8_rmv[shp8_rmv$hra_results == 7,]
data_urban_shp <- data_urban_shp[1:24]
data_urban_shp <- data_urban_shp[-c(2)]
data_urban <- as.data.frame(data_urban_shp)

# rescale data
for (i in 2:23){
  data_urban[,i] <- rescale(data_urban[,i], to=c(0,1))
}

data_urban_shp <- st_as_sf(data_urban)

data_urban <- data_urban[1:23]




cluster_n <- 5

clusters5_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf5_urban <- random_forest(data_urban, clusters5_urban[[1]])

cor <- plot_corr(data_urban)
shp5_urban <- export_shp(clusters5_urban[[4]], "clusters5_urbancenter_poiNumbers_noNA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp5_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)

# test: 6 clusters

cluster_n <- 6
clusters6_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf6_urban <- random_forest(data_urban, clusters6_urban[[1]])

cor <- plot_corr(data_urban)
shp6_urban <- export_shp(clusters6_urban[[4]], "clusters6_urbancenter_poiNumbers_noNA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "lightgrey"), main="City Center Clusters: 6", key.pos = 1)

# CONTINUE W 5 CLUSTERS

cluster_n <- 5

# cor > |0.7|
hca_rmv_urban <- remove_variable(data_urban, "ndvi_sd", distance_method, cluster_method, cluster_n)

# MDA < 10
# nothing to remove

# export + plot

shp5_urban_rmv <- export_shp(hca_rmv_urban[[3]], "clusters5_urbancenter_rmv_poiNumbers_noNA", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp5_urban_rmv["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)





######## ANALYSIS 5: NA values, POI POint values, no elevation, pH fixed #####

#install_packages()
load_packages()

# Load Data

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis", layer="data_poiNumber_NA_noelevation_phFixed")
data <- as.data.frame(data_shp)
data <- data[1:23]
data <- data[-c(2)]

# clustering + mapping #

distance_method = "manhattan"
cluster_method = "ward.D"
cluster_n = 8

clusters8 <- hierarchical_clusters(data, distance_method, cluster_method, cluster_n)
rf8 <- random_forest(data, clusters8[[1]])
cor <- plot_corr(data)
shp8 <- export_shp(clusters8[[4]], "clusters8_noelevation", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)

# remove highly correlated + irrelevant variables #

# correlation > |0.7|
hca_rmv <- remove_variable(data, "ndvi_sd", distance_method, cluster_method, cluster_n)
# MDA < 20
hca_rmv <- remove_variable(hca_rmv[[1]], "supply", distance_method, cluster_method, cluster_n)
hca_rmv <- remove_variable(hca_rmv[[1]], "public", distance_method, cluster_method, cluster_n)
hca_rmv <- remove_variable(hca_rmv[[1]], "culture", distance_method, cluster_method, cluster_n)


shp8_rmv <- export_shp(hca_rmv[[3]], "clusters8_noelevation_removed", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp8_rmv["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)


# recluster cluster 7 (urban center cluster)

data_urban_shp <- shp8_rmv[shp8_rmv$hra_results == 1,]
data_urban_shp <- data_urban_shp[1:23]
data_urban_shp <- data_urban_shp[-c(2)]
data_urban <- as.data.frame(data_urban_shp)

# rescale data
for (i in 2:22){
  data_urban[,i] <- rescale(data_urban[,i], to=c(0,1))
}

data_urban_shp <- st_as_sf(data_urban)

data_urban <- data_urban[1:22]




cluster_n <- 5

clusters5_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf5_urban <- random_forest(data_urban, clusters5_urban[[1]])

cor <- plot_corr(data_urban)
shp5_urban <- export_shp(clusters5_urban[[4]], "clusters5_noelevation", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp5_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)

# test: 6 clusters

cluster_n <- 6
clusters6_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, cluster_n)
rf6_urban <- random_forest(data_urban, clusters6_urban[[1]])

cor <- plot_corr(data_urban)
shp6_urban <- export_shp(clusters6_urban[[4]], "clusters6_noelevation", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "blue"), main="City Center Clusters: 6", key.pos = 1)

# CONTINUE W 6 CLUSTERS

cluster_n <- 6

# cor > |0.7|
hca_rmv_urban <- remove_variable(data_urban, "ndvi_sd", distance_method, cluster_method, cluster_n)
hca_rmv_urban <- remove_variable(data_urban, "open_veg", distance_method, cluster_method, cluster_n)

# MDA < 10
hca_rmv_urban <- remove_variable(data_urban, "protected", distance_method, cluster_method, cluster_n)


# export + plot

shp6_urban_rmv <- export_shp(hca_rmv_urban[[3]], "clusters6_noelevation_removed", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Final_Analysis/results")
plot(shp6_urban_rmv["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "blue"), main="City Center Clusters: 5", key.pos = 1)



