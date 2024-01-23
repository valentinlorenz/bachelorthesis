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
  library(sf)
  
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
means_clusters = function(var_df, results){
  
  
  # add ID column to results for merging
  df <- as.data.frame(results)
  df$ID <- var_df$ID
  
  # bind results to original data
  var_bind <- cbind(var_df, df, by="ID")
  
  # remove final two columns (contain information on merging process; not relevant for further analysis)
  x <- length(var_bind) - 2
  var_bind <- var_bind %>% dplyr::select(1:x)
  
  # create list with all variable names
  names <- colnames(var_df)
  names <- names[c(2:length(names))]
  
  # create new dataframe to mutate
  means <- var_bind
  
  # group data by cluster (column: results)
  # calculate mean for every group and every variable
  # replaces original values
  for (col in names){
    means <- means %>% 
      group_by(results) %>%
      mutate(!!col := mean(!!rlang::sym(col), na.rm = TRUE))
  }
  
  
  # create new dataframe with means for every cluster
  #(adds first entry for every cluster to new dataframe)
  slices <- means %>%
    slice(1)
  
  # replace first column (ID_1) with cluster column
  slices$ID <- slices$results
  slices <- slices[1:length(slices)-1]
  names(slices)[names(slices) == 'ID'] <- 'cluster'
  
  return(slices)
  
}
means_columns = function(var_df){
  
  # create list with names of all variables
  names <- colnames(var_df)
  names <- names[c(2:length(names))]
  
  # create empty dataframe with one row, 30 columns
  means = data.frame(matrix(nrow = 1, ncol = length(names))) 
  names(means) <- names
  
  # convert var_df from tibble to dataframe
  var_df <- as.data.frame(var_df)
  
  for (i in 1:length(names)){
    k = i + 1
    means[, i] <- mean(var_df[, i + 1], na.rm = TRUE)
  }
  
  return(means)
}
means_table = function(vars_df, cluster_results, cluster_n){
  
  # calculate mean values for every cluster
  clustmeans <- means_clusters(vars_df, cluster_results)
  # calculate mean value of clusters
  colmeans <- means_columns(clustmeans)
  colmeans$cluster = 0
  
  # combine means
  means_bind <- rbind(colmeans, clustmeans)
  means_difference <- means_bind
  
  # calculate difference from mean for every cluster
  for (i in 2:cluster_n + 1){
    means_difference[i,] <- means_difference[i,] - means_difference[1,]
  }
  
  return(means_difference)
}

table_sd <- function(means){
  # create new empty dataframe with cluster numbers
  results <- data.frame(matrix(ncol = ncol(means), nrow=nrow(means)))
  colnames(results) <- colnames(means)
  results$cluster <- means$cluster
  
  
  # for every variable 
  for (j in 1:(ncol(means)-1)){
    # calculate standard deviation
    sd <- sd(means[2:nrow(means),][[j]], na.rm = TRUE)
    
    # check how many standard deviations values are removed from the mean
    for (k in 1:nrow(means)){
      # - for negative deviation
      if (is.na(means[k,j])) {
        results[k,j] <- "N/A"
      }
      else if (means[k,j] < 0) {
        if(means[k,j] <= sd*-2){
          results[k,j] <- "----"
        }
        else if(means[k,j] <= sd*-1.5){
          results[k,j] <- "---"
        }
        else if(means[k,j] <= sd*-1){
          results[k,j] <- "--"
        }
        else if(means[k,j] <= sd*-0.5){
          results[k,j] <- "-"
        }
        else {
          results[k,j] <- " "
        }
      }
      # + for positive deviation
      else if (means[k,j] > 0){
        if(means[k,j] >= sd*2){
          results[k,j] <- "++++"
        }
        else if(means[k,j] >= sd*1.5){
          results[k,j] <- "+++"
        }
        else if(means[k,j] >= sd*1){
          results[k,j] <- "++"
        }
        else if(means[k,j] >= sd*0.5){
          results[k,j] <- "+"
        }
        
        else {
          results[k,j] <- " "
        }
      }
      else {
        results[k,j] <- " "
      }
    }
    
    
  }
  
  return(results)
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





######## ANALYSIS #########

#### import data ####

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten", layer="final_data")
data <- as.data.frame(data_shp)
data <- data[1:25]

#### clustering + mapping ####

distance_method = "manhattan"
cluster_method = "ward.D"

clusters8 <- hierarchical_clusters(data, distance_method, cluster_method, 8)
rf8 <- random_forest(data, clusters8[[1]])
means8 <- means_table(data, clusters8[[1]], 8)
table_sd8 <- table_sd(means8)
write.csv(table_sd8, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results/table8_wholearea.csv", row.names=FALSE)

shp8 <- export_shp(clusters8[[4]], "clusters8_wholearea", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp8["hra_results"], pal = c("grey40", "cornsilk1", "mediumpurple", "indianred","plum1", "lightskyblue2","darkorange", "palegreen3"), main="8 Clusters", key.pos = 1)


## recluster cluster 7 + 8 (urban center clusters) ##

data_urban_shp <- shp8[shp8$hra_results == 7 | shp8$hra_results == 8,]
data_urban_shp <- data_urban_shp[, -c(26)] # remove cluster results column
data_urban <- as.data.frame(data_urban_shp)
data_urban <- data_urban[1:(length(data_urban)-2)]

clusters5_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, 5)
rf5_urban <- random_forest(data_urban, clusters5_urban[[1]])
means5_urban <- means_table(data_urban, clusters5_urban[[1]], 5)
table_sd5_urban <- table_sd(means5_urban)
write.csv(table_sd5_urban, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results/table5_urbancenter.csv", row.names=FALSE)
shp5_urban <- export_shp(clusters5_urban[[4]], "clusters5_urbancenter", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp5_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)

# 7 clusters
clusters7_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, 7)
shp7_urban <- export_shp(clusters7_urban[[4]], "clusters7_urbancenter", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp7_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "lightblue", "orange3"), main="City Centre Clusters", key.pos = 1)

# 4 clusters (2nd best number of clusters according to sensitivity analysis)
clusters4_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, 4)
shp4_urban <- export_shp(clusters4_urban[[4]], "clusters4_urbancenter", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp4_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred"), main="City Center Clusters: 4", key.pos = 1)

# 6 clusters
clusters6_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, 6)
shp6_urban <- export_shp(clusters6_urban[[4]], "clusters6_urbancenter", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp6_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "lightblue"), main="City Centre Clusters: 6", key.pos = 1)


# 3 clusters

clusters3_urban <- hierarchical_clusters(data_urban, distance_method, cluster_method, 3)
shp3_urban <- export_shp(clusters3_urban[[4]], "clusters3_urbancenter", data_urban_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp3_urban["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple"), main="City Center Clusters: 3", key.pos = 1)

#### SENSITIVITY ANALYSIS ####

res<-NbClust(data_urban[,2:length(data_urban)], diss=NULL, distance = "manhattan", min.nc=2, max.nc=15, 
             method = "ward.D", index = "all") 

res$All.index

res$Best.nc

res$Best.partition

#### Test: Influence of River Distance Variable ####

data_noriver <- data[-c(22)]
clusters8_noriver <- hierarchical_clusters(data_noriver, distance_method, cluster_method, 8)
shp8_noriver <- export_shp(clusters8_noriver[[4]], "clusters8_noriver", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp8_noriver["hra_results"], pal = c("darkorange", "cornsilk1", "grey40", "mediumpurple","indianred", "lightskyblue2","plum1", "palegreen3"), main="8 Clusters", key.pos = 1)
means8_noriver <- means_table(data_noriver, clusters8_noriver[[1]], 8)
table_sd8_noriver <- table_sd(means8_noriver)
write.csv(table_sd8_noriver, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results/table8_noriver.csv", row.names=FALSE)
cor <- plot_corr(data_noriver)
rf8_noriver <- random_forest(data_noriver, clusters8_noriver[[1]])

# remove variables #

# highest correlation: ndvi_sd & ndvi_max
# remove ndvi_sd: lower MDA

hca_rmv <- remove_variable(data_noriver, "ndvi_sd", distance_method, cluster_method, 8)

# highest correlation: culture & public
# remove culture: lower MDA

hca_rmv <- remove_variable(hca_rmv[[1]], "culture", distance_method, cluster_method, 8)

# highest correlation: supply & public
# remove supply: lower MDA

hca_rmv <- remove_variable(hca_rmv[[1]], "supply", distance_method, cluster_method, 8)

# all variables with correlation > 0.7 removed
# all remaining variables have an MDA > 20

shp8_noriver_removed <- export_shp(hca_rmv[[3]], "clusters8_noriver_removed", data_shp, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp8_noriver_removed["hra_results"], pal = c("darkorange", "cornsilk1", "grey40", "mediumpurple","indianred", "lightskyblue2","plum1", "palegreen3"), main="8 Clusters", key.pos = 1)
means8_noriver_removed <- means_table(hca_rmv[[1]], hca_rmv[[3]], 8)
table_sd8_noriver_removed <- table_sd(means8_noriver_removed)
write.csv(table_sd8_noriver_removed, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results/table8_noriver_removed.csv", row.names=FALSE)
cor <- plot_corr(data_noriver)
rf8_noriver <- random_forest(data_noriver, clusters8_noriver[[1]])

## urban analysis of no river data

data_urban_shp_noriver <- shp8_noriver[shp8_noriver$hra_results == 1 | shp8_noriver$hra_results == 8,]
data_urban_shp_noriver <- data_urban_shp_noriver[, -c(22, 26)] # remove cluster results column & river dist column
data_urban_noriver <- as.data.frame(data_urban_shp_noriver)
data_urban_noriver <- data_urban_noriver[1:24]

clusters5_urban_noriver <- hierarchical_clusters(data_urban_noriver, distance_method, cluster_method, 5)
rf5_urban <- random_forest(data_urban_noriver, clusters5_urban_noriver[[1]])
means5_urban_noriver <- means_table(data_urban_noriver, clusters5_urban_noriver[[1]], 5)
table_sd5_urban_noriver <- table_sd(means5_urban_noriver)
write.csv(table_sd5_urban_noriver, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results/table5_urbancenter_noriver.csv", row.names=FALSE)
shp5_urban_noriver <- export_shp(clusters5_urban_noriver[[4]], "clusters5_urbancenter_noriver", data_urban_shp_noriver, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp5_urban_noriver["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1"), main="City Center Clusters: 5", key.pos = 1)

# test: 6 clusters

clusters6_urban_noriver <- hierarchical_clusters(data_urban_noriver, distance_method, cluster_method, 6)
means6_urban_noriver <- means_table(data_urban_noriver, clusters6_urban_noriver[[1]], 6)
table_sd6_urban_noriver <- table_sd(means6_urban_noriver)
write.csv(table_sd6_urban_noriver, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results/table6_urbancenter_noriver.csv", row.names=FALSE)
shp6_urban_noriver <- export_shp(clusters6_urban_noriver[[4]], "clusters6_urbancenter_noriver", data_urban_shp_noriver, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp6_urban_noriver["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "darkorange3"), main="City Center Clusters: 6", key.pos = 1)

# urban analysis of removed no river data
data_urban_shp_noriver_removed <- shp8_noriver_removed[shp8_noriver_removed$hra_results == 1 | shp8_noriver_removed$hra_results == 7 | shp8_noriver_removed$hra_results == 8,]
data_urban_shp_noriver_removed <- data_urban_shp_noriver_removed[, -c(22, 26)] # remove cluster results column & river dist column
data_urban_noriver_removed <- as.data.frame(data_urban_shp_noriver_removed)
data_urban_noriver_removed <- data_urban_noriver_removed[1:24]

clusters6_urban_noriver_removed <- hierarchical_clusters(data_urban_noriver_removed, distance_method, cluster_method, 6)
rf6_urban_removed <- random_forest(data_urban_noriver_removed, clusters6_urban_noriver_removed[[1]])
cor <- plot_corr(data_urban_noriver_removed)

# remove based on correlation
# remove ndvi_sd (cor with ndvi_max)
hca_rmv <- remove_variable(data_urban_noriver_removed, "ndvi_sd", distance_method, cluster_method, 6)
# remove rec_green (correlated with unsealed surface)
hca_rmv <- remove_variable(hca_rmv[[1]], "rec_green", distance_method, cluster_method, 6)
# remove public (cor with culture)
hca_rmv <- remove_variable(hca_rmv[[1]], "public", distance_method, cluster_method, 6)
# remove supply (cor with culture)
hca_rmv <- remove_variable(hca_rmv[[1]], "supply", distance_method, cluster_method, 6)

# all vars with cor > 0.7 removed
# remove variables with MDA < 20 

hca_rmv <- remove_variable(hca_rmv[[1]], "treecover", distance_method, cluster_method, 6)
hca_rmv <- remove_variable(hca_rmv[[1]], "protected", distance_method, cluster_method, 6)
hca_rmv <- remove_variable(hca_rmv[[1]], "public", distance_method, cluster_method, 6)

shp6_urban_noriver_removed <- export_shp(hca_rmv[[3]], "clusters6_urbancenter_noriver_removed", data_urban_shp_noriver_removed, "C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results")
plot(shp6_urban_noriver_removed["hra_results"], pal = c("aquamarine", "cornsilk1", "mediumpurple", "indianred","plum1", "lightblue"), main="City Center Clusters: 6", key.pos = 1)

means6_urban_noriver_removed <- means_table(hca_rmv[[1]], hca_rmv[[3]], 6)
table_sd6_urban_noriver_removed <- table_sd(means6_urban_noriver_removed)
write.csv(table_sd6_urban_noriver_removed, "C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Results/table6_urbancenter_noriver_removed.csv", row.names=FALSE)


##### FINAL FINAL CLUSTERING

#### import data ####

data_shp <- st_read(dsn="C:/Users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit/Daten", layer="final_data")
data <- as.data.frame(data_shp)
data <- data[1:25]