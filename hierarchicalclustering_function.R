#### Load Data ####

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
    install.packages("rgdal", dep=TRUE)
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
    library(rgdal)
    library(NbClust)
    library(dplyr)
    library(fmsb)
    library(sf)

}
load_data = function(){
    dir.datos="C:/users/quinn/OneDrive/Dokumente/Uni/Bachelorarbeit"
    setwd(dir.datos)

    #Leemos tabla de variables
    variables<-read_sf("./RStudio/urban_extent_final/data_shp.shp") #IMP.: si volvemos a hacer los análisis con "nuevaBD_tfse_R_reord", tenemos que quitar [,2:30] del script (eso lo usabamos para quitar del análisis la columna con el nombre de los municipios).
    variables <- as.data.frame(variables)
    return(variables)
    
    str(variables)
    dim(variables)
    summary(variables)
    colnames(variables)
}

# Corrplot
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

    rf<-randomForest(formula.regresion, data=variables_cut, nodesize=1, importance=TRUE, proximity= TRUE, ntree=1000)#Da error

    #NOTA: Los par�metros nodesize y maxnodes

    #PLOTS DEL ERROR SEG�N N�MERO DE �RBOLES
    plot(rf)

    #DEVIANZA EXPLICADA
    print(rf)
    conf.matrix<-print(rf)

    #CURVAS DE RESPUESTA
    #con plotmo
    plotmo(rf)

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

means_clusters = function(var_df, results){
  
  # add ID column to results for merging
  df <- as.data.frame(results)
  df$ID_1 <- var_df$ID_1
  
  # bind results to original data
  var_bind <- cbind(var_df, df, by="ID_1")
  
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
      mutate(!!col := mean(!!rlang::sym(col)))
  }
  
  # create new dataframe with means for every cluster
  #(adds first entry for every cluster to new dataframe)
  slices <- means %>%
    slice(1)
  
  # replace first column (ID_1) with cluster column
  slices$ID_1 <- slices$results
  slices <- slices[1:length(slices)-1]
  names(slices)[names(slices) == 'ID_1'] <- 'cluster'
  
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
    means[, i] = mean(var_df[, i + 1])
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

export_shp = function(hra_results, filename, shapefile, filepath){
  
  hra_results = hca_final_7[[4]]
  filename = "test_shp"
  shapefile = vars_shp
  filepath = "."
  
  df <- as.data.frame(hra_results)
  df$ID_1 <- shapefile$ID_1
  
  shp <- cbind(shapefile, df, by="ID")
  shp <- shp[ , -which(names(shp) %in% c("by","ID_1.1"))]
  
  st_write(obj=shp, dsn=filepath, layer=filename, driver="ESRI Shapefile", overwrite=TRUE)
  
  return(shp)
}

#### LOAD DATA ####

install_packages()
load_packages()
variables <- load_data()

#selecting variables with VIF < 5
vif(variables)

#### SENSITIVITY ANALYSIS ####

res<-NbClust(variables[,2:length(variables)], diss=NULL, distance = "manhattan", min.nc=2, max.nc=15, 
             method = "ward.D", index = "all") 

res$All.index

res$Best.nc

res$Best.partition

# suggested number of clusters: 5

#### MANHATTEN DISTANCE CLUSTERS ####

cluster_n <- 5
distance_method <- "manhattan"
cluster_method <- "ward.D"
vars_clip <- readOGR("C:/Users/swenj/academiccloudsync/MoveNsense/GIS/SES Archetypes/var_urban.shp")

corr <- plot_corr(variables)
hrca_list_5_clip <- hierarchical_clusters(variables, distance_method, cluster_method, cluster_n)
rf_5_manhattan <- random_forest(variables, hrca_list_10_clip[[1]])


shp <- export_shp(hrca_list_5_clip[[4]], "hca_manhattan_5_urban", vars_clip)
plot(shp)

means_t <- means_table(variables, hrca_list_5_clip[[1]])
write.table(means_t, "./means_differences_urban1.csv", quote=F, row.names=F, col.names=T, dec=",", sep=";")

spiderweb_graph(means_t)

#### REMOVING VARIABLES ####

# remove first variable: ndvi_sd
hrca_rmv <- remove_variable(variables, "ndvi_sd", distance_method, cluster_method, cluster_n)

# remove second variable: elevation (relatively high MDA, but high correlation with river distance)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "elevatn", distance_method, cluster_method, cluster_n)

# remove bld_prc (high correlation with unsealed surface)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "bld_prc", distance_method, cluster_method, cluster_n)

# remove ndvi_max (high correlation with ndvi)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "ndvi_mx", distance_method, cluster_method, cluster_n)


# remove ndvi_mean (high correlation with unsealed surface)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "ndvi_mn", distance_method, cluster_method, cluster_n)

# remove variables with low MDA (< 25)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "quiet", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "public", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "supply", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "culture", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "slope", distance_method, cluster_method, cluster_n)


export_shp(hrca_rmv[[3]], "hca_manhattan_5_removed", vars_clip)

# create table

means_t_rmv <- means_table(hrca_rmv[[1]], hrca_rmv[[3]])
write.table(means_t_rmv, "./means_differences_urban1_rmv.csv", quote=F, row.names=F, col.names=T, dec=",", sep=";")
spiderweb_graph(means_t_rmv)

#### Visualization ####

spiderweb_graph = function(means){

  colnames<-means[length(means)]
  means_plot<-means_t[1:length(means)-1]#delete first column with names
  means_plot <- means_plot[-c(1), ]
  
  data <-as.data.frame(rbind(rep(1,length(means_plot)) , rep(-1,length(means_plot)) , means_plot))#add two rows, with maximum and minimum
  
  # Prepare color
  colors_border=c("red", "green", "orange", "black", "blue")
  
  # Custom the radarChart !
  #windows()
  radarchart( data, axistype=3,
              
              #custom polygon
              pcol=colors_border , pfcol=NA, plwd=2, plty=2 , 
              
              #custom the grid
              cglcol="grey", cglty=2, axislabcol="black", caxislabels=seq(-1,1,0.5), cglwd=1, maxmin = TRUE,
              
              #custom labels
              vlcex=1,
  )
  
  # Legend
  legend(x=1.3, y=1, legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=1.2, xjust=0.2, yjust=0.85,)
}


##### SMALLER EXTENT: Cluster 8 (inner urban area) of initial analysis #####

variables_urban <- load_data()
vars_clip_urban <- readOGR("D:/RStudio/variables_urban_clip.shp")

#selecting variables with VIF < 5
vif(variables_urban)
variables_urban <- subset(variables_urban, select = -c(quiet, ndvi_mx))


#### SENSITIVITY ANALYSIS ####

res<-NbClust(variables_urban[,2:length(variables_urban)], diss=NULL, distance = "manhattan", min.nc=2, max.nc=15, 
             method = "ward.D", index = "all") 

res$All.index

res$Best.nc

res$Best.partition

# best n of clusters: 3 or 5 -> chose 5

#### Clustering ####

corr <- plot_corr(variables_urban)
hrca_5_urban <- hierarchical_clusters(variables_urban, distance_method, cluster_method, cluster_n)
rf_5_urban <- random_forest(variables_urban, hrca_5_urban[[1]])


shp <- export_shp(hrca_5_urban[[4]], "hca_manhattan_urban2", vars_clip_urban)

means_t_2 <- means_table(variables_urban, hrca_5_urban[[1]])
write.table(means_t_2, "./means_differences_urban2.csv", quote=F, row.names=F, col.names=T, dec=",", sep=";")

spiderweb_graph(means_t_2)

#### REMOVING VARIABLES ####

# based on correlation (max |0.5|)
hrca_rmv <- remove_variable(variables_urban, "elevatn", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "ph", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "rec_grn", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "bld_prc", distance_method, cluster_method, cluster_n)

# based on MDA (min 20), keeping biodiversity
hrca_rmv <- remove_variable(hrca_rmv[[1]], "slope", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "prot_rs", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "public", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "supply", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "ndvi_sd", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "culture", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "carbon", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "bus", distance_method, cluster_method, cluster_n)
hrca_rmv <- remove_variable(hrca_rmv[[1]], "open_vg", distance_method, cluster_method, cluster_n)

export_shp(hrca_rmv[[3]], "hca_manhattan_5_removed_2", vars_clip_urban)

# create table

means_t_rmv_2 <- means_table(hrca_rmv[[1]], hrca_rmv[[3]])
write.table(means_t_rmv_2, "./means_differences_urban2_rmv.csv", quote=F, row.names=F, col.names=T, dec=",", sep=";")
spiderweb_graph(means_t_rmv_2)



#### new extent: slubice larger ####

variables_urban_2 <- load_data()
vars_clip_urban_shp <- readOGR("D:/RStudio/data_scaled_3.shp")

#selecting variables with VIF < 5
vif(variables_urban)

#### testing different number of clusters ####

cluster_n <- 5
distance_method = "manhattan"
cluster_method = "ward.D"
hca_5 <- hierarchical_clusters(variables_urban_2, distance_method, cluster_method, cluster_n)
export_shp(hca_5[[4]], "testclustern_5", vars_clip_urban_shp)

cluster_n <- 6
hca_6 <- hierarchical_clusters(variables_urban_2, distance_method, cluster_method, cluster_n)
export_shp(hca_6[[4]], "testclustern_6", vars_clip_urban_shp)

cluster_n <- 7
hca_7 <- hierarchical_clusters(variables_urban_2, distance_method, cluster_method, cluster_n)
export_shp(hca_7[[4]], "testclustern_7", vars_clip_urban_shp)

cluster_n <- 8
hca_8 <- hierarchical_clusters(variables_urban_2, distance_method, cluster_method, cluster_n)
export_shp(hca_8[[4]], "testclustern_8", vars_clip_urban_shp)

cluster_n <- 9
hca_9 <- hierarchical_clusters(variables_urban_2, distance_method, cluster_method, cluster_n)
export_shp(hca_9[[4]], "testclustern_9", vars_clip_urban_shp)

cluster_n <- 10
hca_10 <- hierarchical_clusters(variables_urban_2, distance_method, cluster_method, cluster_n)
export_shp(hca_10[[4]], "testclustern_10", vars_clip_urban_shp)


#### Final Extent ####
variables_urban_4 <- load_data()
variables_urban_4 <- variables_urban_4[1:31]

distance_method <- "manhattan"
cluster_method <- "ward.D"
vars_shp <- read_sf("./RStudio/urban_extent_final/data_shp.shp")

hca_final_7 <- hierarchical_clusters(variables_urban_4, distance_method, cluster_method, 7)
rf <- random_forest(variables_urban_4, hca_final_7[[1]])
shp7 <- export_shp(hca_final_7[[4]], "hca_manhattan_7", vars_shp)

hca_final_8 <- hierarchical_clusters(variables_urban_4, distance_method, cluster_method, 8)
shp8 <- export_shp(hca_final_8[[4]], "hca_manhattan_8", vars_shp)

hca_final_9 <- hierarchical_clusters(variables_urban_4, distance_method, cluster_method, 9)
shp9 <- export_shp(hca_final_9[[4]], "hca_manhattan_9", vars_shp)

means_7 <- means_table(variables_urban_4, hca_final_7[[1]], 7)
means_8 <- means_table(variables_urban_4, hca_final_8[[1]], 8)
means_9 <- means_table(variables_urban_4, hca_final_9[[1]], 9)


spplot(shp7, n = 7, col.regions = brewer.pal(n=7, name="Set1"), cuts = 6, 'hra_results', main="Clusters: 7", col="transparent")
spplot(shp8, n = 8, col.regions = brewer.pal(n=8, name="Set1"), cuts = 7, 'hra_results', main="Clusters: 8", col="transparent")
spplot(shp9, n = 9, col.regions = brewer.pal(n=9, name="Set1"), cuts = 8, 'hra_results', main="Clusters: 9", col="transparent")

table_sd_7 <- table_sd(variables_urban_4, means_7)
table_sd_8 <- table_sd(variables_urban_4, means_8)
table_sd_9 <- table_sd(variables_urban_4, means_9)

table_sd <- function(vars, means){
  results <- data.frame(matrix(ncol = ncol(means), nrow=nrow(means)))
  colnames(results) <- names(means)
  results$cluster <- means$cluster
  
  
  # for every variable 
  for (j in 1:30){
    # calculate standard deviation 
    sd <- sd(vars[[j+1]])
    print(sd)
    
    for (k in 1:8){
      if (means[k,j] < 0) {
        if(means[k,j] <= means[1, j] + sd*-1.5){
          results[k,j] <- "---"
        }
        else if(means[k,j] <= means[1, j] + sd*-1){
          results[k,j] <- "--"
        }
        else if(means[k,j] <= means[1, j] + sd*-0.5){
          results[k,j] <- "-"
        }
        else {
          results[k,j] <- " "
        }
      }
      else if (means[k,j] > 0){
        if(means[k,j] >= means[1, j] + means[1, j] + sd*1.5){
          results[k,j] <- "+++"
        }
        else if(means[k,j] >= means[1, j] + sd*1){
          results[k,j] <- "++"
        }
        else if(means[k,j] >= means[1, j] + sd*0.5){
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
table_sd_values <- function(vars, means){
  results <- data.frame(matrix(ncol = ncol(means), nrow=nrow(means)))
  colnames(results) <- names(means)
  results$cluster <- means$cluster
  
  
  # for every variable 
  for (j in 1:30){
    # calculate standard deviation 
    sd <- sd(vars[[j+1]])
    print(sd)
    
    for (k in 2:8){
      results[k, j] <- (means[k, j]-means[1, j]/sd)
    }
  }
  
  return(results)
}

#### plot data ####


plot <- vars_shp[2:32]
names <- names(vars_shp)
plot(plot, key.pos = 4)

## plotitng (BUGGED)

png(paste(filename="./Daten/plots_new/2.png"))
par(mfrow = c(2,5))
names <- names(vars_shp)

names <- as.name(names)
name = names[1]
for (name in names){
  print(name)
  x = as.name("pop_yng")
  map <- ggplot() + 
    geom_sf(data = vars_shp, aes(fill=name)) +
    coord_sf(datum = NA)
  map
}
dev.off()

map


plot(vars_shp["pop_yng"])



  #### misc ####
  
  # distance method: euclidean
  
  hrca_list_10_clip_eucl <- hierarchical_clusters(variables, "euclidean", "ward.D", 10)
  export_shp(hrca_list_10_clip_eucl[[4]], "hca_euclidean_10", vars_clip)
  
  # distance method: canberra
  
  hrca_list_10_clip_can <- hierarchical_clusters(variables, "minkowski", "ward.D", 10)
  export_shp(hrca_list_10_clip_can[[4]], "hca_canberra_10", vars_clip)
  
  # distance method: minkowski
  
  hrca_list_10_clip_min <- hierarchical_clusters(variables, "euclidean", "ward.D", 10)
  export_shp(hrca_list_10_clip_min[[4]], "hca_minkowski_10", vars_clip)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##### not used ####
  
  cluster_kmeans = function(variables_km, cutoff){
    # K-Means Cluster Analysis
    fit <- kmeans(variables_km, 10) # 10 cluster solution # removed NA exclude
    
    # get cluster means 
    aggregate(variables_km,by=list(fit$cluster),FUN=mean) # removed NA exclude
    
    #EXPORTAMOS LAS CLASES A TABLA (NOT EXECUTED)
    grid<-rownames(variables_km)
    rownames(as.data.frame(fit$cluster))
    
    grid_kmean <- cbind(grid, fit$cluster)
    
    return(list(fit, grid_kmean))
    
    #write.table(grid_kmean, "./kmeans_table.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")
  }
  
  ##### Principal Component Analysis #### BOOK:https://link.springer.com/chapter/10.1007/978-0-387-45972-1_12
  
  ## Data Transformation
  # https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/)
  
  pca = function(variables_pca, cut_hrc){
    #log transform 
    log.var <- log(na.omit(variables_pca))
    
    #Without prior transformation, the PCA is applied directly to the set of variables. However, standardizing beforehand.
    
    ###We apply the PCA, important the argument "scale=TRUE", to standardize the variables, it is highlighted in both tutorials as highly recommended.
    ##Important, we have to join first the column of TFSE classes to the table "variablesTFSE" to color the PCA according to the groups. To do this we have to have made the cl�ster and cut it by the groups first.
    #This is useful when representing the PCA (not when applying it) so that it considers the 8 types of TFSE and paints the points (municipalities) according to the TFSE they are.It is also useful for the decision tree, since it needs to know to which group each municipality belongs.
    #(two options)
    variables_cut <- data.frame(variables_pca[,3:32], cut_hrc) #Nuevo elemento
    #variablesTFSE$cut10 <- cut10 #A�adimos la columna al elemento ya creado
    
    #Importante, para el PCA no tenemos que considerar la columna de los grupos. Utilizamos la funci�n na.exclude para que no tenga en cuenta los "no data".
    PCA.var <- prcomp(na.exclude(variables_pca[,3:32]), center=TRUE, scale. = TRUE)
    
    return(PCA.var)
  }
  
  ## Analysing Results
  
  pca_analysis = function(pca_varTFSE){
    #m�todo print
    print(PCA_varTFSE)
    
    PCA_varTFSE$center #mean of the variables that are used for normalization prior to implementing PCA.
    PCA_varTFSE$scale #standard deviation of the variables that are used for normalization prior to implementing PCA.
    PCA_varTFSE$sdev #la desviaci�n est�ndar.
    eigenvalues<-PCA_varTFSE$sdev^2 #EIGENVALUES. Es la varianza de cada PC, es decir, su desviaci�n est�ndar al cuadrado. How much a PC is able to explain of our initial data set.
    eigenvectors<-PCA_varTFSE$rotation #EIGENVECTORS. The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. (Ser�a el peso de cada variable en cada componente).
    PCA_varTFSE$x #The principal component score vectors. These are simply the rotated data "the centred (and scaled if requested) data multiplied by the rotation matrix (eigenvectors)". (Ser�a, en cada componente, el sumatorio del valor de cada variable en cada municipio multiplicado por el eigenvector (peso de la variable en cada componente). Este valor determina la posici�n de cada municipio en el gr�fico PCA).
    PCA_varTFSE$rotation[1:5,1:4]
    
    #m�todo plot (The plot method returns a plot of the variances (y-axis) associated with the PCs (x-axis)).
    plot(PCA_varTFSE, type = "l", main="PCA_prcomp_74_variables")
    
    #m�todo summary
    summary(PCA_varTFSE) #Standard deviation, proportion of variance and cumulative proportion (of variance) of the principal components.
    
    #Podemos hacer una matriz de correlaci�n entre las variables y los PC (correlaci�n entre los eigenvectors y los valores de las variables). WEB DE APOYO: http://rfunctions.blogspot.com.ar/2015/01/pca-principal-component-analysis.html
    #Est� bien para ver c�mo se correlaciona cada componente principal con las variables de las que es combinaci�n lineal.
    loadings<-PCA_varTFSE$rotation #Eigenvectors
    scores<-PCA_varTFSE$x
    corr.var.PC <- t(loadings)*PCA_varTFSE$sdev
    #Tambi�n mediante (m�s directo):
    corr.var.PC_2<-cor(scores,na.exclude(variables))
    
    #write.table(corr.var.PC, "./correlacion_variables_PC.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")
    
    #Vamos a dibujar esta matriz, pero omitiendo aquellos valores de correlaci�n que no sean estad�sticamente significativos
    #Para ello tenemos que hacer primero la matriz de los p-valores de la correlaci�n
    p.mat2 <- cor.mtest(corr.var.PC)$p #Realmente no s� qu� meter aqu�, si la matriz de correlaci�n anterior (que procede de correlacionar dos matrices!) o la matriz de variables TFSE!
    corrplot(corr.var.PC, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat =, sig.level = 0.01, insig = "blank", main= "M.correlaci�n_variablesTFSEv-PC_(pvalor0.01)")
    ####OTRA WEB DE AYUDA PARA CONSTRUIR GR�FICOS QUE PERMITEN INTERPRETAR EL PCA: https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
    
    #compute standard deviation of each principal component
    std_dev <- PCA_varTFSE$sdev
    
    #compute variance. EIGENVALUES
    pr_var <- std_dev^2
    
    #proportion of variance explained
    prop_varex <- pr_var/sum(pr_var)
    
    #scree plot
    plot(prop_varex, xlab = "Principal Component",
         ylab = "Proportion of Variance Explained",
         type = "b")
    
    #cumulative scree plot
    plot(cumsum(prop_varex), xlab = "Principal Component",
         ylab = "Cumulative Proportion of Variance Explained",
         type = "b")
  }
  
  ## Graphic Representation
  
  pca_graphics = function(){
    #install_github('fawda123/ggord')
    install.packages("ggord")
    library(ggord)
    library(ggplot2)
    
    
    PCA.TFSE <- ggord(PCA.varTFSE, factor((na.exclude(variablesTFSE_cut15))$cut15), ellipse=FALSE, 
                      ellipse_pro = 0.5, arrow = 0.4, ext = 1.2, 
                      vec_ext = 15, size = 2, txt = 3)
    PCA.TFSE + scale_colour_manual(values = c("palegreen","lemonchiffon1","khaki4","yellow4","green2","deepskyblue2","peru","pink","gold","deepskyblue4","tan4","sandybrown","darkgreen","darkorange","forestgreen"))
    
    ####
    ####AHORA HACEMOS UN AN�LISIS CUYO OBJETIVO ES ANALIZAR LOS COMPONENTES PRINCIPALES
    #### web ayuda: http://rfunctions.blogspot.com.ar/2015/01/pca-principal-component-analysis.html
    
    #Extraemos los Eigenvalues
    #ev<-eigenvals(PCA.varTFSE)
    #Otra forma de conseguir los eigenvalues ser�a mediante (es la que funciona con la funci�n que aplicamos abajo):
    ev <- PCA.varTFSE$sdev^2
    summary(ev)
    
    #APLICAMOS LOS CRITERIOS DE "KRAISER GUTTMAN" Y "BROKEN STICK MODEL" PARA SELECCIONAR LOS EJES
    
    #Funci�n EVPLOT (*Imp.: en lugar de con el paquete de abajo, la definimos a partir del c�digo de la web: http://www.davidzeleny.net/anadat-r/doku.php/en:numecolr:evplot)
    install.packages("lmom", dep=TRUE)
    library(lmom)
    
    evplot <- function(ev)
    {
      # Broken stick model (MacArthur 1957)
      n <- length(ev)
      bsm <- data.frame(j=seq(1:n), p=0)
      bsm$p[1] <- 1/n
      for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
      bsm$p <- 100*bsm$p/n
      # Plot eigenvalues and % of variation for each axis
      op <- par(mfrow=c(2,1))
      barplot(ev, main="Eigenvalues", col="bisque", las=2)
      abline(h=mean(ev), col="red")
      legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
      barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
              main="% variation", col=c("bisque",2), las=2)
      legend("topright", c("% eigenvalue", "Broken stick model"), 
             pch=15, col=c("bisque",2), bty="n")
      par(op)
    }
    
    evplot(ev) #Seg�n KGC (gr�f.superior) podr�amos seleccionar los 14 primeros ejes (barra superior a la l�nea). Seg�n BSM (gr�f. inferior) cumplir�an el criterio solo los 5 primeros (barra eigenvalues superior a la roja_BSM).
    
    # Kraiser Guttman Criterion to select axes. Esto nos da directamente lo que podemos visualizar en el gr�fico anterior (el superior).
    ev[ev > mean(ev)] # Selecciona los 14 primeros ejes
  }
  
  
