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

}

##### Correlation ####
##File directory

load_data = function(){
    dir.datos="D:/RStudio"
    setwd(dir.datos)

    #Leemos tabla de variables
    variables<-read.table("variables_scaled_final.csv", header=TRUE, sep=",") #IMP.: si volvemos a hacer los an�lisis con "nuevaBD_tfse_R_reord", tenemos que quitar [,2:30] del script (eso lo usabamos para quitar del an�lisis la columna con el nombre de los municipios).
    return(variables)
    
    str(variables)
    dim(variables)
    summary(variables)
    colnames(variables)
}

# Corrplot
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

plot_corr = function(variables_cor){

    variables.correlacion <- cor(variables_cor[,3:length(variables_cor)], method= "pearson", use= "complete.obs")
    str(variables.correlacion)

    # Correlatiom matrix
    p.mat <- cor.mtest(variables_cor[,3:length(variables_cor)])$p #to remove non-significant elements from matrix
    corrplot(variables.correlacion, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat = NULL, sig.level = 0.01, insig = "blank", main= "Correlation Matrix")
    corrplot(variables.correlacion, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat = p.mat, sig.level = 0.01, insig = "blank") #Eliminate non-significant correlations

    return(variables.correlacion)
}

#### Hierarchical Clustering #####

#OPTION 1: CALCULATE DISTANCE MATRIX WITH THE "DIST" FUNCTION (with this we get the tree a little better, and the later cuts make more sense).
#OPTION 2: CALCULATE DISTANCES WITH "VEGDIST" -> vegdist(varTFSE.norm, binary=FALSE, na.rm=TRUE, method="euc") #I measure dissimilarity.

#https://www.statmethods.net/advstats/cluster.html

hierarchical_clusters = function(variables_hrc=variables, method_dist="manhattan", method_clust="Ward.D", cutoff=10){
    #Distance Matrix
    distances<-dist(variables_hrc[,3:length(variables_hrc)], method=method_dist)
    
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
    
    grid<-rownames(variables)
    rownames(as.data.frame(cut))
    grid_bind <- cbind(grid, cut, cut1)
    
    return(list(cut, grid_bind, cluster, cut1))

    #dim(grid_bind)

    #write.table(grid_bind, "./hrc_table.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")
}

#### k means clustering ####

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

#### Random Forest ####

#We are going to apply this to see which are the most important variables in the classification of our different TFSE categories.

#Dado que nuestros datos tienen diferentes �rdenes de magnitud, primero es necesario estandarizar.
variablesTFSE.est<-scale(variablesTFSE[,2:30])

random_forest = function(variables_rf, cut){
    variables_cut <- data.frame(variables_rf, as.factor(cut)) #Nuevo elemento. Recordar que lo de as.factor es para que entienda a cut10 como categor�as en lugar de un valor num�rico m�s.

    #Regression Model
    #formula.regresion<-as.formula(paste("presencia ~ ", paste(names(resultado.vif), collapse="+"), collapse=""))
    formula.regresion<-as.formula(paste("as.factor(cut) ~ ", paste(names(variables_rf[,3:length(variables_rf)]), collapse="+"), collapse=""))

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

export_shp = function(hra_results, filename, shapefile){
  # shapes <- readOGR(".", layer="data_scaled")
  
  df <- as.data.frame(hra_results)
  df$ID <- seq(from=1, to=length(df$hra_results))
  
  shp <- cbind(shapefile, df, by="ID")
  shp <- shp[1:length(df)-2]
  
  writeOGR(obj=shp, dsn="./clusters_clipped", layer=filename, driver="ESRI Shapefile")
}

main = function(){
  #install_packages()
  load_packages()
  # variables <- load_data()
  vars_clip <- readOGR(".", layer="variables_clip")
  variables <- as.data.frame(vars_clip@data)
  variables <- variables[,1:32]
  
  variables$pop_old <- variables$pop_old/variables$pop_dns
  variables$pop_old[is.na(variables$pop_old)] <- 0
  variables$pop_yng <- variables$pop_yng/variables$pop_dns
  variables$pop_yng[is.na(variables$pop_yng)] <- 0
  
  variables$pop_yng <- rescale(variables$pop_yng, to=c(0,1))
  
  #selecting variables with VIF < 5
  vif(variables)
  
  # clipped dataset, manhattan distance
  
  plot_corr(variables)
  hrca_list_10_clip <- hierarchical_clusters(variables, "manhattan", "ward.D", 10)
  kmeans_list10_clip <- cluster_kmeans(variables, 10)
  
  rf_10_manhattan <- random_forest(variables, hrca_list_10_clip[[1]])
  
  #nvclust
  
  #remove ndvi_max
  
  variables_rmv <- subset(variables, select = -ndvi_mx)
  cor <- plot_corr(variables_rmv)
  hrca_list_10_clip_rmv <- hierarchical_clusters(variables_rmv, "manhattan", "ward.D", 10)
  rf_10_manhattan_rmv <- random_forest(variables_rmv, hrca_list_10_clip_rmv[[1]])
  
  export_shp(hrca_list_10_clip[[4]], "hca_manhattan_10.shp", vars_clip)

  # distance method: euclidean
  
  hrca_list_10_clip_eucl <- hierarchical_clusters(variables, "euclidean", "ward.D", 10)
  export_shp(hrca_list_10_clip_eucl[[4]], "hca_euclidean_10", vars_clip)
  
  # distance method: canberra
  
  hrca_list_10_clip_can <- hierarchical_clusters(variables, "minkowski", "ward.D", 10)
  export_shp(hrca_list_10_clip_can[[4]], "hca_canberra_10", vars_clip)
  
  # distance method: minkowski
  
  hrca_list_10_clip_min <- hierarchical_clusters(variables, "euclidean", "ward.D", 10)
  export_shp(hrca_list_10_clip_min[[4]], "hca_minkowski_10", vars_clip)
  
}
