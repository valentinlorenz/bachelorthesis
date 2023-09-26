###Análisis de correlación variables TFSEs###

##Directorio de trabajo
dir.datos="C:/Users/MANU/Google Drive/TESIS/Manu_Tesis_Doctoral/Capitulo_2/analisisR_TFSE/Cluster_nuevaBD/TFSE/10TFSE_manhattan_29var/Final_var_reord"

setwd(dir.datos)

#Leemos tabla de variables
variablesTFSE<-read.table("nuevaBD_tfse_R_reord_names.csv", header=TRUE, sep=";") #IMP.: si volvemos a hacer los análisis con "nuevaBD_tfse_R_reord", tenemos que quitar [,2:30] del script (eso lo usabamos para quitar del análisis la columna con el nombre de los municipios).

str(variablesTFSE)
dim(variablesTFSE)
summary(variablesTFSE)
colnames(variablesTFSE)

########
######## ÉSTE ES EL ANÁLISIS QUE HACÍAMOS EN ECOINFORMÁTICA
########


#Instalación librería para el análisis de correlación VIF (es el que nos enseñaron en Ecoinformática, que había que hacerlo sucesivamente a medida que íbamos eliminando las variables más correlacionadas).
#install.packages("HH", dep=TRUE)

#Cargamos librería
library(HH)

#todas las variables con valores VIF menores que 5 pueden seleccionarse
vif(variablesTFSE)






########
######## Ahora vamos a hacer el análisis de correlación con todo el set de variables ordenadas.
######## Seguimos los pasos de la Web: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
########

#install.packages("devtools", dep=TRUE)
library(devtools)
#install_github("taiyun/corrplot", build_vignettes = TRUE)
library(corrplot)

variables.correlacion <- cor(variablesTFSE[,2:30], method= "pearson", use= "complete.obs")
str(variables.correlacion)

##Ahora hacemos la matriz de correlación.
p.mat <- cor.mtest(variablesTFSE[,2:30])$p #Con este objeto luego podemos eliminar en la matriz los no significativos
corrplot(variables.correlacion, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat = NULL, sig.level = 0.01, insig = "blank", main= "Matriz de correlación_variables TFSE")
corrplot(variables.correlacion, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat = p.mat, sig.level = 0.01, insig = "blank") #En este se eliminan las correlaciones no significativas. , main= "Matriz de correlación_variables TFSE_(pvalor0.01)"
#(tenemos que buscar la forma de optimizar el plot, porque al guardarlo se ve muy pequeño)

write.table(variables.correlacion, "C:/Users/Manu/Google Drive/TESIS/Manu_Tesis_Doctoral/Capitulo_2/analisisR_TFSE/Cluster_nuevaBD/TFSE/10TFSE_manhattan_29var/Final_var_reord/coor_matrix_nuevaBD_reord.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")


##############
############## A partir de aquí comenzamos con el análisis clúster y con el de componentes principales.
##############

#instalamos los paquetes que van a hacernos falta
#install.packages("vegan", dep=TRUE)
#install.packages("ade4", dep=TRUE)
#install.packages("gclus", dep=TRUE)
#install.packages("labdsv", dep=TRUE)

#Cargamos los paquetes
library(vegan)
library(ade4)
library(gclus)
library(cluster)
library(RColorBrewer)
library(labdsv)

#Leemos tabla de variables
#variablesTFSE<-read.table("variables_tfse_R.csv", header=TRUE, sep=";")


####
####AHORA HACEMOS EL CLÚSTER JERÁRQUICO
####

###OPCIÓN 1: CALCULAMOS MATRIZ DE DISTANCIAS CON LA FUNCIÓN "DIST" (con este nos sale el árbol un poco mejor, y los cortes posteriores tienen más sentido)
### LA OTRA OPCIÓN ES CALCULAR DISTANCIAS CON "VEGDIST" -> vegdist(varTFSE.norm, binary=FALSE, na.rm=TRUE, method="euc") #Mido disimilitud

#ESTANDARIZACIÓN DE LAS VARIABLES

variablesTFSE.est<-scale(variablesTFSE[,2:30])  #https://www.statmethods.net/advstats/cluster.html

#MATRIZ DE DISTANCIAS ENTRE MUNICIPIOS
muni.dist<-dist(variablesTFSE.est, method="manhattan")

#CLUSTER DE VARIABLES SEGÚN LA DISTANCIA (MENOR DISTANCIA = MAYOR CORRELACIÓN)
muni.cluster<-hclust(muni.dist, method="ward.D") # Análisis jerárquico cluster a partir de matriz de disimilitud

#DIBUJAMOS EL CLÚSTER
plot(muni.cluster, xlab="", ylab="Height", labels=FALSE, hang=-1, lwd=2, main="") # title(clases_TFSE_scale, dist, hclus) #labels=variablesTFSE$Name

#PARA HACER LOS GRUPOS DE MUNICIPIOS A PARTIR DEL CLÚSTER
rect.hclust(muni.cluster, 8, border= c("darkred", "darkgreen")) #HAY QUE IR PROBANDO CON DIFERENTE NÚM. DE GRUPOS
rect.hclust(muni.cluster, 10, border= c("red","mistyrose2","darksalmon","darkred","goldenrod4","olivedrab","darkkhaki","darkgreen","darkolivegreen1","palegreen"))
rect.hclust(muni.cluster, 15, border= c("palegreen","lemonchiffon1","khaki4","yellow4","green2","deepskyblue2","peru","pink","gold","deepskyblue4","tan4","sandybrown","darkgreen","darkorange","forestgreen"))
rect.hclust(muni.cluster, 15, border="grey")

#CORTAMOS EL CLÚSTER AL NIVEL SELECCIONADO
cut15 <- cutree(muni.cluster, 15) 

#EXPORTAMOS LAS CLASES A TABLA
municipios<-rownames(variablesTFSE)
rownames(as.data.frame(cut15))

municipios_TFSE <- cbind(municipios, cut15)

dim(municipios_TFSE)

write.table(municipios_TFSE, "C:/Users/Manu/Google Drive/TESIS/Manu_Tesis_Doctoral/Capitulo_2/analisisR_TFSE/cut15_manhattan_29var_nuevaBD.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")

##############
###AHORA PROBAMOS A HACER EL CLÚSTER CON K-MEANS
##############

library(stats)

#ESTANDARIZACIÓN DE LAS VARIABLES
variablesTFSE.est<-scale(variablesTFSE)  #https://www.statmethods.net/advstats/cluster.html

# K-Means Cluster Analysis
TFSE <- kmeans(na.exclude(variablesTFSE.est), 10) # 10 cluster solution

# get cluster means 
aggregate(na.exclude(variablesTFSE.est),by=list(TFSE$cluster),FUN=mean)

#EXPORTAMOS LAS CLASES A TABLA
municipios<-rownames(variablesTFSE)
rownames(as.data.frame(TFSE$cluster))

municipios_TFSE <- cbind(municipios, TFSE$cluster)

dim(municipios_TFSE)

write.table(municipios_TFSE, "d:/Users/usuario/Google Drive/TESIS/Manu_Tesis_Doctoral/Capitulo_2/analisisR_TFSE/cluster_10TFSE_73var_k-means.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")


###################################################################
############## Principal Component Analysis ####################### BOOK:https://link.springer.com/chapter/10.1007/978-0-387-45972-1_12
###################################################################

###TRANSFORMACIONES PREVIAS

#Transformación logarítmica (en este tutorial: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/)

#log transform 
#log.varTFSE <- log(na.omit(variablesTFSE))

#Sin transformación previa, se aplica el PCA directamente al set de variables. Eso sí, estandarizando previamente.

###Aplicamos el PCA, importante el argumento "scale=TRUE", para estandarizar las variables, se desataca en ambos tutoriales como altamente recomendable.

#Importante, tenemos que unir primero la columna de clases de TFSE a la tabla "variablesTFSE" para colorear el PCA en función de los grupos. Para ello tenemos que haber hecho el clúster y cortarlo por los grupos primero.
#Esto sirve a la hora de representar el PCA (no al aplicarlo) para que considere los 8 tipos de TFSE y pinte los puntos (municipios) en función del TFSE que son.También nos sirve para el Árbol de decisión, puesto que necesita saber a que grupo pertenece cada municipio.
#(dos opciones)
variablesTFSE_cut15 <- data.frame(variablesTFSE[,2:30], cut15) #Nuevo elemento
variablesTFSE$cut10 <- cut10 #Añadimos la columna al elemento ya creado

#Importante, para el PCA no tenemos que considerar la columna de los grupos. Utilizamos la función na.exclude para que no tenga en cuenta los "no data".
PCA.varTFSE <- prcomp(na.exclude(variablesTFSE[,2:30]), center=TRUE, scale. = TRUE)

###ANALIZANDO LOS RESULTADOS. INTERPRETACIÓN DEL PCA
#método print
print(PCA.varTFSE)

PCA.varTFSE$center #mean of the variables that are used for normalization prior to implementing PCA.
PCA.varTFSE$scale #standard deviation of the variables that are used for normalization prior to implementing PCA.
PCA.varTFSE$sdev #la desviación estándar.
eigenvalues<-PCA.varTFSE$sdev^2 #EIGENVALUES. Es la varianza de cada PC, es decir, su desviación estándar al cuadrado. How much a PC is able to explain of our initial data set.
eigenvectors<-PCA.varTFSE$rotation #EIGENVECTORS. The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. (Sería el peso de cada variable en cada componente).
PCA.varTFSE$x #The principal component score vectors. These are simply the rotated data "the centred (and scaled if requested) data multiplied by the rotation matrix (eigenvectors)". (Sería, en cada componente, el sumatorio del valor de cada variable en cada municipio multiplicado por el eigenvector (peso de la variable en cada componente). Este valor determina la posición de cada municipio en el gráfico PCA).
PCA.varTFSE$rotation[1:5,1:4]

#método plot (The plot method returns a plot of the variances (y-axis) associated with the PCs (x-axis)).
plot(PCA.varTFSE, type = "l", main="PCA_prcomp_74_variables")

#método summary
summary(PCA.varTFSE) #Standard deviation, proportion of variance and cumulative proportion (of variance) of the principal components.

#Podemos hacer una matriz de correlación entre las variables y los PC (correlación entre los eigenvectors y los valores de las variables). WEB DE APOYO: http://rfunctions.blogspot.com.ar/2015/01/pca-principal-component-analysis.html
#Está bien para ver cómo se correlaciona cada componente principal con las variables de las que es combinación lineal.
loadings<-PCA.varTFSE$rotation #Eigenvectors
scores<-PCA.varTFSE$x
corr.var.PC <- t(loadings)*PCA.varTFSE$sdev
#También mediante (más directo):
corr.var.PC_2<-cor(scores,na.exclude(variablesTFSE))

write.table(corr.var.PC, "C:/Users/Manu/Google Drive/TESIS/Manu_Tesis_Doctoral/Capitulo_2/analisisR_TFSE/correlacion_variables_PC.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")

#Vamos a dibujar esta matriz, pero omitiendo aquellos valores de correlación que no sean estadísticamente significativos
#Para ello tenemos que hacer primero la matriz de los p-valores de la correlación
p.mat2 <- cor.mtest(corr.var.PC)$p #Realmente no sé qué meter aquí, si la matriz de correlación anterior (que procede de correlacionar dos matrices!) o la matriz de variables TFSE!
corrplot(corr.var.PC, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "circle", type = "upper", tl.col = "black", order = "original", p.mat =, sig.level = 0.01, insig = "blank", main= "M.correlación_variablesTFSEv-PC_(pvalor0.01)")

###OTRA WEB DE AYUDA PARA CONSTRUIR GRÁFICOS QUE PERMITEN INTERPRETAR EL PCA: https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/

#compute standard deviation of each principal component
std_dev <- PCA.varTFSE$sdev

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

###REPRESENTACIÓN GRÁFICA SEGÚN EL SCRIPT DE MARÍA

#install_github('fawda123/ggord')
library(ggord)
library(ggplot2)

PCA.TFSE <- ggord(PCA.varTFSE, factor((na.exclude(variablesTFSE_cut15))$cut15), ellipse=FALSE, 
                  ellipse_pro = 0.5, arrow = 0.4, ext = 1.2, 
                  vec_ext = 15, size = 2, txt = 3)
PCA.TFSE + scale_colour_manual(values = c("palegreen","lemonchiffon1","khaki4","yellow4","green2","deepskyblue2","peru","pink","gold","deepskyblue4","tan4","sandybrown","darkgreen","darkorange","forestgreen"))

####
####AHORA HACEMOS UN ANÁLISIS CUYO OBJETIVO ES ANALIZAR LOS COMPONENTES PRINCIPALES
#### web ayuda: http://rfunctions.blogspot.com.ar/2015/01/pca-principal-component-analysis.html

#Extraemos los Eigenvalues
#ev<-eigenvals(PCA.varTFSE)
#Otra forma de conseguir los eigenvalues sería mediante (es la que funciona con la función que aplicamos abajo):
ev <- PCA.varTFSE$sdev^2
summary(ev)

#APLICAMOS LOS CRITERIOS DE "KRAISER GUTTMAN" Y "BROKEN STICK MODEL" PARA SELECCIONAR LOS EJES

#Función EVPLOT (*Imp.: en lugar de con el paquete de abajo, la definimos a partir del código de la web: http://www.davidzeleny.net/anadat-r/doku.php/en:numecolr:evplot)
#install.packages("lmom", dep=TRUE)
#library(lmom)

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

evplot(ev) #Según KGC (gráf.superior) podríamos seleccionar los 14 primeros ejes (barra superior a la línea). Según BSM (gráf. inferior) cumplirían el criterio solo los 5 primeros (barra eigenvalues superior a la roja_BSM).

# Kraiser Guttman Criterion to select axes. Esto nos da directamente lo que podemos visualizar en el gráfico anterior (el superior).
ev[ev > mean(ev)] # Selecciona los 14 primeros ejes

#####################################################################################
######################ÁRBOLES DE REGRESIÓN / DECISIÓN / RANDOM FOREST################
#####################################################################################

#Esto lo vamos a aplicar para ver cuáles son las variables más importantes en la clasificación de nuestras diferentes categorías de TFSE.
#Es necesario haber hecho primero el análisis clúster para saber a qué TFSE pertece cada municipio (objeto cut10).

#install.packages("dismo", dep=TRUE)
#install.packages("plotmo", dep=TRUE)
#install.packages("randomForest", dep=TRUE)
#install.packages("party", dep=TRUE)
#install.packages("tree", dep=TRUE)

library(dismo)
library(plotmo)
library(randomForest)
#library(party)
library(tree)

#Dado que nuestros datos tienen diferentes órdenes de magnitud, primero es necesario estandarizar.
variablesTFSE.est<-scale(variablesTFSE[,2:30])

#Unimos el objeto cut10 (creado en el análisis clúster) a nuestra tabla de variables estandarizada.
variablesTFSE.est_cut15 <- data.frame(variablesTFSE.est, as.factor(cut15)) #Nuevo elemento. Recordar que lo de as.factor es para que entienda a cut10 como categorías en lugar de un valor numérico más.

#GENERA LA FORMULA DE REGRESION (Esto es del script de eccoinformática de modelos_distribucion.R)
#formula.regresion<-as.formula(paste("presencia ~ ", paste(names(resultado.vif), collapse="+"), collapse=""))

formula.regresion<-as.formula(paste("as.factor(cut15) ~ ", paste(names(variablesTFSE[,2:30]), collapse="+"), collapse=""))
formula.regresion #IMPORTANTE: esta fórmula es la que luego pegamos cuando aplicamos la función tree. 


#RANDOM FORESTS
###############
#AYUDA DE randomForest
help(randomForest)

#AJUSTE DEL MODELO randomForest CON LOS DISTINTOS TFSE

rf.TFSE<-randomForest(formula.regresion, data=variablesTFSE.est_cut15, importance=TRUE, proximity= TRUE, na.action=na.exclude, ntree=2000)#Da error

#NOTA: Los parámetros nodesize y maxnodes

#PLOTS DEL ERROR SEGÚN NÚMERO DE ÁRBOLES
plot(rf.TFSE)

#DEVIANZA EXPLICADA
print(rf.TFSE)
conf.matrix<-print(rf.TFSE)

#CURVAS DE RESPUESTA
#con plotmo
plotmo(rf.TFSE)

#con partialPlot (variable a variable)
help(partialPlot)
partialPlot(rf.TFSE, x.var=SEC_10, pred.data=variablesTFSE.est_cut10)

#IMPORTANCIA DE LAS VARIABLES
importance(rf.TFSE)
rf_imp_var<-round(importance(rf.TFSE), 2) #Mucho más factible para su interpretación.

#Gráfico
varImpPlot(rf.TFSE, main="")


###PARA HACER LOS GRÁFICOS FINALES
#Una vez que tengamos hecha la selección de variables final y el análisis clúster

#1º: Unir el la base datos con las variables finales al objeto cut10
variablesTFSE_cut10 <- data.frame(variablesTFSE, cut10) #Nuevo elemento

#2º: Extraer la tabla a csv.
write.table(variablesTFSE_cut10, "C:/Users/pachecom/Google Drive/TESIS/Manu_Tesis_Doctoral/Capitulo_2/analisisR_TFSE/Cluster_nuevaBD/variablesTFSE_cut10_28var_nuevaBD.csv", quote=T, row.names=T, col.names=T, dec=".", sep=";")


