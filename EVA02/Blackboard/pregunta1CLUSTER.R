#==================================================================#
#                       ANÁLISIS CLUSTER                #
#==================================================================#

#------------------------------------------------------------------#
#                          Librerías necesarias                    #
#------------------------------------------------------------------#
library(cluster)
library(factoextra)
library(ggplot2)
library(NbClust)
library(reshape2)
library(purrr)
library(ecodist)
library(clValid)

#------------------------------------------------------------------#
#              1.  Cluster no Jerarquico (Kmeas)                   #
#------------------------------------------------------------------#
#para datos pequeños
Cal<-read.delim("clipboard")
head(Cal)
#convertir la primera columna (ID) como etiqueta
Cal1 <- data.frame(Cal[,-1], row.names=Cal[,1])
boxplot(Cal1)
#boxplot(Cal1^2)
#------------------------------------------------------------------#
#                       Estandarizando las variables               #
#------------------------------------------------------------------#
df<-scale(Cal1)
boxplot(df)

#------------------------------------------------------------------#
#              Analizando las correlaciones                        #
#------------------------------------------------------------------#
cor(df)
library(corrplot)

# Numéricos y círculos
corrplot.mixed(cor(df),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

library(GGally)

ggscatmat(df,corMethod = "pearson") +
  theme_bw()
#------------------------------------------------------------------#
#                          ALGORITMO K-MEAN                        #
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#                  Eligiendo el número de Cluster                  #
#------------------------------------------------------------------#

Nclus <- NbClust(df, distance = "euclidean", min.nc = 3, max.nc = 8,method = "kmeans", index = "alllong")
Nclus
par(mfrow=c(1,1))
res <- data.frame(t(Nclus$Best.nc))
table(res$Number_clusters)
barplot(table(res$Number_clusters),col="blue")
#------------------------------------------------------------------#
#                     Visualización de los cluster                 #
#------------------------------------------------------------------#
set.seed(100)
km.res <- kmeans(df, 3,nstart = 25)
fviz_cluster(km.res, data = df,
             palette = "jco",
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

fviz_cluster(km.res, data = df,
             palette = "jco",
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

#------------------------------------------------------------------#
#                 Características de los cluster                   #
#------------------------------------------------------------------#

# Cortando en 3 cluster
# ----------------------
grp3=km.res$cluster
# Number de casos en cada cluster
# -------------------------------
table(grp3)
data.frame(Cal,grp3)


# Descripción de cada cluster
# -------------------------------
med3<-aggregate(Cal1, by=list(cluster=grp3), mean)
med3

# -------------------------------
# Diagrama de caracterización
# -------------------------------
clust.car <- function(data,cluster,standarized){
  data1 <- data
  if (standarized){data1 <- scale(data1)}
  M<-as.data.frame(t(rbind(aggregate(data1, by=list(cluster=cluster), mean)[,-1])))
  a=as.vector(colMeans(data1))
  fin=data.frame(M,a,names(data))
  names(fin)<-c(paste0("clus",1:nlevels(as.factor(cluster))),"Media","var")
  ali=reshape2::melt(fin,id.vars = "var")
  ggplot(ali, aes(x=var,y=round(value,1),group=variable,colour=variable)) +
    geom_point()+
    geom_line(aes(lty=variable))+
    #expand_limits(y = c(-1.9, 1.9))+
    labs(y="value")
}

clust.car(Depar1, grp3, standarized = T)

# Diagrama de caracterización 2
# -------------------------------
dd <- cbind(Cal1, cluster =grp3 )
dd$cluster<-as.factor(dd$cluster)
df.m <- reshape2::melt(dd, id.var = "cluster")
p <- ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=cluster))+ facet_wrap( ~ variable, scales="free") 
p

#---------------SIMULACION---------------#
#PREGUNTA1
#A
df<-scale(Cal1)
boxplot(df)
round(as.matrix(dist(df)),2)

#B
Nclus <- NbClust(df, distance = "euclidean", min.nc = 3, max.nc = 8,method = "kmeans", index = "alllong")
Nclus
par(mfrow=c(1,1))
res <- data.frame(t(Nclus$Best.nc))
res
table(res$Number_clusters)
barplot(table(res$Number_clusters),col="blue")

#C
Nclus <- NbClust(df, distance = "euclidean", min.nc = 3, max.nc = 8,method = "kmeans", index = "alllong")
Nclus
res <- data.frame(t(Nclus$Best.nc))
res
table(res$Number_clusters)
barplot(table(res$Number_clusters),col="blue")

#D
grp3=km.res$cluster
table(grp3)
data.frame(Cal,grp3)

med3<-aggregate(Cal1, by=list(cluster=grp3), mean)
med3

#E
med3<-aggregate(Cal1, by=list(cluster=grp3), mean)
med3
data_clust1 <- subset(data.frame(Cal,grp3), grp3 == 1)
data_clust1
sd(data_clust1$X1)

#PREGUNTA2
