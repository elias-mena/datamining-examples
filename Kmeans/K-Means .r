##############################################################
## Ejemplo de KMEANS con datos producidos para ver los 
## centroides

## Librerías
install.packages("NbClust")
install.packages("factoextra")

library(NbClust)
library(factoextra)

set.seed(1234) #Hacer los resultados reproducibles
#par(mar=c(0,0,0,0)) #Plot Margins

#Vector numérico de 12 números
#Usando el promedio y la desv estándar
x <- rnorm(12, mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12, mean=rep(c(1,2,1),each=4),sd=0.2)

plot(x,y, col="dark green", pch=19, cex=3)
text(x+0.05,y+0.05,labels=as.character(1:12))

df <- data.frame(x,y)

set.seed(1234) #Hacer los resultados reproducibles

#K-Means ++
kmeans1 <- kmeans(df,centers = 3,iter.max = 50)
kmeans1$cluster
kmeans1$centers

#par(mar=rep(0.2,4))
plot(x,y,col=kmeans1$cluster,pch=19, cex=2)
points(kmeans1$centers, col=1:4, pch=3, lwd=3)

kmeans2 <- kmeans(df,centers = 3,iter.max = 50,nstart = 25)
kmeans2$cluster
kmeans2$centers

#par(mar=rep(0.2,4))
plot(x,y,col=kmeans2$cluster,pch=19, cex=2)
points(kmeans2$centers, col=1:4, pch=3, lwd=3)

fviz_cluster(kmeans2, data = df,
             palette = c("#00AFBB","#2E9FDF", "#E7B800"),
             ggtheme = theme_minimal(),
             main = "Clustering K-Means")


##############################################################
## Ejemplo de KMEANS para IRIS

par(mar=c(1,1,1,1)) #Plot Margins

data("iris")

iris_x <- iris[,-5]
iris_y <- iris$Species

kmeansIris <- kmeans(iris_x, centers=3,iter.max = 300,nstart = 30)

print(paste("K-means    Clustering-  Confusion matrix"))
table(iris_y,kmeansIris$cluster)

mat_avgss <- matrix(nrow=10, ncol=2)


#Promedio del cluster en suma de cuadrados
for (i in (1:10)) 
{
  km_fit <- kmeans(iris_x, centers=i,iter.max = 100,nstart = 30)
  mean_km <- mean(km_fit$withinss) #silhouette coefficient
  print(paste("K-value", i, "Avg.within suma de cuadrados",
              round(mean_km,2)))
  mat_avgss[i,1] <- i
  mat_avgss[i,2] <- mean_km
}


plot(mat_avgss[,1],mat_avgss[,2],type = 'o',
     xlab = "K_Value",ylab = "Avg Suma Cuadrados")
title("Avg. en suma de cuadrados vs K-Values")

#Explicando el % de varianza
mat_varexp <- matrix(nrow=10, ncol=2)
for (i in (1:10)) 
{
  km_fit <- kmeans(iris_x, centers=i,iter.max = 100,nstart = 30)
  var_exp <- km_fit$betweenss/km_fit$totss
  print(paste("K-value", i, "Varianza explicada",
              round(var_exp,2)))
  mat_varexp[i,1] <- i
  mat_varexp[i,2] <- var_exp
}

plot(mat_varexp[,1],mat_varexp[,2],type = 'o',
     xlab = "K_Value",ylab = "% Varianza Exp")
title("Avg. en suma de cuadrados vs K-Values")

fviz_cluster(kmeansIris, data = iris_x,
             palette = c("#00AFBB","#2E9FDF", "#E7B800"),
             ggtheme = theme_minimal(),
             main = "Iris Clustering K-Means")

##############################################################
## Ejemplo de Gustos y Preferencias

## Explore los datos
setwd("C:/Luis Diego/DataMining/K-Means")

teens <- read.csv("snsdata.csv",stringsAsFactors = TRUE)
str(teens)

# Mostrar frecuencia usando NA para género
table(teens$gender)

table(teens$gender, useNA = "ifany")

# NUlls values en la edad. Ojo al rango de edad.
summary(teens$age)
hist(teens$age)

# Eliminar age outliers, ver los NULLs values aumento.
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                     teens$age, NA)

summary(teens$age)
hist(teens$age)

# Male or female
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)

# No Gender, un nuevo vector
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# Verificar la asignación de Género
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# Promedio y el uso del cohort
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) # works

# age by cohort
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# Crear un vector con la edad promedio según el graduation year, 
# para cada persona. Usar ave que devuelve un vector 
ave_age <- ave(teens$age, 
               teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))

str(ave_age)

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# Se elimna los datos NULLs
summary(teens$age)
hist(teens$age)

## Crear K-MEANS Models

# Estandarizar usando z-score (mean = 0, STDEV = 1)
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))


# Compare los resultados
summary(interests$basketball)
summary(interests_z$basketball)
#-0.12
#0.73
hist(interests_z$basketball)

# Clusters using k-means
set.seed(2345)

teen_clusters <- kmeans(interests_z, 
                        centers=5,
                        iter.max = 100,
                        nstart = 30)

## Model performance
# Size
teen_clusters$size
table(teen_clusters$cluster)

fviz_cluster(teen_clusters, data = interests_z,
             palette = c("#00AFBB","#2E9FDF", "#E7B800","#FC4E07","#688ff4"),
             ggtheme = theme_minimal(),
             main = "Teens Clustering K-Means")


mat_avgss <- matrix(nrow=10, ncol=2)

#Heterogeneidad
for (i in (1:10)) 
{
  km_fit <- kmeans(interests_z, centers=i,iter.max = 100,nstart = 30)
  mean_km <- mean(km_fit$withinss) #silhouette coefficient
  print(paste("K-value", i, "Heterogeneidad",
              round(mean_km,2)))
  mat_avgss[i,1] <- i
  mat_avgss[i,2] <- mean_km
}


plot(mat_avgss[,1],mat_avgss[,2],type = 'o',
     xlab = "K_Value",ylab = "Avg Suma Cuadrados")
title("Heterogeneidad")

teen_clusters <- kmeans(interests_z, 
                        centers=3,
                        iter.max = 100,
                        nstart = 30)

fviz_cluster(teen_clusters, data = interests_z,
             palette = c("#00AFBB","#2E9FDF", "#E7B800"),
             ggtheme = theme_minimal(),
             main = "Teens Clustering K-Means")

teen_clusters$size
table(teen_clusters$cluster)


# Centroides

teen_clusters <- kmeans(interests_z, 
                        centers=5,
                        iter.max = 100,
                        nstart = 30)

teen_clusters$centers
c <- teen_clusters$centers

## Model performance
# Asignar el cluster al ds original
teens$cluster <- teen_clusters$cluster

# Primeros 5 registros
teens[1:5, c("cluster", "gender", "age", "friends")]

# Promedio por cluster
aggregate(data = teens, age ~ cluster, mean)

# Proporción de mujeres por cluster
aggregate(data = teens, female ~ cluster, mean)

# Promedio de Num Amigos por cluster
aggregate(data = teens, friends ~ cluster, mean)
