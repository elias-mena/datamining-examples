#Hacer comentarios, se ejecuta ctrl + enter
1 + 1
install.packages("randomForest")
install.packages("dplyr")#siempre usar
install.packages("Rtools")
install.packages("ggplot2")#visual
install.packages("tidyverse")#visual, este instala dplyr y ggplot2
install.packages("xlsx")
library(randomForest) #Con esto se llama en un proyecto
rfNews()
randomForest::rfNews()
if(!require("randomForest"))
  install.packages("randomForest")
?randomForest
vignette(package = "randomForest")
version
library(dplyr)
library(ggplot2)
library(tidyverse)
library(xlsx)
vignette(package = "dplyr")
vignette(package = "dplyr", topic = "programming")
detach(name = package:randomForest)#esto lo elimina
dim(available.packages())
data()
data(package = .packages(all.available = TRUE))
data(package = "dplyr")
data(cars)
data(iris)
data("AirPassengers")
x <- 28 #sirve para asignar usar solo este
x = 28 #sirve tambien para asignar
class(x)
x * 2
is.numeric(x)
y <- "R is  Fantastic"
class(y)
is.character(y)
z <- 42
z
class(z)

p <- 10

### Vector
vec_num <- c(1,10,49)
vec_num[2]

is.vector(vec_num)

### no es usuario usar sentencia for
(vec_num2 <- c(1,10,49))
vec_num2 + 3
vec_num2 <- vec_num2 +3
vec_num2

(vec_num2 <- vec_num2 * 5)
##sacar la media
calif_vec <- c(89, 92, 95, 96, 99)
mean(calif_vec)
median(calif_vec)
setwd("C:\\Users\\psoto\\OneDrive - ULACIT ED CR\\Ingenier???a inform???tica\\VI Cuatrimestre\\Mineria de Datos\\Semana II")
list.files()

PoliceDepartmentIncidentcsv <- read.table(file = "PoliceDepartmentIncident.csv",
                                          header = TRUE,
                                          sep = ",")
PoliceDepartmentIncidentcsv <- read.csv2(file = "PoliceDepartmentIncident.csv",
                                         header = TRUE,
                                         sep = ",")
legalweedAgeLimpio <- legal_weed_age %>%
  mutate(age = recode(age, "89 OR OLDER" = "89")) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(grass = as.factor(grass)) %>%
  mutate(grass = na_if(grass, "DK")) %>%
  mutate(grass = na_if(grass, "IAP")) %>%
  mutate(grass = droplevels(grass)) %>%
  mutate(ageCat = cut(age,
                      breaks = c(-Inf,29,59,74,Inf),
                      labels = c("<30", "30-59", "60-74", "75+")
                      
  ))
str(legalweedAgeLimpio)
summary(legalweedAgeLimpio)

# Pasamos el data frame por un pipe, luego si queremos lo transformamos 
# Y lo visualizamos con ggplot
legalWeedAgeLimpio %>%
  drop_na(grass) %>%
  ggplot(aes(x = grass,
             fill = grass)) +
  geom_bar()+
  scale_fill_manual(values = c("#78A678","#7463AC"), guide = FALSE)+
  theme_minimal()+
  labs(x = "¿Debe legalizarse la mota?",
       y = "Cantidad de respuestas")
  
