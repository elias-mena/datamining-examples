#KNN Ejercicio

#### Paquetes 
install.packages("car")
install.packages("gmodels")
install.packages("caret")
install.packages("tidyverse")
install.packages("class")
install.packages("e1071")
install.packages("psych")
install.packages("dummies")

library(gmodels)
library(caret)
library (tidyverse)
library(class) #K-NN
library(e1071)
library(car)
library(psych)
library(dummies)

#### Funciones

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Funcion para medir el perf
accuracy <- function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
}

processKNN <- function(trainingFeatures,
                       testingFeatures,
                       trainingLabel,
                       neighbors){
  attritionPrediction <-  knn(train=trainingFeatures,
                              test=testingFeatures,
                              cl=trainingLabel,
                              k=neighbors)
  return (attritionPrediction)
}

#function to evaluate performance
knnPerformanceEvolution <- function(start, 
                                    end, 
                                    trainingFeatures,
                                    testingFeatures,
                                    trainingLabel,
                                    trainingTestLabel){
  accuracyEvolution <- data.frame(neighbors=integer(),
                                  accuracy=double())
  for(i in start:end) {
    newPrediction <- processKNN(trainingFeatures,
                                testingFeatures,
                                trainingLabel,
                                i)
    
    tempTable <- table(newPrediction,trainingTestLabel)
    
    accuracyEvolution[nrow(accuracyEvolution) + 1,] = 
      list(i,accuracy(tempTable))
  }
  return(accuracyEvolution <- data.frame(accuracyEvolution))
}

####

setwd("C:/Luis Diego/DataMining/KNN")
getwd()

###########################################################################
# DETECTAR Ingredient
###########################################################################
Ingredient <- read.csv(file = 'Ingredient_csv.csv'
                      ,stringsAsFactors = FALSE)

head(Ingredient)
NROW(Ingredient)
str(Ingredient)

# scatterplot todos los datos del DS, usando default lib
plot(x = Ingredient$Sweetness, 
     y = Ingredient$Crunchiness,
     main = "Scatterplot Training Data Set",
     xlab = "How sweet the food tastes",
     ylab = "How crunchy the food is")

#scatterplot usando ggplot2
#Hay que instalar ggplot2 primero
Ingredient %>%
ggplot(aes(x=Sweetness, 
           y=Crunchiness, 
           color=Ingredient)
       ) +
  geom_point()

Ingredient %>%
ggplot(aes(x=Sweetness, 
           y=Crunchiness, 
           label=Ingredient)
) +
  geom_text()

# Convertir la clase en un factor
Ingredient$FoodType <- factor(Ingredient$FoodType,
                              levels = c("Fruit","Vegetable","Protein"),
                              labels = c("Fruit","Vegetable","Protein"))

str(Ingredient)

table(Ingredient$FoodType)
prop.table(table(Ingredient$FoodType))
prop.table(table(Ingredient$FoodType)) *100
round(prop.table(table(Ingredient$FoodType)) * 100, 
      digits = 1)

# Datos de entrenamiento y de prueba
IngredientTraning <- Ingredient[1:15,-1]
IngredientTraning <- IngredientTraning[,-3]
NROW(IngredientTraning)

IngredientTest <- Ingredient[16:18,c(-1,-4)]
NROW(IngredientTest)

# Si no se tiene idea, comenzar con esto.
# K = 4
sqrt(15)

# Training Labels
IngredientTraningLabels <- Ingredient[1:15,4]
str(IngredientTraningLabels)

#Test Labels
IngredientTestLabels <- Ingredient[16:18,4]
str(IngredientTestLabels)


#class library
#Usar el KNN para clasificar los ingredientes
IngredientKNN <- knn(train=IngredientTraning,
                     test=IngredientTest,
                     cl=IngredientTraningLabels,
                     k=4)

str(IngredientKNN)
IngredientKNN


#Performace
CrossTable(x=IngredientTestLabels,
           y=IngredientKNN,
           prop.chisq = FALSE)

#Crear una Matriz
tb <- table(IngredientKNN,IngredientTestLabels)
tb

#LLamar a la función
accuracy(tb)


#ConfusionMatrix para medir perf
#Requerido instalar e1071 package
confusionMatrix(table(IngredientKNN,
                      IngredientTestLabels))

accuracyEvolution <- knnPerformanceEvolution(3,
                                             10, 
                                             IngredientTraning,
                                             IngredientTest,
                                             IngredientTraningLabels,
                                             IngredientTestLabels)

plot(x = accuracyEvolution[,1], 
     y = accuracyEvolution[,2],
     xlab = "Neighbors (K)",
     ylab = "Accuracy (%)",
     type="l")

#¿Cuál es la categoría para una sandía/aguacate?
sandia <- c(10,3)
aguacate <- c(2,1)

#OJO que no hay modelo, no hay generalización, ni abstracción
#No se puede usar la función predict()
sandiaKNN <- knn(train=IngredientTraning,
                 test=sandia,
                 cl=IngredientTraningLabels,
                 k=4)

sandiaKNN

###########################################################################
# DETECTAR Heart Disease
###########################################################################

# 'n' stands for numerical and 'f' stands for factor.
heart <- read_csv("heart.csv", col_types = "nffnnffnfnfnff")

# Vista previa de los datos, y la metadata
glimpse(heart)

summary(heart)

# Como estamos trabajando con datos médicos, es mejor ser conservativo
# en cuanto a imputar valores nulos, así que los excluísmos.
heart <- heart %>%
  filter(
    !is.na(restingBP) &
      !is.na(cholesterol) &
      !is.na(highBloodSugar) &
      !is.na(restingECG) &
      !is.na(restingHR) &
      !is.na(exerciseAngina) &
      !is.na(STdepression) &
      !is.na(STslope) &
      !is.na(coloredVessels) &
      !is.na(defectType)
  )

#scatterplot usando ggplot2
heart %>%
  sample_n (20) %>%
  ggplot(aes(x=cholesterol, 
             y=age, 
             color=heartDisease)
  ) +
  geom_point()

#Normalizar los valores numéricos
heart <- heart %>%
  mutate(age = normalize(age)) %>%
  mutate(restingBP = normalize(restingBP)) %>%
  mutate(cholesterol = normalize(cholesterol)) %>%
  mutate(restingHR = normalize(restingHR)) %>%
  mutate(STdepression = normalize(STdepression)) %>%
  mutate(coloredVessels = normalize(coloredVessels))

summary(heart)

# Pasar heart de tibble a data.frame para poder aplicar la 
# función dummy. Esto para poder usar las variables categóricas y que
# estén también normalizadas
heart <- data.frame(heart)

# Separar la clase, o variable Y, de las X, o variables predictoras
heart_labels <- heart %>% 
  select(heartDisease)

heart <- heart %>% 
  select(-heartDisease)

colnames(heart)

heart <- dummy.data.frame(data = heart, sep = "_")

colnames(heart)

# Lograr el mismo resultado
set.seed(1234)

#Total de observaciones
nrow(heart)

sample_index <-
  sample(nrow(heart), round(nrow(heart) * .75), 
         replace = FALSE)

# Crear dos datasets, uno de train y otro de test, variables X
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

# Hacer lo mismo con la varible Y
heart_train_labels <- as.factor(heart_labels[sample_index,])
heart_test_labels <- as.factor(heart_labels[-sample_index,])

# Si no sabemos K correctamente, usamos raíz cuadrada de la cantidad
# de observaciones del ds de training
sqrt(nrow(heart_train))

# Crear el modelo
heart_pred1 <-
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 44
  )

# TOP 6
head(heart_pred1)

#### Determinar el performance
#Crear una Matriz
tb <- table(heart_pred1,heart_test_labels)
tb

#LLamar a la función
accuracy(tb)

# ¿Cuál podría ser un mejor K para mejorar el performance?
accuracyEvolution <- knnPerformanceEvolution(1,
                                             50, 
                                             heart_train,
                                             heart_test,
                                             heart_train_labels,
                                             heart_test_labels)

plot(x = accuracyEvolution[,1], 
     y = accuracyEvolution[,2],
     xlab = "Neighbors (K)",
     ylab = "Accuracy (%)",
     type="l")

###########################################################################
# DETECTAR Especies
###########################################################################

data("iris")

n <- nrow(iris) #número de observaciones

#iris$isVersicolor <- iris$Species == "versicolor" #Agregar un nuevo vector

str(iris)
head(iris)

ntrain <- round(n*0.6) #60% para training

set.seed(333) #resultados reproducibles

tindex <- sample(n,ntrain) #obtener un index para filtrar

train_iris_x <- iris[tindex,-5]
test_iris_x <- iris[-tindex,-5]

train_iris_label <- iris[tindex,5]
test_iris_label <- iris[-tindex,5]

head(train_iris_label)
head(test_iris_label)

plot(train_iris_x$Petal.Length,
     train_iris_x$Petal.Width,
     pch=c(train_iris_label)
     )

plot(train_iris_x$Sepal.Length,
     train_iris_x$Sepal.Width,
     pch=c(train_iris_label)
)

sqrt(nrow(train_iris_x))

IrisKNN <- knn(train=train_iris_x,
               test=test_iris_x,
               cl=train_iris_label,
               k=9)

table(IrisKNN,
      test_iris_label)

# K = 9, 3% error, 97% performance.

100 * (2/nrow(test_iris_x))
sum(IrisKNN != test_iris_label)