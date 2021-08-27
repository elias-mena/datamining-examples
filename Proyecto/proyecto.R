install.packages("EBImage")
install.packages("keras")
install.packages("BiocManager")
install.packages("BiocManager")
library(EBImage)
library(keras)
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
getwd();
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg','dog (7).jpg','dog (8).jpg','dog (9).jpg','dog (10).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg','cat (7).jpg','cat (8).jpg','cat (9).jpg','cat (10).jpg');
MyImagenes <- list()
for(i in 1:300) {MyImagenes[[i]] <-  readImage(imagenes[i])}
for(i in 1:20) {MyImagenes[[i]] <-  readImage(imagenes[i])}
for (i in 1:20) {mypic[[1]] <- resize(mypic[[i]], 28, 28)}
for (i in 1:20) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
str(MyImagenes)
#reshape
for (i in 1:20) {MyImagenes[[i]] <- array_reshape(MyImagenes[[i]], c(28, 28, 3))}
n
#reshape
for (i in 1:20) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
library(EBImage)
library(keras)
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg','dog (7).jpg','dog (8).jpg','dog (9).jpg','dog (10).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg','cat (7).jpg','cat (8).jpg','cat (9).jpg','cat (10).jpg');
MyImagenes <- list()
#load images
for(i in 1:20) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:20) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
#reshape
for (i in 1:20) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
reticulate::install_miniconda()
#reshape
for (i in 1:20) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
str(MyImagenes)
# Row Bind
trainx <- NULL
for (i in 1:5) {trainx <- rbind(trainx, MyImagenes[[i]])}
for (i in 1:3) {trainx <- rbind(trainx, MyImagenes[[i]])}
View(trainx)
for (i in 1:12) {trainx <- rbind(trainx, MyImagenes[[i]])}
for (i in 1:10) {trainx <- rbind(trainx, MyImagenes[[i]])}
for (i in 7:20) {trainx <- rbind(trainx, mypic[[i]])}
for (i in 7:20) {trainx <- rbind(trainx, MyImagenes[[i]])}
for (i in 10:20) {trainx <- rbind(trainx, MyImagenes[[i]])}
testx <- rbind(MyImagenes[[10]], MyImagenes[[20]])
View(trainx)
trainy <- c(0,0,0,0,1,1,1,1,1)
testy <- c(0, 1)
trainlabels <- to_categorical(trainy)
install_tensorflow()
install.packages("tensorflow")
install.packages("tensorflow")
library(EBImage)
library(keras)
trainlabels <- to_categorical(trainy)
install.packages("tensorflow")
library(tensorflow)
trainlabels <- to_categorical(trainy)
library(keras)
trainlabels <- to_categorical(trainy)
library(EBImage)
library(keras)
library(tensorflow)
use_condaenv("r-tensorflow")
trainlabels <- to_categorical(trainy)
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
getwd();
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg','dog (7).jpg','dog (8).jpg','dog (9).jpg','dog (10).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg','cat (7).jpg','cat (8).jpg','cat (9).jpg','cat (10).jpg');
MyImagenes <- list()
#load images
for(i in 1:20) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:20) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
display(MyImagenes[[2]])
#load images
for(i in 1:20) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:20) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
str(MyImagenes)
#reshape
for (i in 1:20) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
getwd();
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg','dog (7).jpg','dog (8).jpg','dog (9).jpg','dog (10).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg','cat (7).jpg','cat (8).jpg','cat (9).jpg','cat (10).jpg');
MyImagenes <- list()
#load images
for(i in 1:20) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:20) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
#reshape
for (i in 1:20) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
getwd();
str(MyImagenes)
#reshape
for (i in 1:20) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28))}
library(EBImage)
library(keras)
library(tensorflow)
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
getwd()
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg','dog (7).jpg','dog (8).jpg','dog (9).jpg','dog (10).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg','cat (7).jpg','cat (8).jpg','cat (9).jpg','cat (10).jpg');
MyImagenes <- list()
#load images
for(i in 1:20) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:20) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
#reshape
for (i in 1:20) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
str(MyImagenes)
imagenes <- c('dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg','dog (7).jpg','dog (8).jpg','dog (9).jpg','dog (10).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg','cat (7).jpg','cat (8).jpg','cat (9).jpg','cat (10).jpg');
MyImagenes <- list()
#load images
for(i in 1:19) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:19) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
str(MyImagenes)
#reshape
for (i in 1:19) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg');
MyImagenes <- list()
#load images
for(i in 1:12) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:12) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(28, 28, 3))}
#Resize
for (i in 1:12) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 24, 24)}
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(24, 24, 3))}
str(MyImagenes)
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(24, 24, 2))}
#List where the imagenes will be save
MyImagenes <- for(i in 1:12) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:12) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 28, 28)}
#List where the imagenes will be save
MyImagenes <- list()
#load images
for(i in 1:12) {MyImagenes[[i]] <-  readImage(imagenes[i])}
str(MyImagenes)
?array_reshape
#Resize
for (i in 1:12) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 50, 50)}
str(MyImagenes)
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(50, 50, 3))}
str(MyImagenes)
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(50, 50, -1))}
str(MyImagenes)
#Resize
for (i in 1:12) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 25, 25)}
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(25, 25, -1))}
library(EBImage)
library(keras)
library(tensorflow)
#Set workzone
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
#set imagenes name
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg');
#load images
for(i in 1:12) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#List where the imagenes will be save
MyImagenes <- list()
#load images
for(i in 1:12) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:12) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 25, 25)}
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(25, 25, -1))}
# Row Bind
trainx <- NULL
for (i in 9:19) {trainx <- rbind(trainx, MyImagenes[[i]])}
for (i in 9:11) {trainx <- rbind(trainx, MyImagenes[[i]])}
testx <- rbind(MyImagenes[[10]], MyImagenes[[20]])
modelo <- keras_model_sequential()
library(tensorflow)
modelo <- keras_model_sequential()
library(EBImage)
library(keras)
library(tensorflow)
#Set workzone
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
#set imagenes name
imagenes <- c('dog (1).jpg','dog (2).jpg','dog (3).jpg','dog (4).jpg','dog (5).jpg','dog (6).jpg',
              'cat (1).jpg','cat (2).jpg','cat (3).jpg','cat (4).jpg', 'cat (5).jpg','cat (6).jpg');
#List where the imagenes will be save
MyImagenes <- list()
#load images
for(i in 1:12) {MyImagenes[[i]] <-  readImage(imagenes[i])}
#Resize
for (i in 1:12) {MyImagenes[[i]] <- resize(MyImagenes[[i]], 25, 25)}
#reshape
for (i in 1:12) {MyImagenes[[i]] <-  array_reshape(MyImagenes[[i]], c(25, 25, -1))}
for (i in 9:19) {trainx <- rbind(trainx, MyImagenes[[i]])}
for (i in 9:11) {trainx <- rbind(trainx, MyImagenes[[i]])}
# Row Bind
trainx <- NULL
for (i in 9:11) {trainx <- rbind(trainx, MyImagenes[[i]])}
testx <- rbind(MyImagenes[[10]], MyImagenes[[20]])
trainy <- c(0,0,0,0,1,1,1,1,1)
testy <- c(0, 1)
trainlabels <- to_categorical(trainy)
reticulate::install_miniconda()
install_tensorflow()
library(tensorflow)
trainlabels <- to_categorical(trainy)
trainlabels <- to_categorical(trainy)
testlabels <- to_categorical(testy)
#modelo
modelo <- keras_model_sequential()
#modelo
modelo <- keras_model_sequential()
library(EBImage)
library(keras)
library(tensorflow)
#Set workzone
setwd('C:/Users/jeanf/OneDrive/Escritorio/imagenes')
getwd()


trainy <- c()

for (i in 1:299) {
  if (i < 149) {
    trainy <- c(trainy,0) 
  } 
  if (i > 150) {
    trainy <- c(trainy,1) 
  }
}
