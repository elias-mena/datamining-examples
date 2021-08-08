#Instalar y cargar la libería TM (Text Mining)
#NLP (Natural Language Processing). Es recomendado
#aún si ya están instaldo los paquetes previamente
install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("e1071") #naiveBayes
install.packages("caret")
install.packages("vcd") #Visualizing Categorical Data
install.packages("pROC")

library(tm)
library(NLP)
library(SnowballC)
library(wordcloud)
library(e1071)
library(gmodels)
library(caret)
library(vcd)
library(pROC)
library(tidyverse)

#Establecer el directorio de trabajo
setwd("C:/Luis Diego/DataMining/Naive Bayes")
getwd()

###########################################################################
# Clasificar HAM o SPAM Sin TM
###########################################################################
email <- read_csv("email.csv")

head(email)

# Convertir message en factor.
email <- email %>%
  mutate(message_label = as.factor(message_label))

# Use gather() function para pivotear  columnas en filas.
email %>%
  gather(word, 
         count,
         -message_index, 
         -message_label)

#TOP palabras más frecuentes
email %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

#TOP palabras más frecuentes en ham
email %>%
  filter(message_label=='ham') %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

#TOP palabras más frecuentes en spam
email %>%
  filter(message_label=='spam') %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

#Training/test datasets
set.seed(1234)

sample_set <- sample(nrow(email), 
                     round(nrow(email)*.75), 
                     replace = FALSE)

email_train <- email[sample_set, ]
email_test <- email[-sample_set, ]

#Verificar la proporción, evitar imbalanced classification problem
email %>%
  select(message_label) %>%
  table() %>%
  prop.table() * 100 

email_train %>%
  select(message_label) %>%
  table() %>%
  prop.table() * 100 

email_test  %>%
  select(message_label) %>%
  table() %>%
  prop.table() * 100 

#Construir el modelo Naive Bayes
email_mod <-
  e1071::naiveBayes(message_label ~ . - message_index,
                    data = email_train
                    )

# To get the predicted class labels, we set type="class".
email_pred <- stats::predict(email_mod, 
                             email_test, 
                             type = "class")
head(email_pred)

# Matrix of our results.
email_pred_table <- table(email_test$message_label, 
                          email_pred)
email_pred_table

# Accuracy
sum(diag(email_pred_table)) / nrow(email_test)

#Mejorar el performance
email_mod <-
  e1071::naiveBayes(message_label ~ . - message_index,
                    data = email_train,
                    laplace = 1)

email_pred <- predict(email_mod, 
                      email_test, 
                      type = "class")

email_pred_table <- table(email_test$message_label, 
                          email_pred)
email_pred_table

# Accuracy
sum(diag(email_pred_table)) / nrow(email_test)

email_pred <- predict(email_mod, 
                      email_test, 
                      type = "raw")

head(email_pred)

###########################################################################
# DETECTAR Especies
###########################################################################

data("iris")

n <- nrow(iris) #número de observaciones

#iris$isVersicolor <- iris$Species == "versicolor" #Agregar un nuevo vector

ntrain <- round(n*0.6) #60% para training

set.seed(333) #resultados reproducibles

tindex <- sample(n,ntrain) #obtener un index para filtrar

train_iris <- iris[tindex,]
test_iris <- iris[-tindex,]


plot(train_iris$Petal.Length,
     train_iris$Petal.Width,
     pch=c(train_iris$Species)
)

plot(train_iris$Sepal.Length,
     train_iris$Sepal.Width,
     pch=c(train_iris$Species)
)


IrisNaiveBayes <- naiveBayes(Species~.,
                             data=train_iris)

IrisNaiveBayes

Prediction <-  predict(IrisNaiveBayes,
                       test_iris[,-5])

table(Prediction,
      test_iris$Species)

# 3% error, 97% performance.

#Distrubución de frecuencia de la clase o variable 
#dependiente, o Y
IrisNaiveBayes$apriori

#Lista de tablas, una por cada variable X, o predictora
#Para una variable continua, se proporciona la media y 
#la desviación estándar para cada clase a predecir
IrisNaiveBayes$tables$Petal.Length

plot(function(x) dnorm(x, 1.49,0.1349329),
     0,
     8,
     lty=1,
     main="Petal Distribution",
     col = "green"
     )

curve(dnorm(x, 4.306452,0.3890749),
      add = TRUE,
      lty=2,
      col = "red"
      )

curve(dnorm(x, 5.578571,0.5321088),
      add = TRUE,
      lty=5,
      col = "blue"
)

legend("topright", 
       legend = c("setosa","versicolor","virginica"),
       lty = c(1,2,5),
       col = c("green", "red", "blue"))

###########################################################################
# DETECTAR SPAM
###########################################################################

#Leer el dataset de SMS data
sms_raw <- read.csv("sms_spam.csv", 
                    stringsAsFactors = FALSE)

#Examinar la estructura del dataset
str(sms_raw)

# Convertir Type a un factor
sms_raw$type <- factor(sms_raw$type)

# Eximar la variable Type
str(sms_raw$type)
table(sms_raw$type)

#VCorpus en mem, PCorpus en disco
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# Examinar SMS corpus (recordar que es una lista)
print(sms_corpus)
inspect(sms_corpus[1:2])

#Ver un solo mensaje
as.character(sms_corpus[[1]])

#Ver varios mensajes con la misma instrucción
lapply(sms_corpus[1:4], as.character)

# Clean up the corpus using tm_map()
# pasar todo el texto a minúscula con tm_map
sms_corpus_clean <- tm_map(sms_corpus, 
                           content_transformer(tolower))

# Obsevar la diferencia
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

# Remover numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           removeNumbers) 

# Remove stop words (to, and, but, or, etc)
sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           removeWords, 
                           stopwords()) 
stopwords()
?stopwords
stopwords(kind = "spanish")

# remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           removePunctuation)

# Word stemming
wordStem(c("learn", "learned", "learning", 
           "learns"))

wordStem(c("niños"),language = "spanish")

sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           stemDocument)

# Elimiar espacio en blanco inncesario
sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           stripWhitespace) 

# Eximar el resultado final
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# Dividir cada documento en sus palabras.
# Este proceso se llama Tokenization y se hace
# por medio de la funcion que ofrece TM que se llama
# document-term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# Mismo proceso de tokenization, pero sin 
# el proceso de preparar la data y lo hacemos
# con los parametros.
#               HACER ASÍ
sms_dtm2 <- DocumentTermMatrix(sms_corpus, 
                               control = list(
                                 tolower = TRUE,
                                 removeNumbers = TRUE,
                                 stopwords = TRUE,
                                 removePunctuation = TRUE,
                                stemming = TRUE
                                ))

# Compara los dos frames, observe las diferencias
sms_dtm
sms_dtm2

# El problema es que esta función hace todo el 
# proceso luego de creados los términos, es necesario
# quitar los stopwords antes de crear términos
sms_dtm3 <- DocumentTermMatrix(sms_corpus, 
                               control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = function(x) { removeWords(x, stopwords()) },
    removePunctuation = TRUE,
    stemming = TRUE
))

# compare the result
sms_dtm
sms_dtm2
sms_dtm3

# Training and test datasets
# El orden no importa
sms_dtm_train <- sms_dtm[1:4169, ] #75%
sms_dtm_test  <- sms_dtm[4170:5559, ] #25%

# Labels para traning y test
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

# Los sets son representativos
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# Visualizar text data-words con
# word cloud visualization
wordcloud(sms_corpus_clean, #Datos que más aparecen en el spam
          min.freq = 50, 
          random.order = FALSE)

# Crear dos dataframes una para spam y otro para
# ham
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

wordcloud(spam$text, 
          max.words = 40, 
          scale = c(3, 0.5),
          random.order = FALSE)

wordcloud(ham$text, 
          max.words = 40, 
          scale = c(3, 0.5),
          random.order = FALSE)

# Reducir el número de features, actualmente hay 
# cerca de 7,000. Eliminar cualquier palabra que
# aparezca en menos de 5 palabras, o en menos del 
# 0.1


#Ejemplo con 0.1
sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 
                                        0.999)
sms_dtm_freq_train

# Términos más frecuentes, aquellos que aparecen
# al menos 5 veces en los mensajes
findFreqTerms(sms_dtm_train, 5)

# Salvar los términos más frecuentes en su vector
# para usarlo en el algoritmo.
sms_freq_words <- findFreqTerms(sms_dtm_train, 
                                5)
str(sms_freq_words)


# Crear training y test solamente con términos
# frecuentes. Filtrar por row, col; donde los
# cols son los términos frecuentes. 
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

sms_dtm_freq_train
sms_dtm_freq_test 

# Convertir los 1 y O a Yes or No
# Esto porque Naive B soporta variable categóricas
# (usualmente se entrena con feat categóricas)
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_dtm_freq_train, 
                   MARGIN = 2, 
                   convert_counts)

sms_test  <- apply(sms_dtm_freq_test, 
                   MARGIN = 2, 
                   convert_counts)

sms_train

# Training a model
# Otra opción de librearía sería klaR

# Crear el modelo.
sms_classifier <- naiveBayes(sms_train, 
                             sms_train_labels,
                             laplase = 0
                             )

sms_classifier

# model performance
sms_test_pred <- predict(sms_classifier, 
                         sms_test)

head(sms_test_pred)

sms_test_pred <- predict(sms_classifier, 
                         sms_test,
                         type = 'class')


head (sms_test_pred)


CrossTable(sms_test_pred, 
           sms_test_labels,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# Improving performance
sms_classifier2 <- naiveBayes(sms_train, 
                              sms_train_labels, 
                              laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, 
                          sms_test)

CrossTable(sms_test_pred2, 
           sms_test_labels,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('predicted', 'actual'))

(((1189+167)/1390)*100)

100 - (((16+18)/1390)*100)


#########################
######Entender y mejorar rendimiento
sms_test_prob <- predict(sms_classifier, 
                         sms_test,
                         type = 'raw')


head (sms_test_prob)

#Crear un DF con valores combinados. 
sms_results <- data.frame(actual_type = sms_test_labels,
                          predict_type = sms_test_pred,
                          prob_spam = round(sms_test_prob[ , 2], 5),
                          prob_ham = round(sms_test_prob[ , 1], 5))

head(sms_results)

#¿Qué pasa cuando el probabilidad está entre cero y uno?
head(subset(sms_results, 
            prob_spam > 0.40 & prob_spam < 0.60))

# Casos donde el modelo se equivocó por completo
# Extremadamente confidente, y extremadamente equivocado 
head(subset(sms_results, 
            actual_type != predict_type))

table(sms_results$actual_type, 
      sms_results$predict_type)

#También conocida como Success Rate
accuracy = ((1201+153)/1390)*100
accuracy

ErrorRate = 100 - (((1201+153)/1390)*100)
ErrorRate

#library(gmodels)
CrossTable(sms_results$actual_type, 
           sms_results$predict_type)

#library(caret)
confusionMatrix(sms_results$predict_type, 
                sms_results$actual_type, 
                positive = "spam") #La intención siempre es predecir spam

#Kappa : 0.8801 
#Podemos decir que el resultado del modelo replica, o tienen un muy buen
#nivel de acuerdo con los resultados reales. 

#library(vcd)
Kappa(table(sms_results$actual_type, 
            sms_results$predict_type)) #Valor de interés Unweighted

#library(caret)
#sensitivity & specificity, entre más cerca de 1 mejor (0-1)
sensitivity(sms_results$predict_type, 
            sms_results$actual_type, 
            positive = "spam")

specificity(sms_results$predict_type, 
            sms_results$actual_type, 
            negative = "ham")

(1-0.995029)*100 # ¿Es 0.4% muy agresivo?

## Visualizing Performance Tradeoffs ----
#library(pROC)
sms_roc <- roc(sms_results$actual_type, 
               sms_results$prob_spam)

sms_roc

# ROC curve for Naive Bayes
plot(sms_roc, 
     main = "ROC curve for SMS spam filter", 
     col = "blue", 
     lwd = 2, 
     legacy.axes = TRUE)

# compare to kNN 
sms_results_knn <- read.csv("sms_results_knn.csv")

sms_roc_knn <- roc(sms_results$actual_type, 
                   sms_results_knn$p_spam)

sms_roc_knn

plot(sms_roc_knn, 
     col = "red", 
     lwd = 2, 
     add = TRUE)

# calculate AUC for Naive Bayes and kNN
auc(sms_roc)
auc(sms_roc_knn)




## Estimating Future Performance ----

# partitioning data
library(caret)
credit <- read.csv("credit.csv")

# Holdout method
# using random IDs
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# using caret function
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# 10-fold CV
folds <- createFolds(credit$default, k = 10)
str(folds)
credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----
library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv")

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))

