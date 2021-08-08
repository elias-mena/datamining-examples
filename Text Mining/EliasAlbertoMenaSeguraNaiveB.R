install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("e1071") #naiveBayes
install.packages("caret")
install.packages("vcd") #Visualizing Categorical Data
install.packages("pROC")
install.packages("Rcpp")

library(Rcpp)
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
library(tm) # Para trabajar texto
library(NLP)  # Natural lenguage procesing
library(wordcloud) # Nuve de palabras
library(SnowballC)
library(tm)
library(e1071) # Algoritmo Naive vayes
library(gmodels) # Para ver el performance
library(tydiverse)
library(caret)
library(vcd)
library(pROC)
library(wordcloud)

  # 1.

# Cargamos el archivo orignal
comments_raw <- data.table::fread(file = "TigoComments.txt")

###########

  # 3.

# Convertimos el sentimiento a factor
comments_raw <- comments_raw %>%
  mutate(Sentimiento = as.factor(Sentimiento))

# Corpora
comments_corpus <- VCorpus(VectorSource(comments_raw$Post))

#### Limpiar el corpus

# Pasar el texto a minuscula
comments_corpus_clean <- tm_map(comments_corpus, 
                           content_transformer(tolower))
# Remover numeros
comments_corpus_clean <- tm_map(comments_corpus_clean, 
                           removeNumbers) 

# Remover stop words 
comments_corpus_clean <- tm_map(comments_corpus_clean, 
                           removeWords, 
                           stopwords(kind = "spanish")) 

# Remover signos de punctuación
comments_corpus_clean <- tm_map(comments_corpus_clean, 
                           removePunctuation)

# Word stemming
comments_corpus_clean <- tm_map(comments_corpus_clean, 
                           stemDocument)

# Elimiar espacios en blanco inncesario
comments_corpus_clean <- tm_map(comments_corpus_clean, 
                           stripWhitespace) 

# Tokenizar los documentos de la córpora
comments_dtm <- DocumentTermMatrix(comments_corpus_clean)


# Data sets de prueba y entrenamiento
comments_dtm_train <- comments_dtm[1:44, ] #75%
comments_dtm_test  <- comments_dtm[44:59, ] #25%


# Labels para traning y test
comments_train_labels <- comments_raw[1:44, ]$Sentimiento
comments_test_labels  <- comments_raw[44:59,]$Sentimiento

# Los sets son representativos
prop.table(table(comments_train_labels))
prop.table(table(comments_test_labels))



# Palabras que más aparecen en los posts 
positivos <- subset(comments_raw, Sentimiento == "Positivo")
negativos  <- subset(comments_raw, Sentimiento == "Negativo")

#Ejemplo con 0.1
sms_dtm_freq_train <- removeSparseTerms(comments_dtm_train, 
                                        0.999)
sms_dtm_freq_train

# Palabras que aparecen al menos 5 veces en los posts
findFreqTerms(comments_dtm_train, 5)

# Variables con las palabras más frecuentes para usarlas en el algoritmo
comments_freq_words <- findFreqTerms(comments_dtm_train, 
                                5)
str(comments_freq_words)


# Training y test con términos frecuentes

comments_dtm_freq_train <- comments_dtm_train[ , comments_freq_words]
comments_dtm_freq_test <- comments_dtm_test[ , comments_freq_words]


# Convertir los 1 y O a Yes or No

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}


comments_train <- apply(comments_dtm_freq_train, 
                   MARGIN = 2, 
                   convert_counts)

comments_test  <- apply(comments_dtm_freq_test, 
                   MARGIN = 2, 
                   convert_counts)

comments_train

# Creamos el modelo
comments_classifier <- naiveBayes(comments_train, 
                                  comments_train_labels,
                                   laplase = 0)

comments_classifier

###########

  # 4.

# model performance
comments_test_pred <- predict(comments_classifier, 
                      comments_test)

head(comments_test_pred)

comments_test_pred <- predict(comments_classifier, 
                      comments_test,
                         type = 'class')
head(comments_test_pred)

CrossTable(comments_test_pred, 
           comments_test_labels,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('predicted', 'actual'))


# Calcular performance
(((12+1)/16)*100)
  # 81.25% de exactitud

###########

  # 5. Prueba con un mensaje de la página de Tigo

mensaje_prueba <- "lo que quiero vivir es la experiencia de un buen internet! 
                    2 meses esperando que arreglen una falla y 1 semana y 
                      parece que el internet va para lo mismo Intermitencia a mas no poder"

cospus_prueba <- VCorpus(VectorSource(mensaje_prueba))

dtm_prueba <- DocumentTermMatrix(cospus_prueba, 
                               control = list(
                                 tolower = TRUE,
                                 removeNumbers = TRUE,
                                 stopwords = TRUE,
                                 removePunctuation = TRUE,
                                 stemming = TRUE
                               ))
prueba_ <- apply(dtm_prueba, 
                 MARGIN = 2, 
                 convert_counts)

prueba_pred <- predict(comments_classifier, 
                       prueba_,
                       type = 'class')
head(prueba_pred)
# Lo predijo negativo

###########

# 6.a

comments_df <- tibble(text = comments_raw$Post)



comments_df

#tokenization

comment_words <- comments_df %>%
  tidytext::unnest_tokens(word, text)

# Stop words español

spanish_stop_words <- tm::stopwords(kind = "spanish")

spanish_stop_words <- tibble(word = spanish_stop_words)

comment_words <- comment_words %>%
  anti_join(spanish_stop_words)

# Gráfico de barras con palabras más frecuentes
comment_words %>%
  count(word, sort = TRUE) %>%
  filter(n>4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(aes(fill="lightblue",color="black")) +
  labs(y = "", x = "Frecuencia") +
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


  # 6.b

# Palabras que más aparecen en los posts
wordcloud(comments_corpus_clean, 
          min.freq = 50, 
          random.order = FALSE)

# Palabras que más aparecen en los posts positivos
wordcloud(positivos$Post, 
          max.words = 40, 
          scale = c(3, 0.5),
          random.order = FALSE)

# Palabras que más aparecen en los posts negativos
wordcloud(negativos$Post, 
          max.words = 40, 
          scale = c(3, 0.5),
          random.order = FALSE)



