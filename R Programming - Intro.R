########Actualización del R, si ya está instalado
#Este código se corre en la consola y no en RStudio
install.packages("installr")
library(installr)

updater() 

#############################################
#Actualización para VSC
install.packages("languageserver")


########Instalar una libería y usarla como prueba
install.packages("randomForest")

#cargar el paquete en memoria, en otras palabras, un grupo de funciones
#y dataset que no son parte la distribución base de R
library(randomForest) 

randomForest::rfNews()
rfNews()

#### Instalar un paquete verificando que no esté instalado aún
if(!require("randomForest")) 
  install.packages("randomForest")

### Obtener la información de un paquete
?randomForest

vignette(package = "randomForest")
vignette(package = "dplyr")
vignette(package = "dplyr", topic = "programming")

#### detach un paquete previamente cargado
detach(name = package:randomForest)

########################### Instalación y referencia de paquetes de
#la sesión global
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("xlsx")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(xlsx)
library(dplyr)

########Número de paquetes disponibles en R
dim(available.packages())

######## R DATASETS

#Listar los datasets disponibles. 
#Depende de los paquetes instalados, y de los paquetes que estén
#cargados en memoria al momento de ejecutar el comando
data()

#Listar todos los datasets disponibles de los paquetes instalados
data(package = .packages(all.available = TRUE))

#Listar todos los datasets disponibles de un paquete específico
data(package = 'plyr')

#Conocer más detalles sobre un dataset
?airquality

#Cargar un dataset en memoria
data(iris)
data(cars)
data(AirPassengers)

# Pruebas de datos
summary(cars$speed)
sd(cars$speed)
hist(cars$speed)

summary(cars)

########Declarar variables de diferentes tipos

#Numeric
x <- 28
class(x)
is.numeric(x)

#String/Character
y <- "R is Fantastic"
class(y)
is.character(x)
is.character(y)

#Logical/Boolean
z <- TRUE
class(z)
is.logical(z)

#Integer
i <- 21L
class(i)
is.numeric(i)
is.integer(i)

#Conversiones
as.numeric("1.5")
as.integer("1.5")
as.character("1.5")
as.integer("apple")
as.logical(1)
as.logical(0)
as.logical("true")
as.logical("apple")
is.na(as.logical("apple"))

########Declaración de variables
#Imprimir el valor de X
x <- 42 ### forma de asig.
x

#Imprimir el valor de y
y = 10
y

#Imprimir resultado
x-y

resultado = x-y
resultado

######## Vector: Array de una dimensión

########Numeric
vec_num <- c(1,10,49)
vec_num

vec_num[1]

is.vector(vec_num)

#Crear el vector e imprimir los valores
#en una sola instrucción
(vec.num2 <- c(1,10,49))

#Sumar 3 a cada elemento del vector e imprimir resultado
(vec.num2 + 3)

#Multiplicar por 3 a cada elemento del vector e imprimir resultado
(vec.num2 * 5)

#Asignmar el resultado del producto  a otro vector e impirmir
#resultado en una sola instrucc
(vec.num3 <- vec.num2 * 5)

#Vector de calificaciones
calif_vec <- c(89,92,95,96,99)

#Funciones sobre el vector completo
mean(calif_vec)
median(calif_vec)
min(calif_vec)
max(calif_vec)
sum(calif_vec)

#coercion
calif_mix <- c("Luis",89,"Pablo",92,"Maria",95,"Alexa",96,
               "Jose",99)

class(calif_mix)

calif_vec <- c(89,92,95,96,99)
calif_name <- c("Luis","Pablo","Maria","Alexa","Jose")

calif <- data.frame(calif_name,calif_vec)

#Usar $ para acceder los vectores en el dataframe
mean(calif$calif_vec)

########Character
vec_chr <- c("a","b","c")
vec_chr

########Boolean
vec_bool <- c(TRUE, FALSE, TRUE)
vec_bool

########Operaciones aritmética sobre vectores
vec_1 <- c(1,3,5)
vec_2 <- c(2,4,6)

vec_1 + vec_2

########Slicing Vectors
sclice_vector <- c(10,20,30,40,50,60,70,80,90,100)

########Between
sclice_vector[1:5]

########Except
sclice_vector[-5]
sclice_vector[-5:-8]

########Crear valores en forma rápida
vec_autocreado <- c(1:10)
vec_autocreado

######### Operadores artiméticos
3+4

3*5

(5+5)/2

2^5

28%%6

######## Operadores lógicos
#crear un vector de 1 a 10
vec_oper_log <- c(1:10)
vec_oper_log>5 #no hay necesidad de un FOR


#Extraer valores que cumplen una condic
vec_oper_log[(vec_oper_log>5)]
vec_oper_log[(vec_oper_log>4) & (vec_oper_log<7)]

######## Factores
#Vector género
vector_genero <- c("Masculimo", "Femenimo", "Femenimo","Masculimo","Masculimo")
class(vector_genero)
vector_genero
is.vector(vector_genero)

#Factor nominal de género, convertir un vector a un factor
#factor_genero <- factor(vector_genero)
factor_genero <- as.factor(vector_genero)
class(factor_genero)
factor_genero
is.factor(factor_genero)

#Factor nominal de color
vector_color <- c("Azul", "Rojo", "Verde","Blanco","Negro")
factor_color <- as.factor(vector_color)
factor_color #no se puede dictar un orden


#Factor ordinal
#de más bajo a más alto orden = TRUE 
#de más alto o más bajo orden = FALSE

vector_momento <- c("Noche", "Mañana", "Tarde","Medio Día",
                    "Media Noche", "Noche")
vector_momento
factor_momento <- factor(vector_momento, 
                         order = TRUE,
                         levels = c("Mañana","Medio Día","Tarde","Noche",
                                    "Media Noche")
                         )
factor_momento

summary(factor_momento) #número de ocurrencias por cada nivel

###### DATA FRAMES
a <- c(10,20,30,40)
b <- c('libro','lapicero','libro de texto','estuche')
c <- c(TRUE, FALSE, TRUE, FALSE)
d <- c(7000,500,10000,2500)

df <- data.frame(a,b,c,d)
df

#Asignar los nombres correctos
names(df) <- c('ID', 'Artículo','En Inventario','Precio')
df

#La estrctura del data frame
str(df)

#Filtrar un data frame
df[1,2]

df[,1]

df[1:2,]

df[1:3,3:4]

df[-2] #Eliminar la vector 2

df[,c('ID','En Inventario')] # Con nombre del vector

#Agregar una columna/vector al data frame
cantidad <- c(10,0,35,0)

df$Cantidad <- cantidad
df

#Otra forma de seleccionar o trabajar con vector/columna
df$ID

#Predicados con SUBSET
subset(df,subset = Precio>5000)

#Crear un df con tibble y ordenarlo
set.seed(1234)
df_sort <- tibble(
                  c1 = rnorm(50,5,1.5),
                  c2 = rnorm(50,5,1.5),
                  c3 = rnorm(50,5,1.5),
                  c4 = rnorm(50,5,1.5),
                  c5 = rnorm(50,5,1.5)
                  )
df_sort
df <- df_sort[order(df_sort$c1),] #Solo c1
head(df)

df <- df_sort[order(df_sort$c3,df_sort$c4),] #c3 and c4
head(df)

df <- df_sort[order(-df_sort$c3,df_sort$c4),] #c3 (descending) and c4
head(df)


######## Funciones
funcion_alcuadrado <- function(n)
{
  n^2
}
funcion_alcuadrado(4)


#Borrar objeto
rm(y,z)

funcion_multiplica <- function(x,y)
{
  x*y
}
funcion_multiplica(2,4)

########Administrar Working Directory

#Obtener Working Directory
getwd()

#Set Working Directory. Notar que se usa /
setwd("C:/Luis Diego/DataMining/Program R - INTRO")
setwd('C:/Luis Diego/DataMining/Program R - INTRO')

#Set Working Directory. Windows Syntax Notar que se usa \\
setwd("C:\\Luis Diego\\DataMining\\Program R - INTRO")

########Cargar DATASETs para ML

#listar los archivos
list.files()

#cargar el archivo descargado en un dataframe
#Uso con TABLE
PoliceDepartmentIncidentCSV <- read.table(file = 'PoliceDepartmentIncident.csv', 
                                          header = TRUE,
                                          sep = ',')
#Uso con CSV
PoliceDepartmentIncidentCSV <- read.csv(file = 'PoliceDepartmentIncident.csv', 
                                        stringsAsFactors = FALSE,
                                        sep = ',')

PoliceDepartmentIncidentCSV <- data.table::fread(file = "PoliceDepartmentIncident.csv",
                                                 stringsAsFactors = FALSE,
                                                 sep = ',')

#Top 6 del archivo
head(PoliceDepartmentIncidentCSV)

#Cargar un XLSX Dataset
PoliceDepartmentIncidentXLS <- read.xlsx2(file = 'PoliceDepartmentIncident.xlsx', 
                                          sheetIndex = 1)

#### R data structures --------------------

## Vectors -----

# create vectors of data for three medical patients
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

class(subject_name)

# access the second element in body temperature vector
temperature[2]

## examples of accessing items in vector
# include items in the range 2 to 3
temperature[2:3]

# exclude item 2 using the minus sign
temperature[-2]

# use a vector to indicate whether to include item
temperature[c(TRUE, TRUE, FALSE)]

## Factors -----

# add gender factor
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender
str(gender)
# add blood type factor, categorical/nominal
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood

# add ordered factor, categorical/ordinal
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)
symptoms

# check for symptoms greater than moderate
symptoms > "MODERATE"

## Lists -----

# display information for a patient
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

# create list for a patient
subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

# display the patient
subject1
class(subject1)


## Data frames -----

# create a data frame from medical patient data

pt_data <- data.frame(subject_name, 
                      temperature, 
                      flu_status, 
                      gender,
                      blood, 
                      symptoms, 
                      stringsAsFactors = FALSE)

# display the data frame
pt_data
str(pt_data)
## accessing a data frame

# get a single column
pt_data$subject_name

# get several columns by specifying a vector of names
pt_data[c("temperature", "flu_status")]

# this is the same as above, extracting temperature and flu_status
pt_data[2:3]

# accessing by row and column
pt_data[1, 2]

# accessing several rows and several columns using vectors
pt_data[c(1, 3), c(2, 4)]

## Leave a row or column blank to extract all rows or columns

# column 1, all rows
pt_data[, 1]
# row 1, all columns
pt_data[1, ]
# all rows and all columns
pt_data[ , ]

# the following are equivalent
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5, -6)]

# creating a Celsius temperature column
pt_data$temp_c <- (pt_data$temperature - 32) * (5 / 9)

# comparing before and after
pt_data[c("temperature", "temp_c")]


##### Managing data with R ------------

## saving, loading, and removing R data structures

# show all data structures in memory
ls()

#drop all data structures in memory
rm(list=ls())

rm(blood)

######################################## Preparar datos y visualizarlos
### ¿Se legaliza la marihuana? Encuesta USA 2016
legalWeedAge <- read.csv(file = "legal_weed_age_GSS2016_ch1.csv")
str(legalWeedAge)
summary(legalWeedAge)
head(legalWeedAge)

#Usar la libería data.table para leer y cargar los datos del CSV.
#La notación :: abre el paquete, usa la función y cierra el paquete.
#Muchas veces no es necesario mantener el paquete abierto de inicio a fin
legalWeedAge <- data.table::fread(file = "legal_weed_age_GSS2016_ch1.csv")
str(legalWeedAge)
summary(legalWeedAge)
head(legalWeedAge)

#Convertir la variable a un factor
legalWeedAge$grass <- as.factor(legalWeedAge$grass)

#Recode 89 OR OLDER a 89
legalWeedAge$age[legalWeedAge$age == "89 OR OLDER"] <- "89"

#Cambiar el tipo de Age de char a int
legalWeedAge$age <- as.numeric(legalWeedAge$age)

# Usar tidyverse pipe para:
# 1. Cambiar grass como factor (categórico nominal)
# 3. Cambiar DK/IAP (Don´t Know/INAPLICABLE) por NA (NULL)
# Notar que es NA (todo en mayúscula). NA es palabra reservada
# La función mutate() es usada para recodicar variables
legalWeedAgeLimpio <- legalWeedAge %>%
                      mutate(grass = as.factor(grass)) %>%
                      mutate(grass = na_if(grass,"DK")) %>%
                      mutate(grass = na_if(grass,"IAP"))

#Obsevar como se mantienen los niveles aún
str(legalWeedAgeLimpio)
summary(legalWeedAgeLimpio)

#Para remover niveles luego de una transformación
#usar droplevels
legalWeedAgeLimpio <- legalWeedAge %>%
                      mutate(grass = as.factor(grass)) %>%
                      mutate(grass = na_if(grass,"DK")) %>%
                      mutate(grass = na_if(grass,"IAP")) %>%
                      mutate(grass = droplevels(grass))

str(legalWeedAgeLimpio)
summary(legalWeedAgeLimpio)

#Usar mutate() y el dataset en el pipe para hacer los cambios
#en las variables y las recodificaciones en una sola instrucción.
#Crear las categorías para la edad. Com sigue:
#18-29
#30-59
#60-74
#75+
legalWeedAge <- data.table::fread(file = "legal_weed_age_GSS2016_ch1.csv")

legalWeedAgeLimpio <- legalWeedAge %>%
                      mutate(age = recode(age, "89 OR OLDER" = "89")) %>%
                      mutate(age = as.numeric(age)) %>%
                      mutate(grass = as.factor(grass)) %>%
                      mutate(grass = na_if(grass,"DK")) %>%
                      mutate(grass = na_if(grass,"IAP")) %>%
                      mutate(grass = droplevels(grass)) %>%
                      mutate(ageCat = cut(age,
                                          breaks = c(-Inf, 29, 59, 74, Inf),
                                          labels = c("< 30", "30 - 59", "60 - 74", "75+" )
                                          )
                             )

str(legalWeedAgeLimpio)
summary(legalWeedAgeLimpio)

#Hacer un gráfico simple usando ggplot2
legalizeBar <- legalWeedAgeLimpio %>%
               ggplot(aes(x = grass)) +
               geom_bar()

legalizeBar

#Remover NA del gráfico
legalizeBar <- legalWeedAgeLimpio %>%
               drop_na(grass) %>%
               ggplot(aes(x = grass)) +
               geom_bar()

legalizeBar

#Usar fill para color de las barras
legalizeBar <- legalWeedAgeLimpio %>%
               drop_na(grass) %>%
               ggplot(aes(x = grass,fill = grass)) +
               geom_bar()

legalizeBar

#Cambiar colores y quitar información redundante
legalizeBar <- legalWeedAgeLimpio %>%
               drop_na(grass) %>%
               ggplot(aes(x = grass,fill = grass)) +
               geom_bar() +
               scale_fill_manual(values = c("#78A678", "#7463AC"),
                                 guide = FALSE)

legalizeBar

#Cambiar el background y agregar leyendas
legalizeBar <- legalWeedAgeLimpio %>%
               drop_na(grass) %>%
               ggplot(aes(x = grass,fill = grass)) +
               geom_bar() +
               scale_fill_manual(values = c("#78A678", "#7463AC"),
                                 guide = FALSE) +
               theme_minimal() +
               labs(x = "¿Debe legalizarse la marihuana?",
                    y = "Cantidad de Respuestas")

legalizeBar

#Mostrar el porcentaje en lugar de la frecuencia usando 
#variable especiales (special variables)
#..count.. = frecuencia de una categoría
#sum(..count..) = suma de todas las frecuencias
legalizeBar <- legalWeedAgeLimpio %>%
               drop_na(grass) %>%
               mutate(grass = recode_factor(grass,
                                            "LEGAL" = "SI",
                                            "NOT LEGAL" = "NO")
                      )%>%
               ggplot(aes(x = grass,
                          y = 100 * ((..count..)/(sum(..count..))),
                          fill = grass)) +
               geom_bar() +
               scale_fill_manual(values = c("#78A678", "#7463AC"),
                                 guide = FALSE) +
               theme_minimal() +
               labs(x = "¿Debe legalizarse la marihuana?",
                    y = "Porcentaje de respuestas")

legalizeBar

#Agregar la categoría de edad, hacer visible la leyenda
#crear el gráfico por ambas variables
#Note que todas las barras suman 100%
legalizeBar <- legalWeedAgeLimpio %>%
                drop_na(grass) %>%
                drop_na(age) %>%
                mutate(grass = recode_factor(grass,
                               "LEGAL" = "SI",
                               "NOT LEGAL" = "NO")
                       )%>%
                ggplot(aes(x = ageCat,
                           y = 100 * ((..count..)/(sum(..count..))),
                           fill = grass
                           )
                      ) +
                geom_bar(position = "dodge") +
                scale_fill_manual(values = c("#78A678", "#7463AC"),
                                  name = "¿Debe legalizarse\n la marihuana?") +
                theme_minimal() +
                labs(x = "Categoría de Edad",
                     y = "Porcentaje de respuestas")

legalizeBar

#El siguiente gráfico muestra por cada cateogoría la respuesta
# SI o NO al 100%
legalizeBar <- legalWeedAgeLimpio %>%
               drop_na(grass) %>%
               drop_na(age) %>%
               mutate(grass = recode_factor(grass,
                               "LEGAL" = "SI",
                               "NOT LEGAL" = "NO")
                      )%>%
               group_by(grass, ageCat) %>%
               count() %>%
               group_by(ageCat) %>%
               mutate(percGrass = 100*n/sum(n)) %>%
               ggplot(aes(x = ageCat,
                          fill = grass,
                          y = percGrass
                          )
                      ) +
               geom_col(position = "dodge") +
               scale_fill_manual(values = c("#78A678", "#7463AC"),
                                 name = "¿Debe legalizarse\n la marihuana?") +
               theme_minimal() +
               labs(x = "Categoría de Edad",
                    y = "Porcentaje respuestas en cada Categoría")

legalizeBar


