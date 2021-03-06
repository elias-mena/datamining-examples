install.packages("modeest")
install.packages("psych")
install.packages("odbc")
install.packages("FinCal")
install.packages("fdth")
install.packages("gmodels")
install.packages("caret")
install.packages("scatterplot3d")
install.packages("gridExtra")
install.packages("semTools")
install.packages("descr")

library(odbc)
library(psych)
library(modeest)
library(FinCal)
library(fdth)
library(gmodels)
library(caret)
library(scatterplot3d)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)

STDEVP <- function(x) {
  n <- length(x)
  sd(x,na.rm=TRUE) * (n-1) / n
} 

TableCreation <- function(y){
  data.frame(table(y),
             round(x = prop.table(x = table(y)) * 100, 1))
}

con <- dbConnect(odbc(),
                 Driver = "SQL Server Native Client 11.0",
                 Server = "DESKTOP-F1PQK8T\\MSSQLDEV2019",
                 Database = "AdventureWorks2019",
                 UID = "TestLogin",
                 PWD = "test1234",
                 Port = 1433)

#Extraer toda la informaci�n de los empleados
DF <- dbSendQuery(con,
                  "SELECT *
                   FROM HumanResources.v_Employee;")

OD_Fetch <- dbFetch(DF)

#Data Management
OD_Fetch <- OD_Fetch %>%
  mutate(MaritalStatus = as.factor(MaritalStatus)) %>% 
  mutate(Gender = as.factor(Gender)) %>%
  mutate(SalariedFlag = as.factor(SalariedFlag)) %>%
  mutate(CurrentFlag = as.factor(CurrentFlag)) %>%
  mutate(AgeBucket = as.factor(AgeBucket)) %>%
  mutate(YearsWorkingBucket = factor(YearsWorkingBucket,
                                     labels = c("8-10","11-13","14-16"))) %>%
  mutate(VacationHoursBucket = as.factor(VacationHoursBucket)) %>%
  mutate(SickLeaveHoursBucket = as.factor(SickLeaveHoursBucket)) %>%
  mutate(JobTitle = as.factor(JobTitle))


#Summary, describe pueden actuar sobre una variable o sobre un objeto
summary(OD_Fetch)
psych::describe(OD_Fetch) #psych
?describe

#Summary, describe pueden aceptar vector con variables de inter�s
summary(OD_Fetch[c("VacationHours", "SickLeaveHours")])
psych::describe(OD_Fetch[c("VacationHours", "SickLeaveHours")]) #psych

#Usar esta notaci�n en lugar de la de arriba
OD_Fetch %>%
  select(VacationHours,SickLeaveHours) %>%
  summary() 

OD_Fetch %>%
  select(VacationHours,SickLeaveHours) %>%
  psych::describe()

##################################### Distribuci�n de frecuencias

TableCreation (OD_Fetch$JobTitle)[-3]
TableCreation (OD_Fetch$MaritalStatus)[-3]
TableCreation (OD_Fetch$Gender)[-3]
TableCreation (OD_Fetch$SalariedFlag)[-3]
TableCreation (OD_Fetch$CurrentFlag)[-3]
TableCreation (OD_Fetch$AgeBucket)[-3]
TableCreation (OD_Fetch$YearsWorkingBucket)[-3]
TableCreation (OD_Fetch$VacationHoursBucket)[-3]
TableCreation (OD_Fetch$SickLeaveHoursBucket)[-3]

## Tabla de frecuencias
#Scott,FD
dist <- fdt(OD_Fetch$Age,
            breaks="Scott")
dist
#Donde
#f= frecuencia absoluta
#rf= frecuencia relativa
#rf(%) frecuencia relativa porcentual
#cf= frecuencia acumulada
#cf(%)=frecuencia acumulada porcentual

#�Por qu� las variables continuas no se describen bien con una
#distribuci�n de frecuencias?
OD_Fetch %>%
  select (Age) %>%
  table() %>%
  barplot()

#Usar el paquete descr
descr::freq(OD_Fetch$Age,
            plot = TRUE)

#Tabla de frecuencias personalizada,
#rango diferente (start, end) con amplitud definida (h), 
#utilizamos lo siguiente
dist <- fdt(OD_Fetch$Age,
            breaks="Sturges",
            start=30, 
            end=71, 
            h=5)
dist

#Hay una funci�n escrita en R que estima el numero de clases
nclass.Sturges(OD_Fetch$Age)

#Tabla de frecuencias personalizada,
#utilizamos k para determinar el numero de clases deseado
dist <- fdt(OD_Fetch$Age,
            breaks="Sturges",
            k=nclass.Sturges(OD_Fetch$Age))
dist

##################################### Tendencia Central con Edad
mean(OD_Fetch$Age,na.rm=TRUE) #Media
median(OD_Fetch$Age,na.rm=TRUE) #Mediana

#Moda
mfv(OD_Fetch$Age) #modeest

OD_Fetch %>%
  select (Age) %>%
  summarize(mean = mean(Age, na.rm=TRUE),
            median = median(Age,na.rm=TRUE),
            mode = mlv(Age, method='mfv')
            )

##################################### Misma media y �la STDEV?
ds1 <- c(199,200,201)
ds2 <- c(0,200,400)

summary(ds1)
sd(ds1)

summary(ds2)
sd(ds2)

##################################### Dispersi�n o variabilidad con Edad
range(OD_Fetch$Age,na.rm=TRUE)
max(OD_Fetch$Age) - min(OD_Fetch$Age)
var(OD_Fetch$Age,na.rm=TRUE)
sd(OD_Fetch$Age,na.rm=TRUE)

STDEVP(OD_Fetch$Age) #Poblaci�n

100 * (sd(OD_Fetch$Age,na.rm=TRUE)/mean(OD_Fetch$Age,na.rm=TRUE)) #CV

coefficient.variation(sd=sd(OD_Fetch$Age), 
                      avg = mean(OD_Fetch$Age)) * 100 #FinCal

OD_Fetch %>%
  select (Age) %>%
  summarize(mean = mean(Age, na.rm=TRUE),
            median = median(Age,na.rm=TRUE),
            mode = mlv(Age, method='mfv'),
            max = max(Age),
            min = min(Age),
            range = max(Age) - min(Age),
            sd = sd(Age,na.rm=TRUE),
            sqp = STDEVP(Age),
            CV = 100 * ((STDEVP(Age))/mean(Age,na.rm=TRUE)),
            CVFinCal = coefficient.variation(sd=sd(Age), 
                                             avg = mean(Age)) * 100
            )

#### Medidas de distribuci�n con Edad

#�Qu� forma tienen los datos num�ricos?
hist(OD_Fetch$Age)

#Curtosis (Normalidad, no asimetr�a)
kurtosi(OD_Fetch$Age,na.rm=TRUE)
semTools::kurtosis(OD_Fetch$Age)

#skewness (asimetr�a)
#skew type = 1, Fisher
psych::skew(OD_Fetch$Age,na.rm=TRUE) #psych

#skewness (asimetr�a) usando seemTools
#se = standard error
#z = skew/se
semTools::skew(OD_Fetch$Age)

psych::describe(OD_Fetch$Age,na.rm=TRUE) #psych

hist(OD_Fetch$Age)

IQR(OD_Fetch$Age)

# M�s par�metros de personalizaci�n
hist(OD_Fetch$Age,
     main = "Histograma de frecuencias",
     xlab = "Edad (a�os cumplidos)",
     ylab = "Frecuencia",
     col = "red",
     border = "black",
     xlim = c(30, 70),
     ylim = c(0, 80))

# Histograma de densidad, agregar curva de asimetr�a
hist(OD_Fetch$Age,
     freq = F,
     xlab = "Age",
     main = "")

lines(density(OD_Fetch$Age))

# Histograma de densidad, agregar curva de asimetr�a
# Comparaci�n con curva normal (mismo gr�fico)
#par(mar = c(1, 1, 1, 1)) 
hist(OD_Fetch$Age,
     freq = F,
     xlab = "Age",
     main = "")

dz <- density(OD_Fetch$Age)
lines(dz, col = "red", lwd = 3)

curve(dnorm(x, 
            mean(OD_Fetch$Age), 
            sd(OD_Fetch$Age)),
      col = "blue", 
      lwd = 3, 
      add = TRUE)
# R dibuja una curva de densidad normal (dnorm) para
# los valores del eje x que tienen una media
# mean(OD_Fetch$Age) y una desviaci�n est�ndar
# sd(OD_Fetch$Age).


# Normalidad usando Quantile-Quantile Plot, conocido como q-q plot
qqnorm(OD_Fetch$Age)
qqline(OD_Fetch$Age)


##################################### An�lisis Bidimensional 
## Tablas de contigencia

#Usar freq() del paquete descr (short for descriptives)
descr::freq(OD_Fetch$AgeBucket,
            plot = TRUE)

TableCreation (OD_Fetch$Gender)[-3]
TableCreation (OD_Fetch$AgeBucket)[-3]


table(AgeBucket = OD_Fetch$AgeBucket,
      Gender = OD_Fetch$Gender
)

barplot(table(OD_Fetch$Gender))

barplot(table(OD_Fetch$AgeBucket),
        col= "blue")

#library gmodels
CrossTable(x=OD_Fetch$AgeBucket,
           y=OD_Fetch$Gender,
           prop.chisq = FALSE)

CrossTable(x=OD_Fetch$AgeBucket, 
           y=OD_Fetch$Gender,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Age Bucket', 'Gender'))

########################################### Ejemplo Curso1

setwd("C:/Luis Diego/DataMining/Statistical Analisys")
getwd()

Curso1 <- read.csv(file = 'Curso1.txt', 
                   stringsAsFactors = FALSE,
                   sep = ',')

str(Curso1)

### Notas
psych::describe(Curso1$Nota)
mfv(Curso1$Nota) #modeest

hist(Curso1$Nota, 
     main = "Hist notas del Curso1",
     xlab = "Notas")

# Histograma de densidad, agregar curva de asimetr�a
hist(Curso1$Nota,
     freq = F,
     xlab = "Nota",
     main = "Notas del Curso 1")

dz <- density(Curso1$Nota)
lines(dz, col = "red", lwd = 3)

curve(dnorm(x, 
            mean(Curso1$Nota), 
            sd(Curso1$Nota)),
      col = "blue", 
      lwd = 3, 
      add = TRUE)

qqnorm(Curso1$Nota)
qqline(Curso1$Nota)

kurtosi(Curso1$Nota,na.rm=TRUE)
semTools::kurtosis(Curso1$Nota)
psych::skew(Curso1$Nota,na.rm=TRUE) #psych
semTools::skew(Curso1$Nota)

IQR(Curso1$Nota)

###Edades
psych::describe(Curso1$Edad)
mfv(Curso1$Edad) #modeest

hist(Curso1$Edad, 
     main = "Hist notas del Curso1",
     xlab = "Edad")

# Histograma de densidad, agregar curva de asimetr�a
hist(Curso1$Edad,
     freq = F,
     xlab = "Edad",
     main = "Edad Estudiantes")

dz <- density(Curso1$Edad)
lines(dz, col = "red", lwd = 3)

curve(dnorm(x, 
            mean(Curso1$Edad), 
            sd(Curso1$Edad)),
      col = "blue", 
      lwd = 3, 
      add = TRUE)

qqnorm(Curso1$Edad)
qqline(Curso1$Edad)

##################################### Ejemplo Curso2
Curso2 <- read.csv(file = 'Curso2.txt', 
                   stringsAsFactors = FALSE,
                   sep = ',')

###Notas
describe(Curso2$Nota)
mfv(Curso2$Nota) #modeest

hist(Curso2$Nota, 
     main = "Notas del Curso 2",
     xlab = "Notas")

# Histograma de densidad, agregar curva de asimetr�a
hist(Curso2$Nota,
     freq = F,
     xlab = "Notas",
     main = "Notas del Curso 2")

dz <- density(Curso2$Nota)
lines(dz, col = "red", lwd = 3)

curve(dnorm(x, 
            mean(Curso2$Nota), 
            sd(Curso2$Nota)),
      col = "blue", 
      lwd = 3, 
      add = TRUE)

qqnorm(Curso2$Nota)
qqline(Curso2$Nota)

###Edades
describe(Curso2$Edad)
mfv(Curso2$Edad) #modeest

hist(Curso2$Edad, 
     main = "Edades del Curso 2",
     xlab = "Edad")

# Histograma de densidad, agregar curva de asimetr�a
hist(Curso2$Edad,
     freq = F,
     xlab = "Edad",
     main = "Edades del Curso 2")

dz <- density(Curso2$Edad)
lines(dz, col = "red", lwd = 3)

curve(dnorm(x, 
            mean(Curso2$Edad), 
            sd(Curso2$Edad)),
      col = "blue", 
      lwd = 3, 
      add = TRUE)

qqnorm(Curso2$Nota)
qqline(Curso2$Nota)

