# -------------------------------------------------------------------

#Librerias a utilizar
library(tidyverse) # Para manipular los datos
library(ggplot2)   # Para graficar
library(dplyr)     # Para manipular los datos
library(plotly)    # Para gráficos interactivos
library(lubridate) # Para manejar las fechas

                  #---- Depuración de los datos ----

# Cargamos el archivo orignal y lo exploramos
trips_data <- data.table::fread(file = "Uber/trips_data.csv")

summary(trips_data)


# Filtramos los datos para eliminar los viajes cancelados
filter_data <- subset(trips_data, `Trip or Order Status` == "COMPLETED") 

# Transformamos las variables que vamos a analizar
filter_data <- filter_data %>% 
  mutate(`Product Type` = as.factor(`Product Type`)) %>% 
  mutate(`Trip or Order Status` = as.factor(`Trip or Order Status`))%>% 
  mutate(`Distance (miles)` = as.numeric(`Distance (miles)`)) %>%
  mutate(`Fare Amount` = as.numeric(`Fare Amount`)) %>%
  mutate(`Fare Currency` = as.factor(`Fare Currency`)) %>%
  mutate(`Request Time` = as.Date(`Request Time`))

# Seleccionamos y creamos las columnas del data_frame para el estudio

ProductType <- filter_data$`Product Type`

OrderStatus <- filter_data$`Trip or Order Status`

Distance <- filter_data$`Distance (miles)`

FareAmount <- filter_data$`Fare Amount`

FareCurrency <- filter_data$`Fare Currency`

# Fechas
RequestTimeDate <- filter_data$`Request Time`

MonthName <- months(RequestTimeDate)

DayOfWeek <- weekdays(RequestTimeDate)

WeekOfYear <- paste(year(RequestTimeDate),week(RequestTimeDate),sep="-")

# Creamos el data_frame
clean_data <- data.frame(ProductType = ProductType, OrderStatus = OrderStatus,
                         RequestTimeDate = RequestTimeDate, Distance = Distance,
                         FareAmount = FareAmount, FareCurrency = FareCurrency,
                         MonthName = MonthName, DayOfWeek = DayOfWeek,
                         WeekOfYear = WeekOfYear)

# Exploramos el data_frame limpio
str(clean_data)
summary(clean_data)
head(clean_data)

# -------------------------------------------------------------------

                          #---- EDA ----

# Cantidad de viajes relacionada con el tipo de producto
ggplotly(
  ggplot(clean_data,aes(x =ProductType, fill = ProductType))+
  geom_bar(fill="lightblue",color="black")+  
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Product Type", y = "Trips"))


# Porcentaje de viajes según el tipo de producto
ggplot(clean_data, aes(x = ProductType)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)),
           fill="lightblue",color="black")+
  scale_y_continuous(labels=scales::percent)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Product Type", y = "Percent")
  
# Gráfico línea de tendencia
ggplotly(ggplot(data=clean_data, aes(x=MonthName, y=FareAmount, group=1)) +
  geom_path()+
  geom_point()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Month", y = "Expense"))


# Frecuencia de viajes según el día de la semana
ggplot(clean_data, aes(x = DayOfWeek, fill = DayOfWeek)) +
  geom_bar(width = 1, color="black")+ coord_polar()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Day of Week", y = "Frecuency")

# Frecuencia de viajes según el mes
ggplot(clean_data, aes(x = MonthName, fill = MonthName)) +
  geom_bar(width = 1, color="black")+ coord_polar()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Day of Week", y = "Frecuency")

# -------------------------------------------------------------------

    # Analisis estadístico 

# Punto 1

# Función para crear las tablas de frecuencia
TableCreation <- function(y){
  data.frame(table(y),
             round(x = prop.table(x = table(y)) * 100, 1))
}

TableCreation(clean_data$FareAmount)

# Viendo un resumen de los datos vemos que el rango está entre
# 900 y 4850, por lo que vamos a hacer 4 grupos entre ese rango 
summary(clean_data$FareAmount)

FareAmountGroups <- cut(clean_data$FareAmount,breaks = c(-Inf, 949, 1499, 2499, 3999,Inf),
      labels = c("< 950", "950 - 1499", "1500 - 2499","2500-3999","4000+" ))
FareAmountGroups

TableCreation(FareAmountGroups)

# Punto 2

# 2.a
ggplotly( ggplot(clean_data, aes(x = FareAmount)) + 
  geom_histogram(fill="lightblue",color="black")+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(x = "Fare", y = "Frecuency"))
  

# 2.c
qqnorm(clean_data$FareAmount)
              
# 2.d
ggplotly( ggplot(clean_data, aes(x = FareAmount)) + 
            geom_histogram(aes(y = ..density..),
                           fill="lightblue",color="black")+
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())+
            labs(x = "Tarifa", y = "Densidad")+
            geom_density())

# Punto 3

# Función para calcular la desviación estandar
STDEVP <- function(x) {
  n <- length(x)
  sd(x,na.rm=TRUE) * (n-1) / n
} 

# 3.a

# Obtenemos media y las medidas de disperción

mediaFA <- mean(clean_data$FareAmount)
VarianzaFA <- var(clean_data$FareAmount)

desvEstFA <-sd(clean_data$FareAmount)

Coef <- (desvEstFA/mediaFA)*100
Coef

STDEVP(clean_data$FareAmount)

# 3.b
summary(clean_data$FareAmount)

clean_data$FareAmount

# 3.c

# Punto 4

