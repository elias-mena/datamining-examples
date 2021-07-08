# -------------------------------------------------------------------

#Librerias a utilizar
library(tidyverse) # Para manipular los datos
library(ggplot2)   # Para graficar
library(dplyr)     # Para manipular los datos
library(plotly)    # Para grÃ¡ficos interactivos
library(lubridate) # Para manejar las fechas

                  #---- DepuraciÃ³n de los datos ----

# Cargamos el archivo orignal y lo exploramos
trips_data <- data.table::fread(file = "Uber/trips_data.csv")

summary(trips_data)


# Filtramos los datos para eliminar los viajes cancelados
filter_data <- subset(trips_data, `Trip or Order Status` == "COMPLETED") 

# Transformamos las variables que vamos a analizar
filter_data <- filter_data %>% 
  mutate(`Product Type` = as.factor(`Product Type`)) %>% 
  mutate(`Trip or Order Status` = as.factor(`Trip or Order Status`))%>% 
  #mutate(`Distance (miles)` = as.numeric(`Distance (miles)`)) %>%
  mutate(`Fare Amount` = as.numeric(`Fare Amount`)) %>%
  mutate(`Fare Currency` = as.factor(`Fare Currency`)) %>%
  mutate(`Request Time` = as.Date(`Request Time`))

# Seleccionamos y creamos las columnas del data_frame para el estudio

ProductType <- filter_data$`Product Type`

OrderStatus <- filter_data$`Trip or Order Status`

#Distance <- filter_data$`Distance (miles)`

FareAmount <- filter_data$`Fare Amount`

FareCurrency <- filter_data$`Fare Currency`

# Fechas
RequestTimeDate <- filter_data$`Request Time`

MonthName <- months(RequestTimeDate)

# Convertimos los meses a factor y ordenamos los niveles
MonthName <- sort(factor(MonthName, levels = month.name))


DayOfWeek <- weekdays(RequestTimeDate)

WeekOfYear <- paste(year(RequestTimeDate),week(RequestTimeDate),sep="-")

# Creamos el data_frame
clean_data <- data.frame(ProductType = ProductType, OrderStatus = OrderStatus,
                         RequestTimeDate = RequestTimeDate, 
                         #Distance = Distance,
                         FareAmount = FareAmount, FareCurrency = FareCurrency,
                         MonthName = MonthName, DayOfWeek = DayOfWeek,
                         WeekOfYear = WeekOfYear)


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

# Creamos esta variable para calcular los gastos mensuales
gasto_mensual <- clean_data %>% 
  group_by(MonthName) %>%
  dplyr::summarize( sum(FareAmount))


# GrÃ¡fico línea de tendencia
ggplotly(ggplot(gasto_mensual, aes(x=MonthName, y=`sum(FareAmount)`, group=1)) +
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

# Frecuencia de viajes segÃºn el mes
ggplot(clean_data, aes(x = MonthName, fill = MonthName)) +
  geom_bar(width = 1, color="black")+ coord_polar()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Day of Week", y = "Frecuency")

# -------------------------------------------------------------------

    # Analisis estadístico 

# Punto 1

# FunciÃ³n para crear las tablas de frecuencia
TableCreation <- function(y){
  data.frame(table(y),
             round(x = prop.table(x = table(y)) * 100, 1))
}

# Viendo un resumen de los datos vemos que el rango estÃ¡ entre
# 900 y 4850, por lo que vamos a hacer 4 grupos entre ese rango 
summary(clean_data$FareAmount)

FareAmountGroups <- cut(clean_data$FareAmount,
                        breaks = c(-Inf, 949, 1499, 2499, 3999,Inf),
      labels = c("< 950", "950 - 1499", "1500 - 2499","2500-3999","4000+" ))

tabla <- as.data.frame( TableCreation(FareAmountGroups))

Rango <- tabla$y

Frequency <- tabla$Freq

Percent <- tabla$Freq.1

tablaFrecuencia <- data.frame(Rango, Frequency, Percent)

tablaFrecuencia

# Punto 2

# 2.a
ggplotly( ggplot(clean_data, aes(x = FareAmount)) + 
  geom_histogram(fill="lightblue",color="black")+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(x = "Fare", y = "Frecuency"))
  

# 2.c
qqnorm(clean_data$FareAmount)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
              
# 2.d
ggplotly( ggplot(clean_data, aes(x = FareAmount)) + 
            geom_histogram(aes(y = ..density..),
                           fill="lightblue",color="black")+
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())+
            labs(x = "Tarifa", y = "Densidad")+
            geom_density(alpha=.2,fill="red"))

# Punto 3

# 3.a

# Obtenemos media y las medidas de disperciÃ³n

mediaTarifa <- mean(clean_data$FareAmount)

VarianzaTarifa <- var(clean_data$FareAmount)
# Po

desvEstTarifa <-sd(clean_data$FareAmount)

Coef <- (desvEstFA/mediaFA)*100


# 3.b
summary(clean_data$FareAmount)
library(psych)

kurtosi(clean_data$FareAmount)/sqrt(6/23) 


# 3.c

# Punto 4

# Punto 5

# Creamos esta variable para calcular nuestros gastos semanales

gasto_semanal <- clean_data %>% 
  group_by(WeekOfYear) %>%
  dplyr::summarize( sum(FareAmount))


summary(gasto_semanal$`sum(FareAmount)`)

# Punto 6

summary(gasto_mensual$`sum(FareAmount)`)

ggplot(gasto_mensual, aes(x=`sum(FareAmount)`))+
  geom_histogram()+
  labs(x="Caballos de fuerza", y ="Cantidad de carros",
       tittle="Caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Punto 7

mode <- function(x) {
  return((names(which.max(table(x)))))
}

mode(clean_data$ProductType)

mode(clean_data$FareAmount)

summary(clean_data$FareAmount)

