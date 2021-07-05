# -------------------------------------------------------------------

                  #---- Depuración de los datos ----

# Cargamos el archivo orignal y lo exploramos
trips_data <- data.table::fread(file = "Uber/trips_data.csv")

summary(clean_data)


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

#Cantidad de viajes 
clean_data %>%
  ggplot(aes(x =ProductType, fill = ProductType))+
  geom_bar(fill="lightblue",color="black")+  
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Product Type", y = "Número de viajes")


#
ggplot(clean_data, aes(x = ProductType)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)),
           fill="lightblue",color="black")+
  scale_y_continuous(labels=scales::percent)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Product Type", y = "Porcentaje")
  
#

  
