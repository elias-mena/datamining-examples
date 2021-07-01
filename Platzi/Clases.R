
#------------------------------------------------------------
#                 Tipos de datos 

#Información sobre mtcars
?mtcars
#Clase de la variable vs
class(mtcars$vs)

#as.logical(dataset$variable) cambia el tipo de dato de un dataset a logical
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)
class(mtcars$vs)
class(mtcars$am)

#------------------------------------------------------------
#               Summary, Transform

# str() muestra la estructura del dataset
str(orangeec)
?orangeec

#vsummary() muestra el resumen del dataset
summary(orangeec)

summary(mtcars)

wt <- (mtcars$wt*1000)/2#De libras a kilos
wt

# transform(): permite modificar los valores de un dataset
mtcars.new <- transform(mtcars, wt=wt*1000/2)# Transformamos el peso a kilos
summary(mtcars.new)

#------------------------------------------------------------
#                         Vectores

#Declaracion y suma de vectores numericos
tiempo_platzi <- c(25, 5, 10, 15, 10)
tiempo_lectura <- c(30, 10, 5, 10, 15)
tiempo_aprendizaje <- tiempo_platzi + tiempo_lectura
tiempo_aprendizaje

#Declaración vectores string
dias_aprendizaje <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
dias_aprendizaje

#Declración vectores booleanos
dias_mas_20min <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
dias_mas_20min

#Suma de vectores
total_tiempo_platzi <- sum(tiempo_platzi)
total_tiempo_platzi
total_tiempo_lectura <- sum(tiempo_lectura)
total_tiempo_lectura
total_tiempo_adicional <- total_tiempo_platzi + total_tiempo_lectura
total_tiempo_adicional

#------------------------------------------------------------
#                     Matrices

#Definir una matriz 
tiempo_matriz <- matrix(c(tiempo_platzi, tiempo_lectura), nrow = 2,byrow=TRUE)
#Asignar etiquetas a las filas y columnas
dias <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
tiempo <- c("tiempo platzi", "tiempo lecturas")
colnames(tiempo_matriz) <- dias
rownames(tiempo_matriz) <- tiempo
tiempo_matriz
#Sumar matriz columan por columna
colSums(tiempo_matriz)


#------------------------------------------------------------
            #EDA Scatter plot mtcars
plot(mtcars$mpg ~ mtcars$cyl,
     xlab="cilindros", ylab = "millas por galón",
     main="Relación cilindros y millas por galón")

plot(mtcars$mpg ~ mtcars$hp,
     xlab="caballos de fuerza", ylab = "millas por galón",
     main="Relación caballos de fuerza y millas por galón")

#EDA orangeec
plot(orangeec $Unemployment ~ orangeec$`Education invest % GDP`,
     xlab="Inversión educación (%PIB)",
     ylab = "Desempleo",
     main= "Relación inversión en educación y desempleo")

plot(orangeec $`GDP PC` ~ orangeec$`Creat Ind % GDP`,
     xlab="Aporte economía naranja al PIB(%)",
     ylab = "PIB per cápita",
     main= "Relación economía naranja y PIB per cápita")
#------------------------------------------------------------
#                 EDA histogramas

qplot(mtcars$hp,
      geom = "histogram",
      xlab = "caballos de fuerza",
      main="Carros según caballos de fuerza")

ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Caballos de fuerza", y ="Cantidad de carros",
       tittle="Caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)
  labs(x="Caballos de fuerza", y ="Cantidad de carros",
       tittle="Caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
ggplot()+geom_histogram(data=mtcars,
                        aes(x=hp),fill="lightblue",color="black",
                        binwidth = 20)+
  labs(x="Caballos de fuerza", y ="Cantidad de carros",
       tittle="Caballos de fuerza en carros seleccionados")+
  xlim(c(80,280))+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#histograma orangeec

ggplot()+geom_histogram(data=orangeec,
                        aes(x=`GDP PC`),fill="lightblue",color="black",
                        binwidth = 2000)+
  labs(x="PIB per cápita", y ="Cantidad de paises",
       tittle="PIB per cápita en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=orangeec,
                        aes(x=`Creat Ind % GDP`),fill="lightblue",color="black",
                        binwidth = 1)+#Cada barra es %1
  labs(x="Aporte economía naranja al PIB(%)", y="Cantidad de paises",
       tittle="Contribución economía naranja al PIB en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=orangeec,
                        aes(x=`Internet penetration % population`),fill="lightblue",color="black",
                        binwidth = 5)+
  labs(x="Penetración internet(%)población", y="Cantidad de paises",
       tittle="Penetración internet en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#------------------------------------------------------------
#                       EDA Bocplot
#Los boxplot los debemos hacer con una varibale categorica en x y numerica en y

boxplot(mtcars$hp,
        ylab="Caballos de fuerza",
        main="Caballos de fuerza en carros mtcars")

#ggplot
ggplot(mtcars, aes(x=as.factor(cyl),y=hp,fill=cyl))+
  geom_boxplot(alpha=0.6)+
  labs(x="Cilindros", y ="Caballos de fuerza",
       tittle="Caballos de fuerza según cilindros")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Vamos a cambiar una columna para desplegar un mejor resultado con las cajas

mtcars$am <- factor(mtcars$am, levels=c(1,0),
                    labels=c("Manual","Auntomático"))

ggplot(mtcars,aes(x=am,y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipo de caja",y="Millas por galón",
       tittle="Millas por galón según tipo de caja")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#------------------------------------------------------------

economy <- mean(orangeec$`GDP PC`)
economy


#Ejercicios con dyplyr

#
orangeec <- orangeec %>%
  mutate(Strong_economy = ifelse(`GDP PC` < economy,"PIB por debajo del promedio",
                                 "PIB arriba del promedio"))

#
ggplot(orangeec, aes(x=Strong_economy, y=`Creat Ind % GDP`,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de país", y="Aporte economía naranja al PIB",
       tittle="Aporte economía naranja en PiB paises latam con alto y bajo PIB")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#
ggplot(orangeec, aes(x=Strong_economy, y=Internet.penetration...population,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de país", y="Penetración de internet(%)",
       tittle="Penetración de Internet en paises latam con alto y bajo PIB")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#------------------------------------------------------------
#         Scatter plot

ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="caballos fuerza", y="millas por galón",
       tittle="Relación caballos de fuerza y millas por galón")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#
ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs(x="peso", y="potencia",
       tittle="Relación peso potencia")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#4 variables en un plot
ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am, size=cyl))+
  labs(x="caballos fuerza", y="timepo 1/4 de milla",
       tittle="Caballos-velocidad según cilindraje y tipo de caja")

ggplot(orangeec, aes(`Internet penetration % population`,`Creat Ind % GDP`))+
  geom_point(aes(color=factor(Strong_economy), size=`GDP Growth %`))+
  labs(x="Penetración Internet", y="Aporte economía naranja al PIB",
       tittle="Internet y aporte a la economía naranja según economía y 
       crecimiento PIB")

install.packages("plotly")
#Plot interactivo

my_graph <- ggplot(orangeec, aes(`Internet penetration % population`,
                                 `Creat Ind % GDP`, label=row.names(orangeec)))+
  geom_point()+
  labs(x="Penetración Internet", y="Aporte economía naranja",
       tittle="Penetración internet y aporte economía naranja")

p = ggplotly(my_graph)
p

#------------------------------------------------------------
              #Correlaciones con pairs

pairs(mtcars[,2:6])

newdata <- subset(mtcars,select=c(2,7:8,11,12))
pairs(newdata)

Eficientes <- filter(mtcars,mpg >= 30)
Eficientes

pairs(Eficientes[,2:6])

install.packages("stringr")

#Carros que empiezan con merc
merc <- mtcars %>%
  filter(str_detect(model,"Merc"))
merc
#
pairs(merc[,2:6])

#Confirmar correlaciones con función cor

# Si se acerca a cero no hay correlación, si se acerca a 1 es positiva, si se acerca a -1 es negativa
cor(merc[,2:6])
#Si la correlación es negativa significa que a más de algo, menos de lo otro
#Si es positiva significa que más de algo, más de lo otro
#
cor(newdata)



#Correlaciones economía naranja

pairs(orangeec[,2:6])

#
pairs(orangeec[,5:10])

#
newdata <- subset(orangeec,select=c(5,6,10,11,12,13))
newdata

pairs(newdata)

#
cor(orangeec[,2:6])

#Quitar las variables NA
cor(orangeec[,2:6],use="complete.obs")

#
cor(orangeec[,5:10],use="complete.obs")

cor(newdata, use="complete.obs")

#------------------------------------------------------------

summary(mtcars)

#
sd(mtcars$mpg)
mean(mtcars$mpg)


# Coeficiente de variación  (es optimo menor a 25%)
desv <- sd(mtcars$mpg)
prom <- mean(mtcars$mpg)

CoefVar <- (desv/prom)*100
Coefvar

#------------------------------------------------------------
#                   Eliminar NA de los cálculos

summary(orangeec)

desv <- sd(orangeec$`Internet penetration % population`)# Standard deviation
prom <- mean(orangeec$`Internet penetration % population`) #Promedio

CoefVar <- (desv/prom)*100
CoefVar

#
mean(orangeec$`Creat Ind % GDP`)#Aparece NA
mean(orangeec$`Creat Ind % GDP`,na.rm=TRUE)#Remueve los NA

prom <- mean(orangeec$`Creat Ind % GDP`,na.rm=TRUE)
desv <- sd(orangeec$`Creat Ind % GDP`,na.rm=TRUE)

CoefVar <- (desv/prom)*100
CoefVar


