
#------------------------------------------------------------
#                 Tipos de datos 

#Informaci�n sobre mtcars
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

#Declaraci�n vectores string
dias_aprendizaje <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
dias_aprendizaje

#Declraci�n vectores booleanos
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
     xlab="cilindros", ylab = "millas por gal�n",
     main="Relaci�n cilindros y millas por gal�n")

plot(mtcars$mpg ~ mtcars$hp,
     xlab="caballos de fuerza", ylab = "millas por gal�n",
     main="Relaci�n caballos de fuerza y millas por gal�n")

#EDA orangeec
plot(orangeec $Unemployment ~ orangeec$`Education invest % GDP`,
     xlab="Inversi�n educaci�n (%PIB)",
     ylab = "Desempleo",
     main= "Relaci�n inversi�n en educaci�n y desempleo")

plot(orangeec $`GDP PC` ~ orangeec$`Creat Ind % GDP`,
     xlab="Aporte econom�a naranja al PIB(%)",
     ylab = "PIB per c�pita",
     main= "Relaci�n econom�a naranja y PIB per c�pita")
#------------------------------------------------------------
#                 EDA histogramas

qplot(mtcars$hp,
      geom = "histogram",
      xlab = "caballos de fuerza",
      main="Carros seg�n caballos de fuerza")

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
  labs(x="PIB per c�pita", y ="Cantidad de paises",
       tittle="PIB per c�pita en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=orangeec,
                        aes(x=`Creat Ind % GDP`),fill="lightblue",color="black",
                        binwidth = 1)+#Cada barra es %1
  labs(x="Aporte econom�a naranja al PIB(%)", y="Cantidad de paises",
       tittle="Contribuci�n econom�a naranja al PIB en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=orangeec,
                        aes(x=`Internet penetration % population`),fill="lightblue",color="black",
                        binwidth = 5)+
  labs(x="Penetraci�n internet(%)poblaci�n", y="Cantidad de paises",
       tittle="Penetraci�n internet en paises latam")+
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
       tittle="Caballos de fuerza seg�n cilindros")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Vamos a cambiar una columna para desplegar un mejor resultado con las cajas

mtcars$am <- factor(mtcars$am, levels=c(1,0),
                    labels=c("Manual","Auntom�tico"))

ggplot(mtcars,aes(x=am,y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipo de caja",y="Millas por gal�n",
       tittle="Millas por gal�n seg�n tipo de caja")+
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
  labs(x="Tipo de pa�s", y="Aporte econom�a naranja al PIB",
       tittle="Aporte econom�a naranja en PiB paises latam con alto y bajo PIB")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#
ggplot(orangeec, aes(x=Strong_economy, y=Internet.penetration...population,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pa�s", y="Penetraci�n de internet(%)",
       tittle="Penetraci�n de Internet en paises latam con alto y bajo PIB")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#------------------------------------------------------------
#         Scatter plot

ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="caballos fuerza", y="millas por gal�n",
       tittle="Relaci�n caballos de fuerza y millas por gal�n")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#
ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs(x="peso", y="potencia",
       tittle="Relaci�n peso potencia")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#4 variables en un plot
ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am, size=cyl))+
  labs(x="caballos fuerza", y="timepo 1/4 de milla",
       tittle="Caballos-velocidad seg�n cilindraje y tipo de caja")

ggplot(orangeec, aes(`Internet penetration % population`,`Creat Ind % GDP`))+
  geom_point(aes(color=factor(Strong_economy), size=`GDP Growth %`))+
  labs(x="Penetraci�n Internet", y="Aporte econom�a naranja al PIB",
       tittle="Internet y aporte a la econom�a naranja seg�n econom�a y 
       crecimiento PIB")

install.packages("plotly")
#Plot interactivo

my_graph <- ggplot(orangeec, aes(`Internet penetration % population`,
                                 `Creat Ind % GDP`, label=row.names(orangeec)))+
  geom_point()+
  labs(x="Penetraci�n Internet", y="Aporte econom�a naranja",
       tittle="Penetraci�n internet y aporte econom�a naranja")

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

#Confirmar correlaciones con funci�n cor

# Si se acerca a cero no hay correlaci�n, si se acerca a 1 es positiva, si se acerca a -1 es negativa
cor(merc[,2:6])
#Si la correlaci�n es negativa significa que a m�s de algo, menos de lo otro
#Si es positiva significa que m�s de algo, m�s de lo otro
#
cor(newdata)



#Correlaciones econom�a naranja

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


# Coeficiente de variaci�n  (es optimo menor a 25%)
desv <- sd(mtcars$mpg)
prom <- mean(mtcars$mpg)

CoefVar <- (desv/prom)*100
Coefvar

#------------------------------------------------------------
#                   Eliminar NA de los c�lculos

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
# -------------------------------------------------------------------
              #     dplyr

# Ajustando datos para mejorar visualizaci�nes

eficientes <- mean(mtcars$mpg)
eficientes

#Agregamos una nueva columna que los categoriza por debajo o encima del promedio
mtcars <- mtcars %>%
  mutate(Mas_eficientes = ifelse(mpg <eficientes,
                                 "bajo promedio","en � sobre promedio"))

# Escojemos los carros que duran menos de 16 segundos en el cuarto de milla
Mas_veloces <- mtcars[mtcars$qsec<16,] 
Mas_veloces

#Nueva columna con los carros m�s veloces
mtcars <- mtcars %>%
  mutate(Velocidad_cuarto_milla = ifelse(qsec < 16, 
                                         "Menos 16 segs",
                                         "M�s de 15 segs"))

#
mtcars <- mtcars %>%
  mutate(Peso_kilos=(wt/2)*100)

mtcars <- mtcars %>%
  mutate(Peso = ifelse(Peso_kilos <= 1500,
                       "Livianos",
                       "Pesados"))

# Econom�a naranja

orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(GDP.Growth.. >= 2.5,
                                  "2.5% � m�s",
                                  "Menos de 2.5%"))

#
orangeec <- orangeec %>%
  mutate(Anaranjados = ifelse(Creat.Ind...GDP >= 2.5,
                              "M�s anaranjados",
                              "Menos anaranjados"))

# ranking
orangeec %>%
  arrange(desc(Creat.Ind...GDP))

TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico", "Panam�","Argentina",
                        "Colombia", "Brasil","Paraguay"))

TopNaranjas

#
TopNaranjas %>%
  arrange(desc(Creat.Ind...GDP))


# -------------------------------------------------------------------

          # Facet wrap

mtcars %>%
  arrange(desc(Peso_kilos))

Mas_pesados <- mtcars %>%
  filter(model %in% c("Lincoln Continental","Chrysler Imperial",
                      "Cadillac Fleetwood","Merc 450SE"))
Mas_pesados


ggplot(Mas_pesados, aes(x=hp, y=mpg))+
  geom_point()+
  facet_wrap(~model)# Despliega una gr�fica por cada modelo

#
ggplot(mtcars, aes(x=cyl, y=mpg, size=Peso_kilos))+
  geom_point()+
  facet_wrap(~ am)


# Pa�ses que m�s aportan al pib desde la EN
ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=Services...GDP, size=GDP.PC))+
  geom_point()+
  facet_wrap(~Country)

#
ggplot(TopNaranjas, aes(x=Education.invest.GDP,
                        y=Creat.Ind...GDP, size=Unemployment))+
  geom_point()+
  facet_wrap(~Country)

#   Colores para los gr�ficos
install.packages("RColorBrewer")
library(RColorBrewer)

#Instanciamos la paleta

myColors <- brewer.pal(9,"Reds")

ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=GDP.PC, fill=Creat.Ind..GDP))+
  geom_tile()+
  facet_wrap(~ Country)+
  scale_fill_gradientn(colors=myColors)

#
# -------------------------------------------------------------------
# R Markdown
install.packages("rmarkdown")
library(rmarkdown)

install.packages("knitr")
library(knitr)
