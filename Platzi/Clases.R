
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

