
install.packages("randomForest")
install.packages("dplyr")
install.packages("ggplo2")
install.packages("tidyverse")
install.packages("xlsx")

library(randomForest)
library(dplyr)
library(tidyverse)
library(xlsx)

rfNews()

if(!require(("randomForest")))
  install.packages("randomForest")

?randomForest

vignette(package = "randomForest")
vignette(package = "dplyr")
vignette(package = "dplyr", topic = "programming")

detach(name = package: randomForest)

dim(available.packages())

data()

data(package = .packages((all.avaible = TRUE)))

data(package = "dplyr")

data(cars)
data(iris)
data(AirPassengers)

x <- 28

class(x)
is.numeric(x)

y <- "String"
class(y)
is.character(y)

z <- TRUE
class(z)
is.logical(z)

i <- 21L
class(i)
is.numeric(i)
is.integer(i)

as.numeric("1.5")
as.integer("1.5")
as.character("1.5")





