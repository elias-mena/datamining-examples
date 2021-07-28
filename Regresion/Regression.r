install.packages("lmtest")

library(tidyverse)
library(psych)
library(lmtest)

## Análisis de Correlaciones (Coeficientes de correlación Pearson & Spearman)
## World Health Organization (WHO) & UNESCO Institute for Statistics 
setwd("C:/Luis Diego/DataMining/Regresion Methods")

water.educ <- read.csv(file = "water_educ_who_unesco.csv")
#country = el nombre del pais
#perc.1dollar = porcentaje de cuidadanos viviendo con un 1$ o menos al día
#med.age = la edad media los cuidados en ese país
#perc.basic2015sani = porcentaje de cuidadanos con acceso de salud básico
#perc.safe2015 = porcentaje de cuidadanos con acceso de salud seguro
#perc.basic2015water = porcentaje de cuidadanos con acceso básico al agua
#perc.safe2015water = porcentaje de cuidadanos con acceso seguro al agua
#perc.in.school = porcentaje de cuidadanos en primaria y secundaria
#female.in.school = porcentaje de mujeres en primaria y secundaria
#male.in.school = porcentaje de hombres en primaria y secundaria

summary(water.educ )

#female data
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  summarize(meanFemaleInSchool = mean(x = female.in.school),
            stdevFemaleInSchool = sd(x = female.in.school),
            meanBasicWater = mean(x = perc.basic2015water),
            stdevBasicWater = sd(x = perc.basic2015water))

hist(water.educ$female.in.school)
hist(water.educ$perc.basic2015water)

#male data
water.educ %>%
  drop_na(male.in.school) %>%
  drop_na(perc.basic2015water) %>%
  summarize(meanMaleInSchool = mean(x = male.in.school),
            stdevMaleInSchool = sd(x = male.in.school),
            meanBasicWater = mean(x = perc.basic2015water),
            stdevBasicWater = sd(x = perc.basic2015water))

hist(water.educ$male.in.school)

#Scatterplot para examinar la relación entre dos variables
#numéricas
#Female vs basic water
water.educ %>%
  ggplot(aes(y = female.in.school/100, 
             x = perc.basic2015water/100)) +
  geom_point(aes(color = "Country"), 
             size = 2, 
             alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percent with basic water access") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

#Male vs basic water
water.educ %>%
  ggplot(aes(y = male.in.school/100, 
             x = perc.basic2015water/100)) +
  geom_point(aes(color = "Country"), 
             size = 2, 
             alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of males in school",
       x = "Percent with basic water access") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

#Female vs poverty
water.educ %>%
  ggplot(aes(y = female.in.school/100, 
             x = perc.1dollar/100)) +
  geom_point(aes(color = "Country"), 
             size = 2, 
             alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percent living on less $1 per day") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

#Male vs poverty
water.educ %>%
  ggplot(aes(y = male.in.school/100, 
             x = perc.1dollar/100)) +
  geom_point(aes(color = "Country"), 
             size = 2, 
             alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of males in school",
       x = "Percent living on less $1 per day") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

# covariance of females in school, poverty, and
# percentage with basic access to drinking water
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  drop_na(perc.1dollar) %>%
  summarize(cov.females.water = cov(x = perc.basic2015water,
                                    y = female.in.school),
            cov.females.pov = cov(x = perc.1dollar,
                                  y = female.in.school))

water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  summarize(cov.females.water = cov(x = perc.basic2015water,
                                    y = female.in.school))
water.educ %>%
  drop_na(perc.1dollar) %>%
  drop_na(female.in.school) %>%
  summarize(cov.females.pov = cov(x = perc.1dollar,
                                  y = female.in.school))

# Plot of female education and water access
water.educ %>%
  ggplot(aes(y = female.in.school/100, 
             x = perc.basic2015water/100)) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              aes(linetype = "Fit line"), color
              = "gray60") +
  geom_point(size = 2, aes(color = "Country"), alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# correlation (Person) between water access and female education
water.educ %>%
  summarize(cor.females.water = cor(x = perc.basic2015water,
                                    y = female.in.school,
                                    use = "complete"))

#Plot of female education and water access
water.educ %>%
  ggplot(aes(y = female.in.school/100, 
             x = perc.basic2015water/100)) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "gray60") +
  geom_point(size = 2, 
             color = "#7463AC", 
             alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

# explore plot of female education and water
water.educ %>%
  ggplot(aes(y = female.in.school/100, 
             x = perc.basic2015water/100)) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              aes(color = "Linear fit line")) +
  geom_point(aes(size = "Country"), 
             color = "#7463AC", alpha = .6)+
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "gray60", name = "") +
  scale_size_manual(values = 2, name = "")

# explore plot of female education and poverty
water.educ %>%
  ggplot(aes(y = female.in.school/100, 
             x = perc.1dollar/100)) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              aes(color = "Linear fit line")) +
  geom_point(aes(size = "Country"), 
             color = "#7463AC", alpha = .6)+
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percent living on less $1 per day") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "gray60", name = "") +
  scale_size_manual(values = 2, name = "")

# correlations between water access, poverty, and female education
water.educ %>%
  summarize(cor.females.water = cor(x = perc.basic2015water,
                                    y = female.in.school,
                                    use = "complete"),
            cor.females.pov = cor(x = perc.1dollar,
                                  y = female.in.school,
                                  use = "complete"))

#### Haga el plot y calcula en coeficiente r de Pearson para
#### x = perc.basic2015sani y = female.in.school
water.educ %>%
  ggplot(aes(y = female.in.school/100, 
             x = perc.basic2015sani/100)) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              aes(color = "Linear fit line")) +
  geom_point(aes(size = "Country"), 
             color = "#7463AC", alpha = .6)+
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percet of Basic Sani") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "gray60", name = "") +
  scale_size_manual(values = 2, name = "")

# Prueba de hipótesis
# assign the results to an object
corFemEducWaterHPT <- cor.test(x = water.educ$perc.basic2015water,
                               y = water.educ$female.in.school)
# explore the object
str(object = corFemEducWaterHPT)
corFemEducWaterHPT

# check normality of female.in.school variable
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(x = female.in.school)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Percent of females in school",
       y = "Number of countries")

# Q-Q plot of female.in.school variable to check normality
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(sample = female.in.school)) +
  stat_qq(aes(color = "Country"), alpha = .6) +
  geom_abline(aes(intercept = mean(female.in.school),
                  slope = sd(female.in.school),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed values of percent of\nfemales in school") +
  ylim(0,100) +
  scale_linetype_manual(values = 1, name = "") +
  scale_color_manual(values = "#7463AC", name = "")

# check normality of water access variable
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(x = perc.basic2015water)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Percent with basic water access",
       y = "Number of countries")

# Q-Q plot of water access variable to check normality
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(sample = perc.basic2015water)) +
  stat_qq(aes(color = "Country"), alpha = .6) +
  geom_abline(aes(intercept = mean(x = perc.basic2015water),
                  slope = sd(x = perc.basic2015water),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed values of percent of people\nwith basic water access") +
  ylim(0,100) +
  scale_linetype_manual(values = 1, name = "") +
  scale_color_manual(values = "#7463AC", name = "")

# female education and water graph with linear fit line and Loess curve
water.educ %>%
  ggplot(aes(y = female.in.school/100, x = perc.basic2015water/100)) +
  geom_point(aes(size = "Country"), 
             color = "#7463AC", 
             alpha = .6) +
  geom_smooth(aes(color = "Linear fit line"), 
              method = "lm", 
              se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), 
              se = FALSE) +
  theme_minimal() +
  labs(y = "Percent of females in school",
       x = "Percent with basic access to water") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("gray60", "deeppink"), name= "") +
  scale_size_manual(values = 2, name = "")

# Breusch-Pagan test for equal variance (homocedasticidad)
testVar <- lmtest::bptest(formula = water.educ$female.in.school ~ 
                          water.educ$perc.basic2015water)
testVar


#### Linear Regression COVID 19 -------------------

r = cor(Covid19CR$Dia,Covid19CR$Casos)
r

## Simple Lineal Regression (Lineal)
modelCovid19 <- lm(Covid19CR$Casos ~ Covid19CR$Dia, 
                  data = Covid19CR)

modelCovid19
summary(modelCovid19)

## Simple Lineal Regression (2 Polinomios)
modelCovid19 <- lm(Covid19CR$Casos ~ poly(Covid19CR$Dia,2,raw = TRUE), 
                   data = Covid19CR)
modelCovid19
summary(modelCovid19)


## Simple Lineal Regression (3 Polinomios)
modelCovid19 <- lm(Covid19CR$Casos ~ poly(Covid19CR$Dia,3,raw = TRUE), 
                   data = Covid19CR)
modelCovid19
summary(modelCovid19)


## Gastos Médicos / Multiple Linear Regression ----
insurance <- read.csv("insurance.csv", 
                      stringsAsFactors = TRUE)
str(insurance)

# Estadísticas
summary(insurance$expenses)
sd(insurance$expenses)

# Histograma de Expenses
hist(insurance$expenses)

# Frecuencia
table(insurance$region)
table(insurance$sex)
table(insurance$smoker)

# Correlación entre las variables
cor(insurance[c("age", "bmi", "children", "expenses")])

# Visualizar la correlación
pairs(insurance[c("age", "bmi", "children", "expenses")])

# Scatterplot matrix
psych::pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

## Training a model 
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)

## Es igual que el anterior
ins_model <- lm(expenses ~ ., 
                data = insurance) 

# Ver los coeficientes
ins_model

table(insurance$region)
## Ver los detalles y el performance
summary(ins_model)

## Mejoras el performance

# Efecto de dos polinomios
insurance$age2 <- insurance$age^2

# BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)


# Modelo final. Note la interacción entre BMI30 y SMOKER
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

# Obesidad si y fumador si

summary(ins_model2)

# Precedir con el mismo DataSet
insurance$pred <- predict(ins_model2, insurance)

cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))

6470.543 - 5973.774

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 0,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 31, sex = "male", bmi30 = 1,
                   smoker = "yes", region = "northeast"))


