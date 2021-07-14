library(dslabs)

#¿Es co2 tidy?
data(co2)

class(co2)

#¿Es co2 tidy?
data(ChickWeight)

class(ChickWeight)

#¿Es co2 tidy?
data(BOD)


data("murders")

head(murders)
#Usar mutate() en dplyr para agregar nuevas variables o modificar las 
#existentes. En el ejemplo, agregar una nueva variables que 
#se llama rate
murders <- mutate(murders, 
                  rate = total / population * 100000)

head(murders)

#Usar filter() en dplyr para filtrar los datos
filter(murders, rate <= 0.71)

#Usar select en dplyr para escogencia de variables específicas
new_murders <- select(murders, state, region, rate)
head(new_murders)

filter(murders, state == "New York")
filter(murders, state != "Florida")
filter(murders, state %in% c("New York", "Texas"))
filter(murders, population < 5000000 & region == "Northeast")

#The PIPE/PayLoad
murders %>%
  filter(population < 5000000 & region == "Northeast")

murders %>%
  select(state, region, rate) %>%
  filter(rate <= 0.71) %>%
  head()

#the pipe sends the result of the left side of the pipe to be the first 
#argument of the function on the right side of the pipe
16 %>% sqrt()
16 %>% sqrt() %>% log2()

#summarize en dplyr para estadística descriptiva
data(heights)

heights %>% 
  filter(sex == "Female") %>%
  dplyr::summarize(median = median(height), 
                   STEDV = sd(height),
                   minimum = min(height), 
                   maximum = max(height))

#summarize en dplyr para cálculos de grupos
murders %>%
  dplyr::summarize(rate = sum(total) / sum(population) * 100000)

#Cuando se desea devolver un datos que no sea un data.frame, se debe usar
#pull()
murders %>%
  dplyr::summarize(rate = sum(total) / sum(population) * 100000) %>%
  pull()

#Agrupar y luego sumarizar
heights %>% 
  group_by(sex) %>%
  dplyr::summarize(Cantidad = n())

heights %>% 
  group_by(sex) %>%
  dplyr::summarize(average = mean(height), 
                   STDEV = sd(height))

#arrange() en dplyr para ordenar
murders %>% 
  arrange(rate) %>%
  head()

murders %>% 
  arrange(desc(rate)) %>%
  head()

murders %>% 
  arrange(region, rate) %>% 
  head()

#top_n() en dplyr para filtrar
murders %>% 
  top_n(5, rate)

murders %>% 
  arrange(rate) %>% 
  top_n(5, rate)

murders %>% 
  arrange(desc(rate)) %>% 
  top_n(5, rate)

#CASE
murders %>% 
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "Other")) %>%
  group_by(group) %>%
  dplyr::summarize(rate = sum(total) / sum(population) * 10^5)
