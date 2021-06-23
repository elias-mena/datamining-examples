library(odbc)
library(psych)
library(modeest)
library(FinCal)
library(fdth)
library(gmodels)
library(caret)
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
                 Server = "DESKTOP-DVL012J\\SQLDEV2019",
                 Database = "AdventureWorks2019",
                 UID = "TestLogin",
                 PWD = "test1234",
                 Port = 1433)

#Extraer toda la información de los empleados
#WHERE SalariedFlag = 'Yes'
DF <- dbSendQuery(con,
                  "SELECT *
                   FROM HumanResources.v_Employee;"
                  )

EmployeeDataset <- dbFetch(DF)
str(EmployeeDataset)

DF2 <- dbSendQuery(con,
                   "SELECT *
                   FROM HumanResources.v_EmployeeSales;")

EmployeeSalesDataset <- dbFetch(DF2)
str(EmployeeSalesDataset)

EmployeeDataset <- EmployeeDataset %>%
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

EmployeeSalesDataset <- EmployeeSalesDataset %>%
  mutate(MaritalStatus = as.factor(MaritalStatus)) %>% 
  mutate(Gender = as.factor(Gender)) %>%
  mutate(SalariedFlag = as.factor(SalariedFlag))%>%
  mutate(CurrentFlag = as.factor(CurrentFlag))%>%
  mutate(AgeBucket = as.factor(AgeBucket)) %>%
  mutate(YearsWorkingBucket = factor(YearsWorkingBucket,
                                     labels = c("8-10")))%>%
  mutate(VacationHoursBucket = as.factor(VacationHoursBucket)) %>%
  mutate(SickLeaveHoursBucket = as.factor(SickLeaveHoursBucket)) %>%
  mutate(JobTitle = as.factor(JobTitle)) %>%
  mutate(TotalDueBucket = factor(TotalDueBucket,
                                 labels = c("1-1000","1001-5000","5001-10000",
                                            "10001-20000","20001-30000",
                                            "30001-40000","40001-50000",
                                            "50001-60000","60001-70000",
                                            "70001-80000","80001-90000",
                                            "90001+"))) %>%
  mutate(MonthOfYearName = factor(MonthOfYearName,
                                  labels = c("January","February","March",
                                             "April","May","June","July",
                                             "August","September","October",
                                             "November","December"))) %>%
  mutate(YearMonth = as.factor(YearMonth)) %>%
  mutate(DayofWeekName = factor(DayofWeekName,
                                labels = c("Sunday","Monday","Tuesday",
                                           "Wednesday","Thursday",
                                           "Friday","Saturday")))

str(EmployeeSalesDataset)

TableCreation (EmployeeDataset$JobTitle)[-3]
TableCreation (EmployeeDataset$MaritalStatus)[-3]
TableCreation (EmployeeDataset$Gender)[-3]
TableCreation (EmployeeDataset$SalariedFlag)[-3]
TableCreation (EmployeeDataset$CurrentFlag)[-3]
TableCreation (EmployeeDataset$AgeBucket)[-3]
TableCreation (EmployeeDataset$YearsWorkingBucket)[-3]
TableCreation (EmployeeDataset$VacationHoursBucket)[-3]
TableCreation (EmployeeDataset$SickLeaveHoursBucket)[-3]

TableCreation (EmployeeSalesDataset$DayofWeekName)[-3]

#Frecuencia abs y relativa de JobTitle
ch1 <- EmployeeDataset %>%
  drop_na(JobTitle) %>%
  group_by(JobTitle) %>%
  summarise(count = n()) %>%
  filter(count>2)%>%
  arrange(count) %>%
  ggplot(aes(x = reorder(JobTitle,(count)),
             y = count)) +
  geom_bar(stat = "identity"
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  labs(x = "JobTitle", 
       y = "Frecuencia por JobTitle") +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <- EmployeeDataset %>%
  drop_na(JobTitle) %>%
  group_by(JobTitle) %>%
  summarise(count = n()) %>%
  filter(count>2)%>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = reorder(JobTitle,(count)),
             y = freqrelat)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(x = "Job Title", 
       y = "Frencuencia relativa", 
       subtitle = "Porcentaje del total para Job Title") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ncol = 2)

################## Frecuencia abs y relativa de Marital Status
ch1 <- EmployeeDataset %>%
  drop_na(MaritalStatus) %>%
  group_by(MaritalStatus) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = reorder(MaritalStatus,(-count)),
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = MaritalStatus)) +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Employee por Marital Status") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <- EmployeeDataset %>%
  drop_na(MaritalStatus) %>%
  group_by(MaritalStatus) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = MaritalStatus,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = MaritalStatus)) + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Employee por Marital Status") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

ch3 <- EmployeeSalesDataset %>%
  drop_na(MaritalStatus) %>%
  group_by(MaritalStatus) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = reorder(MaritalStatus,(-count)),
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = MaritalStatus)) +
  labs(x = "Marital Status", 
       y = "Frecuencia", 
       subtitle = "Orders por Marital Status") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch4 <- EmployeeSalesDataset %>%
  drop_na(MaritalStatus) %>%
  group_by(MaritalStatus) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = MaritalStatus,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = MaritalStatus)) + 
  labs(x = "Marital Status", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Marital Status") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ch3,
                        ch4,
                        ncol = 2,
                        nrow = 2)

################## #Frecuencia abs y relativa de Gender
ch1 <- EmployeeDataset %>%
  drop_na(Gender) %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = reorder(Gender,(-count)),
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = Gender)) +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Employee por Gender") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <- EmployeeDataset %>%
  drop_na(Gender) %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = reorder(Gender,(-count)),
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = Gender)) + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Employee por Gender") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

ch3 <- EmployeeSalesDataset %>%
  drop_na(Gender) %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = reorder(Gender,(-count)),
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = Gender)) +
  labs(x = "Gender", 
       y = "Frecuencia", 
       subtitle = "Orders por Gender") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch4 <- EmployeeSalesDataset %>%
  drop_na(Gender) %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = reorder(Gender,(-count)),
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = Gender)) + 
  labs(x = "Gender", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Gender") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ch3,
                        ch4,
                        ncol = 2,
                        nrow = 2)

################### Frecuencia abs y relativa para Salaried Flag
ch1 <- EmployeeDataset %>%
  drop_na(SalariedFlag) %>%
  group_by(SalariedFlag) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = reorder(SalariedFlag,(-count)),
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = SalariedFlag)) +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Employee por Salaried Flag") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <- EmployeeDataset %>%
  drop_na(SalariedFlag) %>%
  group_by(SalariedFlag) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = SalariedFlag,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = SalariedFlag)) + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Salaried Flag") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

ch3 <- EmployeeSalesDataset %>%
  drop_na(SalariedFlag) %>%
  group_by(SalariedFlag) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = reorder(SalariedFlag,(-count)),
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = SalariedFlag)) +
  labs(x = "Salaried Flag", 
       y = "Frecuencia", 
       subtitle = "Orders por Salaried Flag") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch4 <- EmployeeSalesDataset %>%
  drop_na(SalariedFlag) %>%
  group_by(SalariedFlag) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = SalariedFlag,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = SalariedFlag)) + 
  labs(x = "Salaried Flag", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Salaried Flag") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ch3,
                        ch4,
                        ncol = 2,
                        nrow = 2)

################### Frecuencia abs y relativa para AgeBucket
ch1 <- EmployeeDataset %>%
  drop_na(AgeBucket) %>%
  group_by(AgeBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = AgeBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = AgeBucket)) +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Employee por Age Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <- EmployeeDataset %>%
  drop_na(AgeBucket) %>%
  group_by(AgeBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = AgeBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = AgeBucket)) + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Employee por Age Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

ch3 <- EmployeeSalesDataset %>%
  drop_na(AgeBucket) %>%
  group_by(AgeBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = AgeBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = AgeBucket)) +
  labs(x = "Age Bucket", 
       y = "Frecuencia", 
       subtitle = "Order por Age Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch4 <- EmployeeSalesDataset %>%
  drop_na(AgeBucket) %>%
  group_by(AgeBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = AgeBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = AgeBucket)) + 
  labs(x = "Age Bucket", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Age Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ch3,
                        ch4,
                        ncol = 2,
                        nrow = 2)

#################### Frecuencia abs y relativa para YearsWorkingBucket
ch1 <- EmployeeDataset %>%
  drop_na(YearsWorkingBucket) %>%
  group_by(YearsWorkingBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = YearsWorkingBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = YearsWorkingBucket)) +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Employee por Years Working Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <- EmployeeDataset %>%
  drop_na(YearsWorkingBucket) %>%
  group_by(YearsWorkingBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = YearsWorkingBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = YearsWorkingBucket)) + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Employee por Years Working Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

ch3 <- EmployeeSalesDataset %>%
  drop_na(YearsWorkingBucket) %>%
  group_by(YearsWorkingBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = YearsWorkingBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = YearsWorkingBucket)) +
  labs(x = "Years Working Bucket", 
       y = "Frecuencia", 
       subtitle = "Orders por Years Working Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch4 <- EmployeeSalesDataset %>%
  drop_na(YearsWorkingBucket) %>%
  group_by(YearsWorkingBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = YearsWorkingBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = YearsWorkingBucket)) + 
  labs(x = "Years Working Bucket", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Years Working Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ch3,
                        ch4,
                        ncol = 2,
                        nrow = 2)


##################### Frecuencia abs y relativa para VacationHoursBucket
ch1 <- EmployeeDataset %>%
  drop_na(VacationHoursBucket) %>%
  group_by(VacationHoursBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = VacationHoursBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = VacationHoursBucket)) +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Employee por Vacation Hours Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <- EmployeeDataset %>%
  drop_na(VacationHoursBucket) %>%
  group_by(VacationHoursBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = VacationHoursBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = VacationHoursBucket)) + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Employee por Vacation Hours Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

ch3 <- EmployeeSalesDataset %>%
  drop_na(VacationHoursBucket) %>%
  group_by(VacationHoursBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = VacationHoursBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = VacationHoursBucket)) +
  labs(x = "Vacation Hours Bucket", 
       y = "Frecuencia", 
       subtitle = "Orders por Vacation Hours Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch4 <- EmployeeSalesDataset %>%
  drop_na(VacationHoursBucket) %>%
  group_by(VacationHoursBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = VacationHoursBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = VacationHoursBucket)) + 
  labs(x = "Vacation Hours Bucket", 
       y = "Frencuencia relativa", 
       subtitle = "Orders Vacation Hours Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()


gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ch3,
                        ch4,
                        ncol = 2,
                        nrow = 2)


###################### Frecuencia abs y relativa para SickLeaveHoursBucket
ch1 <-  EmployeeDataset %>%
  drop_na(SickLeaveHoursBucket) %>%
  group_by(SickLeaveHoursBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = SickLeaveHoursBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = SickLeaveHoursBucket)) +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Employee por Sick Leave Hours Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch2 <-  EmployeeDataset %>%
  drop_na(SickLeaveHoursBucket) %>%
  group_by(SickLeaveHoursBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = SickLeaveHoursBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = SickLeaveHoursBucket)) + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Employee por Sick Leave Hours Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

ch3 <-  EmployeeSalesDataset %>%
  drop_na(SickLeaveHoursBucket) %>%
  group_by(SickLeaveHoursBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = SickLeaveHoursBucket,
             y = count)) +
  geom_bar(stat = 'identity',
           aes(fill = SickLeaveHoursBucket)) +
  labs(x = "Sick Leave Hours Bucket", 
       y = "Frecuencia", 
       subtitle = "Orders por Sick Leave Hours Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

ch4 <-  EmployeeSalesDataset %>%
  drop_na(SickLeaveHoursBucket) %>%
  group_by(SickLeaveHoursBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = SickLeaveHoursBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity',
           aes(fill = SickLeaveHoursBucket)) + 
  labs(x = "Sick Leave Hours Bucket", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Sick Leave Hours Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

gridExtra::grid.arrange(ch1, 
                        ch2, 
                        ch3,
                        ch4,
                        ncol = 2,
                        nrow = 2)


###################### Frecuencia abs y relativa para Order Due Bucket
EmployeeSalesDataset %>%
  drop_na(TotalDueBucket) %>%
  group_by(TotalDueBucket) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  ggplot(aes(x = TotalDueBucket,
             y = count)) +
  geom_bar(stat = 'identity') +
  labs(x = "", 
       y = "Frecuencia", 
       subtitle = "Orders por Total Due Bucket") +
  scale_fill_brewer(palette = "Pastel1",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = count),
             size = 2.8) +
  theme_minimal()

EmployeeSalesDataset %>%
  drop_na(TotalDueBucket) %>%
  group_by(TotalDueBucket) %>%
  summarise(count = n()) %>%
  mutate(freqrelat = 100 * (count / sum(count))) %>%
  arrange(desc(freqrelat)) %>%
  ggplot(aes(x = TotalDueBucket,
             y = freqrelat)) +
  geom_bar(stat = 'identity') + 
  labs(x = "", 
       y = "Frencuencia relativa", 
       subtitle = "Orders por Total Due Bucket") +
  scale_fill_brewer(palette = "Pastel2",
                    guide = FALSE) +
  geom_label(stat = "identity",
             aes(label = round(freqrelat,2)
             ),
             size = 2.8
  ) +
  theme_minimal()

##################### Órdenes por día
EmployeeSalesDataset %>%
  group_by(OrderDate) %>%
  summarise_at(vars(TotalDue),
               funs(sum)) %>%
  ggplot(aes(x=OrderDate, 
             y=TotalDue/1000000)) +
  geom_point(aes(col=TotalDue/1000000, 
                 size=TotalDue)) + 
  labs(col="Amount USD", 
       size="Tamaño") +
  labs(x = "Order Date", 
       y = "Order Amount USD") + 
  ggtitle("Monto de ventas por día (millones)")+
  guides(size=FALSE)

#Visualización ordenes por calendario 
EmployeeSalesDataset %>%
  group_by(WeekOfMonth,DayofWeekName,YearName,MonthOfYearName) %>%
  summarise(count = n()) %>%
  ggplot(aes(WeekOfMonth, 
             DayofWeekName, 
             fill = count)) + 
  geom_tile(colour = "white") + 
  facet_grid(YearName ~ MonthOfYearName) + 
  scale_fill_gradient(low = "#FFD000", 
                      high = "#FF1919") + 
  ggtitle("Cantidad órdenes por día", 
          "Heatmap por día de la semana, mes y año") +
  labs(x = "Número de semana", y = "Día de la semana") +
  labs(fill = "Cantidad")

#################################### Tendencia Mensual
EmployeeSalesDataset %>%
  group_by(OrderDate) %>%
  summarise_at(vars(TotalDue),
               funs(sum)) %>%
  ggplot(aes(x = OrderDate, 
             y = TotalDue/1000000,
         group = 1),
  stat = "identity") +
  geom_line() +
  geom_point() +
  labs(x = "Order Date", 
       y = "Total Amount USD (Millones)") +
  theme(axis.text.x = element_text(angle = 90)) +
  stat_smooth(method = "lm")

################# ¿Qué día se hacen más órdenes?
EmployeeSalesDataset %>%
  drop_na(DayofWeekName) %>%
  group_by(DayofWeekName) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = DayofWeekName,
             y = count)) +
  geom_col(aes(fill = "#C39BD3")) +
  coord_polar() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16,face = "bold"),
        axis.text.y = element_blank()
  ) +
  ggtitle("Frecuencia",
          "Actividad por día de la semana") +
  scale_fill_manual (values = c("#C39BD3"),
                     guide = FALSE)

################# ¿Qué mes se hacen más órdenes?
EmployeeSalesDataset %>%
  drop_na() %>%
  group_by(MonthOfYearName) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = MonthOfYearName,
             y = count)) +
  geom_col(aes(fill = "#C39BD3")) +
  coord_polar() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16,face = "bold"),
        axis.text.y = element_blank()
  ) +
  ggtitle("Frecuencia",
          "Actividad por día de la mes") +
  scale_fill_manual (values = c("#C39BD3"),
                     guide = FALSE)


################# Relación entre Género y Asalariado
EmployeeDataset %>%
  drop_na(Gender) %>%
  drop_na(SalariedFlag) %>%
  group_by(Gender,SalariedFlag) %>% 
  count() %>% 
  group_by(Gender) %>% 
  mutate(percent = 100*(n/sum(n))) %>% 
  ggplot(aes(x = Gender, 
             fill = SalariedFlag,
             y = percent)) + # use new values from mutate
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(x = "Gender",
       y = "% por grupo",
       subtitle = "Relación entre Gender & SalariedFlag") +
  scale_fill_brewer(palette = "Pastel1",
                    name = "Salaried Flag") +
  geom_text(aes(label = round(percent)),
            size = 3.8,
            position = position_dodge(0.5),
            vjust = 0) 

################# Relación entre Género y SickLeaveHoursBucket
EmployeeDataset %>%
  drop_na(Gender) %>%
  drop_na(SickLeaveHoursBucket) %>%
  group_by(Gender,SickLeaveHoursBucket) %>% 
  count() %>% 
  group_by(SickLeaveHoursBucket) %>% 
  mutate(percent = 100*(n/sum(n))) %>% 
  ggplot(aes(x = SickLeaveHoursBucket, 
             fill = Gender,
             y = percent)) + # use new values from mutate
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(x = "Sick Leave Hours Bucket",
       y = "% por grupo",
       subtitle = "Relación entre Gender & SickLeaveHoursBucket") +
  scale_fill_brewer(palette = "Pastel1",
                    name = "Gender") +
  geom_text(aes(label = round(percent)),
            size = 3.8,
            position = position_dodge(0.5),
            vjust = 0) 

################# Relación entre Género y TotalDueBucket
EmployeeSalesDataset %>%
  drop_na(Gender) %>%
  drop_na(TotalDueBucket) %>%
  group_by(Gender,TotalDueBucket) %>% 
  count() %>% 
  group_by(TotalDueBucket) %>% 
  mutate(percent = 100*(n/sum(n))) %>% 
  ggplot(aes(x = TotalDueBucket, 
             fill = Gender,
             y = percent)) + # use new values from mutate
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(x = "Total Due Bucket",
       y = "% por grupo",
       subtitle = "Relación entre Gender & TotalDueBucket") +
  scale_fill_brewer(palette = "Pastel1",
                    name = "Gender") +
  geom_text(aes(label = round(percent)),
            size = 3.8,
            position = position_dodge(0.5),
            vjust = 0) 

################# Línea de tiempo por Gender
EmployeeSalesDataset %>%
  group_by(OrderDate,Gender)  %>%
  summarise_at(vars(TotalDue),
               funs(sum)) %>%
  ggplot(aes(x = OrderDate, 
             y = TotalDue/1000000,
             group = 1,
             stat = "identity")) +
  geom_line() +
  geom_point() +
  labs(x = "Order Date", 
       y = "Total Amount USD (Millones)") +
  facet_wrap(~Gender) +
  theme(axis.text.x = element_text(angle = 90))  +
  guides(fill = FALSE)+
  stat_smooth(method = "lm")

################# Línea de tiempo por Agebucket
EmployeeSalesDataset %>%
  group_by(OrderDate,AgeBucket)  %>%
  summarise_at(vars(TotalDue),
               funs(sum)) %>%
  ggplot(aes(x = OrderDate, 
             y = TotalDue/1000000,
             group = 1,
             stat = "identity")) +
  geom_line() +
  geom_point() +
  labs(x = "Order Date", 
       y = "Total Amount USD (Millones)") +
  facet_wrap(~AgeBucket) +
  theme(axis.text.x = element_text(angle = 90))  +
  guides(fill = FALSE)+
  stat_smooth(method = "lm")

################# ¿Hay algunas relación entre las órdenes y la edad?
EmployeeSalesDataset %>%
  ggplot(aes(x=Age, 
             y=TotalDue,
             color=Gender,
             shape=Gender)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

EmployeeSalesDataset %>%
  ggplot(aes(x=Age, 
             y=TotalDue)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

################# ¿Hay algunas relación entre las órdenes y las ausencias?
EmployeeSalesDataset %>%
  ggplot(aes(x=SickLeaveHours, 
             y=TotalDue)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

################# ¿Hay algunas relación entre las Edad y las ausencias?
EmployeeDataset %>%
  ggplot(aes(x=Age, 
             y=SickLeaveHours)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

##################################### Distribución de frecuencias
## Tabla de frecuencias
#Scott,FD
#Donde
#f= frecuencia absoluta
#rf= frecuencia relativa
#rf(%) frecuencia relativa porcentual
#cf= frecuencia acumulada
#cf(%)=frecuencia acumulada porcentual

#Hay una función escrita en R que estima el numero de clases
nclass.Sturges(EmployeeDataset$Age)
nclass.Sturges(EmployeeSalesDataset$TotalDue)

dist <- fdt(EmployeeDataset$Age,
            breaks="Sturges",
            k=5)
dist

dist <- fdt(EmployeeSalesDataset$TotalDue,
            breaks="Sturges",
            K=nclass.Sturges(EmployeeSalesDataset$TotalDue))
dist

##################################### Histogramas
hist(EmployeeDataset$Age,
     main = "Histograma de frecuencias",
     xlab = "Employee Age",
     ylab = "Frecuencia",
     col = "gray",
     border = "black",
     xlim = c(25, 75),
     ylim = c(0, 100),
     breaks=10)

hist(EmployeeSalesDataset$TotalDue,
     main = "Histograma de frecuencias",
     xlab = "Total Due",
     ylab = "Frecuencia",
     col = "#7463AC",
     border = "black",
     xlim = c(0, 230000),
     ylim = c(0, 2500),
     breaks=10)

##################################### Tendencia Central
mean(EmployeeDataset$Age,na.rm=TRUE)
psych::geometric.mean(EmployeeDataset$Age,na.rm=TRUE)
psych::harmonic.mean(EmployeeDataset$Age,na.rm=TRUE)

mean(EmployeeSalesDataset$TotalDue,na.rm=TRUE)
psych::geometric.mean(EmployeeSalesDataset$TotalDue,na.rm=TRUE)
psych::harmonic.mean(EmployeeSalesDataset$TotalDue,na.rm=TRUE)

median(EmployeeDataset$Age,na.rm=TRUE)
median(EmployeeSalesDataset$TotalDue,na.rm=TRUE)

#Moda
mfv(EmployeeDataset$Age) #modeest
mfv(EmployeeSalesDataset$TotalDue) #modeest

summary(EmployeeDataset$Age,na.rm=TRUE)
psych::describe(EmployeeDataset$Age)
table(EmployeeDataset$Age)

summary(EmployeeSalesDataset$TotalDue,na.rm=TRUE)
psych::describe(EmployeeSalesDataset$TotalDue)

##################################### Dispersión o variabilidad
range(EmployeeDataset$Age,na.rm=TRUE)
max(EmployeeDataset$Age) - min(EmployeeDataset$Age)
var(EmployeeDataset$Age,na.rm=TRUE)
sd(EmployeeDataset$Age,na.rm=TRUE)

STDEVP(EmployeeDataset$Age) #Población

100 * (sd(EmployeeDataset$Age,na.rm=TRUE)/
         mean(EmployeeDataset$Age,na.rm=TRUE)) #CV

coefficient.variation(sd=sd(EmployeeDataset$Age), 
                      avg = mean(EmployeeDataset$Age)) * 100 #FinCal

range(EmployeeSalesDataset$TotalDue,na.rm=TRUE)
max(EmployeeSalesDataset$TotalDue) - min(EmployeeSalesDataset$TotalDue)
var(EmployeeSalesDataset$TotalDue,na.rm=TRUE)
sd(EmployeeSalesDataset$TotalDue,na.rm=TRUE)

coefficient.variation(sd=sd(EmployeeSalesDataset$TotalDue), 
                      avg = mean(EmployeeSalesDataset$TotalDue)) * 100 #FinCal

psych::describe(EmployeeDataset$Age)
psych::describe(EmployeeSalesDataset$TotalDue)

hist(EmployeeDataset$Age,
     main = "Histograma de frecuencias",
     xlab = "Employee Age",
     ylab = "Frecuencia",
     col = "gray",
     border = "black",
     xlim = c(25, 75),
     ylim = c(0, 100),
     breaks=10)

hist(EmployeeSalesDataset$TotalDue,
     main = "Histograma de frecuencias",
     xlab = "Total Due",
     ylab = "Frecuencia",
     col = "#7463AC",
     border = "black",
     xlim = c(0, 230000),
     ylim = c(0, 2500),
     breaks=10)


################################### Medidas de distribución
#skew type = 1, Fisher
skew(EmployeeDataset$Age,na.rm=TRUE) #psych
skew(EmployeeSalesDataset$TotalDue,na.rm=TRUE)

#skewness (asimetría) usando seemTools
#se = standard error
#z = skew/se
semTools::skew(EmployeeDataset$Age)
semTools::skew(EmployeeSalesDataset$TotalDue)

#Curtosis
kurtosi(EmployeeDataset$Age,na.rm=TRUE)
semTools::kurtosis(EmployeeDataset$Age)

kurtosi(EmployeeSalesDataset$TotalDue,na.rm=TRUE)
semTools::kurtosis(EmployeeSalesDataset$TotalDue)

# Histograma de densidad, agregar curva de asimetría
# Comparación con curva normal (mismo gráfico)
#par(mar = c(1, 1, 1, 1)) 
hist(EmployeeDataset$Age,
     freq = F,
     xlab = "Age",
     main = "")

dz <- density(EmployeeDataset$Age)
lines(dz, col = "red", lwd = 3)

curve(dnorm(x, 
            mean(EmployeeDataset$Age), 
            sd(EmployeeDataset$Age)),
      col = "blue", 
      lwd = 3, 
      add = TRUE)
# R dibuja una curva de densidad normal (dnorm) para
# los valores del eje x que tienen una media
# mean(OD_Fetch$Age) y una desviación estándar
# sd(OD_Fetch$Age).






