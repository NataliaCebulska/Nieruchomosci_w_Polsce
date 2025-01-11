#CZYSZCZENIE DANYCH
# załadowanie wymaganych bibliotek
install.packages("tidyverse")
install.packages("dplyr")
install.packages("editrules")
install.packages("VIM")
install.packages("deducorrect")
install.packages("ISLR")
install.packages("outliers")
install.packages("naniar")

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR)
library(outliers)
library(naniar)

data <- read_csv("apartments_pl_2024_06.csv")
View(data)
#sprawdzenie jakie mamy typy danych
glimpse(data)
#wybór danych istotnych dla analizy, wykluczenie id ogłoszenia i współrzędnych geograficznych

data %>% 
  select(city,type,squareMeters,rooms,floor,floorCount,buildYear,centreDistance,poiCount,ends_with("distance"), ownership, buildingMaterial, condition, starts_with("has"), price)

#Wyfiltrowanie w jakich wierszach jest wartość NA + dodanie tabelki w osobnej karcie, która sumuje ile jest wartości NA w każdej kolumnie
data %>% 
  select(city,type,squareMeters,rooms,floor,floorCount,buildYear,centreDistance,poiCount,ends_with("distance"), ownership, buildingMaterial, condition, starts_with("has"), price) %>% 
filter(!complete.cases(data)) %>% 
  summarise(across(everything(), ~sum(is.na(.)), .names = "NA_count_{.col}")) %>% 
View()

#wykres pudełkowy na ukazanie wartości odstających
par(mfrow=c(1,2))
boxplot(data$schoolDistance, main = "Boxplot dystans od szkoły", col = "lightblue")
boxplot(data$rooms, main = "Boxplot dystans od szkoły", col = "lightblue")


#Test Grubbsa na wartości odstające - metraż

data_column1<- data$squareMeters
clean_data <-data$squareMeters[complete.cases(data$squareMeters)]
result <- grubbs.test(clean_data)
print(result)
#p-value = 0.06864

#Test Grubbsa na wartości odstające - liczba pokoi
data_column1<- data$rooms
clean_data <-data$rooms[complete.cases(data$rooms)]
result <- grubbs.test(clean_data)
print(result)
#p-value = 1 ; brak wartości odstającej

#Test Grubbsa na wartości odstające - odległość od centrum
data_column1<- data$centreDistance
clean_data <-data$centreDistance[complete.cases(data$centreDistance)]
result <- grubbs.test(clean_data)
print(result)
#p-value = 0.1603

#Test Grubbsa na wartości odstające - odległość od szkoły
data_column1<- data$schoolDistance
clean_data <-data$schoolDistance[complete.cases(data$schoolDistance)]
result <- grubbs.test(clean_data)
print(result)
#p-value < 2.2e-16; jest wartość odstająca

#mean(data$squareMeters, na.rm=TRUE)
#min(data$squareMeters, na.rm=TRUE)
#max(data$squareMeters, na.rm=TRUE)
#sum(data$squareMeters == '25')
#sum(data$squareMeters == '125')

#mean(data$rooms, na.rm=TRUE)
#min(data$rooms, na.rm=TRUE)
#max(data$rooms, na.rm=TRUE)
#sum(data$rooms == '1')
#sum(data$rooms == '6')
#sum(data$rooms == '3')

#Wykrywanie braków danych

sum(complete.cases(dane))
nrow(dane[complete.cases(dane), ])/nrow(dane)*100

is.special <- function(x){if (is.numeric(x)) !is.finite(x) else is.na(x)}
sapply(dane, is.special)

for (n in colnames(dane)){
  is.na(dane[[n]]) <- is.special(dane[[n]])
}
summary(dane)


###BRAKUJĄCE OBSERWACJE 

#Utworzenie tabeli podsumowującej braki w tabeli 
miss_var_summary(dane)

#Duże braki są zauważalne w: floor, buildYear 

#Utworzenie tabel podsumowujących według poszczególnych kategori
dane %>%
  group_by(floor) %>%
  miss_var_summary()
head(5)

dane %>%
  group_by(collegeDistance) %>%
  miss_var_summary() 
head(5)

dane %>%
  group_by(floorCount) %>%
  miss_var_summary()
head(5)

dane %>%
  group_by(buildYear) %>%
  miss_var_summary() %>%
  head(5)


#Tabela podsumowująca brakujące wartości według wiersza    
dane %>%
  miss_case_table()

#Wizualizacjia brakujących danych 
vis_miss(dane)

#Wizualizacjia brakujących zmiennych, według poszczególnych wierszy
gg_miss_fct(dane, fct = floor)

gg_miss_fct(dane, fct = collegeDistance)

gg_miss_fct(dane, fct = floorCount)

gg_miss_fct(dane, fct = buildYear)


#Wizualizacja jak często braki współwystępują między zmiennymi 

gg_miss_upset(dane, 
              nsets = 3)

#najwięcej brakuje danych w 

data <-hotdeck(dane)
