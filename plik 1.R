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
install.packages("ggplot2")
install.packages("plotly")

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
library(ggplot2)
library(plotly)

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

sum(complete.cases(data))
nrow(data[complete.cases(data), ])/nrow(data)*100

is.special <- function(x){if (is.numeric(x)) !is.finite(x) else is.na(x)}
sapply(data, is.special)

for (n in colnames(data)){
  is.na(data[[n]]) <- is.special(data[[n]])
}
summary(data)


###BRAKUJĄCE OBSERWACJE 

#Utworzenie tabeli podsumowującej braki w tabeli 
miss_var_summary(data)

#Duże braki są zauważalne w: floor, buildYear 

#Utworzenie tabel podsumowujących według poszczególnych kategori
data %>%
  group_by(floor) %>%
  miss_var_summary()
head(5)

data %>%
  group_by(collegeDistance) %>%
  miss_var_summary() 
head(5)

data %>%
  group_by(floorCount) %>%
  miss_var_summary()
head(5)

data %>%
  group_by(buildYear) %>%
  miss_var_summary() %>%
  head(5)


#Tabela podsumowująca brakujące wartości według wiersza    
data %>%
  miss_case_table()

#Wizualizacjia brakujących danych 
vis_miss(data)

#Wizualizacjia brakujących zmiennych, według poszczególnych wierszy
gg_miss_fct(data, fct = floor)

gg_miss_fct(data, fct = collegeDistance)

gg_miss_fct(data, fct = floorCount)

gg_miss_fct(data, fct = buildYear)


#Wizualizacja jak często braki współwystępują między zmiennymi 

gg_miss_upset(data, 
              nsets = 3)

#najwięcej brakuje danych w 

data <-hotdeck(data)

# Filtrowanie danych - tylko sprzedaż mieszkań
data <- data %>% filter(type == "apartmentBuilding" | type == "blockOfFlats")

# Wykres 1: Histogram cen mieszkań
p1 <- ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Rozkład cen mieszkań", x = "Cena (PLN)", y = "Liczba mieszkań") +
  theme_minimal()
ggplotly(p1)

# Wykres 3: Scatter plot - Odległość od centrum vs cena
p3 <- ggplot(data, aes(x = centreDistance, y = price, color = city)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(title = "Odległość od centrum a cena", x = "Odległość od centrum (km)", y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p3)

# Wykres 4: Liczba mieszkań według liczby pokoi
p4 <- ggplot(data, aes(x = as.factor(rooms))) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Liczba mieszkań według liczby pokoi", x = "Liczba pokoi", y = "Liczba mieszkań") +
  theme_minimal()
ggplotly(p4)

# Wykres 5: Cena w zależności od stanu mieszkania
p5 <- ggplot(data, aes(x = condition, y = price, fill = condition)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Cena w zależności od stanu mieszkania", x = "Stan mieszkania", y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p5)

# Wykres 6: Cena w zależności od piętra
p6 <- ggplot(data, aes(x = as.factor(floor), y = price)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Cena w zależności od piętra", x = "Piętro", y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p6)

# Wykres 7: Cena w zależności od odległości od uczelni
p7 <- ggplot(data, aes(x = collegeDistance, y = price)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm") +
  labs(title = "Cena w zależności od odległości od uczelni", 
       x = "Odległość od uczelni (km)", 
       y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p7)

# Wykres 8: Cena w zależności od liczby pięter w budynku
p8 <- ggplot(data, aes(x = as.factor(floorCount), y = price)) +
  geom_boxplot(fill = "lightpink", outlier.color = "darkred") +
  labs(title = "Cena w zależności od liczby pięter w budynku", x = "Liczba pięter", y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p8)

# Wykres 9: Cena w zależności od roku budowy
p9 <- ggplot(data, aes(x = buildYear, y = price)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm") +
  labs(title = "Cena w zależności od roku budowy", 
       x = "Rok budowy", 
       y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p9)

