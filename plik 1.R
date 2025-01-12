#CZYSZCZENIE DANYCH
#Instalacja oraz załadowanie potrzebnych bibliotek
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
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

#1. Data cleansing

#Wczytanie oraz wyświetlenie danych
data <- read_csv("apartments_pl_2024_06.csv")
View(data)


#sprawdzenie jakie mamy typy danych
glimpse(data)


#Sprawdzenie czy mamy prakujące obserwacje oraz ile ich jest
n_miss(data)


#Sprawdzenie ile brakujących obserwacji jest w każdej kolumnie
miss_var_summary(data)
#największe braki danych w kolumnach condition, buildingMaterial, type, floor oraz buildYear (w każdym powyżej 15%) 


#Pokazanie ile jest brakujących obserwacji w wierszach
miss_case_table(data)
#Większość obserwacji ma od 1 do 3 braków, co pozwala na zastosowanie imputacji brakujących danych


#Wizualizacja braków danych
vis_miss(data, cluster = TRUE, sort_miss = TRUE)
#graficzne ukzanie, w których kolumnach mamy najwięcej brakujacych danych. Wykres potwierdza, że są to kolumny condition (74%), building material (41%), type (20%), floor (17%) oraz buildYear (16%)



#wykres przedstawiający wzorce braków danych
gg_miss_upset(data)
#Z wykresu możemy odczytać, że najwięcej braków danych występuje jedynie w kolumnie condition, bez współwystępowania braku w innych kolumnach. Większa liczba braków danych występuje też zależnie zarówno w kolumnie condition jak i buildingMaterial oraz w kolumnach condition, buildingMaterial i type



# Grupowanie braków według miasta
data %>% 
  group_by(city) %>% 
  miss_var_summary() %>% 
  View()
#Nie ma istotnych różnic w brakach danych między miastami. Ww wszystkich największy odsetek brakujacych danych stanowi kolumna condition.




#Sprawdzenie wartości odstających

numeric_columns <- data %>% select(where(is.numeric)) %>% colnames()

for (col in numeric_columns) {
  cat("\nKolumna:", col, "\n")
  
  # Tworzenie wykresu pudełkowego
  boxplot_result <- boxplot(data[[col]], 
                            main = paste("Wykres pudełkowy dla", col), 
                            col = "lightblue", 
                            ylab = col)
  

}

#Analiza wartości odstających:

#Cena -> wartości odstające powyżej 2mln, do 3mn - brak podstaw do usuwania dancyh odstających, gdyż są to realne ceny za mieszkania
#Metry kwadratowe -> wartości odstające do max 140m2 - mają sens
#Pokoje, piętra oraz liczba pięter -> niewielka liczba możliwych wartości odstających, piętro na kt.orym jest mieszkanie nie przewyższa ogólnej liczby pięter
#Rok budowy -> wartości odstające poniżej roku 1900, lecz powyżej roku 1850 - mają sens
#poiCount?
#Dużo wartości odstających w odległościach do różnych punkót usłgowych, lecz poniżej 5km - wyniki mają sens
#Na podstawie powyższej analizy, możemy stwierdzic, że pomimo występowania wartości odstających nie wyglądają one na błędy i nasze dane są spójne, brak konieczności usuwania danych odstających



#Imputacja danych

#1. Condition
# Z racji tego, że stan normalny zazwyczaj nie jest określany w ogłoszeniach nieruchomości (chcemy się pochwalić stanem premium lub zaznaczyć niski standard mieszkania), z logicznego punktu widzenia, możemy uzupełnić braki danych w tej kolumnie jako 'standard'

data1Condition <- data %>% 
  mutate(condition = ifelse(is.na(condition), "Standard", condition))
View(data1Condition)
na_count_condition <- sum(is.na(data1Condition$condition))
print(na_count_condition)
#Brak wartości NA



#2. Building Material 
#Zmienna ta nie powinna mieć istotnego wpływu na cenę, kupujący zazwyczaj nie zwacają uwagi na aspekt matriałów użytych do budowy domu, w którym znajduje się mieszkanie. Aby zastąpić brakujące obserwacje użyjemy sformułowania "Material is not known/different building material". Jeśli w późniejszej analizie zostanie wykazany wpływ tego czynnika na cenę, bedziemy brać go pod uwagę.
data2Condition.buildingMaterial <- data1Condition %>% 
  mutate(buildingMaterial = ifelse(is.na(buildingMaterial), "Material is not known/different building material", buildingMaterial))
View(data2Condition.buildingMaterial)
na_count_condition <- sum(is.na(data2Condition.buildingMaterial$buildingMaterial))
print(na_count_condition)
#Brak wartości NA



#3.Type
#Brakujące wartości zostaną uzupełnione z użyciem metody k-NN (k-Nearest Neighbors), która zastąpi nam brakujące wartości na podstawie sąsiadujących obserwacji, które są najbardziej podobne do brakującego wiersza


dataCBT <- kNN(data2Condition.buildingMaterial, 
                    variable = c("type"),        
                    k = 5,                       
                    weightDist = TRUE)          
View(dataCBT)

na_count_condition <- sum(is.na(dataCBT$type))
print(na_count_condition)
#Brak wartości NA



#4. Floor Count 
#Brakujące wartości uzupełnione na podstawie grupowania oraz wyliczania mediany wartości w tychże grupach. Do porównania zostały użyte zmienne city, type oraz condition.
dataCBTFC <- dataCBT %>%
  group_by(city, type, condition) %>%  
  mutate(floorCount = ifelse(is.na(floorCount), median(floorCount, na.rm = TRUE), floorCount)) %>%
  ungroup()
View(dataCBTFC)
na_count_condition <- sum(is.na(dataCBTFC$floorCount))
print(na_count_condition)
#Brak wartości NA



#5.Floor 
#Brakujące wartości uzupełnione na podstawie grupowania oraz wyliczania mediany wartości w tychże grupach. Do porównania zostały użyte zmienne floorCount oraz type.

data2 <- dataCBTFC %>%
  group_by(floorCount, type) %>%
  mutate(floor = ifelse(is.na(floor), median(floor, na.rm = TRUE), floor)) %>%
  ungroup()
View(data2)
na_count_condition <- sum(is.na(data2$floor))
print(na_count_condition)
#Brak wartości NA



#6. Build Year
#Brakujące wartości uzupełnione na podstawie grupowania oraz wyliczania mediany wartości w tychże grupach. Do porównania zostały użyte zmienne floorCount, type, city.

data3 <- data2 %>%
  group_by(floorCount, type, city) %>%
  mutate(buildYear = ifelse(is.na(buildYear), median(buildYear, na.rm = TRUE), buildYear)) %>%
  ungroup()
View(data3)
na_count_condition <- sum(is.na(data3$buildYear))
print(na_count_condition)

#37 wartości NA, ponawiamy imputację danych za pomocą mediany, lecz tym razem dla całego zbioru danych (liczba brakujących obserwacji jest bardzo mała)

data4 <- data3 %>%
  mutate(buildYear = ifelse(is.na(buildYear), 
                            median(buildYear, na.rm = TRUE), 
                            buildYear))
na_count_condition <- sum(is.na(data4$buildYear))
print(na_count_condition)
#Brak wartości NA



#7.has Elevator
#Brakujące wartości zostaną uzupełnione z użyciem metody k-NN (k-Nearest Neighbors), która zastąpi nam brakujące wartości na podstawie sąsiadujących obserwacji, które są najbardziej podobne do brakującego wiersza

data5 <- kNN(data4, 
                variable = c("hasElevator"),
                k = 5, 
                weightDist = TRUE)
View(data5)

na_count_condition <- sum(is.na(data5$hasElevator))
print(na_count_condition)
#Brak wartości NA



#8. Distance - college, clinic, restaurant, pharmacy, postoffice, kindergarten, school
#Mała liczba wartości NA, imputacja może być wyliczana na podstawie mediany dla wszystkich obserwacji

data1 <- data5 %>%
  mutate(collegeDistance = ifelse(is.na(collegeDistance), median(collegeDistance, na.rm = TRUE), collegeDistance))

miss_var_summary(data1)

data2 <- data1 %>%
  mutate(clinicDistance = ifelse(is.na(clinicDistance), median(clinicDistance, na.rm = TRUE), clinicDistance))

miss_var_summary(data2)

data3 <- data2 %>%
  mutate(restaurantDistance = ifelse(is.na(restaurantDistance), median(restaurantDistance, na.rm = TRUE), restaurantDistance))

miss_var_summary(data3)

data4 <- data3 %>%
  mutate(pharmacyDistance = ifelse(is.na(pharmacyDistance), median(pharmacyDistance, na.rm = TRUE), pharmacyDistance))

miss_var_summary(data4)

data5 <- data4 %>%
  mutate(postOfficeDistance = ifelse(is.na(postOfficeDistance), median(postOfficeDistance, na.rm = TRUE), postOfficeDistance))

miss_var_summary(data5)

data6 <- data5 %>%
  mutate(kindergartenDistance = ifelse(is.na(kindergartenDistance), median(kindergartenDistance, na.rm = TRUE), kindergartenDistance))

miss_var_summary(data6)

data7 <- data6 %>%
  mutate(schoolDistance = ifelse(is.na(schoolDistance), median(schoolDistance, na.rm = TRUE), schoolDistance))
miss_var_summary(data7)

#BRAK WARTOŚCI ODSTAJĄCYCH W NASZYM ZBIORZE DANYCH






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

