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
install.packages("knitr")
install.packages("gtsummary")
install.packages("corrplot")

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(editrules)
library(VIM)
library(corrplot)
library(deducorrect)
library(ISLR)
library(outliers)
library(naniar)
library(ggplot2)
library(plotly)
library(knitr)
library(gtsummary)

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



#Wizualizacja braków danych
vis_miss(data, cluster = TRUE, sort_miss = TRUE)



#wykres przedstawiający wzorce braków danych
gg_miss_upset(data)



# Grupowanie braków według miasta
data %>% 
  group_by(city) %>% 
  miss_var_summary() %>% 
  View(data)




#Sprawdzenie wartości odstających

library(ggplot2)
library(patchwork)

wykres1 <- ggplot(data, aes(y = centreDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot dystans do centrum")

wykres2 <- ggplot(data, aes(y = schoolDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot dystans do szkoły")

combined_plots <- wykres1 + wykres2 
combined_plots

wykres3 <- ggplot(data, aes(y = clinicDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot dystans do kliniki")


wykres4 <- ggplot(data, aes(y = postOfficeDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot dystans do poczty")

combined_plots <- wykres3 + wykres4 
combined_plots

wykres5 <- ggplot(data, aes(y = kindergartenDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot dystans do przedszkola")

wykres6 <- ggplot(data, aes(y = restaurantDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot dystans do restauracji")

combined_plots <- wykres5 + wykres6
combined_plots

wykres7 <- ggplot(data, aes(y = collegeDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot dystans do uniwersytetu")

wykres8 <- ggplot(data, aes(y = pharmacyDistance)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot liczba dystans do apteki")

combined_plots <- wykres7 + wykres8 
combined_plots

#Imputacja danych

#1. Condition

data1Condition <- data %>% 
  mutate(condition = ifelse(is.na(condition), "Standard", condition))
View(data1Condition)
na_count_condition <- sum(is.na(data1Condition$condition))
print(na_count_condition)




#2. Building Material 
data2Condition.buildingMaterial <- data1Condition %>% 
  mutate(buildingMaterial = ifelse(is.na(buildingMaterial), "Material is not known/different building material", buildingMaterial))
View(data2Condition.buildingMaterial)
na_count_condition <- sum(is.na(data2Condition.buildingMaterial$buildingMaterial))
print(na_count_condition)



#3.Type


dataCBT <- kNN(data2Condition.buildingMaterial,
                    variable = c("type"),        
                    k = 5,                       
                    weightDist = TRUE)          
View(dataCBT)

na_count_condition <- sum(is.na(dataCBT$type))
print(na_count_condition)




#4. Floor Count 
dataCBTFC <- dataCBT %>%
  group_by(city, type, condition) %>%  
  mutate(floorCount = ifelse(is.na(floorCount), median(floorCount, na.rm = TRUE), floorCount)) %>%
  ungroup()
View(dataCBTFC)
na_count_condition <- sum(is.na(dataCBTFC$floorCount))
print(na_count_condition)




#5.Floor 
data2 <- dataCBTFC %>%
  group_by(floorCount, type) %>%
  mutate(floor = ifelse(is.na(floor), median(floor, na.rm = TRUE), floor)) %>%
  ungroup()
View(data2)
na_count_condition <- sum(is.na(data2$floor))
print(na_count_condition)




#6. Build Year
data3 <- data2 %>%
  group_by(floorCount, type, city) %>%
  mutate(buildYear = ifelse(is.na(buildYear), median(buildYear, na.rm = TRUE), buildYear)) %>%
  ungroup()
View(data3)
na_count_condition <- sum(is.na(data3$buildYear))
print(na_count_condition)



data4 <- data3 %>%
  mutate(buildYear = ifelse(is.na(buildYear), 
                            median(buildYear, na.rm = TRUE), 
                            buildYear))
na_count_condition <- sum(is.na(data4$buildYear))
print(na_count_condition)




#7.has Elevator

data5 <- kNN(data4, 
                variable = c("hasElevator"),
                k = 5, 
                weightDist = TRUE)
View(data5)

na_count_condition <- sum(is.na(data5$hasElevator))
print(na_count_condition)




#8. Distance - college, clinic, restaurant, pharmacy, postoffice, kindergarten, school


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



# Wykres 1: Histogram cen mieszkań
p1 <- ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Rozkład cen mieszkań", x = "Cena (PLN)", y = "Liczba mieszkań") +
  theme_minimal()
ggplotly(p1)

# Wykres 2: Scatter plot - Odległość od centrum vs cena
p2 <- ggplot(data, aes(x = centreDistance, y = price)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Odległość od centrum a cena", 
       x = "Odległość od centrum (km)", 
       y = "Cena (PLN)") +
  theme_minimal() +
  facet_wrap(~ city)
ggplotly(p2)

# Wykres 3: Cena w zależności od odległości od uczelni
p3 <- ggplot(data, aes(x = collegeDistance, y = price)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm") +
  labs(title = "Cena w zależności od odległości od uczelni", 
       x = "Odległość od uczelni (km)", 
       y = "Cena (PLN)") +
  theme_minimal() +
  facet_wrap(~ city)
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

# Wykres 7: Cena w zależności od liczby pięter w budynku
p7 <- ggplot(data, aes(x = as.factor(floorCount), y = price)) +
  geom_boxplot(fill = "lightpink", outlier.color = "darkred") +
  labs(title = "Cena w zależności od liczby pięter w budynku", x = "Liczba pięter", y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p7)

# Wykres 8: Cena w zależności od roku budowy
p8 <- ggplot(data, aes(x = buildYear, y = price)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm") +
  labs(title = "Cena w zależności od roku budowy", 
       x = "Rok budowy", 
       y = "Cena (PLN)") +
  theme_minimal()
ggplotly(p8)

#Analiza opisowa
#Obliczenie statystyk opisowych dla ceny mieszkania w zależności od miast, liczby pokoi oraz typu budynku
#Tabela 1 Rozkład cen w zależności od liczby pokoi
data7 %>%
  select(price, rooms) %>%
  tbl_summary(
    by = rooms,  # Grupowanie wg liczby pokoi
    type = all_continuous() ~ "continuous2",  # Określenie typu zmiennej ciągłej
    statistic = all_continuous() ~ c(
      "{N_nonmiss}",  # Liczba niebrakujących wartości
      "{mean}",        # Średnia
      "{sd}",          # Odchylenie standardowe
      "{median} ({p25}, {p75})",  # Mediana i kwartyle
      "{min}, {max}"   # Minimalna i maksymalna wartość
    ),
    missing = "no",  # Brakujące dane ignorowane
    label = price ~ "Cena"  # Zmiana nazwy etykiety dla kolumny 'price'
  ) %>%
  modify_header(label ~ "**Zmienna**") %>%  # Nagłówek dla etykiety zmiennej
  modify_caption("**Tabela 1. Rozkład cen wg liczby pokoi**") %>%  # Nagłówek tabeli
  bold_labels() %>%  # Pogrubienie etykiet
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) 

#Tabela 2. Rozkład cen w zależności od miejscowości
data7 %>%
  select(price, city) %>%
  tbl_summary(
    by = city,  # Grupowanie wg miejscowości
    type = all_continuous() ~ "continuous2",  # Określenie typu zmiennej ciągłej
    statistic = all_continuous() ~ c(
      "{N_nonmiss}",  # Liczba niebrakujących wartości
      "{mean}",        # Średnia
      "{sd}",          # Odchylenie standardowe
      "{median} ({p25}, {p75})",  # Mediana i kwartyle
      "{min}, {max}"   # Minimalna i maksymalna wartość
    ),
    missing = "no",  # Brakujące dane ignorowane
    label = price ~ "Cena"  # Zmiana nazwy etykiety dla kolumny 'price'
  ) %>%
  modify_header(label ~ "**Zmienna**") %>%  # Nagłówek dla etykiety zmiennej
  modify_caption("**Tabela 2. Rozkład cen w zależności od miejscowości**") %>%  # Nagłówek tabeli
  bold_labels() %>%  # Pogrubienie etykiet
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))

#Tabela 3 Rozkład cen w zależności od typu mieszkania
data7 %>%
  select(price, type) %>%
  tbl_summary(
    by = type,  # Grupowanie wg miejscowości
    type = all_continuous() ~ "continuous2",  # Określenie typu zmiennej ciągłej
    statistic = all_continuous() ~ c(
      "{N_nonmiss}",  # Liczba niebrakujących wartości
      "{mean}",        # Średnia
      "{sd}",          # Odchylenie standardowe
      "{median} ({p25}, {p75})",  # Mediana i kwartyle
      "{min}, {max}"   # Minimalna i maksymalna wartość
    ),
    missing = "no",  # Brakujące dane ignorowane
    label = price ~ "Cena"  # Zmiana nazwy etykiety dla kolumny 'price'
  ) %>%
  modify_header(label ~ "**Zmienna**") %>%  # Nagłówek dla etykiety zmiennej
  modify_caption("**Tabela 3. Rozkład cen w zależności od typu **") %>%  # Nagłówek tabeli
  bold_labels() %>%  # Pogrubienie etykiet
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))  


#WNIOSKOWANIE STATYCZNE 

#przeprowadzenie testu ANOVA - sprawdzenie czy średnie ceny różnią się między miastami
anova_model <-aov(price ~ city, data = data7)
summary(anova_model)

if(summary(anova_model)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_model)
  print(tukey_result)}

#Przeprowadzenie testu ANOVA, aby sprawdzić, czy średnie ceny różnią się między typami mieszkań
anova_type <- aov(price ~ type, data = data7)
summary(anova_type)


tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# Przeprowadzenie testu ANOVA, aby sprawdzić, czy średnie ceny różnią się między typami mieszkań
anova_type <- aov(price ~ type, data = data7)
summary(anova_type)

if (summary(anova_type)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_type <- TukeyHSD(anova_type)
  print(tukey_type)
}


# Korelacja między ceną a odległością od uczelni
cor_test_college <- cor.test(data7$price, data7$collegeDistance, method = "pearson")
print(cor_test_college)



# Korelacja między ceną a odległością od centrum
cor_test_centre <- cor.test(data7$price, data7$centreDistance, method = "pearson")
print(cor_test_centre)


# Korelacja między ceną a rokiem budowy
cor_test_buildYear <- cor.test(data7$price, data7$buildYear, method = "pearson")
print(cor_test_buildYear)


# Przeprowadzenie testu ANOVA, aby sprawdzić, czy średnie ceny różnią się między stanami mieszkań
anova_condition <- aov(price ~ condition, data = data7)
summary(anova_condition)


if (summary(anova_condition)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_condition <- TukeyHSD(anova_condition)
  print(tukey_condition)
}

ggplot(data7, aes(x = condition, y = price, fill = condition)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Średnie ceny mieszkań w zależności od stanu", x = "Stan mieszkania", y = "Cena (PLN)") +
  theme_minimal()

# Tworzenie tabeli kontyngencji dla miasta i typu mieszkania
contingency_table <- table(data7$city, data7$type)

# Przeprowadzenie testu Chi-kwadrat
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)
print(chi_square_result$expected)
mosaicplot(contingency_table, main = "Mozaikowy wykres zależności", color = TRUE, las = 3)

# Przeprowadzenie testu t-Studenta, aby porównać ceny mieszkań z windą i bez windy
t_test_elevator <- t.test(price ~ hasElevator, data = data7)
print(t_test_elevator)


ggplot(data7, aes(x = hasElevator, y = price, fill = hasElevator)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Porównanie cen mieszkań z windą i bez windy",
       x = "Czy mieszkanie ma windę?",
       y = "Cena (PLN)") +
  theme_minimal()


# Test Kruskala-Wallisa do porównania cen między miastami
kruskal_result <- kruskal.test(price ~ city, data = data7)
print(kruskal_result)

pairwise.wilcox.test(data7$price, data7$city, p.adjust.method = "bonferroni")

ggplot(data7, aes(x = city, y = price, fill = city)) +
  # geom_boxplot(outlier.color = "blue") +
  labs(title = "Porównanie cen mieszkań w różnych miastach",
       x = "Miasto",
       y = "Cena (PLN)") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Wykorzystanie pakietu ggstatsplot
install.packages("ggstatsplot")
library(ggstatsplot)
library(dplyr)

top_cities <- data7 %>%
  group_by(city) %>%
  summarise(mean_price = mean(price)) %>%
  arrange(desc(mean_price)) %>%
  slice(1:5) # Ograniczenie do 5 miast z najwyższymi cenami

filtered_data <- data7 %>%
  filter(city %in% top_cities$city)

ggbetweenstats(
  data = filtered_data,
  x = city,
  y = price,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant", # Tylko istotne porównania
  mean.plotting = FALSE,            # Wyłącz dolny wykres średnich
  p.adjust.method = "bonferroni",
  ggtheme = ggplot2::theme_minimal(),
  title = "Porównanie cen mieszkań w wybranych miastach"
) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

#Dodanie testów zgodności rozkładu
install.packages("nortest")
library(nortest)
lillie.test(data7$price) # Test Lillieforsa dla ceny

#Analiza wielowymiarowa – rozszerzenie modelu regresji
lm_interaction <- lm(price ~ rooms * buildYear, data = data7)
summary(lm_interaction)
plot(lm_model) # Wykresy diagnostyczne

#Przykłady rozszerzonych analiz i wizualizacji
#Porównanie odległości od uczelni i od centrum w kontekście cen mieszkań
ggscatterstats(
  data = data7,
  x = collegeDistance,
  y = price,
  title = "Korelacja między ceną a odległością od uczelni",
  xlab = "Odległość od uczelni (km)",
  ylab = "Cena (PLN)")


