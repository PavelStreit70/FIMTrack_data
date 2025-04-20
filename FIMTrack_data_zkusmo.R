##########

#Načíst data, vybrat parametry a postupně jednotlivé hodnoty transportovat
#do GraphPadu

library(readr)
kontrola_1_varka <- read_csv("Rko/250324_1.csv")
View(kontrola_1_varka)

str(kontrola_1_varka)

#Knihovny

library(readxl)
library(dplyr)
library(tidyr)
library(lme4) #lmer
library(lmerTest)
library(multcomp) #Attaching package: ‘MASS’, dplyr::select masked
library(emmeans) #emmeans
library(effects) #allEffects
library(ggplot2)
library(nortest)  #normality testing 
library(car)   #Levene test
library(ggprism) #aby to vypadalo podobně jako když se používá Prism
library (psych)

#Vyhledání konkrétních parametrů

colnames(kontrola_1_varka)

#Název prvního sloupce je debilní - bacha na to!

kontrola_1_varka %>%
  filter(grepl("velocity", ...1)) %>% 
  summary()

#Lepší - vypíše to pouze průměry pro každou larvu

kontrola_1_varka %>%
  filter(grepl("velocity", ...1)) %>%           
  summarise(across(starts_with("larva"),                
                   ~mean(.x, na.rm = TRUE))) 

#Pokus o update - mělo by to vyhledat potřebný parametr a vypočítat mi z něj průměr
#NA hodnoty by tam neměly být... zítra pokračovat

keywords <- c("velocity", "acceleration")  # sem si napiš libovolné výrazy

kontrola_1_varka %>%
  filter(Reduce(`|`, lapply(keywords, function(k) grepl(k, ...1)))) %>%
  mutate(parametr = case_when(
    grepl("velocity", ...1) ~ "velocity",
    grepl("acceleration", ...1) ~ "acceleration",
  )) %>%
  group_by(parametr) %>%
  summarise(across(starts_with("larva"), ~mean(.x, na.rm = TRUE)))


#Vypsat celkovou uraženou vzdálenost acc_dist

####Ještě jednou

# Načti knihovnu pro práci s daty
library(dplyr)

# Načti data
data <- read.csv("Rko/250331_1.csv", check.names = FALSE)

# Přejmenuj první sloupec na 'parameter'
colnames(data)[1] <- "parameter"

# Vyfiltruj řádky s acc_dst
acc_dst_rows <- data %>% filter(grepl("acc_dst", parameter))

# Funkce na získání poslední nenulové / ne-NA hodnoty ve sloupci
get_last_valid <- function(column) {
  rev_column <- rev(column)
  last_value <- rev_column[!is.na(rev_column) & rev_column != 0][1]
  return(last_value)
}

# Aplikuj funkci na každý sloupec kromě 'parameter'
last_valid_values <- sapply(acc_dst_rows[ , -1], get_last_valid)

# Výpis
print(last_valid_values)

#Vypsat coiling a go phase - součty hodnot - 0 a 1

library(dplyr)

keywords <- c("coiling", "go_phase")  # Klíčová slova, která hledáme v textu

kontrola_1_varka %>%
  filter(Reduce(`|`, lapply(keywords, function(k) grepl(k, ...1)))) %>%  # Filtrovat podle klíčových slov
  mutate(parametr = case_when(
    grepl("coiling", ...1) ~ "coiling",  # Pokud obsahuje "coiling", označíme jako "coiling"
    grepl("go_phase", ...1) ~ "go_phase"  # Pokud obsahuje "go_phase", označíme jako "go_phase"
  )) %>%
  group_by(parametr) %>%  # Skupina podle "parametr"
  summarise(across(starts_with("larva"), ~mean(.x, na.rm = TRUE)))  # Průměr pro každý "larva" sloupec


#Přidat podmínku s gophase - hýbaly se aspoň 50% času - půjde to/dáva to smysl?
#Tuhle podmínku nechat na konec, pokud na ni zbyde čas
