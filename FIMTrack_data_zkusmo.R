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

kontrola_1_varka %>%
  filter(grepl("acc_dst", `...1`)) %>%  # Filtrování podle parametrů
  slice_tail(n = 1)  # Získání posledního řádku

kontrola_1_varka %>%
  filter(grepl("acc_dst", `...1`)) %>%  # Filtrovat řádky, kde je 'acc_dst'
  mutate(acc_dst_value = as.numeric(str_extract(`...1`, "(?<=acc_dst=)[0-9.]+"))) %>%  # Extrahujeme číslo za 'acc_dst='
  filter(!is.na(acc_dst_value)) %>%  # Odstraníme řádky s NA
  summarise(last_acc_dst = tail(acc_dst_value, 1))  # Získáme poslední hodnotu

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
#Tuhle podmínku nechat na konec
