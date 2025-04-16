##########

#Načíst data, vybrat parametry a postupně jednotlivé hodnoty transportovat
#do GraphPadu

library(readr)
kontrola_1_varka <- read_csv("Rko/outputs/Prvni varka/output_2025-03-27_17-11-54/250324_1.csv")
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


#Přidat podmínku s gophase - hýbaly se aspoň 50% času - půjde to/dáva to smysl?
