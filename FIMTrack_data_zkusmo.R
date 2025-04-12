##########

#Načíst data, vybrat parametry a postupně jednotlivé hodnoty transportovat
#do GraphPadu

library(readr)
kontrola_1_varka <- read_csv("Rko/outputs/Prvni varka/output_2025-03-27_17-11-54/table.csv")
View(kontrola_1_varka)

str(kontrola_1_varka)

#Knihovny

library(readxl)
library(dplyr)
library(tidyr)
library(lme4)# lmer
library(lmerTest)
library(multcomp) # Attaching package: ‘MASS’, dplyr::select masked
library(emmeans) # emmeans
library(effects) # allEffects
library(ggplot2)
library(nortest)  # normality testing 
library(car)   # Levene test
library(ggprism)




