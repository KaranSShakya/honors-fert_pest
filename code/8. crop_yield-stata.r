#Library-------------
library(tidyverse)
library(readr)
library(stargazer)
library(knitr)
library(xtable)
library(ggplot2)
library(ggpubr)

#Panel regression
library(plm)

#Importing data
final1 <- read_csv("Summary_all.csv", 
                   na = "NA")

m1 <- final1 %>% 
  filter(Year>=2002) %>% 
  filter(Year<=2016) %>% 
  select(2, 3, 4, 5, 16) %>% 
  na.omit()

#Country select-------------------
m1.country <- m1 %>% 
  filter(Country == "Albania" |
           Country == "Algeria" |
           Country == "Angola" |
           Country == "Argentina" |
           Country == "Armenia" |
           Country == "Australia" |
           Country == "Austria" |
           Country == "Azerbaijan" |
           Country == "Bangladesh" |
           Country == "Belarus" |
           Country == "Belize" |
           Country == "Bhutan" |
           Country == "Brazil" |
           Country == "BruneiDarussalam" |
           Country == "Bulgaria" |
           Country == "BurkinaFaso" |
           Country == "Burundi" |
           Country == "Cameroon" |
           Country == "Canada" |
           Country == "Chile" |
           Country == "China" |
           Country == "Colombia" |
           Country == "CostaRica" |
           Country == "Croatia" |
           Country == "Cyprus" |
           Country == "Denmark" |
           Country == "DominicanRepublic" |
           Country == "Ecuador" |
           Country == "Egypt" |
           Country == "ElSalvador" |
           Country == "Estonia" |
           Country == "Fiji" |
           Country == "Finland" |
           Country == "France" |
           Country == "Germany" |
           Country == "Ghana" |
           Country == "Greece" |
           Country == "Guatemala" |
           Country == "Hungary" |
           Country == "India" |
           Country == "Indonesia" |
           Country == "Ireland" |
           Country == "Israel" |
           Country == "Italy" |
           Country == "IvoryCoast" |
           Country == "Jamaica" |
           Country == "Japan" |
           Country == "Jordan" |
           Country == "Kazakhstan" |
           Country == "Kenya" |
           Country == "Kyrgyzstan" |
           Country == "Lebanon" |
           Country == "Libya" |
           Country == "Lithuania" |
           Country == "Madagascar" |
           Country == "Malawi" |
           Country == "Malaysia" |
           Country == "Malta" |
           Country == "Mauritius" |
           Country == "Mexico" |
           Country == "Moldova" |
           Country == "Morocco" |
           Country == "Mozambique" |
           Country == "Myanmar" |
           Country == "Namibia" |
           Country == "Nepal" |
           Country == "Netherlands" |
           Country == "NewZealand" |
           Country == "Nicaragua" |
           Country == "Niger" |
           Country == "Norway" |
           Country == "Oman" |
           Country == "Panama" |
           Country == "PapuaNewGuinea" |
           Country == "Paraguay" |
           Country == "Peru" |
           Country == "Poland" |
           Country == "Portugal" |
           Country == "Romania" |
           Country == "RussianFederation" |
           Country == "Samoa" |
           Country == "SaudiArabia" |
           Country == "Senegal" |
           Country == "Slovenia" |
           Country == "SouthAfrica" |
           Country == "Spain" |
           Country == "SriLanka" |
           Country == "Sweden" |
           Country == "Switzerland" |
           Country == "SyrianArabRepublic" |
           Country == "Thailand" |
           Country == "Tonga" |
           Country == "TrinidadandTobago" |
           Country == "Tunisia" |
           Country == "Turkey" |
           Country == "Uganda" |
           Country == "Ukraine" |
           Country == "UnitedKingdom" |
           Country == "UnitedStates" |
           Country == "Uruguay" |
           Country == "Vietnam" |
           Country == "Yemen" |
           Country == "Zambia" |
           Country == "Zimbabwe")

#Creating log------------------------------
m1.log <- m1.country %>% 
  mutate(Lg_crop=log(Crop_yield), Lg_fert=log(Fertilizer_use), Lg_pest=log(Pesticide_use))

m1.log1 <- m1.log %>% 
  select(-3,-5,-4)

pdim(m1.log1)

write.csv(m1.log1, file="crop-stata.csv")

