#Library-------------
library(tidyverse)
library(readr)
library(stargazer)
library(knitr)
library(xtable)
library(ggplot2)
library(ggpubr)

#Panel regression
library(sandwich)
library(lmtest)
library(AER)
library(forecast)
library(plm)

#Importing data (1970 - 2000)
final1 <- read_csv("Summary_all.csv", 
                   na = "NA")

m1 <- final1 %>% 
  filter(Year>=1970) %>% 
  filter(Year<=2000) %>% 
  select(2, 3, 35, 15, 20,22, 34, 17) %>% 
  na.omit()

#Country select (Omit Zambia + Libya)
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
           Country == "Zimbabwe")


#Log construction
m1.log <- m1.country %>% 
  mutate(Lg_gdp=log(GDP_capita), 
         Lg_export=log(Crop_exp_quantity), Lg_import=log(Crop_imp_quantity),
         Lg_rural=log(Rural_a), Lg_area=log(Crop_area))

m1.log1 <- m1.log %>% 
  select(-4,-5,-6,-7,-8)

#Balenced panel test
pdim(m1.log1)

#Export excel
write.csv(m1.log1, file="mach_stata.csv")
