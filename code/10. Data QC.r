#Library
library(tidyverse)
library(readr)
library(ggplot2)
library(readxl)
library(ggpubr)

#Regions Classification
class0 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-Classification.xls")
class1 <- class0 %>% 
  select(3,6)
names(class1)[1] <- "Country"

class1$Country <- gsub("Bahamas, The", "Bahamas", class1$Country)
class1$Country <- gsub("Caribbean small states", "Caribbean", class1$Country)
class1$Country <- gsub("Congo, Dem. Rep.", "Congo", class1$Country)
class1$Country <- gsub("Congo, Rep.", "Congo Rep", class1$Country)
class1$Country <- gsub("Cote d'Ivoire", "Ivory Coast", class1$Country)
class1$Country <- gsub("Czech Republic", "Czechia", class1$Country)
class1$Country <- gsub("Egypt, Arab Rep.", "Egypt", class1$Country)
class1$Country <- gsub("Gambia, The", "Gambia", class1$Country)
class1$Country <- gsub("Iran, Islamic Rep.", "Iran", class1$Country)
class1$Country <- gsub("Kyrgyz Republic", "Kyrgyzstan", class1$Country)
class1$Country <- gsub("Lao PDR", "Lao", class1$Country)
class1$Country <- gsub("Korea, Rep", "South Korea", class1$Country)
class1$Country <- gsub("Slovak Republic", "Slovakia", class1$Country)
class1$Country <- gsub("Venezuela, RB", "Venezuela", class1$Country)
class1$Country <- gsub("Yemen, Rep.", "Yemen", class1$Country)

#Importing Selected Countries
final1 <- read_csv("Summary_all.csv", 
                   na = "NA")

final2 <- final1 %>% 
  filter(Year>=2002) %>% 
  filter(Year<=2016) %>% 
  select(2,3,4,5,15,16,17,20,22,26,36)

final3 <- final2 %>% 
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
           Country == "Brunei Darussalam" |
           Country == "Bulgaria" |
           Country == "Burkina Faso" |
           Country == "Burundi" |
           Country == "Cameroon" |
           Country == "Canada" |
           Country == "Chile" |
           Country == "China" |
           Country == "Colombia" |
           Country == "Costa Rica" |
           Country == "Croatia" |
           Country == "Cyprus" |
           Country == "Denmark" |
           Country == "Dominican Republic" |
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
           Country == "Ivory Coast" |
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
           Country == "New Zealand" |
           Country == "Nicaragua" |
           Country == "Niger" |
           Country == "Norway" |
           Country == "Oman" |
           Country == "Panama" |
           Country == "Papua New Guinea" |
           Country == "Paraguay" |
           Country == "Peru" |
           Country == "Poland" |
           Country == "Portugal" |
           Country == "Romania" |
           Country == "Russian Federation" |
           Country == "Samoa" |
           Country == "Saudi Arabia" |
           Country == "Senegal" |
           Country == "Slovenia" |
           Country == "South Africa" |
           Country == "Spain" |
           Country == "Sri Lanka" |
           Country == "Sweden" |
           Country == "Switzerland" |
           Country == "Syrian Arab Republic" |
           Country == "Thailand" |
           Country == "Tonga" |
           Country == "Trinidad and Tobago" |
           Country == "Tunisia" |
           Country == "Turkey" |
           Country == "Uganda" |
           Country == "Ukraine" |
           Country == "United Kingdom" |
           Country == "United States" |
           Country == "Uruguay" |
           Country == "Vietnam" |
           Country == "Yemen" |
           Country == "Zambia" |
           Country == "Zimbabwe")

final5 <- merge(x=final3, y=class1, by="Country") %>% 
  na.omit()

final6 <- final5 %>%
  mutate(lg_pesticide=log(Pesticide_use), lg_fertilizer=log(Fertilizer_use),
         lg_gdp=log(GDP_capita), lg_area=log(Crop_area),
         lg_export=log(Crop_exp_quantity), lg_rural=Ruralpop_per) %>% 
  select(-3:-10)

write.csv(final6, file="FINAL_BASELINE.csv")


