#Library
library(tidyverse)
library(readr)
library(readxl)
library(xlsx)

faostat <- read_csv("OUTPUT_FAOSTAT.csv", 
                    na = "NA")
worldbank <- read_csv("OUTPUT_WorldBank.csv", 
                      na = "NA")

#FAOSTAT country aggregate--------------------
faostat$Country <- gsub("China, mainland", "China", faostat$Country)
faostat$Country <- gsub("Democratic Republic of the Congo", "Congo Rep", faostat$Country)
faostat$Country <- gsub("C<f4>te d'Ivoire", "Ivory Coast", faostat$Country)
faostat$Country <- gsub("Bolivia (Plurinational State of)", "Bolivia", faostat$Country)
faostat$Country <- gsub("Iran (Islamic Republic of)", "Iran", faostat$Country)
faostat$Country <- gsub("Lao People's Democratic Republic", "Lao", faostat$Country)
faostat$Country <- gsub("Republic of Korea", "South Korea", faostat$Country)
faostat$Country <- gsub("Republic of Moldova", "Moldova", faostat$Country)
faostat$Country <- gsub("United Republic of Tanzania", "Tanzania", faostat$Country)
faostat$Country <- gsub("United States of America", "United States", faostat$Country)
faostat$Country <- gsub("Venezuela (Bolivarian Republic of)", "Venezuela", faostat$Country)
faostat$Country <- gsub("Viet Nam", "Vietnam", faostat$Country)

#WorldBank country aggregate-------------------
worldbank$Country <- gsub("Bahamas, The", "Bahamas", worldbank$Country)
worldbank$Country <- gsub("Caribbean small states", "Caribbean", worldbank$Country)
worldbank$Country <- gsub("Congo, Dem. Rep.", "Congo", worldbank$Country)
worldbank$Country <- gsub("Congo, Rep.", "Congo Rep", worldbank$Country)
worldbank$Country <- gsub("Cote d'Ivoire", "Ivory Coast", worldbank$Country)
worldbank$Country <- gsub("Czech Republic", "Czechia", worldbank$Country)
worldbank$Country <- gsub("Egypt, Arab Rep.", "Egypt", worldbank$Country)
worldbank$Country <- gsub("Gambia, The", "Gambia", worldbank$Country)
worldbank$Country <- gsub("Iran, Islamic Rep.", "Iran", worldbank$Country)
worldbank$Country <- gsub("Kyrgyz Republic", "Kyrgyzstan", worldbank$Country)
worldbank$Country <- gsub("Lao PDR", "Lao", worldbank$Country)
worldbank$Country <- gsub("Korea, Rep", "South Korea", worldbank$Country)
worldbank$Country <- gsub("Slovak Republic", "Slovakia", worldbank$Country)
worldbank$Country <- gsub("Venezuela, RB", "Venezuela", worldbank$Country)
worldbank$Country <- gsub("Yemen, Rep.", "Yemen", worldbank$Country)

final <- merge(x=faostat, y=worldbank, by=c("Country", "Year"), all=TRUE) %>% 
  select(-3)

write.csv(final, file="FINAL_DATA.csv")

remove(faostat, worldbank)







