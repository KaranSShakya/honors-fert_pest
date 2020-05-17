#Library
library(tidyverse)
library(readr)
library(ggplot2)
library(readxl)
library(ggpubr)

data0 <- read_csv("BASELINE.csv",
                 na = "NA")

#Scatterplot - Fertilizer + Pesticide (2016)---------
data1 <- data0 %>% 
  select(2,3,5,6, 13)
data2 <- data1 %>%
  filter(Year=="2016")

ggplot(data2, aes(x=log(GDP_capita), y=log(Fertilizer_use), color=Region))+
  geom_point()+
  geom_text(aes(label=Country), hjust=1, vjust=0)+
  labs(title="Fertilizer Use (2016)", x="Log(GDP per Capita)", y="Log(Fertilizer Use)")+
  theme_classic(base_size = 12)+
  coord_cartesian(xlim=c(5.5, 11.2))

data3 <- data0 %>% 
  select(2,3,4,6, 13)
data4 <- data3 %>%
  filter(Year=="2016")

ggplot(data4, aes(x=log(GDP_capita), y=log(Pesticide_use), color=Region))+
  geom_point()+
  geom_text(aes(label=Country), hjust=1, vjust=0)+
  labs(title="Pesticide Use (2016)", x="Log(GDP per Capita)", y="Log(Fertilizer Use)")+
  theme_classic(base_size = 12)+
  coord_cartesian(xlim=c(5.5, 11.2))

#Other Variable Trend--------------
region <- data0 %>% 
  select(-1,-2,-10)

region$Region <- gsub("Europe & Central Asia", "Europe", region$Region)
region$Region <- gsub("Middle East & North Africa", "Africa", region$Region)
region$Region <- gsub("Sub-Saharan Africa", "Africa", region$Region)
region$Region <- gsub("Latin America & Caribbean", "America", region$Region)
region$Region <- gsub("East Asia & Pacific", "Asia", region$Region)
region$Region <- gsub("South Asia", "Asia", region$Region)
region$Region <- gsub("North America", "America", region$Region)


region1 <- region %>%
  group_by(Year, Region) %>% 
  summarise(a_pest=mean(Pesticide_use), a_fert=mean(Fertilizer_use),
            a_area=mean(Crop_area), a_exp=mean(Crop_exp_quantity),
            a_rur=mean(Rural_a), a_emp=mean(Employ_per),
            a_yield=mean(Crop_yield)) %>% 
  ungroup()

ggplot(region1, aes(x=Year, y=log(a_fert), color=Region))+
  geom_line()

ggplot(region1, aes(x=Year, y=log(a_pest), color=Region))+
  geom_line()

ggplot(region1, aes(x=Year, y=log(a_area), color=Region))+
  geom_line()

ggplot(region1, aes(x=Year, y=log(a_exp), color=Region))+
  geom_line()

ggplot(region1, aes(x=Year, y=log(a_rur), color=Region))+
  geom_line()

ggplot(region1, aes(x=Year, y=log(a_emp), color=Region))+
  geom_line()

ggplot(region1, aes(x=Year, y=log(a_yield), color=Region))+
  geom_line()
