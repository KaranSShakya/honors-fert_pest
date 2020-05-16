#Library
library(tidyverse)
library(readr)
library(stargazer)
library(knitr)
library(xtable)
library(ggplot2)
library(ggpubr)
library(readxl)

#Panel regression
library(sandwich)
library(lmtest)
library(AER)
library(forecast)
library(plm)

#Importing Baseline table
data0 <- read_csv("BASELINE.csv", na = "NA")

data1 <- data0 %>% 
  select(2,3,4,6,8,9,10,11,12,13)

data1.log <- data1 %>% 
  mutate(Lg_pesticide=log(Pesticide_use), Lg_gdp=log(GDP_capita), 
         Lg_export=log(Crop_exp_quantity), Lg_import=log(Crop_imp_quantity),
         Lg_rural=log(Rural_a), Lg_area=log(Crop_area)) %>% 
  select(-3:-8)

#Panel Regression
m1 <- pdata.frame(data1.log, index=c("Country", "Year"))

m1.fixed <- plm(Lg_pesticide~Lg_gdp+I(Lg_gdp^2)+Lg_area+Lg_export+Lg_rural+Employ_per, 
                data=m1, model="within", effect = "twoway")
stargazer(m1.fixed, type='text')

stargazer(coeftest(m1.fixed, vcovHC), type="text")

m1.random <- plm(Lg_fertilizer~Lg_gdp+I(Lg_gdp^2)+Lg_area+Lg_export+Lg_rural+Employ_per, 
                 data=m1, model="random")
stargazer(m1.random, type='text')

phtest(m1.fixed, m1.random)

#Region Filter
Asia.table <- data1.log %>% 
  filter(Region=="East Asia & Pacific" | Region=="South Asia")
Europe.table <- data1.log %>% 
  filter(Region=="Europe & Central Asia")
America.table <- data1.log %>% 
  filter(Region=="Latin America & Caribbean" | Region=="North America") 
Africa.table <- data1.log %>% 
  filter(Region=="Middle East & North Africa" | Region=="Sub-Saharan Africa")

write.csv(Africa.table, file="pest-africa_final.csv")
