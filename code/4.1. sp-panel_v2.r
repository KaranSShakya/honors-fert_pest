#Library------------------------------
library(tidyverse)
library(readr)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(forecast)
library(plm)

#Importing
final1 <- read_csv("Summary_all.csv", 
                   na = "NA")

#Model 1 - Fertilizer (2002 - 2016) -------------------------
m1 <- final1 %>% 
  filter(Year>=2002) %>% 
  filter(Year<=2016) %>% 
  select(2,3,5,15) %>%  
  filter(Fertilizer_use>0) %>% 
  na.omit()

m1.p <- pdata.frame(m1, index=c("Country", "Year"))

#Fixed effect model
m1.fixed1 <- plm(Fertilizer_use~GDP_capita, 
                data=m1.p, model="within")
stargazer(m1.fixed1, type='text')

#Random effect model
m1.random <- plm(Fertilizer_use~GDP_capita, 
                 data=m1.p, model="random")
stargazer(m1.random, type='text')

#Hauseman
phtest(m1.fixed1, m1.random)

