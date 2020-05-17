#Library------------------------------
library(tidyverse)
library(readr)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(forecast)
library(plm)

final1 <- read_csv("Summary_all.csv", 
                  na = "NA")

###           Model 1 - Raw       ###-------------- 
#y = Fertilizer_use
#x = GDP_capita, Employ_per, Urban_pop, dev_assist, Crop_area

m1 <- final1 %>% 
  filter(Year>=2002) %>% 
  filter(Year<=2016) %>% 
  select(2,3,5,15,20,32,34) %>% 
  na.omit()

m1.p <- pdata.frame(m1, index=c("Country", "Year"))

#Pooling 
m1.pooling <- plm(Fertilizer_use~GDP_capita, data=m1.p, model="pooling")
stargazer(m1.pooling, type='text')
vif(m1.pooling)

#Pooling_v2
m1.pooling_1 <- plm(Fertilizer_use~GDP_capita+Employ_per+Urban_pop+dev_assist+Crop_area, 
                  data=m1.p, model="pooling")
stargazer(m1.pooling, type='text')
vif(m1.pooling_1)

#Fixed effect model
m1.fixed <- plm(Fertilizer_use~GDP_capita+Employ_per, 
             data=m1.p, model="within")
stargazer(m1.fixed, type='text')

#Random effect model
m1.random <- plm(Fertilizer_use~GDP_capita+Employ_per, 
                data=m1.p, model="random")
stargazer(m1.random, type='text')

#Hauseman test
m1.hauseman <- phtest(m1.fixed, m1.random)
m1.hauseman

###           Model 2 - Log       ###-------------- 
#y = Fertilizer_use
#x = GDP_capita, Employ_per, Urban_pop, Crop_export_quantity

m2 <- final1 %>% 
  filter(Year>=2002) %>% 
  filter(Year<=2016) %>% 
  select(2,3,5,15,20, 34, 32) %>% 
  na.omit() %>% 
  mutate(lg_fer=log(Fertilizer_use), lg_urban=log(Urban_pop), 
         lg_crop_exp=log(Crop_exp_quantity), lg_gdp=log(GDP_capita))

m2.p <- pdata.frame(m2, index=c("Country", "Year"))

any(!sapply(m2.p, is.finite))

#Pooling 
m2.pooling <- plm(lg_fer~lg_gdp+lg_crop_exp+lg_urban+Employ_per, 
                    data=m2.p, model="pooling")
stargazer(m2.pooling, type='text')
vif(m2.pooling)

#Fixed effect model
m2.fixed <- plm(Fertilizer_use~GDP_capita+Crop_exp_quantity+Urban_pop+
                    Employ_per, data=m2.p, model="within")
stargazer(m2.fixed, type='text')

#Random effect model
m2.random <- plm(Fertilizer_use~Crop_exp_quantity,
                   data=m2.p, model="random")
stargazer(m2.random, type='text')

#Hauseman test
m1.hauseman <- phtest(m1.fixed, m1.random)
m1.hauseman






