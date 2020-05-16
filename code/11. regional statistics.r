#Library-------------
library(tidyverse)
library(readr)
library(ggplot2)
library(readxl)
library(ggpubr)

reg0 <- read_csv("BASELINE.csv",
                 na = "NA")

reg1 <- reg0 %>% 
  select(2,3,13)

reg2 <- reg1 %>% 
  filter(Year=="2008")

reg3 <- reg2 %>% 
  group_by(Region) %>% 
  summarise(count=n())

#Excel Filter
reg.e <- reg0 %>% 
  select(2,3,13,7,8) %>% 
  group_by(Year, Region) %>% 
  summarise(Yield=sum(Crop_yield), Area=sum(Crop_area)) %>% 
  ungroup()

rge.e.eap <- reg.e %>% 
  filter(Region=="East Asia & Pacific")
rge.e.sa <- reg.e %>% 
  filter(Region=="South Asia")
rge.e.eca <- reg.e %>% 
  filter(Region=="Europe & Central Asia")
rge.e.mena <- reg.e %>% 
  filter(Region=="Middle East & North Africa")
rge.e.ssa <- reg.e %>% 
  filter(Region=="Sub-Saharan Africa")
rge.e.noa <- reg.e %>% 
  filter(Region=="North America")
rge.e.lac <- reg.e %>% 
  filter(Region=="Latin America & Caribbean")

#GGPLOT
EAP <- read_excel("Relative Change - GRAPH2.xlsx", 
                  sheet = "East Asia & Pacific")
SA <- read_excel("Relative Change - GRAPH2.xlsx", 
                 sheet = "South Asia")
ECA <- read_excel("Relative Change - GRAPH2.xlsx", 
                  sheet = "Europe & Central Asia")
MENA <- read_excel("Relative Change - GRAPH2.xlsx", 
                   sheet = "Middle East & North Africa")
SSA <- read_excel("Relative Change - GRAPH2.xlsx", 
                  sheet = "Sub-Saharan Africa")
NAM <- read_excel("Relative Change - GRAPH2.xlsx", 
                  sheet = "North America")
LAC <- read_excel("Relative Change - GRAPH2.xlsx", 
                  sheet = "Latin America & Caribbean")

a <- ggplot(EAP, aes(x=Year))+
  geom_line(aes(y=Yeild_r), color="Orange", size=1)+
  geom_line(aes(y=Area_r), color="Orange", linetype="dashed", size=1)+
  labs(title="East Asia & Pacific", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

b <- ggplot(SA, aes(x=Year))+
  geom_line(aes(y=Yeild_r), color="Blue", size=1)+
  geom_line(aes(y=Area_r), color="Blue", linetype="dashed", size=1)+
  labs(title="South Asia", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

c <- ggplot(ECA, aes(x=Year))+
  geom_line(aes(y=Yeild_r), color="Red", size=1)+
  geom_line(aes(y=Area_r), color="Red", linetype="dashed", size=1)+
  labs(title="Europe & Central Asia", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

d <- ggplot(MENA, aes(x=Year))+
  geom_line(aes(y=Yeild_r), color="Purple", size=1)+
  geom_line(aes(y=Area_r), color="Purple", linetype="dashed", size=1)+
  labs(title="Middle East & North Africa", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

e <- ggplot(SSA, aes(x=Year))+
  geom_line(aes(y=Yeild_r), color="Yellow3", size=1)+
  geom_line(aes(y=Area_r), color="Yellow3", linetype="dashed", size=1)+
  labs(title="Sub-Saharan Africa", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

f <- ggplot(NAM, aes(x=Year))+
  geom_line(aes(y=Yeild_r), color="Lightskyblue4", size=1)+
  geom_line(aes(y=Area_r), color="Lightskyblue4", linetype="dashed", size=1)+
  labs(title="North America", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)  

g <- ggplot(LAC, aes(x=Year))+
  geom_line(aes(y=Yeild_r), color="Green3", size=1)+
  geom_line(aes(y=Area_r), color="Green3", linetype="dashed", size=1)+
  labs(title="Latin America & Caribbean", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)    

ggarrange(a,b,c,d,e,f,g,
          ncol = 2, nrow = 4)

