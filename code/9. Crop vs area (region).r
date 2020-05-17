#Library-------------
library(tidyverse)
library(readr)
library(ggplot2)
library(readxl)
library(ggpubr)

final1 <- read_csv("Summary_all.csv", 
                   na = "NA")

#Filter country---------------
final2 <- final1 %>% 
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

#Country region classification---------------
class0 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-Classification.xls")
class1 <- class0 %>% 
  select(3,6)
names(class1)[1] <- "Country"

final3 <- merge(x=final2, y=class1, by="Country")

remove(class0, class1, final1, final2)

#Filter-------------
final4 <- final3 %>% 
  filter(Year>=2002) %>% 
  filter(Year<=2016) 

#%>%
  select(3,16,17,37)

final5 <- final4 %>% 
  group_by(Year, Region) %>% 
  summarise(Yield=sum(Crop_yield), Area=sum(Crop_area)) %>% 
  ungroup()

final6 <- final5 %>% 
  filter(Region=="Sub-Saharan Africa")

remove(final3, final4, final6)

#Import Region Relative------------------
EAP <- read_excel("Relative Change - GRAPH.xlsx", 
                  sheet = "East Asia & Pacific")
SA <- read_excel("Relative Change - GRAPH.xlsx", 
                  sheet = "South Asia")
ECA <- read_excel("Relative Change - GRAPH.xlsx", 
                 sheet = "Europe & Central Asia")
MENA <- read_excel("Relative Change - GRAPH.xlsx", 
                  sheet = "Middle East & North Africa")
SSA <- read_excel("Relative Change - GRAPH.xlsx", 
                  sheet = "Sub-Sahara Africa")
NAM <- read_excel("Relative Change - GRAPH.xlsx", 
                  sheet = "North America")
LAC <- read_excel("Relative Change - GRAPH.xlsx", 
                  sheet = "Latin America & Caribbean")


#GGplot--------------
a <- ggplot(EAP, aes(x=Year))+
  geom_line(aes(y=Yield_r), color="Orange", size=1)+
  geom_line(aes(y=Area_r), color="Orange", linetype="dashed", size=1)+
  labs(title="East Asia & Pacific", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

b <- ggplot(SA, aes(x=Year))+
  geom_line(aes(y=Yield_r), color="Blue", size=1)+
  geom_line(aes(y=Area_r), color="Blue", linetype="dashed", size=1)+
  labs(title="South Asia", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

c <- ggplot(ECA, aes(x=Year))+
  geom_line(aes(y=Yield_r), color="Red", size=1)+
  geom_line(aes(y=Area_r), color="Red", linetype="dashed", size=1)+
  labs(title="Europe & Central Asia", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

d <- ggplot(MENA, aes(x=Year))+
  geom_line(aes(y=Yield_r), color="Purple", size=1)+
  geom_line(aes(y=Area_r), color="Purple", linetype="dashed", size=1)+
  labs(title="Middle East & North Africa", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

e <- ggplot(SSA, aes(x=Year))+
  geom_line(aes(y=Yield_r), color="Yellow3", size=1)+
  geom_line(aes(y=Area_r), color="Yellow3", linetype="dashed", size=1)+
  labs(title="Sub-Saharan Africa", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)

f <- ggplot(NAM, aes(x=Year))+
  geom_line(aes(y=Yield_r), color="Lightskyblue4", size=1)+
  geom_line(aes(y=Area_r), color="Lightskyblue4", linetype="dashed", size=1)+
  labs(title="North America", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)  

g <- ggplot(LAC, aes(x=Year))+
  geom_line(aes(y=Yield_r), color="Green3", size=1)+
  geom_line(aes(y=Area_r), color="Green3", linetype="dashed", size=1)+
  labs(title="Latin America & Caribbean", x="Year", y="Relative Change")+
  theme_bw(base_size = 12)+
  coord_cartesian(ylim=c(80, 140))+
  scale_y_continuous(breaks = seq(80, 140, 10))+
  scale_x_continuous(breaks = seq(2002, 2016, 2))+
  geom_segment(aes(x=2002, y=100, xend=2016, yend=100), size=0.5)    

ggarrange(a,b,c,d,e,f,g,
          ncol = 2, nrow = 4)





  









            


