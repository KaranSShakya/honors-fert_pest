#Library ------------------------------------------
library(tidyverse)
library(readr)
library(readxl)
library(xlsx)

#Testing Merge - FAOSTAT vs World Bank (Countries) --------------------------
# worldbank <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-GDP-1960_2018.csv", 
#                       na = "NA") %>% 
#   select(1, 6)
# names(worldbank)[1] <- "Area"
# worldbank <- cbind(worldbank, a=1) 
#   
# faostat <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-foreign-1991_2018.csv", 
#                     na = "NA") %>% 
#   select(2,8)
# 
# faostat2 <- faostat %>% 
#   group_by(Area) %>% 
#   summarise_each(funs(n_distinct(.)))
# faostat2 <- cbind(faostat2, a=1)
# 
# test3 <- merge(x=worldbank, y=faostat2, by=c("Area", "a"), all=TRUE)
# 
# write.xlsx(worldbank, file="worldbank.xlsx")
# write.xlsx(faostat2, file="faostat.xlsx")



#Pesticide Use-----------------------------
pest0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-pesticide_use-1997_2017.csv", 
                  na = "NA")

pest1 <- pest0 %>% 
  filter(Item=="Pesticides (total)")
  
pest_F <- pest1 %>% 
  select(2,8,10)
names(pest_F)[1] <- "Country"
names(pest_F)[2] <- "Year"
names(pest_F)[3] <- "Pesticide_use" #tonnes

remove(pest0, pest1)


#Pesticide trade-----------------------------------------------------------
pest_trade0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-pesticide_trade-1961-2017.csv", 
                        na = "NA")
pest_trade1 <- pest_trade0 %>% 
  filter(Item=="Pesticides (total)")

ptrade_impd_F <- pest_trade1 %>% 
  filter(Element=="Import Value") %>% 
  select(2,8,10)
names(ptrade_impd_F)[1] <- "Country"
names(ptrade_impd_F)[3] <- "Pesticide_imp_value"

ptrade_expd_F <- pest_trade1 %>% 
  filter(Element=="Export Value") %>% 
  select(2,8,10)
names(ptrade_expd_F)[1] <- "Country"
names(ptrade_expd_F)[3] <- "Pesticide_exp_value"

remove(pest_trade0, pest_trade1)


#Fertilizer use-----------------------------
fert0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-fertilizer_use-2002_2017.csv", 
                  na = "NA")

  #Contains agricultural use, Import/Export quantity, Production
fert1 <- fert0 %>% 
  select(2,6,8,10) %>% 
  group_by(Area, Element, Year) %>% 
  summarise(Sum=sum(Value, na.rm=TRUE)) %>%  #tonnes
  ungroup()

names(fert1)[1] <- "Country"

  #Agriculture use
fert_F <- fert1 %>% 
  filter(Element =="Agricultural Use") %>%  
  select(-2)
names(fert_F)[3] <- "Fertilizer_use"

fert_F2 <- fert1 %>% 
  filter(Element == "Import Quantity") %>% 
  select(-2)
names(fert_F2)[3] <- "Fertilizer_imp_quantity"

fert_F3 <- fert1 %>% 
  filter(Element=="Export Quantity") %>% 
  select(-2)
names(fert_F3)[3] <- "Fertilizer_exp_quantity"

fert_F4 <- fert1 %>% 
  filter(Element=="Production") %>% 
  select(-2)
names(fert_F4)[3] <- "Fertilizer_production"

remove(fert0, fert1)


#Energy use-------------------------------------------
energy0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-energyuse-1970_2002.csv", 
                    na = "NA")
energy1 <- energy0 %>% 
  select(2,4,6,8,9,10) #terajoule

energy_F <- energy1 %>% 
  filter(Item=="Electricity") %>% 
  filter(Element=="Consumption in Agriculture") %>%
  filter(Unit=="million kWh") %>% 
  select(-2,-3,-5)  
names(energy_F)[3] <- "Electricty_use"  

energy_F2 <- energy1 %>% 
  filter(Item=="Gas-Diesel oil") %>% 
  filter(Element=="Consumption in Agriculture") %>%  
  select(-2,-3,-5) 
names(energy_F2)[3] <- "GasDieselOil_use"  

remove(energy0, energy1)  

#Government Expenditure---------------------------------
govt0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-govtexp-2001_2018.csv", 
                  na = "NA")

govt_F <- govt0 %>% 
  filter(Element=="Value US$") %>% 
  filter(Item=="Agriculture, forestry, fishing (Central Government)") %>% 
  select(2,8,10)

names(govt_F)[1] <- "Country"
names(govt_F)[3] <- "Govt_ag"

remove(govt0)


#Private Bank Credit-----------------------------------------------
private0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-credit-1991_2018.csv", 
                     na = "NA")
private1 <- private0 %>% 
  filter(Item=="Credit to Agriculture, Forestry and Fishing") %>% 
  filter(Element=="Value US$")

private_F <- private1 %>% 
  select(2,8,10) #millions US$
names(private_F)[1] <- "Country"
names(private_F)[3] <- "Private_ag"

remove(private0, private1)

#Foreign Direct Investment-------------------------------------
foreign0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-foreign-1991_2018.csv", 
                     na = "NA")
foreign1 <- foreign0 %>% 
  filter(Item=="FDI inflows to Agriculture, Forestry and Fishing") %>% 
  filter(Element=="Value US$")

foreign2 <- foreign0 %>% 
  filter(Item=="FDI outflows to Agriculture, Forestry and Fishing") %>% 
  filter(Element=="Value US$")

foreign_F <- foreign1 %>% 
  select(2,8,10)
names(foreign_F)[1] <- "Country"
names(foreign_F)[3] <- "Foreign_inflow" #millions (US$)

foreign_F2 <- foreign2 %>% 
  select(2,8,10)
names(foreign_F2)[1] <- "Country"
names(foreign_F2)[3] <- "Foreign_outflow" #millions (US$)

remove(foreign0, foreign1, foreign2)

#GDP per Capita------------------------------------------------------------
gdp0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-GDP-1970_2017.csv", 
                 na = "NA")
gdp1 <- gdp0 %>% 
  filter(Element=="Value US$") %>% 
  filter(Item=="Gross Domestic Product per capita")

gdp_F <- gdp1 %>% 
  select(2,8,10)
names(gdp_F)[1] <- "Country"
names(gdp_F)[3] <- "GDP_capita"
  
remove(gdp0, gdp1)

#Crops (Area Harvested, Yields, Production)--------------------------
crop0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-crops-1961_2017.csv", 
                  na = "NA")
crop1 <- crop0 %>% 
  select(2,6,8,10) 
names(crop1)[1] <- "Country"

crop_F <- crop1 %>% 
  filter(Element=="Yield") %>% #hg/ha
  select(-2) %>% 
  group_by(Country, Year) %>% 
  summarise(Yield=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
names(crop_F)[3] <- "Crop_yield"

crop_F2 <- crop1 %>% 
  filter(Element=="Area harvested") %>%  #ha
  select(-2) %>% 
  group_by(Country, Year) %>% 
  summarise(Area_harvested=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
names(crop_F2)[3] <- "Crop_area"

crop_F3 <- crop1 %>% 
  filter(Element=="Production") %>%  #tonnes
  select(-2) %>% 
  group_by(Country, Year) %>% 
  summarise(Production=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
names(crop_F3)[3] <- "Crop_production"

remove(crop0, crop1)

#Crop (Trade - Price and Quantity - Import / Export)--------------------------------------
agtrade0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-agtrade-1961-2017.csv", 
                     na = "empty")
agtrade1 <- agtrade0 %>% 
  select(2,4,6,8,9,10)

agexport1 <- agtrade1 %>% 
  filter(Element=="Export Value") #1000 US$   
agexport_F <- agexport1 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_exp_value=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
agimport1 <- agtrade1 %>% 
  filter(Element=="Import Value")
agimport_F <- agimport1 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_imp_value=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
names(agexport_F)[1] <- "Country"
names(agimport_F)[1] <- "Country"

agexport2 <- agtrade1 %>% 
  filter(Element=="Export Quantity") #tonnes
agexport_F2 <- agexport2 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_exp_quantity=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
agimport2 <- agtrade1 %>% 
  filter(Element=="Import Quantity") #tonnes
agimport_F2 <- agimport2 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_imp_quantity=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
names(agexport_F2)[1] <- "Country"
names(agimport_F2)[1] <- "Country"

remove(agtrade0, agtrade1, agexport1, agexport2, agimport1, agimport2)

#Machinery ------------------------------
mach0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-machinery.csv", 
                     na = "empty")
mach1 <- mach0 %>% 
  select(2,4,6,8,9,10)

agexport1 <- agtrade1 %>% 
  filter(Element=="Export Value") #1000 US$   
agexport_F <- agexport1 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_exp_value=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
agimport1 <- agtrade1 %>% 
  filter(Element=="Import Value")
agimport_F <- agimport1 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_imp_value=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
names(agexport_F)[1] <- "Country"
names(agimport_F)[1] <- "Country"

agexport2 <- agtrade1 %>% 
  filter(Element=="Export Quantity") #tonnes
agexport_F2 <- agexport2 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_exp_quantity=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
agimport2 <- agtrade1 %>% 
  filter(Element=="Import Quantity") #tonnes
agimport_F2 <- agimport2 %>% 
  select(1,4,6) %>% 
  group_by(Area, Year) %>% 
  summarise(Crop_imp_quantity=sum(Value, na.rm=TRUE)) %>% 
  ungroup()
names(agexport_F2)[1] <- "Country"
names(agimport_F2)[1] <- "Country"

remove(agtrade0, agtrade1, agexport1, agexport2, agimport1, agimport2)




#     World Bank
#Agricultural Area (% of Total Area)-------------------------------------
agland0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-agland-1961-2016.csv", 
                    na = "NA")
agland1 <- agland0 %>% 
  select(-2,-3,-4)
agland_F <- agland1 %>% 
  gather(key="Year", value="Agland_per", 2:61) %>% 
  filter(Year <= 2016) %>% 
  filter(Year >= 1961)
names(agland_F)[1] <- "Country"

remove(agland0, agland1)

#Rural Population (% of Total Pop)----------------------------------------
rural0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-ruralpop-1961-2018.csv", 
                   na = "NA")
rural1 <- rural0 %>% 
  select(-2,-3,-4,-64)
rural_F <- rural1 %>% 
  gather(key="Year", value="Ruralpop_per", 2:60)
names(rural_F)[1] <- "Country"

remove(rural0, rural1)

#Employment female (% of total female employment)--------------------------
female0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-employ_female-1960-2018.csv", 
                    na = "NA")
female1 <- female0 %>% 
  select(-2,-3,-4)
female_F <- female1 %>% 
  gather(key="Year", value="Female", 2:61)
names(female_F)[1] <- "Country"
names(female_F)[3] <- "Female_per"

remove(female0, female1)

#Employment male (% of total male employment)--------------------------
male0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-employ_male-1960-2018.csv", 
                  na = "NA")
male1 <- male0 %>% 
  select(-2,-3,-4)
male_F <- male1 %>% 
  gather(key="Year", value = "Male", 2:61)

names(male_F)[1] <- "Country"
names(male_F)[3] <- "Male_per"

remove(male0, male1)


#Population total---------------------------
pop.t0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-population_total.csv")
pop.t1 <- pop.t0 %>% 
  select(-2,-3,-4)
pop.tF <- pop.t1 %>% 
  gather(key="Year", value="Pop_t", 2:61)

names(pop.tF)[1] <- "Country"
names(pop.tF)[3] <- "Pop_total"

remove(pop.t0, pop.t1)

#Male-population------------------------------
male.p0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-pop_male.csv")
male.p1 <- male.p0 %>% 
  select(-2,-3,-4)
male.pF <- male.p1 %>% 
  gather(key="Year", value="Male_popu", 2:61)

names(male.pF)[1] <- "Country"

remove(male.p0, male.p1)

#Female-population------------------------------
female.p0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-pop_female.csv")
female.p1 <- female.p0 %>% 
  select(-2,-3,-4)
female.pF <- female.p1 %>% 
  gather(key="Year", value="Female_popu", 2:61)

names(female.pF)[1] <- "Country"

remove(female.p0, female.p1)

#Urban Population (total)
urban0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-urbanpop.csv")
urban1 <- urban0 %>% 
  select(-2,-3,-4)
urbanF <- urban1 %>% 
  gather(key="Year", value="Urban_pop", 2:61)

names(urbanF)[1] <- "Country"

remove(urban0, urban1)

#Development assistance
dev_assist0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-dev-assist.csv")
dev_assist1 <- dev_assist0 %>% 
  select(-2,-3,-4)
dev_assistF <- dev_assist1 %>% 
  gather(key="Year", value="dev_assist", 2:61)

names(dev_assistF)[1] <- "Country"

remove(dev_assist0, dev_assist1)

#Rural Population (absolute)
Rural_a0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/WorldBank-ruralpop.csv")
Rural_a1 <- Rural_a0 %>% 
  select(-2,-3,-4)
Rural_aF <- Rural_a1 %>% 
  gather(key="Year", value="Rural_a", 2:61)

names(Rural_aF)[1] <- "Country"

remove(Rural_a0, Rural_a1)

#Mach tracktor
mach_track0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Economics Honors Research/Data Downloads/FAOSTAT-mach_track.csv")

mach_track1 <- mach_track0 %>% 
  select(-2,-3,-4)
mach_trackF <- mach_track1 %>% 
  gather(key="Year", value="Mach_track", 2:61)

names(mach_trackF)[1] <- "Country"

remove(mach_track0, mach_track1)


  ################

#COMBINING-------------------------------------------------------------------

merge <- merge(x=pest_F, y=fert_F, by=c("Country", "Year"), all=TRUE)  
merge <- merge(x=merge, y=fert_F2, by=c("Country", "Year"), all=TRUE)  
merge <- merge(x=merge, y=fert_F3, by=c("Country", "Year"), all=TRUE)
merge <- merge(x=merge, y=fert_F4, by=c("Country", "Year"), all=TRUE)
merge <- merge(x=merge, y=energy_F, by=c("Country", "Year"), all=TRUE)  
merge <- merge(x=merge, y=energy_F2, by=c("Country", "Year"), all=TRUE)
merge <- merge(x=merge, y=govt_F, by=c("Country", "Year"), all=TRUE)  
merge <- merge(x=merge, y=private_F, by=c("Country", "Year"), all=TRUE)
merge <- merge(x=merge, y=foreign_F, by=c("Country", "Year"), all=TRUE)
merge <- merge(x=merge, y=foreign_F2, by=c("Country", "Year"), all=TRUE)

merge2 <- merge(x=merge, y=gdp_F, by=c("Country", "Year"), all=TRUE)
merge2 <- merge(x=merge2, y=crop_F, by=c("Country", "Year"), all=TRUE)
merge2 <- merge(x=merge2, y=crop_F2, by=c("Country", "Year"), all=TRUE)
merge2 <- merge(x=merge2, y=crop_F3, by=c("Country", "Year"), all=TRUE)
merge2 <- merge(x=merge2, y=agexport_F, by=c("Country", "Year"), all=TRUE)
merge2 <- merge(x=merge2, y=agexport_F2, by=c("Country", "Year"), all=TRUE)
merge2 <- merge(x=merge2, y=agimport_F, by=c("Country", "Year"), all=TRUE)
merge2 <- merge(x=merge2, y=agimport_F2, by=c("Country", "Year"), all=TRUE)

merge3 <- merge(x=merge2, y=ptrade_impd_F, by=c("Country", "Year"), all=TRUE)
merge3 <- merge(x=merge3, y=ptrade_expd_F, by=c("Country", "Year"), all=TRUE)

merge_w <- merge(x=agland_F, y=rural_F, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=female_F, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=male_F, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=pop.tF, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=male.pF, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=female.pF, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=urbanF, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=dev_assistF, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=Rural_aF, by=c("Country", "Year"), all=TRUE)
merge_w <- merge(x=merge_w, y=mach_trackF, by=c("Country", "Year"), all=TRUE)

write.csv(merge3, file="OUTPUT_FAOSTAT.csv")
write.csv(merge_w, file="OUTPUT_WorldBank.csv")



