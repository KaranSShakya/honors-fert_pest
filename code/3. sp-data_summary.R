#Library
library(tidyversee)
library(readr)
library(readxl)

#Importing data
final <- read_csv("FINAL_DATA.csv", 
                  na = "NA") %>% 
  select(-1,-25)

#Data construcion
final1 <- final %>% 
  mutate(Employ_per=((Female_per+Male_per)/200)*100)

#Data summary
year_sum <- final1 %>% 
  group_by(Year) %>% 
  summarise_all(funs(sum(!is.na(.))))

country_sum <- final1 %>% 
  group_by(Country) %>% 
  summarise_all(funs(sum(!is.na(.))))


write.csv(final1, file="Summary_all.csv")
write.csv(year_sum, file="Summary_year.csv")
write.csv(country_sum, file="Summary_country.csv")
  


  