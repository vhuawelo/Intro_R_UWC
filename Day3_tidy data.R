#Tidy data
#Day4
#vhuawelo simba
#01 february 2019


load("SACTN_mangled.RData")
library(tidyverse)

ggplot(SACTN1, aes(x=date, y=temp))+
  geom_line(aes(colour=site, group=paste0(site, src)))+ #paste0 when you want to group more than one variables
  labs(x="date", y="Temp")

SACTN2_tidy <- SACTN2 %>%           
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") #We use Gather(to squish columns together)
                                                        
SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val) 

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") #use seperate to seperate one column into more. 

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-") #we use unite to combine different columns into 1.

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy) #combining different datasets into one data set.


