#weekend homework
#Vhuawelo Simba
#02 february 2019
# Section 3

#load libraries
library(tidyverse)
library(ggsn)
library(scales)
library(ggpubr)
library(lubridate)

SACTN <- read_csv("SACTN_day_1.csv") #loading data by using read_csv because the data is saved in csv file and assigning a name to it so it appears in my environment.


#usinging summary to get the overview of my data.
summary(SACTN)
head(SACTN,n=5) #shows the fisrt 10 rows
tail(SACTN,n=2) #shows the last 5 rows
glimpse(SACTN)

# Here create a graph showing temperature variation between sites #group by site
ggplot(data = SACTN, aes(x = date, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site))) +
  labs(x = "", y = "Temperature (°C)", colour = "Site") +
  theme_bw()

# Select all the temperatures recorded at the site Port Nolloth during August or September

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 08 | month(date) == 09)

# Select all the monthly temperatures recorded in Port Nolloth during the year 1994
SACTN %>% 
  filter(site == "Port Nolloth", year(date) == 1994)




    
