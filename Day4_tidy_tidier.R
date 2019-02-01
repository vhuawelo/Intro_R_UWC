#Tidy data 2.0
#Day4
#vhuawelo simba
#01 february 2019

# Load libraries
library(tidyverse)
library(lubridate)

# Load the data from a .RData file, you don't use read_by because it is already converted into Rdata file
load("SACTNmonthly_v4.0.RData")

# assign a new name to the data set to aviod typing in the long name all the time. 
SACTN <- SACTNmonthly_v4.0

# Rm removes data from the environment.
rm(SACTNmonthly_v4.0)

SACTN %>% 
  filter(site == "Amanzimtoti") #extract from the site column Amanzimtoti

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1) #filter extracs out a certain point from the data.

SACTN %>% 
  arrange(depth, temp) #arrange arranges columns you selected from the lowest to the highest value.

SACTN %>% 
  arrange(desc(temp))  #arrange arranges columns you selected from the highest to the lowest value.

SACTN %>% 
  filter(site == "Humewood", year(date) == 1991)

# Select columns individually by name
try1 <- SACTN %>% 
  select(site, src, date, temp)

try2 <- SACTN %>% 
  select(site:temp) 
#select columns from site to temperature.

try3 <- SACTN %>% 
  select(-date, -depth)
#to exludes columns use select and -before the column you want to exclude.
#Remember not to put the minus sign behind select.

try4 <- SACTN %>% 
  select(-(date:depth))
#excludes columns from date to depth

try5 <- SACTN %>% 
  mutate(kelvin = temp + 273.15)
#creating a new column called kelvin which is the value of temperature+273.15

try6 <- SACTN %>% 
  mutate(kelvin=temp/2)
#creating a new column called kelvin which is the value of temperature devided by 2


SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = T),
            min_temp = min(temp, na.rm = T),
            max_temp = max(temp, na.rm = T)
  )
#within summarise function you have mini function like mean,sd,min etc.











