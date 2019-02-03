#Day1
#laminaria dataset exploring and learning
#Vhuawelo/Nobuhle
#29 jan 2019

#loading libraries

library(tidyverse)
lam <- read_csv("laminaria.csv")
head(lam)#shows first six rows
tail(lam)
head(lam,n=3)#To show the fisrt 3 rows
tail(lam,n=3)

lam_select <- lam %>% 
  select(site,total_length) %>% 
slice(54:80)

lam_kom <- lam %>% 
  filter(site=="Kommetjie")

#R is case sensitive,stick to one case
#in the laminaria dataset select only the site and the blade_length column
#filter only the Sea Point

lam_try <- lam %>% 
select(site, blade_length) %>% 
filter(site=="Sea Point")

lam %>% 
  filter(total_length==max(total_length))

summary(lam)
dim(lam)

lam %>% 
  summarize(avrg_bl=mean(blade_length),
            med_bl=median(blade_length),
            sd_bl=sd(blade_length)) 
#don't use %>% beacuse it is the same function, instead use comma.

lam %>% 
  group_by(site) %>% 
  summarise(var_bl=var(blade_length),
            n=n()) %>% 
  mutate(se=sqrt(var_bl/n))
#mutate create a new column

lam_2 <- lam %>% 
  select(-blade_thickness, -blade_length)

lam %>%
  select(blade_length) %>%
  na.omit %>% 
  summarise(n=n())

 lam %>%
  select(stipe_mass) %>%
   na.omit %>% 
  summarise(n=n())
# na.omit excludes the columns with N/A meaning those that are empty.
 
 ggplot(data = lam, aes(x = stipe_mass, y = stipe_length)) +
   geom_point(shape = 21, colour = "blue", fill = "red") +
   labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")
# Drawing graphs
 
 total_lenth_half <- lam %>% 
   mutate(total_length_half=total_length / 2) %>% 
   filter(total_length_half < 100) %>% 
   select(site,total_length_half) 
   
 #Create a new data frame from the `laminaria` dataset that meets the following criteria: contains only the `site` column and a new column called `total_length_half` containing values that are half of the `total_length`. In this `total_length_half` column, there are no `NA`s and all values are less than 100.
 #think about how the commands should be ordered to produce this data frame!
 
 
 lam %>% 
   group_by(site) %>%
   summarise(avr_bl=mean(blade_length),
             min_bl=min(blade_length),
             max_bl=max(blade_length),
             n=n())
 #Use `group_by()` and `summarize()` to find the mean, min, and max blade_length for each site. Also add the number of observations (hint: see `?n`).

 lam %>%
   group_by(site) %>% 
  filter(stipe_mass==max(stipe_mass)) %>% 
   select(site,region,stipe_length)

  #What was the heaviest stipe measured in each site? Return the columns `site`, `region`, and `stipe_length`
 

 # [A.A]
 # Neat script
 # Some comments within the script, more comments would increase your marks
 # Neat script
 # Shows some undertanding of the code
 # Script runs
 