#weekend homework
#Vhuawelo Simba
#02 february 2019
# Section 4

#load libraries

library(tidyverse)
library(ggpubr)
library(scales)
library(ggsn)
library(ggplot2)

Orange <- datasets::Orange #loading data from built in data base by using datasets:: and select the data i want, then assign a name to the dataset selected.

Orange %>% #specifying the data being used.
  group_by(Tree) %>% #group by Tree using the function Group_by
  summarise(var_cir=var(circumference),#calculating the circumference's variance using the summarise function.
            n=n()) %>% 
  mutate(circumference=(var_cir*2)) #creating a new column wich includes the value of circumference's varience multiplied by 2.

Orange%>% #specifying the data being used.
  group_by(Tree) %>% #group by Tree using the function Group_by
  filter(circumference==min(circumference)) %>% #using filter to select only the row that contains the minimum value of the circumference.
  select(Tree, circumference) #including only Tree and circumference columns.

co2 <- datasets::CO2 #load data from built in data base by using datasets:: and select the data i want, then assign a name to the dataset selected.

co2 %>% 
  group_by(Type) %>%
  summarise(avr_bl=mean(uptake),
            min_bl=min(uptake),
            max_bl=max(uptake),
            n=n()) 

co2_chilled <- co2 %>% #assigning a new name to the dataset.
  filter(conc==95, uptake>10) %>% #selecting only rows that have the concentration of 95 and the co2 uptake is greater than 10.
  select(Plant,uptake,conc) #selecting only plant, uptake and conc columns

co2_chilled %>% #specifying that co2_chilled is the data being used.
  group_by(Plant) %>% #grouping by plant using the Goup_by fiunction.
  summarise(var_max=max(uptake, na.rm =TRUE)) #claculating the uptake maximum value and exluding entries that are NA/empty.

#using Scatter graph to see the relationship between the age of the Tree and the circumference of the tree.
#conclusion:when age increases, the circumference of the tree also increases.

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point() + #plotting the points on the graph.
  scale_x_continuous("age", breaks = seq(0,0.35,0.05))+ 
  scale_y_continuous("circumference", breaks = seq(0,270,by = 20))+ 
  theme_bw() 

#creating a heat map to see the uptake of co2 by plants from each site in two different treatment.

ggplot(co2, aes(Treatment, Type))+ 
  geom_raster(aes(fill = uptake))+
  labs(title ="uptake Heat Map", x = "Treament", y = "Type")+
  scale_fill_continuous(name = "uptake")
#The darker potion indicates the lowest co2 uptake and the lighter potion indicates the highest co2 uptake.
#co2 uptake is much higher in Quebec, expecially in a non chilled treatment.
#co2 uptake is lowest in a chilled treatment in mississippi.

  
    

