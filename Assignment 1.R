#Assignment
#Day2
#Vhuawelo
#29 january 2019

#loading libraries
library(tidyverse)
library(ggpubr)
library(boot)

co2 <- datasets::CO2 #assigned a name to the dataset
??co2                #Run on help to get more details about the dataset
               
plot1 <- ggplot(co2, aes(x=conc, y=uptake, colour=Type))+
  geom_point()+
  geom_line(aes(group=Plant))+
  geom_smooth(method="lm")+
  ggtitle("A")
#when co2 concentration increases, the plant's uptake also increases.

plot2<- ggplot(co2, aes(x=conc, y=uptake, colour=Type))+
  geom_point()+
  geom_line(aes(group=Plant))+
  geom_smooth(method="lm")+
  facet_wrap(~Type, ncol=4)
  
plot3 <- ggplot(co2, aes(x=Type, y=uptake))+
  geom_boxplot(aes(fill=Type))+
  labs(x="Type", y="uptake")+
  ggtitle("B")
#Quebec plants uptake more co2 compared to Mississippi plants.

plot_combine <- ggarrange(plot1, plot2, plot3)

#finding the mean of conc of each Type.
#first tell R which dataset to use.
#Group by each type

co2 %>%                               
  group_by(Type) %>%                  
  summarise(avrg_conc=mean(conc))

#assign a name to the dataset so that it appears in the envrinment.
#Run on help to get more details about the dataset

Orange <- datasets::Orange
??Orange

plot4 <- ggplot(Orange, aes(x=age, y=circumference, colour=Tree))+
  geom_point(colour="black")+
  geom_line(aes(group=Tree))+
  ggtitle("A")
#circumferance of the Tree increases as the tree grows over the age.

plot5 <- ggplot(Orange, aes(x=age, y=circumference, colour=Tree))+
  geom_point()+
  geom_line(aes(group=Tree))+
  facet_wrap(~Tree, ncol=4)+
  labs(x="age", y="circumference")
#The rate at which the circumference grows with age differs with each tree.

plot6 <- ggplot(Orange, aes(x=age, y=circumference))+
  geom_boxplot(aes(fill=Tree))+
  labs(x="age", y="circumference")+
  ggtitle("D")
#The rate at which the circumference grows with age differs with each tree.

#finding the mean of circumference of each Tree.
#first tell R which dataset to use.
#Group by each type

Orange %>%                               
  group_by(Tree) %>%                  
  summarise(avrg_circum=mean(circumference))

women <- datasets::women
??women

plot7 <- ggplot(women, aes(x=height, y=weight))+
  geom_point(colour="red")+
  geom_line()+
  labs(x="height(cm)", y="weight(kg)")+
  ggtitle("Vhuawelo")
#weight increases with height.

plot8 <- ggplot(women, aes(x=height, y=weight, colour=height))+
  geom_point()+
  geom_line()+
  labs(x="height(cm)", y="weight(kg)")+
  ggtitle("Vhuawelo")
  
lam <- read.csv("laminaria.csv")

lma1 <- ggplot(lam, aes(x=stipe_length, y=stipe_mass, colour=region))+
  geom_point()+
  geom_line(aes(group=region))+
  labs(x="stipe length", y="stipe mass")+
  ggtitle("A")+
  theme_bw()
#Stipe_mass increases with stipe_length
#wc regoin have long and havier stipes compared to FB region

lma2 <- ggplot(lam, aes(x=stipe_length, y=stipe_mass, colour=region))+
  geom_point()+
  geom_line()+
  facet_wrap(~region, ncol=2)

lma3 <- plot4 <- ggplot(lam, aes(x=region, y=stipe_length))+
  geom_boxplot(aes(fill=region))+
  labs(x="regoin", y="stipe_length (cm)")





  
  
  

  


  