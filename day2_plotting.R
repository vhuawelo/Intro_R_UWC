# plotting in R using ggplot2
# Day 2
# vhuawelo
# 30th jan 2019

# load libraries
library(tidyverse)

chicks <- datasets::ChickWeight
??ChickWeight

ggplot(data=chicks, aes(x=Time, y=weight))+
  geom_point()+
  geom_line(aes(group=Chick))
#we use + instead of %>% when plotting graphs.

ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point()+
  geom_line(aes(group=Chick))

ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point()+
  geom_smooth(method="lm") 
#"lm"=linear model

ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point(colour="red")+
  geom_line(aes(group=Chick))

ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point(aes(size=weight))+
  geom_smooth(method="lm")+
  labs(x="Days", y="weight(kg)")+
  ggtitle("Vhuawelo")+
  theme_bw()

cars <- datasets::cars 
#Assign a name to a build in dataset you want to work with.

ggplot(cars, aes(x=dist, y=speed))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")+
  ggtitle("vhuawelo")

# facetting in ggplot
library(ggpubr)

ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Diet, ncol=2)
#ncol is the number of columns 
#facet_wrap splited the graph that each diet have its own graph

Chicks_2 <- chicks %>% 
  filter(Time ==21)

plot_1 <- ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point()+
  geom_line(aes(group=Chick))+
labs(x="Days", y="weight")+
  ggtitle("A")
plot_1

plot_2 <- ggplot(chicks, aes(x=Time, y=weight, colour=Diet))+
  geom_point()+
  geom_smooth(method="lm")+
  ggtitle("B")
plot_2

plot3 <- ggplot(data=Chicks_2, aes(x=weight))+
  geom_histogram(aes(fill=Diet), position = "dodge", binwidth = 100)+
  labs(x="Final mass (g)", y="count")
plot3
#when plotting histogram, dodge seperates your bars.

plot4 <- ggplot(data=Chicks_2, aes(x=Diet, y=weight))+
  geom_boxplot(aes(fill=Diet))+
  labs(x="Diet", y="Final mass (g)")
plot4

plot_combined <- ggarrange(plot_1, plot_2, plot3, plot4) 
#Use ggarrange to combine different graphs.
#ggarrange funtion is found in ggpubr library.

#3rd library
library(boot)

urine <- boot::urine
??urine

urine %>% 
  select(-cond)
#to remove a column you use the select function then -(minus) that column name.

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond))

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r)))
