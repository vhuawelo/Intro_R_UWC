#Execersize 2
#Intro_R course
#Vhuawelo Simba
#02 february 2019

#load libraries
library(tidyverse)
library(lubridate)
library(ggsn)
library(scales)
library(ggpubr)

grDevices::colors() #colour pallet

SACTN <- load("SACTNmonthly_v4.0.RData") #assigning name to the data frame.

SACTN <- SACTNmonthly_v4.0 

temp1 <- SACTN %>% #assigning name to the data frame so it appears on my enivronment
  filter(src=="KZNSB") %>% #using filter function to filter out only rows of KZNSB source.
  separate(col = date, into = c("year", "month","day"), sep = "-") %>% 
  group_by(site, year) %>% #grouping by site and year
  na.omit() #removing the empty entries 

ggplot(data=temp1, aes(x=year,y=temp))+ #assigning a name to the plot, then specify the data being used, then using aes to specify the x and y values.
  geom_line(aes(group=site),colour="yellow")+ #joining the pointswith a line and grouping them by sites then select the line colour.
  facet_wrap(~site,nrow=9)+ #separating each site to have its own graph
  labs(x="year",y="temperature")+ #labeling my x and y axis
  scale_x_discrete(breaks = c(1980, 2000), labels = c("1980","2000")) #positioning the X axis scale.

lam <-read.csv("laminaria.csv") #loading a csv file using the funtion read.csv 

lam2 <- lam %>% #assigning name to the data frame.
  filter(region == "FB") %>% #pulling rows from region FB only
  group_by(site) #gruoping by site
  
lam_FB1 <- ggplot(data=lam2, aes(x=blade_length,y=blade_weight, colour=site))+ #assigning a name to the plot, then specify the data being used, then using aes to specify the x and y values.
  scale_colour_brewer(palette= "Accent")+ #adding accent palette to exclude roman Rock plots
  geom_point()+ #plotting points 
  geom_line(aes(group=site))+ #joining points by line and grouping them by site.
  facet_wrap(~site,nrow=3)+ #separating each site to have its own graph
  labs("x=Blade length(cm)", y="Blade mass")+ #labeling my x and y axis
  ggtitle("A crazy graph of some data for False Bay sites") #Adding title to the graph

lam_FB2 <- ggplot(data=lam2, aes(x=blade_length,y=blade_weight, colour=site))+ #assigning a name to the plot, then specify the data being used, then using aes to specify the x and y values.
  geom_point()+ #plotting points 
  geom_line(aes(group=site))+ #joining points by line and grouping them by site.
  facet_wrap(~site,nrow=3)+ ##separating each site to have its own graph
  labs("x=Blade length(cm)", y="Blade mass")+ #labeling my x and y axis
  ggtitle("A crazy graph of some data for False Bay sites") #Adding title to the graph

lam_FB3 <- ggarrange(lam_FB1,lam_FB2) #Combining the two graphs as two sub-plots
 
  

Toothgrowth <- datasets::ToothGrowth %>% #assigned a name to the dataset
  group_by(supp,dose) %>% #grouping by supp and dose
  summarise(avr_len=mean(len), #calculating the len mean
            sd_len=sd(len)) #calculating the len sd


Toothgrowth1 <- ggplot(Toothgrowth, aes(x=dose, y=avr_len, fill=supp))+ #assigning a name to the plot, then specify the data being used, then using aes to specify the x and y values and filling the bars by supp.
  geom_col(aes(fill=supp), position = "dodge", colour= "black" )+ #filling each bar by supp
  geom_errorbar(aes(ymin=avr_len-sd_len, #specifiying the +/- of the sd on the bars.
                    ymax=avr_len+sd_len),
                position="dodge")+
  labs(x="Dose (mg/d)", y="Tooth length(mm)") #labeling my x and y axis
  

   











                     


