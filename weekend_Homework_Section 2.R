#weekend homework
#Vhuawelo Simba
#02 february 2019
# Section 2

#load libraries
library(tidyverse)
library(ggsn)
library(scales)
library(ggpubr)
library(lubridate)

ecklonia <- read_csv("ecklonia.csv") #loading data by using read_csv because the data is saved in csv file and assigning a name to it so it appears in my environment.

#usinging summary to get the overview of my data.
summary(ecklonia)
head(ecklonia,n=5) #shows the fisrt 10 rows
tail(ecklonia,n=2) #shows the last 5 rows
glimpse(ecklonia)

dim(ecklonia) #showing the dimension of the data.

grDevices::colors() #colour sheet

#Hypothesis:I expect species with longer stipes to be more heavier.
#conclusions:The mass of the stipe increases as the length of the stipe increases, thus species with longer stipes are heavier.

Line_graph <- ggplot(ecklonia, aes(x=stipe_length, y=stipe_mass, colour=species))+ #assigning a name to the plot, then specify the data being used, then using aes to specify the x and y values and colouring the points by different sites.
  geom_point(aes(size=stipe_length))+ #plotting the points and dtermining the size of the point by the lenth of the stipe.
  geom_line(aes(group=species))+ #joining the pointswith a line and grouping them by sites.
  geom_smooth(method="lm")+ #using the linear model to ???
  labs(x="stipe length", y="stipe masss")+ #labeling the x and the y axis
  ggtitle("figure1")+ #Giving the graph a title.
  theme_bw() #the theme of the graph.




#Hypothesis:Batsata Rock has longer stipes, i expert to find that it's stipe's mass will be greater than that of boulders beach.
#conclusion:Batsata stipe's mass is greater than that of Boulders beach

Boxplot_graph <- ggplot(ecklonia, aes(x=site, y=stipe_mass))+ #assigning a name to the plot, then specify the data being used, then using aes to specify that x=site and y=stipe_mass and colouring the points by different sites.
  geom_boxplot(aes(fill=site))+ #plotting boxplot by using the function geom_plot and filing by site.
  labs(x="site", y="tipe_mass")+ #labeling the x and the y axis
  ggtitle("figure2")#Giving the graph a title. 

ecklonia %>% #specifying the data being used
  group_by(site) %>% #grouping by site
summarise(avr_sl=mean(stipe_length)) #naming a column,then calculating the average of stpie_length of each site.

                    
#Hypothesis:The average Stipe_length of species in Batsata is higher than those of Boulders Beach, so i expect it to have longer stipes. 
#conclusion:Batsata Rock has longer stipes.

ecklonia_2 <- ecklonia %>% #assigning a name to a new set of data
  filter(digits ==8) #filtering out only rows of ID=13

Bar_graph <- ggplot(ecklonia_2, aes(x=stipe_length))+ #assigning a name to the plot, then specify the data being used, then using aes to specify that x=Stipe_length. 
  geom_histogram(aes(fill=site), position = "dodge", binwidth = 100)+ #ploting a bar graph using the function geom_histogram,fill by site, position my bars by dodge to avoid them stacking on each other, and making binwidth size 100.
  labs(x="stipe_length (g)", y="count")#labeling the x and the y axis

#combining all the graph into one using ggarrange.
combined_graph <- ggarrange(Line_graph, Boxplot_graph, Bar_graph, #arranging my graphs into one plot
  ncol = 2, nrow = 2, # Set number of rows and columns
 common.legend = TRUE) # Create common legend


#finding the mean,max,min,median and variance for the stipe_length for each of the sites using the function summarise.
ecklonia %>% #specifying the data being used
  group_by(site) %>% #grouping by site
  summarise(avr_sl=mean(stipe_length),#calculating stipe_length's mean.
            min_sl=min(stipe_length),#calculating stipe_length's minimum value.
            max_sl=max(stipe_length), #calculating stipe_length's maximum value.
            med_sl=median(stipe_length), #calculating stipe_length's median value.
            var_sl=var(stipe_length), #calculating the variance of stipe_lenth.
            sd_sl=var(stipe_length)/2, #calculating stipe_length's standard error
            n=n()) #

# Calculate the mean,max,min,median and variance for the stipe_diameter for each of the sites.
ecklonia %>% 
  group_by(site) %>%
  summarise(avr_sl=mean(stipe_diameter),
            min_sl=min(stipe_diameter),
            max_sl=max(stipe_diameter),
            med_sl=median(stipe_diameter),
            var_sl=var(stipe_diameter),
            sd_sl=var(stipe_diameter)/2, 
            n=n())


# Determining the min and maximum frond length and stipe length
ecklonia %>% 
  summarise(min_sl=min(stipe_length),
            max_sl=max(stipe_length),
            min_sl=min(stipe_diameter),
            max_bl=max(stipe_diameter),
            n=n())

summary(ecklonia) 

          
           


 




