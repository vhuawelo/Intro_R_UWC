#weekend homework
#Vhuawelo Simba
#02 february 2019
# Section 1

#load libraries

library(tidyverse)
library(ggpubr)
library(scales)
library(ggsn)


#We use load  instead of Read_csv because the file is already converted to RData.
load("rast_feb.RData")
load("rast_aug.RData")
load("sa_provinces.RData")
load("africa_map.RData")


#usinging summary to get the overview of my data.
summary(rast_aug)
head(rast_aug,n=10) #shows the fisrt 10 rows
tail(rast_aug,n=5) #shows the last 5 rows
glimpse(rast_aug)

#summary(rast_aug)
head(rast_feb,n=10) #shows the fisrt 2 rows
tail(rast_feb,n=5) #shows the last 20 rows
glimpse(rast_feb)

#creating a colour pallet
colrs_pallet <- c("#F0ABDF","#7DA5CA","#35908A","#426F46","#4E4B1E","#442915")

grDevices::colors() #colour sheet

ggplot()+
  borders()+
  coord_equal()


                                                     # Creating a map by making use of the lat and long variables
feb_map <- ggplot(rast_feb, aes(x = lon, y = lat)) + #selecting the data being used, then specifying the x and y value using aes. 
  geom_point()+                                      #plotting points using geom_point
  labs(x="longtd", y="lat")+                         #labeling the x and y axis
  ggtitle("figure_1_Temperature variation")          #title of the graph using ggtitle

feb_map1 <- feb_map+ #assigning a name to the map
  geom_polygon(colour = "black", fill = "turquoise3"  )+       #drawing a polygon using the function geom_polygon
  geom_path(data=sa_provinces, aes(group=group))+              #joining the path which in this case gives me different provinces.
  scale_fill_manual("Temp. (°C)", values = colrs_pallet)+ 
  coord_equal(xlim = c(15,34), ylim = c(-37,-29), expand = 0)+ #specifying the minimum and maximum value the x and y values to be included in the map.
scale_x_continuous(position = "bottom")+                       # Put x axis labels on top of figure
 theme(axis.title = element_blank(),                           # Remove the axis labels
        legend.text = element_text(size = 7),                  # Change text size in legend
        legend.title = element_text(size = 7),                 # Change legend title text size
        legend.key.height = unit(0.3, "cm"),                   # Change size of legend
        legend.background = element_rect(colour = "white"),    # Add legend background
        legend.justification = c(1, 0),                        # Change position of legend
        legend.position = c(0.55, 0.4))                        # Fine tune position of legend
  
feb_map2 <- feb_map1 +                      #assigning a name to the map
annotate("text", label = "Atlantic\nOcean", #adding the name of the ocean.
         x = 17.1, y = -35.0,               #specifiying where the label should be located on the map.
         size = 4.0,                        #selecting the size of the label's text.
         angle = 30,                        #specifying the angle of the label
         colour =  "violetred1") +          #the colour of text
  annotate("text", label = "Indian\nOcean", 
           x = 29.2, y = -34.0, 
           size = 4.0, 
           angle = 360, 
           colour = "yellow3" )

feb_map3 <- feb_map2 +
  scalebar(x.min = 27, x.max = 33, y.min = -35.6, y.max = -35, # Set location of bar
           dist = 300, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") +                    # Set appearance
  north(x.min = 30.5, x.max = 32.5, y.min = -34.0, y.max = -32,# Set location of symbol
        scale = 1.2, symbol = 16)


feb_map3+
annotation_custom(grob = ggplotGrob(africa_map),#putting a anothe map inside the othe map
                  xmin = 30.5, xmax = 33.9,     #setting location of the map.
                  ymin = -35, ymax = -29.5)


 
  
  
            
   



  
  
  
 


