#Day3 assignment
#vhuawelo simba
#31 january 2019




library(scales)
#Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.

library(ggsn)
#Adds north symbols (18 options) and scale bars in kilometers to maps in geographic or metric coordinates created with 'ggplot' or 'ggmap'.

#2.Convert the laminaria csv file to R formatted dataset with just one line of code:
laminaria<- read.csv("laminaria.csv")
save(laminaria, file = "laminaria.Rdata")


# [A.A]
# very brief discriptions
# Moe infomation may be found in several pdf documents
# Neat