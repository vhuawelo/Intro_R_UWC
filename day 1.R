#DAY_1.R
#Calculate a monthly climatology per site
#Author: Nobuhle and vhuawelo
#Date:29 january 2019

library(tidyverse)
library(lubridate)

temp <- read_csv("SACTN_data.csv")

temp2 <- temp%>%
dplyr::mutate(month=month(date))%>%
dplyr::group_by(site,month) %>% 
dplyr::summarise(temp=mean(temp,na.rm = true)) %>% 
ungroup()

ggplot(data=temp2, aes(x=month,y=temp))+
  geom_line()+
  facet_wrap(-site,nrow=3)+
  labs(x="month",y="temperature(")

# Neat script
# Lack of comments
# Comments shows a clear disscription of each line of code and will improve your understanding and marks