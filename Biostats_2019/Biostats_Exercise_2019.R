#Biostats_2019
#Exercises
#18 april 2019
#Vhuawelo

#Exercise 6.7.1
#Find or create your own normally distributed data and think of a hypothesis you could use a t-test for. Write out the hypothesis, test it, and write a one sentence conclusion for it. Provide all of the code used to accomplish this.

#Loading libraries

co2 <- datasets::CO2
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(ggpubr)
library(Rmisc)

vhuawelo <- data.frame(dat = c(rnorm(n = 249, mean = 20.14458, sd = 6.41470),
                            rnorm(n = 79, mean = 30.48101, sd = 6.10771)),
                    sample = c(rep("A", 249), rep("B", 79)))

#Using the shapiro test if the data is normally distributed
shapiro.test(vhuawelo$dat)
# The p value is greater than 0.05 which means that the data is normally distributed.

#HYPOTHESIS
#H0:population means are equal for the two samples
#H1:population means are not equal for the two samples

plot_1<- ggplot(data = vhuawelo, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = "grey70", alpha = 0.4) +
  labs(x = "value")
plot_1

#Using t-test to see the relationship between the means of population A and Population B
t.test(dat ~ sample, data = vhuawelo, var.equal = TRUE)
# Beacause the p value is lower than 0.05 i reject the null hypothesis
#Thus the mean of population A is not equal to the mean of population B

#Exercise 6.7.2


#Exercise 7.4.1
#Here is bunch of data for pigs raised on different diets. The experiment is similar to the chicken one. Does feed type have an effect on the mass of pigs at the end of the experiment?
# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

#Hypotheis
#H0:The different feed types doesn't have an effect on the pig mass at the end of the experiment
#H1:he different feed types have an effect on the pig mass at the end of the experiment
#Compairing the mean of mass of different feed types.
mass <- compare_means(mass ~ feed, data = bacon, method = "t.test")
bacon.aov1 <- aov(mass ~ feed, data = bacon)
summary(bacon.aov1)

#conclusion: P value of all mass values were lower than 0.05 which means we reject the null hypothesis
#Thus the different feed types have an effect on the pig mass at the end of the experiment

#Exercise 7.4.2
Tooth<- datasets::ToothGrowth

#Hypothesis
#H0:Both supplements given to the guinea pigs have the same effect on the growth of the teeth of the pigs.
#H1:Both supplements given to the guinea pigs  have different effect on the growth of the teeth of the pigs.

Tooth.aov1 <- aov(len ~ supp, data = filter(Tooth, dose %in% c(0.5, 1,2)))
summary(Tooth.aov1 )


#conclusion: P value is higher than 0.05 which means we accept the null hypothesis

#Exercise 7.4.3
orange <- Orange
#Hypothesis
#H0: Age is not the determining factor of the growth of thecircuference on trees
#H1: Age is the determining factor of the growth of thecircuference on trees

orange.aov1 <- aov(age ~ circumference, data = filter(orange, Tree %in% c(1, 2, 3, 4,5)))
summary(orange.aov1)

#Conclusion, the p value is lower than 0.05 therefore we reject H0 and accept H1

orange.summary <- orange %>% 
  group_by(Tree) %>% 
  summarise(mean_circumference = mean(circumference),
            sd_circumference = sd(circumference)) %>% 
  ungroup()

orange.summary2 <- summarySE(data = orange, measurevar = "circumference", 
                             groupvars = c("Tree"))

ggplot(data = orange, aes(x = age, y = circumference)) +
  geom_segment(data = orange.summary2, aes(x = age, xend = age, 
                                           y = circumference - ci, 
                                           yend = circumference + ci, 
                                           colour = Tree),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)








