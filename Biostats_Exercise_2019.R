#Biostats_2019
#Exercises
#18 april 2019
#Vhuawelo

#Exercise 6.7.1
#Find or create your own normally distributed data and think of a hypothesis you could use a t-test for. Write out the hypothesis, test it, and write a one sentence conclusion for it. Provide all of the code used to accomplish this.

#Loading libraries
library(tidyverse)
library(plotly)
library(ggpubr)


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

compare_means(mass ~ feed, data = bacon, method = "t.test")














