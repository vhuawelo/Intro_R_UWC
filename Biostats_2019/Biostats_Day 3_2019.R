#Bio stats
#Day 3
#Vhuawelo
#4 April 2019

library(fitdistrplus)
library(logspline)
library(tidyverse)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

faithful <- datasets::faithful

slope <- round(eruption.lm$coef[2], 3)
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(faithful, aes(x=eruptions, y=waiting))+
  geom_point()+
  annotate("text", x =1 , y = 90, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 1, y = 80, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 1, y = 70, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  geom_smooth(method="lm")+
labs(x="eruptins(min)", y="waiting(min)")+
ggtitle("Vhuawelo")+
  theme_bw()


slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")

n <- 100
set.seed(666)
rand.df <- data.frame(x = seq(1:n),
                      y = rnorm(n = n, mean = 20, sd = 3))
ggplot(data = rand.df, aes(x = x, y = y)) +
  geom_point(colour = "blue") +
  stat_smooth(method = "lm", colour = "purple", size = 0.75, fill = "turquoise", alpha = 0.3) +
  labs(title = "Random normal data",
       subtitle = "Linear regression",
       x = "X (independent variable)",
       y = "Y (dependent variable)")

#Chapter 9

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("Biostats_2019/ecklonia.csv")
View(ecklonia)

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)


ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

corrplot(ecklonia_pearson, method = "circle")

# The same applies to this script as the other two script
# Did you just copy? 
# Do you know what this code means?
# This will all reflect in the exam so please do go through it and make sure you understannd what the code means
# Nontheless the script i neat and everything runs