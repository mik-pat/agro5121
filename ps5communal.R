# Load libraries
library(tidyverse)
library(car)
library(ggplot2)
library(multcomp)

# Load the battery data from PS3
batt_data = read.csv("H:/My Drive/1 ExpDesign/Problem Sets/PS5/PS5_Code/PS3_Data.csv", header = TRUE )
summary(batt_data)

# Create vector of treatment codes and add as new column to battery data
Treatment <- c(11,12,11,22,11,11,12,21,22,12,12,21,22,21,22,21)
batt_data$Treatment <- Treatment

# Load meat data
meat_data = read.csv("H:/My Drive/1 ExpDesign/Problem Sets/PS5/PS5_Code/q4.csv", header = TRUE )
summary(meat_data)

# Question 3 Stuff
# I don't know if we need the mans and stuff given or can just use the data set?
q3df <- data.frame(Treatments = c(11, 12, 21, 22),
                   Means = c(570.55, 860.50, 433.00, 496.25))
q3df$Treatments <- as.factor(q3df$Treatments)
# I'm not sure how to incorporate the msE?
q3msE <- 2367.71

# Define contrast matrix
q3contrasts <- cbind("11_12" = c(1,-1,0,0),
                     "21_22" = c(0,0,1,-1),
                     "11_21" = c(1,0,-1,0),
                     "12_22" = c(0,1,0,-1))

# Probably a linear model?

# Calculate contrasts & Confidence intervals