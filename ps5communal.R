
# Load libraries
library(tidyverse)
library(car)
library(ggplot2)
library(multcomp)
library(DescTools)

# Load the battery data from PS3
batt_data = read.csv("H:/My Drive/1 ExpDesign/Problem Sets/PS5/PS5_Code/PS3_Data.csv", header = TRUE )
summary(batt_data)

# Create vector of treatment codes and add as new column to battery data
Treatment <- c(11,12,11,22,11,11,12,21,22,12,12,21,22,21,22,21)
batt_data$Treatment <- Treatment

# Load meat data
meat_data = read.csv("H:/My Drive/1 ExpDesign/Problem Sets/PS5/PS5_Code/q4.csv", header = TRUE )
meat_data$Fat <- as.factor(meat_data$Fat)
meat_data$Method <- as.factor(meat_data$Method)
summary(meat_data)

# Question 3 Stuff
# I don't know if we need the mans and stuff given or can just use the data set?
# Setting up some variables
q3df <- data.frame(Treatments = c(11, 12, 21, 22),
                   Means = c(570.55, 860.50, 433.00, 496.25))
q3df$Treatments <- as.factor(q3df$Treatments)
# I'm not sure how to incorporate the msE?
q3msE <- 2367.71
n <- 4 # reps
n_treat <- length(q3df$Treatments)
df_error <- n*n_treat - 1
ME <- q_value * SE
# Trying something ...
# Studentized range
q_value <- qtukey(.95, n_treat, df_error)
# Standard error
SE <- sqrt(q3msE/n)
# Calculate tukey intevals
# I think this just does pairwise compairsons, but the setup is useful
tukey_intervals <- matrix(NA, nrow = n_treat, ncol= n_treat)
for(i in 1:(n_treat-1)) {
  for (j in (i + 1):n_treat) {
    diff_means <- q3df$Means[i] - q3df$Means[j]
    tukey_intervals[i,j] <- diff_means - q_value * SE
    tukey_intervals[j, i] <- diff_means + q_value * SE
  }
}
print(tukey_intervals)  
# Hmm
# This appears to have given me pairwise comparisons. Not sure how to apply this to the "simple effects"?
# I guess multiply by the contrast matrix first?

# Simple effects calculation
# Ok, I think this worked
simple_effects <- list(
  T11_T12 = c(q3df$Means[q3df$Treatments=='11'] - q3df$Means[q3df$Treatments=='12'] - ME, q3df$Means[q3df$Treatments=='11'] - q3df$Means[q3df$Treatments=='12'] + ME),
  T21_T22 = c(q3df$Means[q3df$Treatments=='21'] - q3df$Means[q3df$Treatments=='22'] - ME, q3df$Means[q3df$Treatments=='21'] - q3df$Means[q3df$Treatments=='22'] + ME),
  T11_T21 = c(q3df$Means[q3df$Treatments=='11'] - q3df$Means[q3df$Treatments=='21'] - ME, q3df$Means[q3df$Treatments=='11'] - q3df$Means[q3df$Treatments=='21'] + ME),
  T12_T22 = c(q3df$Means[q3df$Treatments=='12'] - q3df$Means[q3df$Treatments=='22'] - ME, q3df$Means[q3df$Treatments=='12'] - q3df$Means[q3df$Treatments=='22'] + ME))
print(simple_effects)

# Define contrast matrix
# Or not, I didn't get this to work
q3contrasts <- cbind("11_12" = c(1,-1,0,0),
                     "21_22" = c(0,0,1,-1),
                     "11_21" = c(1,0,-1,0),
                     "12_22" = c(0,1,0,-1))

# Q4 - I'll get to it

# Q5 - Two-Way Complete Meat
# Anova table
meatnova <- aov(Grams ~ Fat * Method, data = meat_data)
summary(meatnova)

# Interaction plot
# Box
ggplot(meat_data, aes(x = interaction(Fat, Method), y = Grams)) +
  geom_boxplot() +
  labs(title = "Finished Cooking Weight by Fat Percent and Method", x = "Fat-Method Combination", y = "Finished weight(g)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Line
interaction.plot(x.factor = meat_data$Fat, trace.factor = meat_data$Method, 
                 response = meat_data$Grams,
                 main = "Interaction Plot", xlab = "Fat Percent", ylab = "Finished Weight (g)",
                 trace.label = "Cooking Method", fixed = TRUE)
# And Swapped
interaction.plot(x.factor = meat_data$Method, trace.factor = meat_data$Fat, 
                 response = meat_data$Grams,
                 main = "Interaction Plot", xlab = "Cooking method", ylab = "Finished Weight (g)",
                 trace.label = "Fat Percent", fixed = TRUE)

# Scheffe Test ?? 
# Make a linear model
#meat_model <- lm(meat_data$Grams ~ meat_data$Fat * meat_data$Method, data = meat_data)
#summary(meat_model)
# Scheffe comparison
#meat_comp <- glht(meat_model, linfct = mcp(meat_data$Fat = "Scheffe"))

# Didn't work? Anova version?
meatnova <- aov(meat_data$Grams ~ meat_data$Fat, data = meat_data)
meat_schef <- ScheffeTest(meatnova, conf.level = .95)
meat_schef

# Part D - All contrasts for Cooking method
# Add new column with finished weight difference
meat_data$diff <- meat_data$Grams - 110

# New dataframe with just differences for each method at each fat
avg_diff <- aggregate(diff ~ Fat + Method, data = meat_data, FUN = mean)

# ChatGPT keeps either giving me code using glht that doesn't work
# or code claiming to use Scheffe's method that actally just uses TukeyHSD
# And event he Tukey code doesn't actually work

# Part E
# Yet another case of ChatGPT generating me several different kinds of non-working code

