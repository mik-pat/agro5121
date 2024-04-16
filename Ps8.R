library(emmeans)
library(agricolae)
library(tidyverse)
library(multcomp)
library(multcompView)
library(car)
library(MASS)
library(lme4)
library(lmerTest)
library(performance)
library(dplyr)
library(ggplot2)
library(ggsignif)
library("texreg")
library(memisc)
library(xtable)


# Import the dataframe and get it set up
df <- read.csv("H:/My Drive/1 ExpDesign/Problem Sets/PS8/berry_csv.csv", header=TRUE)
summary(df)
head(df)
df$Treatment <- as.factor(df$Treatment)
df$trt <- as.factor(df$trt)
df$rep <- as.factor(df$rep)
df$undamaged <- 100-df$damage
# Plot
ggplot(df) +
  aes(x = Treatment, y = damage, fill = Treatment) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
ggplot(df) +
  aes(x = Treatment, y = undamaged, fill = Treatment) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# Mixed model
model <- lmer(damage ~ Treatment + (1|rep), data = df)
plot(model)
summary(model)
# xtable(model)
# QQ Plot
qqnorm(resid(model))
qqline(resid(model))

# Redo for undamaged
umodel <- lmer(undamaged ~ Treatment + (1|rep), data = df)
plot(umodel)
summary(umodel)
# xtable(model)
# QQ Plot
qqnorm(resid(umodel))
qqline(resid(umodel))
# I think these are all still for error, not sure how to do for random effect
# Residuals?
df$Fitercept <- predict(model)
df$FiterceptU <- predict(umodel)
# Run the Mixed model
texreg(umodel)

# Means separation
xt <- emmeans(model, list(pairwise~Treatment), adjust = "tukey")
xtu <- emmeans(umodel, list(pairwise~Treatment), adjust = "tukey")
# Table output not working. Really don't want to do it manually.
xtable(xt$`emmeans of Treatment`, caption = 'Means Comparison for Treatments', label = NULL, align = NULL,
       digits = 4, display = NULL, auto = FALSE)
xtable(xt$`pairwise differences of Treatment`, caption = 'Pairwise Comparisons of Treatments')
means <- as.data.frame(xt$`emmeans of Treatment`)
pairws <- as.data.frame(xt$`pairwise differences of Treatment`)
texreg(xtdf)

# Extract estimated marginal means and standard errors
mean_values <- emmeans::emmip(xt, ~Treatment, at = list(variable = "constant"))

model_means_cld <- cld(object = xt,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)
umodel_means_cld <- cld(object = xtu,
                        adjust = "Tukey",
                        Letters = letters,
                        alpha = 0.05)
summary(model_means_cld)
summary(umodel_means_cld)
model_means_cld$Treatment <- reorder(model_means_cld$Treatment, desc(model_means_cld$emmean))
umodel_means_cld$Treatment <- reorder(umodel_means_cld$Treatment, desc(model_means_cld$emmean))
summary(model_means_cld)
model_means_cld$Signif <- c("C", "BC", "ABC", "ABC", "AB", "AB", "A")
umodel_means_cld$Signif <- c("C", "BC", "BC", "ABC", "ABC", "AB", "A")
summary(model_means_cld)
# Contrasts?
# Contrasts?
contrast_list <- list(
  "Permethrin vs Water" =c(0,0,0,0,1,0,-1),
  "Carbaryl vs Water" =c(1,0,0,0,0,0,-1),
  "Malathion vs Water" =c(0,0,1,0,0,0,-1),
  "Spinosad vs Water" =c(0,0,0,0,0,1,-1),
  "Esfenvalerate vs Water" =c(0,1,0,0,0,0,-1),
  "Neem oil vs Water" =c(0,0,0,1,0,0,-1)
)
contrast_water <- contrast(xtu, contrast_list)
contrast_water
xtable(contrast_water)

#Plots
ggplot(model_means_cld) +
 aes(x = Treatment, y = emmean, fill = Treatment) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 theme_minimal() +
# Add error bars

geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +  # Adjust width as needed

# Add comparison letters
labs(y="Percent damage") +
geom_text(aes(label = Signif), position = position_dodge(width = 0.9), vjust = -5, size = 3.5)
# Add comparison letters


ggplot(umodel_means_cld) +
  aes(x = Treatment, y = emmean, fill = Treatment) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  # Add error bars
  
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +  # Adjust width as needed
  labs(y="Percent Undamaged Berries") +
  # Add comparison letters
  
  geom_text(aes(label = Signif), position = position_dodge(width = 0.9), vjust = -2, size = 3.5)
# Add comparison letters

