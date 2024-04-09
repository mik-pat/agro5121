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

# Import the dataframe and get it set up
df <- read.csv("H:/My Drive/1 ExpDesign/Problem Sets/PS8/berry_csv.csv", header=TRUE)
summary(df)
head(df)
df$Treatment <- as.factor(df$Treatment)
df$trt <- as.factor(df$trt)
df$rep <- as.factor(df$rep)

# Plot
ggplot(df) +
  aes(x = Treatment, y = damage, fill = Treatment) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# Mixed model
model <- lmer(damage ~ Treatment + (1|rep), data = df)
plot(model)
summary(model)

# QQ Plot
qqnorm(resid(model))
qqline(resid(model))

# Residuals?
df$Fitercept <- predict(model)
