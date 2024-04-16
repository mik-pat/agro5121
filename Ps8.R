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
# xtable(model)
# QQ Plot
qqnorm(resid(model))
qqline(resid(model))

# I think these are all still for error, not sure how to do for random effect
# Residuals?
df$Fitercept <- predict(model)

# Run the Mixed model
texreg(model)

# Means separation
xt <- emmeans(model, list(pairwise~Treatment), adjust = "tukey")

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
summary(model_means_cld)
model_means_cld$Treatment <- reorder(model_means_cld$Treatment, desc(model_means_cld$emmean))
summary(model_means_cld)
model_means_cld$Signif <- c("C", "BC", "BC", "ABC", "ABC", "AB", "A")
summary(model_means_cld)
# 
ggplot(model_means_cld) +
 aes(x = Treatment, y = emmean, fill = Treatment) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 theme_minimal() +
# Add error bars

geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +  # Adjust width as needed

# Add comparison letters

geom_signif(comparisons = list(c("A", "AB", "ABC", "ABC", "BC", "BC", "C")), map_signif_level = TRUE, textsize = 4, vjust = -0.5) +
geom_text(aes(label = Signif), position = position_dodge(width = 0.9), vjust = -5, size = 3.5)
# Add comparison letters

# Display the plot
print(barplot_with_letters)
