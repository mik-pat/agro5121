# Problem Set Six Here We Goooooo
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
library(psych)
library(agricolae)
library(xtable)
library(DescTools)

# Question 1 Zinc Plating


# Question 2 Randomization
# Assumes s=1, randomizes into 5 blocks
# Part 1

# Used psych library
block_rand <- block.random(n=20, c(trt=4))
headTail(block_rand)
export_table1 <- xtable(block_rand$sketch, auto=TRUE)
print(export_table1, type="latex")



# Using Agricolae

# Part 2
trt_2 = c("1","2","3","4","5","6")
block_rand_2 <- design.rcbd(trt_2, 4, serie=0, seed=0)
block_rand_2
export_table <- xtable(block_rand_2$sketch, auto=TRUE)
print(export_table, type="latex")

# Part 3 Respiratory Exchange

resp_ex <- data.frame(Protocol = c(1,2,3,4,5,6,7,8,9),
                      Subject_1= c(.79, .84, .84, .83, .84, .83, .77, .83, .81),
                      Subject_2= c(.80, .84, .93, .85, .78, .75, .76, .85, .77),
                      Subject_3= c(.83, .81, .88, .79, .88, .86, .71, .78, .72))
resp_ex$Protocol <- as.factor(resp_ex$Protocol)
summary(resp_ex)
# Make long format
resp_long <- pivot_longer(resp_ex, !Protocol, names_to ="Subject", values_to="Response")
resp_long$Subject <- as.factor(resp_long$Subject)
#QC Data & Check Assumptions
summary(resp_long)
res_model <-lm(Response ~ Protocol + Subject, resp_long)
plot(res_model)

# Get Residuals
res_stnd <- rstandard(res_model)

# Plot'em
plot(res_stnd)

#Common variance looks good? Nothing past 2, not really flared
plot(res_stnd, type = "b", main = "Standardized Residuals",
     xlab = "Observation Index", ylab = "Standardized Residuals")
abline(h = 0, col = "red")

# QQ for normality - a little wonky, at least one weird outlier at the high end
ggplot() +
  geom_qq(aes(sample = rstandard(res_model))) +
  geom_abline(color = "red") +
  coord_fixed()

# Box Plot for good measure
# Looks like pairwise comps should have 3+ levels? 3 & 7 in their own
resp_plot <- ggplot(resp_long, aes(x=Protocol, y=Response)) +
  geom_boxplot() +
  geom_point(size=3, alpha=0.5, aes(color=Subject)) +
  labs(title="Response x Treatment",
       x="Protocol",
       y="Response")
resp_plot

# Anova of model 
res_model %>% anova()

# Anova regular style
aov_blocked <- aov(Response ~ Protocol + Subject, data = resp_long)
aov_un <- aov(Response ~ Protocol, data= resp_long)
aov_subj <- aov(Response ~ Subject, data = resp_long)

summary(aov_blocked)
# Export table in LaTeX format
print(xtable(anova(aov_blocked)))
summary(aov_un)
summary(aov_subj)

# Scheffe pairwise comparisons + inpatient vs outpatient contrast
# Scheffe for all protocols, not blocked since blocking wasn't significant
ScheffeTest(aov_un, conf.level = .99)
# I guess?
scheffe.test(aov_un, "Protocol", alpha = .01, console = TRUE)
# This seems like a totally different thing? Both indicate there isn't much signif though.

ScheffeTest(aov_un, contrasts = c(1, -.5, -.5), conf.level = .99)
# I just put in the contrast as provided. It may not match up with the actual treatments
# I'll check it against the book tomorrow.
# And maybe try the agricolae sheffe.test instead
