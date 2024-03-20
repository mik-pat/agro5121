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
library(readr)
library(tidyverse) # Loads ggplot2 and dplyr among others
library(emmeans)
library(multcomp)
library(multcompView)
library(knitr)
library(car)
library(MASS)
library(readxl)


#Question 1
df <- read_excel(path = "~/Desktop/Applied Experimental Design/PS6/PS6Q1Data.xlsx")
df$Vendor<- as.factor(df$Vendor)
df$bracket<-as.factor(df$bracket)



#Quality check data
summary(df)

#1a Plot plating thickness vs bracket thickness by vendor
ggplot(df) +
  aes(
    x = plating_thickness,
    y = bracket_thickness,
    colour = Vendor
  ) +
  geom_point(shape = "circle", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(
    values = c(`1` = "#0016FF",
               `2` = "#00C19F",
               `3` = "#FF61C3")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#1b Plot residuals 
ancova_model <- lm(plating_thickness ~ Vendor + bracket_thickness, data = df)
residuals<-residuals(ancova_model)
rstandard <- rstandard(ancova_model)
rstudent <- rstudent(ancova_model)

# Obtain standardized residuals
rstandard <- rstandard(ancova_model)

# Obtain predicted values
predicted <- predict(ancova_model)

# Normal scores
# Assume 'residuals' contains the standardized residuals
residuals_mean <- mean(residuals)
residuals_sd <- sd(residuals)
# Calculate z-scores (standardized residuals)
z_scores <- (residuals - residuals_mean) / residuals_sd
# Perform quantile transformation (optional)
normal_scores <- qnorm(pnorm(z_scores))

# Plot standardized residuals against the covariate
plot(df$bracket_thickness, rstandard, main = "Standardized Residuals vs. Covariate", xlab = "Covariate (Bracket thickness)", ylab = "Standardized Residuals")
abline(h = 0, col = "red")

# Plot standardized residuals against predicted values
plot(predicted, rstandard, main = "Standardized Residuals vs. Predicted Values", xlab = "Predicted Values", ylab = "Standardized Residuals")
abline(h = 0, col = "red")

# Plot standardized residuals against normal scores
plot(normal_scores, rstandard, main = "Standardized Residuals vs. Normal Scores", xlab = "Normal Scores", ylab = "Standardized Residuals")
abline(h = 0, col = "red")

#1d
ancova_model <- lm(plating_thickness ~ Vendor + bracket_thickness, data = df)
ancova_anova <- anova(ancova_model)
print(ancova_anova)

# Extract F-statistic and p-value from the summary
F_statistic <- 8.1267
p_value <- 0.01184

# Set significance level
alpha <- 0.05

# Print F-statistic and p-value
cat("F-statistic:", F_statistic, "\n")
cat("p-value:", p_value, "\n")

# Compare p-value to significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis: There is evidence of inequality of slopes.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no evidence of inequality of slopes.\n")
}


#1e
ancova_model <- lm(plating_thickness ~ Vendor + bracket_thickness, data = df)
ancova_anova <- anova(ancova_model)
print(ancova_anova)

# Extract F-statistic and p-value from the summary
F_statistic <- 5.2805
p_value <- 0.05063

# Set significance level
alpha <- 0.05

# Print F-statistic and p-value
cat("F-statistic:", F_statistic, "\n")
cat("p-value:", p_value, "\n")

# Compare p-value to significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis: There is evidence of treatment effects being different.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no evidence of treatment effects being different.\n")
}

#1f
# Fit ANOVA model
anova_model <- lm(plating_thickness ~ Vendor, data = df)
ancova_anova <- anova(ancova_model)
print(ancova_anova)

# Extract F-statistic and p-value from the summary
F_statistic <- 5.2805
p_value <- 0.05063

# Set significance level
alpha <- 0.05

# Print F-statistic and p-value
cat("F-statistic:", F_statistic, "\n")
cat("p-value:", p_value, "\n")

# Compare p-value to significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis: There is evidence of treatment effects being different.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no evidence of treatment effects being different.\n")
}

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

# Question 3 Respiratory Exchange

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

# Q4 Weed Control
weed <- read.csv("H:/My Drive/1 ExpDesign/Problem Sets/PS6/treatments_randomized.csv", header = TRUE)
headTail(weed)
weed$WeedControl <- as.factor(weed$WeedControl)
weed$SoilFertility <- as.factor(weed$SoilFertility)
weed$Block <- as.factor(weed$Block)

# Add protocol column
weed2 <- mutate(weed, Protocol = ifelse(WeedControl == "With" &
                                       SoilFertility == "High", 1, 
                                     ifelse(WeedControl == "Without" &
                                                 SoilFertility == "Low", 2, 
                                            ifelse(WeedControl == "With" &
                                                           SoilFertility == "Low", 3,
                                                   ifelse(WeedControl == "Without" &
                                                            SoilFertility == "High", 4, NA)))))

weed2$Protocol <- as.factor(weed2$Protocol)
weedmod <- aov(Yield ~ Protocol + Block, data = weed2)
summary(weedmod)
weedmod <- aov(Yield ~ WeedControl + SoilFertility + Block, data = weed)
summary(weedmod)
# That's uhhh... really significant

weed_model <-lm(Yield ~ Protocol + Block, weed2)
plot(weed_model)
# QQ definitely looks a little wonky
# Residuals vs fitted shows clear clustering?

# Get Residuals
weed_stnd <- rstandard(weed_model)

# Plot'em
plot(weed_stnd)
# These look ok, common variance - check.
# One outlier ~3.5. Filter?

#Common variance 
plot_weed <- plot(weed_stnd, type = "b", main = "Standardized Residuals",
              xlab = "Observation Index", ylab = "Standardized Residuals") +
  abline(h = 0, col = "red")
# Likewise, looks ok

# QQ for normality 
ggplot() +
  geom_qq(aes(sample = rstandard(weed_model))) +
  geom_abline(color = "red") +
  coord_fixed()
# Eehhhhh... not great. Not terrible.

# Pairs
marge <- emmeans(weed_model, ~ Protocol, level = .99)
marge
pairs(marge)
ScheffeTest(aov_weed, conf.level = .99)
# I guess?
scheffe.test(aov_weed, "Protocol", alpha = .01, console = TRUE)
# This seems like a totally different thing? Both indicate there isn't much signif though.
# I guess all one level because conf level is .99?
aov_weed <- aov(Yield ~ Protocol, data= weed2)
ScheffeTest(aov_weed, contrasts = c(-.5,.5,.5,-.5), conf.level = .99)


ScheffeTest(aob_weed, conf.level = .99)
# I guess?
scheffe.test(aov_weed, "Protocol", alpha = .01, console = TRUE)
# This seems like a totally different thing? Both indicate there isn't much signif though.
# I guess all one level because conf level is .99?

ScheffeTest(aov_weed, contrasts = c(-.5, .5, .5,-.5), conf.level = .99)
print(xtable(pairs(marge)))
