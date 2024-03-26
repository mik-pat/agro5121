# Lucky 7.

library(tidyverse)
library(dplyr)
library(ggplot2)
library(xtable)
library(ibd)

# Q1 
# 1A - Mostly on paper

# 1B - Mostly on Paper

# 1C - Graphical

# 1D - Expected values to show estimability

# 2 Randomization

# bibd(v = treatments, r = reps each, b = num blocks, k = block size, lambda = pairs in block)
# Design I
designI <- bibd(v = 4, r = 3, b = 6, k = 2, lambda = 1)
xtable(designI$design, caption = "Design I Randomization", digits = c(0,0,0))

# Design II
designII <- ibd(v = 9, b = 9, k = 3) # Doesn't satisfy reqs for BIBD?
xtable(designII$design, caption = "Design II Randomization", digits = c(0,0,0,0))

# Design III
designIII <- ibd(v=8, b = 8, k = 3) # Doesn't satisfy regs for BIBD?
xtable(designIII$design, caption = "Randomization of Design 3", digits = c(0,0,0,0))

# Q3 Balanced Incomplete Block Design
# A - theoretical
# B -  theoretical
# C - all possible combinations of treatments from 7 with r = 15, calculate number of blocks
design <- bibd(v = 7, r = 15, k = 5, lambda = 1) # Requires a B to be specified, doesn't work

# Define parameters
v <- 7  # Number of treatments
k <- 5  # Block size (treatments per block)
r <- 15  # Replications (total observations per treatment)

# Calculate lambda prime (necessary condition for BIBD)
lambda_prime <- (v - 1) * (k - 1) / (r - k)

# Check if lambda_prime is an integer (necessary condition for BIBD)
if (lambda_prime %% 1 != 0) {
  print("A Balanced Incomplete Block Design is not possible with these parameters.")
} else {
  b <- (v * (r - k)) / (k * lambda_prime)  # Number of blocks (if BIBD were possible)
  print("Number of blocks (b) for BIBD:", b)
} # lambda isn't an integer :/

# Q4 
# A - theoretical
# B - randomization
design4 <- bibd(v = 4, r = 3, k = 3, b = 4,lambda = 2)
xtable(design4$design, caption = "Randomization of BIBD for Q4", digits = c(0,0,0,0))
# C - Calculated on paper Qi = Ti - 1/k[Sum of Block Totals Where Treatment appears]
# D - Confidence interval
# Function to calculate the studentized range statistic (q_tukey)
q_tukey <- function(k, alpha, df) {
  qtukey(1 - alpha, df, k - 1)
}

# Inputs (replace with your actual data)
b <- c(417, 507, 469, 577)# Vector of block totals
t <- c(385, 582, 329, 674) # Vector of treatment totals
n <- sum(b)  # Total number of observations
p <- length(t)  # Number of treatments
gm <- 164.1667# Grand mean
mse <-  3.683# Mean square of error
  
# Calculate adjusted treatment totals (assuming equal replication within blocks)
at <- t - (b - n * gm) / p

# Define the contrast coefficients (e.g., -1 for tau_2, 1 for tau_3)
contrast <- c(0,-1,1,0)  # Adjust for your specific contrast

# Calculate the standard error of the contrast estimate
se_contrast <- sqrt(mse * sum(contrast^2) / n)

# Calculate the studentized range statistic (q_tukey) for 95% confidence interval
q <- q_tukey(p, 0.05, n - p - 1)  # Adjust p and df based on your design

# Calculate the lower and upper limits of the contrast interval
lower_limit <- at[2] - q * se_contrast
upper_limit <- at[2] + q * se_contrast

# Print the contrast interval
cat("Contrast interval for tau_3 - tau_2 (95% Tukey):")
cat(" [", round(lower_limit, 4), ", ", round(upper_limit, 4), "]")
