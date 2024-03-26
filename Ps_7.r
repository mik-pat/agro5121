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
# Calculate tukey critical value
qtukey(.95, 2, 11)

# E - Difference between treatments
# Define data (replace with your actual data)
block_totals <- c(417, 507, 469, 577)  # Block totals (number of blocks per treatment)
treatment_totals <- c(385, 582, 329, 674)   # Treatment totals (number of treatments per block)
grand_mean <- 164.1667                    # Grand mean
mean_square_error <- 3.684              # Mean square error

# Calculate degrees of freedom
n_blocks <- length(block_totals)
n_treatments <- length(treatment_totals)
n_total <- sum(block_totals)
df_total <- n_total - 1
df_blocks <- n_blocks - 1
df_treatments <- n_treatments - 1
df_error <- df_total - df_blocks - df_treatments

# Calculate sum of squares
SS_total <- mean_square_error * df_total
SS_blocks <- sum((block_totals - mean(block_totals))^2) / (n_treatments)  # Mean squares method for balanced designs
SS_treatments <- sum((treatment_totals - mean(treatment_totals))^2) / (n_blocks)  # Mean squares method for balanced designs

# Calculate mean square
MS_blocks <- SS_blocks / df_blocks
MS_treatments <- SS_treatments / df_treatments

# Calculate F-statistic
F_statistic <- MS_treatments / MS_blocks

# Calculate p-value
p_value <- pf(F_statistic, df_treatments, df_blocks, lower.tail = FALSE)

