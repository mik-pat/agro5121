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
designI <- bibd(v = 7, r = 15, k = 5, lambda = 1) # Requires a B to be specified, doesn't work

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
