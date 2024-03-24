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