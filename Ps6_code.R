# Problem Set Six Here We Goooooo
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
library(psych)
library(agricolae)
library(xtable)

# Question 1 Zinc Plating


# Question 2 Randomization
# Assumes s=1, randomizes into 5 blocks
# Part 1

# Used psych library
block_rand <- block.random(n=20, c(trt=4))
headTail(block_rand)



# Using Agricolae

# Part 2
# In result DF, blocks 1 & 2 make up the first run, blocks 3 & 4 make up the second
trt_2 = c("1","2","3","4","5","6")
block_rand_2 <- design.rcbd(trt_2, 4, serie=0, seed=0)
block_rand_2
export_table <- xtable(block_rand_2$sketch, auto=TRUE)
print(export_table, type="latex")
