library(tidyverse)
library(glmnet)
library(betareg)
library(randomForest)
library(sandwich)
library(lmtest)
df <- read.csv("/Users/viviennezhang/Desktop/D2D2016FoodStudy_data.csv")

df <- na.omit(df)
df$log_income <- log(df$income)
y <- df$percent_discard

behavior_vars <- c(
  "preshop_1","preshop_2","preshop_3","preshop_4",
  "instore_5","instore_6","instore_7",
  "tosspre_8","tosspre_9","tosspre_10","tosspre_11",
  "tosspre_12","tosspre_13","tosspre_14",
  "tosspost_15","tosspost_16","tosspost_17","tosspost_18","tosspost_19",
  "skill_28","skill_29","skill_30","skill_31","skill_32","skill_33",
  "family_34","family_35","compost_36","recycle_37"
)
control_vars <- c("beef","class1","level2","level3","bestby","useby",
                  "date_days","price","large","age","gender","educ",
                  "log_income","hhld_size","child_pres","nonwhite")

X_behav <- as.matrix(df[, behavior_vars])
X_control <- as.matrix(df[, control_vars])
