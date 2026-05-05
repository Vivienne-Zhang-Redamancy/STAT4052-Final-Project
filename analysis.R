source("/Users/viviennezhang/Desktop/4052_project/data_processing.R")
source("//Users/viviennezhang/Desktop/4052_project/run_models.R")

library(tidyverse)
set.seed(1234)
split_seeds <- 101:105
results_list <- lapply(split_seeds, run_models)
results_array <- simplify2array(results_list)
mean_results <- apply(results_array, c(1,2), mean)
print("=== Average Test Performance ===")
print(mean_results)

rmse_df <- data.frame(
  Model = rep(rownames(mean_results), each = length(split_seeds)),
  RMSE = as.vector(results_array[, "RMSE", ])
)

ggplot(rmse_df, aes(x = Model, y = RMSE)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "RMSE Distribution Across Splits")

var_list <- lapply(split_seeds, function(s){
  run_models(s, return_vars = TRUE)$selected
})

all_vars <- unlist(var_list)

var_freq <- as.data.frame(table(all_vars))
colnames(var_freq) <- c("Variable", "Frequency")

var_freq <- var_freq %>% arrange(desc(Frequency))

print("=== LASSO Variable Selection Frequency ===")
print(head(var_freq, 15))

set.seed(4052)
train_idx <- sample(1:nrow(df), size = floor(0.7 * nrow(df)))
test_idx <- setdiff(1:nrow(df), train_idx)

print(sum(complete.cases(df)) / nrow(df))
print(mean(is.na(df)))

y_train <- y[train_idx]
y_test <- y[test_idx]

X_behav_train <- X_behav[train_idx, ]
X_behav_test  <- X_behav[test_idx, ]
X_control_train <- X_control[train_idx, ]
X_control_test  <- X_control[test_idx, ]

behav_mean <- colMeans(X_behav_train)
behav_sd   <- apply(X_behav_train, 2, sd)

X_behav_train <- scale(X_behav_train, center = behav_mean, scale = behav_sd)
X_behav_test  <- scale(X_behav_test,  center = behav_mean, scale = behav_sd)

X_train <- data.frame(X_behav_train, X_control_train)
X_test  <- data.frame(X_behav_test,  X_control_test)

X_train <- as.data.frame(lapply(X_train, as.numeric))
X_test  <- as.data.frame(lapply(X_test, as.numeric))

X_train_matrix <- as.matrix(X_train)
X_test_matrix  <- as.matrix(X_test)

set.seed(2026)
n <- nrow(df)
train_idx <- sample(1:n, size = floor(0.7 * n))
length(train_idx)
length(test_idx)

apply(results_array[, "RMSE", ], 1, sd)

print(sig_ols)

