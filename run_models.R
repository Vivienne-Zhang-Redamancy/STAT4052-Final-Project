run_models <- function(seed_val, return_vars = FALSE) {
  
  set.seed(seed_val)
  n <- nrow(df)
  train_idx <- sample(1:n, size = floor(0.7 * n))
  test_idx  <- setdiff(1:n, train_idx)
  
  y_train <- y[train_idx]
  y_test  <- y[test_idx]
  
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
  
  penalty_factor <- c(rep(1, length(behavior_vars)), rep(0, length(control_vars)))
  
  cv_lasso <- cv.glmnet(X_train_matrix, y_train,
                        alpha = 1,
                        penalty.factor = penalty_factor,
                        nfolds = 10)
  
  pred_lasso <- predict(cv_lasso, newx = X_test_matrix, s = "lambda.min")
  
  lasso_coef <- coef(cv_lasso, s = "lambda.min")
  selected_vars <- rownames(lasso_coef)[which(lasso_coef != 0)][-1]
  
  all_predictors <- unique(c(selected_vars, control_vars))
  X_train_ols <- X_train[, all_predictors, drop = FALSE]
  X_test_ols  <- X_test[,  all_predictors, drop = FALSE]
  
  ols_model <- lm(y_train ~ ., data = cbind(y_train = y_train, X_train_ols))
  pred_ols <- predict(ols_model, newdata = X_test_ols)
  
  cv_ridge <- cv.glmnet(X_train_matrix, y_train, alpha = 0)
  pred_ridge <- predict(cv_ridge, newx = X_test_matrix, s = "lambda.min")
  
  y_prop <- y_train / 100
  n_train <- length(y_train)
  y_beta <- (y_prop * (n_train - 1) + 0.5) / n_train
  
  beta_model <- betareg(y_beta ~ ., data = data.frame(y_beta, X_train))
  pred_beta <- predict(beta_model, newdata = X_test) * 100
  pred_beta <- pmax(pmin(pred_beta, 100), 0)
  
  rf_model <- randomForest(x = X_train, y = y_train,
                           ntree = 500,
                           mtry = floor(ncol(X_train)/3))
  pred_rf <- predict(rf_model, newdata = X_test)
  
  calc_metrics <- function(pred, true) {
    c(
      MSE = mean((pred - true)^2),
      RMSE = sqrt(mean((pred - true)^2)),
      MAE = mean(abs(pred - true))
    )
  }
  
  results <- rbind(
    LASSO = calc_metrics(pred_lasso, y_test),
    PostLASSO_OLS = calc_metrics(pred_ols, y_test),
    Ridge = calc_metrics(pred_ridge, y_test),
    Beta  = calc_metrics(pred_beta, y_test),
    RF    = calc_metrics(pred_rf, y_test)
  )
  
  if(return_vars){
    return(list(results = results, selected = selected_vars, ols_model = ols_model))
  } else {
    return(results)
  }
}

