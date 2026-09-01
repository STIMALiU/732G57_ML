# ==========================================================
# Regression with XGBoost
# Dataset: diamonds
# ==========================================================
rm(list=ls())
library(ggplot2)
library(caret)
library(xgboost)

# ==========================================================
# Load data
# ==========================================================

data(diamonds)

set.seed(123)

# Use a random subset of 10 000 observations
sample_index <- sample(
  seq_len(nrow(diamonds)),
  size = 10000
)

df <- diamonds[sample_index, ]

dim(df)
str(df)

# ==========================================================
# Split into training and validation data
# ==========================================================

set.seed(123)

train_index <- createDataPartition(
  y = df$price,
  p = 0.70,
  list = FALSE
)

train_df <- df[train_index, ]
valid_df <- df[-train_index, ]

nrow(train_df)
nrow(valid_df)

# ==========================================================
# Prepare predictors and response variables
# ==========================================================

# model.matrix() converts categorical predictors into
# numeric indicator variables.
x_train <- model.matrix(
  ~ . - price,
  data = train_df
)

x_valid <- model.matrix(
  ~ . - price,
  data = valid_df
)

# Remove the intercept column.
x_train <- x_train[
  ,
  colnames(x_train) != "(Intercept)",
  drop = FALSE
]

x_valid <- x_valid[
  ,
  colnames(x_valid) != "(Intercept)",
  drop = FALSE
]

# Use the same columns in the same order.
x_valid <- x_valid[
  ,
  colnames(x_train),
  drop = FALSE
]

y_train <- train_df$price
y_valid <- valid_df$price

# Check the predictor matrices and response variables.
dim(x_train)
dim(x_valid)

typeof(x_train)
typeof(x_valid)

stopifnot(
  is.numeric(x_train),
  is.numeric(x_valid),
  identical(colnames(x_train), colnames(x_valid)),
  !anyNA(x_train),
  !anyNA(x_valid)
)

# ==========================================================
# Fit the initial XGBoost model
# ==========================================================
#
# nrounds:
#   Number of boosting iterations.
#
# learning_rate:
#   Controls how much each new tree contributes to the model.
#   A smaller value usually requires more iterations.
#
# max_depth:
#   Maximum depth of each tree. Deeper trees give a more
#   flexible model but increase the risk of overfitting.
#
# min_child_weight:
#   Minimum sum of Hessian values required in a child node.
#   Larger values generally result in a less flexible model.
#
# subsample:
#   Proportion of training observations used in each iteration.
#
# colsample_bytree:
#   Proportion of predictors available to each tree.
#
# ==========================================================

set.seed(123)

model_initial <- xgboost(
  x = x_train,
  y = y_train,
  objective = "reg:squarederror",
  nrounds = 100,
  learning_rate = 0.1,
  max_depth = 3,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbosity = 0
)

model_initial

# ==========================================================
# Evaluate the initial model
# ==========================================================

pred_train_initial <- predict(
  model_initial,
  x_train
)

pred_valid_initial <- predict(
  model_initial,
  x_valid
)

# Root mean squared error on the training data
rmse_train_initial <- sqrt(
  mean((y_train - pred_train_initial)^2)
)

# Root mean squared error on the validation data
rmse_valid_initial <- sqrt(
  mean((y_valid - pred_valid_initial)^2)
)

rmse_train_initial
rmse_valid_initial

# ==========================================================
# Hyperparameter combinations
# ==========================================================

parameter_grid <- data.frame(
  learning_rate = c(0.05, 0.05, 0.10, 0.10, 0.20, 0.20),
  max_depth = c(2, 4, 2, 4, 2, 4),
  min_child_weight = c(1, 5, 5, 1, 1, 5)
)

parameter_grid

# ==========================================================
# Prepare data for xgb.cv()
# ==========================================================

dtrain <- xgb.DMatrix(
  data = x_train,
  label = y_train
)

# ==========================================================
# Cross-validation
# ==========================================================

cv_results <- parameter_grid

cv_results$CV_RMSE <- NA_real_
cv_results$CV_SD <- NA_real_

set.seed(123)

for (j in seq_len(nrow(parameter_grid))) {
  
  cv_fit <- xgb.cv(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      learning_rate = parameter_grid$learning_rate[j],
      max_depth = parameter_grid$max_depth[j],
      min_child_weight = parameter_grid$min_child_weight[j],
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = dtrain,
    nrounds = 100,
    nfold = 5,
    showsd = TRUE,
    verbose = FALSE
  )
  
  # Mean validation RMSE across the five folds
  # after 100 boosting iterations
  cv_results$CV_RMSE[j] <-
    cv_fit$evaluation_log$test_rmse_mean[100]
  
  # Standard deviation of validation RMSE across the folds
  cv_results$CV_SD[j] <-
    cv_fit$evaluation_log$test_rmse_std[100]
}

# Sort from the lowest to the highest cross-validation RMSE.
cv_results <- cv_results[
  order(cv_results$CV_RMSE),
]

row.names(cv_results) <- NULL

round(cv_results, 3)

# ==========================================================
# Select the best hyperparameter combination
# ==========================================================

# The best combination is the one with the lowest
# cross-validation RMSE.
best_parameters <- cv_results[1, ]

best_parameters

# ==========================================================
# Fit the tuned model on all training data
# ==========================================================

set.seed(123)

model_final <- xgboost(
  x = x_train,
  y = y_train,
  objective = "reg:squarederror",
  nrounds = 100,
  learning_rate = best_parameters$learning_rate,
  max_depth = best_parameters$max_depth,
  min_child_weight = best_parameters$min_child_weight,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbosity = 0
)

model_final

# ==========================================================
# Evaluate the tuned model
# ==========================================================

pred_train_final <- predict(
  model_final,
  x_train
)

pred_valid_final <- predict(
  model_final,
  x_valid
)

# Root mean squared error on the training data
rmse_train_final <- sqrt(
  mean((y_train - pred_train_final)^2)
)

# Root mean squared error on the validation data
rmse_valid_final <- sqrt(
  mean((y_valid - pred_valid_final)^2)
)

rmse_train_final
rmse_valid_final

# ==========================================================
# Compare the initial and tuned models
# ==========================================================

results_comparison <- data.frame(
  Model = c("Initial", "Tuned"),
  Training_RMSE = c(
    rmse_train_initial,
    rmse_train_final
  ),
  Validation_RMSE = c(
    rmse_valid_initial,
    rmse_valid_final
  )
)


