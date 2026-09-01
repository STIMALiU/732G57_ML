# ==========================================================
# Binary classification with XGBoost
# Dataset: GermanCredit from the caret package
# ==========================================================

rm(list=ls())
library(caret)
library(xgboost)

# ==========================================================
# Load and examine the data
# ==========================================================

data("GermanCredit", package = "caret")
?GermanCredit
df <- GermanCredit

dim(df)
str(df)

# "Bad" is defined as the positive class.
class_levels <- c("Good", "Bad")

df$Class <- factor(
  df$Class,
  levels = class_levels
)

# Examine class balance
table(df$Class)
prop.table(table(df$Class))

# ==========================================================
# Split into training and validation data
# ==========================================================

set.seed(123)

# A stratified split is used to preserve the class proportions.
train_index <- createDataPartition(
  y = df$Class,
  p = 0.70,
  list = FALSE
)

train_df <- df[train_index, ]
valid_df <- df[-train_index, ]

nrow(train_df)
nrow(valid_df)

# Class balance in the training data
table(train_df$Class)
prop.table(table(train_df$Class))

# Class balance in the validation data
table(valid_df$Class)
prop.table(table(valid_df$Class))

# ==========================================================
# Prepare predictors and response variables
# ==========================================================

# model.matrix() converts categorical predictors into
# numeric indicator variables.
x_train <- model.matrix(
  ~ . - Class,
  data = train_df
)

x_valid <- model.matrix(
  ~ . - Class,
  data = valid_df
)

# Remove the intercept column.
x_train <- x_train[, colnames(x_train) != "(Intercept)", drop = FALSE]
x_valid <- x_valid[, colnames(x_valid) != "(Intercept)", drop = FALSE]

# Use the same columns in the same order.
x_valid <- x_valid[, colnames(x_train), drop = FALSE]

# Keep the response variables as factors with identical levels.
y_train <- factor(
  train_df$Class,
  levels = class_levels
)

y_valid <- factor(
  valid_df$Class,
  levels = class_levels
)

# Check the predictor matrices and response variables.
dim(x_train)
dim(x_valid)

typeof(x_train)
typeof(x_valid)

levels(y_train)
levels(y_valid)


# ==========================================================
# Fit the first XGBoost model
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
# Evaluate the first model
# ==========================================================

pred_train_initial <- predict(
  model_initial,
  x_train,
  type = "class"
)

pred_valid_initial <- predict(
  model_initial,
  x_valid,
  type = "class"
)

# Explicitly set the same factor levels for all class vectors.
pred_train_initial <- factor(
  pred_train_initial,
  levels = class_levels
)

pred_valid_initial <- factor(
  pred_valid_initial,
  levels = class_levels
)


# "Bad" is treated as the positive class.
cm_train_initial <- confusionMatrix(
  data = pred_train_initial,
  reference = y_train,
  positive = "Bad"
)

cm_valid_initial <- confusionMatrix(
  data = pred_valid_initial,
  reference = y_valid,
  positive = "Bad"
)

cm_train_initial
cm_valid_initial

train_results_initial <- c(
  Accuracy = unname(cm_train_initial$overall["Accuracy"]),
  Sensitivity = unname(cm_train_initial$byClass["Sensitivity"]),
  Specificity = unname(cm_train_initial$byClass["Specificity"])
)

valid_results_initial <- c(
  Accuracy = unname(cm_valid_initial$overall["Accuracy"]),
  Sensitivity = unname(cm_valid_initial$byClass["Sensitivity"]),
  Specificity = unname(cm_valid_initial$byClass["Specificity"])
)

# Performance on the training data
round(train_results_initial, 3)

# Performance on the validation data
round(valid_results_initial, 3)





# ==========================================================
# Hyperparameter combinations
# ==========================================================

# Six combinations of learning_rate, max_depth and
# min_child_weight are compared.
parameter_grid <- data.frame(
  learning_rate = c(0.05, 0.05, 0.10, 0.10, 0.20, 0.20),
  max_depth = c(2, 4, 2, 4, 2, 4),
  min_child_weight = c(1, 5, 5, 1, 1, 5)
)

parameter_grid

# ==========================================================
# Prepare data for xgb.cv()
# ==========================================================

# The low-level xgb.cv() interface uses a numeric response:
#   Good = 0
#   Bad  = 1
y_train_numeric <- ifelse(y_train == "Bad", 1, 0)

dtrain <- xgb.DMatrix(
  data = x_train,
  label = y_train_numeric
)

# ==========================================================
# Cross-validation
# ==========================================================

# Store the mean cross-validation accuracy and its standard
# deviation for each hyperparameter combination.
cv_results <- parameter_grid

cv_results$CV_Accuracy <- NA_real_
cv_results$CV_SD <- NA_real_

set.seed(123)

for (j in seq_len(nrow(parameter_grid))) {
  
  cv_fit <- xgb.cv(
    params = list(
      objective = "binary:logistic",
      eval_metric = "error",
      learning_rate = parameter_grid$learning_rate[j],
      max_depth = parameter_grid$max_depth[j],
      min_child_weight = parameter_grid$min_child_weight[j],
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = dtrain,
    nrounds = 100,
    nfold = 5,
    stratified = TRUE,
    showsd = TRUE,
    verbose = FALSE
  )
  
  # test_error_mean contains the average classification error
  # across the five held-out folds.
  #
  # Accuracy = 1 - classification error.
  cv_results$CV_Accuracy[j] <- 1 -
    cv_fit$evaluation_log$test_error_mean[100]
  
  # Standard deviation is unchanged by the transformation
  # Accuracy = 1 - Error.
  cv_results$CV_SD[j] <-
    cv_fit$evaluation_log$test_error_std[100]
}

# Sort the combinations by mean cross-validation accuracy.
cv_results <- cv_results[
  order(cv_results$CV_Accuracy, decreasing = TRUE),
]

row.names(cv_results) <- NULL

round(cv_results, 3)

# ==========================================================
# Select the best hyperparameter combination
# ==========================================================

best_parameters <- cv_results[1, ]

best_parameters

# ==========================================================
# Fit the tuned model on all training data
# ==========================================================

set.seed(123)

model_final <- xgboost(
  x = x_train,
  y = y_train,
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
  x_train,
  type = "class"
)

pred_valid_final <- predict(
  model_final,
  x_valid,
  type = "class"
)

# Use the same factor levels and the same ordering for
# observed and predicted classes.
pred_train_final <- factor(
  pred_train_final,
  levels = class_levels
)

pred_valid_final <- factor(
  pred_valid_final,
  levels = class_levels
)


# "Bad" is treated as the positive class.
cm_train_final <- confusionMatrix(
  data = pred_train_final,
  reference = y_train,
  positive = "Bad"
)

cm_valid_final <- confusionMatrix(
  data = pred_valid_final,
  reference = y_valid,
  positive = "Bad"
)

cm_train_final
cm_valid_final

train_results_final <- c(
  Accuracy = unname(cm_train_final$overall["Accuracy"]),
  Sensitivity = unname(cm_train_final$byClass["Sensitivity"]),
  Specificity = unname(cm_train_final$byClass["Specificity"])
)

valid_results_final <- c(
  Accuracy = unname(cm_valid_final$overall["Accuracy"]),
  Sensitivity = unname(cm_valid_final$byClass["Sensitivity"]),
  Specificity = unname(cm_valid_final$byClass["Specificity"])
)

# Performance for the tuned model on the training data
round(train_results_final, 3)

# Performance for the tuned model on the validation data
round(valid_results_final, 3)

# ==========================================================
# Compare the initial and tuned models
# ==========================================================

results_comparison <- rbind(
  Initial_Training = train_results_initial,
  Initial_Validation = valid_results_initial,
  Tuned_Training = train_results_final,
  Tuned_Validation = valid_results_final
)

round(results_comparison, 3)
