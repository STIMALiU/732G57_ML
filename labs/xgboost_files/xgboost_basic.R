# ==========================================================
# Binary classification with XGBoost
# Dataset: Ionosphere from the mlbench package
# ==========================================================

library(mlbench)
library(caret)
library(xgboost)

# ==========================================================
# Load and examine the data
# ==========================================================

data("Ionosphere", package = "mlbench")
?Ionosphere
df <- Ionosphere

# Examine the dimensions and variable types
dim(df)
str(df)

# Define the order of the classes:
# "bad" is the negative class and "good" is the positive class.
class_levels <- c("bad", "good")

df$Class <- factor(
  df$Class,
  levels = class_levels
)

# Examine the number and proportion of observations
# in each class
table(df$Class)
prop.table(table(df$Class))

# ==========================================================
# Split the data into training and validation data
# ==========================================================

set.seed(123)

# createDataPartition() performs a stratified split and attempts
# to preserve the class proportions in both datasets.
train_index <- createDataPartition(
  y = df$Class,
  p = 0.70,
  list = FALSE
)

train_df <- df[train_index, ]
valid_df <- df[-train_index, ]

# Number of observations in the two datasets
nrow(train_df)
nrow(valid_df)

# ==========================================================
# Examine class balance
# ==========================================================

# Class distribution in the training data
table(train_df$Class)
prop.table(table(train_df$Class))

# Class distribution in the validation data
table(valid_df$Class)
prop.table(table(valid_df$Class))

# ==========================================================
# Prepare predictors and response variables
# ==========================================================

predictor_names <- setdiff(names(df), "Class")

x_train <- train_df[, predictor_names]
x_valid <- valid_df[, predictor_names]

# Keep the responses as factors and explicitly use the same
# factor levels in the same order in both datasets.
y_train <- factor(
  train_df$Class,
  levels = class_levels
)

y_valid <- factor(
  valid_df$Class,
  levels = class_levels
)

# Verify the factor levels
levels(y_train)
levels(y_valid)

# ==========================================================
# Fit the XGBoost model
# ==========================================================
#
# nrounds:
#   Number of boosting iterations.
#   More iterations give a more flexible model but can increase
#   the risk of overfitting.
#
# learning_rate:
#   Controls the contribution from each new tree.
#   A smaller value gives slower and often more stable learning
#   but usually requires more boosting iterations.
#
# max_depth:
#   Maximum depth of each tree.
#   Deeper trees can capture more complicated relationships
#   but increase the risk of overfitting.
#
# min_child_weight:
#   Minimum sum of Hessian values required in a child node.
#   Larger values make splits more difficult and generally
#   give a less flexible model.
#
# subsample:
#   Proportion of training observations used during each
#   boosting iteration. Values below 1 add randomness and
#   can reduce overfitting.
#
# colsample_bytree:
#   Proportion of predictor variables available to each tree.
#   Values below 1 add randomness and can reduce overfitting.
#
# ==========================================================

model <- xgboost(
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

# Display information about the fitted model
model

# ==========================================================
# Calculate predicted probabilities
# ==========================================================

# For binary classification, type = "response" returns the
# estimated probability of the second factor level.
#
# Since the levels are c("bad", "good"), these values are
# estimated probabilities of the class "good".
pred_train_prob <- predict(
  model,
  x_train,
  type = "response"
)

pred_valid_prob <- predict(
  model,
  x_valid,
  type = "response"
)

# Examine the first predicted probabilities
head(pred_train_prob)
head(pred_valid_prob)

# ==========================================================
# Calculate predicted classes
# ==========================================================

pred_train <- predict(
  model,
  x_train,
  type = "class"
)

pred_valid <- predict(
  model,
  x_valid,
  type = "class"
)

# Explicitly use the same factor levels for observed and
# predicted classes. This avoids errors in confusionMatrix()
# caused by missing or differently ordered factor levels.
pred_train <- factor(
  pred_train,
  levels = class_levels
)

pred_valid <- factor(
  pred_valid,
  levels = class_levels
)



# ==========================================================
# Calculate confusion matrices
# ==========================================================

# "good" is treated as the positive class.
cm_train <- confusionMatrix(
  data = pred_train,
  reference = y_train,
  positive = "good"
)

cm_valid <- confusionMatrix(
  data = pred_valid,
  reference = y_valid,
  positive = "good"
)

# Complete results for the training data
cm_train

# Complete results for the validation data
cm_valid

# ==========================================================
# Extract selected performance measures
# ==========================================================

train_results <- c(
  Accuracy = unname(cm_train$overall["Accuracy"]),
  Sensitivity = unname(cm_train$byClass["Sensitivity"]),
  Specificity = unname(cm_train$byClass["Specificity"])
)

valid_results <- c(
  Accuracy = unname(cm_valid$overall["Accuracy"]),
  Sensitivity = unname(cm_valid$byClass["Sensitivity"]),
  Specificity = unname(cm_valid$byClass["Specificity"])
)

# Accuracy, sensitivity and specificity on the training data
round(train_results, 3)

# Accuracy, sensitivity and specificity on the validation data
round(valid_results, 3)
