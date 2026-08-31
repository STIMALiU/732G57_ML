############################################################
# Random Forest tutorial
# Course: 732G57
#
# Dataset:
#   MDRR data from the caret package
#
# Models:
#   1. One unpruned tree
#   2. Bagging
#   3. Random Forest with mtry = sqrt(p)
#   4. Random Forest with mtry = p / 3
############################################################


############################################################
# 1. Load packages and data
############################################################

library(caret)
library(randomForest)

data(mdrr)

# Predictor matrix
X <- mdrrDescr

# Binary response
y <- mdrrClass

# Inspect the data
dim(X)
table(y)
str(y)

# Number of observations and predictors
n <- nrow(X)
p <- ncol(X)

cat("Number of observations:", n, "\n")
cat("Number of predictors:", p, "\n")
cat("Classes:\n")
print(table(y))


############################################################
# 2. Remove near-zero variance predictors
############################################################

# Some molecular descriptors may contain almost no variation.
# These variables contribute little information and can create
# unnecessary computations.

nzv_index <- nearZeroVar(X)

if (length(nzv_index) > 0) {
  X <- X[, -nzv_index, drop = FALSE]
}

p <- ncol(X)

cat("Number of predictors after preprocessing:", p, "\n")


############################################################
# 3. Split into training and validation data
############################################################

set.seed(73257)

# Stratified split:
# approximately 70% from each class goes to training data.

train_index <- createDataPartition(
  y = y,
  p = 0.70,
  list = FALSE
)

X_train <- X[train_index, , drop = FALSE]
X_valid <- X[-train_index, , drop = FALSE]

y_train <- y[train_index]
y_valid <- y[-train_index]

cat("\nTraining data:\n")
print(dim(X_train))
print(table(y_train))

cat("\nValidation data:\n")
print(dim(X_valid))
print(table(y_valid))


############################################################
# 4. Choose values for mtry
############################################################

p <- ncol(X_train)

mtry_bagging <- p
mtry_sqrt <- max(1, floor(sqrt(p)))
mtry_third <- max(1, floor(p / 3))

cat("\nValues of mtry:\n")
cat("Bagging:", mtry_bagging, "\n")
cat("sqrt(p):", mtry_sqrt, "\n")
cat("p / 3:", mtry_third, "\n")


############################################################
# 5. Model 1: One tree
############################################################

# The randomForest package does not fit a pruned CART tree.
#
# Here we create one randomForest-style tree:
#   - ntree = 1 gives one tree
#   - mtry = p allows all predictors at each split
#   - replace = FALSE and sampsize = n use all training data
#
# This is suitable for illustrating the transition from
# one tree to bagging and Random Forest.

set.seed(1001)

fit_tree <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 1,
  mtry = p,
  replace = FALSE,
  sampsize = nrow(X_train),
  nodesize = 1,
  importance = TRUE
)

print(fit_tree)


############################################################
# 6. Model 2: Bagging
############################################################

# Bagging uses all p predictors as candidates at every split.
#
# Each tree is fitted to a bootstrap sample, but there is no
# random predictor selection at the splits.

set.seed(1002)

fit_bagging <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 500,
  mtry = mtry_bagging,
  replace = TRUE,
  nodesize = 1,
  importance = TRUE
)

print(fit_bagging)


############################################################
# 7. Model 3: Random Forest with mtry = sqrt(p)
############################################################

# At each split, approximately sqrt(p) randomly selected
# predictors are considered.

set.seed(1003)

fit_rf_sqrt <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 500,
  mtry = mtry_sqrt,
  replace = TRUE,
  nodesize = 1,
  importance = TRUE
)

print(fit_rf_sqrt)


############################################################
# 8. Model 4: Random Forest with mtry = p / 3
############################################################

# A larger set of candidate predictors is considered
# at each split.

set.seed(1004)

fit_rf_third <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 500,
  mtry = mtry_third,
  replace = TRUE,
  nodesize = 1,
  importance = TRUE
)

print(fit_rf_third)


############################################################
# 9. Predicted classes for training data
############################################################

tree_train_class <- predict(
  fit_tree,
  newdata = X_train,
  type = "response"
)

bagging_train_class <- predict(
  fit_bagging,
  newdata = X_train,
  type = "response"
)

rf_sqrt_train_class <- predict(
  fit_rf_sqrt,
  newdata = X_train,
  type = "response"
)

rf_third_train_class <- predict(
  fit_rf_third,
  newdata = X_train,
  type = "response"
)


############################################################
# 10. Predicted classes for validation data
############################################################

tree_valid_class <- predict(
  fit_tree,
  newdata = X_valid,
  type = "response"
)

bagging_valid_class <- predict(
  fit_bagging,
  newdata = X_valid,
  type = "response"
)

rf_sqrt_valid_class <- predict(
  fit_rf_sqrt,
  newdata = X_valid,
  type = "response"
)

rf_third_valid_class <- predict(
  fit_rf_third,
  newdata = X_valid,
  type = "response"
)


############################################################
# 11. Predicted probabilities for training data
############################################################

tree_train_prob <- predict(
  fit_tree,
  newdata = X_train,
  type = "prob"
)

bagging_train_prob <- predict(
  fit_bagging,
  newdata = X_train,
  type = "prob"
)

rf_sqrt_train_prob <- predict(
  fit_rf_sqrt,
  newdata = X_train,
  type = "prob"
)

rf_third_train_prob <- predict(
  fit_rf_third,
  newdata = X_train,
  type = "prob"
)


############################################################
# 12. Predicted probabilities for validation data
############################################################

tree_valid_prob <- predict(
  fit_tree,
  newdata = X_valid,
  type = "prob"
)

bagging_valid_prob <- predict(
  fit_bagging,
  newdata = X_valid,
  type = "prob"
)

rf_sqrt_valid_prob <- predict(
  fit_rf_sqrt,
  newdata = X_valid,
  type = "prob"
)

rf_third_valid_prob <- predict(
  fit_rf_third,
  newdata = X_valid,
  type = "prob"
)


############################################################
# 13. Collect fitted values for training data
############################################################

train_predictions <- data.frame(
  observed = y_train,
  tree = tree_train_class,
  bagging = bagging_train_class,
  rf_sqrt = rf_sqrt_train_class,
  rf_p_third = rf_third_train_class,
  tree_prob_active = tree_train_prob[, "Active"],
  bagging_prob_active = bagging_train_prob[, "Active"],
  rf_sqrt_prob_active = rf_sqrt_train_prob[, "Active"],
  rf_p_third_prob_active = rf_third_train_prob[, "Active"]
)

head(train_predictions)


############################################################
# 14. Collect predictions for validation data
############################################################

valid_predictions <- data.frame(
  observed = y_valid,
  tree = tree_valid_class,
  bagging = bagging_valid_class,
  rf_sqrt = rf_sqrt_valid_class,
  rf_p_third = rf_third_valid_class,
  tree_prob_active = tree_valid_prob[, "Active"],
  bagging_prob_active = bagging_valid_prob[, "Active"],
  rf_sqrt_prob_active = rf_sqrt_valid_prob[, "Active"],
  rf_p_third_prob_active = rf_third_valid_prob[, "Active"]
)

head(valid_predictions)


############################################################
# 15. Compute accuracy
############################################################

classification_accuracy <- function(observed, predicted) {
  
  mean(observed == predicted)
  
}

accuracy_results <- data.frame(
  model = c(
    "One tree",
    "Bagging",
    "Random Forest: sqrt(p)",
    "Random Forest: p / 3"
  ),
  mtry = c(
    p,
    mtry_bagging,
    mtry_sqrt,
    mtry_third
  ),
  train_accuracy = c(
    classification_accuracy(y_train, tree_train_class),
    classification_accuracy(y_train, bagging_train_class),
    classification_accuracy(y_train, rf_sqrt_train_class),
    classification_accuracy(y_train, rf_third_train_class)
  ),
  validation_accuracy = c(
    classification_accuracy(y_valid, tree_valid_class),
    classification_accuracy(y_valid, bagging_valid_class),
    classification_accuracy(y_valid, rf_sqrt_valid_class),
    classification_accuracy(y_valid, rf_third_valid_class)
  )
)

print(accuracy_results)


############################################################
# 16. Confusion matrices
############################################################

cat("\n==================================================\n")
cat("One tree: validation data\n")
cat("==================================================\n")

print(
  confusionMatrix(
    data = tree_valid_class,
    reference = y_valid,
    positive = "Active"
  )
)

cat("\n==================================================\n")
cat("Bagging: validation data\n")
cat("==================================================\n")

print(
  confusionMatrix(
    data = bagging_valid_class,
    reference = y_valid,
    positive = "Active"
  )
)

cat("\n==================================================\n")
cat("Random Forest with mtry = sqrt(p): validation data\n")
cat("==================================================\n")

print(
  confusionMatrix(
    data = rf_sqrt_valid_class,
    reference = y_valid,
    positive = "Active"
  )
)

cat("\n==================================================\n")
cat("Random Forest with mtry = p / 3: validation data\n")
cat("==================================================\n")

print(
  confusionMatrix(
    data = rf_third_valid_class,
    reference = y_valid,
    positive = "Active"
  )
)


############################################################
# 17. Compare training and validation accuracy
############################################################

accuracy_matrix <- rbind(
  training = accuracy_results$train_accuracy,
  validation = accuracy_results$validation_accuracy
)

colnames(accuracy_matrix) <- accuracy_results$model

barplot(
  accuracy_matrix,
  beside = TRUE,
  ylim = c(0, 1),
  ylab = "Accuracy",
  main = "Training and validation accuracy",
  legend.text = rownames(accuracy_matrix),
  args.legend = list(
    x = "bottomright",
    inset = 0.02
  )
)


############################################################
# 18. Out-of-bag error
############################################################

# The one-tree model is excluded because the tree used all
# observations without replacement and therefore has no
# meaningful out-of-bag sample.

cat("\nFinal OOB error for bagging:\n")
print(tail(fit_bagging$err.rate[, "OOB"], 1))

cat("\nFinal OOB error for Random Forest with sqrt(p):\n")
print(tail(fit_rf_sqrt$err.rate[, "OOB"], 1))

cat("\nFinal OOB error for Random Forest with p / 3:\n")
print(tail(fit_rf_third$err.rate[, "OOB"], 1))


############################################################
# 19. Plot out-of-bag error as trees are added
############################################################

plot(
  fit_bagging,
  main = "Bagging: out-of-bag error"
)

plot(
  fit_rf_sqrt,
  main = "Random Forest with mtry = sqrt(p)"
)

plot(
  fit_rf_third,
  main = "Random Forest with mtry = p / 3"
)


############################################################
# 20. Variable importance
############################################################

varImpPlot(
  fit_rf_sqrt,
  n.var = 20,
  main = "Variable importance: Random Forest: m=sqrt(p)"
)


varImpPlot(
  fit_rf_third,
  n.var = 20,
  main = "Variable importance: Random Forest: m=p/3"
)

varImpPlot(
  fit_bagging,
  n.var = 20,
  main = "Variable importance: Bagging"
)


############################################################
# 21. Inspect individual predictions
############################################################

head(
  valid_predictions[
    order(-valid_predictions$rf_sqrt_prob_active),
  ],
  10
)


############################################################
# 22. Final summary
############################################################

print(accuracy_results)

