
# Lab: Decision Trees


## Fitting Classification Trees

###
library(tree)
library(ISLR2)
attach(Carseats)
### skapar ny responsvariabel
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
# samlar alla variabler i Carseats
Carseats <- data.frame(High=High,Carseats[,-1] )
head(Carseats)

# delar upp i träning och validering
set.seed(533)
index_train<-sample(x = nrow(Carseats),size = nrow(Carseats)*0.75)
Carseats_train<-Carseats[index_train,]
Carseats_validation<-Carseats[-index_train,]


library(xgboost)
library(Matrix)


# Ta bort faktorer (XGBoost kräver numeriska variabler)
Carseats_train_matrix <- model.matrix(High ~ . - 1, data = Carseats_train)
Carseats_validation_matrix <- model.matrix(High ~ . - 1, data = Carseats_validation)

# Konvertera responsvariabel till binär (0 = No, 1 = Yes)
y_train <- as.numeric(Carseats_train$High == "Yes")
y_validation <- as.numeric(Carseats_validation$High == "Yes")

# Skapa DMatrix-objekt
dtrain <- xgb.DMatrix(data = Carseats_train_matrix, label = y_train)
dvalid <- xgb.DMatrix(data = Carseats_validation_matrix, label = y_validation)

# Anpassar modellern 
xgb_model <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 50,
  max_depth = 3,
  eta = 0.1,
  verbose = 0
)


# Beräkna och visa feature importance
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(Carseats_train_matrix))

# Plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

print(importance_matrix)


# Prediktion på träningsdatan
pred_probs_train <- predict(xgb_model, dtrain)
pred_class_train <- ifelse(pred_probs_train > 0.5, "Yes", "No")

# Confusion matrix
table(Predicted = pred_class_train, Actual = Carseats_train$High)
mean(pred_class_train==Carseats_train$High)

# Prediktion på valideringsdata
pred_probs_val <- predict(xgb_model, dvalid)
pred_class_val <- ifelse(pred_probs_val > 0.5, "Yes", "No")

# Confusion matrix
table(Predicted = pred_class_val, Actual = Carseats_validation$High)
mean(pred_class_val==Carseats_validation$High)

# utan någon hyperparametersökning så får vi rätt bra värden på 
#träffsäkerhet på valideringsdata

