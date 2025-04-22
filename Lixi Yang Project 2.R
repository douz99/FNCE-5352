install.packages(c("tidyverse", "caret", "e1071", "randomForest", "xgboost",
                   "pROC", "naivebayes", "class", "glmnet", "FactoMineR", "factoextra", 
                   "kernlab","klaR", "rpart"))

library(tidyverse)
library(caret)

# load data
train <- read.csv("ConsumerCred-development.csv")
test <- read.csv("ConsumerCred-newdata.csv")

# drop ID column
train <- train[,-1]
test_id <- test$id
test <- test[,-1]

# fill missing values using median
for (col in names(train)) {
  if (any(is.na(train[[col]]))) {
    med <- median(train[[col]], na.rm = TRUE)
    train[[col]][is.na(train[[col]])] <- med
  }
}
for (col in names(test)) {
  if (any(is.na(test[[col]]))) {
    med <- median(test[[col]], na.rm = TRUE)
    test[[col]][is.na(test[[col]])] <- med
  }
}

# data normalize excluding target column
preproc <- preProcess(train[ , -1], method = c("center", "scale"))
train_scaled <- predict(preproc, train[ , -1])
test_scaled <- predict(preproc, test)

library(pROC)
library(e1071)
library(randomForest)
library(naivebayes)
library(glmnet)
library(xgboost)
library(FactoMineR)
library(factoextra)

# set seed for reproducibility
set.seed(123)

# define control for training (with 5-fold cross-validation and AUC metric)
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# add target variable back and encode it as "Yes"/"No"
train_final <- cbind(SeriousDlqin2yrs = train$SeriousDlqin2yrs, train_scaled)
train_final$SeriousDlqin2yrs <- factor(ifelse(train_final$SeriousDlqin2yrs == 1, "Yes", "No"))

model_list <- list()
auc_list <- c()

# Logistic Regression
model_list$logit <- train(SeriousDlqin2yrs ~ ., data = train_final,
                          method = "glm", family = "binomial",
                          trControl = ctrl, metric = "ROC")
auc_list["Logistic"] <- max(model_list$logit$results$ROC)

# Decision Tree
model_list$tree <- train(SeriousDlqin2yrs ~ ., data = train_final,
                         method = "rpart", trControl = ctrl, metric = "ROC")
auc_list["Decision Tree"] <- max(model_list$tree$results$ROC)

# Random Forest (Cut tree number to 100 for efficiency)
model_list$rf <- train(SeriousDlqin2yrs ~ ., data = train_final,
                       method = "rf", trControl = ctrl, metric = "ROC", 
                       ntree = 100)
auc_list["Random Forest"] <- max(model_list$rf$results$ROC)

# Support Vector Machine (Cut sample size for efficiency)
small_data <- train_final[sample(nrow(train_final), 20000), ]
model_list$svm <- train(SeriousDlqin2yrs ~ ., data = small_data,
                        method = "svmRadial", trControl = ctrl, metric = "ROC")
auc_list["SVM"] <- max(model_list$svm$results$ROC)

# K-Nearest Neighbors
model_list$knn <- train(SeriousDlqin2yrs ~ ., data = train_final,
                        method = "knn", trControl = ctrl, metric = "ROC")
auc_list["KNN"] <- max(model_list$knn$results$ROC)

# Naive Bayes
model_list$nb <- train(SeriousDlqin2yrs ~ ., data = train_final,
                       method = "naive_bayes", trControl = ctrl, metric = "ROC")
auc_list["Naive Bayes"] <- max(model_list$nb$results$ROC)

# Gradient Boosting (XGBoost)
model_list$xgb <- train(SeriousDlqin2yrs ~ ., data = train_final,
                        method = "xgbTree", trControl = ctrl, metric = "ROC")
auc_list["XGBoost"] <- max(model_list$xgb$results$ROC)

# Linear Regression (treat as probability)
lm_model <- train(as.numeric(SeriousDlqin2yrs) ~ ., data = train_final,
                  method = "lm", trControl = trainControl(method = "cv"))
preds_lm <- predict(lm_model, newdata = train_final)
roc_lm <- roc(train_final$SeriousDlqin2yrs, preds_lm)
auc_list["Linear Regression"] <- auc(roc_lm)

# PCA + Logistic Regression
pca_result <- PCA(train_final[ , -1], graph = FALSE)
pca_data <- as.data.frame(pca_result$ind$coord[, 1:5])  # top 5 PCs
pca_data$SeriousDlqin2yrs <- train_final$SeriousDlqin2yrs
model_list$pca_logit <- train(SeriousDlqin2yrs ~ ., data = pca_data,
                              method = "glm", family = "binomial",
                              trControl = ctrl, metric = "ROC")
auc_list["PCA + Logistic"] <- max(model_list$pca_logit$results$ROC)

# Print sorted AUC results
print(sort(auc_list, decreasing = TRUE))

# XGBoost is the best model
# Predict probabilities for test set
test_probs <- predict(model_list$xgb, newdata = test_scaled, type = "prob")

# Probability of SeriousDlqin2yrs = 1)
predicted_prob <- test_probs$Yes

# Create submission DataFrame
submission <- data.frame(id = test_id, SeriousDlqin2yrs = predicted_prob)

# Write to CSV
write.csv(submission, "LixiYang_Submission.csv", row.names = FALSE)