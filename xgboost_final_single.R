library(tidyverse)
library(gmodels)
library(caret)
library(mlr)
library(xgboost)
library(parallel)
library(parallelMap)
library(ggplot2)
library(pROC)
library(rbenchmark)
library(yardstick)


# Get Clean Data from Raw Data
# source('Che - Standard Data Set for Modeling.R')
# saveRDS(app_train1,'app_train1.RDS')
# saveRDS(app_train2,'app_train2.RDS')

#app_train1 <- readRDS('app_train1.RDS')
app_train1 <- readRDS('app_train2.RDS')

# Get Best Features LIst
best_features <- readRDS('best_features.RDS')

# Select best features
data <- app_train1 %>% select(c(SK_ID_CURR, TARGET, all_of(best_features)))

# Create Train and Test sets for Modeling
set.seed(1234)
train.index <- sample(nrow(data), floor(nrow(data)*0.7), replace = F)
train_df <- data[train.index,]
test_df <- data[-train.index,]
remove(train.index, app_train1, data)

#Verify Target variable distribution in Train and Test
CrossTable(train_df$TARGET)
CrossTable(test_df$TARGET)

ratio <- (sum(ifelse(train_df$TARGET == '0',1,0)))/(sum(ifelse(train_df$TARGET == '1',1,0))) # Ratio Negative Response : Positive Response

# Handle IDs
id.train <- train_df$SK_ID_CURR
id.test <- test_df$SK_ID_CURR
target.train <- ifelse(train_df$TARGET == '1',1,0)
target.test <- ifelse(test_df$TARGET == '1',1,0)

# Convert Factor Variables to Numeric (Train)
train_df <- train_df %>% select(-c(SK_ID_CURR)) 
test_df <- test_df %>% select(-c(SK_ID_CURR)) 

train_df[2:20] <- train_df[2:20] %>% replace(is.na(.), 0) # Replace NaN values
test_df[2:20] <- test_df[2:20] %>% replace(is.na(.), 0) # Replace NaN values
train_df$TARGET <- ifelse(train_df$TARGET == '1',1,0) # TARGET
for(c in 1:length(colnames(train_df))){
  if(grepl('FLAG_',colnames(train_df)[c])){ 
    train_df[,c] <- ifelse(train_df[,c] == '1',1,0) # Convert FLAG_ Factor columns to numeric
  } else if (grepl('REG_', colnames(train_df)[c])) {
    train_df[,c] <- ifelse(train_df[,c] == '1',1,0) # Convert REG_ Factor columns to numeric
  } else if (grepl('LIVE_', colnames(train_df)[c])) {
    train_df[,c] <- ifelse(train_df[,c] == '1',1,0) # Convert LIVE_ Factor columns to numeric
  } else if (grepl('AMT_REQ_CREDIT_BUREAU_', colnames(train_df)[c])) {
    train_df[c] <- lapply(train_df[c], function(x) as.numeric(as.character(x))) # Convert Credit Bureau Inquiries Columns to numeric
  }
}

# Convert Factor Variables to Numeric (Test)
test_df$TARGET <- ifelse(test_df$TARGET == '1',1,0) # TARGET
for(c in 1:length(colnames(test_df))){
  if(grepl('FLAG_',colnames(test_df)[c])){ 
    test_df[,c] <- ifelse(test_df[,c] == '1',1,0) # Convert FLAG_ Factor columns to numeric
  } else if (grepl('REG_', colnames(test_df)[c])) {
    test_df[,c] <- ifelse(test_df[,c] == '1',1,0) # Convert REG_ Factor columns to numeric
  } else if (grepl('LIVE_', colnames(test_df)[c])) {
    test_df[,c] <- ifelse(test_df[,c] == '1',1,0) # Convert LIVE_ Factor columns to numeric
  } else if (grepl('AMT_REQ_CREDIT_BUREAU_', colnames(test_df)[c])) {
    test_df[c] <- lapply(test_df[c], function(x) as.numeric(as.character(x))) # Convert Credit Bureau Inquiries Columns to numeric
  }
}

# Drop TARGET from DFs
train_df$TARGET <- NULL
test_df$TARGET <- NULL

# Convert training data to matrix for use in xgBoost
dtrain <- as.matrix(train_df)
dtest <- as.matrix(test_df)

# Create training matrix
dtrain.xgb <- xgb.DMatrix(dtrain, label = target.train, nthread = 4)

# Simple xgBoost model as baseline
model1 <- xgboost(data = dtrain.xgb,
                  subsample = 0.5333193,
                  colsample_bytree = 0.8473639,
                  max_depth = 8, 
                  min_child_weight = 6.356507,
                  nrounds = 460, 
                  eta = 0.9114226,
                  gamma = 2.946654,
                  objective = "binary:logistic", 
                  scale_pos_weight = ratio)

# Predict against training data
pred.train <- predict(model1, dtrain)
pred.train_0.5 <- as.numeric(pred.train > 0.6)
confusionMatrix(factor(pred.train_0.5),factor(target.train))

pred.test <- predict(model1, dtest)
pred.test_0.5 <- as.numeric(pred.test > 0.6)
confusionMatrix(factor(pred.test_0.5),factor(target.test))

# Feature Importance Plot
fi.matrix <- xgb.importance (feature_names = colnames(dtrain),model = model1)
fi <- xgb.plot.importance (importance_matrix = fi.matrix, col = 'red', xlab = "Relative Importance", main='Relative Importance of Available Features to Model Performance')

# ROC-AUC
test_roc = roc(target.test ~ pred.test, plot = TRUE, print.auc = TRUE, main='ROC Curve of XGBoost Model')




model_training_results <- data.frame(matrix(ncol = 6, nrow = 100))
colnames(model_training_results) <- c('nrounds','train_bal_acc','test_bal_acc','Sensitivity','Specificity','Kappa')


for (x in 1:100){
  model_training_results$nrounds[x] <- x
  # Simple xgBoost model as baseline
  model1 <- xgboost(data = dtrain.xgb,
                    subsample = 0.5333193,
                    colsample_bytree = 0.8473639,
                    max_depth = 8, 
                    min_child_weight = 6.356507,
                    nrounds = x, 
                    eta = 0.1,
                    gamma = 2.946654,
                    objective = "binary:logistic", 
                    scale_pos_weight = ratio)
  
  # Predict against training data
  pred.train <- predict(model1, dtrain)
  pred.train_0.6 <- as.numeric(pred.train > 0.5)
  train_matrix <- confusionMatrix(factor(pred.train_0.6),factor(target.train))
  model_training_results$train_bal_acc[x] <- train_matrix$byClass[11]
  
  pred.test <- predict(model1, dtest)
  pred.test_0.6 <- as.numeric(pred.test > 0.5)
  test_matrix <- confusionMatrix(factor(pred.test_0.6),factor(target.test))
  model_training_results$test_bal_acc[x] <- test_matrix$byClass[11]
  model_training_results$test_Sensitivity[x] <- test_matrix$byClass[1]
  model_training_results$test_Specificity[x] <- test_matrix$byClass[2]
  model_training_results$test_Kappa[x] <- test_matrix$overall[2]
  
  
}


# Plot model_training_results
ggplot(model_training_results)+
  geom_line(aes(nrounds, train_bal_acc), color = 'blue')+
  geom_line(aes(nrounds, test_bal_acc), color = '#800000') +
  geom_line(aes(nrounds, test_Specificity), color = '#F64A8A') +
  geom_line(aes(nrounds, test_Sensitivity), color='red') +
  geom_line(aes(nrounds, test_Kappa), color = 'darkgreen')
