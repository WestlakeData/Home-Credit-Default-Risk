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

#Return TARGET to factor variable
train_df$TARGET <- as.factor(train_df$TARGET)
test_df$TARGET <- as.factor(test_df$TARGET)

# Set up Tasks
trainTask <- makeClassifTask(data = train_df, target = 'TARGET')
testTask <- makeClassifTask(data = test_df, target = 'TARGET')

# one-hot encoding
trainTask <- createDummyFeatures(obj = trainTask)
testTask <- createDummyFeatures(obj = testTask)

# Create Learner
lrn <- makeLearner('classif.xgboost', predict.type = 'response')
lrn$par.vals <- list(objective = 'binary:logistic',
                     booster = 'gbtree',
                     eval_metric = 'error@0.55'
)
makeWeightedClassesWrapper(lrn,wcw.weight = ratio)

# Set Hyperparameter Space
params <- makeParamSet(makeIntegerParam('max_depth', lower = 3, upper = 9),
                       makeIntegerParam('nrounds', lower = 200, upper = 500),
                       makeNumericParam('min_child_weight', lower = 5, upper = 10),
                       makeNumericParam('subsample', lower = 0.5, upper = 1),
                       makeNumericParam('gamma', lower = 0, upper = 5),
                       makeNumericParam('colsample_bytree', lower = 0.7, upper = 1),
                       makeNumericParam('eta', lower = 0.01, upper = 1))

# Set resampling
rdesc <- makeResampleDesc('CV', stratify = F, iters = 5)

# Set search
ctrl <- makeTuneControlRandom(maxit = 50)

# Set up Parallel Processing
parallelStartSocket(cpus = ceiling(detectCores()))

# Tuning
tune <- tuneParams(learner = lrn, 
                   task = trainTask,
                   resampling = rdesc,
                   measures = fpr,
                   par.set = params,
                   control = ctrl,
                   show.info = T)

lrn.tune <- setHyperPars(lrn, par.vals = tune$x)

model <- train(learner = lrn.tune, task = trainTask)

xgb.pred <- predict(model,testTask)
confusionMatrix(xgb.pred$data$response,xgb.pred$data$truth)

saveRDS(tune, 'best_model_tuned_50_55.tune')
saveRDS(model, 'best_model_tuned_50_55.xgb')
