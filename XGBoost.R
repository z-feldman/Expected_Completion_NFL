
# Load Libraries ----------------------------------------------------------

library(xgboost)
library(caret)
library(tidyverse)
library(e1071)
library(gmailr)
library(glue)
library(magrittr)


# Load Data ---------------------------------------------------------------


cpoe_passes <- readRDS("data/cpoe_passes")


# Select only the variables needed for the model --------------------------

model_vars <- cpoe_passes %>% 
  select(complete_pass, air_yards, yardline_100, ydstogo, receiver_position, 
         down, qtr, wp, ep, air_is_zero, pass_is_middle, under_two_min, early_downs, 
         tipped, hail_mary, roof, posteam_type, week)


# Convert chr to factor ---------------------------------------------------

model_vars %<>% modify_if(is_character, as_factor)


# check to make sure no characters
str(model_vars)

# set seed for reproducibility -------------------------------------------
set.seed(69)

# create indecies for splitting (need to use a specific column from df for it to run)
train_index <- createDataPartition(y = model_vars$complete_pass, p = .7, list = FALSE)

# split into train and test
train <- model_vars[train_index,]
test <- model_vars[-train_index,]


# convert the train and test into xgb.DMatrix
# using model.matrix will handle converting factors to dummy columns
x_train = xgb.DMatrix(model.matrix(~.+0, data = train %>% select(-complete_pass)), label = train$complete_pass)
y_train = train$complete_pass %>% as.factor() 

x_test = xgb.DMatrix(model.matrix(~.+0, data = test %>% select(-complete_pass)), label = test$complete_pass)
y_test = test$complete_pass %>% as.factor() 

full_train = xgb.DMatrix(model.matrix(~., data = model_vars %>% select(-complete_pass)), label = model_vars$complete_pass)
# tuning parameters to try
xgb_tgrid <- expand.grid(
  nrounds = c(15:20),
  eta = c(0.2, 0.3),
  gamma = 4,
  colsample_bytree = 0.7,
  subsample = 0.7,
  max_depth = c(4:6),
  min_child_weight = c(1:3))

# create seeds for reproducible parallel processing
# set seed for randomly obtaing seeds
set.seed(1)

# length = num folds + 1
seeds <- vector(mode = "list", length = 6)

# seed for each of the parameter options (hence nrow(tgrid))
for (i in 1:5) {
  seeds[[i]] <- sample.int(1000, nrow(xgb_tgrid))
}

# final model seed, only 1  
seeds[[6]] <- sample.int(100, 1)


# specifying cv method and num folds, enable parallel compute
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE,
  seeds = seeds)

set.seed(12345)
# train model
start_time <- proc.time()
xgb_model <- caret::train(x_train,
                         y_train,
                         trControl = xgb_trcontrol,
                         tuneGrid = xgb_tgrid,
                         method = "xgbTree")
end_time <- proc.time()
run_time <- (end_time - start_time)[3] %>% lubridate::seconds_to_period() %>% round()


# setup email
gm_auth_configure(path = "credentials.json")
gm_auth(email = TRUE)


model_email <-
  gm_mime() %>%
  gm_to("8477224534@vtext.com") %>%
  gm_from("bosoxboy3@gmail.com") %>%
  gm_subject("") %>%
  gm_text_body(glue("Run Time: {run_time}
  Accuracy: {xgb_model$results$Accuracy %>% max() %>% round(4)}
  nrounds: {xgb_model$bestTune$nrounds}
  max_depth: {xgb_model$bestTune$max_depth}
  eta: {xgb_model$bestTune$eta}
  gamma: {xgb_model$bestTune$gamma}
  colsample_bytree: {xgb_model$bestTune$colsample_bytree}"))

gm_send_message(model_email)


xgb_model$finalModel$param




# using the xgboost package and cv method built in

params <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.1, gamma = 3, max_depth = 6, min_child_weight = 1, subsample = 1, colsample_bytree = 1, lambda = 10, alpha = 5)
xcv_1 <-xgboost::xgb.cv(params = params, data = x_train, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 1, early_stopping_rounds = 20, seed = 69)


# which round was lowest test error from
nrounds <- xcv_1$best_iteration

# train model with that number of rounds
xgb_mod <- xgboost::xgboost(params = params, data = x_train, nrounds = nrounds, verbose = 2, seed = 69)

# plot importance of variables
xgb.ggplot.importance(xgb.importance(model = xgb.Booster.complete(xgb_mod)))

xgb.importance(feature_names = colnames(xgb_mod), model = xgb_mod, target = )


# make prediction with model
xgb_mod_pred <- predict(xgb_mod, x_test)

# add predictions to test data
test$pred <- xgb_mod_pred

# find abs and sq error
test %<>% mutate(error = complete_pass - pred)

test %>% summarise(mean_abs_error = mean(abs(error)),
                   mean_sq_error = mean(error^2),
                   root_mean_sq_error = sqrt(mean(error^2)))

# tune for full dataset
set.seed(1)
xgb_cv_full <- xgboost::xgb.cv(params = params, data = full_train, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 1, early_stopping_rounds = 10)

# what was the lowest error
min(xgb_cv_full$evaluation_log$test_error_mean)
min(xgb_cv_full$evaluation_log$train_error_mean)

# which round was it from
nrounds <- which(xgb_cv_full$evaluation_log$test_error_mean == min(xgb_cv_full$evaluation_log$test_error_mean))[1]
which(xgb_cv_full$evaluation_log$train_error_mean == min(xgb_cv_full$evaluation_log$train_error_mean))

xgb_mod_full <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)


# plot importance of variables
xgb.ggplot.importance(xgb.importance(model = xgb.Booster.complete(xgb_mod_full)))


# make prediction with model
xgb_mod_full_pred <- predict(xgb_mod_full, full_train)

# add predictions to test data
model_vars$pred <- xgb_mod_full_pred

# find abs and sq error
model_vars %<>% mutate(error = complete_pass - pred)

model_vars %>% summarise(mean_abs_error = mean(abs(error)),
                   mean_sq_error = mean(error^2),
                   root_mean_sq_error = sqrt(mean(error^2)))
cpoe_passes$pred <- model_vars$pred
# now do to YoYTest to compare with other models

