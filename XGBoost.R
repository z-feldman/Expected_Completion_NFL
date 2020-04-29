
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
         tipped, hail_mary, roof, posteam_type, week, season)


# Clean Data Type ---------------------------------------------------

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
x_test = xgb.DMatrix(model.matrix(~.+0, data = test %>% select(-complete_pass)), label = test$complete_pass)

full_train = xgb.DMatrix(model.matrix(~., data = model_vars %>% select(-complete_pass)), label = model_vars$complete_pass)


# using the xgboost package and cv method built in, test different parameters
set.seed(69)
params <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.15, gamma = 12, max_depth = 6, min_child_weight = 1, subsample = 1, colsample_bytree = 1, base_score = mean(model_vars$complete_pass), seed = 69)
xcv_1 <-xgboost::xgb.cv(params = params, data = x_train, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 1, early_stopping_rounds = 20)


# which round was lowest best iteration from
nrounds <- xcv_1$best_iteration

# train model with that number of rounds
set.seed(69)
xgb_mod <- xgboost::xgboost(params = params, data = x_train, nrounds = nrounds, verbose = 2)

# plot importance of variables
importance_plot <- xgb.ggplot.importance(xgb.importance(model = xgb.Booster.complete(xgb_mod)))

importance_plot

importance <- xgb.importance(feature_names = colnames(xgb_mod), model = xgb_mod)

importance

# make prediction with model
xgb_mod_pred <- predict(xgb_mod, x_test)

# add predictions to test data
test$pred <- xgb_mod_pred

# find abs and sq error
test %<>% mutate(error = complete_pass - pred)

test %>% summarise(mean_abs_error = mean(abs(error)),
                   mean_sq_error = mean(error^2),
                   root_mean_sq_error = sqrt(mean(error^2)))

# Train on All Data ---------
set.seed(69)
xgb_mod_full <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)


# make prediction
xgb_mod_full_pred <- predict(xgb_mod_full, full_train)

# add predictions to all the data
full_pred_data <- data.frame("complete_pass" = model_vars$complete_pass, "pred" = xgb_mod_full_pred)

# find abs and sq error
full_pred_data %<>% mutate(error = complete_pass - pred)

full_pred_data %>% summarise(mean_abs_error = mean(abs(error)),
                   mean_sq_error = mean(error^2),
                   root_mean_sq_error = sqrt(mean(error^2)))

