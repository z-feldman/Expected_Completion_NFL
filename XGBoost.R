sample <- sample.int(n = nrow(cpoe_plays), size = floor(0.7*nrow(cpoe_plays)), replace = F)
train <- cpoe_plays[sample,]
test <- cpoe_plays[-sample,]

train %>% sparse.model.matrix(complete_pass~.-1, data = .)

bst <- xgboost(data = as.matrix(train[,-1]), 
               label = as.matrix(train[,1]),
               nrounds = 15,
               objective = "binary:logistic")
xgboost::xgb.importance(model = bst) %>% xgboost::xgb.ggplot.importance()

