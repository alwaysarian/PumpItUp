require(caret) #select tuning parameters
require(e1071) #SVM
require(MASS)

train <- read.csv('train.csv', as.is = T);
test <- read.csv('test.csv', as.is = T)

train <- train[,c(1,2,5,10,14,18,26,27,28,29,30,31,32,33,34,35,36,37,38,40,41)]
test <- test[,c(1,2,5,10,14,18,26,27,28,29,30,31,32,33,34,35,36,37,38,40)]




train$extraction_type_group <- as.factor(train$extraction_type_group)
train$extraction_type_class <- as.factor(train$extraction_type_class)
train$management <- as.factor(train$management)
train$management_group <- as.factor(train$management_group)
train$payment_type <- as.factor(train$payment_type)
train$payment <- as.factor(train$payment)
train$water_quality <- as.factor(train$water_quality)
train$quality_group  <- as.factor(train$quality_group)
train$quantity <- as.factor(train$quantity)
train$quantity_group <- as.factor(train$quantity_group)
train$source <- as.factor(train$source)
train$source_type <- as.factor(train$source_type)
train$source_class <- as.factor(train$source_class)
train$waterpoint_type <- as.factor(train$waterpoint_type)
train$waterpoint_type_group <- as.factor(train$waterpoint_type_group)
train$status_group <- as.factor(train$status_group)


test$extraction_type_group <- as.factor(test$extraction_type_group)
test$extraction_type_class <- as.factor(test$extraction_type_class)
test$management <- as.factor(test$management)
test$management_group <- as.factor(test$management_group)
test$payment_type <- as.factor(test$payment_type)
test$payment <- as.factor(test$payment)
test$water_quality <- as.factor(test$water_quality)
test$quality_group  <- as.factor(test$quality_group)
test$quantity <- as.factor(test$quantity)
test$quantity_group <- as.factor(test$quantity_group)
test$source <- as.factor(test$source)
test$source_type <- as.factor(test$source_type)
test$source_class <- as.factor(test$source_class)
test$waterpoint_type <- as.factor(test$waterpoint_type)
test$waterpoint_type_group <- as.factor(test$waterpoint_type_group)

train <- na.omit(train)
test<- na.omit(test)










knn4 <- train(status_group ~ ., data = train, method = "knn",
              
              preProcess = c("center", "scale"), tuneLength = 10,
              
              
              trControl = trainControl(method = "cv"))

update(knn4, list(.k = 9))

knn4_pred <- predict(knn4,newdata = test)

knn4_pred

FINALSUB <-data.frame(test$id, knn4_pred)

write.csv(file = "final1.csv", FINALSUB)