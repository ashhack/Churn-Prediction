install.packages("ROCR")
library(ROCR)
library(caTools)

#Splitting
set.seed(100)
split = sample.split(train$churn_bool, SplitRatio = 0.75 )
train_train <- subset(train, split==TRUE)
train_test <- subset(train, split==FALSE)

#Modelling
log.1 <- glm(churn_bool ~., data = train_train, family = binomial)#AIC 2664


predict.train <- predict(log.1, type = "response")
predict.test <- predict(log.1, type = "response", newdata = train_test)

tapply(predict.train, train_train$churn_bool, mean)
tapply(predict.test, train_test$churn_bool, mean)


truth_table <- table(train_train$churn_bool, predict.train >0.5)
truth_table.1 <- table(train_train$churn_bool, predict.test >0.5)

#ROC curve
library(ROCR)
#train
ROCRpred = prediction(predict.train, train_train$churn_bool)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = TRUE)
as.numeric(performance(ROCRpred,"auc")@y.values)

#test
ROCRpred1 = prediction(predict.test, train_test$churn_bool)
ROCRpref1 = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref1, colorize = TRUE)
as.numeric(performance(ROCRpred1,"auc")@y.values)


