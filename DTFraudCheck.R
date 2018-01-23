fraud_data <- read.csv(file.choose(), stringsAsFactors = F)
attach(fraud_data)
str(fraud_data)
fraud_data$Taxable.Income <- cut(fraud_data$Taxable.Income, breaks = c(0,30000,1000000), labels = c("Risky","Good"))
str(fraud_data)
summary(fraud_data)

library(magrittr)
library(dplyr)
cols <- c(1,2,6)

fraud_data %<>%
  mutate_each_(funs(factor(.)),cols)
str(fraud_data)


table(fraud_data$Taxable.Income)
library("C50")
library("tree")


n <- nrow(fraud_data)
n1 <-floor(n*0.70)
train <- sample(1:n,n1)
fraud_data_train <- as.data.frame(fraud_data[train,])
prop.table(table(fraud_data_train$Taxable.Income))
fraud_data_test <- as.data.frame(fraud_data[-train,])
prop.table(table(fraud_data_test$Taxable.Income))
?C5.0
m1 <- C5.0(fraud_data_train[,-3],fraud_data_train$Taxable.Income)
#m1 <- tree(Taxable.Income~., fraud_data_train)
summary(m1)
plot(m1)

pred_m1 <- predict(m1,fraud_data_test)
pred_m1

library(gmodels)
CrossTable(fraud_data_test$Taxable.Income,pred_m1)
mean(pred_m1==fraud_data_test$Taxable.Income)

##########################################################################3333
library("ROSE")
#salary_data_ROSE <- ROSE(Salary~., data = salary_data, seed = 3)$data
fraud_data_sampled <- as.data.frame(ovun.sample(Taxable.Income~.,data =fraud_data,method = "both", N =952)$data)
table(fraud_data_sampled$Taxable.Income)

n <- nrow(fraud_data_sampled)
n1 <-floor(n*0.7)
train <- sample(1:n,n1)

fraud_data_train <- as.data.frame(fraud_data_sampled[train,])
prop.table(table(fraud_data_train$Taxable.Income))
fraud_data_test <- as.data.frame(fraud_data_sampled[-train,])
prop.table(table(fraud_data_test$Taxable.Income))
?C5.0
m2 <- C5.0(fraud_data_train[,-3],fraud_data_train$Taxable.Income)
#m1 <- tree(Taxable.Income~., fraud_data_train)
summary(m2)
plot(m2)

pred_m2 <- predict(m2,fraud_data_test)
pred_m2
mean(pred_m2==fraud_data_test$Taxable.Income)
library("gmodels")
CrossTable(fraud_data_test$Taxable.Income,pred_m2)
###############################################################
#Random Forest#######################
###############################################################
#install.packages("randomForest")
library(randomForest)
table(is.na(fraud_data_train))
str(fraud_data_train)
m3 <- randomForest(Taxable.Income~., data = fraud_data_train,na.action = na.roughfix)
plot(m3)
pred_m3 <- predict(m3, fraud_data_test)
CrossTable(fraud_data_test$Taxable.Income,pred_m3)
mean(fraud_data_test$Taxable.Income== pred_m3)
###############################################################3