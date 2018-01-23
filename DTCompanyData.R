data <- read.csv(file.choose(), stringsAsFactors = F)
attach(data)
install.packages("psych")
library(psych)
str(data)
#data$Sales <- factor(data$Sales)
summary(data)
str(data)
describe(data)
library(magrittr)
library(dplyr)
cols <- c(7,10,11)

data %<>%
  mutate_each_(funs(factor(.)),cols)
str(data)

n <- nrow(data)
n1 <-floor(n*0.70)
train <- sample(1:n,n1)

#View(glass_n[train,])
#View(glass_n[-train,])


data_train <- as.data.frame(data[train,])
data_test <- as.data.frame(data[-train,])

library(rpart)
m1 <- rpart(Sales~., data = data_train)
summary(m1)
plot(m1)
text(m1)
pred_m1 <- predict(m1,data_test)

actual <- data_test$Sales
error <- actual - pred_m1
tst.rmse <- sqrt(sum(error^2)/nrow(data_test))
trn.rmse <- sqrt(sum(m1$residuals^2)/n1)
dif1 <- abs(tst.rmse- trn.rmse)
dif1

#####################################################
library(randomForest)
m2 <- randomForest(Sales~., data = data_train, na.action = na.roughfix)
plot(m2)
pred_m2 <- predict(m2, data_test)

actual <- data_test$Sales
error <- actual - pred_m2
tst.rmse <- sqrt(sum(error^2)/nrow(data_test))
trn.rmse <- sqrt(sum(m2$residuals^2)/n1)
dif2 <- abs(tst.rmse- trn.rmse)
dif2
