data("iris")
attach(iris)
str(iris)

iris_setosa <- as.data.frame(iris[which(iris$Species=="setosa"),])
iris_versicolor <- as.data.frame(iris[which(iris$Species=="versicolor"),])
iris_virginica <- as.data.frame(iris[which(iris$Species=="virginica"),])

iris_train <- as.data.frame(rbind(iris_setosa[1:35,],iris_versicolor[1:35,],iris_virginica[1:35,]))
iris_test <- as.data.frame(rbind(iris_setosa[36:50,],iris_versicolor[36:50,],iris_virginica[36:50,]))

#install.packages("party")
library(party)
m1 <- ctree(Species~., data = iris_train)
plot(m1)
pred_m1 <- predict(m1,iris_test)
library(gmodels)
CrossTable(iris_test$Species,pred_m1)
mean(iris_test$Species==pred_m1)
#################################################
#Random Forest###############
#################################################
library("randomForest")

m2 <- randomForest(Species~., data = iris_train)
plot(m2)

pred_m2 <- predict(m2,iris_test)
library(gmodels)
CrossTable(iris_test$Species,pred_m2)
mean(iris_test$Species==pred_m2)
##################################################3