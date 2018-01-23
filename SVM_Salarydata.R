salary_data <- read.csv(file.choose(),stringsAsFactors = F)
salary_data_test <- read.csv(file.choose(),stringsAsFactors = F)

attach(salary_data)
str(salary_data)
salary_data$Salary <- factor(salary_data$Salary)
str(salary_data)
prop.table(table(salary_data$Salary))
#dataset is imbalanced, so  we have to handle the imbalanced dataset
#we have to convert character variables into factors


library(magrittr)
library(dplyr)
cols <- c(2,3,5:9,13)

salary_data %<>%
  mutate_each_(funs(factor(.)),cols)
str(salary_data)
table(salary_data$Salary)

salary_data_test %<>%
  mutate_each_(funs(factor(.)),cols)
str(salary_data_test)
table(salary_data_test$Salary)

?dplyr
?magrittr
?mutate_each
normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#normalize the training and test dataset
cols <- c(1,4,10:12)

salary_data %<>%
  mutate_each_(funs(normal),cols)
str(salary_data)
View(salary_data)

salary_data_test %<>%
  mutate_each_(funs(normal),cols)
str(salary_data_test)
#View(salary_data_test)



#sampling technique for handling imbalancy
library("ROSE")
#salary_data_ROSE <- ROSE(Salary~., data = salary_data, seed = 3)$data
salary_data_ROSE <- as.data.frame(ovun.sample(Salary~.,data = salary_data,method = "both", N = 45306)$data)

table(salary_data_ROSE$Salary)
library(kernlab)
library(caret)
m1 <- ksvm(Salary~., salary_data_ROSE,kernel = "vanilladot")
pred_m1 <- predict(m1, salary_data_test)

library(gmodels)
CrossTable(salary_data_test$Salary, pred_m1)
mean(salary_data_test$Salary== pred_m1)

m2 <- ksvm(Salary~., salary_data_ROSE,kernel = "rbfdot")
pred_m2 <- predict(m2, salary_data_test)
CrossTable(salary_data_test$Salary, pred_m2)
mean(salary_data_test$Salary== pred_m2)

m3 <- ksvm(Salary~., salary_data_ROSE,kernel = "polydot")
pred_m3 <- predict(m3, salary_data_test)
CrossTable(salary_data_test$Salary, pred_m3)
mean(salary_data_test$Salary== pred_m3)

m4 <- ksvm(Salary~., salary_data_ROSE,kernel = "tanhdot")
pred_m4 <- predict(m4, salary_data_test)
CrossTable(salary_data_test$Salary, pred_m4)
mean(salary_data_test$Salary== pred_m4)

m5 <- ksvm(Salary~., salary_data_ROSE,kernel = "besseldot")
pred_m5 <- predict(m5, salary_data_test)
CrossTable(salary_data_test$Salary, pred_m5)
mean(salary_data_test$Salary== pred_m5)

#we made different models using the kernel trick and will use the model that has 
#maximum accuracy