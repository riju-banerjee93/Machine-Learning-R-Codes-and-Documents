salary_data <- read.csv(file.choose(),stringsAsFactors = F)
salary_data_test <- read.csv(file.choose(),stringsAsFactors = F)

attach(salary_data)
str(salary_data)
salary_data$Salary <- factor(salary_data$Salary)
str(salary_data)

#dataset is imbalanced
#we have to convert character variables into factors
#animals <-data.frame(lapply(animals[,2:ncol(animals)],as.factor))
#salary_data <- data.frame(lapply(salary_data[,c(2,3,5:9,13)],as.factor))
#str(salary_data)
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


cols <- c(1,4,10:12)

salary_data %<>%
  mutate_each_(funs(normal),cols)
str(salary_data)
View(salary_data)

salary_data_test %<>%
  mutate_each_(funs(normal),cols)
str(salary_data_test)
#View(salary_data_test)

  
  

library("ROSE")
#salary_data_ROSE <- ROSE(Salary~., data = salary_data, seed = 3)$data
salary_data_ROSE <- as.data.frame(ovun.sample(Salary~.,data = salary_data,method = "both", N = 45306)$data)

table(salary_data_ROSE$Salary)

library("e1071")
?naiveBayes

m1 <- naiveBayes(Salary~., data = salary_data_ROSE, laplace = 2)

pred_m1 <- predict(m1,salary_data_test)

library(gmodels)
CrossTable(pred_m1, salary_data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
mean(pred_m1==salary_data_test$Salary)
