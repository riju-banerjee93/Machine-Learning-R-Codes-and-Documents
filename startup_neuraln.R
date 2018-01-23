Startup_data <- read.csv(file.choose(), stringsAsFactors = F)
attach(Startup_data)
str(Startup_data)
library(Hmisc)
describe(Startup_data)
#there are no missing values in our dataset
#All the variables are normally distributed 
#Startup_data$State <- as.factor(Startup_data$State)
str(Startup_data)
#we will convert the state variable into dummy variables because neuralnet() function
#does not work on factors

new_york <- as.data.frame(ifelse(Startup_data$State == "New York",1,0))
california <- as.data.frame(ifelse(Startup_data$State == "California",1,0))
Startup_data <- cbind(Startup_data[,-4],new_york)
Startup_data <- cbind(Startup_data,california)
class(Startup_data)
#using below code we can change the name of columns
a = c("","","","","new_york", "california")
a[5]

for (i in 5:6) {
  names(Startup_data)[i] <- a[i]
  
}

library(dplyr)
library(magrittr)

normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#we will normalize the entire dataset
Startup_data_norm <- as.data.frame(lapply(Startup_data, FUN=normal))

train_data <- Startup_data_norm[1:35,]
test_data <- Startup_data_norm[36:50,]

attach(Startup_data)
library(neuralnet)
library(nnet)
?nnet
m1 <- neuralnet(Profit~ R.D.Spend+Administration+Marketing.Spend+new_york+california, train_data)
str(m1)
plot(m1)
?compute
m1_results <- compute(m1,test_data[,c(1:3,5,6)])
predicted_profit <- m1_results$net.result
cor(predicted_profit,test_data$Profit)
plot(predicted_profit,test_data$Profit)

m2 <- neuralnet(Profit~ R.D.Spend+Administration+Marketing.Spend+new_york+california, train_data,hidden = 2)
str(m2)
plot(m2)
?compute
m2_results <- compute(m2,test_data[,c(1:3,5,6)])
predicted_profit1 <- m2_results$net.result
cor(predicted_profit1,test_data$Profit)
plot(predicted_profit1,test_data$Profit)
