#library(readr)
#startup <- read_csv("E:/career/study/BHWKD(20-11)/startup.csv")
#View(startup)

attach(startup)
#EDA
summary(startup)
require(moments)||install.packages("moments")
library("moments", lib.loc="~/R/win-library/3.4")
skewness(R.D.Spend)
#0.1590405
#right skewed
skewness(Administration)
#-0.4742301
#left skewed
skewness(Marketing.Spend)
#-0.04506632
#left skewed
skewness(Profit)
#0.02258638
#right skewed



kurtosis(R.D.Spend)
#2.194932
kurtosis(Administration)
#3.085538
kurtosis(Marketing.Spend)
#2.275967
kurtosis(Profit)
#2.824704

hist(R.D.Spend)
#More than 12 firms has spend between 60K-70K

hist(Administration)
#Most firms has spend large amount in admiministration

hist(Marketing.Spend)

hist(Profit)



boxplot(R.D.Spend,horizontal = TRUE)
boxplot(Administration,horizontal = TRUE)
boxplot(Marketing.Spend,horizontal = TRUE)
boxplot(Profit,horizontal = TRUE)
#the boxplots suggests there are no outliers.
#
qqnorm(R.D.Spend)
qqline(R.D.Spend)
qqnorm(Administration)
qqline(Administration)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
qqnorm(Profit)
qqline(Profit)
#

#
pairs(startup)
#we can clearly see a linear relationship between profit and RD Spend.
#profit is same irrespective of state
#we make a model using all the variables
m1 <- lm(Profit~ ., data = startup)
summary(m1)

influenceIndexPlot(m1,id.n = 3)
#The coefficient of Administration, marketing spend and state are insignificant
#as determined by the corresponding p-values
plot(m1)
#Heteroscedasticity problem is there
# so, we make another model, removing State
m2 <- lm(Profit~ R.D.Spend+ Administration+Marketing.Spend)
summary(m2)
#again, Administration and Marketing Spend are insignificant 
plot(m2)
#Heteroscedasticity problem is still there
av.plots(m2, id.n = 2, id.cex = 0.7)
# the profit of a firm is not dependent on amount spend on administration 
#so we remove administration and build another model
m3 <- lm(Profit~ R.D.Spend)
summary(m3)
plot(m3)
av.plots(m3, id.n = 2, id.cex = 0.7)
#we can find an influential observation, i.e., 50th observation
m4 <- lm(Profit~ R.D.Spend+ Marketing.Spend, data = startup[-50,])
summary(m4)
plot(m4)
#Marketing Spend variable has become significant
m5 <- lm(Profit~ R.D.Spend+ I(R.D.Spend^2), data = startup[-50,])
summary(m5)
infIndexPlot(m5,id.n = 3)
plot(m5)
#


n <- nrow(startup)
n1 <-floor(n*0.7)
n2 <- n-n1
train <- sample(1:n,n1)


train.m4 <- lm(Profit~ R.D.Spend+ Marketing.Spend, data = startup[train,])
summary(train.m4)
plot(train.m4)


pred <- predict(train.m4, newdata = startup[-train,])
summary(pred)
actual <- startup$Profit[-train]
error <- actual - pred
tst.rmse <- sqrt(sum(error^2)/n2)
trn.rmse <- sqrt(sum(train.m4$residuals^2)/n1)
dif1 <- abs(tst.rmse- trn.rmse)
dif1




 
