attach(calories_consumed)
#EDA
summary(calories_consumed)
sd(wtgn)
sd(Cc)
var(wtgn)
var(Cc)
#Visualisation
hist(wtgn)
hist(Cc)
#
boxplot(wtgn, horizontal = TRUE)
boxplot(Cc, horizontal = TRUE)
#
qqnorm(wtgn)
qqline(wtgn)#not normally distributed
qqnorm(Cc)
qqline(Cc)
#
plot(Cc,wtgn)
#
m1 <- lm(wtgn~ Cc)
summary(m1)
plot(m1)
#########################################
m2 <- lm(wtgn~ Cc + I(Cc^2))
summary(m2)
plot(m2)
#
m3 <- lm(wtgn~ log(Cc))
summary(m3)
plot(m3)
#
m4 <- lm(wtgn~ log(Cc)+ I(log(Cc)^2))
summary(m4)
plot(m4)
#
n <- nrow(calories_consumed)
n1 <-floor(n*0.7)
n2 <- n-n1
train <- sample(1:n,n1)


train.m4 <- lm(wtgn~ log(Cc)+ I(log(Cc)^2), data = calories_consumed[train,])
summary(train.m4)
plot(train.m4)

pred <- predict(train.m4, newdata = calories_consumed[-train,])
summary(pred)
actual <- calories_consumed$wtgn[-train]
error <- actual - pred
tst.rmse <- sqrt(sum(error^2)/n2)
trn.rmse <- sqrt(sum(train.m4$residuals^2)/n1)
dif1 <- abs(tst.rmse- trn.rmse)
dif1

