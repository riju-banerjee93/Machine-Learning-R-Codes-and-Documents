
attach(delivery_time)
summary(delivery_time)
sd(Dt)
sd(St)
hist(Dt)
hist(St)
boxplot(Dt)
boxplot(St)
qqnorm(Dt)
qqline(Dt)
qqnorm(St)
qqline(St)
#Not normal, So we transform the data
plot(St,Dt)
#Model without any transformation
m1 <- lm(Dt~St)
summary(m1)
################################
Dt1 <- log(Dt)
plot(St,Dt1)
m2 <- lm(Dt1~ St)
summary(m2)
########################
Dt2 <- sqrt(Dt)
plot(St,Dt2)
m3 <- lm(Dt2~ St)
summary(m3)
######################
Dt3 <- 1/sqrt(Dt)
qqnorm(Dt3)
qqline(Dt3)
plot(St,Dt3)
m4 <- lm(Dt3~St)
summary(m4)
#####################
m5 <- lm(Dt3~ St + I(St^2))
summary(m5)
#####################
Dt4 <- 1/log(Dt)
plot(St,Dt4)
m6 <- lm(Dt4~ St +I(St^2))
summary(m6)
plot(m6)
#######################################

