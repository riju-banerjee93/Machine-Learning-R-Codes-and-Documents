attach(Salary_Data)
summary(Salary_Data)
sd(ye)
sd(sl)
hist(ye)
hist(sl)
boxplot(ye)
boxplot(sl)
qqnorm(ye)
qqline(ye)
qqnorm(sl)
qqline(sl)
#sl is not normal, so we transform
plot(ye,sl)
cor(ye,sl)
##############################
m1 <- lm(sl~ ye)
summary(m1)
plot(m1)
#################################

