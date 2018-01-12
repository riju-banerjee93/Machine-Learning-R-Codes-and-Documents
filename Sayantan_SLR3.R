attach(emp_data)
summary(emp_data)
#
sd(Salary_hike)
sd(Churn_out_rate)
#
hist(Salary_hike)
hist(Churn_out_rate)
#
boxplot(Salary_hike)
boxplot(Churn_out_rate)
#
qqnorm(Salary_hike)
qqline(Salary_hike)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)
#
cor(Salary_hike, Churn_out_rate)
plot(Salary_hike, Churn_out_rate)
#####################################
m1 <- lm(Churn_out_rate~ Salary_hike)
summary(m1)
plot(m1)
#############################
m2 <- lm(Churn_out_rate~ Salary_hike+ I(Salary_hike^2))
summary(m2)
plot(m2)

