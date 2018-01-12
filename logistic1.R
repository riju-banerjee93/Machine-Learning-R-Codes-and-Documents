install.packages("AER")
data(Affairs, package = "AER")
#i converted affairs column (having affair = 1, not having affair = 0)
Affairs$binaryAffairs <- as.numeric(Affairs$affairs>0)
View(Affairs)
attach(Affairs)
#EDA
summary(Affairs)

hist(age)
hist(yearsmarried)

boxplot(age,main = "boxplot for age")
boxplot(yearsmarried, main = "boxplot for yearsMarried")



pairs(Affairs)




#
m1 <- glm(binaryAffairs~ factor(gender)+ age+ yearsmarried+ factor(children)+factor(religiousness)+ factor(education)+factor(occupation)+factor(rating), family = "binomial", data = Affairs)
summary(m1)
plot(m1)

vif(m1)


exp(coef(m1))

#confusion matrix
   
prob <- predict(m1,type=c("response"),Affairs)
prob
confusion<-table(prob>0.5,Affairs$binaryAffairs)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy


# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,Affairs$binaryAffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
###########################################################
#i found education to be most insignificant, so i removed it and made a different model
m2 <- glm(binaryAffairs~ factor(gender)+ age+ yearsmarried+ factor(children)+ factor(religiousness)+factor(occupation)+ factor(rating), family = "binomial", data = Affairs)
summary(m2)
#the AIC value decreased,thus i conclude that this model is better than previous one
exp(coef(m2))



prob1 <- predict(m2,type=c("response"),Affairs)
prob1
confusion1<-table(prob1>0.5,Affairs$binaryAffairs)
confusion1

Accuracy1<-sum(diag(confusion1)/sum(confusion1))
Accuracy1
#i could not find any significant increase in accuracy

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob1,Affairs$binaryAffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#################################################################
# i make another model removing occupation
m3 <- glm(binaryAffairs~ factor(gender)+ age+ yearsmarried+ factor(children)+ factor(religiousness)+ factor(rating), family = "binomial", data = Affairs)
summary(m3)
#The AIC value increased so we will make another model
exp(coef(m3))



prob2 <- predict(m3,type=c("response"),Affairs)
prob2
confusion2<-table(prob2>0.5,Affairs$binaryAffairs)
confusion2

Accuracy2<-sum(diag(confusion2)/sum(confusion2))
Accuracy2

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob2,Affairs$binaryAffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
##############################################################
#i removed children and made another model
m4 <- glm(binaryAffairs~ factor(gender)+ age+ yearsmarried+ factor(religiousness)+ factor(rating), family = "binomial", data = Affairs)
summary(m4)
# no considerable change in AIC value, so i will make a different model
exp(coef(m4))



prob3 <- predict(m4,type=c("response"),Affairs)
prob3
confusion3<-table(prob3>0.5,Affairs$binaryAffairs)
confusion3

Accuracy3<-sum(diag(confusion3)/sum(confusion3))
Accuracy3

#accuracy increased a bit
# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob3,Affairs$binaryAffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
##############################################################
#i removed gender and made a different model
m5 <- glm(binaryAffairs~  age+ yearsmarried+ factor(religiousness)+ factor(rating), family = "binomial", data = Affairs)
summary(m5)


exp(coef(m5))

#confusion matrix

prob4 <- predict(m5,type=c("response"),Affairs)
prob4
confusion4<-table(prob4>0.5,Affairs$binaryAffairs)
confusion4

Accuracy4<-sum(diag(confusion4)/sum(confusion4))
Accuracy4
#accuracy decreased
pairs(Affairs)

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob4,Affairs$binaryAffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))


#From all the models, i conclude that m1 model is most accurate



