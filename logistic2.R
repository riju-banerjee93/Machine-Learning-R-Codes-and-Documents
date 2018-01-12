bank_data <- na.omit(bank_data)
attach(bank_data)

summary(bank_data)

m <- glm(y~., family = "binomial", data = bank_data)
summary(m)
#we found out from the summary table that column 13,16,19,and 31 have value NA.
#so i have removed those columns from the dataset to make the other models

m1 <- glm(y~., family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m1)
#vif(m1)

prob <- predict(m1,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

#m2 <- glm(y~ balance+ housing+ loan+ duration+ campaign+poutfailure+ poutother+poutsuccess+ con_cellular+con_telephone+divorced+married+joretired+jostudent, family = "binomial", data = bank_data)
#summary(m2)
#vif(m2)


m3 <- glm(y~ age+factor(default)+balance+factor(housing)+factorloan+duration+campaign+pdays+previous+poutfailure+poutother+poutsuccess+con_cellular+con_telephone+divorced+married+joadmin.+joblue.collar+joentrepreneur+johousemaid+joretired+joself.employed+joservices+jostudent+jotechnician+jounemployed, family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m3)
vif(m3)

prob <- predict(m3,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

m4 <- glm(y~ default+balance+housing+loan+duration+campaign+pdays+previous+poutfailure+poutother+poutsuccess+con_cellular+con_telephone+divorced+married+joadmin.+joblue.collar+joentrepreneur+johousemaid+joretired+joself.employed+joservices+jostudent+jotechnician+jounemployed, family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m4)

prob <- predict(m4,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

m5 <-  glm(y~ default+balance+housing+loan+duration+campaign+previous+poutfailure+poutother+poutsuccess+con_cellular+con_telephone+divorced+married+joadmin.+joblue.collar+joentrepreneur+johousemaid+joretired+joself.employed+joservices+jostudent+jotechnician+jounemployed, family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m5)

prob <- predict(m5,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

m6 <-glm(y~ default+balance+housing+loan+duration+campaign+previous+poutfailure+poutother+poutsuccess+con_cellular+con_telephone+divorced+married+joblue.collar+joentrepreneur+johousemaid+joretired+joself.employed+joservices+jostudent+jotechnician+jounemployed, family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m6)

prob <- predict(m6,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

m7 <-glm(y~ balance+housing+loan+duration+campaign+previous+poutfailure+poutother+poutsuccess+con_cellular+con_telephone+divorced+married+joblue.collar+joentrepreneur+johousemaid+joretired+joself.employed+joservices+jostudent+jotechnician+jounemployed, family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m7)

exp(coef(m7))

#confusion matrix

prob <- predict(m7,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

m8 <-glm(y~ balance+housing+loan+duration+campaign+poutfailure+poutother+poutsuccess+con_cellular+con_telephone+divorced+married+joblue.collar+joentrepreneur+johousemaid+joretired+joself.employed+joservices+jostudent+jotechnician+jounemployed, family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m8)

prob <- predict(m8,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

m9 <-glm(y~ balance+housing+loan+duration+campaign+poutfailure+poutother+poutsuccess+con_cellular+con_telephone+divorced+married+joblue.collar+joentrepreneur+johousemaid+joretired+joself.employed+joservices+jostudent+jotechnician, family = "binomial", data = bank_data[,-c(13,16,19,31)])
summary(m9)

prob <- predict(m9,type=c("response"),bank_data[,-c(13,16,19,31)])
prob
confusion<-table(prob>0.5,bank_data[,-c(13,16,19,31)]$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,bank_data[,-c(13,16,19,31)]$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))