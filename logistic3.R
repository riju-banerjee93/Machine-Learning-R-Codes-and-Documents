

attach(election_data)

m1 <- glm(Result~ Year+ Amount.Spent+ Popularity.Rank,family = binomial("logit"), maxit = 100, data = election_data)
summary(m1)

exp(coef(m1))

#confusion matrix

prob <- predict(m1,type=c("response"),election_data)
prob
confusion<-table(prob>0.5,election_data$Result)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy


# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,election_data$Result)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
