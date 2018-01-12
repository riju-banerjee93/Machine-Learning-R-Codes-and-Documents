attach(EastWestAirlines)
my_data <- EastWestAirlines[,3:ncol(EastWestAirlines)-1]
#View(my_data)
normdata <- scale(my_data)

wss <- (nrow(normdata)-1)*sum(apply(normdata,2,var))
for(i in 2:15)wss[i]<- sum(fit=kmeans(normdata,centers=i)$withinss)
plot(1:15,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")






#from the scree plot we take k = 7
fit <- kmeans(normdata,7)
final1 <- data.frame(my_data,fit$cluster)
final1
final2 <- final1[, c(ncol(final1),1:ncol(final1)-1)]
View(final2)
aggregate(my_data, by=list(fit$cluster), FUN=mean)

