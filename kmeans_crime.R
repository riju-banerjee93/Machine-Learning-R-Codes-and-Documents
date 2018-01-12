attach(crime_data)
mdata <- crime_data[,2:ncol(crime_data)]
norm_k_data <- scale(mdata)

wss <- (nrow(norm_k_data)-1)*sum(apply(norm_k_data,2,var))
for(i in 2:10)wss[i]<- sum(fit=kmeans(norm_k_data,centers=i)$withinss)
plot(1:10,wss,type="b",main="10 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")

fit <- kmeans(norm_k_data,4)
fin <- data.frame(crime_data,fit$cluster)
fin
final2 <- fin[, c(ncol(fin),1:ncol(fin)-1)]
View(final2)
aggregate(mdata, by=list(fit$cluster), FUN=mean)
