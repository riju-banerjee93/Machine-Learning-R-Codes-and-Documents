attach(crime_data)
mdata <- crime_data[,2:ncol(crime_data)]
norm_crime <- scale(crime_data[,2:ncol(crime_data)])
d <- dist(norm_crime, method = "euclidean")
fit <- hclust(d,method = "complete")
plot(fit, hang = -1)
groups <- cutree(fit, k = 5)
rect.hclust(fit, k = 5, border = "red")
table(groups)
membership <- as.matrix(groups)
my_data <- data.frame(crime_data,membership)
my_data1 <- my_data[,c(ncol(my_data),1:ncol(my_data)-1)]
View(my_data1)
aggregate(mdata, by=list(membership), FUN=mean)

