glass <- read.csv(file.choose(), stringsAsFactors = F)
attach(glass)

str(glass)
glass$Type <- factor(glass$Type)
str(glass)
table(glass$Type)


#we will take type which has max(inthis case, 2) and least(3) and make another dataset for oversampling
glass2_3 <-glass[c(which(glass$Type == 2),which(glass$Type == 3)),]
glass2_5 <- glass[c(which(glass$Type == 2),which(glass$Type == 5)),]
glass2_6 <- glass[c(which(glass$Type == 2),which(glass$Type == 6)),]
glass2_7 <- glass[c(which(glass$Type == 2),which(glass$Type == 7)),]





#install.packages("ROSE")
library("ROSE")

glass2_3over <- as.data.frame(ovun.sample(Type~., data = glass2_3, method = "over",N = 152)$data)
glass2_5over <- as.data.frame(ovun.sample(Type~., data = glass2_5, method = "over",N = 152)$data)
glass2_6over <- as.data.frame(ovun.sample(Type~., data = glass2_6, method = "over",N = 152)$data)
glass2_7over <- as.data.frame(ovun.sample(Type~., data = glass2_7, method = "over",N = 152)$data)

table(glass2_3over$Type)

glass1 <- (glass[which(glass$Type == 1),])
glass2 <- (glass[which(glass$Type == 2),])
glass3 <- glass2_3over[which(glass2_3over$Type == 3),]
glass5 <- glass2_5over[which(glass2_5over$Type == 5),]
glass6 <- glass2_6over[which(glass2_6over$Type == 6),]
glass7 <- glass2_7over[which(glass2_7over$Type == 7),]

glass <- rbind( glass1 , glass2)
glass <- rbind(glass,glass3)
glass <- rbind(glass,glass5)
glass <- rbind(glass,glass6)
glass <- rbind(glass,glass7)
table(glass$Type)
#end of pre processing
#######################################################################################




round(prop.table(table(glass$Type))*100,1)


normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normal(c(1,2,3,4,5))
normal(c(10,20,30,40,50))

glass_n <- as.data.frame(lapply(glass[1:9], normal))
str(glass_n)
#View(glass_n)
?sample

n <- nrow(glass_n)
n1 <-floor(n*0.70)
train <- sample(1:n,n1)

#View(glass_n[train,])
#View(glass_n[-train,])


glass_train <- as.data.frame(glass_n[train,])
glass_test <- as.data.frame(glass_n[-train,])

glass_label_train <- as.data.frame(glass[train,ncol(glass)])
glass_label_test <- as.data.frame(glass[-train,ncol(glass)])

table(glass_label_train)
table(glass_label_test)






round(prop.table(table(glass_label_train[,1]))*100,1)
round(prop.table(table(glass_label_test[,1]))*100,1)
library("class")
?knn
pred_model1 <- knn(train = glass_train, test = glass_test, cl = glass_label_train[,1], k=1)

#install.packages("gmodels")
library("gmodels")
?class
# Create cross table of predicted and actual
CrossTable( x =  glass_label_test[,1], y = pred_model1)
mean(pred_model1==glass_label_test[,1])
#################################################################################################################3

glass_s <- as.data.frame(scale(glass[1:9]))
#View(glass_s)


glass_train <- as.data.frame(glass_s[train,])
glass_test <- as.data.frame(glass_s[-train,])
#View(glass_test)

glass_label_train <- as.data.frame(glass[train,ncol(glass)])
glass_label_test <- as.data.frame(glass[-train,ncol(glass)])

round(prop.table(table(glass_label_train[,1]))*100,1)
round(prop.table(table(glass_label_test[,1]))*100,1)


#View(glass_label_test)


pred_model2 <- knn(train = glass_train, test = glass_test, cl = glass_label_train[,1], k=1)


# Create cross table of predicted and actual
CrossTable( x =  glass_label_test[,1], y = pred_model2)
mean(pred_model2==glass_label_test[,1])
##################################################################################################


