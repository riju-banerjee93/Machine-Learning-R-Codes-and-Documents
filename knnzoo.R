animals <- read.csv(file.choose(), stringsAsFactors = F)
str(animals)
#animals <-data.frame(lapply(animals[,2:ncol(animals)],as.factor))
animals$type <- factor(animals$type)
str(animals)
table(animals$type)
round(prop.table(table(animals$type))*100,1)

animals1_2 <- animals[c(which(animals$type == 1),which(animals$type == 2)),]
animals1_3 <- animals[c(which(animals$type == 1),which(animals$type == 3)),]
animals1_4 <- animals[c(which(animals$type == 1),which(animals$type == 4)),]
animals1_5 <- animals[c(which(animals$type == 1),which(animals$type == 5)),]
animals1_6 <- animals[c(which(animals$type == 1),which(animals$type == 6)),]
animals1_7 <- animals[c(which(animals$type == 1),which(animals$type == 7)),]


##below is another way of doing it
#glas23 <- subset(glass, glass$Type == 2,select = c(RI:Type))
#View(glas23)
#str(glas23)


#View(glass2_3)

#install.packages("ROSE")
library("ROSE")

animals1_2over <- as.data.frame(ovun.sample(type~.,data = animals1_2,method = "both", N = 82)$data)
animals1_3over <- as.data.frame(ovun.sample(type~.,data = animals1_3,method = "both", N = 82)$data)
animals1_4over <- as.data.frame(ovun.sample(type~.,data = animals1_4,method = "both", N = 82)$data)
animals1_5over <- as.data.frame(ovun.sample(type~.,data = animals1_5,method = "both", N = 82)$data)
animals1_6over <- as.data.frame(ovun.sample(type~.,data = animals1_6,method = "both", N = 82)$data)
animals1_7over <- as.data.frame(ovun.sample(type~.,data = animals1_7,method = "both", N = 82)$data)



table(animals1_2over$type)
table(animals1_3over$type)
table(animals1_4over$type)
table(animals1_5over$type)
table(animals1_6over$type)
table(animals1_7over$type)



animals1 <- animals[which(animals$type == 1),]
animals2 <- animals1_2over[which(animals1_2over$type == 2),]
animals3 <- animals1_3over[which(animals1_3over$type == 3),]
animals4 <- animals1_4over[which(animals1_4over$type == 4),]
animals5 <- animals1_5over[which(animals1_5over$type == 5),]
animals6 <- animals1_6over[which(animals1_6over$type == 6),]
animals7 <- animals1_7over[which(animals1_7over$type == 7),]

animals <- rbind(animals1,animals2)
animals <- rbind(animals,animals3)
animals <- rbind(animals,animals4)
animals <- rbind(animals,animals5)
animals <- rbind(animals,animals6)
animals <- rbind(animals,animals7)


table(animals$type)





















normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normal(c(1,2,3,4,5))
normal(c(10,20,30,40,50))

animals_n <- as.data.frame(lapply(animals[2:(ncol(animals)-1)], normal))
str(animals_n)
#View(glass_n)
?sample

  n <- nrow(animals_n)
  n1 <-floor(n*0.70)
  train <- sample(1:n,n1)
  
  
  #View(glass_n[train,])
  #View(glass_n[-train,])
  
  animals_train <- as.data.frame(animals_n[train,])
  animals_test <- as.data.frame(animals_n[-train,])
  
  animals_label_train <- as.data.frame(animals[train,ncol(animals)])
  animals_label_test <- as.data.frame(animals[-train,ncol(animals)])
  
round(prop.table(table(animals_label_train[,1]))*100,0)
round(prop.table(table(animals_label_test[,1]))*100,0)
  
  
  
  
  



library("class")
?knn
pred_model1 <- knn(train = animals_train, test = animals_test, cl = animals_label_train[,1], k=1)

#install.packages("gmodels")
library("gmodels")
?class
# Create cross table of predicted and actual
CrossTable( x =  animals_label_test[,1], y = pred_model1)
mean(pred_model1==animals_label_test[,1])
