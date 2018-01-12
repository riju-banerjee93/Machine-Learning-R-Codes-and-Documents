library(arules)
library(arulesViz)
attach(my_movies)
my_movies1 <- my_movies[,1:5]
attach(my_movies1)
summary(my_movies1)
my_movies1[] <- lapply(my_movies1,as.factor)



rules <- apriori(my_movies1, parameter = list(support = 0.2, confidence = 0.5,minlen = 2))
rules
inspect(head(sort(rules, by = "lift")))
plot(rules)
head(quality(sort(rules, by = "lift")))
plot(rules, method = "grouped")

