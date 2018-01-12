library(arules)
library(arulesViz)
attach(groceries)
summary(groceries)
groceries[] <- lapply(groceries,as.factor)

 

rules <- apriori(groceries, parameter = list(support = 0.003, confidence = 0.6,minlen = 2))
rules
inspect(head(sort(rules, by = "lift")))
plot(rules)
head(quality(sort(rules, by = "lift")))
plot(rules, method = "grouped")
