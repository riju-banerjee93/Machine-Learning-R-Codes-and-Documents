library(arules)
library(arulesViz)
attach(transactions_retail1)
summary(transactions_retail1)
transactions_retail1[] <- lapply(transactions_retail1,as.factor)



rules <- apriori(transactions_retail1, parameter = list(support = 0.004, confidence = 0.5,minlen = 3))
rules
inspect(head(sort(rules, by = "lift")))
plot(rules)
head(quality(sort(rules, by = "lift")))
plot(rules, method = "grouped")

