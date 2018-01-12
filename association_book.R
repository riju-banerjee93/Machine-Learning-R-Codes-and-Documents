library(arules)
library(arulesViz)
attach(book)
View(book)
summary(book)
book[] <- lapply(book,as.factor)



rules <- apriori(book, parameter = list(support = 0.5, confidence = 0.7,minlen = 3))
rules
inspect(head(sort(rules, by = "lift")))
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
