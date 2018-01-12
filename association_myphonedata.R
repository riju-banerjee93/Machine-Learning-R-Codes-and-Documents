library(arules)
library(arulesViz)
my_data <- myphonedata[,1:3]
summary(my_data)
 

my_data[] <- lapply(my_data,as.factor)



rules <- apriori(my_data, parameter = list(support = 0.1, confidence = 0.5,minlen = 2))
rules
inspect(head(sort(rules, by = "lift")))
plot(rules)
head(quality(sort(rules, by = "lift")))
plot(rules, method = "grouped")
