attach(fishing)
require('mlogit')||install.packages('mlogit')
library(mlogit)
require('nnet') ||install.packages('nnet')
library(nnet)

table(fishing$mode)

fishing.mode <- multinom(mode~ price.beach+price.pier+ price.boat+price.charter+catch.beach+catch.pier+catch.boat+catch.charter+income)
summary(fishing.mode)
z <- summary(fishing.mode)$coefficients/summary(fishing.mode)$standard.errors
p_value <- (1-pnorm(abs(z),0,1))*2
summary(fishing.mode)$coefficients
p_value
