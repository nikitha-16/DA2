amazon <- read.csv("amazon_compare.csv")

str(amazon)
summary(amazon)

amazon2 <- amazon[which(!is.na(amazon$price) & !is.na(amazon$price_online)),]
summary(amazon2)


install.packages("GGally")
library(GGally)
priceset <- data.frame(amazon2$price, amazon2$price_online, amazon2$price_amazon)
priceset2 <- priceset[which((priceset$amazon2.price < 2000) & (priceset$amazon2.price_amazon < 2000)),]
summary(priceset2)

ggpairs(priceset)
