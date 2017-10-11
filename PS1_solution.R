setwd("folder location in quotation marks")

## Q1
amazon <- read.csv("amazon_compare.csv")

## Q2
sum(is.na(amazon$price))
sum(is.na(amazon$price_online))

amazon2 <- amazon[which(!is.na(amazon$price) & !is.na(amazon$price_online)),]
## Equivalently: amazon <- amazon[which(!(is.na(amazon$price) | is.na(amazon$price_online))),]

## Some descriptive statistics
hist(amazon2$price)
max(amazon2$price)
mean(amazon2$price)
hist(amazon2$price_online)
max(amazon2$price_online)
mean(amazon2$price_online)

amazon3 <- amazon2[which(amazon2$price <= quantile(amazon2$price, prob = 0.99) & amazon2$price_online <= quantile(amazon2$price_online, prob = 0.99)),]

## Q3
set.seed(20171001)
amazon3$id_rand <- rnorm(nrow(amazon3), mean = 0, sd = 1)
amazon4 <- amazon3[order(amazon3$id_rand, decreasing = FALSE),]
amazon4 <- amazon4[1:1000,]

## Q4
mean(amazon3$price)
sd(amazon3$price)
sd(amazon3$price)^2
quantile(amazon3$price, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(amazon3$price, probs = seq(from = 0, to = 1, by = 0.1))

mean(amazon3$price_online)
sd(amazon3$price_online)
sd(amazon3$price_online)^2
quantile(amazon3$price_online, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(amazon3$price_online, probs = seq(from = 0, to = 1, by = 0.1))

## Q5
var_price <- sum((amazon3$price - mean(amazon3$price))^2) / (nrow(amazon3))
# var_price <- sum((amazon3$price - mean(amazon3$price))^2) / (nrow(amazon3) - 1) % matches the built-in function and this is an unbiased estimate for the variance
sd_price <- var_price^(1/2)

var_price_online <- sum((amazon3$price_online - mean(amazon3$price_online))^2) / nrow(amazon3)
# var_price_online <- sum((amazon3$price_online - mean(amazon3$price_online))^2) / (nrow(amazon3) - 1)
sd_price_online <- var_price_online^(1/2)

## Q6
amazon3$indic <- as.integer(amazon3$price > amazon3$price_online)

## Q7
amazon3$diff_price <- amazon3$price - amazon3$price_online
amazon3$diff_indic <- as.integer(amazon3$diff_price > 0)

amazon3$diff <- !(amazon3$diff_indic == amazon3$indic)

sum(amazon3$diff) # equals 0, therefore the two indicator variables are exactly the same

## Q8
sum(amazon3$diff_price > 0) / nrow(amazon3)

sum(amazon3$diff_price == 0) / nrow(amazon3)

sum(amazon3$diff_price < 0) / nrow(amazon3)

sum(amazon3$diff_price > 0) / nrow(amazon3) + sum(amazon3$diff_price == 0) / nrow(amazon3) + sum(amazon3$diff_price < 0) / nrow(amazon3)

## Q9
sum(amazon3$diff_price > 0 & amazon3$category == "Electronics") / nrow(amazon3[which(amazon3$category == "Electronics"),])
# cat <- unique(amazon3$category)
# sum(amazon3$diff_price > 0 & amazon3$category == cat[1]) / nrow(amazon3[which(amazon3$category == cat[1]),])
# sum(amazon3$diff_price <= 0 & amazon3$category == cat[1]) / nrow(amazon3[which(amazon3$category == cat[1]),])
# sum(amazon3$diff_price > 0 & amazon3$category == cat[2]) / nrow(amazon3[which(amazon3$category == cat[2]),])
# sum(amazon3$diff_price <= 0 & amazon3$category == cat[2]) / nrow(amazon3[which(amazon3$category == cat[2]),])
# sum(amazon3$diff_price > 0 & amazon3$category == cat[3]) / nrow(amazon3[which(amazon3$category == cat[3]),])
# sum(amazon3$diff_price <= 0 & amazon3$category == cat[3]) / nrow(amazon3[which(amazon3$category == cat[3]),])
# sum(amazon3$diff_price > 0 & amazon3$category == cat[4]) / nrow(amazon3[which(amazon3$category == cat[4]),])
# sum(amazon3$diff_price <= 0 & amazon3$category == cat[4]) / nrow(amazon3[which(amazon3$category == cat[4]),])
# sum(amazon3$diff_price > 0 & amazon3$category == cat[5]) / nrow(amazon3[which(amazon3$category == cat[5]),])
# sum(amazon3$diff_price <= 0 & amazon3$category == cat[5]) / nrow(amazon3[which(amazon3$category == cat[5]),])

sum(amazon3$diff_price == 0 & amazon3$category == "Home and Appliances") / nrow(amazon3[which(amazon3$category == "Home and Appliances"),])

## Q10
plot(amazon3$price, amazon3$price_online)
plot(amazon3$price, amazon3$price_amazon)
cor(amazon3$price, amazon3$price_online)
cor(amazon3$price, amazon3$price_amazon)

# amazon5 <- amazon3[which(amazon3$price_amazon < quantile(amazon3$price_amazon, probs = 0.99)),]
# plot(amazon5$price, amazon5$price_online)
# plot(amazon5$price, amazon5$price_amazon)
# cor(amazon5$price, amazon5$price_online)
# cor(amazon5$price, amazon5$price_amazon)

## Q11
boxplot(amazon3$diff_price[which(amazon3$category == "Electronics")])
# sum(amazon4$category == "Electronics")
hist(amazon3$diff_price[which(amazon3$category == "Electronics")])

boxplot(amazon3$diff_price[which(amazon3$category == "Pharmacy and Health")])
# sum(amazon3$category == "Pharmacy and Health")
hist(amazon3$diff_price[which(amazon3$category == "Pharmacy and Health")])
