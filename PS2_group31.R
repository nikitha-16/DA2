rm(list=ls())
amazon <- read.csv("amazon_compare.csv")

str(amazon)
summary(amazon)


#price correlation

amazon2 <- amazon[which(!is.na(amazon$price) & !is.na(amazon$price_online)),]
summary(amazon2)

install.packages("GGally")
library(GGally)

priceset <- data.frame(amazon2$price, amazon2$price_online, amazon2$price_amazon)
priceset2 <- priceset[which((priceset$amazon2.price < 2000) & (priceset$amazon2.price_amazon < 2000)),]
summary(priceset2)

ggpairs(priceset2)

#missing observations in the original dataset

summary(amazon)
str(amazon)

#missing values by category like retailer
library(data.table)
amazon3 <- data.table(amazon) 
table(is.na(amazon3$price_online), amazon3$category)
table(is.na(amazon3$price_online), amazon3$retailer_id)

median <- median(amazon3$price_amazon)

amazon4 <- amazon3[, Amz_pricecat := cut(price_amazon, c(0,
                                                     median,
                                                     Inf),
                         labels = c('low', 'high')),]

table(is.na(amazon4$price_online), amazon4$Amz_pricecat)

#replace missing values in price_online with its mean

mean <- mean(amazon4$price_online, na.rm = TRUE)
amazon4$price_online[which(is.na(amazon4$price_online))] <- mean

summary(amazon4$price_online)

#replace missing values in price_online with category means like retailer or product
#first i go one step back and create amazon4 one more time. Because we need the version with NAs:
amazon4 <- amazon3[, Amz_pricecat := cut(price_amazon, c(0,
                                                         median,
                                                         Inf),
                                         labels = c('low', 'high')),]

#Now we can do the replacement

str(which(is.na(amazon4$price_online)))
amazon4$price_online[which(is.na(amazon4$price_online))]

amazon4[, sum(is.na(price_online)), by = retailer_id]

amazon4$price_online <- amazon4[, ifelse(is.na(price_online), 
                 mean(price_online, na.rm = TRUE), 
                 price_online),
                 by = retailer_id][,2]

summary(amazon4$price_online)
amazon4[, mean(price_online), by = retailer_id]

#  a variable that is the difference between price and online price

amazon4$diff_price <- amazon4$price - amazon4$price_online
summary((amazon4$diff_price))

library(GGally)
ggpairs(amazon4, columns = 4:6, title = 'with extreme values')

# drop 95th percentile of it
amazon5 <- amazon4[which(amazon4$diff_price <= quantile(amazon2$price, prob = 0.95))]
summary((amazon5$diff_price))

ggpairs(amazon5, columns = 4:6, title = 'after dropping 95th percentile')

