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

retailer_prc_avg <- amazon4[, .(retailer_prc_avg = mean(price_online, na.rm = TRUE)), by = retailer_id]

## test comment


