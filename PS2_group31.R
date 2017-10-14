rm(list=ls())
library(data.table)

## 1) Use the amazon_compare.csv file and load it into Stata or R.
amazon <- read.csv("amazon_compare.csv")

## ==================================================================================================

## 2) Inspect your data. How many string variables are included in the dataset? For each non-string
## variable state if it is continuous or discrete. You can get also points for stating the type of 5-6
## variables included in the dataset. (Not mandatory!)
str(amazon)
summary(amazon)

## please find our dteailed answer in the pdf sheet attached

## ==================================================================================================

## 3) Check the pairwise correlations among the price variables in the dataset. What do expect
## from these correlations? Should they be higher or lower than what they actually are?

install.packages("GGally")
library(GGally)

## Checking the correlation between price and price_amazon
priceset <- data.frame(amazon$price, amazon$price_amazon)
priceset2 <- priceset[which((priceset$amazon.price < 2000) & (priceset$amazon.price_amazon < 2000)),]
## we droppped observations with prices higher than 2000
## now comparing price with price_amazon
summary(priceset2)
ggpairs(priceset2)

## dropping the 1310 NAs from the data set
amazon2 <- amazon[which(!is.na(amazon$price) & !is.na(amazon$price_online)),]
summary(amazon2) # no we have only 2682 observations

## Now comparing the three different price variable
priceset <- data.frame(amazon2$price, amazon2$price_online, amazon2$price_amazon)
priceset2 <- priceset[which((priceset$amazon2.price < 2000) & (priceset$amazon2.price_amazon < 2000)),]
## we droppped observations with prices higher than 2000
## let's see the plot of the comparison
summary(priceset2)
ggpairs(priceset2)

## please find our detailed answer in the pdf sheet attached

## ==================================================================================================

## 4) Which variables in the dataset have more missing observations? Can you think of a reason
## why this is the case? (Not mandatory!)  

## Checking missing observations in the original dataset
summary(amazon)
str(amazon)

## cases where the value of product_online is missing
amazon[amazon$product_amazon == "AmazonBasics Apple Certified Lightning to USB Cable - 6 Feet (1.8 Meters) - White", .N]
amazon[amazon$product_amazon == "AmazonBasics Apple Certified Lightning to USB Cable - 6 Feet (1.8 Meters) - White",8]
amazon[amazon$product_amazon == "Instant Pot IP-DUO60 7-in-1 Multi-Functional Pressure Cooker, 6Qt/1000W",.N]
amazon[amazon$product_amazon == "Instant Pot IP-DUO60 7-in-1 Multi-Functional Pressure Cooker, 6Qt/1000W",8]
## cases where the value of merchant is missing
amazon[amazon$merchant == "",.N]

## please find our detailed answer in the pdf sheet attached

## ==================================================================================================

## 5) What is the fraction of missing observations in the online price variable? Are these missing
## observations more likely to occur in a particular good category, retailer or any other variable
## included in the dataset? Hint: you might want to create a categorical variable taking value one
## if the price (or amazon price) is high and zero otherwise. You can define a high price as the
## price being above the mean, median or 80th percentile and check if online price is more likely
## to have missing values if the price (or amazon price) is high. Another way to might be to create
## a discrete variable taking values 1-4 for each quartile of the price (or amazon price) variable.

## missing values by category like retailer
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


