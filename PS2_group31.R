rm(list=ls())
library(data.table)
install.packages("GGally")
library(GGally)

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

## Checking the correlation between price and price_amazon
priceset <- data.frame(amazon$price, amazon$price_amazon)

## We could drop the extreme values to see the difference. A possible way to do it is the following:
boxplot(amazon$price, amazon$price_amazon)
priceset2 <- priceset[which((priceset$amazon.price < 3000) & (priceset$amazon.price_amazon < 3000)),]

## now comparing price with price_amazon
summary(priceset)
ggpairs(priceset, title = "Correlation between price and price_amazon created by ggpairs")
ggpairs(priceset2, title = "Correlation between price and price_amazon created by ggpairs")

## dropping the 1310 NAs from the original data set
amazon2 <- amazon[which(!is.na(amazon$price) & !is.na(amazon$price_online)),]
summary(amazon2) # no we have only 2682 observations

## Now comparing the three different price variable
priceset3 <- data.frame(amazon2$price, amazon2$price_online, amazon2$price_amazon)

## let's see the plot of the comparison
summary(priceset3)
ggpairs(priceset3, title = "Correlation among price, price_online and price_amazon")

priceset4 <- priceset3[which((priceset3$amazon2.price < 3000) & (priceset3$amazon2.price_amazon < 3000)),]
summary(priceset4)
ggpairs(priceset4, title = "Correlation among price, price_online and price_amazon")

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

## missing values by category and retailer
amazon3 <- data.table(amazon) 
table(is.na(amazon3$price_online), amazon3$category)
table(is.na(amazon3$price_online), amazon3$retailer_id)

median <- median(amazon3$price_amazon)

amazon4 <- amazon3[, Amz_pricecat := cut(price_amazon, c(0,
                                                     median,
                                                     Inf),
                         labels = c('low', 'high')),]

table(is.na(amazon4$price_online), amazon4$Amz_pricecat)

## please find our detailed answer in the pdf sheet attached

## ==================================================================================================

## 6) Replace missing observations in the online price variable with its mean or mode. Does it
## make more sense to replace them with the mean/mode or should we replace them by using the
## mean/mode of online price levels for various retailer type or good category? (Not mandatory!)

## replacing missing values in price_online with its mean

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

## please find our detailed answer in the pdf sheet attached

## ==================================================================================================

## 7) Generate a variable that is the difference between price and online price, call this variable
## diff_price and drop the observations that are larger than the 95 percentile.

## creating diff_preice
amazon4$diff_price <- amazon4$price - amazon4$price_online
summary((amazon4$diff_price))

# dropping the observations that are larger than the 95th percentile
amazon5 <- amazon4[which(amazon4$diff_price <= quantile(amazon2$price, prob = 0.95))]
summary((amazon5$diff_price))

## ==================================================================================================

## 8) Check the pairwise correlations among the price variables in the dataset. How do they change
## when you drop these extreme values?

ggpairs(amazon4, columns = 4:6, title = 'Correlation with extreme values')
ggpairs(amazon5, columns = 4:6, title = 'Correlation after dropping extreme values')
str(amazon4)

## ==================================================================================================

## 9) Make summary statistics (mean, standard deviations, quartiles, percentiles) of the diff_price.

## Summary of diff_price before dropping extreme values
summary(amazon4$diff_price)
sd(amazon4$diff)

## Summary of diff_price after dropping extreme values
## a simpler way to do it
summary(amazon5$diff_price)
sd(amazon5$diff)

## a more customized way to do it
data_check_q9<-data.frame(avg=double(),sd=double(),min=double(),q1=double(),q2=double(),q3=double(),max=double())
data_check_q9[1,1]<-amazon5[,mean(diff_price,na.rm=TRUE)]
data_check_q9[1,2]<-amazon5[,sd(diff_price,na.rm=TRUE)]
data_check_q9[1,3]<-amazon5[,quantile(diff_price,c(0),na.rm=TRUE)]
data_check_q9[1,4]<-amazon5[,quantile(diff_price,c(0.25),na.rm=TRUE)]
data_check_q9[1,5]<-amazon5[,quantile(diff_price,c(0.5),na.rm=TRUE)]
data_check_q9[1,6]<-amazon5[,quantile(diff_price,c(0.75),na.rm=TRUE)]
data_check_q9[1,7]<-amazon5[,quantile(diff_price,c(1),na.rm=TRUE)]
data_check_q9

## ==================================================================================================

## 10) Create a random number generator from a uniform distribution, with parameters of your own
## choice setting the seed equal to your group number. Call this variable id_rand.

#If you want to generate a decimal number where any value (including fractional values) between
## the stated minimum and maximum is equally likely, we need to use the runif function. This
## function generates values from the Uniform distribution.

set.seed(31)
## creating a random number generator that generates 3970 rounded numbers between 1 and 3970. The
## reason for 3970 is that after dropping the extreme values we have 3970 observations.
round(runif(nrow(amazon5), 1, nrow(amazon5)))
## If we wanted to generate random numbers without replacement, we need to use the sample function.
## It generates also 3970 rounded numbers between 1 and 3970, and one number can only occur
## once among the numbers, let's call it id_rand.
amazon5$id_rand <- sample(1:nrow(amazon5), nrow(amazon5), replace = F)
str(amazon5)

## ==================================================================================================

## 11) Sort the variable and select the first 100 observations in your dataset. Make summary
## statistics (mean, standard deviations, quartiles, percentiles) of the diff_price.
## How do these statistics compare with those in 9?

amazon6 <- amazon5[order(id_rand)]

## a simpler way to do it
summary(amazon6[1:100,diff_price])
sd(amazon6[1:100,diff_price])

## a more customized way to do it
data_check_q11<-data.frame(avg=double(),sd=double(),min=double(),q1=double(),q2=double(),q3=double(),max=double())
data_check_q11[1,1]<-amazon6[1:100,mean(diff_price,na.rm=TRUE)]
data_check_q11[1,2]<-amazon6[1:100,sd(diff_price,na.rm=TRUE)]
data_check_q11[1,3]<-amazon6[1:100,quantile(diff_price,c(0),na.rm=TRUE)]
data_check_q11[1,4]<-amazon6[1:100,quantile(diff_price,c(0.25),na.rm=TRUE)]
data_check_q11[1,5]<-amazon6[1:100,quantile(diff_price,c(0.5),na.rm=TRUE)]
data_check_q11[1,6]<-amazon6[1:100,quantile(diff_price,c(0.75),na.rm=TRUE)]
data_check_q11[1,7]<-amazon6[1:100,quantile(diff_price,c(1),na.rm=TRUE)]
data_check_q11

## Please find the values in the pdf attached

## ==================================================================================================

## 12) Use the same procedure as in point 10 and select the last 200 observations in your dataset.
## Make summary statistics (mean, standard deviations, quartiles, percentiles) of the diff_price.
## How do these statistics compare with those in 9 and 11?

## a simpler way to do it
summary(amazon6[(nrow(amazon6)-199):nrow(amazon6),diff_price])
sd(amazon6[(nrow(amazon6)-199):nrow(amazon6),diff_price])

## a more customized way to do it
data_check_q12<-data.frame(avg=double(),sd=double(),min=double(),q1=double(),q2=double(),q3=double(),max=double())
data_check_q12[1,1]<-amazon6[3771:3970,mean(diff_price,na.rm=TRUE)]
data_check_q12[1,2]<-amazon6[3771:3970,sd(diff_price,na.rm=TRUE)]
data_check_q12[1,3]<-amazon6[3771:3970,quantile(diff_price,c(0),na.rm=TRUE)]
data_check_q12[1,4]<-amazon6[3771:3970,quantile(diff_price,c(0.25),na.rm=TRUE)]
data_check_q12[1,5]<-amazon6[3771:3970,quantile(diff_price,c(0.5),na.rm=TRUE)]
data_check_q12[1,6]<-amazon6[3771:3970,quantile(diff_price,c(0.75),na.rm=TRUE)]
data_check_q12[1,7]<-amazon6[3771:3970,quantile(diff_price,c(1),na.rm=TRUE)]
data_check_q12

## Please find the values in the pdf attached

## ==================================================================================================

## 13) Use the same procedure as in point 10 and select the first 1000 observations in your dataset.
## Make summary statistics (mean, standard deviations, quartiles, percentiles) of the variable
## diff_price.How do these statistics compare with those in 9, 11, and 12? (Not mandatory!)

summary(amazon6[1:1000,diff_price])
sd(amazon6[1:1000,diff_price])

## a more customized way to do it
data_check_q13<-data.frame(avg=double(),sd=double(),min=double(),q1=double(),q2=double(),q3=double(),max=double())
data_check_q13[1,1]<-amazon6[1:1000,mean(diff_price,na.rm=TRUE)]
data_check_q13[1,2]<-amazon6[1:1000,sd(diff_price,na.rm=TRUE)]
data_check_q13[1,3]<-amazon6[1:1000,quantile(diff_price,c(0),na.rm=TRUE)]
data_check_q13[1,4]<-amazon6[1:1000,quantile(diff_price,c(0.25),na.rm=TRUE)]
data_check_q13[1,5]<-amazon6[1:1000,quantile(diff_price,c(0.5),na.rm=TRUE)]
data_check_q13[1,6]<-amazon6[1:1000,quantile(diff_price,c(0.75),na.rm=TRUE)]
data_check_q13[1,7]<-amazon6[1:1000,quantile(diff_price,c(1),na.rm=TRUE)]
data_check_q13

## Please find the values in the pdf attached

## ==================================================================================================

## 14) Use the same procedure as in point 10 and select the last 1000 observations in your dataset.
## Make summary statistics (mean, standard deviations, quartiles, percentiles) of the diff_price.
## How do these statistics compare with those in 9, 11, 12, and 13. (Not mandatory!)

summary(amazon6[(nrow(amazon6)-999):nrow(amazon6),diff_price])
sd(amazon6[(nrow(amazon6)-999):nrow(amazon6),diff_price])

## a more customized way to do it
data_check_q14<-data.frame(avg=double(),sd=double(),min=double(),q1=double(),q2=double(),q3=double(),max=double())
data_check_q14[1,1]<-amazon6[2971:3970,mean(diff_price,na.rm=TRUE)]
data_check_q14[1,2]<-amazon6[2971:3970,sd(diff_price,na.rm=TRUE)]
data_check_q14[1,3]<-amazon6[2971:3970,quantile(diff_price,c(0),na.rm=TRUE)]
data_check_q14[1,4]<-amazon6[2971:3970,quantile(diff_price,c(0.25),na.rm=TRUE)]
data_check_q14[1,5]<-amazon6[2971:3970,quantile(diff_price,c(0.5),na.rm=TRUE)]
data_check_q14[1,6]<-amazon6[2971:3970,quantile(diff_price,c(0.75),na.rm=TRUE)]
data_check_q14[1,7]<-amazon6[2971:3970,quantile(diff_price,c(1),na.rm=TRUE)]
data_check_q14

## Please find the values in the pdf attached

## ==================================================================================================

## 15) From the statistics of the different random samples that you selected which are those that are
## more similar to the statistics when considering the full dataset? Why do you think this is the
## case?

## ==================================================================================================

## 16) Use the original dataset and repeat 9-14 (If you have done point 6, replace the missing
## observations as in 6). How do your statistics change when you include the extreme values
## (outliers) and when you exclude them? In light of the above evidence, what are your final
## conclusions? Should we keep extreme values or should we drop them from the dataset? (Not
## mandatory for part-time students!)

## ==================================================================================================