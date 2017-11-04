#Problem Set 3
rm(list=ls())
# 1.	Use the amazon_compare.csv file and load it into R.
amazon <- read.csv("amazon_compare.csv")

# 2.	Do steps 2-3 exactly as you did in PS1. 
# The random number generator should be from the same distribution as in PS1. 
# What type of sampling is this?
sum(is.na(amazon$price))
sum(is.na(amazon$price_online))

amazon2 <- amazon[which(!is.na(amazon$price) & !is.na(amazon$price_online)),]

amazon3 <- amazon2[which(amazon2$price <= quantile(amazon2$price, prob = 0.99) & 
                           amazon2$price_online <= quantile(amazon2$price_online, prob = 0.99)),]

set.seed(31)
amazon3$id_rand <- rnorm(nrow(amazon3), mean = 0, sd = 1)
amazon4 <- amazon3[order(amazon3$id_rand, decreasing = FALSE),]
amazon4 <- amazon4[1:1000,]
#This is simple random samlping

# 3.	Copy-paste your answer in Q4 in PS1.
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

# 4. Use a t-test: compare the means of price and online price
# H0 = means are the same
# Ha = means are different, they are not equal

t.test(amazon3$price == amazon3$price_online, alternative = c('two.sided'))

# extremely small p value. We can reject the null hypothesis. They are not the same.

# Question 5

x <- mean(amazon3$price_online)
t.test(amazon3$price, mu=x) # gives p value > 0.05: the means are not different. Fail to reject the Null

y <- mean(amazon3$price)
t.test(amazon3$price_online, mu=y) # gives p value > 0.05: the means are not different. Fail to reject the Null

# Question 6.	Use once again the original dataset and drop observations 
# for that are above the 95 percentile of the price variable.

amazon5 <- amazon[which(amazon$price <= quantile(amazon$price, prob = 0.95)),]


#7.	Generate a dummy variable taking value one if price_online is missing and zero otherwise. 
# Call this variable missing_online. 
# Make histograms and boxplots of price and price_amazon for each value of this indicator variable. 
# What can you say with respect to these conditional distribu-tions?

amazon5$missing_online <- ifelse(is.na(amazon5$price_online), 1, 0)
amazon5$missing_online <- factor(amazon5$missing_online)

library(ggplot2)
ggplot(amazon5, aes(price)) + geom_histogram() + facet_wrap(~missing_online)
ggplot(amazon5, aes(price_online)) + geom_histogram() + facet_wrap(~missing_online)

ggplot(amazon5, aes(missing_online, price)) + geom_boxplot()
ggplot(amazon5, aes(missing_online, price_online)) + geom_boxplot()

#End of coding, you can go ahead with question 8
