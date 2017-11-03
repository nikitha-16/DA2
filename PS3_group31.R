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

amazon3 <- amazon2[which(amazon2$price <= quantile(amazon2$price, prob = 0.99) & amazon2$price_online <= quantile(amazon2$price_online, prob = 0.99)),]

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

# extremely small p value. We can reject the null hypothesis

# Question 5

x <- mean(amazon3$price_online)
t.test(x == amazon3$price, alternative = c('two.sided'))

y <- mean(amazon3$price)
t.test(y == amazon3$price_online, alternative = c('two.sided'))

