### Getting Started
# Read financial data
data <- read.csv("Financial data.csv", header = T)
# Check what objects have been created 
ls()
# Summary of tobinsQ
summary(data$tobinsQ)
# List of industry classes
sort(unique(data$indclass))
# Records for each industry class
table(data$indclass)

### Subsetting Data
tq15 <- data$tobinsQ[data$indclass == 15 & !is.na(data$indclass)]
tq26 <- data$tobinsQ[data$indclass == 26 & !is.na(data$indclass)]
# Records in tq15 and tq26
length(tq15)
length(tq26)
# Summary
summary(tq15)
summary(tq26)
# Difference in means
diff_mean <- mean(tq15) - mean(tq26)

### Boxplot
boxplot(tq15, tq26,
        notch = T,
        col = c("orange", "lightblue"))

### t-test 
t.test(x = tq15, y = tq26)

### Bootstrap method to calculate empirical confidence intervals
# Load simpleboot and boot packages
require(simpleboot)
require(boot)
# Getting empirical distribution of differences between two means
b1 <- two.boot(sample1 = tq15, sample2 = tq26, FUN = mean, R = 500)
# Calculating confidence interval
boot.ci(boot.out = b1,conf = 0.95, type = c("perc"))
# Shape of bootstrap distribution
hist(b1) # Red line is the difference between the means

### 95% confidence interval for industry class 15
# Lower confidence interval
lower <- mean(tq15) + qt(0.025, df = length(tq15)-1) * sd(tq15)/sqrt(length(tq15))
lower
# Upper confidence interval
upper <- mean(tq15) - qt(0.025, df = length(tq15)-1) * sd(tq15)/sqrt(length(tq15))
upper

### Bootstrap to get confidence interval for one variable - industry class 15
b1 <- one.boot(data = tq15, FUN = mean, R = 2000)
boot.ci(boot.out = b1, conf = 0.95, type = c("perc"))