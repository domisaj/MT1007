### z-score

# Vector of scores
a <- c(5, 3, 7, 5, 5, 3, 4)
# Mean of a 
mean(a)
# SD of a
sd(a)
# Returns vector with standardized scores
a.z <- (a-mean(a)) / sd(a)
a.z

# Check mean is 0
mean(a.z)
# Check SD is 1
sd(a.z)

# Two vectors with their corresponding standardized scores
plants <- c(12, 8, 1, 6, 2)
fruits <- c(100, 520, 430, 200, 700)
plants.z <- (plants - mean(plants)) / sd(plants)
plants.z
fruits.z <- (fruits - mean(fruits)) / sd(fruits)
fruits.z

# Checking
mean(plants.z)
sd(plants.z)
mean(fruits.z)
sd(fruits.z)

### t-tests

x = rnorm(10)
y = rnorm(10)
ttest <- t.test(x, y)
ttest
names(ttest)
names(t.test)
ttest$p.value
ttest$statistic
# Critical value
qt(1-0.05/2, 17.66) # two-sided t-test
qt(0.05/2, 17.66, lower.tail = FALSE) # two-sided t-test
qt(0.05/2, df = 17.66, lower.tail = FALSE)

# Default values
# Can change mu if testing for a mean difference  other than 0
# Can change alt to one-sided if we'd like to do a one-sided test less than
# or greater than
# Can change var.eq to TRUE when we have equal variances
# Can change paired when we have within t-test
data <- read.table("LungCapData.txt", header = TRUE)
t.test(data$LungCap ~ data$Smoke, mu = 0, alt="two.sided", 
       conf= 0.95, var.eq = F, paired = F)
t.test(data$LungCap[data$Smoke == "no"], data$LungCap[data$Smoke == "yes"])
# Test whether variance is equal or nonequal between the groups 
# using Levene's test
library(car)
leveneTest(data$LungCap~data$Smoke)
# use the ~ when the x has two categories, otherwise use , when x is 
# continuous
# t-test: ways of examining the relationship between a numeric outcome
# variable (Y) and a category explanatory variable (X, with 2 levels)
t.test(data$LungCap ~ data$Gender)
t.test(data$LungCap[data$Gender == "female"], data$LungCap[data$Gender == "male"])