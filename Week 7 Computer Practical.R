# Read in the financial data
findata <- read.csv("Financial data.csv",header=TRUE) 
# Create Tobins Q indicator variable
findata$tqind <- ifelse(findata$tobinsQ>1, 1, 0) # ifelse() ifselse(test, yes, no)

# Save chosen years
which.years <- c(1990, 2000, 2010)
# Select records for chosen years
seldata <- findata[is.element(findata$year,which.years), ]

# table
table(seldata$tqind,seldata$year)

# table of total number of records in each year
table(seldata$year)

# number of records each year
tab.yr <- table(seldata$year)
tab.yr

# selects number of records where TQ > 1
tab.tq <- table(seldata$tqind, seldata$year)[2, ]
tab.tq

# Total number of records
tot.yr <- sum(tab.yr)
# Total number of TQ>1 records 
tot.tq <- sum(tab.tq)

# Proportion of records in each year
prop.yr <- tab.yr/tot.yr
prop.yr

#Proprtions of TQ>1 records in each year
prop.tq <- tab.tq/tot.tq
prop.tq

par(mfrow=c(1,2))
barplot(prop.yr,main='Proportion of records \n in each year') 
barplot(prop.tq,main='Proportion of records \n TQ>1 in each year')

# Calculate expected number of records TQ>1
exp.tq <- prop.yr * tot.tq
exp.tq

# Calculate chi-square values for all years
chisq.tq <- (tab.tq - exp.tq)^2/exp.tq
chisq.tq

# Calculate the chi-square test statistic
chisq.stat <- sum(chisq.tq)
chisq.stat

# Histogram of 10000 random values generated from a
# x2 distribution
hist(rchisq(n=10000, df=2), xlim=c(0,22))
abline(v = chisq.stat, col = 2)

# p-value
pchisq(chisq.stat, df=2, lower.tail=FALSE)
# critical value
qchisq(0.05, df = 2, lower.tail = FALSE)

# plot
chisq.crit <- qchisq(0.05, df=2, lower.tail=FALSE)
hist(rchisq(n=10000, df=2),main="df=2",xlab="",xlim=c(0,22))
abline(v=chisq.stat,col=2)
abline(v=chisq.crit,col=3)

# x2 goodness of fit test
chisq.test(tab.tq, p = prop.yr)

### x2 test of independence

# make matrix of observations
obs <- c(150,86,65,72,39,6,15,21,25,5,10,15)
obs.mat <- matrix(obs, nrow = 4, ncol = 3, byrow = FALSE)
obs.mat
# x2 test of independence
res <- chisq.test(obs.mat)
res
names(res)
# x2 statistic
res$statistic

# plot
par(mfrow = c(1,1)
# Histogram of 10000 random values generated from
# x2 distribution
hist(rchisq(10000, df = 6))

# get critical value
critical.value <- qchisq(0.05, df = 6, lower.tail = FALSE)

# plot
hist(rchisq(10000, df=6))
abline(v=critical.value, col=3) # line for critical value
abline(v=res$statistic, col=2) # line for test statistic

# expected values
res$expected
# observed values
obs.mat
# chi-squre value for each category
((obs.mat - res$expected)^2)/res$expected
