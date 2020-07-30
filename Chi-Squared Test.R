findata <- read.csv("Financial data.csv", header = TRUE)

findata[ , 1:2] <- NULL


# Create Tobins Q indicator variable
findata$tqind <- ifelse(findata$tobinsQ > 1, 1, 0)

# Save chosen years
which.years <- c(1990, 2000, 2010)
# Select records for chosen years
seldata <- findata[is.element(findata$year, which.years), ]

# table
table(seldata$tqind, seldata$year)

table(seldata$year)

tab.yr <- table(seldata$year)
tab.yr

tab.tq <- table(seldata$tqind, seldata$year)[2, ]

## x2 test of independence

obs <- c(150,86,65,72,39,6,15,21,25,5,10,15)
obs.mat <- matrix(obs, ncol = 3, nrow = 4, byrow = FALSE)
obs.mat
res <- chisq.test(obs.mat)
res
hist(rchisq(10000, df = 6))
names(res)
res$statistic
qchisq(0.05, df = 6, lower.tail = FALSE)

abline(v = qchisq(0.05, df = 6, lower.tail = FALSE), col = 3)
abline(v = res$statistic, col = 2)
res$expected

# Get chi-squared value
((obs.mat - res$expected)^2/ res$expected)
