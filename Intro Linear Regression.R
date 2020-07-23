data <- read.csv("weight_height.csv", header = TRUE)

# Fit a simple linear regression model using least squares
lm1 <- lm(Weight ~ Height, data = data)
lm1

# Intercept of the model
lsint <- -39.06
# Slope of the model
lsslope <- 61.27
# Simple regression equation
lsfit <- lsint + lsslope*data$Height
# Residuals 
lsres <- data$Weight - lsfit
  
# Plot scatterplot
plot(data$Height, data$Weight, xlab = "Height", ylab = "Weight")
# Add simple linear model to the scatterplot, shown by a red line 
points(data$Height, lsfit, col = 2)

# Minimum sum of squared residuals 
# Also SS for the residuals in anova function
sum(lsres^2)

# Histogram to check shape of distribution of residuals - error term
# assumed to follow a normal distribution with mean 0 and constant SD
hist(lsres)
# Check mean of residuals 
mean(lsres)

# Obtain more information about the model 
# Residual  standard error is the variance of the errors
summary(lm1)
# Use anova function to get more info about the model
anova(lm1)
# Calculate R2 - matches multiple R-squared
r2 <- 685.88/(685.88+7.49)
r2

# Histogram of 10,000 values from F distribution
hist(rf(10000, df = 1, df2 = 13))
# Get critical value
qf(p = 0.05, df1=1, df2=13, lower.tail=FALSE)
# F test statistic
f_value <- 1190
# F statistic greater than critical value, leading to a very small p-value and
# a relationship between height and