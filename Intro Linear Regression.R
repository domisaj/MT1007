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
# Add regression line
abline(lm(Weight~Height, data = data), col = 2)

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
# a relationship between height and weight

### PREDICTION

# Get predicted values for all rows in the original dataset
data$Predicted <- predict(lm1, newdata = data)

# Create a dataframe of new values of height for which we want to predict
# weight using the linear model
new_heights <- data.frame(Height = c(1.85, 1.87, 1.89))
# Add a new column to the dataframe with the predicted values for height
new_heights$Weight <- predict(lm1, newdata = new_heights)    

# Scatterplot of original values
plot(data$Height, data$Weight, xlab = "Height", ylab = "Weight")
# Add red dots showing the predicted values
points(data$Height, data$Predicted, col = 2)

### CHECKING ASSUMPTIONS

# Storing residuals
res <- residuals(lm1)

## Checking normality in the errors

# QQ Plot
qqnorm(res)
# Histogram
hist(res, nclass = 10)
# Shapiro-Wilks test
shapiro.test(res)

## Assessing constant error variance

# Plot predicted values vs residuals
plot(data$Predicted, res, xlab = "Predicted Values", ylab = "Residuals")
abline(h=0, lty=2)

# Breusch-Pagan test
ncvTest(lm1)

## Checking independence

# Plot of residuals
plot(res)
# Durbin-Watson test
durbinWatsonTest(lm1) # p-value is small so evidence to reject null hypothesis
                      # and conclude residuals are correlated

