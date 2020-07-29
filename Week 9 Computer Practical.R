# Read in financial data
findata <- read.csv("Financial data.csv", header = TRUE)

#Select data for industry class 26 and 49
selclass <- c(26, 49)
seldata <- findata[is.element(findata$indclass, selclass) & !is.na(findata$indclass), ]

# Plot multiple scatterplot to look at relationships between continuous variables
pairs(seldata[ c(3, 2, 4, 10)])

# Look at relationship between assets and the 2 indclasses using boxplot
boxplot(seldata$assets ~ seldata$indclass)

# Summary of the 2 indclasses
tapply(seldata$assets, seldata$indclass, summary)

# Fit a multiple linear regression model
lm1 <- lm(assets ~ capex + year + rd + as.factor(indclass), data = seldata)
# Get info about the fitted model
summary(lm1)

# Load car package
require(car)
# Get ANOVA table - use ANOVA table to decide whether or not to drop terms 
# from the model
anova(lm1)

# Drop rd to find a better model
lm2 <- lm(assets ~ capex + year + as.factor(indclass), data = seldata)
# Summary 
summary(lm2)
# ANOVA table
anova(lm2)

# Use AIC to find the best model - only main effects
AIC(lm1)
# AIC is lower in lm2 than lm1, so lm2 is a better model
AIC(lm2)

# Model selection by AIC
step(lm1, direction = "both")

### Prediction

# Create a dataframe containing values we want to predict
df <- data.frame(capex = c(10, 10), year = c(2004, 2004), rd = c(3,3), indclass = c(26, 49))
# Add column in the dataframe with the predicted values
df$predicted <- predict(lm2, newdata = df)

# Plot assets against capex and then fix values for the other variables
plot(seldata$capex, seldata$assets, xlab = "Capital Expenditure",
     ylab = "Assets")
# Create data frame for indclass 26
fitdata <- data.frame(capex=c(0,50),year=c(2004,2004),rd=c(3,3),indclass=c(26,26)) 
# Add column in dataframe with predicted values
fitdata$fitval <- predict(lm2, newdata=fitdata)
# Add fitted values of indclass 26 to scatterplot
lines(fitdata$capex, fitdata$fitval, col = 2)

# Create data frame for indclass 49
fitdata2 <- data.frame(capex=c(0,50),year=c(2004,2004),rd=c(3,3),indclass=c(49,49)) 
fitdata2$fitval <- predict(lm2, newdata=fitdata2)
 # Add fitted values of indclass 49 to scatterplot
lines(fitdata$capex, fitdata2$fitval,col=3)


### Check Collinearity
vif(lm2) # vifs around 1 and not a cause for concern

### Including Interactions

# Finding best model with interaction using AIC
lm3 <- lm(assets ~ capex + year + rd + as.factor(indclass) + capex:year, data = seldata)
AIC(lm3)
lm4 <- lm(assets ~ capex + year + as.factor(indclass) + year:as.factor(indclass), data = seldata)
AIC(lm4) # best model, lowest AIC
lm5 <- lm(assets ~ capex + year + as.factor(indclass) + capex:as.factor(indclass), data = seldata)
AIC(lm5) 

# lm4 is the best model
summary(lm4)

### Predictions and Plots

# Scatterplot between capex and assets
plot(seldata$capex, seldata$assets, xlab = "Capital Expenditure",
     ylab = "Assets")
# Create data frame for indclass 26
fitdata3 <- data.frame(capex=c(0,50),year=c(2004,2004),rd=c(3,3),indclass=c(26,26)) 
# Add column in dataframe with predicted values
fitdata3$fitval <- predict(lm4, newdata=fitdata3)
# Add fitted values of indclass 26 to scatterplot
lines(fitdata$capex, fitdata3$fitval, col = 2)

# Create data frame for indclass 49
fitdata4 <- data.frame(capex=c(0,50),year=c(2004,2004),rd=c(3,3),indclass=c(49,49)) 
fitdata4$fitval <- predict(lm4, newdata=fitdata2)
# Add fitted values of indclass 49 to scatterplot
lines(fitdata$capex, fitdata4$fitval,col=3)

### Extrapolation - predict assets for year 2030 using max values 
df2 <- data.frame(capex = c(55, 55), year = c(2030, 2030), rd = c(20,20), indclass = c(26, 49))
df2$predicted <- predict(lm4, newdata = df2)

### Questions from Lab
df3 <- data.frame(capex = 10, year = 2004, rd = 3, indclass = 49)
df3predicted <- predict(lm1, newdata = df3)
df3predicted

lm5 <- lm(assets ~ capex + year + as.factor(indclass) + year:as.factor(indclass), data = seldata)
df4 <- data.frame(capex = 10, year = 2004, indclass = 49)
df4predicted <- predict(lm5, newdata = df4)
df4predicted

vif(lm1)

summary(lm1)

# Get p-value associated with the F statistic for model lm1
pf(21.7, 4, 62, lower.tail=FALSE)
# Get critical value associated with the F statistic for model lm1
qf(0.05, 4, 62, lower.tail=FALSE)