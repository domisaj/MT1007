findata <- read.csv("Financial data.csv", header = TRUE)
findata$pperatio <- findata$ppe/findata$assets
head(findata)
findata[1:2, ]
findata[1, 1:5]
findata[2,3]
selclass <- c(14, 17, 43)
seldata <- findata[is.element(findata$indclass, selclass) & !is.na(findata$indclass), ]
seldata$indclass <- as.factor(seldata$indclass)
tapply(seldata$pperatio, seldata$indclass, summary)
tapply(seldata$pperatio, seldata$indclass, sd)
tapply(seldata$pperatio, seldata$indclass, length)
boxplot(seldata$pperatio~seldata$indclass)
aov.ppe <- aov(seldata$pperatio~seldata$indclass)
aov.ppe
summary(aov.ppe)
TukeyHSD(aov.ppe, conf.level = 0.95)
plot(TukeyHSD(aov.ppe, conf.int = 0.95))
findata[3,6]
group1 <- findata$pperatio[findata$indclass==43 & !is.na(findata$indclass)] 
group2 <- findata$pperatio[findata$indclass==14 & !is.na(findata$indclass)]
len1 <- length(group1)
len2 <- length(group2)
df.dif <- len1 + len2 - 2
var1 <- var(group1)
var2 <- var(group2)
dif <- mean(group1) - mean(group2)
sp <- sqrt(((len1-1)*var1 + (len2-1)*var2)/df.dif)
se.dif <- sp*sqrt((1/len1) + (1/len2))
int.dif <- abs(qt(0.025,df=df.dif)) * se.dif
# Lower CI
ll <- dif - int.dif
# Upper CI
ul <- dif + int.dif
ll
ul
se.dif
