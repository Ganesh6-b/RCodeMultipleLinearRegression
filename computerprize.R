setwd("F://R//Rfiles")
computerdata <- read.csv("Computer_Data.csv")
View(computerdata)

attach(computerdata)
library(dummies)

dummycd <- dummy(cd)
dummymulti <- dummy(multi)
dummypremium <- dummy(premium)

compdata <- cbind(computerdata, dummycd,dummymulti, dummypremium)

View(compdata)

finalcompdata <- compdata[,-c(1,7,8,9)]
View(finalcompdata)

plot(finalcompdata)
str(finalcompdata)

#building model

model1 <- lm(price ~ finalcompdata$speed + finalcompdata$hd + finalcompdata$ram + screen + finalcompdata$ads + finalcompdata$trend + finalcompdata$cdno + finalcompdata$cdyes + finalcompdata$multino + finalcompdata$multiyes + finalcompdata$premiumno + finalcompdata$premiumyes)
summary(model1)

#everything is achieving p value perfectly

#RMSE value

sqrt(mean(model1$residuals)^2)

confint(model1, level= 0.95)
predict(model1, interval = "predict")
#evaluation
plot(model1)
