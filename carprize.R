library(RCurl)
question <- readLines("https://s3-ap-southeast-1.amazonaws.com/elearning.excelr.com/DS/Assignments/Multi+Linear+Regression/Toyota+Corolla.txt")
View(question)

setwd("F://R//files")
carmodel <- read.csv("ToyotaCorolla.csv")

View(carmodel)
attach(carmodel)


finalmodel <- carmodel[,c(3,4,7,9,13,14,16,17,18)]
View(finalmodel)

#visualizations
plot(finalmodel)
pairs(finalmodel)

#to find correlation
cor(finalmodel)

#partial correlation matrix
library(corpcor)
cor2pcor(cor(finalmodel))

finalmodel <- as.dataframe(finalmodel)
#modelbuilding

model1 <- lm(finalmodel$Price ~ finalmodel$Age_08_04 + finalmodel$KM + finalmodel$HP + finalmodel$cc + finalmodel$Doors + finalmodel$Gears + finalmodel$Quarterly_Tax, finalmodel$Weight, data = finalmodel)
summary(model1)

install.packages("VIF")
library(VIF)
vif(model1)
#building model only for cc
model2 <- lm(finalmodel$Price ~ finalmodel$cc)
summary(model2)
plot(model2)
library(psych)
pairs.panels(finalmodel)

#to find influence factor
influence.measures(model1)

library(car)
influenceIndexPlot(model1)
influencePlot(model1)

finalmodel1 <- finalmodel[-c(1084,1094,1280,1267,1172,111),]
model3 <- lm(finalmodel1$Price ~ finalmodel1$Age_08_04 + finalmodel1$KM + finalmodel1$HP + finalmodel1$cc + finalmodel1$Doors + finalmodel1$Gears + finalmodel1$Quarterly_Tax, finalmodel1$Weight, data = finalmodel1)

summary(model3)

confint(model3, level=0.95)
predict(model3, interval = "predict")
#evaluation
plot(model3)

#LMSE value 
sqrt(mean(model3$residuals)^2)

