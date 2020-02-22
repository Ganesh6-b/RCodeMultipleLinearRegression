setwd("F://R//files")

startupdata<- read.csv("50_Startups.csv")
View(startupdata)
attach(startupdata)
colnames(startupdata)

#profit is output y

library(dummies)

startupdata <- cbind(startupdata, dummy(State))

View(startupdata)

finaldata <- startupdata[,-4]
View(finaldata)

#visualizations
plot(finaldata)
pairs(finaldata)

finaldata[,c(1,2,3)] <- scale(finaldata[,c(1,2,3)])

cor(finaldata)

library(corpcor)
cor2pcor(cor(finaldata))

#build a model
attach(finaldata)
model1 <- lm(finaldata$Profit ~ finaldata$R.D.Spend + finaldata$Administration + finaldata$Marketing.Spend + finaldata$startupdataCalifornia + finaldata$startupdataFlorida + finaldata$`startupdataNew York`)
summary(model1)

model2 <- lm(Profit ~ Administration)
summary(model2)

model3 <- lm(Profit ~ R.D.Spend)
summary(model3)

model4 <- lm(Profit ~ Marketing.Spend)
summary(model4)

model5 <- lm(Profit ~ R.D.Spend + Administration)
summary(model5)

#influence factor
library(car)
influenceIndexPlot(model1)
#there is no correlation between profit and administration.... so i removed administration
datas <- finaldata[-c(46,47)]
model6 <- lm(datas$Profit ~ datas$R.D.Spend  + datas$Marketing.Spend, data= datas)
summary(model6)
library(car)
influenceIndexPlot(model6)
vif(model1)
?vif
predict = predict(model6, level = "predict")
predict
