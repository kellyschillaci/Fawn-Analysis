##Read the data

URLToRead <- "http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
library(gdata)
TestFrame <- read.xls(URLToRead)
str(TestFrame)

##Changing Column Names

library(data.table)
setnames(TestFrame, old=c("X1","X2","X3","X4"), new=c("Fawn","Antelope","Precipitation","WinterRating"))
str(TestFrame)

#Creating bivariate plots
library(ggplot2)
AntelopePlot <- ggplot(data=TestFrame, aes(x=Antelope, y=Fawn))+geom_point()+xlab("Antelope")+ylab("Fawn")
PrecipitationPlot <- ggplot(data=TestFrame, aes(x=Precipitation, y=Fawn))+geom_point()+xlab("Precipitation")+ylab("Fawn")
WinterRatingPlot <- ggplot(data=TestFrame, aes(x=WinterRating, y=Fawn))+geom_point()+xlab("WinterRating")+ylab("Fawn")

#Regression Models

##Model 1 Predict the Number of Fawns from the Severity of the Winter

model1 <- lm(formula=Fawn ~ WinterRating, data=TestFrame)
summary(model1)
##The adjusted R squared is too low to consider this a good model.  The x variable and the intercept are significant because they have a p value of less than .05.

plot(TestFrame$WinterRating, TestFrame$Fawn)
abline(model1)
range(TestFrame$WinterRating)
test=data.frame(WinterRating=2)
predict(model1, test, type="response")

Model1Plot <- WinterRatingPlot+stat_smooth(method="lm", col="red")
##Model 2 Predict the Number of Fawns from the Severity of the Winter and Precipitation

model2 <- lm(formula=Fawn ~ WinterRating + Precipitation, data=TestFrame) 
summary(model2)

##The adjusted R squared is much better for this equation.  The precipitation and the intercept are definitely significant because they have a p value of less than .05.  I would still use the Winter rating in my equation even though it has a p value of .19.
range(TestFrame$Precipitation)
newdata <- data.frame(WinterRating=2, Precipitation=12)
predict(model2,newdata, type="response")

Model2Plot <- ggplot(TestFrame, aes(x=WinterRating, y=Precipitation))+geom_point(aes(size=Fawn, color=Fawn))+geom_smooth(method="lm")

##Model 3 Predict the Number of Fawns from the Severity of the Winter, Precipitation, and Number of Antelope

model3 <- lm(formula=Fawn ~ WinterRating + Precipitation + Antelope, data=TestFrame)
summary(model3)

##The adjusted R squared is the highest for this equation because it takes all the data in to effect.  All variables have a p value of less than .05 so we would use them all in the equation.  Even though this has the most variables, I would still use this one.
range(TestFrame$Antelope)
newdata2 <- data.frame(WinterRating=2, Precipitation=12, Antelope=8)
predict(model3,newdata2, type="response")

Model3Plot <- ggplot(TestFrame, aes(x=WinterRating, y=Precipitation))+geom_point(aes(size=Fawn, color=Antelope))+geom_smooth(method="lm")
