rm(list=ls())

library(survey)
library(plyr)
library(tseries)
setwd("C:/Users/Christopher/Documents/Projects/Project - Economic Machine")

d <- read.csv("VIX.CSV")
# Interestdata <- read.csv("FRB_H15.CSV")
#data(AirPassengers)
#ap <- AirPassengers

#Remove the last column Volumne, which is all zero
d <- d[,-c(7)]
#Interestdata <- Interestdata[-c(1:4),1:12]

#Create Time Series Variables
TS_Open <- ts(d[,2],start = c(1990,1), frequency = 365)
TS_Open_2000 <- ts(d[2529:6995,2], start = c(2000,1), frequency = 365)
TS_Open_2004 <- ts(d[3989:6995,2], start = c(2004,1), frequency = 365)

tsclean(TS_Open)

#TS_High <- ts(d[,3],start = c(1990,1), frequency = 365)
#TS_Low <- ts(d[,4],start = c(1990,1), frequency = 365)
#TS_Close <- ts(d[,5],start = c(1990,1), frequency = 365)


#Decompose function visualization
TS.decom = decompose(TS_Open,type="mult")
plot(TS.decom)

#Plot Trend*Seasonality
ts.plot(cbind(TS.decom$trend, TS.decom$trend * TS.decom$seasonal),lty=1:2)

#Decompose the random section for ARIMA analysis
random=TS.decom$random


#partial autocorrelations
pacf(random,na.action=na.exclude,main="Part-Corr")

#autocorrelations or autocovariance
acf(random,na.action=na.exclude,main="Auto-Corr")


###############################################################
# Model 1

m1 = arima(random,order=c(1,0,1))
m1

#aic = -10461.2

res1 = residuals(m1)

#Shapiro-wilk normality test, if saller than 0.05 then reject the null and residuals are not normally distributed
shapiro.test(res1)
confint(m1)
#hist(res1)
predict(m1,n.ahead=10)



##############################################################
# Model 2

m2 = arima(random,order=c(2,0,0))
m2

#aic = -10471.28

res2 = residuals(m2)
shapiro.test(res2)
confint(m2)

predict(m2,n.ahead=10)

#############################################################
# Model 3

m3 = arima(random,order=c(1,0,1))
m3

#aic = -10472.35

res3 = residuals(m3)
shapiro.test(res3)
confint(m3)

predict(m3,n.ahead=10)


#############################################################









# create/specify survey design
survey_design <- svydesign(ids=~1, strata = ~stype, fpc = ~fpc, weights = ~pw, data = d)


