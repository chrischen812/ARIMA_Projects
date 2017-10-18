#Information from 
#https://machinelearningmastery.com/arima-for-time-series-forecasting-with-python/


rm(list=ls())

library('ggplot2')
library('forecast')
library('tseries')

setwd("C:/Users/Christopher/Documents/Projects/GitHub R/ARIMA_Projects/")

daily_data = read.csv('sales-of-shampoo-over-a-three-ye.csv', header=TRUE, stringsAsFactors=FALSE)



ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") + xlab("")


TS_sales <- ts(daily_data[1:36,2], start = c(2001,1), frequency = 12)
TS_sales = na.omit(tsclean(TS_sales))
plot(TS_sales)



#Decomposion
decomp = stl(TS_sales, s.window="periodic")
decomp2 = decompose(TS_sales,type="mult")

deseasonal_cnt <- seasadj(decomp)
deseasonal_cnt2 <- seasadj(decomp2)

plot(deseasonal_cnt)
plot(deseasonal_cnt2)


#Finding Integrating part - Stationary
adf.test(TS_sales_d1, alternative = "stationary")

TS_sales_d1 = diff(TS_sales, differences = 1)
plot(TS_sales_d1)


#AutoCorrelation
Acf(TS_sales, main='ACF for Differenced Series')
Acf(TS_sales_d1, main='ACF for Differenced Series')

Pacf(TS_sales, main='PACF for Differenced Series')
Pacf(TS_sales_d1, main='PACF for Differenced Series')



auto.arima(TS_sales, seasonal=FALSE)
arima(TS_sales_d1,order=c(5,1,12))

#








