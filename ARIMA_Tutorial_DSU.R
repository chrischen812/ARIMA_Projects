#Information from 
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials


library('ggplot2')
library('forecast')
library('tseries')


setwd("C:/Users/Christopher/Documents/Projects/GitHub R/ARIMA_Projects/")

daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)


#Step 1 Stating point is to plot the series and visually examine it for any outliers, volatility, or irregularities.
daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") + xlab("")
 


#Step 2 Using the ts() command to create a time series object to pass to tsclean()
count_ts = ts(daily_data[, c('cnt')])

daily_data$clean_cnt = tsclean(count_ts)

ggplot() +   geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

#Step 3 The wider the window of the moving average, the smoother original series becomes. In our bicycle example, we can take weekly or monthly moving average, smoothing the series into something more stable and therefore predictable
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')


#Step 4 The building blocks of a time series analysis are seasonality, trend, and cycle. 
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
#decomp2 = decompose(count_ma,type="mult")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#plot(decomp2)

#Step 5 The augmented Dickey-Fuller (ADF) test is a formal statistical test for stationarity.
adf.test(count_ma, alternative = "stationary")


#Step 6 ACF plots display correlation between a series and its lags. In addition to suggesting the order of differencing, ACF plots can help in determining the order of the M A (q) model. Partial autocorrelation plots (PACF), as the name suggests, display correlation between a variable and its lags that is not explained by previous lags. PACF plots are useful when determining the order of the AR(p) model.
Acf(count_ma, main='')

Pacf(count_ma, main='')

#Step 7 Start with the order of d = 1 and re-evaluate whether further differencing is needed.
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
#ndiffs(deseasonal_cnt, alpha=0.05, test = c("adf"))

#Step 8 Spikes at particular lags of the differenced series can help inform the choice of p or q for our model:

Acf(count_d1, main='ACF for Differenced Series')
#ACF plots display correlation between a series and its lags. 
Pacf(count_d1, main='PACF for Differenced Series')
#PACF plots display correlation between a variable and its lags that is not explained by previous lags. 

#There are significant auto correlations at lag 1 and 2 and beyond. Partial correlation plots show a significant spike at lag 1 and 7. This suggests that we might want to test models with AR or MA components of order 1, 2, or 7. A spike at lag 7 might suggest that there is a seasonal pattern present, perhaps as day of the week. We talk about how to choose model order in the next step.


#step 9 There exist a number of such criteria for comparing quality of fit across multiple models. Two of the most widely used are Akaike information criteria (AIC) and Baysian information criteria (BIC). 
#When comparing models, one wants to minimize AIC and BIC.

auto.arima(deseasonal_cnt, seasonal=FALSE)


#Step 10 Examining ACF and PACF plots for model residuals. If model order parameters and structure are correctly specified, we would expect no significant autocorrelations present. 

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=15, main='(1,1,1) Model Residuals')

#There is a clear pattern present in ACF/PACF and model residuals plots repeating at lag 7. This suggests that our model may be better off with a different specification, such as p = 7 or q = 7. 


#Step 11 Refitting with MA(7) component and examine diagnostic plots again.

fit2 = arima(deseasonal_cnt, order=c(7,1,7))

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')


#Step 12 Forecast the next 30 periods
fcast <- forecast(fit2, h=30)
plot(fcast)


#Step 13 Reserve a portion of our data as a "hold-out" set, fit the model, and then compare the forecast to the actual observed values:
hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

#Recall that the model is assuming a series with no seasonality, and is differencing the original non-stationary data. In other words, plotted predictions are based on the assumption that there will be no other seasonal fluctuations in the data and the change in number of bicycles from one day to another is more or less constant in mean and variance. This forecast may be a naive model, but it illustrates the process of choosing an ARIMA model and could also serve as a benchmark to grade against as more complex models are built.


#Step 14 Forecast Improvement. One simple change is to add back the seasonal component we extracted earlier. Another approach would be to allow for (P, D, Q) components to be included to the model, which is a default in the auto.arima() function.

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality


seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)

tsdisplay(residuals(fit_w_seasonality), lag.max=15, main='Seasonal Model Residuals')







#Test with "hold-out" set and seasonality adjustment

fit_w_seasonality_no_holdout = auto.arima(deseasonal_cnt[-c(700:725)], seasonal=TRUE)


fcast_w_seasonality_no_holdout <- forecast(fit_w_seasonality_no_holdout, h=25)

plot(fcast_w_seasonality_no_holdout)
lines(ts(deseasonal_cnt))


