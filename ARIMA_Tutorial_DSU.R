#Information from https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials


library('ggplot2')
library('forecast')
library('tseries')

setwd("C:/Users/Christopher/Documents/Projects/Project - Economic Machine")

daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)


#Step 1
daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") + xlab("")



#Step 2
count_ts = ts(daily_data[, c('cnt')])

daily_data$clean_cnt = tsclean(count_ts)

ggplot() +   geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

#Step 3
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')


#Step 4
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#Step 5
adf.test(count_ma, alternative = "stationary")


#