#Import csv file and read it into a data frame
setwd("/Users/archiejain/ALY6020_Predictive Analytics/Week-4")
trend <- read.csv("suicide_pre.csv")
library(gtrendsR)
gtrends_data <- gtrends("Suicide + How to commit suicide", time="2017-01-15 2017-03-31")
sapply(trend, class)
class(trend$Day)
trend$Day <- as.Date(trend$Day , format="%m/%d/%y")
trend_dataframe<- data.frame(date=trend$Day, trend$Interest)
#Convert the data frame into time series and plot it to see the trend
trendts <- ts(trend_dataframe$trend.Interest)
plot.ts(trendts)

library(ggplot2)
ggplot(trend_dataframe, aes(trend_dataframe$date, trend_dataframe$trend.Interest)) + geom_line() +
  scale_x_date(date_labels = "%m/%d/%y") + 
  xlab("Time before show premiered") + ylab("Related search volume") + ggtitle("Time series plot before show's premier")

#Trying to decompose the series-- This will not work as the series has less components to be decomposed
# components.ts <- decompose(trendts)
# plot(components.ts)

#differencing the series since it has varying mean and variance (resulting in d=1)
library(funitRoots)
#urkpssTest(trendts, type=c("tau"), lafs=c("short"), use.lag=NULL, doplot=TRUE)
trendts_stationary = diff(trendts, differences=1)
plot(trendts_stationary)

#Correlation and Auto-correlation values
acf(trendts, lag.max = 34)
pacf(trendts, lag.max = 34)

#Fitting the model
fitARIMA <- arima(trendts_stationary, order= c(6,0,0), method ="ML")
summary(fitARIMA)
library(forecast)
fitARIMA_plot <- forecast(fitARIMA, h=12)
plot(fitARIMA_plot, xlab = "time", ylab="interests in search for suicide related terms")

trendplotfor <- auto.arima(trendts,trace = TRUE, seasonal = TRUE)
library(tseries)
trendplotfor1 <- forecast(trendplotfor, h=12)
summary(trendplotfor1)
plot(trendplotfor1)

# p1a<-ggplot(data=trend_dataframe, aes(x=trend_dataframe$date, y=trend_dataframe$trend.Interest)) 
# p1a<-p1a+geom_line(aes(y=fitARIMA_plot))
# p1a<-p1a+scale_x_date(name='',breaks='1 month',date_labels = "%b-%y",expand=c(0,0))
# p1a<-p1a+scale_y_continuous(name='Units of Y')
# p1a

## Graph showing the post premeir data of suicide related searched on google
trend1 <- read.csv("suicide_post.csv") 
trend1$Day <- as.Date(trend1$Day , format="%m/%d/%y")
trend1_dataframe<- data.frame(date=trend1$Day, trend1$Interest)
trend1ts <- ts(trend1$Interest)
plot.ts(trend1ts)

ggplot(trend1_dataframe, aes(trend1_dataframe$date, trend1_dataframe$trend1.Interest)) + geom_line() +
  scale_x_date(date_labels = "%m/%d/%y") + 
  xlab("Time post show premiered") + ylab("Interest") + ggtitle("Time series plot")

#differencing the series since it has varying mean and variance (d=1)
trend1ts_stationary = diff(trend1ts, differences=1)
plot(trend1ts_stationary)





