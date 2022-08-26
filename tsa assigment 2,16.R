#TSA ASSIGNMENT

#Walmart data Weekly Sales Forecast

#Install and load Packages
library(fpp2)
library(forecast)
library(readxl)
install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tseries)

#Data processing
df <- read_excel(file.choose())
df
start.date  <- decimal_date(ymd(min(df$Date[3])))
df_ts<-ts(df$Weekly_Sales, start = start.date,frequency = 52)
df_ts
ts.plot(df_ts)
plot(decompose(df_ts))

#training data and testing data
train.df<-slice(df, 1:104)
test.df<-slice(df,105:143)
test.date <- decimal_date(ymd("2012-02-03"))
df_train<-ts(train.df$Weekly_Sales, start = start.date,frequency = 52)
df_test<- ts(test.df$Weekly_Sales, start =test.date, frequency = 52 )
length(df_train)
length(df_test)

#Models

#hw - triple exponential 
HW1 <- HoltWinters(df_train)
forecasthw<-forecast(HW1, h =39)
autoplot(forecasthw)
#Accuracy
accuracy(forecasthw)
summary(forecasthw)

#Seasonal Arima
#ACF and PACF
Acf(df_train, lag.max = 50)
Pacf(df_train, lag.max = 50)

#test for staionarity
st<-kpss.test(df_train)
st$p.value    #staionarity 

#SArima 1
#seasonal Arima order (0,0,1), seasonal (0,1,1)
aa_m<-Arima(df_train, order = c(0,0,1), seasonal = list(order  = c(0,1,0), period  =52  ))
autoplot(forecast(aa_m))
#accuracy
accuracy(aa_m)
summary(aa_m)

#seasonal Arima 2 order (0,1,1), seasonal (0,1,1)
aa_m2<-Arima(df_train, order = c(0,1,1), seasonal = list(order  = c(0,1,1), period  =52  ))
autoplot(forecast(aa_m2))
#accuracy
accuracy(aa_m2)
summary(aa_m2)


