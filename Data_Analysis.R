library(readxl)
library(stats)
library(ggplot2)

# Collect Data
data<- read_excel("ch3_UnemploymentRate.xls")

#plotting the data
plot(data, type = 'l', main = 'Unemployment Rate')

#creating time series
unrate <- ts(data$UNRATE, frequency = 12, start = 1948)

#removing trend and seasonality
unrate_d12 <- diff(unrate, differences = 12)
plot(unrate_d12)

#create acf and pacf 12m
acf(unrate_d12)
pacf(unrate_d12)

#create acf and pacf
acf(unrate)
pacf(unrate)

#creating linear MA model
MA <- arima(unrate, order = c(0,0,1))
print(MA)

#creating linear AR model
AR <- arima(unrate, order = c(1,0,0))
print(AR)

#creating linear ARMA model
ARMA <- arima(unrate, order = c(1,0,1))
print(ARMA)

#create acf plot with lag 20
acf(unrate, lag.max = 20, main = 'ACF PLOT')

# create time series data
ts_data <- unrate
ggtsdisplay(unrate, main = "ACF and PACF Plots")

# box test
Box.test(unrate, type='Ljung-Box')
