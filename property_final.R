## R project: Time Series Forecasting of Property Sales in England and Wales
rm(list=ls())
library(forecast)

# Loading data
data <- read.csv("propertysales.csv")
summary(data)
head(data)

# Creating time series object
psts <- ts(data$Total_Sales, start = c(1995,1), end = c(2016,11), frequency = 12)
psts
length(psts)
plot(psts)

# Creating training and test dataset
train <- subset(psts, end = length(psts)-20)
test <- tail(psts,20)

# Decomposing the time series data
plot(decompose(psts)) 

# Using Simple Forecasting Methods 
fcnaive <- naive(train, h = 20) # Naive forecast
fcsnaive <- snaive(train, h = 20) # Seasonal Naive forecast
fcmean <- meanf(train, h = 20) # Mean forecast
fcdrift <- rwf(train, h = 20, drift = TRUE) # Drift forecast

# Plotting Simple Forecasting Methods 
autoplot(train) + autolayer(fcnaive$mean) + autolayer(fcsnaive$mean) +
  autolayer(fcdrift$mean) + autolayer(fcmean$mean)

# Generating performance metrics
accuracy(fcnaive,psts)
accuracy(fcsnaive,psts)
accuracy(fcmean,psts)
accuracy(fcdrift,test)

# Performing Exponential Smoothing
fcses <- ses(train, h = 20) # Simple Exponential Smoothing
fcholt <- holt(train, h = 20) # Holt Linear Method
fchw <- hw(train, h = 20) # Additive Holt Winters Method
fchwm <- hw(train, h = 20, seasonal = "multiplicative") # Multiplicative Holt Winters Method

# Plotting Exponential Smoothing Methods 
autoplot(train) + autolayer(fcses$mean) + autolayer(fcholt$mean) +
  autolayer(fchw$mean) + autolayer(fchwm$mean) 

# Generating performance metrics
accuracy(fcses,psts)
accuracy(fcholt,psts)
accuracy(fchw,psts)
accuracy(fchwm,psts)

# Performing Auto ARIMA

fcauto <- auto.arima(train)
fcauto

# Checking Residuals
checkresiduals(fcauto)

# Generating performance metrics
accuracy(forecast(fcauto),psts)

# Plotting Auto ARIMA
fcauto_plot <- forecast(fcauto)
autoplot(psts) + autolayer(fcauto_plot$mean)

# Generating forecast for next year ie. till Nov 2017
predict_nov2017 <- forecast(fcauto, 32)
predict_nov2017
autoplot(psts) + autolayer(predict_nov2017$mean)
