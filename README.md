# SPY Tracker


# Install required packages

install.packages(c("quantmod", "tseries", "timeSeries", "forecast", "xts"))

# Load necessary libraries

library(quantmod)

library(tseries)

library(timeSeries)

library(forecast)

library(xts)

# Pull 28 years worth of SPY data from Yahoo Finance
getSymbols('SPY', from = '1995-01-01', to = '2023-03-23')

# Check the class of the SPY data
class(SPY)

# Plot the SPY closing prices
plot(SPY$SPY.Close)

# ACF and PACF plots
par(mfrow=c(1,2))

Acf(SPY$SPY.Close, main='ACF for Differenced Series')

Pacf(SPY$SPY.Close, main= 'PACF for Difference Series')

![Rplot03](https://github.com/dylanpriceginno/SPYTracker/assets/85695465/5a77bf95-e214-4509-a3f1-8cdcfd6563ce)


# Test for stationarity (Augmented Dickey-Fuller Test)

adf_result <- adf.test(SPY$SPY.Close)

print(adf_result) # p-value = 0.8506, lag order = 19

# Auto ARIMA model selection

arima_model <- auto.arima(SPY$SPY.Close, seasonal = FALSE)

tsdisplay(residuals(arima_model), lag.max=40, main='ARIMA Model Residuals')

auto.arima(SPY$SPY.Close, seasonal = FALSE) # AICc=31995.59, BIC=32016.19

# Fit ARIMA models with different orders

# fitA (auto ARIMA)
fitA <- arima(SPY$SPY.Close, order=c(1,1,0))

tsdisplay(residuals(fitA), lag.max=40, main='(1,1,0) Model Residuals')

# fitB (manual ARIMA)
fitB <- arima(SPY$SPY.Close, order=c(1,1,19))

tsdisplay(residuals(fitB), lag.max=40, main='(1,1,19) Model Residuals')

# fitC (different d-value), 2 seasonality
fitC <- arima(SPY$SPY.Close, order=c(1,2,38))

tsdisplay(residuals(fitC), lag.max=40, main='(1,2,38) Model Residuals')

# fitD (standard ARIMA)
fitD <- arima(SPY$SPY.Close, order = c(1,1,1))

tsdisplay(residuals(fitD), lag.max=40, main = '(1,1,1) Model Residuals')

# Reset plot layout
par(mfrow=c(2,2))

# 100-day forecast
term <- 100

fcast1 <- forecast(fitA, h=term) 

fcast2 <- forecast(fitB, h=term)

fcast3 <- forecast(fitC, h=term)

fcast4 <- forecast(fitD, h=term)

# Plot 100-day forecasts
plot(fcast1)

plot(fcast2)

plot(fcast3)

plot(fcast4)

# Calculate and display accuracy (MAPE) for the 100-day forecasts
accuracy(fcast1) # 99.18%

accuracy(fcast2) # 99.18%

accuracy(fcast3) # 99.17%

accuracy(fcast4) # 99.18%


# Results: Potential drop off towards the end of the year, mostly flat. Avoid investment currently

![Rplot](https://github.com/dylanpriceginno/SPYTracker/assets/85695465/af060418-b6c8-4a86-83d3-afd96755dcd5)

# 365-day forecast

term2 <- 365

fcast5 <- forecast(fitA, h=term2) 

fcast6 <- forecast(fitB, h=term2)

fcast7 <- forecast(fitC, h=term2)

fcast8 <- forecast(fitD, h=term2)

# Plot 365-day forecasts

plot(fcast5)

plot(fcast6)

plot(fcast7)

plot(fcast8)

# Results: Expect a slight constant increase over the next year
![Rplot02](https://github.com/dylanpriceginno/SPYTracker/assets/85695465/3016375f-0974-4f5f-9b49-56d19bcb4c57)
