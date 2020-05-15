# Time Series Analysis Example 1
library(ggfortify)
library(tseries)
library(forecast)

fix(AirPassengers)
AP <- AirPassengers
class(AP)
?abline
?lm
# Mean changing, Variance increasing so not stationary
reg = lm(AP~time(AP))
plot(AP, ylab = "AirPassegers")
abline(reg = reg, col = 'blue')
# Transforming data to make stationary
?plot
?aggregate
# Trend (Upwards)
plot(aggregate(AP, FUN = mean), ylab = "Mean", main = "Trend", col = 'red')
# Trend + Seasonality + Irregular Fluctuations
# Analyse them
?boxplot
# July most busy
boxplot(AP~cycle(AP))

# Log transform
plot(log(AP))
# Log still not stationary because mean is changing
# difference it
# Works. YAY!
plot(diff(log(AP)), ylab = "Difference of log AP")


# Lets Do ARIMA model to explain the series and Forecast
?acf
acf(AP)
acf(diff(log(AP))) # q = 1 # MA
pacf(diff(log(AP))) # p = 0 # AR
# d = 1 (first difference)

?arima
fit <- arima(log(AP), c(0,1,1), seasonal = list(order = c(0,1,1)))
?predict.Arima
pred <- predict(fit, n.ahead = 10*12)
attributes(pred)
pred1 <- 2.718^pred$pred
pred1
ts.plot(AP, pred1, lty = c(1,3), log = 'y')
?ts.plot
?lty
# Test
?ts
start(AP)
end(AP)
train <- ts(AP, start = c(1949,1), end = c(1959,12), frequency = 12)
fit_testing <- arima(log(train), c(0,1,1), seasonal = list(order = c(0,1,1)))
pred_testing <- predict(fit_testing, n.ahead = 12)
pred_testing$pred
pred_testing = 2.718^pred_testing$pred
pred_testing
original_1960 <- tail(AP, 12)

# Mean Sq Err
MSE <- (mean(original_1960) - mean(pred_testing))^2/12
MSE
