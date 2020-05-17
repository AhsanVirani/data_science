# https://otexts.com/fpp2/tspatterns.html
rm(list = ls())
install.packages("fpp2")
library(fpp2)
data1 = read.csv("C:/Users/asan/Desktop/RSXFSN.csv")
# Convert to time series
class(data)
?ts
data <- ts(data=data1[,2], start = c(1992,1), frequency = 12)

# Time plot
?autoplot
autoplot(data, ylab = "Sales")

# Strong trend in data
# Take the first difference of data
autoplot(diff(data), ylab = "Sales")
# Trend removed now, series looks trend-stationary. Look seasonality now
?ggseasonplot
ggseasonplot(diff(data))
# Subseries season ploy
ggsubseriesplot(diff(data))
# Seasonal Naive method as benchmark
# Trend gone seasonality there so need to consider
?snaive
fit <- snaive(diff(data))
print(summary(fit))
# Residual sd: 9075.8456
checkresiduals(fit)
# ETS model. Exponential smoothing model
fit.ets <- ets(data)
print(summary(fit.ets))
checkresiduals(fit.ets)
