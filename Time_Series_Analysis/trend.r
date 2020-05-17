data = nottem
str(nottem)
?stl
# perform classical decomposition
decomp <- stl(data, s.window = "periodic")
attributes(decomp)
plot(decomp)
plot(stl(data, "per"))

######################
# Example 2
######################

library(fpp2)
library(forecast)
data <- ts(ausbeer, start = 1956, end = 1976, frequency = 4)
?ts
plot(data)
plot(log(data))
# create moving average that will be close to trend
?ma
data.trend = ma(data, order = 4, centre = TRUE)
plot(data)
lines(data.trend, col = "red")

# Remove trend
data.detrend = data - data.trend
plot(data.detrend)  # Much easier to predict


#############
# Adding trend to Linear Regression
#############
trend <- seq(1:length(data))
