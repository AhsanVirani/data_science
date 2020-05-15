## Analysing Stock Market Data using R
# Using Quantmod package -> For Quantitative financial modelling
install.packages("quantmod")
library(quantmod)
?quantmod

?getSymbols
# Load and Manage Data from Multiple Sources
getSymbols('^GSPC', src='yahoo')
dim(GSPC)
head(GSPC)
tail(GSPC)
class(GSPC)
# Time series data
summary(GSPC)

# loading YELP, AMAZON AND APPLE DATA
basket_symbols <- c('YELP', 'AMZN', 'AAPL')
getSymbols(basket_symbols, src='yahoo')
# Merge all 3 time series data into 1 data frame
?merge
basket <- data.frame(as.xts(merge(YELP, AMZN, AAPL)))
class(basket)
tail(basket)
head(basket)
# YELP founded later. AMZN and AAPL already there since start

## Graph Plotting with quantmod

plot(GSPC)
names(GSPC)
?lineChart
lineChart(GSPC, line.type = 'h', theme = 'white')
barChart(GSPC, bar.type = 'hlc', TA = NULL)
candleChart(GSPC, TA = NULL, subset = '2015')
# From 2015-01 to all the way
candleChart(GSPC, TA = NULL, subset = '2015-01::')
# From 2015-01 to March
candleChart(GSPC, subset = '2015-01::2015-03')
# Changing Themes
?candleChart
candleChart(GSPC, theme = 'white', subset = '2015-01::2015-03')
?chartTheme
candleChart(GSPC, theme = chartTheme(theme = 'white', up.col='green',dn.col='darkred'),
            subset = '2015-01::2015-03')
chartSeries(GSPC,
            type = c('matchsticks'), subset = '2015-01')            

######### ADDING INDICATORS TO OUR QUANTMOD CHARTS
## INDICATORS IN TTR PACKAGE
install.packages("TTR")
library(TTR)
?TTR
chartSeries(GSPC, TA = "addVo();addBBands();addCCI()",theme='white', subset = '2015')
addSMA(n=200)
addROC(n=200)
?EMA
GSPC.EMA.20 <- EMA(GSPC$GSPC.Close, n=20)
GSPC.EMA.100 <- EMA(GSPC$GSPC.Close, n=100)
head(GSPC.EMA.20, 25)
tail(GSPC.EMA.20)
?addTA
chartSeries(GSPC, theme = chartTheme('white'), up.col = 'black', dn.col = 'black', subset = '2015')
addTA(GSPC.EMA.20, on=1, col='red')
addTA(GSPC.EMA.100, on=1, col='blue')
addTA(GSPC.EMA.20 - GSPC.EMA.100, col='blue', type = 'h', legend = "20-100 MA")






################## Create simple MA from Scratch ####################
getSymbols('QQQ', src='yahoo')
plot(QQQ$QQQ.Close)
period <- 100
price_vector <- QQQ$QQQ.Close
length(price_vector)
moving_average_vector <- rep(NA, period)
## Look to traverse the vector and assign MA values
for (ind in seq((period+1),length(price_vector))) {
  moving_average_vector <- 
    c(moving_average_vector, mean(price_vector[(ind-period):ind]))
}

par(mfrow=c(1,1))
plot(QQQ$QQQ.Close)
plot(moving_average_vector, type='l', col='red', lwd=3,
     main=paste('SMA', period))
length(moving_average_vector)
length(price_vector)
head(moving_average_vector)
QQQ$QQQ.Close.SMA <- moving_average_vector
plot(QQQ$QQQ.Close)
lines(QQQ$QQQ.Close.SMA, type='l', col='red', lwd=3)

getSymbols(c('QQQ'), src='yahoo')
chartSeries(QQQ, theme = 'white', TA="addSMA(100)")

getSymbols(c('^IBEX'), src='yahoo')
chartSeries(IBEX, theme = 'white', TA="addSMA(100)")
IBEX <- na.exclude(IBEX)

#### Adding 2 moving avg - slow and fast
chartSeries(IBEX$IBEX.Close, theme = "white",
            TA="addEMA(n=50, col='black');addEMA(n=200, col='blue')")

chartSeries(GSPC$GSPC.Close, theme = "white",
            TA="addEMA(n=50, col='black');addEMA(n=200, col='blue')")

library(TTR)
GSPC.EMA.50 <- EMA(GSPC$GSPC.Close, n=50)
GSPC.EMA.200 <- EMA(GSPC$GSPC.Close, n=200)
addTA(GSPC.EMA.50 - GSPC.EMA.200,col='blue', type='h', legend = '50-200 EMA')



chartSeries(IBEX$IBEX.Close, theme = "white",
            TA="addEMA(n=50, col='black');addEMA(n=200, col='blue')")
IBEX.EMA.50 <- EMA(IBEX$IBEX.Close, n=50)
IBEX.EMA.200 <- EMA(IBEX$IBEX.Close, n=200)
addTA(IBEX.EMA.50 - IBEX.EMA.200,col='blue', type='h', legend = '50-200 EMA')


chartSeries(IBEX$IBEX.Close, theme = "white",
            TA="addEMA(n=50, col='black');addEMA(n=200, col='blue')")
IBEX.EMA.10 <- EMA(IBEX$IBEX.Close, n=10)
fast.diff <- IBEX.EMA.10 - IBEX.EMA.50
slow.diff <- IBEX.EMA.50 - IBEX.EMA.200
addTA(fast.diff,col='blue', type='h', legend = '10-50 EMA')
addTA(slow.diff,col='red', type='h', legend = '50-200 EMA')



chartSeries(GSPC$GSPC.Close, theme = "white",
            TA="addEMA(n=50, col='black');addEMA(n=200, col='blue')")
GSPC.EMA.10 <- EMA(GSPC$GSPC.Close, n=10)
fast.diff <- GSPC.EMA.10 - GSPC.EMA.50
slow.diff <- GSPC.EMA.50 - GSPC.EMA.200
addTA(fast.diff,col='blue', type='h', legend = '10-50 EMA')
addTA(slow.diff,col='red', type='h', legend = '50-200 EMA')

### You can only enter in the direction of the red slow.diff indicator, if its 
##above 0 you can take long signals, if its below 0, you can take short signals. The fast.diff indicator dictates entries.
## When the blue line goes from neg to pos, its a long trade ( and the slower red slow.diff indicator is above 0)
## Same thing for shorts. This is also called moving average crossover trading system.
### RULES DESRCIBED IN DESCRIPTION.
## TREND FOLLOWING SYSTEM

### USE PACKAGE binhf. VERY USEFUL for lagging indicators
install.packages("binhf")
library(binhf)
?shift
## Shift use to shift the vector position
tail(as.numeric(fast.diff))
tail(shift(v=as.numeric(fast.diff), places = 1, dir = "right"))

## Long trades
?ifelse
long_trades <- ifelse(
                  slow.diff > 0 &
                  fast.diff > 0 &
                  shift(v=as.numeric(fast.diff), places = 1, dir = "right") < 0,
                  IBEX$IBEX.Close, NA
                    )
     
## Short trades
short_trades <- ifelse(
  slow.diff < 0 &
    fast.diff < 0 &
    shift(v=as.numeric(fast.diff), places = 1, dir = "right") > 0,
  IBEX$IBEX.Close, NA
)

chartSeries(IBEX$IBEX.Close, name = "IBEX Trend Trading", theme = "white", up.col="black")
plot(IBEX$IBEX.Close)
points(long_trades, col="blue", cex=1.5, pch=18)
points(short_trades, col="red", cex=1.5, pch=18)




#### SAME THING FOR GSPC

## Long trades
?ifelse
long_GSPC_trades <- ifelse(
  slow.diff > 0 &
    fast.diff > 0 &
    shift(v=as.numeric(fast.diff), places = 1, dir = "right") < 0,
  GSPC$GSPC.Close, NA
)

## Short trades
short_GSPC_trades <- ifelse(
  slow.diff < 0 &
    fast.diff < 0 &
    shift(v=as.numeric(fast.diff), places = 1, dir = "right") > 0,
  GSPC$GSPC.Close, NA
)

?plot
plot(GSPC$GSPC.Close)
points(long_GSPC_trades, col="blue", cex=1.5, pch=18)
points(short_GSPC_trades, col="red", cex=1.5, pch=18)




############################## TWEAKING THE MODEL ####################################
### CHANGE fast.diff / slow.diff and try
IBEX.EMA.20 <- EMA(IBEX$IBEX.Close, n=20)
IBEX.EMA.100 <- EMA(IBEX$IBEX.Close, n=100)
IBEX.EMA.300 <- EMA(IBEX$IBEX.Close, n=300)

fast.diff <- IBEX.EMA.20 - IBEX.EMA.100
slow.diff <- IBEX.EMA.100 - IBEX.EMA.300

## Long trades

long_trades <- ifelse(
                  slow.diff > shift(v=as.numeric(fast.diff), places = 1, dir = "right")  &
                  fast.diff > 0 &
                  shift(v=as.numeric(fast.diff), places = 1, dir = "right") < 0,
                IBEX$IBEX.Close, NA
                )

## Short trades
short_trades <- ifelse(
                  slow.diff < shift(v=as.numeric(fast.diff), places = 1, dir = "right") &
                  fast.diff < 0 &
                  shift(v=as.numeric(fast.diff), places = 1, dir = "right") > 0,
                IBEX$IBEX.Close, NA
                )

plot(IBEX$IBEX.Close)
points(long_trades, col="blue", cex=1.5, pch=18)
points(short_trades, col="red", cex=1.5, pch=18)

##### REPEATING SAME FOR GSPC 
GSPC.EMA.300 <- EMA(GSPC$GSPC.Close, n=300)

fast.diff <- GSPC.EMA.20 - GSPC.EMA.100
slow.diff <- GSPC.EMA.100 - GSPC.EMA.300

## Long trades

long_trades <- ifelse(
  slow.diff > shift(v=as.numeric(fast.diff), places = 1, dir = "right")  &
    fast.diff > 0 &
    shift(v=as.numeric(fast.diff), places = 1, dir = "right") < 0,
  GSPC$GSPC.Close, NA
)

## Short trades
short_trades <- ifelse(
  slow.diff < shift(v=as.numeric(fast.diff), places = 1, dir = "right") &
    fast.diff < 0 &
    shift(v=as.numeric(fast.diff), places = 1, dir = "right") > 0,
  GSPC$GSPC.Close, NA
)

plot(GSPC$GSPC.Close)
points(long_trades, col="blue", cex=1.5, pch=18)
points(short_trades, col="red", cex=1.5, pch=18)
########### TWEAK DOESNT WORK HAHAHA ###################################
#### GO BACK TO OLD MODEL
#### FUCK ME
##### OLD CHART WITH A 20-100 fast and 100-300 slow diff WOULD WORK BETTER


########################### INSIGHTS FROM COMMON INDICATORS _ ADX &VWAP

chartSeries(IBEX, theme = "white",
            TA = "addADX(n=14, maType = 'EMA', wilder=TRUE)", subset = '2013::2015')


chartSeries(GSPC, theme = "white",
            TA = "addADX(n=14, maType = 'EMA', wilder=TRUE)", subset = '2013::2015')

?VWAP
### Adding indicators to GSPC
VWAP.slow <- VWAP(price=GSPC$GSPC.Close, volume = GSPC$GSPC.Volume, n=100)
VWAP.fast <- VWAP(price=GSPC$GSPC.Close, volume = GSPC$GSPC.Volume, n=20)
VWAP.diff <- VWAP.fast- VWAP.slow
chartSeries(GSPC, theme = "white", TA = "addVo();addTA(VWAP.slow, on=1, col='red');addTA(VWAP.fast, on=1, col='blue');addTA(VWAP.diff, col='blue')")

## Adding indicators to IBEX
VWAP.slow <- VWAP(price=IBEX$IBEX.Close, volume = IBEX$IBEX.Volume, n=100)
VWAP.fast <- VWAP(price=IBEX$IBEX.Close, volume = IBEX$IBEX.Volume, n=20)
VWAP.diff <- VWAP.fast- VWAP.slow
chartSeries(IBEX, theme = "white", TA = "addVo();addTA(VWAP.slow, on=1, col='red');addTA(VWAP.fast, on=1, col='blue');addTA(VWAP.diff, col='blue')")

##### NEW TRADING SYSTEM ON ADX
?ADX
ADX.20 <- ADX(IBEX, n=14)

# Looking for long entries
long_trades <- ifelse(
                  (ADX.20$ADX > 30 & ADX.20$ADX < 45) &
                  VWAP.diff > 300, IBEX$IBEX.Close, NA
                )

short_trades <- ifelse(
                  (ADX.20$ADX > 30 & ADX.20$ADX < 45) &
                  VWAP.diff < -300, IBEX$IBEX.Close, NA
                )

plot(IBEX$IBEX.Close)
points(long_trades, col="blue", cex=1, pch=18)
points(short_trades, col="red", cex=1, pch=18)
       
######## SASME ON GSPC   
ADX.20 <- ADX(GSPC, n=14)

long_trades <- ifelse(
  (ADX.20$ADX > 30 & ADX.20$ADX < 45) &
    VWAP.diff > 50, GSPC$GSPC.Close, NA
)

short_trades <- ifelse(
  (ADX.20$ADX > 30 & ADX.20$ADX < 45) &
    VWAP.diff < -50, GSPC$GSPC.Close, NA
)

plot(GSPC$GSPC.Close)
points(long_trades, col="blue", cex=1, pch=18)
points(short_trades, col="red", cex=1, pch=18)


################## COUNTER TREND SYSTEM - ROC, RSI, CCI, CHAIKIN VOLITILITY
chartSeries(IBEX, theme="white", TA="addRSI(n=100);addCCI(n=100);addROC(n=100)")
chartSeries(GSPC, theme="white", TA="addRSI(n=100);addCCI(n=100);addROC(n=100)")

#### A NEW SYSTEM BASED ON CCI
chartSeries(IBEX, theme="white", TA="addCCI(n=100);addEMA(n=50, col='blue');addEMA(n=200, col='red')")
slow.diff <- IBEX.EMA.50 - IBEX.EMA.200
?CCI
CCI.IND <- CCI(HLC = IBEX[,"IBEX.High",IBEX$IBEX.Low, IBEX$IBEX.Close,
                          n = 100])

## LONG TRADES
long_trades <- ifelse(
                  shift(v=as.numeric(CCI.IND), places = 1, dir = "right") > CCI.IND &
                  CCI.IND < 100 &
                  slow.diff > 0, IBEX$IBEX.Close, NA
                )
short_trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places = 1, dir = "right") < CCI.IND &
    CCI.IND > -100 &
    slow.diff < 0, IBEX$IBEX.Close, NA
)

plot(IBEX$IBEX.Close)
points(long_trades, col="blue", cex=1, pch=18)
points(short_trades, col="red", cex=1, pch=18)

### WAY OVERTRADING
# Lets look at volatility
chartSeries(IBEX, theme="white", TA="addCCI(n=100);addEMA(n=50, col='blue');addEMA(n=200, col='red');addChVol(n=100)")

slow.diff <- IBEX.EMA.50 - IBEX.EMA.200
?CCI
CCI.IND <- CCI(HLC = IBEX[,"IBEX.High",IBEX$IBEX.Low, IBEX$IBEX.Close,
                          n = 100])
CV.IND <- chaikinVolatility(HL=IBEX[,c("IBEX.High", "IBEX.Low")], n = 100)

## LONG TRADES
long_trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places = 1, dir = "right") > CCI.IND &
    CCI.IND < 100 &
    CV.IND < 0    &
    slow.diff > 0, IBEX$IBEX.Close, NA
)
short_trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places = 1, dir = "right") < CCI.IND &
    CCI.IND > -100 &
    CV.IND < 0     &
    slow.diff < 0, IBEX$IBEX.Close, NA
)

plot(IBEX$IBEX.Close)
points(long_trades, col="blue", cex=1, pch=18)
points(short_trades, col="red", cex=1, pch=18)

## Still a mess. Lets try RSI
chartSeries(IBEX, theme="white", TA="addRSI(n=100);addChVol(n=100)")
chartSeries(IBEX, theme="white", TA="addRSI(n=30);addChVol(n=100)")
RSI.fast <- RSI(price = IBEX$IBEX.Close, n =10)
RSI.slow <- RSI(price = IBEX$IBEX.Close, n =30)
RSI.diff <- RSI.fast - RSI.slow
addTA(RSI.diff, col='blue', type='h', legend = "RSI DIFF")

RSI.IND <- RSI(price = IBEX$IBEX.Close, n=30)

## LONG TRADES
long_trades <- ifelse(
                  RSI.diff <0 &
                  shift(v=as.numeric(RSI.diff), places = 1, dir = "right") > RSI.diff &
                    CCI.IND < 100 &
                    CV.IND < -0.1    &
                    slow.diff > 0, IBEX$IBEX.Close, NA
)
short_trades <- ifelse(
                  RSI.diff > 0 &
                  shift(v=as.numeric(RSI.diff), places = 1, dir = "right") < RSI.diff &
                    CCI.IND > -100 &
                    CV.IND < 0     &
                    slow.diff < 0, IBEX$IBEX.Close, NA
)

plot(IBEX$IBEX.Close, main = "RSI")
points(long_trades, col="blue", cex=1, pch=18)
points(short_trades, col="red", cex=1, pch=18)
