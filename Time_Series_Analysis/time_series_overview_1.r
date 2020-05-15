library("astsa")
# Overview of the Time Series Graphs

?plot
# J&J stock 
plot(jj, type='o', ylab="Quarterly Earnings per Share")
# Global warning hypothesis 
plot(gtemp, type="o", ylab="Global Temperature Deviations")
# Speech Recog
plot(speech)
# NYSE returns
plot(nyse, ylab="NYSE Returns")
# Two series together
par(mfrow = c(1,1)) # set up the graphics
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")
# Earthquakes and Explosions
par(mfrow=c(2,1))
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion")
