####### Question 12

# Function
power <- function() {print(2^3)}
power()

power2 <- function(x, a) {print(x^a)}
power2(3,8)

power3 <- function(x, a) {return(x^a)}

x <- 1:10
plot(x, power3(x, 2), main = "log of x^2 vs lof of x", log = "xy", xlab = "log of x", ylab = "log of y = x^2")

plotPower <- function(x, a) {plot(x, power3(x, a)) } 
plotPower(1:10, 3)

