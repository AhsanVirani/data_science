# By Ahsan Muhammad
# Modified 15/2/20
# Learning R basics

usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)
summary(usedcars[c("price", "mileage")])
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.2))
boxplot(usedcars$price, main ="Boxplot of Used Car Prices",
        ylab = "price ($)")
boxplot(usedcars$mileage, main ="Boxplot of Used Car Mileage",
        ylab = "Odometer (mi.)")
IQR(usedcars$price)
10995-1.5*3905
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")
SD = sd(usedcars$price)
MEAN = mean(usedcars$price)
MEAN - SD
MEAN + SD
str(usedcars)
unique(usedcars$transmission)
table(usedcars$year)
model_table <- table(usedcars$model)
prop.table(model_table)
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)
mode(usedcars$color)

#Two way relation
plot(x = usedcars$mileage, y = usedcars$price,
      main = "Scatterplot of Price vs Mileage",
      xlab = "Used Car Odometer (mi.)",
      ylab = "Used Car Price ($)")
install.packages("gmodels")
library(gmodels)
usedcars$conservative <- 
  usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)
CrossTable(x = usedcars$model, y = usedcars$conservative, chisq = TRUE)
