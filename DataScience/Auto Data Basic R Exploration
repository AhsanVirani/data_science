Auto = read.csv("Auto.csv")
summary(Auto)
Auto$horsepower <- gsub("?",NA,Auto$horsepower, fixed = TRUE)
Auto = na.omit(Auto)
rownames(Auto) <- NULL
Auto$cylinders <- as.factor(Auto$cylinders)
Auto$origin <- as.factor(Auto$origin)
Auto$name <- as.factor(Auto$name)
Auto$horsepower <- as.integer(Auto$horsepower)

# Qualitative variables are 1)cylinder 2)years 3)origin
qualitative_columns = c(2,8,9)
sapply(Auto[,-qualitative_columns], range)

# Mean and SD of each quantitative predictor
sapply(Auto[,-qualitative_columns], mean)
sapply(Auto[,-qualitative_columns], sd, na.rm=TRUE)
str(Auto)

# remove 10th thr 85th observations
sapply(Auto[-seq(from=10, to=85, by=1),-qualitative_columns], range)
sapply(Auto[-seq(from=10, to=85, by=1),-qualitative_columns], mean)
sapply(Auto[-seq(from=10, to=85, by=1),-qualitative_columns], sd)

pairs( Auto[,-qualitative_columns] )
plot( as.factor(Auto$cylinders), Auto$mpg )
plot( as.factor(Auto$origin), Auto$mpg ) 
