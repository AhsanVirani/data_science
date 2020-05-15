# R Basics
# Concatenate
x <- c(1,6,2)
y <- c(1,4,3)
length(x)
length(y)
x+y
ls()
rm(x,y)
ls()

# Remove all at once
rm(list=ls())
?matrix
x = matrix(data = c(1,2,3,4), nrow=2, ncol=2)
x = matrix(data = c(1,2,3,4), nrow=2, ncol=2, byrow = TRUE)
sqrt(x)
x^2

# Generate vector of random normal variables
?rnorm
x = rnorm(50)
x
y = x+rnorm(50,mean=50,sd=.1)
cor(x,y)

# Reproduce exact same set of random numbers
set.seed(1303)
rnorm(50)
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Indexing Data
A=matrix(1:16,4,4)
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[,1:2]
A[1:2,]
A[1,]
A[-c(1,3),]
dim(A)

#Loading Data
Auto = read.table("Auto.data")
fix(Auto)
?read.table
Auto=read.table("Auto.data", header = T, na.strings = "?")
fix(Auto)
Auto=read.csv("Auto.csv", header = T, na.strings = "?")
fix(Auto)
dim(Auto)
Auto[1:4,]
names(Auto)
