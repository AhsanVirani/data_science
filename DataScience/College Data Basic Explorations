
college = read.csv("college.csv")
fix(college)
# changing row names
rownames(college)=college[,1]
# Removing first column from the data set
college=college[,-1]
fix(college)
summary(college)
?pairs  
pairs(college[,1:10])
plot(college$Private, college$Outstate, xlab="Private", ylab="Outstate")
?rep
# Making a new character vector of size of #of rows in college with No on every row
Elite=rep("No", nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab="Elite", ylab="Outstate")

#Making Histogram
par("mar")
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
hist(college$Outstate, col="green", xlab="Outstate", ylab="Frequency")


## Some interesting observations
# Find position. what is the university with the most students in the top 10% of class
college[which.max(college$Top10perc),]
acceptance_rate = college$Accept/college$Apps
# College with min acceptance rate
college[which.min(acceptance_rate),]
# college with max acceptance rate
college[which.max(acceptance_rate),]


