wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
# ID is unique identifier and should be removed to avoid overfitting 
# ID can uniquely predict each example
wbcd <- wbcd[-1]
count_diag <- table(wbcd$diagnosis)
#Many R ML classifiers req target feature coded as factor (Diagnosis feature of interest)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                                          labels = c("Benign", "Malignant"))
str(wbcd$diagnosis)
round(prop.table(count_diag)*100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
# Scaling problem looking at the variables
# Cure is transformation - normalizing numeric data
# Function to normalize. Takes a vector x of numeric values and normalize
normalize <- function(x)
{
  return ((x - min(x)) / (max(x) - min(x)))
}
# Checking function
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
?lapply
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)
# Divide data into train and test datasets
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
install.packages("class")
library(class)
?knn
#root of 469 observations so k = 21
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k =21)


# PERFORMANCE EVALUATION
library(gmodels)
?CrossTable
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
# Transform using z-score standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k =21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)
