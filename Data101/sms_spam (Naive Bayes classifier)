sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
#Text mining package
install.packages("tm")
library(tm)
# Making collection of text documents for each message
sms_corpus <- Corpus(VectorSource(sms_raw$text))
?Corpus
print(sms_corpus)
inspect(sms_corpus[1:3])
#Convert all the SMS to lowercase and remove any numbers
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
?tm_map
inspect(corpus_clean[1:3])
# Removing stopwords and punctuations
stopwords()
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
# Remove additional white spaces
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[1:3])
# Takes a corpus and create a data structure called sparse matrix -> tokenize
sms_dtm <- DocumentTermMatrix(corpus_clean)
#Splitting dataframe for train and test for raw, dtm and corpus
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

# Check subsets are representative of a complete set of SMS data
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
install.packages("wordcloud")
library(wordcloud)
?wordcloud
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)
# create subset of spam and ham
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
findFreqTerms(sms_dtm_train, 5)
install.packages("dictionary")
# The words with freq 5 will only go onto document term matrix
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train,
                                list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,
                               list(dictionary = sms_dict))
# Function to make categorical variables in the Document Matrix for NVT to work
convert_counts <- function(x){
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}
?apply
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

# Training a model on the data
# Naive Bayes implementation package
install.packages("e1071")
library(e1071)
?naiveBayes
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier[1,1]

# Evaluating model perf
sms_test_pred <- predict(sms_classifier, sms_test)
?predict
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
# Improving model performance
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
