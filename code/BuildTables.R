### Thomas Hepner
### 4/1/2016
### Prediction Algorithm

### Set Work Directory
rm(list = ls())  
setwd("C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/Coursera-SwiftKey/final/en_US/")
set.seed(1)

### Load Text Prediction Packages
library(quanteda)
library(Matrix)
library(data.table)
library(stringr)

### Create Corpuses
us_dir = "C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/Coursera-SwiftKey/final/en_US/samples/"
project_dir = "C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/Coursera-SwiftKey/final/en_US/tokenized_data/"

### Create blogs sample with some twitter data
numLines = 1000 # 300,000 ; 100,000
blogs = readLines('en_US.blogs.txt', encoding = 'UTF-8', n = 300000, skipNul= TRUE) # 899,288 lines, # 248.5 Mb
twitter = readLines('en_US.twitter.txt', encoding = 'UTF-8', n = 100000, skipNul= TRUE) #  2,360,148 lines, # 301.4 Mb

blogs = paste(blogs) 
twitter = paste(twitter)

blogs = sample(blogs, size = length(blogs), replace = FALSE)
twitter = sample(twitter, size = length(twitter), replace = FALSE)

### Create data sets
n = length(blogs)
blogs_train = blogs[1:(0.9*n)]
blogs_devtest = blogs[(0.9*(n+1)):(0.95*n)]
blogs_test = blogs[(0.95*(n+1)):n]
rm(blogs)

n = length(twitter)
twitter_train = twitter[1:(0.9*n)]
twitter_devtest = twitter[(0.9*(n+1)):(0.95*n)]
twitter_test = twitter[(0.95*(n+1)):n]

combined_train = c(blogs_train, twitter_train)
combined_devtest= c(blogs_devtest, twitter_devtest)
combined_test = c(blogs_test, twitter_test)
rm(blogs_train, twitter_train, blogs_devtest, twitter_devtest, blogs_test, twitter_test)

## Write data sets to files in directory
writeLines(combined_train, paste(us_dir, "combined_train.txt", sep = ""))
writeLines(combined_devtest, paste(us_dir, "combined_devtest.txt", sep = ""))
writeLines(combined_test, paste(us_dir, "combined_test.txt", sep = ""))

### Clean, Transform, and Tokenize Data Sets
## 1-Gram, 2-Gram, 3-Gram, 4-Gram, 5-Gram for Training Data
## 1-Gram for devtest and test data
ptm = proc.time()
OneGramTrain = toLower(tokenize(combined_train, what = "word", removeNumbers = TRUE , removePunct = TRUE
                                , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                                , ngrams = 1, concatenator= " "
                                , simplify = TRUE))
x = proc.time() - ptm
print(x)

## 2-Gram
TwoGramTrain = toLower(tokenize(combined_train, what = "word", removeNumbers = TRUE, removePunct = TRUE
                                , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                                , ngrams = 2, concatenator= " "
                                , simplify = TRUE))

## 3-Gram
ThreeGramTrain = toLower(tokenize(combined_train, what = "word", removeNumbers = TRUE, removePunct = TRUE
                                  , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                                  , ngrams = 3, concatenator = " "
                                  , simplify = TRUE))

## 4-Gram
FourGramTrain = toLower(tokenize(combined_train, what = "word", removeNumbers = TRUE, removePunct = TRUE
                                 , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                                 , ngrams = 4, concatenator = " "
                                 , simplify = TRUE))

## 5-Gram
ptm = proc.time()
FiveGramTrain = toLower(tokenize(combined_train, what = "word", removeNumbers = TRUE, removePunct = TRUE
                                 , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                                 , ngrams = 5, concatenator = " "
                                 , simplify = TRUE))
x = proc.time() - ptm
print(x)

## 1-Gram Devtest Data
OneGramDevTest = toLower(tokenize(combined_devtest, what = "word", removeNumbers = TRUE, removePunct = TRUE
                                  , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                                  , ngrams = 1, concatenator = " "
                                  , simplify = TRUE))

## 1-Gram Test Data
OneGramTest = toLower(tokenize(combined_test, what = "word", removeNumbers = TRUE, removePunct = TRUE
                               , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                               , ngrams = 1, concatenator = " "
                               , simplify = TRUE))

### Save Tokenized Data as R-Vectors
project_dir =  "C:/Users/thep3/OneDrive/Documents/Coursera/Data Science Capstone/Coursera-SwiftKey/final/en_US/tokenized_data/"

### Helper function to apply Good-Turing transformation to term frequency vectors
goodTuringTransformation = function(terms) {
  ## discount frequency of terms
  ## k = 5 and counts of 1 replaced with 0
  terms = terms[which(terms != 1)]
  terms[which(terms == 2)] = 2 * (2/3)
  terms[which(terms == 3)] = 3 * (3/4)
  terms[which(terms == 4)] = 4 * (4/5)
  terms[which(terms == 5)] = 5 * (5/6)
  return(terms)
}

### Helper function to create data tables
createDataTable = function(tokenized_data, n) {
  freqTable = sort(table(tokenized_data), decreasing = TRUE)
  freqTable = goodTuringTransformation(freqTable)
  ngram = NULL
  for(i in -n:-1) {
    ngram = cbind(ngram, word(names(freqTable), i))
  }
  freqs = as.vector(freqTable)
  ngram = cbind(ngram, freqs)
  ngram = data.frame(ngram)
  names(ngram) = c(c("word1", "word2", "word3", "word4", "word5")[1:n], "freq")
  ngram = ngram[which(ngram$freq != 1),]
  rm(tokenized_data)
  return(ngram)
}

# createDataTable2 = function(tokenized_data, n) {
#   n = 5
#   tokenized_data = OneGramTrain
#   terms = NULL
#   for(i in n:length(tokenized_data)) {
#     text = c(tokenized_data[i-4], tokenized_data[i-3], tokenized_data[i-2], tokenized_data[i-1], tokenized_data[i])[1:n]
#     text = paste(text, sep = " ")
#   }
# }
# 
# ptm = proc.time()
# OneGram = createDataTable2(tokenized_data = OneGramTrain, n = 5)
# x = proc.time() - ptm
# print(x)

### Create Data Tables and Apply Good-Turing Transformation
OneGram = createDataTable(tokenized_data = OneGramTrain, n = 1)
TwoGram = createDataTable(tokenized_data = TwoGramTrain, n = 2)
ThreeGram = createDataTable(tokenized_data = ThreeGramTrain, n = 3)
FourGram = createDataTable(tokenized_data = FourGramTrain, n = 4)
FiveGram = createDataTable(tokenized_data = FiveGramTrain, n = 5)

### Save as txt files
write.table(OneGram, file=paste(project_dir, "1Gram.txt", sep = ""), row.names = FALSE) 
write.table(TwoGram, file=paste(project_dir, "2Gram.txt", sep = ""), row.names = FALSE) 
write.table(ThreeGram, file=paste(project_dir, "3Gram.txt", sep = ""), row.names = FALSE) 
write.table(FourGram, file=paste(project_dir, "4Gram.txt", sep = ""), row.names = FALSE) 
write.table(FiveGram, file=paste(project_dir, "5Gram.txt", sep = ""), row.names = FALSE) 
saveRDS(OneGramDevTest, file=paste(project_dir, "1Gram_devtest.rds", sep = ""))
saveRDS(OneGramTest, file=paste(project_dir, "1Gram_test.rds", sep = ""))
