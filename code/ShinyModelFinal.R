### Thomas Hepner
### 4/11/2016
### Prediction Algorithm

### Remove existing objects
rm(list = ls())  
set.seed(1)

### Load Text Prediction Packages
library(quanteda)
library(Matrix)
library(stringr)
library(data.table)

### Directories
project_dir = "C:/Users/thep3/OneDrive/Documents/Coursera/Data Science Capstone/App-1/"

### Load Tokenized Data
OneGram = fread(input="1Gram.txt") 
TwoGram = fread(input="2Gram.txt") 
ThreeGram = fread(input="3Gram.txt") 
FourGram = fread(input="4Gram.txt") 
FiveGram = fread(input="5Gram.txt") 
OneGramDevTest = readRDS(file="1Gram_devtest.rds") 
OneGramTest = readRDS(file="1Gram_test.rds")

### Testing: Load Tokenized Data
# OneGram = fread(input="C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/App-1/1Gram.txt") 
# TwoGram = fread(input="C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/App-1/2Gram.txt") 
# ThreeGram = fread(input="C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/App-1/3Gram.txt") 
# FourGram = fread(input="C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/App-1/4Gram.txt") 
# FiveGram = fread(input="C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/App-1/5Gram.txt") 
# OneGramDevTest = readRDS(file="C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/App-1/1Gram_devtest.rds") 
# OneGramTest = readRDS(file="C:/Users/Thomas/OneDrive/Documents/Coursera/Data Science Capstone/App-1/1Gram_test.rds")

### Helper function to calculate frequencies/probabilities of terms
getProbs = function(text, data, n) {
  ## text: text being evaluated
  ## tokenized_data: list of tokens
  ## n: n-grams searching for

  ## Create word-key for searching data
  words = c()
  for(i in -(n-1):-1) {
    word = word(text, i)
    words = c(words, word)
  }
  
  ## Create subset of data
  subset = NULL
  if(n == 1) {
    subset = data
  }
  if(n == 2) {
    subset = data[which(data$word1 == words[1]), ]    
  }
  if(n == 3) {
    subset = data[which(data$word1 == words[1] & data$word2 == words[2]), ]
  } 
  if(n == 4) {
    subset = data[which(data$word1 == words[1] & data$word2 == words[2] & data$word3 == words[3]), ]
  }
  if(n == 5) {
    subset = data[which(data$word1 == words[1] & data$word2 == words[2] & data$word3 == words[3] & data$word4 == words[4]), ]   
  }
  subset = data.frame(subset)
  
  ## Create vector of prominant terms
  terms = as.vector(subset$freq, mode = "numeric")
  names(terms) = as.character(subset[, dim(subset)[2]-1])

  ## Calculate and return probabilities
  probs = terms
  total = sum(terms)
  if(total == 0) {
    total = 1
  }  
  probs = terms / total
  return(probs)
}

### Helper function to build Interpolation model
buildModel = function(oneProbs, twoProbs, threeProbs, fourProbs, fiveProbs) {
  ## Takes 4 vectors, representing probabilities of different words in 1,2,3, and 4-gram models
  
  ## Lambda values for interpolation model
  lambda1 = 4/10; lambda2 = 3/10; lambda3 = 2/10; lambda4 = 0.7/10; lambda5 = 0.3/10
  
  ## Create list/dataframe of possible words model could return
  possible_words = unique(c(names(fiveProbs)[1:50], c(names(fourProbs)[1:50], c(names(threeProbs)[1:50], c(names(twoProbs)[1:50], c(names(oneProbs)[1:50]))))))[1:50]
  if(!is.null(possible_words)) {
    possible_words = possible_words[!is.na(possible_words)]
    word_probs = data.frame(words = possible_words, probs = rep(0, length(possible_words)))
    
    ## Calculate probability of each word with interpolation
    for(i in possible_words) {
      valueFive = lambda1 * ifelse(length(which(names(fiveProbs) == i)) > 0, fiveProbs[which(names(fiveProbs) == i)], 0)
      valueFour = lambda2 * ifelse(length(which(names(fourProbs) == i)) > 0, fourProbs[which(names(fourProbs) == i)], 0)
      valueThree = lambda3 * ifelse(length(which(names(threeProbs) == i)) > 0, threeProbs[which(names(threeProbs) == i)], 0)
      valueTwo = lambda4 * ifelse(length(which(names(twoProbs) == i)) > 0, twoProbs[which(names(twoProbs) == i)], 0)
      valueOne = lambda5 * ifelse(length(which(names(oneProbs) == i)) > 0, oneProbs[which(names(oneProbs) == i)], 0)
      word_probs[which(word_probs$words == i),]$probs = valueFive + valueFour + valueThree + valueTwo + valueOne
    }
    word_probs$probs[is.na(word_probs$probs)] = 0
    word_probs = word_probs[order(word_probs$probs, decreasing = TRUE),] 
    word_probs$probs = word_probs$probs / sum(word_probs$probs)
  } 
  else {
    word_probs = OneGram[1:50, ]
    colnames(word_probs) = c("possible_words", "probs")
  }
  
  ## Return Most probable words
  return(word_probs)
}

### Build Predictive Text Model
library(stringr)
predictNextWord = function(text) {
  ## Takes a string of text as input and outputs dataframe of words and probability of being the next word
  
  ## Clean text to prepare for model building
  if(!is.null(text) || text != "") {
  text = toLower(tokenize(text, what = "word", removeNumbers = TRUE, removePunct = TRUE
                  , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                  , ngrams = 1, concatenator= " "
                  , simplify = TRUE))
  n = length(text) 
  if(n >= 5) {
    text = paste(text[(n-4):n], collapse = ' ')    
  } else {
    text = paste(text[1:n], collapse = ' ')      
  }
  text = str_trim(text, side = c("right"))

  ## Create term probabilities
  oneProbs = getProbs(text = text, data = OneGram, n = 1)
  twoProbs = getProbs(text = text, data = TwoGram, n = 2)
  threeProbs = getProbs(text = text, data = ThreeGram, n = 3)
  fourProbs = getProbs(text = text, data = FourGram, n = 4)
  fiveProbs = getProbs(text = text, data = FiveGram, n = 5)

  ## Interpolation Model
  ## Create probabilities for all words
  top50 = buildModel(oneProbs, twoProbs, threeProbs, fourProbs, fiveProbs)

  ## Return dataframe with probabilities for top words
  return(top50)
  }
  else {
    
  }
}

### Check Model Accuracy as additional input text is added
checkAccuracy = function(text) {
  ## Takes a string of text as input and outputs vector of term-accuracy
  
  ## Clean text
  text = toLower(tokenize(text, what = "word", removeNumbers = TRUE, removePunct = TRUE
                          , removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE
                          , ngrams = 1, concatenator= " "
                          , simplify = TRUE))
  
  ## Create term-accuracy vector
  n = length(text) 
  terms = c()
  count = 0
  accurate = 0
  terms = c()
  for(i in 1:n) {
    count = count + 1
    subText = paste(text[0:(i-1)], collapse = " ")
    top3 = predictNextWord(subText)[1:3,]
    if(text[i] %in% top3$words) {
      accurate = accurate + 1
    }    
    terms = c(terms, accurate / i)
  }
  names(terms) = text
  terms = data.frame(terms)
  names(terms) = 'accuracy'
  
  ## Return dataframe with probabilities for top words
  return(terms)
}