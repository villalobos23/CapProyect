getMostFrequentStart<- function(freqs,literal){
  freqs[which(grepl(paste("^",literal,sep=""),freqs$word)),]
}

getMostFrequentEnd <- function(freqs, literal){
  freqs[which(grepl(paste(literal,"$",sep=""),freqs$word)),]
}

getMostFrequent <- function(freqs, literal){
  freqs[which(grepl(literal,freqs$word)),]
}

#This gives the relative frequency between an n-gram and its n-1gram based model
# It divides the frequency of the ngram by the frequency of the n-1gram
createRelativeFreq <- function(suffixFreqs,prefixFreqs,literal){
  augmentedLiteral <- paste(paste("\\b",literal,sep=""),"\\b",sep="")
  wordsM <- getMostFrequentStart(suffixFreqs,augmentedLiteral)
  wordsM$relFreq <- wordsM$freq/getMostFrequentStart(prefixFreqs,augmentedLiteral)$freq
  wordsM
}

#This gives the relative frequency of a literal as an starting word in an ngram
startingFreq <- function(xgram,literal){
  augmentedLiteral <- paste(paste("\\b",literal,sep=""),"\\b",sep="")
  startF <- sum(getMostFrequentStart(xgram,augmentedLiteral)$freq)
  mLength <- length(xgram$word)
  startF/mLength
}

#This gives the relative frequency of a literal as an starting word in an ngram
#It appears to be incorrect, the division should be made by the number of sentence starts
#not by the frequency of the starting words
startingFreq2 <- function(xgram,ygram,literal){
  augmentedLiteral <- paste(paste("\\b",literal,sep=""),"\\b",sep="")
  startF <- sum(getMostFrequentStart(xgram,augmentedLiteral)$freq)
  mLength <- getMostFrequentStart(ygram,augmentedLiteral)$freq
  startF/mLength
}

#This gives the relative frequency of a literal as an ending word in an ngram
endingFreq <- function(xgram,ygram,literal){
  augmentedLiteral <- paste(paste("\\b",literal,sep=""),"\\b",sep="")
  endings <- getMostFrequentEnd(xgram,augmentedLiteral)
  asPrefix <- getMostFrequentStart(ygram,augmentedLiteral)
  sum(endings$freq)/asPrefix$freq
}

createTestData <- function(){
  
}

#Create functions that give the next possible word from frequencies
  
#create functions to feedback the model

#Match next common word  - match next common regex word 
#4gram
#3gram
#2gram
#sorted by frequencies

#which should be the first word ever?