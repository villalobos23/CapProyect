getMostFrequentStart<- function(freqs,literal){
  freqs[which(grepl(paste("^",literal,sep=""),freqs$word)),]
}

getMostFrequentEnd <- function(freqs, literal){
  freqs$word[which(grepl(paste(literal,"$",sep=""),freqs$word)),]
}

getMostFrequent <- function(freqs, literal){
  freqs$word[which(grepl(literal,freqs$word)),]
}

createRelativeFreq <- function(suffixFreqs,prefixFreqs,literal){
  wordsM <- getMostFrequentStart(suffixFreqs,literal)$word
  freqsM <- getMostFrequentStart(suffixFreqs,literal)$freq/getMostFrequentStart(prefixFreqs,literal)$freq
  relFreq <- df(wordsM,freqsM)
  relFreq
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