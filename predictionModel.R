getMostFrequent<- function(freqs,literal){
  freqs$word[which(grepl(paste("^",literal,sep=""),freqs$word))]
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