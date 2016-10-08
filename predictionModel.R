#loadTheFrequencies

library(dplyr)
library(stringi)
if(!exists("addSentenceMarks", mode="function")||
   !exists("cleanSentence", mode="function")) source("utils.R")

loadFreqs <- function(){
  if(!exists("freqMap")){
    print("Loading Data")
    load("onePercentFreq.RData")
    freqMap <<- list()
    freqMap[[1]] <<- unigramSW.freq
    freqMap[[2]] <<- bigramSW.freq
    freqMap[[3]] <<- trigramSW.freq
    freqMap[[4]] <<- fgramSW.freq
  }
  freqMap
}

loadFreqs()

getMostFrequentStart<- function(freqs,literal){
  regexMatches <- grepl(paste("^",literal,sep=""),freqs$word)
  freqs[which(regexMatches),]
}

getMostFrequentEnd <- function(freqs, literal){
  freqs[which(grepl(paste(literal,"$",sep=""),freqs$word)),]
}

getMostFrequent <- function(freqs, literal){
  freqs[which(grepl(literal,freqs$word)),]
}

#This gives the relative frequency between an n-gram and its n-1gram based model
# It divides the frequency of the ngram by the frequency of the n-1gram
createRelativeFreq <- function(suffixFreqs,prefixFreqs,literal,partial=""){
  augmentedLiteral <<- paste(paste("\\b",literal,sep=""),"\\b",sep="")
  prefixes <- getMostFrequentEnd(prefixFreqs,augmentedLiteral)
  if(partial != ""){
    augmentedLiteral <- paste(augmentedLiteral,partial,sep=" ")
  }
  possibilities <- getMostFrequentStart(suffixFreqs,augmentedLiteral)
  possibilities$relFreq <- possibilities$freq/prefixes$freq
  possibilities <- arrange(possibilities,desc(relFreq))
  possibilities <- head(select(possibilities,word,relFreq),3)
  possibilities$word <- stri_extract_last_words(possibilities$word)
  possibilities
}

getSimpleFreq <- function(words.df,search){
  simpleResults <- getMostFrequentStart(words.df,search)
  simpleResults <- head(arrange(simpleResults, desc(freq)),3)
  simpleResults
}

#Taken from 
#http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Use the regular expression symbol \\W to match non-word characters, 
#using + to indicate one or more in a row, along with gregexpr to find
#all matches in a string. Words are the number of word separators plus 1
createSuggestions <- function(literal){
  completeLiteral <-""
  if (grepl("[[:blank:]]$",literal)){#Espera recomendacion completa
    print("complete word")
    wordCount <- sapply(gregexpr("\\W+", literal), length)
    completeLiteral <- trim(literal)
    partialFinalWord <- ""
  }else{#remover ultima palabra y predecir esa palabra que se esta escribiendo
    partialFinalWord <- stri_extract_last_words(literal)
    print(paste("currently writing ",partialFinalWord,sep=""))
    #remove last word being typed
    completeLiteral <- gsub("\\s*\\w*$","",literal)
    wordCount <- sapply(gregexpr("\\W+", completeLiteral), length)
    completeLiteral <- trim.leading(completeLiteral)
    print(completeLiteral)
  }
  #reduce literal
  print(wordCount)
  ngrams <- getNgrams(wordCount)
  ##This condition help us manage base case of backoff recursion
  if(wordCount == 1 && completeLiteral==""){
    suggestionList <-  getSimpleFreq(ngrams[[1]],partialFinalWord)
  }else{
    suggestionList <- createRelativeFreq(suffixFreqs = ngrams[[2]],
                     prefixFreqs = ngrams[[1]],
                     completeLiteral,
                     partialFinalWord)
  }
  
  #add to corresponding ngram
  addGrams(ngrams[[1]],literal)
  
  if(length(suggestionList$word) == 0){
    #backingOff with recursion
    createSuggestions(gsub("^\\s*\\w*","",literal))
  }else{
    suggestionList
  }
}

getNgrams <- function(amountOfWords){
  result <<- list()
  ngram <- amountOfWords + 1
  result[[1]] <<- freqMap[[amountOfWords]]
  result[[2]] <<- freqMap[[ngram]]
  result
}

addGrams <- function(frame,input){
  #http://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement-in-r
  #http://stackoverflow.com/questions/19713130/updating-individual-values-not-rows-in-an-r-data-frame
}

getNextWord <- function(textInput){
  createSuggestions(cleanSentence(textInput))
}




