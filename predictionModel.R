
library(dplyr)
library(data.table)
library(stringi)
if(!exists("addSentenceMarks", mode="function")||
   !exists("cleanSentence", mode="function")) source("utils.R")

loadFreqs <- function(){
  if(!exists("freqMap")){
    #print("Loading Data")
    load("onePercentFreq.RData")
    freqMap <<- list()
    freqMap[["unigram"]] <<- as.data.table(unigramSW.freq)
    freqMap[["bigram"]] <<- as.data.table(bigramSW.freq)
    freqMap[["trigram"]] <<- as.data.table(trigramSW.freq)
    freqMap[["fgram"]] <<- as.data.table(fgramSW.freq)
  }
  wrongs <<- 0
  freqMap
}

loadFreqs()

getMostFrequentStart<- function(freqs,literal){
  regexMatches <- grepl(paste("^",literal,sep=""),freqs$word)
  freqs[which(regexMatches),]
}

getMostFrequentEnd <- function(freqs, literal){
  regexMatches <- grepl(paste(literal,"$",sep=""),freqs$word)
  freqs[which(regexMatches),]
}

getMostFrequent <- function(freqs, literal){
  freqs[which(grepl(literal,freqs$word)),]
}

#This gives the relative frequency between an n-gram and its n-1gram based model
# It divides the frequency of the ngram by the frequency of the n-1gram
createRelativeFreq <- function(suffixFreqs,prefixFreqs,literal,partial="",amount=3){
  prefixRegex <<- paste(paste("\\b",literal,sep=""),"\\b",sep="")
  suffixRegex <<- ""
  #print(paste("literal used for the prefix ",prefixRegex))
  prefixes <<- getMostFrequentEnd(prefixFreqs,prefixRegex)
  if(partial != ""){
    suffixRegex <<- paste(prefixRegex,partial,collapse=" ")
  }else{
    suffixRegex <<- prefixRegex
  }
  #print(paste("literal used for the suffix ",suffixRegex))
  possibilities <<- getMostFrequentStart(suffixFreqs,suffixRegex)
  if(length(prefixes$freq)==0 && length(possibilities$freq)> 0){
    previousFreq <- sum(possibilities$freq) #add it to the df
  }else{
    previousFreq <- head(prefixes,1)$freq
  }
  possibilities$relFreq <- possibilities$freq/previousFreq
  possibilities <- arrange(possibilities,desc(relFreq))
  possibilities <- head(select(possibilities,word,relFreq),amount)
  possibilities$word <- stri_extract_last_words(possibilities$word)
  possibilities
}

getSimpleFreq <- function(words.df,search){
  simpleResults <- getMostFrequentStart(words.df,search)
  simpleResults <- head(arrange(simpleResults, desc(freq)),3)
  simpleResults
}

#Use the regular expression symbol \\W to match non-word characters, 
#using + to indicate one or more in a row, along with gregexpr to find
#all matches in a string. Words are the number of word separators plus 1
createSuggestions <- function(literal){
  completeLiteral <-""
  if(is.na(literal)){
    literal <- " "
  }
  addOccurence <- FALSE
  if (grepl("[[:blank:]]$",literal)){#Espera recomendacion completa
    #print("complete word")
    wordCount <- sapply(gregexpr("\\W+", literal), length)
    completeLiteral <- trim(literal)
    partialFinalWord <- ""
    addOccurence <- TRUE
  }else{#remover ultima palabra y predecir esa palabra que se esta escribiendo
    partialFinalWord <- stri_extract_last_words(literal)
    #print(paste("currently writing ",partialFinalWord,sep=""))
    #remove last word being typed
    completeLiteral <- gsub("\\s*\\w*$","",literal)
    wordCount <- sapply(gregexpr("\\W+", completeLiteral), length)
    completeLiteral <- trim.leading(completeLiteral)
  }
  #print(paste("prefix to use",completeLiteral))
  #print(paste("partial being used",partialFinalWord))
  #reduce literal
  #print(paste("Word count of",wordCount))
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
  
  if(addOccurence){
    #add to corresponding ngram
    #print("adding Ocurrence")
    addNGramOcurrence(ngrams[[1]],trim(literal),wordCount)
  }
  
  if(length(suggestionList$word) == 0){
    #backingOff with recursion
    wrongs <<- wrongs+1
    #print(paste("Backing off to ",wordCount-1))
    createSuggestions(gsub("^\\s*\\w*","",literal))
  }else{
    suggestionList
  }
}

getNgrams <- function(amountOfWords){
  result <- list()
  ngram <<- amountOfWords + 1
  result[[1]] <- freqMap[[amountOfWords]]
  result[[2]] <- freqMap[[ngram]]
  result
}

addNGramOcurrence <- function(frame,input,mapIndex,amount = 1){
  index <- 0
  if(mapIndex == 0){
   index <- 1
  }else{
    index <- mapIndex
  }  
  regexString <- paste(paste("\\b",input,sep=""),"\\b$",sep="")  
  newGram <- frame[which(grepl(regexString,word)),freq := freq+amount]
  modifiedGram <- list()
  modifiedGram[[getListName(mapIndex)]] <- newGram
  freqMap <<- modifyList(freqMap,modifiedGram)
}

getListName <- function(index){
  listName <- ""
  if(index == 1 || index == 0){
    listName <- "unigram"
  }else if(index == 2){
    listName <- "bigram"
  }else if(index == 3){
    listName <- "trigram"
  }else{
    listName <- "fgram"
  }
  listName
}


getNextWord <- function(textInput){
  createSuggestions(cleanSentence(textInput))
}

getUnknowns <- function(){
  wrongs
}

#addedDT <- data.table(word=c("qqqq"),freq=c(1))
#dtt <- as.data.table(freqMap[[1]])
#dtt <- rbindlist(list(dtt,addedDT))
