library(RWeka)
library(tm)
library(ggplot2)
#Rgraph viz
#> source("http://bioconductor.org/biocLite.R")
#    biocLite("Rgraphviz")

createCorpus <- function(texts){
  resultCorpus <- VCorpus(VectorSource(texts)) 
  resultCorpus
}

cleanCorpus <- function(dCorpus,removeSW = TRUE,allLower=TRUE){
  cCorpus <- dCorpus
  if(allLower){
    cCorpus <- tm_map(cCorpus, content_transformer(tolower))
  }
  cCorpus <- tm_map(cCorpus, removePunctuation)
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  removeApostrophe <- function(x) gsub("'","",x)
  removeAphView <- function(x) gsub("â","",x)
  removeOtherChrs <- function(x) gsub("[^[:alnum:]///' ]", "", x) 
  cCorpus <- tm_map(cCorpus, content_transformer(removeURL))
  cCorpus <- tm_map(cCorpus, content_transformer(removeApostrophe))
  cCorpus <- tm_map(cCorpus, content_transformer(removeOtherChrs))
  cCorpus <- tm_map(cCorpus, content_transformer(removeAphView))
  cCorpus <- tm_map(cCorpus,stripWhitespace)
  cCorpus <- tm_map(cCorpus,removeNumbers)
  if(removeSW)
    cCorpus <- tm_map(cCorpus,removeWords, stopwords("english"))
  cCorpus
}

stemCorpus <- function(sCorpus){
  corpus.temp <- tm_map(sCorpus,stemDocument,language="english",lazy=TRUE)
  corpus.temp
}

createTermDocumentMatrix <- function(corpus,
                                     lowerN,
                                     upperN,
                                     removeSparse=TRUE,
                                     sparsityTh = 0.15){
  xGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = lowerN, max = upperN))
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = xGramTokenizer))
  if(removeSparse)
    tdm <- removeSparseTerms(tdm,sparsityTh)
  tdm
}

createFrequencyMatrix <- function(tdm, col=FALSE){
  freqs <- as.matrix(tdm)
  if(col){
    freqs <- sort(colSums(freqs),decreasing = T)
  }else{
   freqs <- sort(rowSums(freqs),decreasing = T)
  }
  freqs <- data.frame(word = names(freqs),freq = freqs)
  freqs
}

createFrequencyPlot <- function(fm,useMean=TRUE,minFreq=1,plotTitle="N-gram Freq."){
  currentMean <- 0
  if(useMean){
    currentMean <- mean(fm$freq)
  }else{
    currentMean <- minFreq
  }
  p <- ggplot(subset(fm, freq>currentMean), aes(word, freq))    
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
  p <- p + ggtitle(plotTitle) + labs(x="Words",y="Frequencies")
  p
}

getCoverageAmount <- function(freqMatrix, coverage){
  freqSum <- cumsum(freqMatrix$freq)
  min(which(freqSum > (coverage*sum(freqMatrix$freq))))
}

createWordCorrelationPlot <- function(tdm, maxNet = 50,lowerFreq = 2, corThres= 0.5){
  plot(tdm, terms = findFreqTerms(tdm, lowfreq = lowerFreq)[1:maxNet], corThreshold = corThres)
}