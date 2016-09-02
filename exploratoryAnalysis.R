library(RWeka)
library(tm)

#Rgraph viz
#> source("http://bioconductor.org/biocLite.R")
#    biocLite("Rgraphviz")

ngramsGist <- function(){#https://gist.github.com/benmarwick/5370329
  data("crude")
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
  
  inspect(tdm[340:345,1:10])
  
  plot(tdm, terms = findFreqTerms(tdm, lowfreq = 2)[1:50], corThreshold = 0.5)
}

kaggleMultipleTokenizer <- function (){
  data("crude")
  ngramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
  dtmH = DocumentTermMatrix(crude, control = list(tokenize = ngramTokenizer))
  dtmH = removeSparseTerms(dtmH, 0.3)
  dtmH = as.data.frame(as.matrix(dtmH))
  dtmH
}

createCorpus <- function(texts){
  resultCorpus <- VCorpus(VectorSource(texts)) 
  resultCorpus
}

cleanCorpus <- function(dCorpus,removeSW = TRUE,allLower=FALSE){
  cCorpus <- tm_map(dCorpus, removePunctuation)
  cCorpus <- tm_map(dCorpus,removeNumbers)
  
  if(allLower){
    cCorpus <- tm_map(cCorpus, tolower)
  }
  if(removeSW)
    cCorpus <- tm_map(cCorpus,removeWords, stopwords("english"))
  cCorpus
}

stemCorpus <- function(sCorpus){
  tm_map(sCorpus,)
}

createTermDocumentMatrix <- function(corpus,
                                     lowerN,
                                     upperN,
                                     removeSparse=TRUE,
                                     sparsityTh = 0.5){
  xGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = lowerN, max = upperN))
  tdm <- TermDocumentMatrix(crude, control = list(tokenize = xGramTokenizer))
  if(removeSparse)
    tdm <- removeSparseTerms(tdm,sparsityTh)
  tdm
}