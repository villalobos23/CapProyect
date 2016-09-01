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