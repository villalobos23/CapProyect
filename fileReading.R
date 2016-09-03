getNumLines <- function(fileName){
  testcon <- file(fileName,open="r")
  readsizeof <- 20000
  nooflines <- 0
  ( while((linesread <- length(readLines(testcon,readsizeof))) > 0 ) 
    nooflines <- nooflines+linesread )
  close(testcon)
  nooflines
}

getFileSize <- function(fileName){
  size <- file.size(fileName)
  utils:::format.object_size(size, "auto")
}

getLongestLineLength <- function(fileName){
  testcon <- file(fileName,open="r")
  readsizeof <- 1
  maxLength <- 0
  maxString <- ""
  while(length(lineread <- readLines(testcon,readsizeof)) > 0 ){
    currentLength <- nchar(lineread)
    if(currentLength > maxLength){
      maxLength <- currentLength
      maxString <- lineread
    }
  }
  close(testcon)
  print(maxString)
  maxLength
}

getWordCount <- function(fileName){
  testcon <- file(fileName,open="r")
  readsizeof <- 2000
  wordCount <- 0
  while(length(lineread <- readLines(testcon,readsizeof)) > 0 ){
   wordCount <- wordCount + sum(sapply(gregexpr("[[:alpha:]]+", lineread), function(x) sum(x > 0)))
  }
  close(testcon)
  wordCount
}

readNLines <- function(fileName,n){
  testcon <- file(fileName,open="r")
  readsizeof <- n
  lineread <- readLines(testcon,readsizeof)
  close(testcon)
  lineread
}

countWordOccurrenceByLine <- function(fileName,word,printMatch=FALSE){
  testcon <- file(fileName,open="r")
  readsizeof <- 20000
  wordFrequency <- 0
  while(length(linesread <- readLines(testcon,readsizeof)) > 0 ){
    matches <- grepl(word,linesread)
    freqAmount <- length(which(matches))
    wordFrequency <- wordFrequency + (1*freqAmount)
    if(freqAmount > 0 && printMatch)
    {
      print(linesread[which(matches)])
    }
  }
  close(testcon)
  wordFrequency
}

extractFiles <- function(tarball){
  unzip(tarball,overwrite = TRUE)
}

#This is a file of aprox 500mb 
downloadZipFile <- function(){
  download.file(
    "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
}