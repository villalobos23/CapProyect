getNumLines <- function(fileName){
  testcon <- file(fileName,open="r")
  readsizeof <- 20000
  nooflines <- 0
  ( while((linesread <- length(readLines(testcon,readsizeof))) > 0 ) 
    nooflines <- nooflines+linesread )
  close(testcon)
  nooflines
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

readNLines <- function(fileName,n,nTest){
  testcon <- file(fileName,open="r")
  readsizeof <- n
  lineread <- addSentenceMarks(readLines(testcon,readsizeof))
  lineread.test <- addSentenceMarks(readLines(testcon,nTest))
  close(testcon)
  finalMatrix <- list(dev=iconv(lineread,'UTF-8','ASCII'),test=iconv(lineread.test,'UTF-8','ASCII'))
  finalMatrix
}

addSentenceMarks <- function(lineread){
  replaced <- paste("<s>",lineread,sep=" ")
  replaced <- replaceTitles(replaced)
  replaced <- gsub("[(.?!,)][^$]","</s> <s> ",replaced,ignore.case = TRUE)
  replaced <- paste(replaced,"</s>",sep = " ")
  replaced
}

replaceTitles <- function(line){
  replaced <- gsub("mr.","Mr",line,ignore.case = TRUE)
  replaced <- gsub("ms.","Ms",line,ignore.case = TRUE)
  replaced <- gsub("phd.","PhD",line,ignore.case = TRUE)
  replaced <- gsub("md.","MD",line,ignore.case = TRUE)
  replaced <- gsub("msc.","Msc",line,ignore.case = TRUE)
  replaced
  
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