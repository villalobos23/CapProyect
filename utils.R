addSentenceMarks <- function(lineread){
  replaced <- paste("{sntcst}",lineread,sep=" ")
  replaced <- replaceTitles(replaced)
  #adding sentence breakers
  replaced <- gsub("[(.?!,)][^$][^A-Za-z]*"," {sntcnd} {sntcst} ",replaced,ignore.case = TRUE)
  #replaced repeated consecutive for a single onw
  #replaced <- gsub("(\\{sntcnd\\} \\{sntcst\\} )+","{sntcnd} {sntcst} ",replaced)
  #replaced <- gsub("\\b(\\{sntcnd\\} \\{sntcst\\})\\s+\\1\\b","{sntcnd} {sntcst}",replaced)
  replaced <- paste(replaced,"{sntcnd}",sep = " ")
  replaced <- gsub("\\{sntcnd\\} \\{sntcst\\}  \\{sntcnd\\}","{sntcnd}",replaced)
  replaced
}


replaceTitles <- function(line){
  replaced <- gsub("Mr.","Mr",line,ignore.case = TRUE)
  replaced <- gsub("ms.","Ms",replaced,ignore.case = TRUE)
  replaced <- gsub("phd.","PhD",replaced,ignore.case = TRUE)
  replaced <- gsub("md.","MD",replaced,ignore.case = TRUE)
  replaced <- gsub("msc.","Msc",replaced,ignore.case = TRUE)
  replaced
  
}

cleanSentence <- function(textInput){
  if(textInput == "") textInput <- " "
  sentences <- strsplit(textInput,"[[:punct:]]")[[1]]
  lastSentence <- strsplit(tail(sentences,1)," ",fixed=TRUE)[[1]]
  lastWords <- paste(tail(lastSentence,3),collapse=" ")
  if(length(sentences) > 1 &&
     length(lastSentence) < 3 ){
    #There was a punctuation sign and the token can be used
    lastWords <- paste("sntcst",lastWords,collapse = " ")
  }
  if(grepl("[[:blank:]]$",textInput)){
    lastWords <- paste(lastWords," ")
  }
  tolower(lastWords)
}

#Taken from 
#http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)