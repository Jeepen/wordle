setwd("~/Dropbox/")
library(stringr)


# Dictionaries ------------------------------------------------------------
# Danish
d <- read.table("RO2012.opslagsord.med.homnr.og.ordklasse.txt", header = FALSE, sep = ";", quote = "\"")
words <- tolower(d$V1)
# English
d <- words::words
words <- tolower(d[,1])

mysubset <- function(d, length = 5, notin = NULL, contains = NULL, known = NULL, pos = NULL){
  words <- d[nchar(d) == length]
  if(!is.null(notin)){
    notin <- strsplit(notin, "")[[1]]
    for(i in 1:length(notin)){
      words <- words[!str_detect(words, notin[i])]  
    }
  }
  if(!is.null(contains)){
    contains <- strsplit(contains, "")[[1]]
    for(i in 1:length(contains)){
      if(known[i]){
        words <- words[substr(words, pos[i], pos[i]) == contains[i]]    
      }
      else{
        words <- words[substr(words, pos[i], pos[i]) != contains[i]]
        words <- words[str_detect(words, contains[i])]
      }
    }
  }
  words <- words[!unlist(lapply(strsplit(words, ""), function(i) any(i %in% c("-", " ", "/", ".") )))]
  cat("Number of words: ", length(words), "\n")
  test <- lapply(strsplit(words, ""), function(i) length(unique(i))) == length
  cat("Word: ", ifelse(any(test == 1), words[match(1, test)], words[1]), "\n")
  words
  # cat("Words: ", words[1:5], "\n")
}


word <- mysubset(words, length = 5, notin = "randoljusp", 
                 contains = "cei",
                 known = c(1,1,1),
                 pos = c(4,5,3))

word <- mysubset(words, length = 6, notin = "mn", 
                 contains = "base",
                 known = c(1,1,1,1),
                 pos = c(1,2,4,5))

word <- mysubset(words, length = 7, notin = "abikdch", 
                 contains = "rosse",
                 known = c(0,1,0,0,0),
                 pos = c(2,3,4,7,7))



