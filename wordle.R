setwd("~/Dropbox/")
library(stringr)


# Dictionaries ------------------------------------------------------------
# Danish
d <- read.table("RO2012.opslagsord.med.homnr.og.ordklasse.txt", header = FALSE, sep = ";", quote = "\"")
words <- tolower(d$V1)
words[substr(words,2,2) == "."] <- substr(words[substr(words,2,2) == "."], 4, max(nchar(words)))
# English
d <- words::words
words <- tolower(d[,1])

# words <- gsub("(.)|( )|([1-9])", "", words)

mysubset <- function(d, size = 5, notin = NULL, contains = NULL, known = NULL, pos = NULL, bestWord = FALSE){
  words <- d[nchar(d) == size]
  words <- d <- words[!unlist(if(length(words) != 0) lapply(strsplit(words, ""), function(i) any(i %in% c("-", " ", "/", ".") )) else character(0))]
  if(!is.null(notin) & !is.null(contains)) notin <- paste(setdiff(strsplit(notin, "")[[1]], strsplit(contains, "")[[1]]), collapse = "")
  if(!is.null(notin)){
    notin <- strsplit(notin, "")[[1]]
    for(i in 1:length(notin)){
      words <- words[!str_detect(words, notin[i])]  
    }
  }
  if(!is.null(contains) && !identical(contains, "")){
    contains <- strsplit(contains, "")[[1]]
    known <- as.numeric(strsplit(known, "")[[1]])
    pos <- as.numeric(strsplit(pos, "")[[1]])
    if(length(contains) != length(known) | length(known) != length(pos)){
      warning("contains, known, and pos must have equal length")
    }
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
  cat("Number of words:", length(words), "\n")
  # test <- if(length(words) != 0) lapply(strsplit(words, ""), function(i) length(unique(i))) == length else F
  
  if(bestWord){
    howmany <- rep(NA, length(words))
    for(i in 1:length(words)){
      howmany[i] <- checkword(words[i], words)
    }
    most <- which(howmany == max(howmany))
    test <- if(length(most) != 0) lapply(strsplit(words[most], ""), function(i) length(unique(i))) == size else F
    
    # cat("Word:", ifelse(any(test == 1), words[match(1, test)], words[1]), "\n")
    if(any(test == 1)){
      cat("Words:", sample(words[most[which(test == 1)]], 1), "\n")
    }
    else{
      cat("Words:", sample(words[most], 1), "\n")
    }
    # cat("Word:", ifelse(any(test == 1), words[most[which(test == 1)]], words[most]), "\n")
  }
  if(!is.null(known) && !identical(known, "")){
    if(length(unique(pos[known == 1])) == (size - 1) & length(words) >= 3){
      subwords <- strsplit(words, "")
      tmp <- rep(NA, length(words))
      for(i in 1:length(words)) tmp[i] <- subwords[[i]][!((1:size) %in% pos[known == 1])]
      dsub <- d[!(d %in% words)]
      
      for(i in 1:(size-1)){
        position <- unique(pos[known == 1])[i]
        dsub <- dsub[substr(dsub, position, position) != unique(substr(words, position, position))]
      }
      
      howmany <- rep(NA, length(dsub))
      for(i in 1:length(dsub)){
        howmany[i] <- checkword(dsub[i], tmp)
      }
      most <- which(howmany == max(howmany))
      test <- if(length(most) != 0) lapply(strsplit(dsub[most], ""), function(i) length(unique(i))) == size else F
      
      # cat("Word:", ifelse(any(test == 1), words[match(1, test)], words[1]), "\n")
      if(any(test == 1)){
        cat("Words:", sample(dsub[most[which(test == 1)]], 1), "\n")
      }
      else{
        cat("Words:", sample(dsub[most],1), "\n")
      }
    } 
  }
  correct <- readline(prompt = "Correct? ")
  if(!as.logical(correct)){
    notinAdd <- readline(prompt = "notin? ")
    containsAdd <- readline(prompt = "contains? ")
    knownAdd <- readline(prompt = "known? ")
    posAdd <- readline(prompt = "pos? ")
    notin <- paste(c(notin, notinAdd), collapse = "")
    contains <- paste(c(contains, containsAdd), collapse = "")
    known <- paste(c(known, knownAdd), collapse = "")
    pos <- paste(c(pos, posAdd), collapse = "")
    mysubset(words, size = size, notin = notin, contains = contains, known = known, pos = pos, bestWord = TRUE)
  }
  else{
    cat("You're welcome!")
  }
  # cat("Words: ", words[1:5], "\n")
}


checkword <- function(word, dictionary){
  word <- paste0("(", paste0(strsplit(word, "")[[1]], collapse = ")|("), ")")
  sum(grepl(word, dictionary))
}

mysubset(words, size = 5, bestWord = TRUE)



