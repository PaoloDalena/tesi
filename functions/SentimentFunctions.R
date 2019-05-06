

create_matrix <- function(textColumns, language="italian", minDocFreq=1, minWordLength=3, 
                          removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, 
                          removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, 
                          weighting=weightTf) {
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(wordStem(split[[1]],language=language))
  }
  control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
  if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  gc()
  return(matrix)
}


myClassEmotion <- function (textColumns, algorithm = "bayes", prior = 1, lexicon = NULL, ...) 
{
  print("creazione DocumentTermMatrix")
  cat("\n")
  matrix <- create_matrix(textColumns, ...)
  if(is.null(lexicon)){
    lexicon <- read.csv("emotions_en.csv", header = FALSE)
  } else {
    lexicon <- read.csv(lexicon, header = FALSE)
  }
  ll <- table(lexicon[,2])
  lemo <- dimnames(ll)[[1]]
  counts <- list(length(which(lexicon[, 2] == lemo[1])), 
                 length(which(lexicon[, 2] == lemo[2])), 
                 length(which(lexicon[, 2] == lemo[3])), 
                 length(which(lexicon[, 2] == lemo[4])), 
                 length(which(lexicon[, 2] == lemo[5])), 
                 length(which(lexicon[, 2] == lemo[6])), 
                 nrow(lexicon))
  names(counts) <- c(lemo,"total")
  documents <- c()
  documenti <- data.frame(numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),character(),stringsAsFactors = F)
  colnames(documenti) <- c(lemo,"numParole","best_fit")
  parole <- data.frame(document=numeric(),word=character(),category=character(),score=numeric(),stringsAsFactors = F)
  {pb <- txtProgressBar(min = 0,max = nrow(matrix),style=3)
  for (i in 1:nrow(matrix)) {
    setTxtProgressBar(pb,i)
    scores <- list(0, 0, 0, 0, 0, 0)
    names(scores) <- lemo
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    tro <- 0
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[, 2] == key),]
        index <- match(word, emotions[, 1], nomatch = 0)
        if (index > 0) {
          tro <- tro+1
          entry <- emotions[index, ]
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          score <- 1
          if (algorithm == "bayes") 
            score <- abs(log(score * prior/count))
          nr <- dim(parole)[1]+1
          parole[nr,1] <- i
          parole[nr,2] <- word
          parole[nr,3] <- category
          parole[nr,4] <- score
          
          scores[[category]] <- scores[[category]] + score
        }
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    }
    else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    nscores <- unlist(scores)
    if(tro>0){
      mmx <- nscores[which.max(nscores)]
    if (algorithm == "bayes") {
      best_fit <- ifelse(abs((max(nscores[-which.max(nscores)])/mmx)-1)<0.001,"mix",names(scores)[which.max(unlist(scores))])
    } else {
      if(sum(nscores) < 0.0001){
        best_fit <- "NC" 
      } else {
        best_fit <- ifelse(abs((max(nscores[-which.max(nscores)])/mmx)-1)<0.001,"mix",names(scores)[which.max(unlist(scores))])
      }
    }
    } else {
      best_fit <- "NC"
    }
    documenti[i,1] <- scores[[1]]
    documenti[i,2] <- scores[[2]]
    documenti[i,3] <- scores[[3]]
    documenti[i,4] <- scores[[4]]
    documenti[i,5] <- scores[[5]]
    documenti[i,6] <- scores[[6]]
    documenti[i,7] <- tro
    documenti[i,8] <- best_fit
  }
    close(pb)}
  risult <- list(documenti,parole)
  return(risult)
}


myClassPolarity <- function (textColumns, algorithm = "bayes", pstrong = 0.5, pweak = 1, 
                              prior = 1,  lexicon = NULL, ...)   
{
  print("creazione DocumentTermMatrix")
  cat("\n")
  matrix <- create_matrix(textColumns, ...)
  if(is.null(lexicon)){
    lexicon <- read.csv("subjectivity_en.csv", header = FALSE)
  } else {
    lexicon <- read.csv(lexicon, header = FALSE)
  }
  lexicon <- lexicon[lexicon$V3 %in% c("negativo","positivo"),]
  lexicon$V3 <- droplevels(lexicon$V3)
  ll <- table(lexicon[,3])
  lemo <- dimnames(ll)[[1]]
  lemo <- c(lemo[which(regexpr("pos",lemo)==1)], lemo[which(regexpr("neg",lemo)==1)])
  counts <- list(length(which(lexicon[, 3] == lemo[1])), 
                 length(which(lexicon[, 3] == lemo[2])), 
                 nrow(lexicon))
  names(counts) <- c(lemo,"total")
  documenti <- data.frame(POS=numeric(),NEG=numeric(),Ratio=numeric(),best_fit=character(),stringsAsFactors = F)
  esteso <- data.frame(document=numeric(),word=character(),category=character(),polarity=character(),score=numeric(),stringsAsFactors = F)
  {pb <- txtProgressBar(min = 0,max = nrow(matrix),style=3)
  for (i in 1:nrow(matrix)) {
    setTxtProgressBar(pb,i)
    scores <- list(0, 0)
    names(scores) <- lemo
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    tro <- 0
    for (word in words) {
      index <- match(word, lexicon[, 1], nomatch = 0)
      if (index > 0) {
        tro <- 1
        entry <- lexicon[index, ]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        if (algorithm == "bayes"){ 
          score <- pweak
          if (polarity == "strongsubj"){ 
            score <- pstrong}
          score <- abs(log(score * prior/count))
        } else {
          score <- 1
        }
        
        nr <- dim(esteso)[1]+1
        esteso[nr,1] <- i
        esteso[nr,2] <- word
        esteso[nr,3] <- category
        esteso[nr,4] <- polarity
        esteso[nr,5] <- score
        scores[[category]] <- scores[[category]] + score
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- round(abs(scores[[1]]/scores[[2]]),1)
    if(tro == 0) ratio <- 1
    if (ratio == 1) 
      best_fit <- "neutral"
    documenti[i,1] <- scores[[1]]
    documenti[i,2] <- scores[[2]]
    documenti[i,3] <- ratio # abs(scores[[1]]/scores[[2]])
    documenti[i,4] <- best_fit
  }
    close(pb)}
  risult <- list(documenti=documenti,parole=esteso)
  return(risult)
}
