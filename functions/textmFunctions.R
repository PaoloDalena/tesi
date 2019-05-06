###########  FUNZIONI

#### FUNZIONE PER PRE-TRATTAMENTO PULIZIA DEI TESTI
# si occupa della conversione della codifica 
#           della trasformazione in minuscolo
#           se richiesto della pulizia dei testi, specificando la funzione di pulizia
preTrat <- function(x = NULL, Clean = F, FUN=cleanTweets(x)){
  # x = vettore di testi
  # Clean = valore logico che indica se procedere al cleaning dei testi
  # FUN: se Clean=T allora è la funzione di cleaning
  if(is.null(x)){return()}
  txt <- iconv(x, from = "UTF-8", to = "latin1", sub = "")
  txt <- tolower(txt)
  if(Clean == T){
    txt <- FUN(txt)
  }
  return(txt)
}

#### FUNZIONE PER COSTRUZIONE WORDCLOUD
## dal vettore di testi crea la wordcloud
## e ritorna il data.frame con le frequenze delle parole
doWrdCld <- function(x = NULL, stopw = NULL, col = "black", scale = c(4, 0.5), 
                     maxw = 100, lang = "italian", stem = F){
  # x      vettore di testi (i tweet ripuliti)
  # stopw  vettore che le stopwords definite dall'utente
  # col    vettore con i colori
  # scale  proporzioni delle parole nella wordcloud
  # maxw   numero massimo di parole da riportare nella wordcloud
  # lang   lingua rispetto a cui rimuovere le stopwords
  # stem   valore logico che indica se fare o meno lo stemming
  if(is.null(x)){return()}      
  crps <- Corpus(VectorSource(x))
  crps <- tm_map(crps, removeWords, stopwords(lang))
  if(!is.null(stopw)){
    crps <- tm_map(crps, removeWords, stopw)
  }
  if(stem==T){
    crps <- tm_map(crps, stemDocument, language = lang)
  }
  tdm <- TermDocumentMatrix(crps)
  m <- as.matrix(tdm)
  ddm <- as.data.frame(m)
  word_freqs= sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  par(mar=c(0,0,0,0))
  wordcloud(dm$word, dm$freq,scale= scale,
            max.words= maxw,random.order=FALSE,
            colors=col)
  return(ddm)
}

#### FUNZIONE PER LEMMATIZZAZIONE
# utilizzabile per lemmatizzare una TermDocumentMatrix trasformata in data.frame
# sia per wordcloud sia per comparison e commonality cloud
# ritorna un data.frame con lemma, forma grammaticale e frequenze
lemmatiz <- function(x = NULL, comparison = F, nomidoc = NULL){
  # x           data.frame di una termDocumentMatrix, che può essere l'output della funzione doWrdCld
  # comparison  valore logico che indica se l'output deve essere utilizzato per una comparison cloud
  # nomidoc     vettore con i nomi dei documenti, utilizzato solo se comparison = T
  if(is.null(x)){return()}
  dm_lem <- x
  if(comparison==T & !is.null(nomidoc)){
    if(dim(dm_lem)[2] != length(nomidoc)){return()}
    colnames(dm_lem) <- nomidoc
  }
  dm_lem$freq <- rowSums(dm_lem)
  dm_lem$forma <- rownames(dm_lem)
  dm_lem$lemma <- NA
  dm_lem$catgr <- NA
  {pb <- txtProgressBar(min = 0,max = dim(dm_lem)[1],style=3)
    for(i in 1:dim(dm_lem)[1]){
      setTxtProgressBar(pb,i)
      frm <- dm_lem[i,"forma"]
      ric <- dizTot[dizTot$Forma2==frm,c("Lemma2","Cat_Gramm.L")]
      if(dim(ric)[1]>0){
        dm_lem[i,"lemma"] <- ric[1,"Lemma2"]
        dm_lem[i,"catgr"] <- ric[1,"Cat_Gramm.L"]
      } else {
        dm_lem[i,"lemma"] <- frm
        dm_lem[i,"catgr"] <- "na"
      }
    }
    close(pb)}
  if(comparison==F){
    dm_lem <- dm_lem[,c("forma","lemma","catgr","freq")]
    dm_rid <- aggregate(dm_lem[,4],by=list(dm_lem$lemma,dm_lem$catgr),FUN=sum, na.rm=TRUE)
    colnames(dm_rid) <- c("lemma","catgr","freq")
  }
  if(comparison==T){
    base.fields <- c("forma","lemma","catgr")
    freq.fields <- colnames(dm_lem)[!colnames(dm_lem) %in% base.fields]
    extr.fields <- c(base.fields,freq.fields)
    dm_lem <- dm_lem[,extr.fields]
    dm_rid <- aggregate(dm_lem[,4:(dim(dm_lem)[2])],by=list(dm_lem$lemma,dm_lem$catgr),FUN=sum, na.rm=TRUE)
    colnames(dm_rid)[1:2] <- c("lemma","catgr")
  }
  dm_rid <- dm_rid[order(-dm_rid$freq),]
  rownames(dm_rid) <- ifelse(duplicated(dm_rid$lemma),paste(dm_rid$lemma,dm_rid$catgr,sep = ":"),dm_rid$lemma)
  return(dm_rid)
}
