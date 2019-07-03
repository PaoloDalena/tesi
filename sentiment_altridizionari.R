# Rieseguo la sentiment analysis cambiando dizionario, sperando di ottenere
# risultati più soddisfacenti.
library(tm)
# carico myClassPolarity, ricTxtLem e il dizionario mySntIT
ricTxtLem <- function(x = NULL){
  # x = data.frame della term document matrix lemmatizzata
  if(is.null(x)){return()} 
  out <- character()
  nc <- dim(x)[2]-1
  nt <- 0
  for(i in 3:nc){
    nt <- nt + 1
    out[nt] <- paste(x[x[,i] > 0, "lemma"], collapse = " ")
  }
  return(out)
}

pol10 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf10)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
prop.table(table(pol10$parole$category))

# confronto con altro dizionario
# installo pacchetto syuzhet e carico funzione my_get_nrc_sentiment

library(syuzhet)
my_get_nrc_sentiment <- function (char_v, cl = NULL, language = "english", lexicon=NULL){
  # Funxione per il calcolo del tipo di emozione e della polarità per un vettore di testi
  if (!is.character(char_v))
    stop("Data must be a character vector.")
  if (!is.null(cl) && !inherits(cl, "cluster"))
    stop("Invalid Cluster")
  if(is.null(lexicon)){
    lexicon <- dplyr::filter_(nrc, ~lang == language)
  }
  word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
  if (is.null(cl)) {
    nrc_data <- lapply(word_l, get_nrc_values, lexicon = lexicon)
  }
  else {
    nrc_data <- parallel::parLapply(cl = cl, word_l, lexicon = lexicon,
                                    get_nrc_values)
  }
  result_df <- as.data.frame(do.call(rbind, nrc_data), stringsAsFactors = F)
  my_col_order <- c("anger", "anticipation", "disgust",
                    "fear", "joy", "sadness", "surprise",
                    "trust", "negative", "positive")
  result_df[, my_col_order]
}

pol10_new <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf10)),
  lexicon = mySntIT
  )
prop.table(colSums(pol10_new[,9:10]))

# okay, cambia di molto.
# ora lo faccio con tutte e eseguo il confronto
# (rimando a risultati ottenuti in "sentiment.R")
npol8 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf08)),
  lexicon = mySntIT
)
npol9 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf09)),
  lexicon = mySntIT
)
npol10 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf10)),
  lexicon = mySntIT
)
npol11 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf11)),
  lexicon = mySntIT
)
npol12 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf12)),
  lexicon = mySntIT
)
npol13 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf13)),
  lexicon = mySntIT
)
npol14 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf14)),
  lexicon = mySntIT
)
npol15 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf15)),
  lexicon = mySntIT
)
npol16 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf16)),
  lexicon = mySntIT
)
npol17 <- my_get_nrc_sentiment(
  ricTxtLem(taggedText(tag_cf17)),
  lexicon = mySntIT
)
npol.tot <- list(npol8,npol9,npol10,npol11,npol12,npol13,npol14,npol15,npol16,npol17)
prop.table(colSums(npol.tot[[1]][,9:10]))["negative"]

npos <- NULL
nneg <- NULL
for (i in 1:10){
  npos[i] <- prop.table(colSums(npol.tot[[i]][,9:10]))["positive"]
  nneg[i] <- prop.table(colSums(npol.tot[[i]][,9:10]))["negative"]
}
npos
nneg
# a confronto con i risultati con l'altro dizionario
pos
neg

# abbiamo risultati più rilevanti. Bene.
ndf_sent <- data.frame(
  "Anno"=2008:2017,
  "Positivi"= npos,
  "Negativi"= nneg
)
library(scales)
ggplot(ndf_sent,aes(x=Anno)) +
  geom_line(aes(y=npos),col="darkgreen")+
  geom_label(aes(y=npos, label=percent(npos)),
             color="grey2",
             fill= c ("orange1",
                      "yellow",
                      "yellow",
                      "green",
                      "green",
                      "green3",
                      "green2",
                      "orangered",
                      "gold",
                      "orange1"))+
  geom_line(aes(y=mean(npos)),col="blue",size=0.5)+
  geom_label(aes(x=2012.5,y=mean(npos), label="Media = 63.9%"))+
  #geom_point(aes(y=pos),col="darkgreen") +
  theme_gray()+
  labs(
    title = NULL,
    y = "Percentuale sul totale dei lemmi",
    x = "Anni"
  )+
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = seq(2008,2017,1)
  )

percent(mean(npos))

# i due grafici sono molto diversi, provo a sovrapporli.
ggplot(ndf_sent,aes(x=Anno)) +
  geom_line(aes(y=pos),col="grey2")+
  geom_point(aes(y=pos),
             #color="grey2",
             color = c ("orange1",
                      "green2",
                      "green3",
                      "gold",
                      "gold",
                      "green",
                      "yellow",
                      "orangered",
                      "yellow",
                      "orange1"),
             size = 3.5)+
  geom_line(aes(y=mean(pos)),col="blue",size=0.5)+
  geom_label(aes(x=2013,y=mean(pos), label = percent(mean(pos))))+
  geom_label(aes(x=2013,y=mean(pos)-0.01, label="subjectivity lexicon"), size = 3)+
  #geom_point(aes(y=pos),col="darkgreen") +
  geom_line(aes(y=npos),col="grey2")+
  geom_point(aes(y=npos),
             #color="grey2",
             color= c ("orange1",
                      "yellow",
                      "yellow",
                      "green",
                      "green",
                      "green3",
                      "green2",
                      "orangered",
                      "gold",
                      "orange1"),
             size = 3.5)+
  geom_line(aes(y=mean(npos)),col="blue",size=0.5)+
  geom_label(aes(x=2013,y=mean(npos), label=percent(mean(npos))))+
  geom_label(aes(x=2013,y=mean(npos)-0.01, label="NRC emotion lexicon"), size = 3)+
  theme_gray()+
  labs(
    title = NULL,
    y = "Percentuale sul totale dei lemmi",
    x = "Anni"
  )+
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = seq(2008,2017,1)
  )+
  scale_y_continuous(
    minor_breaks = seq(0.50,0.70,0.01),
    breaks = seq(0.46,0.76,0.02)
  )

percent(mean(pos))
npos
percent(mean(npos[1:3]))
percent(mean(npos[4:7]))
percent(mean(npos[8:10]))

