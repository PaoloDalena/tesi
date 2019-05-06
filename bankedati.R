# ven3mag2019 @pd
library(tm)
# slides banche dati

# creo corpus
cf_corpus <- tm::Corpus(tm::DirSource(here::here("cf_txt")))

# creo tdm
tdm <- TermDocumentMatrix(
  cf_corpus,
  control = list(stopwords = stopwords("italian"))
)
tdm

#sostituisco nomi documenti
library(stringr)
nomi <- c(
  "cf08", "cf09", "cf10", "cf11", "cf12", "cf13", "cf14", "cf15", "cf16", "cf17"
)
tdm$dimnames$Docs <- nomi
inspect(tdm)

#osservo termini più frequenti
findFreqTerms(tdm, 100)
findAssocs(tdm, "crisi", corlimit = 0.9)


#inserisco stopwords appropriate e rimando
mystop <- c("banca","banche","d'italia", "considerazioni","finali")
mystop
tdm_1 <- TermDocumentMatrix(
  cf_corpus,
  control = list(stopwords = c(mystop,stopwords("italian")))
)
tdm_1
tdm_1$dimnames$Docs <- nomi
inspect(tdm_1)
findFreqTerms(tdm_1, 100)


# trasformo tdm in una matrice
m = as.matrix(tdm_1)
m[1:10,]
####RICORDA DI PULIRE IL TESTO

# ordino le parole in ordine decrescente
word_freqs = sort(rowSums(m), decreasing=TRUE)

# creo il data.frame
dm = data.frame(word=names(word_freqs), freq=word_freqs)
head(dm,10)

# visualizzazione della nuvola di parole
install.packages("wordcloud")
library(wordcloud)
wordcloud(dm$word, dm$freq,c(1,.3), min.freq=50,
          random.order=F, colors=brewer.pal(8, "Dark2"))


#lucidi2
findFreqTerms(tdm_1,100)
getwd()
library(here)
here()
load(here('functions','dizFormeLemmi.RData'))
#lemmatizzazione
dm_lem <- dm # creazione archivio per lemmatizzazione 
dm_lem$forma <- rownames(dm_lem) # creazione campo con forme
dm_lem$lemma <- NA # per lemmi
dm_lem$catgr <- NA # per categoria grammaticale
{pb <- txtProgressBar(min= 0, max= dim(dm_lem)[1], style = 3) 
  for(i in 1:dim(dm_lem)[1]){
  setTxtProgressBar(pb, i)
  frm <- dm_lem[i,"forma"]
  ric <- dizTot[dizTot$Forma2==frm,c("Lemma2","Cat_Gramm.L")] 
  if(dim(ric)[1]>0){
    dm_lem[i,"lemma"] <- ric[1,"Lemma2"]
    dm_lem[i,"catgr"] <- ric[1,"Cat_Gramm.L"] } else {
      dm_lem[i,"lemma"] <- frm
      dm_lem[i,"catgr"] <- "na"
    }
}
  close(pb)}
head(dm)  
head(dm_lem)

#install.packages('sqldf')
library(sqldf)
dm_rid <- sqldf("select lemma, sum(freq) as freq, catgr from dm_lem group by lemma")
head(dm_rid)
head(dm_lem)
wordcloud(dm_rid$lemma, dm_rid$freq,c(3,0),max.words = 150,
random.order=FALSE, colors=rev(brewer.pal(8, "Dark2"))) #wc con lemmi

par(mar=c(1,1,1,1))
wordcloud(dm_rid[dm_rid$catgr=="S","lemma"],
          dm_rid[dm_rid$catgr=="S","freq"],
          c(2,0), max.words = 150,
          random.order=FALSE,
          colors=(brewer.pal(8, "YlOrRd"))) #wc lemmi solo sostantivi


wordcloud(dm_rid[dm_rid$catgr=="G","lemma"],
          dm_rid[dm_rid$catgr=="G","freq"],
          c(2,0), max.words = 150,
          random.order=FALSE,
          colors=rev(brewer.pal(9, "Set1"))) #wc solo aggettivi

display.brewer.all()
