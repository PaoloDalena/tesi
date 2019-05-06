# Differenze lessicali

# NB: fino a cf10 c'era draghi, poi è subentrato visco


# 1 Creazione tagged texts --------------------------------------------------
library(koRpus)
library(koRpus.lang.it)
library(here)

tag_cf08 <- treetag(
  here('cf_txt','cf08.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf08"
)

tag_cf09 <- treetag(
  here('cf_txt','cf09.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf09"
)

tag_cf10 <- treetag(
  here('cf_txt','cf10.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf10"
)

tag_cf11 <- treetag(
  here('cf_txt','cf11.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf11"
)

tag_cf12 <- treetag(
  here('cf_txt','cf12.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf12"
)

tag_cf13 <- treetag(
  here('cf_txt','cf13.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf13"
)

tag_cf14 <- treetag(
  here('cf_txt','cf14.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf14"
)

tag_cf15 <- treetag(
  here('cf_txt','cf15.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf15"
)

tag_cf16 <- treetag(
  here('cf_txt','cf16.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf16"
)

tag_cf17 <- treetag(
  here('cf_txt','cf17.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf17"
)


# 2 Statistiche descrittive -----------------------------------------------
library(tidyverse)
docs <- list(tag_cf08,tag_cf09,tag_cf10,tag_cf11,tag_cf12,
          tag_cf13,tag_cf14,tag_cf15,tag_cf16,tag_cf17)
docs_names <- c("tag_cf08","tag_cf09",str_c("tag_cf",10:17))
desc <- map(docs, describe)

# Numero delle parole
numero_parole <- NULL
for (i in 1:10) {
  numero_parole[i] <-  desc[[i]]$words
}
numero_parole
barplot(numero_parole,
        names.arg = 2008:2017,
        xlab = "Anni",
        ylab = "Numero di parole",
        main = "Numero di parole nelle cf negli anni",
        col = rainbow(10))

# Numero delle frasi
numero_frasi <- NULL
for (i in 1:10) {
  numero_frasi[i] <-  desc[[i]]$sentences
}
numero_frasi
barplot(numero_frasi,
        names.arg = 2008:2017,
        xlab = "Anni",
        ylab = "Numero di frasi",
        main = "Numero di frasi nelle cf negli anni",
        col = rainbow(10))

# Lunghezza media delle parole
media_parole <- NULL
for (i in 1:10) {
  media_parole[i] <-  desc[[i]]$avg.word.length
}
media_parole
barplot(media_parole,
        names.arg = 2008:2017,
        xlab = "Anni",
        ylab = "Lunghezza media parole",
        ylim = c(5.4,5.8),
        xpd = F,
        main = "Lunghezza media delle parole nelle cf negli anni",
        col = rainbow(10))
grid()

# Lunghezza media delle frasi
media_frasi <- NULL
for (i in 1:10) {
  media_frasi[i] <-  desc[[i]]$avg.sentc.length
}
media_frasi
barplot(media_frasi,
        names.arg = 2008:2017,
        xlab = "Anni",
        ylab = "Lunghezza media frasi",
        ylim = c(15,25),
        xpd = F,
        main = "Lunghezza media delle frasi nelle cf negli anni",
        col = rainbow(10))
grid()

# MTLD - measure of textual lexical diversity
# Misura della ricchezza del lessico

MTLDs <- map(docs, koRpus::MTLD)
MTLDs
num_MTLDs <- c(178.53,
               188.43,
               196.73,
               191.68,
               193.18,
               185.57,
               172.14,
               181.33,
               176.75,
               163.17)

plot.default(num_MTLDs,
             x = 2008:2017,
             type = "b",
             main = "MTLD delle cf negli anni",
             xlab = "Anni",
             ylab = "Misura della ricchezza del lessico",
             col = c(1,1,3,1,1,1,2,1,1,2))
grid()

# Degno di nota anche
plot(tag_cf08)
