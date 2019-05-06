# Comprendo il funzionamento di treetagger

#install.packages("koRpus")
library(koRpus)
#available.koRpus.lang()
#?install.koRpus.lang
#install.koRpus.lang('it')
library(koRpus.lang.it)

# lemmatizzo con treetagger
?koRpus
?treetag
library(here)
here()

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

# treetag(here('cf_txt','cf08.txt'),
#         treetagger="/Users/PaoloMacbook/Desktop/lemm/cmd/tree-tagger-italian",
#         lang="it")

taggedText(tag_cf08) #lo stesso però in un dataframe
str(tag_cf08)
str(taggedText(tag_cf08))

head(taggedText(tag_cf08))

table(taggedText(tag_cf08)[,"wclass"]) #per differenze linguistiche
table(taggedText(tag_cf08)[,"tag"])

#statistiche descrittive
describe(tag_cf08) #ma che figata

#differenze lessicali
#lex.div(
#  tag_cf08,
#  measure=c("TTR", "MSTTR", "MATTR","HD-D", "MTLD", "MTLD-MA"),
#  char=c("TTR", "MATTR","HD-D", "MTLD", "MTLD-MA")
#)
MTLD(tag_cf08) #178.53, proviamo a fare un confronto

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
MTLD(tag_cf09) #188.43
