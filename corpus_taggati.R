# Costruzione corpus taggati con tm.plugin.koRpus

set.kRp.env(
  TT.cmd = "manual",
  TT.options=list(
  path="/Users/PaoloMacbook/Desktop/lemm",
  preset="it"),
  lang = "it"
)

tag_corpus <- readCorpus(here('cf_txt'))
corpusTm(tag_corpus)
class(tag_corpus)
class(corpusTm(tag_corpus)) #OK!
# mi permette di trasformare in un corpus utilizzabile con tm

#creo, dunque la tdm
mystop <- c("banca","banche","d'italia", "considerazioni","finali")
tag_tdm <- TermDocumentMatrix(
  corpusTm(tag_corpus),
  control = list(stopwords = c(mystop, stopwords("italian")))
)
tag_tdm

inspect(tag_tdm)

tag_tdm$dimnames$Docs

findFreqTerms(tag_tdm,100)
findFreqTerms(tdm_1,100) #ovviamente coincidono

