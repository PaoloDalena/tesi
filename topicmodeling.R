
# Introduzione Random per capire come funziona ----------------------------

library(tm)
library(topicmodels)
tag_corpus
tag_tdm
inspect(tag_tdm)

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 2

#Run LDA using Gibbs sampling
ldaOut <-LDA(tag_tdm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics[2010:2030,]
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 5 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,5))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
head(topicProbabilities)

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(tag_tdm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

head(topic1ToTopic2,30)



# Facciamolo per bene con la mia roba -------------------------------------

# Innanzitutto creo la mia funzioncina per il pretrattamento del testo

div_test <- function(x){
  out <- taggedText(x)
  out <- out$lemma
  out %>%
    paste(collapse = " ") %>%
    str_remove_all("@card@") %>%
    str_remove_all("<unknown>")
}
txt8 <- div_test(tag_cf08)
str_view_all(txt8,"il crisi nel mondo")
str(txt8)
str(str_split_fixed(txt8,"il crisi nel mondo",n=2))
str(str_split_fixed(str_split_fixed(txt8,"il crisi nel mondo",n=2),"il ripercussione del crisi in Italia",n=2))  

# Cerco di capire come funziona la divisione
prova <- ("frase numero uno poi frase numero due ancora frase tre infine frase 4")
prova <- prova%>%
  str_split_fixed("poi",n=2)%>%
  str_split_fixed("ancora",n=2) %>%
  str_split_fixed("infine",n=2)
prova[prova!=""]

# Provo ad applicarlo a txt8
txt8 <- txt8%>%
  str_split_fixed("il crisi e il mondo",n=2)%>%
  str_split_fixed("il ripercussione del crisi in Italia",n=2) %>%
  str_split_fixed("il crisi e il banca",n=2)
str(txt8[txt8!=""])
#wow. funziona. ora automatizziamolo

div_test <- function(x,key1,key2,key3){
  out <- taggedText(x)
  out <- out$lemma
  out <- out %>%
    paste(collapse = " ") %>%
    str_remove_all("@card@") %>%
    str_remove_all("<unknown>")
  out <- out%>%
    str_split_fixed(key1,n=2)%>%
    str_split_fixed(key2,n=2) %>%
    str_split_fixed(key3,n=2)
  out <- out[out!=""]
  out <- SimpleCorpus(VectorSource(out),control = list(language = "it"))
  out <- DocumentTermMatrix(
    out,
    control = list(
      stopwords = c(
        stopwords("it"),
        "essere","avere","banca","anno","considerazione","finale","essere|stare","italia")
      )
    )
}

#str_view_all(txt8,"il crisi nel mondo")
#str_view_all(txt8,"il ripercussione del crisi in Italia")
#str_view_all(txt8,"il crisi e il banca")
#str(txt8)
#txt8 <- str_split(txt8,"il crisi nel mondo", n=2,simplify = T)
#str(txt8)
#txt8 <- str_split(txt8,"il ripercussione del crisi in Italia", n=2,simplify = T)
#str(txt8)
#txt8 <- str_split(txt8,"il crisi e il banca", n=2,simplify = T)
#length(txt8)

prova <- div_test(tag_cf08,
         "il crisi nel mondo",
         "il ripercussione del crisi in Italia",
         "il crisi e il banca")
inspect(prova)
# Perfetto!
# Proviamo a fare il tutto con cf08, dopodichè lo farò per tutte

dtm_cf8 <- div_test(tag_cf08,
                    "il crisi nel mondo",
                    "il ripercussione del crisi in Italia",
                    "il crisi e il banca")

cf8_lda <- LDA(dtm_cf8,k=4,control = list(seed=1234))
cf8_lda
library(tidytext)
cf8_topics <- tidy(cf8_lda, matrix = "beta")
cf8_topics
library(ggplot2)
library(dplyr)
cf8_top_terms <- cf8_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
cf8_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(
    title = "2008",
    x=NULL,
    y=NULL
  )

plot_terms <- function(dtm, tit = "Anno?"){
  lda <- LDA(dtm,k=4,control = list(seed=1234))
  topics <- tidy(lda, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(
      title = tit,
      x=NULL,
      y=NULL
    )
}
plot_terms(dtm_cf8, tit = "Prova")
# Perfetto!

# Ora facciamolo per bene