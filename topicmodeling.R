# Topic modeling
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
