getwd()
#install.packages("tidytext")

# The tidy text format ----------------------------------------------------

library(tm)
library(here)
library(tidytext)
library(dplyr)
cf_corpus <- tm::Corpus(tm::DirSource(here::here("cf_txt")))
cf08 <- cf_corpus[["cf08_considerazioni_finali.pdf.txt"]][["content"]]
text_df <- tibble(text = cf08)
text_df
text_df %>%
  unnest_tokens(word, text)

tdm = TermDocumentMatrix(cf_corpus,
                         control = list(stopwords = stopwords("italian")))
tdm$dimnames
