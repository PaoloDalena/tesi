
# Funzioni superfighe da utilizzare ---------------------------------------
library(topicmodels)
library(tidytext)

clean_text <- function(x) {
  out <- taggedText(x)
  out <- out$lemma
  out %>%
    paste(collapse = " ") %>%
    str_remove_all("@card@") %>%
    str_remove_all("<unknown>")
}

div_text <- function(x, key1, key2, key3) {
  out <- taggedText(x)
  out <- out$lemma
  out <- out %>%
    paste(collapse = " ") %>%
    str_remove_all("@card@") %>%
    str_remove_all("<unknown>")
  out <- out %>%
    str_split_fixed(key1, n = 2) %>%
    str_split_fixed(key2, n = 2) %>%
    str_split_fixed(key3, n = 2)
  out <- out[out != ""]
  out <- SimpleCorpus(VectorSource(out), control = list(language = "it"))
  out <- DocumentTermMatrix(
    out,
    control = list(
      stopwords = c(
        stopwords("it"),
        "essere", "avere", "banca", "anno", "considerazione", "finale",
        "essere|stare", "italia", "paese", "cento","stare","oltre","lungo",
        "fare"
      )
    )
  )
}

plot_terms <- function(dtm, tit = "Anno?") {
  lda <- LDA(dtm, k = 4, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(4, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free") +
    coord_flip() +
    labs(
      title = tit,
      x = NULL,
      y = NULL
    )
}

# Analisi anno per anno ---------------------------------------------------

# cf08
txt8 <- clean_text(tag_cf08)
str_view_all(txt8, "il crisi nel mondo")
str_view_all(txt8, "il ripercussione del crisi in Italia")
str_view_all(txt8, "il crisi e il banca")
dtm8 <- div_text(
  tag_cf08,
  "il crisi nel mondo",
  "il ripercussione del crisi in Italia",
  "il crisi e il banca"
)
top8 <- plot_terms(dtm8, tit = "2008")
top8
top8[1]
a <- taggedText(tag_cf08)
a <- a$lemma
a <- a %>%
  paste(collapse = " ") %>%
  str_remove_all("@card@") %>%
  str_remove_all("<unknown>")
a <- SimpleCorpus(VectorSource(a), control = list(language = "it"))
a <- DocumentTermMatrix(
  a,
  control = list(
    stopwords = c(
      stopwords("it"),
      "essere", "avere", "banca", "anno", "considerazione", "finale",
      "essere|stare", "italia", "paese", "cento"
    )))
plot_terms(a)
# cf09
txt9 <- clean_text(tag_cf09)
str_view_all(txt9, "cooperazione internazionale")
str_extract_all(txt9, "cooperazione internazionale")
str_extract_all(txt9, "il area del euro il politica")
str_extract_all(txt9, "banca , vigilanza")
dtm9 <- div_text(
  tag_cf09,
  "cooperazione internazionale",
  "il area del euro il politica",
  "banca , vigilanza"
)
top9 <- plot_terms(dtm9, tit = "2009")
top9

# cf10
txt10 <- clean_text(tag_cf10)
str_view_all(txt10, "il mondo dopo il crisi")
str_extract_all(txt10, "il mondo dopo il crisi")
str_extract_all(txt10, "il economia italiano in Italia")
str_extract_all(txt10, "banca e vigilanza")
dtm10 <- div_text(
  tag_cf10,
  "il mondo dopo il crisi",
  "il economia italiano in Italia",
  "banca e vigilanza"
)
top10 <- plot_terms(dtm10, tit = "2010")
top10

# cf11
txt11 <- clean_text(tag_cf11)
str_view_all(txt11, "il economia e il politica monetario")
str_extract_all(txt11, "il economia e il politica monetario")
str_extract_all(txt11, "il sistema finanziario")
str_extract_all(txt11, "il Europa e il Italia se")
dtm11 <- div_text(
  tag_cf11,
  "il economia e il politica monetario",
  "il sistema finanziario",
  "il Europa e il Italia se"
)
top11 <- plot_terms(dtm11, tit = "2011")
top11

# cf12
txt12 <- clean_text(tag_cf12)
str_view_all(txt12, "il politica monetario nel")
str_extract_all(txt12, "il politica monetario nel")
str_extract_all(txt12, "il economia italiano")
str_extract_all(txt12, "il banca e il credito")
dtm12 <- div_text(
  tag_cf12,
  "il politica monetario nel",
  "il economia italiano",
  "il banca e il credito"
)
top12 <- plot_terms(dtm12, tit = "2012")
top12

# cf13
txt13 <- clean_text(tag_cf13)
str_view_all(txt13, "istituzione aperto")
str_extract_all(txt13, "istituzione aperto")
str_extract_all(txt13, "uscita dal crisi")
str_extract_all(txt13, "il banca , il credito")
dtm13 <- div_text(
  tag_cf13,
  "istituzione aperto",
  "uscita dal crisi",
  "il banca , il credito"
)
top13 <- plot_terms(dtm13, tit = "2013")
top13

# cf14
txt14 <- clean_text(tag_cf14)
str_view_all(txt14, "il politica monetario e il crescita")
str_extract_all(txt14, "il politica monetario e il crescita")
str_extract_all(txt14, "il banca e")
str_extract_all(txt14, "il vigilanza e")
dtm14 <- div_text(
  tag_cf14,
  "il politica monetario e il crescita",
  "il banca e",
  "il vigilanza e"
)
top14 <- plot_terms(dtm14, tit = "2014")
top14

# cf15
txt15 <- clean_text(tag_cf15)
str_view_all(txt15, "il risposta del politica")
str_extract_all(txt15, "il risposta del politica")
str_extract_all(txt15, "il costruzione europeo :")
str_extract_all(txt15, "oggi aprire")
dtm15 <- div_text(
  tag_cf15,
  "il risposta del politica",
  "il costruzione europeo :",
  "oggi aprire"
)
top15 <- plot_terms(dtm15, tit = "2015")
top15

# cf16
txt16 <- clean_text(tag_cf16)
str_view_all(txt16, "il anno del crisi")
str_extract_all(txt16, "il anno del crisi")
str_extract_all(txt16, "lavoro e crescita")
str_extract_all(txt16, "il azione di vigilanza e il sfida")
dtm16 <- div_text(
  tag_cf16,
  "il anno del crisi",
  "lavoro e crescita",
  "il azione di vigilanza e il sfida"
)
top16 <- plot_terms(dtm16, tit = "2016")
top16

# cf17
txt17 <- clean_text(tag_cf17)
str_view_all(txt17, "il economia italiano oggi")
str_extract_all(txt17, "il economia italiano oggi")
str_extract_all(txt17, "il finanza pubblico")
str_extract_all(txt17, "patrimonio informativo")
dtm17 <- div_text(
  tag_cf17,
  "il economia italiano oggi",
  "il finanza pubblico",
  "patrimonio informativo"
)
top17 <- plot_terms(dtm17, tit = "2017")
top17

eplot_grid(top8, top9, top10,
  nrow = 2,
  ncol = 2
)
plot_grid(top11, top12, top13, top14,
  nrow = 2,
  ncol = 2
)
plot_grid(top15, top16, top17,
  nrow = 2,
  ncol = 2
)

?LDA
data("AssociatedPress", package = "topicmodels")
lda <- LDA(AssociatedPress[1:20,], control = list(alpha = 0.1), k = 2)
lda_inf <- posterior(lda, AssociatedPress[21:30,])
lda_inf$topics
