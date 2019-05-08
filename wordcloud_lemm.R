# Casino necessario alla wordcloud lemmatizzata
tag_cf08
df <- taggedText(tag_cf08)
str(df)
library(dplyr)

df_lem_tag <- df[,c("lemma","wclass")]
table(df[,"lemma"])
df_freq <- as.data.frame(table(df[,"lemma"]))
names(df_freq)[1] <- "lemma"
head(df_lem_tag,10)
head(df_freq,10)
nrow(df_lem_tag)
nrow(df_freq)
df_tot_rep <- left_join(df_lem_tag, df_freq, by="lemma")
df_tot <- unique(df_tot_rep)
df_tot <- df_tot[df_tot$lemma!="<unknown>",]
df_tot <- df_tot[df_tot$lemma!="banca",]
df_tot <- df_tot[df_tot$lemma!="Italia",]
df_tot <- df_tot[df_tot$lemma!="anno",]
df_tot <- df_tot[df_tot$lemma!="essere",]
df_tot <- df_tot[df_tot$lemma!="avere",]
df_tot <- df_tot[df_tot$lemma!="essere|stare",]



wordcloud(
  df_tot[df_tot$wclass=="noun","lemma"],
  df_tot[df_tot$wclass=="noun","Freq"],
  c(3,0),
  max.words = 150,
  random.order=FALSE,
  colors=rev(brewer.pal(8, "Dark2"))
)
text(0.5,1,"Wordcloud lemmi treetagger cf_08",cex=1,family = "mono", font = 2)

wordcloud(
  df_tot[df_tot$wclass=="adjective","lemma"],
  df_tot[df_tot$wclass=="adjective","Freq"],
  c(3,0),
  max.words = 150,
  random.order=FALSE,
  colors=rev(brewer.pal(8, "Dark2"))
)
text(0.5,1,"Wordcloud aggettivi treetagger cf_08",cex=1,family = "mono", font = 2)

wordcloud(
  df_tot[df_tot$wclass=="verb","lemma"],
  df_tot[df_tot$wclass=="verb","Freq"],
  c(3,0),
  max.words = 150,
  random.order=FALSE,
  colors=rev(brewer.pal(8, "Dark2"))
)
text(0.5,1,"Wordcloud aggettivi treetagger cf_08",cex=1,family = "mono", font = 2)
# abbastanza inutile la wordcloud dei verbi


# creo testi taggati divisi per periodi
tag_cf080910 <- treetag(
  here('cf_txt','cf080910.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf080910",
  stopwords=c(tm::stopwords("it"),"banca","banche","italia","anno")
)

tag_cf11121314 <- treetag(
  here('cf_txt','cf11121314.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf1121314",
  stopwords=c(tm::stopwords("it"),"banca","banche","italia","anno")
)

tag_cf151617 <- treetag(
  here('cf_txt','cf151617.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf151617",
  stopwords=c(tm::stopwords("it"),"banca","banche","italia","anno")
)

# wordcloud per periodi
df_p1 <- taggedText(tag_cf080910)
df_lem_tag_p1 <- df_p1[,c("lemma","wclass")]
df_freq_p1 <- as.data.frame(table(df_p1[,"lemma"]))
names(df_freq_p1)[1] <- "lemma"
df_tot_rep_p1 <- left_join(df_lem_tag_p1, df_freq_p1, by="lemma")
df_tot_p1 <- unique(df_tot_rep_p1)
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="<unknown>",]
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="anno",]
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="banca",]

wordcloud(
  df_tot_p1[df_tot_p1$wclass=="noun","lemma"],
  df_tot_p1[df_tot_p1$wclass=="noun","Freq"],
  c(3,0),
  max.words = 150,
  random.order=FALSE,
  colors=rev(brewer.pal(8, "Dark2"))
)
text(0.5,1,"Wordcloud lemmi treetagger periodo 1",cex=1,family = "mono", font = 2)

df_p2 <- taggedText(tag_cf11121314)
df_lem_tag_p2 <- df_p2[,c("lemma","wclass")]
df_freq_p2 <- as.data.frame(table(df_p2[,"lemma"]))
names(df_freq_p2)[1] <- "lemma"
df_tot_rep_p2 <- left_join(df_lem_tag_p2, df_freq_p2, by="lemma")
df_tot_p2 <- unique(df_tot_rep_p2)
df_tot_p2 <- df_tot_p2[df_tot_p2$lemma!="<unknown>",]
df_tot_p2 <- df_tot_p2[df_tot_p2$lemma!="anno",]
df_tot_p2 <- df_tot_p2[df_tot_p2$lemma!="banca",]

wordcloud(
  df_tot_p2[df_tot_p2$wclass=="noun","lemma"],
  df_tot_p2[df_tot_p2$wclass=="noun","Freq"],
  c(3,0),
  max.words = 150,
  random.order=FALSE,
  colors=rev(brewer.pal(8, "Dark2"))
)
text(0.5,1,"Wordcloud lemmi treetagger periodo 2",cex=1,family = "mono", font = 2)

df_p3 <- taggedText(tag_cf151617)
df_lem_tag_p3 <- df_p3[,c("lemma","wclass")]
df_freq_p3 <- as.data.frame(table(df_p3[,"lemma"]))
names(df_freq_p3)[1] <- "lemma"
df_tot_rep_p3 <- left_join(df_lem_tag_p3, df_freq_p3, by="lemma")
df_tot_p3 <- unique(df_tot_rep_p3)
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="<unknown>",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="anno",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="banca",]

wordcloud(
  df_tot_p3[df_tot_p3$wclass=="noun","lemma"],
  df_tot_p3[df_tot_p3$wclass=="noun","Freq"],
  c(3,0),
  max.words = 150,
  random.order=FALSE,
  colors=rev(brewer.pal(8, "Dark2"))
)
text(0.5,1,"Wordcloud lemmi treetagger periodo 3",cex=1,family = "mono", font = 2)

# Passiamo alle comparison cloud
head(df_tot_p1)
head(df_tot_p2)
head(df_tot_p3)
df_tot_p12 <- left_join(df_tot_p1, df_tot_p2, by = "lemma")
df_tot_p123 <- left_join(df_tot_p12, df_tot_p3, by = "lemma")
names(df_tot_p123)[c(3,5,7)] <- c("2008-2010","2011-2014","2014-2017")
df_tot_p123 <- unique(df_tot_p123[,c(1,3,5,6,7)])
head(df_tot_p123)

nrow(df_tot_p123)
!duplicated(df_tot_p123$lemma)
nrow(df_tot_p123[!duplicated(df_tot_p123$lemma),])

df_tot_p123 <- df_tot_p123[!duplicated(df_tot_p123$lemma),]

rownames(df_tot_p123) <- df_tot_p123$lemma

head(df_tot_p123)

na.omit(df_tot_p123)
comparison.cloud(
  na.omit(df_tot_p123[df_tot_p123$wclass=="noun",c("2008-2010","2011-2014","2014-2017")]),
  scale=c(3,.5),
  max.words = 250,
  random.order=FALSE,
  title.colors = "gold3",
  title.size = 1.5,
  use.r.layout = FALSE,
  title.bg.colors = "grey2",
  col = c("dodgerblue4","brown","darkgreen")
)
