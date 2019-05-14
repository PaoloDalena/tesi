# Casino necessario alla wordcloud lemmatizzata
library(koRpus)
library(koRpus.lang.it)
library(dplyr)
library(wordcloud)

tag_cf08
df <- taggedText(tag_cf08)
str(df)


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
  colors=rev(brewer.pal(8, "Dark2")),
  family = "mono"
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
mystop <- c("banca","banche","italia","anno","anni","considerazione",
            "considerazioni","finale","paese","paesi")

tag_cf080910 <- treetag(
  here('cf_txt','cf080910.txt'),
  treetagger="manual",
  lang="it",
  TT.options=list(
    path="/Users/PaoloMacbook/Desktop/lemm",
    preset="it"
  ),
  doc_id="cf080910",
  stopwords=c(tm::stopwords("it"),mystop)
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
  stopwords=c(tm::stopwords("it"), mystop)
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
  stopwords=c(tm::stopwords("it"), mystop)
)

# wordcloud per periodi
df_p1 <- taggedText(tag_cf080910)
df_lem_tag_p1 <- df_p1[,c("lemma","wclass")]
df_freq_p1 <- as.data.frame(table(df_p1[,"lemma"]))
names(df_freq_p1)[1] <- "lemma"
library(dplyr)
df_tot_rep_p1 <- left_join(df_lem_tag_p1, df_freq_p1, by="lemma")
df_tot_p1 <- unique(df_tot_rep_p1)
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="<unknown>",]
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="anno",]
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="banca",]
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="paese",]
df_tot_p1 <- df_tot_p1[df_tot_p1$lemma!="considerazione",]

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
df_tot_p2 <- df_tot_p2[df_tot_p2$lemma!="considerazione",]
df_tot_p2 <- df_tot_p2[df_tot_p2$lemma!="paese",]
df_tot_p2 <- df_tot_p2[df_tot_p2$lemma!="miliardo",]

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
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="potere",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="crisi",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="mercato",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="paese",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="considerazione",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="governatore",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="europeo",]
df_tot_p3 <- df_tot_p3[df_tot_p3$lemma!="economia",]


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
#!duplicated(df_tot_p123$lemma)
nrow(df_tot_p123[!duplicated(df_tot_p123$lemma),])

df_tot_p123 <- df_tot_p123[!duplicated(df_tot_p123$lemma),]

rownames(df_tot_p123) <- df_tot_p123$lemma

head(df_tot_p123)

na.omit(df_tot_p123)


df_tot_p123 <- df_tot_p123[df_tot_p123$lemma!="governatore",]
df_tot_p123 <- df_tot_p123[df_tot_p123$lemma!="relazione",]

par(mar=rep(0,4))
comparison.cloud(
  na.omit(df_tot_p123[df_tot_p123$wclass=="noun",c("2008-2010","2011-2014","2014-2017")]),
  scale=c(3,.5),
  max.words = 250,
  random.order=FALSE,
  title.colors = "gold3",
  title.size = 1.5,
  use.r.layout = TRUE,
  title.bg.colors = "grey2",
  col = c("dodgerblue4","brown","darkgreen")
)

# Commonality cloud
commonality.cloud(
  na.omit(df_tot_p123[df_tot_p123$wclass=="noun",c("2008-2010","2011-2014","2014-2017")]),
  scale=c(3,.5),
  max.words = 150,
  random.order=FALSE,
  colors=rev(brewer.pal(3, "Spectral"))
)

# wordcloud2
remove.packages("wordcloud2")
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
df_tot_p1_o <- df_tot_p1%>%
  arrange(desc(Freq))
wordcloud2(
  df_tot_p1_o[df_tot_p1_o$wclass=="noun",c(1,3)],
  size = 0.4,
  minSize = 0.1,
  rotateRatio = 0.6,
  fontFamily = "Courier",
  color = rep(c('pink', 'hotpink'), length.out=nrow(df_tot_p1_o)),
  backgroundColor = "black",
  figPath = here('img','pig2.png'),
  fontWeight = "600"
)
colors()[134]
?wordcloud2
?display.brewer.all()
df_tot_p2_o <- df_tot_p2%>%
  arrange(desc(Freq))
wordcloud2(
  df_tot_p2_o[df_tot_p2_o$wclass=="noun",c(1,3)],
  size = 0.4,
  minSize = 0,
  rotateRatio = 0.6,
  fontFamily = "Courier",
  color = rep(c('gold', 'darkgoldenrod'), length.out=nrow(df_tot_p2_o)),
  backgroundColor = "navy",
  figPath = here('img','euro1.png'),
  fontWeight = "600"
)

df_tot_p3_o <- df_tot_p3%>%
  arrange(desc(Freq))
wordcloud2(
  df_tot_p3_o[df_tot_p3_o$wclass=="noun",c(1,3)],
  size = 0.3,
  minSize = 0,
  rotateRatio = 0.6,
  fontFamily = "Courier",
  color = rep(c('red', 'firebrick'), length.out=nrow(df_tot_p3_o)),
  backgroundColor = "black",
  figPath = here('img','debt.png'),
  fontWeight = "600"
)
#reale per pubblico è 99
df_tot_p3[df_tot_p3$lemma=="pubblico","Freq"] <- 70
df_tot_p3[df_tot_p3$lemma=="pubblico",]
head(df_tot_p3_o[df_tot_p3_o$wclass=="noun",c(1,3)],15)

?wordcloud2
letterCloud(
  df_tot_p2[df_tot_p2$wclass=="noun",c(1,3)],
  "123",
  size = 1.2,
  minSize = 0,
  fontFamily = "Arial",
  rotateRatio = 0.6,
  color = "random-light",
  backgroundColor = "grey2"
)