# Sentyment analysis
library(tm)
# carico myclasspolarity e myclassemotion
ricTxtLem(taggedText(tag_cf08))
tag_cf08[,"lemma"]
pol08 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf08)),
  lexicon = here('functions','subjectivity_it_lem.csv')
  )
str(pol08)
pol08$parole$category
prop.table(table(pol08$parole$category))
barplot(prop.table(table(pol08$parole$category))*100,ylim=c(0,60), ylab="valori %", cex.lab=0.8,cex.axis = 0.7,cex.names = 0.9)
grid(nx = 0,ny = 6,col = "gray50")

pol09 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf09)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
prop.table(table(pol09$parole$category))
barplot(prop.table(table(pol09$parole$category))*100,ylim=c(0,60), ylab="valori %", cex.lab=0.8,cex.axis = 0.7,cex.names = 0.9)
grid(nx = 0,ny = 6,col = "gray50")

# una volta compreso per bene il funzionamento procedo con la creazione
# di un vettore con i valori di positività e negatività nel tempo
pol8 <- pol08
pol9 <- pol09
rm(pol08,pol09)
pol10 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf10)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
pol11 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf11)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
pol12 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf12)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
pol13 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf13)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
pol14 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf14)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
pol15 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf15)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
pol16 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf16)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)
pol17 <- myClassPolarity(
  ricTxtLem(taggedText(tag_cf17)),
  lexicon = here('functions','subjectivity_it_lem.csv')
)

prop.table(table(pol8$parole$category))["negativo"]
pol.tot <- list(pol8,pol9,pol10,pol11,pol12,pol13,pol14,pol15,pol16,pol17)
pol.tot[[1]]$parole$category

pos <- NULL
neg <- NULL
for (i in 1:10){
  prop.table(table(pol.tot[[i]]$parole$category))["positivo"]
  pos[i] <- prop.table(table(pol.tot[[i]]$parole$category))["positivo"]
  neg[i] <- prop.table(table(pol.tot[[i]]$parole$category))["negativo"]
}
pos
neg

df_sent <- data.frame(
  "Anno"=2008:2017,
  "Positivi"=pos,
  "Negativi"= neg
)

ggplot(df_sent,aes(x=Anno)) +
  geom_line(aes(y=pos),col="darkgreen")+
  geom_label(aes(y=pos, label=percent(pos)),
             color="grey2",
             fill= c ("orange1",
                      "green2",
                      "green3",
                      "gold",
                      "gold",
                      "green",
                      "yellow",
                      "orangered",
                      "yellow",
                      "orange1"))+
  geom_line(aes(y=mean(pos)),col="green3",size=0.5)+
  #geom_point(aes(y=pos),col="darkgreen") +
  theme_gray()+
  labs(
    title = "Andamento dei lemmi positivi",
    y = "Percentuale sul totale dei lemmi",
    x = "Anni"
  )+
    scale_x_continuous(
      minor_breaks = NULL,
      breaks = seq(2008,2017,1)
    )+
    theme(
      plot.title = element_text(color = "grey2", size=16, face="bold")
    )

ggplot2::ggsave("sent_pos.png")
