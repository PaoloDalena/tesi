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
str(desc[[1]])
# Numero delle parole
par(mfrow=c(2,2))
numero_parole <- NULL
for (i in 1:10) {
  numero_parole[i] <-  desc[[i]]$words
}
numero_parole
mean(numero_parole)
barplot(numero_parole,
        names.arg = 2008:2017,
        xlab = "Anni",
        ylab = "Numero di parole",
        main = "Numero di parole nelle cf negli anni",
        col = rainbow(10))
grid()

# Numero delle frasi
numero_frasi <- NULL
for (i in 1:10) {
  numero_frasi[i] <-  desc[[i]]$sentences
}
numero_frasi
mean(numero_frasi)
barplot(numero_frasi,
        names.arg = 2008:2017,
        xlab = "Anni",
        ylab = "Numero di frasi",
        main = "Numero di frasi nelle cf negli anni",
        col = rainbow(10))
grid()

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
mean(media_frasi)
mean(media_frasi[5:10])
barplot(media_frasi,
        names.arg = 2008:2017,
        xlab = "Anni",
        ylab = "Lunghezza media frasi",
        ylim = c(15,25),
        xpd = F,
        main = "Lunghezza media delle frasi nelle cf negli anni",
        col = rainbow(10))
grid()


#par(mfrow=c(2,2))


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

# Facciamolo manualmente solo con ciò che mi interessa
table(taggedText(tag_cf08)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")]
pie(
  table(
    taggedText(tag_cf08)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf08"
)

pie(
  table(
    taggedText(tag_cf09)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf09"
)

pie(
  table(
    taggedText(tag_cf10)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf10"
)

pie(
  table(
    taggedText(tag_cf11)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf11"
)

pie(
  table(
    taggedText(tag_cf12)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf12"
)

pie(
  table(
    taggedText(tag_cf13)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf13"
)

pie(
  table(
    taggedText(tag_cf14)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf14"
)

pie(
  table(
    taggedText(tag_cf15)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf15"
)

pie(
  table(
    taggedText(tag_cf16)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf16"
)

pie(
  table(
    taggedText(tag_cf17)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")],
  col = brewer.pal(5, "Blues"),
  main = "Parti del discorso cf17"
)
par(mfrow = c(3,3)) # praticamente tutti la stessa cosa
par(mar=c(0,0,0,0))


# Grafici con ggplot2 -----------------------------------------------------

# rendo i grafici più fichi con ggplot
library(ggplot2)
qplot(x = cty, y = hwy, color = cyl, data = mpg, geom = "point")
mpg
ggplot(data = mpg, aes(x=cty,y=hwy)) +
  geom_point(aes(color = cyl)) +
  geom_smooth(method ="lm") +
  coord_cartesian() + 
  scale_color_gradient() +
  theme_bw()

num_MTLDs

dfMTLD <- data.frame(
  MTDL = num_MTLDs,
  numero_frasi,
  numero_parole,
  "Lunghezza media frasi" = media_frasi,
  media_parole,
  Anni = 2008:2017)

dfMTLD

ggplot(data = dfMTLD,aes(x = Anni, y = MTDL)) +
  geom_point(shape=media_frasi) + 
  geom_line(size=0.1) +
  labs(title="MTDL negli anni", x="Anni", y="MTDL")


ggplot(dfMTLD, aes(x = Anni))+
  geom_line(aes(y = MTDL, colour = "MTDL"))+
  geom_line(aes(y = media_frasi, colour = "media_frasi"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Numero medio di frasi"))+
  scale_colour_manual(values = c("blue", "red"))+
  theme(legend.position = c(0.65, 0.9))
#ci siamo quasi, devo solo cambiare la scala

#grafico potentissimo per il confronto lunghezza frasi/MTLD
ggplot(dfMTLD, aes(x = Anni)) +
  geom_point(aes(y=media_frasi, color="Lunghezza media frasi")) +
  geom_line(aes(y=media_frasi, color="Lunghezza media frasi")) +
  geom_point(aes(y=MTDL/8.5, color="MTLD")) +
  geom_line(aes(y=MTDL/8.5, color="MTLD")) +
  scale_y_continuous(
    minor_breaks = c(18.83,21.18,22.35),
    sec.axis = sec_axis(~.*8.5, name = "Measure of Textual Lexical Diversity")
    ) +
  scale_colour_manual(values = c("grey2", "chartreuse3"))+
  labs(
    title = "MTLD e Lunghezza media delle frasi a confronto",
    y = "Numero medio di parole per frase",
    x = "Anni",
    colour = NULL
    ) +
  theme_grey()+
  theme(legend.position = c(0.7,0.2))+
  theme(axis.text.y.left = element_text( color ="grey2" )) +
  theme(axis.title.y.left = element_text( color ="grey2" )) +
  theme(axis.title.y.right = element_text( color ="chartreuse3" )) +
  theme(axis.text.y.right = element_text( color ="chartreuse3" )) +
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = seq(2008,2017,1)
    )+
  theme(
    plot.title = element_text(color = "grey2", size=16, face="bold")
  )+
  background_grid(
    colour.major ="grey2" ,
    colour.minor = "chartreuse2",
    size.major = 0.1,
    size.minor = 0.2
    )
ggplot2::ggsave("MTLD_frasi.png")
?ggsave
library(cowplot)
#con i labels, figo ma incomprensibile
head(dfMTLD,5)
plot1 <- ggplot(dfMTLD,aes(x=Anni)) +
  geom_line(aes(y=MTDL)) +
  geom_label(aes(y=MTDL,label=2008:2017),
             color="chartreuse3",
             fill="white",
             show.legend = T) +
  geom_line(aes(y=MTDL), size=0.05)+
  theme_gray()

plot2 <- ggplot(dfMTLD,aes(x=Anni)) +
  geom_line(aes(y=numero_parole)) +
  geom_label(aes(y=numero_parole,label=2008:2017),
             color="blue",
             fill="white",
             show.legend = T) +
  geom_line(aes(y=numero_parole), size=0.05)+
  theme_gray()

plot3 <- ggplot(dfMTLD,aes(x=Anni)) +
  geom_line(aes(y=numero_frasi)) +
  geom_label(aes(y=numero_frasi,label=2008:2017),
             color="red",
             fill="white",
             show.legend = T) +
  geom_line(aes(y=numero_frasi), size=0.05)+
  theme_gray()

plot4 <- ggplot(dfMTLD,aes(x=Anni)) +
  geom_line(aes(y=media_frasi)) +
  geom_label(aes(y=media_frasi,label=2008:2017),
             color="grey2",
             fill="white",
             show.legend = T) +
  geom_line(aes(y=media_frasi), size=0.05)+
  theme_gray()

a <- plot_grid(plot2, plot3, plot4, nrow=1,
          align = 'v', axis = 'l') # aligning vertically along the left axis
plot_grid(a,plot1, nrow=2)

#grafico riassuntivo delel diff lessicali
plot1 <- ggplot(dfMTLD,aes(x=Anni)) +
  geom_line(aes(y=MTDL), color="chartreuse3") +
  geom_point(aes(y=MTDL),color=c(rep(1,5),2,1,2,2,1))+
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = seq(2008,2017,1)
  ) +
  labs(
    title = "Measure of Textual Lexical Diversity",
    x=NULL,
    y=NULL
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(color = "chartreuse3")
  )



plot2 <- ggplot(dfMTLD,aes(x=seq(8,17,1))) +
  geom_line(aes(y=numero_parole), color="blue") +
  geom_point(aes(y=numero_parole),color=c(rep(1,5),2,1,2,2,1))+
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = seq(8,17,1)
  ) +
  labs(
    title = "Numero di parole",
    x=NULL,
    y=NULL
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(color = "blue")
  )


plot3 <- ggplot(dfMTLD,aes(x=seq(8,17,1))) +
  geom_line(aes(y=numero_frasi), color="red") +
  geom_point(aes(y=numero_frasi),color=c(rep(1,5),2,1,2,2,1))+
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = seq(8,17,1)
  ) +
  labs(
    title = "Numero di frasi",
    x=NULL,
    y=NULL
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(color = "red")
  )


plot4 <- ggplot(dfMTLD,aes(x=seq(8,17,1))) +
  geom_line(aes(y=media_frasi), color="grey2") +
  geom_point(aes(y=media_frasi),color=c(rep(1,5),2,1,2,2,1))+
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = seq(8,17,1)
  ) +
  labs(
    title = "Numero medio di parole per frase",
    x=NULL,
    y=NULL
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(color = "grey2")
  )

a <- plot_grid(plot2, plot3, plot4, nrow=1,
               align = 'v', axis = 'l') # aligning vertically along the left axis
plot_grid(a,plot1, nrow=2)
ggplot2::ggsave("MTLD_sunto_2.png")

#pie per le parti del testo
pie <- table(taggedText(tag_cf080910)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")]+table(taggedText(tag_cf11121314)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")]+table(taggedText(tag_cf151617)[,"wclass"])[c("noun","verb","adjective","pronoun","adverb")]
pie
pie(pie)
pie <- as.data.frame(pie)
library(scales)
percent(pie$Freq/sum(pie$Freq))

ggplot(pie, aes(x="", y=Freq, fill=Parte_del_discorso)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0, direction = 2) +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Freq/5 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = percent(Freq/48860)), size=3)

pie <- pie %>%
  mutate(Prop = round(pie$Freq/sum(pie$Freq),2)) %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)
pie

ggplot(pie, aes(x = "", y = Prop, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = percent(Prop,accuracy = 1)), color = "black")+
  scale_fill_brewer(palette = "BrBG") +
  theme_void()+
  labs(
    title = "Distribuzione delle principali parti del dicorso",
    x=NULL,
    y=NULL
    ) +
  theme(
  plot.title = element_text(color = "grey2",size = 16),
  legend.title = element_text(color="white")
  )

ggplot2::ggsave("pie_partidiscorso.png")

# nuova pie con parti variabili e invariabili del discorso
# parti variabili
variabili <- table(taggedText(tag_cf080910)[,"wclass"])[c("noun","verb","adjective","pronoun","determiner")]+table(taggedText(tag_cf11121314)[,"wclass"])[c("noun","verb","adjective","pronoun","determiner")]+table(taggedText(tag_cf151617)[,"wclass"])[c("noun","verb","adjective","pronoun","determiner")]
variabili
pie(variabili)
variabili <- as.data.frame(variabili)
variabili <- variabili %>%
  mutate(Prop = round(variabili$Freq/sum(variabili$Freq),2)) %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)


var <- ggplot(variabili, aes(x = "", y = Prop, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = percent(Prop,accuracy = 1)), color = "black")+
  scale_fill_brewer(palette = "Reds",labels = c("Nomi", "Verbi", "Aggettivi","Pronomi","Articoli")) +
  theme_void()+
  #labs(
  #  title = "(c)",
  #  x=NULL,
  #  y=NULL
  #)+
  theme(
    plot.title = element_text(color = "grey2",size = 16),
    legend.title = element_text(color="white")
  )

# invariabili
invariabili <- table(taggedText(tag_cf080910)[,"wclass"])[c("preposition","conjunction","adverb")]+table(taggedText(tag_cf11121314)[,"wclass"])[c("preposition","conjunction","adverb")]+table(taggedText(tag_cf151617)[,"wclass"])[c("preposition","conjunction","adverb")]
invariabili
pie(invariabili)
invariabili <- as.data.frame(invariabili)
invariabili <- invariabili %>%
  mutate(Prop = round(invariabili$Freq/sum(invariabili$Freq),2)) %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)

invariabili

invar <- ggplot(invariabili, aes(x = "", y = Prop, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = percent(Prop,accuracy = 1)), color = "black")+
  scale_fill_brewer(palette = "Greens",labels = c("Preposizioni", "Congiunzioni", "Avverbi")) +
  theme_void()+
  #labs(
  #  title = "(b)",
  #  x=NULL,
  #  y=NULL
  #)+
  theme(
    plot.title = element_text(color = "grey2",size = 16),
    legend.title = element_text(color="white")
  )

# ora vorrei vedere quanta parte del discorso è variabile e quanta invariabile
sum(variabili$Freq)
sum(invariabili$Freq)
varinvar <- data.frame("Var1"=c("variabili","invariabili"),"Freq"=c(sum(variabili$Freq),sum(invariabili$Freq)))
varinvar <- varinvar %>%
  mutate(Prop = round(varinvar$Freq/sum(varinvar$Freq),2)) %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)

varinvar

varinvarp <- ggplot(varinvar, aes(x = "", y = Prop, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = percent(Prop,accuracy = 1)), color = "black")+
  scale_fill_manual(values=c("#41AB5D","#CB181D"),labels = c("Parti invariabili", "Parti variabili")) +
  theme_void()+
  #labs(
  #  title = "(a)",
  #  x=NULL,
  #  y=NULL
  #)+
  theme(
    plot.title = element_text(color = "grey2",size = 16),
    legend.title = element_text(color="white")
  )

library(RColorBrewer)
brewer.pal(8,"Greens")
brewer.pal(8,"Reds")

library(cowplot)
b <- plot_grid(invar, var, nrow=1,
                    align = 'v', axis = 'l')
plot_grid(varinvarp,b, nrow=2)
ggsave("distribuzione_sunto.png")
