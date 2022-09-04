#reading in polish dataset and English noun class system from Elnaz Shafei-Bajestan
words = read.table("words_with_header.csv", header=TRUE,
                   stringsAsFactors=FALSE, sep=",", encoding="UTF-8")
words = words[order(words$word),]
classes = read.csv("sg-pl-dataset2.csv", header=TRUE, stringsAsFactors = FALSE)
v=classes$broad_category
names(v)=classes$sg
#length(unique(classes$sg))
tab=table(classes$sg)
table(tab)
head(tab[tab==2])
classes2=unique(classes[,-1])
tab=table(classes2$sg)
table(tab)
sum(words$lemma_english %in% classes2$sg)


#merging
m<-merge.data.frame(words, classes2, by.x="lemma_english", by.y="sg")
#write.csv(m, "words_with_English_noun_classes.csv", row.names= FALSE)


#reading in embeddings
ft = read.table("fasttext_with_homographs.txt",stringsAsFactors=F)
ft = unique(ft)
#nrow(ft)
ftm = as.matrix(ft[,-1])
rownames(ftm) = ft[,1]
ftm=ftm[rownames(ftm) %in% unique(m$word),]
nrow(ftm)
ftm = ftm[order(rownames(ftm)),]
#save(ftm,file="ftm.rda")


#remove homographs
#frequency information does not differentiate case:
#sampling over homographs and chosing only one
tab = table(m$word)
m_without = m[m$word %in% names(tab[tab==1]),]
m_with= m[m$word  %in% names(tab[tab>1]),]

m_with = m_with[order(m_with$word,m_with$frequency),]
#head(m_with)

rownames(m_with) = 1:nrow(m_with)

pos=tapply(1:nrow(m_with), m_with$word, function(v)sample(v,1))
#head(pos)
m_with2= m_with[pos,]
#head(m_with2)
table(table(m_with2$word))

dfr = rbind(m_without, m_with2)
nrow(dfr)==nrow(ftm)
dfr = dfr[order(dfr$word),]
#head(dfr)
#sum(dfr$word==rownames(ftm))
#nrow(dfr)
#save(dfr,file="dfr.rda")
