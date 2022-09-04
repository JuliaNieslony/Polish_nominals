setwd("~/Dropbox/Mac/Documents/General_Linguistics/Polish/fasttext_vectors")
words<-read.csv("words_annotated.csv", header=TRUE, sep= ";")
vectors<-read.table("my_embeddings.txt", header=FALSE)
words
vectors
w<-as.data.frame(words)
w
v<-as.data.frame(vectors)
v
names(w)
w[order(w$word),]
v[order(v$V1),]
names(v)[names(v)=="V1"]<-"word"
v
main<-merge(w,v, by="word")
#rownames(v)=v$V1
#rownames(v)
#V=v[,-1]
#V
#main<-w[w$words %in% rownames(v),]
main
rownames(main)
main$word
write.csv(main,"~/Dropbox/Mac/Documents/General_Linguistics/Polish/fasttext_vectors/embeddings.csv")
