words = read.table("words_with_header.csv", header=TRUE, 
                   stringsAsFactors=FALSE, sep=",")
words = words[order(words$word),]
ft = read.table("fasttext_with_homographs.txt",stringsAsFactors=F)
ftm = as.matrix(ft[,-1])
rownames(ftm) = ft[,1]
ftm = ftm[order(rownames(ftm)),]

tab = table(rownames(ftm))
pos = which(rownames(ftm) %in% names(tab[tab>1]))

dfr = data.frame(pos=pos, words=rownames(ftm)[pos], stringsAsFactors=FALSE)
minpos = tapply(dfr$pos, dfr$words, min)
remove = dfr$pos[!is.element(dfr$pos, minpos)]

words2 = words[-remove,]
ftm2 = ftm[-remove,]



library(Rtsne)
library(plotly)

x = Rtsne(ftm2)


X = data.frame(x$Y)
X$case = tolower(words2$case)
X$number = tolower(words2$number)
X$noun = tolower(words2$word)
X$gender = tolower(words2$gender)
save(X, file="X.rda")

fig = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~case,
              text = ~paste("case: ", case, 
                            "\nnumber: ", number, 
                            "\nword: ", noun),
              symbol = ~gender)
fig

fig2 = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~case,
               text = ~paste("case: ", case, 
                             "\nnumber: ", number, 
                             "\nword: ", noun),
               symbol = ~number)
fig2
