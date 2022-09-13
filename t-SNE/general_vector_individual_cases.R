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
X$word = tolower(words2$word)
X$gender = tolower(words2$gender)
save(X, file="X.rda")

fig = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~case,
              text = ~paste("case: ", case, 
                            "\nnumber: ", number, 
                            "\nword: ", word),
              symbol = ~gender)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                     yaxis = list(title = list(text ='t-SNE 2')))
fig

fig2 = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~case,
               text = ~paste("case: ", case, 
                             "\nnumber: ", number, 
                             "\nword: ", word),
               symbol = ~number)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig2


#tsne plotting of individual cases
#from biggest to smallest group
#sort(table(X$case))
#
#the genitive case

G = X[X$case=="gen",]
fig_gen = plot_ly(G,
                  x= ~X1,
                  y= ~X2,
                  color = ~number,
                  text = ~paste("case: ", case,
                                "\nnumber: ", number,                      
                                "\ngender: ", gender,
                                "\nword: ", word),
                  sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_gen


fig_gen2 = plot_ly(G,
                   x= ~X1,
                   y= ~X2,
                   color = ~gender,
                   text = ~paste("case: ", case,
                                 "\nnumber: ", number,  
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_gen2


#the nominative case
N = X[X$case=="nom",]
fig_nom = plot_ly(N,
                  x= ~X1,
                  y= ~X2,
                  color = ~number,
                  text = ~paste("case: ", case,
                                "\ngender: ", gender,
                                "\nword: ", word),
                  sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_nom


fig_nom2 = plot_ly(N,
                   x= ~X1,
                   y= ~X2,
                   color = ~gender,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_nom2



#the locative case
L = X[X$case=="loc",]
fig_loc = plot_ly(L,
                  x= ~X1,
                  y= ~X2,
                  color = ~number,
                  text = ~paste("case: ", case,
                                "\ngender: ", gender,
                                "\nword: ", word),
                  sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_loc


fig_loc2 = plot_ly(L,
                   x= ~X1,
                   y= ~X2,
                   color = ~gender,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_loc2

#the accusative case
A = X[X$case=="acc",]
fig_acc = plot_ly(A,
                  x= ~X1,
                  y= ~X2,
                  color = ~number,
                  text = ~paste("case: ", case,
                                "\ngender: ", gender,
                                "\nword: ", word),
                  sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_acc


fig_acc2 = plot_ly(A,
                   x= ~X1,
                   y= ~X2,
                   color = ~gender,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_acc2


#the accusative case
I = X[X$case=="inst",]
fig_inst = plot_ly(I,
                   x= ~X1,
                   y= ~X2,
                   color = ~number,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_inst


fig_inst2 = plot_ly(I,
                    x= ~X1,
                    y= ~X2,
                    color = ~gender,
                    text = ~paste("case: ", case,
                                  "\ngender: ", gender,
                                  "\nword: ", word),
                    sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_inst2


#plotting shift vectors for dative case
#x = Rtsne(dat_shift_vec)
#error: perplexity is too large for the number of samples


#
#
#combine plots of individual cases in one overview graphic
fig_number=subplot(fig_gen, fig_nom, fig_loc,fig_acc,fig_inst,nrows = 3)%>% 
  layout(
    annotations=list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "genitive",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "nominative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.2,  
        y = 0.635,  
        text = "locative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.8,  
        y = 0.635,  
        text = "accusative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.2,  
        y = 0.3,  
        text = "instrumental",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )))

fig_number

fig_gender = subplot(fig_gen2, fig_nom2, fig_loc2, fig_acc2, fig_inst2, nrows = 3)%>% 
  layout(
    annotations = list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "genitive",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "nominative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE
      ),  
      list( 
        x = 0.2,  
        y = 0.635,  
        text = "locative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.8,  
        y = 0.635,  
        text = "accusative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.2,  
        y = 0.3,  
        text = "instrumental",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )))
fig_gender




