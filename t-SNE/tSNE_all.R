load("dfr.rda")
load("ftm.rda")


library(Rtsne)
library(plotly)

x = Rtsne(ftm)


X = data.frame(x$Y)
X$case = tolower(dfr$case)
X$number = tolower(dfr$number)
X$word = tolower(dfr$word)
X$gender = tolower(dfr$gender)
X$broad_category = tolower(dfr$broad_category)
#save(X, file="X.rda")

fig_number = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~case,
              text = ~paste("case: ", case, 
                            "\nnumber: ", number, 
                            "\ngender: ", gender, 
                            "\nsupersense: ", broad_category, 
                            "\nword: ", word),
              symbol = ~number)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_number

fig_gender = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~case,
               text = ~paste("case: ", case, 
                             "\nnumber: ", number, 
                             "\ngender: ", gender, 
                             "\nsupersense: ", broad_category, 
                             "\nword: ", word),
               symbol = ~gender)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_gender

#modelling singular and plural individually
#only singulars
SG = X[X$number=="sg",]
fig_sg = plot_ly(SG,
                     x= ~X1,
                     y= ~X2,
                     color = ~case,
                     text = ~paste("case: ", case, 
                                   "\nnumber: ", number, 
                                   "\ngender: ", gender, 
                                   "\nsupersense: ", broad_category, 
                                   "\nword: ", word),
                     symbol = ~gender)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_sg

#only plurals
PL = X[X$number=="pl",]
fig_pl = plot_ly(PL,
                     x= ~X1,
                     y= ~X2,
                     color = ~case,
                     text = ~paste("case: ", case, 
                                   "\nnumber: ", number, 
                                   "\ngender: ", gender, 
                                   "\nsupersense: ", broad_category, 
                                   "\nword: ", word),
                     symbol = ~gender)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_pl

#creating a combined plot
fig_sg_pl=subplot(fig_sg, fig_pl, nrows = 1)%>% 
  layout(
    annotations=list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "singular",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "plural",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )))
fig_sg_pl

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
overview_number=subplot(fig_gen, fig_nom, fig_loc,fig_acc,fig_inst,nrows = 3)%>% 
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

overview_number

overview_gender = subplot(fig_gen2, fig_nom2, fig_loc2, fig_acc2, fig_inst2, nrows = 3)%>% 
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
overview_gender


