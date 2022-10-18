load("dfr.rda")
load("ftm.rda")


library(Rtsne)
library(plotly)
library(rstatix)


x = Rtsne(ftm)


#X = data.frame(x$Y)
#X$case = tolower(dfr$case)
#X$number = tolower(dfr$number)
#X$word = tolower(dfr$word)
#X$gender = tolower(dfr$gender)
#X$broad_category = tolower(dfr$broad_category)
#save(X, file="X.rda")

load("X.rda")

#identifying outliers 
outliers = data.frame(identify_outliers(X, X1))
#excluding outliers
X1=X[!is.element(X$word,outliers$word),]

#t-SNE with outliers
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
#saved as 'general_case_number.html'


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
#saved as 'general_case_gender.html'

#without outliers
fig_number1 = plot_ly(X1,
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
fig_number1
#saved as 'general_case_number_o.html'


fig_gender1 = plot_ly(X1,
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
fig_gender1
#saved as 'general_case_gender_o.html'


#modelling singular and plural individually
#only singulars
SG = X[X$number=="sg",]

#outlier detection
oSG = identify_outliers(SG, X1)
SG1 = SG[!is.element(SG$word,oSG$word),]

#t-SNE with outliers
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

#outlier detection
oPL = identify_outliers(PL, X1)
PL1 = PL[!is.element(PL$word,oPL$word),]

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
#saved as 'sg_vs_pl_case_gender.html'


#t-SNE without outliers
fig_sg1 = plot_ly(SG1,
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
fig_sg1

fig_pl1 = plot_ly(PL1,
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
fig_pl1

#creating a combined plot
fig_sg_pl1=subplot(fig_sg1, fig_pl1, nrows = 1)%>% 
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
fig_sg_pl1
#saved as 'sg_vs_pl_case_gender_o.html'


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




#outlier detection
oG = identify_outliers(G, X1)
G1 = G[!is.element(G$word,oG$word),]

#t-SNE without outliers
fig_gen1 = plot_ly(G1,
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
fig_gen1


fig_gen3 = plot_ly(G1,
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
fig_gen3




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


#outlier detection
oN = identify_outliers(N, X1)
N1 = N[!is.element(N$word,oN$word),]

#t-SNE without outliers
fig_nom1 = plot_ly(N1,
                  x= ~X1,
                  y= ~X2,
                  color = ~number,
                  text = ~paste("case: ", case,
                                "\ngender: ", gender,
                                "\nword: ", word),
                  sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_nom1


fig_nom3 = plot_ly(N1,
                   x= ~X1,
                   y= ~X2,
                   color = ~gender,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_nom3


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

#outlier detection
oL = identify_outliers(L, X1)
L1 = L[!is.element(L$word,oL$word),]

#t-SNE without outliers
fig_loc1 = plot_ly(L1,
                  x= ~X1,
                  y= ~X2,
                  color = ~number,
                  text = ~paste("case: ", case,
                                "\ngender: ", gender,
                                "\nword: ", word),
                  sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_loc1


fig_loc3 = plot_ly(L1,
                   x= ~X1,
                   y= ~X2,
                   color = ~gender,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_loc3

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


#outlier detection
oA = identify_outliers(A, X1)
A1 = A[!is.element(A$word,oA$word),]

#t-SNE without outliers
fig_acc1 = plot_ly(A1,
                  x= ~X1,
                  y= ~X2,
                  color = ~number,
                  text = ~paste("case: ", case,
                                "\ngender: ", gender,
                                "\nword: ", word),
                  sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_acc1


fig_acc3 = plot_ly(A1,
                   x= ~X1,
                   y= ~X2,
                   color = ~gender,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_acc3

#the instrumental case
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


#outlier detection
oI = identify_outliers(I, X1)
I1 = I[!is.element(I$word,oI$word),]

#t-SNE without outliers
fig_inst1 = plot_ly(I1,
                   x= ~X1,
                   y= ~X2,
                   color = ~number,
                   text = ~paste("case: ", case,
                                 "\ngender: ", gender,
                                 "\nword: ", word),
                   sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_inst1


fig_inst3 = plot_ly(I1,
                    x= ~X1,
                    y= ~X2,
                    color = ~gender,
                    text = ~paste("case: ", case,
                                  "\ngender: ", gender,
                                  "\nword: ", word),
                    sizes=2)%>% 
  layout(xaxis = list(title = list(text ='t-SNE 1')),
         yaxis = list(title = list(text ='t-SNE 2')))
fig_inst3



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
#saved as 'overview_case_number.html'


#keeping only one legend for figures in report
overview_number2=subplot(fig_gen, style(fig_nom, showlegend=F), style(fig_loc, showlegend=F),style(fig_acc, showlegend=F),style(fig_inst, showlegend=F),nrows = 3)%>% 
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

overview_number2


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
#saved as 'overview_case_gender.html'

#keeping only one legend for figures in report

overview_gender2 = subplot(fig_gen2, style(fig_nom2, showlegend=F), style(fig_loc2, showlegend=F),style(fig_acc2, showlegend=F),style(fig_inst2, showlegend=F),nrows = 3)%>% 
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
overview_gender2






####
#without outliers
overview_number1=subplot(fig_gen1, fig_nom1, fig_loc1,fig_acc1,fig_inst1,nrows = 3)%>% 
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

overview_number1
#saved as 'overview_case_number_o.html'


#keeping only one legend for figures in report
overview_number1.2=subplot(fig_gen1, style(fig_nom1, showlegend=F), style(fig_loc1, showlegend=F),style(fig_acc1, showlegend=F),style(fig_inst1, showlegend=F),nrows = 3)%>% 
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

overview_number1.2


overview_gender1 = subplot(fig_gen3, fig_nom3, fig_loc3, fig_acc3, fig_inst3, nrows = 3)%>% 
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
overview_gender1
#saved as 'overview_case_gender_o.html'

#keeping only one legend for figures in report

overview_gender1.2 = subplot(fig_gen3, style(fig_nom3, showlegend=F), style(fig_loc3, showlegend=F),style(fig_acc3, showlegend=F),style(fig_inst3, showlegend=F),nrows = 3)%>% 
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
overview_gender1.2

