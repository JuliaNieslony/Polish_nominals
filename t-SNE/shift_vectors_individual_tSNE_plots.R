load("dfr.rda")
load("ftm.rda")


#ANALYSIS
#looking at cases individually
#
#nominal case
#create dataset including all occurences of nom
#extract pl-sg pairs to obseve

all_nom = dfr[dfr$case=="nom",]
#head(all_nom)
table(dfr$gender)
tab = table(all_nom$lemma)
table(tab)
tab[tab==3]

all_nom2 = all_nom[all_nom$lemma %in% names(tab[tab==2]),]
#head(all_nom2)

table(table(all_nom2$lemma))
table(all_nom2$number)

tab = table(all_nom2$lemma, all_nom2$number)
#head(tab)
which(tab[,"sg"]==2)
which(tab[,"pl"]==2)
all_nom3 = all_nom2[!is.element(all_nom2$lemma, names(which(tab[,"sg"]==2))),]
table(all_nom3$lemma, all_nom3$number)

all_nom3 = all_nom3[order(all_nom3$lemma,all_nom3$number),]
#head(all_nom2)
all_nom3_sg = all_nom3[all_nom3$number== "sg",]
all_nom3_pl = all_nom3[all_nom3$number=="pl",]

all_nom3_sg_vec = ftm[all_nom3_sg$word,]
all_nom3_pl_vec = ftm[all_nom3_pl$word,]


nom_shift_vec = all_nom3_pl_vec-all_nom3_sg_vec




#tsne plotting of the nominal case
library(Rtsne)
library(plotly)
x = Rtsne(nom_shift_vec)
X= data.frame(x$Y)
X = cbind(all_nom3_sg, X)
head(X)

fig_nom = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~broad_category,
              text = ~paste("case: ", case,
                            "\nnumber: ", number,
                            "\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                    yaxis = list(title = list(text ='t-SNE 2')))
fig_nom


fig_nom2 = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~gender,
               text = ~paste("case: ", case,
                             "\nnumber: ", number,
                             "\nword: ", lemma,
                             "\nbroad category: ", broad_category),
               sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
              yaxis = list(title = list(text ='t-SNE 2')))
fig_nom2



#
#
#genitive
#
all_gen = dfr[dfr$case=="gen",]
#head(all_gen)
#dim(all_gen)
tab = table(all_gen$lemma)
table(tab)
tab[tab==3]
#names(tab[tab>2])
all_gen[all_gen$lemma %in% names(tab[tab==3]),]


#
all_gen2 = all_gen[!(all_gen$word %in% c("dziawuv", "posta", "robota", "rzędu")) & all_gen$lemma %in% names(tab[tab==2]),]
#head(all_gen2)
#dim(all_gen2)

table(table(all_gen2$lemma))
table(all_gen2$number)

#head(all_gen2)
tab = table(all_gen2$lemma, all_gen2$number)
#head(tab)
which(tab[,"sg"]==2)
which(tab[, "pl"]==2)
all_gen3 = all_gen2[!is.element(all_gen2$lemma, names(which(tab[,"sg"]==2))),]
table(all_gen3$lemma, all_gen3$number)

all_gen3 = all_gen3[order(all_gen3$lemma,all_gen3$number),]
#head(all_gen3)
all_gen3_sg = all_gen3[all_gen3$number== "sg",]
all_gen3_pl = all_gen3[all_gen3$number=="pl",]

all_gen3_sg_vec = ftm[all_gen3_sg$word,]
all_gen3_pl_vec = ftm[all_gen3_pl$word,]


gen_shift_vec = all_gen3_pl_vec-all_gen3_sg_vec



#plotting shift vectors for genitive case
x = Rtsne(gen_shift_vec)
X= data.frame(x$Y)
X = cbind(all_gen3_sg, X)
head(X)
fig_gen = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~broad_category,
              text = ~paste("case: ", case,
                            "\nnumber: ", number,
                            "\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                     yaxis = list(title = list(text ='t-SNE 2')))
fig_gen

fig_gen2 = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~gender,
               text = ~paste("case: ", case,
                             "\nnumber: ", number,
                             "\nword: ", lemma,
                             "\nbroad category: ", broad_category),
               sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                      yaxis = list(title = list(text ='t-SNE 2')))
fig_gen2




#
#
#accusative
all_acc = dfr[dfr$case=="acc",]
#head(all_acc)
#nrow(all_acc)
tab = table(all_acc$lemma)
table(tab)
tab[tab==3]
all_acc[all_acc$lemma %in% names(tab[tab==3]),]



#
all_acc2 = all_acc[all_acc$word != "strone"  &all_acc$lemma %in% names(tab[tab==2]),]
#head(all_acc2)
#dim(all_acc2)

table(table(all_acc2$lemma))
table(all_acc2$number)

tab = table(all_acc2$lemma, all_acc2$number)
#head(tab)
#tab[,"sg"]
which(tab[,"sg"]==2)
which(tab[, "pl"]==2)
all_acc3 = all_acc2[!is.element(all_acc2$lemma, names(which(tab[,"sg"]==2))),]
table(all_acc3$lemma, all_acc3$number)

all_acc3 = all_acc3[order(all_acc3$lemma,all_acc3$number),]
#head(all_acc3)
all_acc3_sg = all_acc3[all_acc3$number== "sg",]
all_acc3_pl = all_acc3[all_acc3$number=="pl",]

all_acc3_sg_vec = ftm[all_acc3_sg$word,]
all_acc3_pl_vec = ftm[all_acc3_pl$word,]



acc_shift_vec = all_acc3_pl_vec-all_acc3_sg_vec


#plotting shift vectors for accusative case
x = Rtsne(acc_shift_vec)
X= data.frame(x$Y)
X = cbind(all_acc3_sg, X)
head(X)
fig_acc = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~broad_category,
              text = ~paste("case: ", case,
                            "\nnumber: ", number,
                            "\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                     yaxis = list(title = list(text ='t-SNE 2')))
fig_acc

fig_acc2 = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~gender,
               text = ~paste("case: ", case,
                             "\nnumber: ", number,
                             "\nword: ", lemma,
                             "\nbroad category: ", broad_category),
               sizes=2)%>% 
                layout(xaxis = list(title = list(text ='t-SNE 1')),
                       yaxis = list(title = list(text ='t-SNE 2')))
fig_acc2



#
#
#locative
all_loc = dfr[dfr$case=="loc",]
#head(all_loc)
#nrow(all_loc)
table(all_loc$gender)
tab = table(all_loc$lemma)
table(tab)
tab[tab==3]


#
all_loc2 = all_loc[all_loc$lemma %in% names(tab[tab==2]),]
#head(all_loc2)
#dim(all_loc2)

table(table(all_loc2$lemma))
table(all_acc2$number)

tab = table(all_loc2$lemma, all_loc2$number)
#head(tab)
which(tab[,"sg"]==2)
which(tab[, "pl"]==2)
all_loc3 = all_loc2[!is.element(all_loc2$lemma, names(which(tab[,"sg"]==2))) & !is.element(all_loc2$lemma, names(which(tab[,"pl"]==2))),]
#table(all_loc3$lemma, all_loc3$number)

all_loc3 = all_loc3[order(all_loc3$lemma,all_loc3$number),]
#head(all_loc3)
all_loc3_sg = all_loc3[all_loc3$number== "sg",]
all_loc3_pl = all_loc3[all_loc3$number=="pl",]

all_loc3_sg_vec = ftm[all_loc3_sg$word,]
all_loc3_pl_vec = ftm[all_loc3_pl$word,]


loc_shift_vec = all_loc3_pl_vec-all_loc3_sg_vec


#plotting shift vectors for locative case
x = Rtsne(loc_shift_vec)
X= data.frame(x$Y)
X = cbind(all_loc3_sg, X)
#head(X)
fig_loc = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~broad_category,
              text = ~paste("case: ", case,
                            "\nnumber: ", number,
                            "\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                     yaxis = list(title = list(text ='t-SNE 2')))
fig_loc

fig_loc2 = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~gender,
               text = ~paste("case: ", case,
                             "\nnumber: ", number,
                             "\nword: ", lemma,
                             "\nbroad category: ", broad_category),
               sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                     yaxis = list(title = list(text ='t-SNE 2')))
fig_loc2



#
#
#instrumental
all_inst = dfr[dfr$case=="inst",]
#head(all_inst)
#nrow(all_inst)
tab = table(all_inst$lemma)
table(tab)
tab[tab==3]
all_inst[all_inst$lemma %in% names(tab[tab==3]),]

#
all_inst2 = all_inst[!(all_inst$word %in% c("latami","oczyma","słowy")) & all_inst$lemma %in% names(tab[tab==2]),]
#head(all_inst2)
#dim(all_inst2)

table(table(all_inst2$lemma))
table(all_inst2$number)

tab = table(all_inst2$lemma, all_inst2$number)
head(tab)
which(tab[,"sg"]==2)
which(tab[, "pl"]==2)

all_inst2 = all_inst2[order(all_inst2$lemma,all_inst2$number),]
#head(all_inst2)
all_inst2_sg = all_inst2[all_inst2$number== "sg",]
all_inst2_pl = all_inst2[all_inst2$number=="pl",]

all_inst2_sg_vec = ftm[all_inst2_sg$word,]
all_inst2_pl_vec = ftm[all_inst2_pl$word,]


inst_shift_vec = all_inst2_pl_vec-all_inst2_sg_vec


#plotting shift vectors for instrumental case
x = Rtsne(inst_shift_vec)
X= data.frame(x$Y)
X = cbind(all_inst2_sg, X)
head(X)
fig_inst = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~broad_category,
              text = ~paste("case: ", case,
                            "\nnumber: ", number,
                            "\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)%>% 
            layout(xaxis = list(title = list(text ='t-SNE 1')),
                   yaxis = list(title = list(text ='t-SNE 2')))
fig_inst

fig_inst2 = plot_ly(X,
               x= ~X1,
               y= ~X2,
               color = ~gender,
               text = ~paste("case: ", case,
                             "\nnumber: ", number,
                             "\nword: ", lemma,
                             "\nbroad category: ", broad_category),
               sizes=2)%>% 
              layout(xaxis = list(title = list(text ='t-SNE 1')),
                     yaxis = list(title = list(text ='t-SNE 2')))
fig_inst2





#
#
#dative
#
all_dat = dfr[dfr$case=="dat",]
#head(all_dat)
#dim(all_dat)
table(all_dat$gender)
tab = table(all_dat$lemma)
table(tab)
tab[tab==3]


#
all_dat2 = all_dat[all_dat$lemma %in% names(tab[tab==2]),]
#head(all_dat2)
#dim(all_dat2)

table(table(all_dat2$lemma))
table(all_dat2$number)

head(all_dat2)
tab = table(all_dat2$lemma, all_dat2$number)
#head(tab)
which(tab[,"sg"]==2)
which(tab[, "pl"]==2)
table(all_dat2$lemma, all_dat2$number)

all_dat2 = all_dat2[order(all_dat2$lemma,all_dat2$number),]
#head(all_dat2)
all_dat2_sg = all_dat2[all_dat2$number== "sg",]
all_dat2_pl = all_dat2[all_dat2$number=="pl",]

all_dat2_sg_vec = ftm[all_dat2_sg$word,]
all_dat2_pl_vec = ftm[all_dat2_pl$word,]


dat_shift_vec = all_dat2_pl_vec-all_dat2_sg_vec


#plotting shift vectors for dative case
#x = Rtsne(dat_shift_vec)
#error: perplexity is too large for the number of samples




#
#
#
#vocative
all_voc = dfr[dfr$case=="voc",]
head(all_voc)
tab = table(all_voc$lemma)
table(tab)
head(tab)
#only singular occurences!



#
#
#combine plots of individual cases in one overview graphic
fig_semantics=subplot(fig_nom,fig_gen,fig_acc,fig_inst,fig_loc,nrows = 3)%>% 
              layout(
                annotations=list( 
                list( 
                  x = 0.2,  
                  y = 1.0,  
                  text = "nominative",  
                  xref = "paper",  
                  yref = "paper",  
                  xanchor = "center",  
                  yanchor = "bottom",  
                  showarrow = FALSE 
                ),  
                list( 
                  x = 0.8,  
                  y = 1,  
                  text = "genitive",  
                  xref = "paper",  
                  yref = "paper",  
                  xanchor = "center",  
                  yanchor = "bottom",  
                  showarrow = FALSE 
                ),  
                list( 
                  x = 0.2,  
                  y = 0.635,  
                  text = "accusative",  
                  xref = "paper",  
                  yref = "paper",  
                  xanchor = "center",  
                  yanchor = "bottom",  
                  showarrow = FALSE 
                ),
                list( 
                  x = 0.8,  
                  y = 0.635,  
                  text = "instrumental",  
                  xref = "paper",  
                  yref = "paper",  
                  xanchor = "center",  
                  yanchor = "bottom",  
                  showarrow = FALSE 
                ),
                list( 
                  x = 0.2,  
                  y = 0.3,  
                  text = "locative",  
                  xref = "paper",  
                  yref = "paper",  
                  xanchor = "center",  
                  yanchor = "bottom",  
                  showarrow = FALSE 
                )))

fig_semantics
#saved as 'overview_shift_case_semantics_individual.html'

fig_gender=subplot(fig_nom2,fig_gen2,fig_acc2,fig_inst2,fig_loc2,nrows = 3)%>% 
  layout(
    annotations=list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "nominative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "genitive",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE
      ),  
      list( 
        x = 0.2,  
        y = 0.635,  
        text = "accusative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.8,  
        y = 0.635,  
        text = "instrumental",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.2,  
        y = 0.3,  
        text = "locative",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )))
fig_gender
#saved as 'overview_shift_case_gender_individual.html'
#
#
#
#
#CALCULATE GENERAL SHIFT VECTORS
#in reference to case
#
#
general_shift_vec=rbind(nom_shift_vec, gen_shift_vec, acc_shift_vec,
                        dat_shift_vec, inst_shift_vec, loc_shift_vec)
w = c(rownames(nom_shift_vec),rownames(gen_shift_vec),rownames(acc_shift_vec),
      rownames(dat_shift_vec),rownames(inst_shift_vec),rownames(loc_shift_vec))
dfr2=dfr[as.character(dfr$word) %in% w,]
#dim(dfr2)
#dim(general_shift_vec)
rownames(dfr2)=dfr2$word
dfr2=dfr2[w,]
#general_shift_vec[1:5,1:5]
labels=c(rep("nom",nrow(nom_shift_vec)),
         rep("gen",nrow(gen_shift_vec)),
         rep("acc",nrow(acc_shift_vec)),
         rep("dat",nrow(dat_shift_vec)),
         rep("inst",nrow(inst_shift_vec)),
         rep("loc",nrow(loc_shift_vec)))



x = Rtsne(general_shift_vec)
X= data.frame(x$Y)
X$case = labels
X$word = w
X$gender = dfr2$gender
head(X)
fig = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~case,
              text = ~paste("case: ", case,
                            "\nword: ", word,
                            "\ngender: ", gender),
              
              symbol = ~gender,
              sizes=2)
fig
#saved as 'general_shift_vectors.html'


