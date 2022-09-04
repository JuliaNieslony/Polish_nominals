load("dfr.rda")
load("ftm.rda")



#tsne plotting
#library(Rtsne)
#library(plotly)
#x=Rtsne(ftm)
#X=data.frame(x$Y)
#X$case = tolower(dfr$case)
#X$number = tolower(dfr$number)
#X$noun = tolower(dfr$word)
#X$gender = tolower(dfr$gender)
#X$broad_category = tolower(dfr$broad_category)
#X$category = tolower(dfr$category)

#head(X)
#save(X, "words_Eng_noun.classes.rda")
#fig = plot_ly(X,
#              x= ~X1,
#              y= ~X2,
#              color = ~broad_category,
#              text = ~paste("case: ", case,
#                            "\nnumber: ", number,
#                            "\nword: ", noun,
#                            "\nbroad category: ", broad_category),
#              sizes=2,
#              symbol = ~number)
#fig


ftm.pca = prcomp(ftm, center=TRUE, scale = TRUE)
library(MASS)

#general model
dfr.lda=lda(ftm.pca$x[,1:200],dfr$broad_category)
str(dfr.lda)
p = predict(dfr.lda)$class
table(p)
tab = table(p,dfr$broad_category)
diag(tab)
#accuracy:
acc = sum(diag(tab))/sum(tab)



#specific models
##creating individual datasets for different categories
#singular
sg = dfr[which(dfr$number=="sg"),]
head(sg)
#adjust vector set
ftm_sg = ftm[rownames(ftm) %in% sg$word,]

#calculate model for singular
ftm_sg.pca = prcomp(ftm_sg, center=TRUE, scale = TRUE)
library(MASS)
sg.lda=lda(ftm_sg.pca$x[,1:200],sg$broad_category)
str(sg.lda)
p_sg = predict(sg.lda)$class
table(p_sg)
tab_sg = table(p_sg,sg$broad_category)
diag(tab_sg)
#accuracy:
acc_sg = sum(diag(tab_sg))/sum(tab_sg)
acc_sg

sort(table(sg$broad_category))

#plural
pl = dfr[which(dfr$number=="pl"),]
head(pl)
#adjust vector set
ftm_pl = ftm[rownames(ftm) %in% pl$word,]


#calculate model for plural
ftm_pl.pca = prcomp(ftm_pl, center=TRUE, scale = TRUE)
pl.lda=lda(ftm_pl.pca$x[,1:200],pl$broad_category)
str(pl.lda)
p_pl = predict(pl.lda)$class
table(p_pl)
tab_pl = table(p_pl,pl$broad_category)
diag(tab_pl)
#accuracy:
acc_pl = sum(diag(tab_pl))/sum(tab_pl)
acc_pl

#genitive singular
gen_sg =sg[which(sg$case=="gen"),]
head(gen_sg)
#adjust vector set
ftm_gen_sg = ftm[rownames(ftm) %in% gen_sg$word,]
nrow(ftm_gen_sg)
nrow(gen_sg)

#calculate model for genitive singular
ftm_gen_sg.pca = prcomp(ftm_gen_sg, center=TRUE, scale = TRUE)
gen_sg.lda=lda(ftm_gen_sg.pca$x[,1:200],gen_sg$broad_category)
str(gen_sg.lda)
p_gen_sg = predict(gen_sg.lda)$class
table(p_gen_sg)
tab_gen_sg = table(p_gen_sg, gen_sg$broad_category)
diag(tab_gen_sg)
#accuracy:
acc_gen_sg = sum(diag(tab_gen_sg))/sum(tab_gen_sg)
acc_gen_sg

sort(table(gen_sg$broad_category))



#nominative singular
nom_sg =sg[which(sg$case=="nom"),]
head(nom_sg)
#adjust vector set
ftm_nom_sg = ftm[rownames(ftm) %in% nom_sg$word,]
nrow(ftm_nom_sg)
nrow(nom_sg)

#calculate model for nominative singular
ftm_nom_sg.pca = prcomp(ftm_nom_sg, center=TRUE, scale = TRUE)
nom_sg.lda=lda(ftm_nom_sg.pca$x[,1:200],nom_sg$broad_category)
str(nom_sg.lda)
p_nom_sg = predict(nom_sg.lda)$class
table(p_nom_sg)
tab_nom_sg = table(p_nom_sg,nom_sg$broad_category)
diag(tab_nom_sg)
#accuracy:
acc_nom_sg = sum(diag(tab_nom_sg))/sum(tab_nom_sg)
acc_nom_sg

sort(table(nom_sg$broad_category))


#accusative singular
acc_sg =sg[which(sg$case=="acc"),]
head(acc_sg)
#adjust vector set
ftm_acc_sg = ftm[rownames(ftm) %in% acc_sg$word,]
nrow(ftm_acc_sg)
nrow(acc_sg)
#calculate model for accusative singular
ftm_acc_sg.pca = prcomp(ftm_acc_sg, center=TRUE, scale = TRUE)
acc_sg.lda=lda(ftm_acc_sg.pca$x[,1:200],acc_sg$broad_category)
str(acc_sg.lda)
p_acc_sg = predict(acc_sg.lda)$class
table(p_acc_sg)
tab_acc_sg = table(p_acc_sg,acc_sg$broad_category)
diag(tab_acc_sg)
#accuracy:
acc_acc_sg = sum(diag(tab_acc_sg))/sum(tab_acc_sg)
acc_acc_sg

sort(table(acc_sg$broad_category))

#locative singular
loc_sg =sg[which(sg$case=="loc"),]
head(loc_sg)
#adjust vector set
ftm_loc_sg = ftm[rownames(ftm) %in% loc_sg$word,]
nrow(ftm_loc_sg)
nrow(loc_sg)

#calculate model for locative singular
ftm_loc_sg.pca = prcomp(ftm_loc_sg, center=TRUE, scale = TRUE)
loc_sg.lda=lda(ftm_loc_sg.pca$x[,1:200],loc_sg$broad_category)
str(loc_sg.lda)
p_loc_sg = predict(loc_sg.lda)$class
table(p_loc_sg)
tab_loc_sg = table(p_loc_sg,loc_sg$broad_category)
diag(tab_loc_sg)
#accuracy:
acc_loc_sg = sum(diag(tab_loc_sg))/sum(tab_loc_sg)
acc_loc_sg

sort(table(loc_sg$broad_category))


#instrumental singular
inst_sg =sg[which(sg$case=="inst"),]
head(inst_sg)
#adjust vector set
ftm_inst_sg = ftm[rownames(ftm) %in% inst_sg$word,]
nrow(ftm_inst_sg)
nrow(inst_sg)

#calculate model for instrumental singular
ftm_inst_sg.pca = prcomp(ftm_inst_sg, center=TRUE, scale = TRUE)
inst_sg.lda=lda(ftm_inst_sg.pca$x[,1:200],inst_sg$broad_category)
str(inst_sg.lda)
p_inst_sg = predict(inst_sg.lda)$class
table(p_inst_sg)
tab_inst_sg = table(p_inst_sg,inst_sg$broad_category)
diag(tab_inst_sg)
#accuracy:
acc_inst_sg = sum(diag(tab_inst_sg))/sum(tab_inst_sg)
acc_inst_sg

sort(table(inst_sg$broad_category))


#dative singular
dat_sg =sg[which(sg$case=="dat"),]
head(dat_sg)
#adjust vector set
ftm_dat_sg = ftm[rownames(ftm) %in% dat_sg$word,]
nrow(ftm_dat_sg)
ncol(ftm_dat_sg)
nrow(dat_sg)

#calculate model for dative singular
ftm_dat_sg.pca = prcomp(ftm_dat_sg, center=TRUE, scale = TRUE, rank = 35)
dim(ftm_dat_sg.pca$x)
#dat_sg.lda=lda(ftm_dat_sg.pca$x[,1:200],dat_sg$broad_category)
dat_sg.lda=lda(ftm_dat_sg.pca$x[,1:30],dat_sg$broad_category)
qr(ftm_dat_pl.pca$x)$rank
table(dat_sg$broad_category)
dat_sg = droplevels(dat_sg)
#indication exceeds limits
#dat_sg.lda=lda(ftm_dat_sg.pca$x[,1:36],dat_sg$broad_category)
#variables are collinear
#try without principal component analysis
#dat_sg.lda=lda(ftm_dat_sg,dat_sg$broad_category)
#leads to colinearity
#accuracy goes up to 0.7567568
str(dat_sg.lda)
p_dat_sg = predict(dat_sg.lda)$class
table(p_dat_sg)
tab_dat_sg = table(p_dat_sg,dat_sg$broad_category)
diag(tab_dat_sg)
#accuracy:
acc_dat_sg = sum(diag(tab_dat_sg))/sum(tab_dat_sg)
acc_dat_sg

sort(table(dat_sg$broad_category))


#vocative singular
voc_sg =sg[which(sg$case=="voc"),]
head(voc_sg)
#adjust vector set
ftm_voc_sg = ftm[rownames(ftm) %in% voc_sg$word,]
nrow(ftm_voc_sg)
nrow(voc_sg)

#calculate model for vocative singular
ftm_voc_sg.pca = prcomp(ftm_voc_sg, center=TRUE, scale = TRUE)
#voc_sg.lda=lda(ftm_voc_sg.pca$x[,1:200],voc_sg$broad_category)
# error: indication outside limitations
dim(ftm_voc_sg.pca$x)
voc_sg.lda=lda(ftm_voc_sg.pca$x[,1:12],voc_sg$broad_category)
#variables are collinear!
#try without principal component analysis
#voc_sg.lda=lda(ftm_voc_sg,voc_sg$broad_category)
#still collinear
#accuracy does not change: 0.5384615
str(voc_sg.lda)
p_voc_sg = predict(voc_sg.lda)$class
table(p_voc_sg)
tab_voc_sg = table(p_voc_sg,voc_sg$broad_category)
diag(tab_voc_sg)
#accuracy:
acc_voc_sg = sum(diag(tab_voc_sg))/sum(tab_voc_sg)
acc_voc_sg

sort(table(voc_sg$broad_category))




#calculate model for genitive plural
gen_pl =pl[which(pl$case=="gen"),]
head(gen_pl)
#adjust vector set
ftm_gen_pl = ftm[rownames(ftm) %in% gen_pl$word,]
nrow(ftm_gen_pl)
nrow(gen_pl)

ftm_gen_pl.pca = prcomp(ftm_gen_pl, center=TRUE, scale = TRUE)
gen_pl.lda=lda(ftm_gen_pl.pca$x[,1:200],gen_pl$broad_category)
str(gen_pl.lda)
p_gen_pl = predict(gen_pl.lda)$class
table(p_gen_pl)
tab_gen_pl = table(p_gen_pl,gen_pl$broad_category)
diag(tab_gen_pl)
#accuracy:
acc_gen_pl = sum(diag(tab_gen_pl))/sum(tab_gen_pl)
acc_gen_pl

sort(table(gen_pl$broad_category))


#calculate model for nominative plural
nom_pl =pl[which(pl$case=="nom"),]
head(nom_pl)
#adjust vector set
ftm_nom_pl = ftm[rownames(ftm) %in% nom_pl$word,]
nrow(ftm_nom_pl)
nrow(nom_pl)

ftm_nom_pl.pca = prcomp(ftm_nom_pl, center=TRUE, scale = TRUE)
nom_pl.lda=lda(ftm_nom_pl.pca$x[,1:200],nom_pl$broad_category)
str(nom_pl.lda)
p_nom_pl = predict(nom_pl.lda)$class
table(p_nom_pl)
tab_nom_pl = table(p_nom_pl,nom_pl$broad_category)
diag(tab_nom_pl)
#accuracy:
acc_nom_pl = sum(diag(tab_nom_pl))/sum(tab_nom_pl)
acc_nom_pl

sort(table(nom_pl$broad_category))


#calculate model for accusative plural
acc_pl =pl[which(pl$case=="acc"),]
head(acc_pl)
#adjust vector set
ftm_acc_pl = ftm[rownames(ftm) %in% acc_pl$word,]
nrow(ftm_acc_pl)
nrow(acc_pl)

ftm_acc_pl.pca = prcomp(ftm_acc_pl, center=TRUE, scale = TRUE)
acc_pl.lda=lda(ftm_acc_pl.pca$x[,1:200],acc_pl$broad_category)
str(acc_pl.lda)
p_acc_pl = predict(acc_pl.lda)$class
table(p_acc_pl)
tab_acc_pl = table(p_acc_pl,acc_pl$broad_category)
diag(tab_acc_pl)
#accuracy:
acc_acc_pl = sum(diag(tab_acc_pl))/sum(tab_acc_pl)
acc_acc_pl

sort(table(acc_pl$broad_category))

#calculate model for locative plural
loc_pl = pl[which(pl$case=="loc"),]
head(loc_pl)
#adjust vector set
ftm_loc_pl = ftm[rownames(ftm) %in% loc_pl$word,]
nrow(ftm_loc_pl)
nrow(loc_pl)

ftm_loc_pl.pca = prcomp(ftm_loc_pl, center=TRUE, scale = TRUE)
loc_pl.lda=lda(ftm_loc_pl.pca$x[,1:200],loc_pl$broad_category)
str(loc_pl.lda)
p_loc_pl = predict(loc_pl.lda)$class
table(p_loc_pl)
tab_loc_pl = table(p_loc_pl,loc_pl$broad_category)
diag(tab_loc_pl)
#accuracy:
acc_loc_pl = sum(diag(tab_loc_pl))/sum(tab_loc_pl)
acc_loc_pl

sort(table(loc_pl$broad_category))

#calculate model for instrumental plural
inst_pl = pl[which(pl$case=="inst"),]
head(inst_pl)
#adjust vector set
ftm_inst_pl = ftm[rownames(ftm) %in% inst_pl$word,]
nrow(ftm_inst_pl)
ncol(ftm_inst_pl)
nrow(inst_pl)

ftm_inst_pl.pca = prcomp(ftm_inst_pl, center=TRUE, scale = TRUE)
dim(ftm_inst_pl.pca$x)
#inst_pl.lda=lda(ftm_inst_pl.pca$x[, 1:200],inst_pl$broad_category)
#error:  indication exceeds limitations --> since inst_pl.lda only contains 154 rows?
#select all :! 154 seems so be constant across groups
#->  go down to 150
#!!! variables are collinear
inst_pl.lda=lda(ftm_inst_pl.pca$x[,1:150],inst_pl$broad_category)
#!what now??
#try without prcomp
#inst_pl.lda=lda(ftm_inst_pl,inst_pl$broad_category)
#still collinear
#accuracy goes up to 0.9805195
str(inst_pl.lda)
p_inst_pl = predict(inst_pl.lda)$class
table(p_inst_pl)
tab_inst_pl = table(p_inst_pl,inst_pl$broad_category)
diag(tab_inst_pl)
#accuracy:
acc_inst_pl = sum(diag(tab_inst_pl))/sum(tab_inst_pl)
acc_inst_pl

sort(table(inst_pl$broad_category))


#calculate model for dative plural
dat_pl =pl[which(pl$case=="dat"),]
head(dat_pl)
#adjust vector set
ftm_dat_pl = ftm[rownames(ftm) %in% dat_pl$word,]
nrow(ftm_dat_pl)
nrow(dat_pl)

ftm_dat_pl.pca = prcomp(ftm_dat_pl, center=TRUE, scale = TRUE)
#dat_pl.lda=lda(ftm_dat_pl.pca$x[,1:200],dat_pl$broad_category)
#! indication exceeds limitations
dim(ftm_dat_pl.pca$x)
#dat_pl.lda=lda(ftm_dat_pl.pca$x,dat_pl$broad_category)
# error: 38 seems to be constant across groups
dat_pl.lda=lda(ftm_dat_pl.pca$x[,1:35],dat_pl$broad_category)
#variables are collinear
#what now?
#without prcomp
#dat_pl.lda=lda(ftm_dat_pl,dat_pl$broad_category)
#still colinearity
#accuracy goes down to 0.8157895
str(dat_pl.lda)
p_dat_pl = predict(dat_pl.lda)$class
table(p_dat_pl)
tab_dat_pl = table(p_dat_pl,dat_pl$broad_category)
diag(tab_dat_pl)
#accuracy:
acc_dat_pl = sum(diag(tab_dat_pl))/sum(tab_dat_pl)
acc_dat_pl

sort(table(dat_pl$broad_category))


#calculate model for vocative plural
voc_pl =pl[which(pl$case=="voc"),]
head(voc_pl)
#adjust vector set
ftm_voc_pl = ftm[rownames(ftm) %in% voc_pl$word,]
nrow(ftm_voc_pl)
nrow(voc_pl)

ftm_voc_pl.pca = prcomp(ftm_voc_pl, center=TRUE, scale = TRUE)
#voc_pl.lda=lda(ftm_voc_pl.pca$x[,1:200],voc_pl$broad_category)
#indication exceeds limitations
dim(ftm_voc_pl.pca$x)
voc_pl.lda=lda(ftm_voc_pl.pca$x[,1:10],voc_pl$broad_category)
#variables are collinear!
#without prcomp
#voc_pl.lda=lda(ftm_voc_pl,voc_pl$broad_category)
#accuracy goes down to 0.2727273
#still collinear
str(voc_pl.lda)
p_voc_pl = predict(voc_pl.lda)$class
table(p_voc_pl)
tab_voc_pl = table(p_voc_pl, voc_pl$broad_category)
diag(tab_voc_pl)
#accuracy:
acc_voc_pl = sum(diag(tab_voc_pl))/sum(tab_voc_pl)
acc_voc_pl

sort(table(voc_pl$broad_category))

sort(table(sg$case))

accuracies_sg= c(acc_sg, acc_gen_sg, acc_nom_sg, acc_acc_sg, acc_loc_sg, acc_inst_sg, acc_dat_sg, acc_voc_sg)
names_accuracies = c("general", "gen", "nom", "acc", "loc", "inst", "dat", "voc")
accuracies_sg.df = data.frame(names_accuracies, accuracies_sg)
accuracies_sg.df

accuracies_pl = c(acc_pl, acc_gen_pl, acc_nom_pl, acc_acc_pl, acc_loc_pl, acc_inst_pl, acc_dat_pl, acc_voc_pl)
accuracies_pl.df = data.frame(names_accuracies, accuracies_pl)
accuracies_pl.df

plot(accuracies_sg.df$accuracies_sg, accuracies_pl.df$accuracies_pl)
abline(0,1)



#write loop for all cases in sg
#for (i in unique(sg$case)){
#  i_sg =sg[which(sg$case==i),]
#  head(i_sg)
#  ftm_i_sg = ftm[rownames(ftm) %in% i_sg$word,]
#  nrow(ftm_i_sg)
#  nrow(i_sg)
#  ftm_i_sg.pca = prcomp(ftm_i_sg, center=TRUE, scale = TRUE)
#  i_sg.lda=lda(ftm_i_sg.pca$x[,1:200],i_sg$broad_category)
#  str(i_sg.lda)
#  p_i_sg = predict(i_sg.lda)$class
#  print(i, "sg predicted classes:", table(p_i_sg))
#  tab_i_sg = table(p_i_sg,i_sg$broad_category)
 # print(i, "sg diagonal table of predicted classes:", diag(tab_i_sg))
#  print("prediction accuracy for ", i, "sg: ", sum(diag(tab_i_sg))/sum(tab_i_sg))
#  print("distribution of categories wihin ",i , "sg: ",sort(table(i_sg$broad_category)))
#}






#
#
all_nom = dfr[dfr$case=="nom",]
head(all_nom)
table(dfr$gender)
tab = table(all_nom$lemma)
table(tab)
tab[tab==3]
all_nom[all_nom$lemma=="muzyk",]

all_nom2 = all_nom[all_nom$word!="muzyka" & all_nom$lemma %in% names(tab[tab==2]),]
head(all_nom2)

table(table(all_nom2$lemma))
table(all_nom2$number)

head(all_nom2)
tab = table(all_nom2$lemma, all_nom2$number)
head(tab)
tab[,"sg"]
which(tab[,"sg"]==2)
all_nom3 = all_nom2[!is.element(all_nom2$lemma, names(which(tab[,"sg"]==2))),]
table(all_nom3$lemma, all_nom3$number)

all_nom3 = all_nom3[order(all_nom3$lemma,all_nom3$number),]
head(all_nom3)
all_nom3_sg = all_nom3[all_nom3$number== "sg",]
all_nom3_pl = all_nom3[all_nom3$number=="pl",]

all_nom3_sg_vec = ftm[all_nom3_sg$word,]
all_nom3_pl_vec = ftm[all_nom3_pl$word,]
nrow(all_nom3_sg)
nrow(all_nom3_sg_vec)
nrow(all_nom3_pl)
nrow(all_nom3_pl_vec)

nom_shift_vec = all_nom3_pl_vec-all_nom3_sg_vec

library(Rtsne)
library(plotly)
x = Rtsne(nom_shift_vec)
X= data.frame(x$Y)
X = cbind(all_nom3_sg, X)
head(X)
fig = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~broad_category,
              text = ~paste("case: ", case,
                            "\nnumber: ", number,
                            "\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)
fig

fig2 = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~gender,
              text = ~paste("case: ", case,
                            "\nnumber: ", number,
                            "\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)
fig2
