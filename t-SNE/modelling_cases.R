load("dfr.rda")
load("ftm.rda")



ftm.pca = prcomp(ftm, center=TRUE, scale = TRUE)
library(MASS)

#
#general model
dfr.lda=lda(ftm.pca$x[,1:200],dfr$broad_category)
str(dfr.lda)
p = predict(dfr.lda)$class
table(p)
tab = table(p,dfr$broad_category)
diag(tab)
#accuracy:
acc = sum(diag(tab))/sum(tab)
acc


#specific models
#singular
sg = dfr[which(dfr$number=="sg"),]
head(sg)
#adjust vector set
ftm_sg = ftm[rownames(ftm) %in% sg$word,]

#calculate model for singular
ftm_sg.pca = prcomp(ftm_sg, center=TRUE, scale = TRUE)
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


#model individual cases (without specifying number)
sort(table(dfr$case))

#genitive
gen = dfr[which(dfr$case=="gen"),]
head(gen)
ftm_gen = ftm[rownames(ftm) %in% gen$word,]
nrow(ftm_gen)
nrow(gen)
ftm_gen.pca = prcomp(ftm_gen, center=TRUE, scale = TRUE)
gen.lda=lda(ftm_gen.pca$x[,1:200], gen$broad_category)
str(gen.lda)
p_gen = predict(gen.lda)$class
table(p_gen)
tab_gen = table(p_gen, gen$broad_category)
diag(tab_gen)
acc_gen = sum(diag(tab_gen))/sum(tab_gen)
acc_gen
sort(table(gen$broad_category))


#nominative
nom =dfr[which(dfr$case=="nom"),]
head(nom)
#adjust vector set
ftm_nom = ftm[rownames(ftm) %in% nom$word,]
nrow(ftm_nom)
nrow(nom)
#calculate model for nominative
ftm_nom.pca = prcomp(ftm_nom, center=TRUE, scale = TRUE)
nom.lda=lda(ftm_nom.pca$x[,1:200],nom$broad_category)
str(nom.lda)
p_nom = predict(nom.lda)$class
table(p_nom)
tab_nom = table(p_nom,nom$broad_category)
diag(tab_nom)
#accuracy:
acc_nom = sum(diag(tab_nom))/sum(tab_nom)
acc_nom
sort(table(nom$broad_category))



#accusative
acc =dfr[which(dfr$case=="acc"),]
head(acc)
#adjust vector set
ftm_acc = ftm[rownames(ftm) %in% acc$word,]
nrow(ftm_acc)
nrow(acc)
#calculate model for accusative
ftm_acc.pca = prcomp(ftm_acc, center=TRUE, scale = TRUE)
acc.lda=lda(ftm_acc.pca$x[,1:200],acc$broad_category)
str(acc.lda)
p_acc = predict(acc.lda)$class
table(p_acc)
tab_acc = table(p_acc,acc$broad_category)
diag(tab_acc)
#accuracy:
acc_acc = sum(diag(tab_acc))/sum(tab_acc)
acc_acc
sort(table(acc$broad_category))


#locative
loc =dfr[which(dfr$case=="loc"),]
head(loc)
#adjust vector set
ftm_loc = ftm[rownames(ftm) %in% loc$word,]
nrow(ftm_loc)
nrow(loc)
#calculate model for locative
ftm_loc.pca = prcomp(ftm_loc, center=TRUE, scale = TRUE)
loc.lda=lda(ftm_loc.pca$x[,1:200],loc$broad_category)
str(loc.lda)
p_loc = predict(loc.lda)$class
table(p_loc)
tab_loc = table(p_loc,loc$broad_category)
diag(tab_loc)
#accuracy:
acc_loc = sum(diag(tab_loc))/sum(tab_loc)
acc_loc
sort(table(loc$broad_category))



#instrumental
inst =dfr[which(dfr$case=="inst"),]
head(inst)
#adjust vector set
ftm_inst = ftm[rownames(ftm) %in% inst$word,]
nrow(ftm_inst)
nrow(inst)
#calculate model for instrumental singular
ftm_inst.pca = prcomp(ftm_inst, center=TRUE, scale = TRUE)
inst.lda=lda(ftm_inst.pca$x[,1:200],inst$broad_category)
str(inst.lda)
p_inst = predict(inst.lda)$class
table(p_inst)
tab_inst = table(p_inst,inst$broad_category)
diag(tab_inst)
#accuracy:
acc_inst = sum(diag(tab_inst))/sum(tab_inst)
acc_inst
sort(table(inst$broad_category))



#dative
dat =dfr[which(dfr$case=="dat"),]
head(dat)
#adjust vector set
ftm_dat = ftm[rownames(ftm) %in% dat$word,]
nrow(ftm_dat)
nrow(dat)
#calculate model for dative
#adjust pca through rank: dimension
#eve with combined case classes including both sg and pl, the limit of about at least 200 observations is underwent with the dative
ftm_dat.pca = prcomp(ftm_dat, center=TRUE, scale = TRUE, rank = 75)
#dim(ftm_dat.pca$x)
dat.lda=lda(ftm_dat.pca$x,dat$broad_category)
#qr(ftm_dat.pca$x)$rank
str(dat.lda)
p_dat = predict(dat.lda)$class
table(p_dat)
tab_dat = table(p_dat,dat$broad_category)
diag(tab_dat)
#accuracy:
acc_dat = sum(diag(tab_dat))/sum(tab_dat)
acc_dat
sort(table(dat$broad_category))



#vocative
voc =dfr[which(dfr$case=="voc"),]
head(voc)
#adjust vector set
ftm_voc = ftm[rownames(ftm) %in% voc$word,]
nrow(ftm_voc)
nrow(voc)
#calculate model for vocative
#dim too small: adjust with rank
ftm_voc.pca = prcomp(ftm_voc, center=TRUE, scale = TRUE, rank = 21)
#voc_sg.lda=lda(ftm_voc_sg.pca$x[,1:200],voc_sg$broad_category)
# error: indication outside limitations
dim(ftm_voc.pca$x)
voc.lda=lda(ftm_voc.pca$x,voc$broad_category)
#variables are collinear
str(voc.lda)
p_voc = predict(voc.lda)$class
table(p_voc)
tab_voc = table(p_voc,voc$broad_category)
diag(tab_voc)
#accuracy:
acc_voc = sum(diag(tab_voc))/sum(tab_voc)
acc_voc
sort(table(voc$broad_category))


#accuracies:
accuracies = c(acc, acc_sg, acc_pl, acc_gen, acc_nom, acc_acc, acc_loc, acc_inst, acc_dat, acc_voc)
names_accuracies = c("general", "sg", "pl", "gen", "nom", "acc", "loc", "inst", "dat", "voc")
accuracies.df = data.frame(names_accuracies, accuracies)
accuracies.df
write.csv(accuracies.df, "accuracies_cases_lda")
