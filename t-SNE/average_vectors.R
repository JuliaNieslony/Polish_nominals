load("dfr.rda")
load("ftm.rda")

#calculate average lexeme vectors

lexemes = unique(dfr$lemma)
head(lexemes)


mean_mat=matrix(0,nrow=length(lexemes),ncol=ncol(ftm))
row.names(mean_mat)=lexemes
for(i in 1:length(lexemes)){
  lex = lexemes[i]
  pos = which(dfr$lemma==lex)
  if(length(pos)==1){
    mean_vec=ftm[pos,]
  }else{
    m = ftm[pos,]
    mean_vec = apply(m,2,mean)
  }
  mean_mat[i,]=mean_vec
}
mean_mat[1:5,1:5]


dfr_lex = unique(dfr[,c("lemma","broad_category")])



#nrow(dfr_lex)
rownames(dfr_lex)=1:nrow(dfr_lex)
#length(lexemes)
#nrow(mean_mat)
tab=table(dfr_lex$lemma)
tab[tab>1]
dfr_lex[dfr_lex$lemma=="post",]
dfr_lex=dfr_lex[-1297,]
dfr_lex=dfr_lex[order(dfr_lex$lemma),]


x = Rtsne(mean_mat)
X= data.frame(x$Y)
X = cbind(dfr_lex, X)
fig = plot_ly(X,
              x= ~X1,
              y= ~X2,
              color = ~broad_category,
              text = ~paste("\nword: ", lemma,
                            "\nbroad category: ", broad_category),
              sizes=2)
fig
#saved as 'average_vectors_semantics.html'


#
#
#calculate average vectors for case and gender
#
#mean vectors for cases in general
case = unique(dfr$case)
mean_case_mat=matrix(0,nrow=length(case),ncol=ncol(ftm))
row.names(mean_case_mat)=case
for(i in 1:length(case)){
  ca = case[i]
  pos = which(dfr$case==ca)
  m = ftm[pos,]
  mean_case_vec = apply(m,2,mean)
  mean_case_mat[i,]=mean_vec
}
mean_case_mat[1:5,1:5]









#
#gender
#
gender = unique(dfr$gender)
mean_gender_mat=matrix(0,nrow=length(gender),ncol=ncol(ftm))
row.names(mean_gender_mat)=gender
for(i in 1:length(gender)){
  gen = gender[i]
  pos = which(dfr$gender==gen)
  m = ftm[pos,]
  mean_vec = apply(m,2,mean)
  mean_gender_mat[i,]=mean_vec
}
mean_gender_mat[1:2,1:5]

