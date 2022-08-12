library(RTextTools)
library(tm)
library(log)

#1)	(25	pts)	Given	the	following	documents,	create	a	doc-term	matrix	using	the	following	
#definition	for	term	frequency:

#The	column	of	the	doc-term	matrix	should	be	in	the following	order:
#technology class	 computer server web site

documents<-c("technology class computer. server technology","web technology server. web server","web site server class")
corpus<-create_matrix(documents,removeStopwords = FALSE,stemWords= FALSE,language = "english",weighting = weightTf)
corpus<-as.matrix(corpus)
print(corpus)

#2)	(25	pts)	Compute	the	idf values	for	each	of	the	columns	above (use	log	base-10):
corpus2<-create_matrix(documents,removeStopwords = FALSE,stemWords= FALSE,language = "english",weighting=weightTf)
corpus2<-as.matrix(corpus2)
idf<-round(log10(nrow(corpus2)/colSums(corpus2)),3)
print(data.frame(idf))

#3)	(10 pts)	Compute	the	tf-idf	matrix	using	the	information	computed	in	questions	1	and	2.
idf<-log(nrow(corpus2)/colSums(corpus2))
tfidf<-round(corpus2*idf, 3)
print(tfidf)

#4)	(40	pts)	Compute	the	cosine	similarity	between	each	document	in	the matrix	computed in	
#question	3	(tf-idf)	and	the	query	“web	site” q=(0,0,0,0,1,1).

q1="web site"

cosinesim<-function(query,document){
  #Your code here
  corpus<-create_matrix(document, minWordLength = 1, removeStopwords = FALSE, language = "english")
  corpus<- as.matrix(corpus)
  
  q1matrix<-create_matrix(query, originalMatrix=corpus, removeStopwords = FALSE)
  q1matrix<-as.matrix(q1matrix)
  
  print(data.frame(q1matrix))
}

cosinesim(q1,tfidf)