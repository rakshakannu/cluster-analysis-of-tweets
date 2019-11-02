#read text file
library(tm)
setwd("~/Desktop")
tweets<-readLines("Tweets.txt")
tweets

#build corpus
corpus<- Corpus(VectorSource(tweets))

#create term document matrix
tdm<-TermDocumentMatrix(corpus, control = list(minWordLength=c(1,Inf)))
tdm

#reduce the sparsity
t <- removeSparseTerms(tdm, sparse=0.98)
t

#convert into matrix to do further analysis
m<- as.matrix(t)
m

#plot frequent terms
freq<- rowSums(m)
freq <- subset(freq, freq>=50)
barplot(freq, las=2, col= rainbow(25))

#hierarchial word/tweet clustering using dendrogram
distance <- dist(scale(m))
print(distance, digits=2)
hc <- hclust(distance, method = "ward.D")
plot(hc, hang=-1)
rect.hclust(hc, k=12)

#nonhierarchical k-means clustering of words/tweets
m1<-t(m) #transpose
set.seed(222)
k<-3
kc<-kmeans(m1, k)

