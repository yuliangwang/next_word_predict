##############Randomly sample 50% of data##############
system("perl -ne 'print if (rand() < .50)' en_US.blogs.txt > en_US.blogs.subset.txt") 
system("perl -ne 'print if (rand() < .50)' en_US.news.txt > en_US.news.subset.txt")
system("perl -ne 'print if (rand() < .50)' en_US.twitter.txt > en_US.twitter.subset.txt")  

##############Use the quanteda package to calculate 1-,2-,3- and 4-grams####
require(quanteda)
txt<-textfile("*subset.txt")
myCorpus<-corpus(txt)
dfm_uni<-dfm(myCorpus,stem=F,removeTwitter = F,ngrams=1)
uni_freq<-colSums(dfm_uni)
uni_freq <- sort(uni_freq, decreasing=TRUE)


dfm_bi<-dfm(myCorpus,stem=F,removeTwitter = F,ngrams=2)
bi_freq<-colSums(dfm_bi)
bi_freq <- sort(bi_freq, decreasing=TRUE)
#thresh<-cumsum(bi_freq)/sum(bi_freq)>0.95
#bi_freq<-bi_freq[!thresh]

dfm_tri<-dfm(myCorpus,stem=F,removeTwitter = F,ngrams=3)
tri_freq<-colSums(dfm_tri)
tri_freq <- sort(tri_freq, decreasing=TRUE)

dfm_quad<-dfm(myCorpus,stem=F,removeTwitter = F,ngrams=4)
quad_freq<-colSums(dfm_quad)
quad_freq <- sort(quad_freq, decreasing=TRUE)

##########Pre-compute all modified interpolated Kneser Ney method probabilities for all 2-,3- and 4-grams######
##########This saves response time of the app on shinyapps.io #######
#Use data.table for efficient calculations
library(data.table)
dt2<-data.table(bigram=names(bi_freq),freq=bi_freq,key="bigram")
D2<-sum(bi_freq==1)/(sum(bi_freq==1)+2*sum(bi_freq==2)) #Discount value
dt2<-dt2[freq>1] #ignore singletons
dt2<-dt2[,unigram1:=sapply(strsplit(bigram,"_"),function(x) x[1])]
dt2<-dt2[,unigram2:=sapply(strsplit(bigram,"_"),function(x) x[2])]
#unigram frequency of the w_(i-1)
ind<-match(dt2$unigram1,names(uni_freq))
dt2$unigram1_freq<-uni_freq[ind]
dt2<-na.omit(dt2)
#discounted bigram probability
dt2<-dt2[,bi_prob:=(freq-D2)/unigram1_freq]
#unigram continuation probability
uniq_unigram2<-table(dt2$unigram2)/length(bi_freq)
ind<-match(dt2$unigram2,names(uniq_unigram2))
dt2$unigram2_continue<-uniq_unigram2[ind]
#calculate unique number of bigrams that start with the unigram
uniq_unigram1<-table(dt2$unigram1)
ind<-match(dt2$unigram1,names(uniq_unigram1))
dt2$unigram1_follow<-uniq_unigram1[ind]
#finally, get modified Kneser Ney probabilities
dt2<-dt2[,pkn:=bi_prob+D2*unigram1_follow*unigram2_continue/unigram1_freq]

######3- and 4-grams are similarly calculated, recursively using 2-gram and 3-gram Kneser Ney probabilities, respectively
#3-gram
dt3<-data.table(trigram=names(tri_freq),freq=tri_freq,key="trigram")
D3<-sum(tri_freq==1)/(sum(tri_freq==1)+2*sum(tri_freq==2))
dt3<-dt3[freq>1]
dt3<-dt3[,bigram1:=sapply(strsplit(trigram,"_"), function(x) paste(x[1],x[2],sep="_"))]
dt3<-dt3[,bigram2:=sapply(strsplit(trigram,"_"), function(x) paste(x[2],x[3],sep="_"))]
ind<-match(dt3$bigram1,dt2$bigram)
dt3$bigram1_freq<-dt2$freq[ind]
dt3<-dt3[,tri_prob:=(freq-D3)/bigram1_freq]
ind<-match(dt3$bigram2,dt2$bigram)
dt3$bigram_pkn<-dt2$pkn[ind]
uniq_bigram1<-table(dt3$bigram1)
ind<-match(dt3$bigram1,names(uniq_bigram1))
dt3$bigram1_follow<-uniq_bigram1[ind]
dt3<-dt3[,pkn:=tri_prob+D3*bigram1_follow*bigram_pkn/bigram1_freq]

#4-gram
dt4<-data.table(quadgram=names(quad_freq),freq=quad_freq,key="quadgram")
D4<-sum(quad_freq==1)/(sum(quad_freq==1)+2*sum(quad_freq==2))
dt4<-dt4[freq>1]
dt4<-dt4[,trigram1:=sapply(strsplit(quadgram,"_"), function(x) paste(x[1],x[2],x[3],sep="_"))]
dt4<-dt4[,trigram2:=sapply(strsplit(quadgram,"_"), function(x) paste(x[2],x[3],x[4],sep="_"))]
ind<-match(dt4$trigram1,dt3$trigram)
dt4$trigram1_freq<-dt3$freq[ind]
dt4<-dt4[,quad_prob:=(freq-D4)/trigram1_freq]
ind<-match(dt4$trigram2,dt3$trigram)
dt4$trigram_pkn<-dt3$pkn[ind]
uniq_trigram1<-table(dt4$trigram1)
ind<-match(dt4$trigram1,names(uniq_trigram1))
dt4$trigram1_follow<-uniq_trigram1[ind]
dt4<-dt4[,pkn:=quad_prob+D4*trigram1_follow*trigram_pkn/trigram1_freq]


#Select revelant columns for further calculation
dt4<-dt4[,.(quadgram,trigram1,freq,pkn)]
dt3<-dt3[,.(trigram,bigram1,freq,pkn)]
dt2<-dt2[,.(bigram,unigram1,freq,pkn)]

#Order from high to low frequency, select 2-, 3- and 4-grams that cover 90% of all counts. 
#This is necessary to speed up response time on shiny server.
dt4<-dt4[order(-freq)]
dt3<-dt3[order(-freq)]
dt2<-dt2[order(-freq)]
dt2<-dt2[,thresh:=cumsum(freq)/sum(freq)]
dt2<-dt2[thresh<=0.9,]
dt3<-dt3[,thresh:=cumsum(freq)/sum(freq)]
dt3<-dt3[thresh<=0.9,]
dt4<-dt4[,thresh:=cumsum(freq)/sum(freq)]
dt4<-dt4[thresh<=0.9,]

##the "end" column stores the "next word", i.e., last word in a n-gram.  
dt4<-dt4[,end:=sapply(strsplit(quadgram,"_"), function(x) x[4])]
dt3<-dt3[,end:=sapply(strsplit(trigram,"_"), function(x) x[3])]
dt2<-dt2[,end:=sapply(strsplit(bigram,"_"), function(x) x[2])]

#Order by modified Kneser Ney probabilities
dt4<-dt4[order(-pkn)]
dt3<-dt3[order(-pkn)]
dt2<-dt2[order(-pkn)]

#Since everything is now sorted, only the (n-1)-gram and the end word is necessary for lookup on shiny server.
dt4<-dt4[,.(trigram1,end)]
dt3<-dt3[,.(bigram1,end)]
dt2<-dt2[,.(unigram1,end)]
save(uni_freq,dt2,dt3,dt4,file="KN_prob.RData")
