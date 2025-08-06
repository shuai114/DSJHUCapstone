#setwd("C:/Users/Wei/Documents/DSJHU/en_US")
setwd("C:/shuai personal stuff/My documents/en_US")
con<-file("sample.blogs.txt","r")
con<-file("sample.news.txt","r")
con<-file("sample.twits.txt","r")

someblogs<-readLines(con,encoding="Latin1")
close(con)
library(tm)
someblogs<-VCorpus(VectorSource(someblogs))
someblogs

#preprocessing functions
preprocess<-function(line){
line<-gsub(']','}',line) #change [] to {} to facilitate strange character removing
line<-gsub('\\[','{',line)
line<-gsub('[^-A-Za-z0-9 ,.!?%&()+=_:;\'\"~`@#$^*<>\\/|{}]',' ',line) #remove strange characters (cannot specify [] in this expression)
line<-tolower(line)
line<-gsub('[^ ]*<u\\+([0-9a-f]){4,4}>[^ ]*','',line) #remove unrecognized UTF-8 codes
line<-gsub('-+ | -+',' ',line) #remove heading or tailing -
#expand the contracted forms (choose is between is/has, and would between had/would)
line<-gsub("`","'",line)
line<-gsub("i'm","i am",line)
line<-gsub("i'd","i would",line) #had/would
line<-gsub("he's","he is",line) #is/has
line<-gsub("he'd","he would",line) #had/would
line<-gsub("she's","she is",line) #is/has
line<-gsub("she'd","she would",line) #had/would
line<-gsub("it's","it is",line) #is/has
line<-gsub("it'd","it would",line) #had/would
line<-gsub("you're","you are",line)
line<-gsub("you'd","you would",line) #had/would
line<-gsub("we're","we are",line)
line<-gsub("we'd","we would",line) #had/would
line<-gsub("they're","they are",line)
line<-gsub("they'd","they would",line) #had/would
line<-gsub("'ve"," have",line)
line<-gsub("'ll"," will",line)
line<-gsub("can't|cannot","can not",line)
line<-gsub("shan't","shall not",line)
line<-gsub("won't","will not",line)
line<-gsub("n't"," not",line)
line<-gsub("let's","let us",line)
line<-gsub('#',' number ',line) #so '#1' can be recoded as 'number 1'
#unifying money, time, and number
line<-gsub('\\$[0-9]+((,[0-9]+)*)(.[0-9]+)?','$$',line)
line<-gsub('([0-9]){1,2}(:([0-9]){1,2})+','#time#',line)
line<-gsub('[0-9]+((,[0-9]+)*)(.[0-9]+)?','##',line)
#separate the punctuations
line<-gsub('[,]',' , ',line)
line<-gsub('[.]',' . ',line)
line<-gsub('[!]',' ! ',line)
line<-gsub('[?]',' ? ',line)
line<-gsub('[(]',' ( ',line)
line<-gsub('[)]',' ) ',line)
line<-gsub('[:]',' : ',line)
line<-gsub('[;]',' ; ',line)
line<-gsub('[\"]',' \" ',line)
line<-gsub('[~]',' ~ ',line)
line<-gsub('[+]',' + ',line)
line<-gsub('[=]',' = ',line)
line<-gsub('[*]',' * ',line)
line<-gsub('[\\]',' / ',line) #cannot output '\', so use '/'
line<-gsub('[/]',' / ',line)
line<-gsub('[|]',' | ',line)
line<-gsub('[{]',' { ',line)
line<-gsub('[}]',' } ',line)
#profanity words removal
line<-gsub('ass|asshole|damn|goddamn|damnit|damned|fuck|fucked|fucking',' ',line)
}

#for adjacent word investigation
someblogs<-tm_map(someblogs,content_transformer(preprocess))
someblogs<-tm_map(someblogs,stripWhitespace)
#for whole document investigation
someblogs<-tm_map(someblogs,removeWords,stopwords("english"))
someblogs<-tm_map(someblogs,removePunctuation)
someblogs<-tm_map(someblogs,stripWhitespace)
#someblogs<-tm_map(someblogs,stemDocument)

#document-term-matrix
dtm<-DocumentTermMatrix(someblogs)
freqterm<-findFreqTerms(dtm,1000)
length(freqterm)
freqterm
bacon<-dtm[,c("mean")]
bacon<-as.matrix(bacon)
freqbacon<-sum(bacon[,1]!=0)
freqbacon
bacon<-dtm[bacon[,1]!=0,]

#associated words with their frequencies
library(slam)
termfreq<-rollup(bacon,1,FUN=sum) #change the considered document-term-matrix
termfreq<-as.matrix(termfreq)
termname<-dimnames(termfreq)$Terms
termfreq<-as.vector(termfreq)
names(termfreq)<-termname
termfreq<-sort(termfreq,decreasing=T)
head(termfreq,100)
termfreq<-termfreq[termfreq!=0]
termfreq<-termfreq[termfreq<=20]

#histogram of the term frequencies
hist(termfreq,xlim=c(0,20),ylim=c(0,1000),col="green",xlab="Term Frequency",main="Histogram of Term Frequency")

#correlation between the terms
assocs<-unlist(findAssocs(dtm,"mean",0.1)) #romant sunshin
length(assocs)
assocs

#number of times pairs of terms both apear
baconBBQ<-dtm[,c("mean","does","not","world")]
baconBBQ<-as.matrix(baconBBQ)
bothappear<-NULL
#number of times the considered word appears
bothappear[1]<-sum(baconBBQ[,1]!=0)
#number of times the considered pair appears
bothappear[2]<-sum(baconBBQ[,1]!=0 & baconBBQ[,2]!=0)
bothappear[3]<-sum(baconBBQ[,1]!=0 & baconBBQ[,3]!=0)
bothappear[4]<-sum(baconBBQ[,1]!=0 & baconBBQ[,4]!=0)
names(bothappear)<-c("mean","mean&does","mean&not","mean&world")
bothappear
indices<-which(baconBBQ[,1]!=0 & baconBBQ[,4]!=0)
baconBBQ[c(1,indices),]

#take a look at the sample data
relatedDocs<-someblogs[indices]
lapply(relatedDocs,as.character)
as.character(someblogs[[56659]])

#less Sparse matrix
lessSparse<-removeSparseTerms(dtm,0.8)
dim(lessSparse)
dimnames(lessSparse)$Terms
termfreq<-rollup(lessSparse,1,FUN=sum) #column sum of lessSparse
inspect(termfreq)
inspect(dtm[40000:40010,50000:50010])
