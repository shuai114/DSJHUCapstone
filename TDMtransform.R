#start TermDocumentMatrix transform and generate frequency table for n-grams
con<-file("twit0.txt","r")

begintime<-Sys.time()
someblogs<-readLines(con,encoding="Latin1")
close(con)
someblogs<-VCorpus(VectorSource(someblogs))

someblogs<-tm_map(someblogs,content_transformer(preprocess))
someblogs<-tm_map(someblogs,removePunctuation)
someblogs<-tm_map(someblogs,removeNumbers)
someblogs<-tm_map(someblogs,stripWhitespace)

#n-gram tokenization and Term Document Matrix
tdm<-TermDocumentMatrix(someblogs,control=list(tokenize=myTokenizer)) #tolower by default
termfreq<-rollup(tdm,2,FUN=sum)
termname<-dimnames(termfreq)$Terms
twit0<-data.frame(term=termname,freq.twit0=as.vector(termfreq))
save(twit0,file="twit0.RData")
rm(someblogs,tdm,termfreq,termname,twit0)
#gc()
Sys.time()-begintime
