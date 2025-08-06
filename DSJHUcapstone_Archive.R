##Data management:
#"en_US.news_alt.txt" is got by replacing  with # in "en_US.news.txt", so we can read the whole file

##Useful commands:
#sampcon<-file("sample.blogs.txt","a")
#writeLines(line,sampcon)
#write.table(someblogs,"sample.blogs.csv",sep=",",qmethod="double",row.names=F,col.names=F)
#scan(con,'character',1,quote="",na.strings="",allowEscapes=T) #nlines,flush
#use nchar() to calculate the string length
#iconv(line, "UTF-8", "latin1")
#Encoding(line)
#Encoding(line) <- "latin1"
#line<-strsplit(line,"[ ,.!?\"]")

##Program starts:
setwd("C:/Users/Wei/Documents/DSJHU/en_US")
setwd("C:/Users/shuai/Downloads/DSJHU")

#Get a sample for training (writing to csv file)
con<-file("en_US.blogs.txt","r")
linenum<-0
someblogs<-NULL
set.seed(1234)
line<-readLines(con, 1, encoding="UTF-8")
while(length(line)){
    if(rbinom(1,1,0.01)) {
        linenum<-linenum+1
        someblogs[linenum]<-line
        }
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
write.table(someblogs,"sample.blogs.csv",sep=",",qmethod="double",row.names=F,col.names=F)

#Get a sample for training (writing to txt file)
sampcon<-file("sample.blogs.txt","a")
con<-file("en_US.blogs.txt","r")
set.seed(1234)
line<-readLines(con, 1, encoding="UTF-8")
while(length(line)){
    if(rbinom(1,1,0.01)) writeLines(line,sampcon)
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
close(sampcon)

##Useful functions
#number of lines containing a specific pattern
numlines.for.pattern<-function(pattern,notregexp=F){
con<-file("en_US.blogs.txt","r")
linenum<-0
line<-readLines(con, 1, encoding="UTF-8")
while(length(line)){
    if(grepl(pattern,line,fixed=notregexp)) linenum<-linenum+1
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
linenum
}

#print out the lines containing a specific pattern
lines.for.pattern<-function(pattern,notregexp=F,limit=2360148){ #default limit is the maximum number of lines in the three txt files
con<-file("en_US.blogs.txt","r")
linenum<-0
line<-readLines(con, 1, encoding="UTF-8")
while(length(line) && linenum<limit){
    if(grepl(pattern,line,fixed=notregexp)) {linenum<-linenum+1; print(line)}
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
}

numlines.for.pattern('[a-zA-Z]\\?[a-zA-Z]')
lines.for.pattern('It|it\\?s',limit=10)

con<-file("sample.blogs.txt","r")
line<-readLines(con, 1, encoding="latin1")
line
close(con)

## tm package exploration and its workable functions
library(tm)
load("someblogs.RData")
temp<-data.frame(content=someblogs[1:5,1])
sample<-DataframeSource(temp)
sample<-DirSource(directory="./blogs", encoding="Latin1", mode="text")
str(sample)
blogs<-VCorpus(sample,readerControl=list(reader=readPlain,language="la",load=TRUE))
#new() is a command from methods package, not from tm package
#system.file() is a command from base package to find the full file/dir names of files in packages
blogs[1:3]
blogs[[1]]$content
c(blogs[1:2],blogs[3:4])
length(blogs)
show(blogs) #default when just type blogs directly
summary(blogs)
inspect(blogs)
meta(blogs,type="corpus","foo")<-"bar"
meta(blogs,type="corpus")
meta(blogs,"someTag")<-6:10
meta(blogs)
## another example of VectorSource
con<-file("sample.blogs.txt","r")
someblogs<-readLines(con,encoding="Latin1")
close(con)
someblogs<-VCorpus(VectorSource(someblogs))
#str(someblogs[1:2])
#VCorpus someblogs is a list of documents,
#each document is a list of two elements: content(a string) and meta(a list)
someblogs<-someblogs[1:2]
inspect(someblogs)
writeLines(as.character(someblogs[[1]]))
lapply(someblogs,as.character)
#start transformations
someblogs<-tm_map(someblogs,stripWhitespace)
lapply(temp,as.character)
## another example of DirSource
t.data <- Corpus(DirSource("./training/"), readerControl = list(language="en_US"))
t.data_df <- data.frame(text=unlist(sapply(t.data, '[',"content")),stringsAsFactors=F)

data.twit <- VectorSource(blogs[[2]])
names(data.twit$content)
meta(data.twit$content)
length(data.twit$content$content)
data.twit$content$content[1]
twits<-data.twit$content$content
twits <- strsplit(twits, '\t')
twits <- gsub(pattern="[^a-zA-Z ,.!?\'\"]", replacement="", twits)

twits.vcorp <- VectorSource(twits)
twits.vcorp <- VCorpus(twits.vcorp)
twits.vcorp <- tm_map(twits.vcorp, removePunctuation)
twits.vcorp <- tm_map(twits.vcorp, content_transformer(tolower))
twits.vcorp <- tm_map(twits.vcorp, removeWords, stopwords("english"))
twits.vcorp <- tm_map(twits.vcorp, removeWords, c("also", "but", "just","however", "just"))
twits.vcorp <- tm_map(twits.vcorp, stripWhitespace)
library(SnowballC)
twits.vcorp <- tm_map(twits.vcorp, stemDocument)
twits.vcorp <- tm_map(twits.vcorp, removeNumbers)
library(lava)
twits.vcorp <- tm_map(twits.vcorp, content_transformer(trim))
#other examples for preprocessing
corpus <- tm_map(corpus, PlainTextDocument)
profanitylist <- readLines("./en_US/profanity.txt") #removing profanity words
corpus<- tm_map(corpus,removeWords,profanitylist)
corpus <- tm_map(corpus, stemDocument, language = "english")

twits.tdm <- TermDocumentMatrix(twits.vcorp)
# findFreqTerms(x=twits.tdm, lowfreq=100)
twits.dtm <- DocumentTermMatrix(twits.vcorp)
# findFreqTerms(x=twits.dtm, lowfreq=100)
# findAssocs(twits.dtm, "back", corlimit=0.1)

#data visualization
library(Rgraphviz)
plot(twits.dtm, terms=findFreqTerms(twits.dtm, lowfreq=100)[1:20], corThreshold=0.1)
freq <- sort(colSums(as.matrix(twits.dtm)), decreasing=TRUE)

library(wordcloud)
twits.m <- as.matrix(twits.tdm)
twits.v <- sort(rowSums(twits.m),decreasing=TRUE)
twits.d <- data.frame(word = names(twits.v),freq=twits.v)
# table(twits.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("./Examples_files/figure-html/word cloud-1.png", width=1280,height=800)
wordcloud(twits.d$word, twits.d$freq, scale=c(8,.2),
          min.freq=1, max.words=Inf, random.order=FALSE, 
          rot.per=.15, colors=pal2)
dev.off()
#other wordcloud examples
wordcloud(words1, freq1, max.words = 50, random.order = F, scale = c(3,1),
	rot.per = 0.35, use.r.layout = F, colors=brewer.pal(8, "Dark2"), main="1 gram")
wordcld <- wordcloud(corpus, scale=c(4,0.5), max.words=200, random.order=FALSE,
	rot.per=0.4, use.r.layout=FALSE, colors=brewer.pal(8,'Dark2'))

library(ggvis)
words <- twits.dtm %>%
    as.matrix %>%
    colnames %>%
    (function(x) x[nchar(x) < 10])
length(words)

library(ggplot2)
data.frame(nletters=nchar(words)) %>%
 ggplot(aes(x=nletters)) +
 geom_histogram(binwidth=1) +
 geom_vline(xintercept=mean(nchar(words)),
 colour="green", size=1, alpha=.5) +
 labs(x="Number of Letters", y="Number of Words")

#sent_detect() can be used to detect sentence (from package qdap)
#countLines() can be used to get line number of a file
#str_count() can be used to count the number of string patterns (you can use regular expression)
word.list = strsplit(text.data, "\\W+", perl=TRUE)
#try density plot instead of histogram
m <- ggplot(hi.df, aes(x = nchar))+ggtitle(paste("density plot number of characters per line in",filename))
m <- m + geom_density()

#Package RWeka, tm, slam are used to do 1-gram and 2-gram tokenization
#1-gram tokenization and Term Document Matrix
onegramTokenizer <- function (x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
onegram_tdm <- TermDocumentMatrix(corpus, control = list(tokenize = onegramTokenizer))
tdm1e <- rollup(onegram_tdm, 2, na.rm = TRUE, FUN = sum)
tdm1 <- as.matrix(tdm1e)
#2-gram tokenization and Term Document Matrix
twogramTokenizer <- function (x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
twogram_tdm <- TermDocumentMatrix(corpus, control = list(tokenize = twogramTokenizer))
tdm2e <- rollup(twogram_tdm, 2, na.rm = TRUE, FUN = sum)
tdm2 <- as.matrix(tdm2e)
#another way to do 2-gram tokenization
bigram <- NGramTokenizer(corpus, control=Weka_control(min = 2, max = 2))
bigram <- data.frame(table(bigram))
bigram <-bigram[order(bigram$Freq, decreasing = TRUE),]
colnames(bigram) <- c("Word","Freq")
saveRDS(bigram, file = "./en_US/sample/bigram.Rda")

