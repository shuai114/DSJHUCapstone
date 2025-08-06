##Data management:
#"en_US.news_alt.txt" is got by replacing  with # in "en_US.news.txt",
# so we can read the whole file

##Training starts:
#setwd("C:/Users/Wei/Documents/DSJHU/en_US")
#setwd("C:/shuai personal stuff/My documents/jobs/en_US")
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US")
sampcon<-file("sample.blogs.txt","a")
con<-file("en_US.blogs.txt","r")
sampcon<-file("sample.news.txt","a")
con<-file("en_US.news_alt.txt","r")
sampcon<-file("sample.twits.txt","a")
con<-file("en_US.twitter.txt","r")

#Get a sample for training and remove the encoding
set.seed(1234)
line<-readLines(con, 1, encoding="UTF-8")
while(length(line)){
    if(rbinom(1,1,0.1)) writeLines(line,sampcon) #change the probability to get more data
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
close(sampcon)
#Open the document got above in Microsoft WORD using Western European (Windows) encoding, and type in "''"\
#    (use the character saved in "en_US.try.txt" to get left single quotation mark '),
#    (The characters typed in will be converted to Western European (Windows) encoding automatically)
#Copy and paste into Wordpad, and use the characters got in WORD for "''" to replace them by normal characters
#You also need to replace \ by \\, otherwise R cannot read it.
#    (Among all the symbols (~`@#$^*{}[]|\<>/",.!?%&()-+=_:;'") only four("''") will change under Western European (Windows) encoding
#    (Note: there are two types of single/double quotation marks in those symbols)

##Useful functions
#number of lines containing a specific pattern
numlines.for.pattern<-function(txtfile,pattern,notregexp=F){
con<-file(txtfile,"r")
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
lines.for.pattern<-function(pattern,notregexp=F,limit=2360148){
#default limit is the maximum number of lines in the three txt files
linenum<-0
line<-readLines(con, 1, encoding="UTF-8")
while(length(line) && linenum<limit){
    if(grepl(pattern,line,fixed=notregexp)) {linenum<-linenum+1; print(line)}
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
}
lines.for.pattern("to the point",T,limit=10)

#3/43=0.06976744 in sample.blogs, 22/555=0.03963964 in en_US.blogs, 2/393=0.005089059 in en_US.news_alt, 234/730=0.3205479 in en_US.twitter
n<-numlines.for.pattern("en_US.twitter.txt","mean the world")
d<-numlines.for.pattern("en_US.twitter.txt","mean the")
c(n,d,n/d)
#28/31=0.9032258 in sample.blogs, 262/305=0.8590164 in en_US.blogs, 78/79=0.9873418 in en_US.news_alt, 36/48=0.75 in en_US.twitter
n<-numlines.for.pattern("en_US.twitter.txt","quite some time")
d<-numlines.for.pattern("en_US.twitter.txt","quite some")
c(n,d,n/d)
#32/1168=0.02739726 in sample.blogs, 336/11540=0.02911612 in en_US.blogs, 34/1537=0.02212101 in en_US.news_alt, 891/15407=0.05783086 in en_US.twitter
n<-numlines.for.pattern("en_US.twitter.txt","on my way")
d<-numlines.for.pattern("en_US.twitter.txt","on my")
c(n,d,n/d)
#6/7=0.85714286 in en_US.blogs, 1/2=0.50 in en_US.news_alt, 4/4=1.00 in en_US.twitter
numlines.for.pattern("en_US.blogs.txt","bottom to the")
numlines.for.pattern("en_US.blogs.txt","bottom to the top")
#4/51=0.07843137 in en_US.blogs, 1/15=0.06666667 in en_US.news_alt, 1/16=0.0625 in en_US.twitter
numlines.for.pattern("en_US.blogs.txt","bottom to")
numlines.for.pattern("en_US.blogs.txt","bottom to top")
#show the relevant lines
con<-file("en_US.news_alt.txt","r")
lines.for.pattern("bottom to the")

#training
#setwd("C:/shuai personal stuff/My documents/jobs/en_US")
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US")
con<-file("en_US.blogs.txt","r")
con<-file("en_US.news_alt.txt","r")
con<-file("en_US.twitter.txt","r")
con<-file("sample.blogs.txt","r")
con<-file("sample.twits.txt","r")

someblogs<-readLines(con,encoding="Latin1")
close(con)
library(tm)
someblogs<-VCorpus(VectorSource(someblogs))
someblogs
#someblogs<-VCorpus(DirSource())

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
#line<-gsub('\\$[0-9]+((,[0-9]+)*)(.[0-9]+)?','$$',line)
#line<-gsub('([0-9]){1,2}(:([0-9]){1,2})+','#time#',line)
#line<-gsub('[0-9]+((,[0-9]+)*)(.[0-9]+)?','##',line)
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

#split into sentences
#library(qdap)
#con<-file("sample.blogs.txt","r")
#sampcon<-file("sample.temp.txt","a")
#line<-readLines(con, 1, encoding="latin1")
#lines<-sent_detect(line)
#writeLines(lines,sampcon)
#close(con)
#close(sampcon)

someblogs<-tm_map(someblogs,content_transformer(preprocess))
#someblogs<-tm_map(someblogs,removeWords,stopwords("english"))
#someblogs<-tm_map(someblogs,removeWords,c("also", "but", "just","however", "just"))
#profanitylist<-readLines("profanity.txt") #removing profanity words
#someblogs<-tm_map(someblogs,removeWords,profanitylist)
someblogs<-tm_map(someblogs,removePunctuation)
someblogs<-tm_map(someblogs,removeNumbers)
#someblogs<-tm_map(someblogs,stemDocument,language="english")
someblogs<-tm_map(someblogs,stripWhitespace)

#test n-gram
library(RWeka)
library(slam)
begintime<-Sys.time()
#n-gram tokenization and Term Document Matrix
myTokenizer<-function(x) {NGramTokenizer(x,Weka_control(min=6,max=6))} #remove punctuation (' \r\n\t.,;:'"()?!') by default
tdm<-TermDocumentMatrix(someblogs,control=list(tokenize=myTokenizer)) #tolower by default, other option: weighting, bounds(global and local), removePunctuation, removeNumbers, stopwords, stemming, dictionary, wordLengths
termfreq<-rollup(tdm,2,FUN=sum)
termname<-dimnames(termfreq)$Terms
Sys.time()-begintime
save(tdm,termfreq,termname,file="sampletwitter.RData")
load("sampletwitter.RData")

lessname<-findFreqTerms(termfreq,20) #should be less than 200000, better less than 20000
length(lessname)
lessterm<-termfreq[lessname,]

match<-function(string,trmfreq,trmname){
splitted<-unlist(strsplit(string,' '))
strhead<-splitted[1]
strtail<-gsub("^.*? ","",string)
regexpr<-paste0("(^",strhead,"| ",strhead,") ",strtail," [a-z]+$")
matchedindex<-grep(regexpr,trmname)
matchedTF<-trmfreq[matchedindex,]
matchednames<-trmname[matchedindex]
matchedTF<-as.vector(matchedTF)
names(matchedTF)<-matchednames
matchedTF<-sort(matchedTF,decreasing=T)
matchedTF
}

match("on my",lessterm,lessname)
match("mean the",termfreq,termname)
match("quite some",termfreq,termname)
match("to the",lessterm,lessname) #use the maximum occurence term with "bottom"

tdm.ct<-TermDocumentMatrix(someblogs,control=list(weighting=weightBin))
candidates<-c("bottom","number","gym","next","new","world","show","top","best","beach","game","point","end","mall","people","city","public","store","airport","weekend")
tdm.candct<-tdm.ct[candidates,]
assocs<-unlist(findAssocs(tdm.candct,"bottom",0.01))
gsub("^.*\\.","",names(assocs))

#take a look at the sample data
temp<-someblogs[indices]
lapply(temp,as.character)
as.character(someblogs[[56659]])

