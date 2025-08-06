##   en_US.blogs.txt    en_US.news.txt en_US.twitter.txt 
##            899288           1010242           2360148
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US")

#Get all data for final application (the four lines with embedded nul in en_US.twitter.txt are corrected)
sampcon1<-file("twit1.txt","a")
sampcon2<-file("twit2.txt","a")
sampcon3<-file("twit3.txt","a")
sampcon4<-file("twit4.txt","a")
sampcon5<-file("twit5.txt","a")
sampcon6<-file("twit6.txt","a")
sampcon7<-file("twit7.txt","a")
sampcon8<-file("twit8.txt","a")
sampcon9<-file("twit9.txt","a")
sampcon0<-file("twit0.txt","a")
con<-file("en_US.twitter.txt","r")
eachpart<-236014

lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon1)
close(sampcon1)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon2)
close(sampcon2)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon3)
close(sampcon3)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon4)
close(sampcon4)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon5)
close(sampcon5)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon6)
close(sampcon6)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon7)
close(sampcon7)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon8)
close(sampcon8)
lines<-readLines(con, eachpart, encoding="UTF-8")
writeLines(lines,sampcon9)
close(sampcon9)
lines<-readLines(con, encoding="UTF-8")
writeLines(lines,sampcon0)
close(sampcon0)
close(con)
#Open the document got above in Microsoft WORD using Western European (Windows) encoding, and type in "''"\...
#    (use the character saved in "en_US.try.txt" to get left single quotation mark ' and ...),
#    (The characters typed in will be converted to Western European (Windows) encoding automatically)
#Copy and paste into Wordpad, and use the characters got in WORD for "''"... to replace them by normal characters
#You also need to replace \ by \\, otherwise R cannot read it.
#    (Among all the symbols (~`@#$^*{}[]|\<>/",.!?%&()-+=_:;'") only four("''") will change under Western European (Windows) encoding
#    (Note: there are two types of single/double quotation marks in those symbols)
#<U+0092> should be replace by ', <U+0093> and <U+0094> should be replaced by " (did not do it because we can remove them in R)

#preparation for TermDocumentMatrix transform
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata")
library(tm)
library(RWeka)
library(slam)
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
#line<-gsub('#',' number ',line) #so '#1' can be recoded as 'number 1' (keep it for twits)
#separate the punctuations (codes removed)
#profanity words removal
line<-gsub('ass|asshole|damn|goddamn|damnit|damned|fuck|fucked|fucking|f[*]+ck|f[*]+cked|f[*]+cking',' ',line)
}
myTokenizer<-function(x) {NGramTokenizer(x,Weka_control(min=2,max=4))} #remove punctuation (' \r\n\t.,;:'"()?!') by default

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

#tdm.ct<-TermDocumentMatrix(someblogs,control=list(weighting=weightBin))
#termname.ct<-dimnames(tdm.ct)$Terms
#tdm.idf<-TermDocumentMatrix(discrimwords,control=list(weighting=weightTfIdf))
#termname.idf<-dimnames(tdm.idf)$Terms
#save(termfreq,termname,tdm.ct,termname.ct,tdm.idf,termname.idf,file="sampletwitter.RData")
#save(termfreq,termname,file="wholeblogs.RData")

#merge frequency table components
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata")
#merge the first five components
#initialize
cat("Merging part 1 and 2 ... \n")
load("twit1.RData")
load("twit2.RData")
twits<-merge(twit1,twit2,by="term",all=TRUE)
twits$freq.twits<-rowSums(twits[,c("freq.twit1","freq.twit2")],na.rm=TRUE)
twits$freq.twit1<-NULL
twits$freq.twit2<-NULL
rm(twit1,twit2)
#keeping merging
cat("Merging part 3 ... \n")
load("twit3.RData")
twits<-merge(twits,twit3,by="term",all=TRUE)
twits$freq.twits<-rowSums(twits[,c("freq.twits","freq.twit3")],na.rm=TRUE)
twits$freq.twit3<-NULL
rm(twit3)
#keeping merging
memory.limit(30000)
cat("Merging part 4 ... \n")
load("twit4.RData")
twits<-merge(twits,twit4,by="term",all=TRUE)
twits$freq.twits<-rowSums(twits[,c("freq.twits","freq.twit4")],na.rm=TRUE)
twits$freq.twit4<-NULL
rm(twit4)
gc()
#keeping merging
cat("Merging part 5 ... \n")
begintime<-Sys.time()
print(begintime)
load("twit5.RData")
twits<-merge(twits,twit5,by="term",all=TRUE)
twits$freq.twits<-rowSums(twits[,c("freq.twits","freq.twit5")],na.rm=TRUE)
twits$freq.twit5<-NULL
rm(twit5)
save(twits,file="wholetwits1.RData")
print(object.size(twits))
rm(twits)
gc()
print(Sys.time()-begintime)
#merge the last five components
#initialize
cat("Merging part 6 and 7 ... \n")
load("twit6.RData")
load("twit7.RData")
twits2<-merge(twit6,twit7,by="term",all=TRUE)
twits2$freq.twits2<-rowSums(twits2[,c("freq.twit6","freq.twit7")],na.rm=TRUE)
twits2$freq.twit6<-NULL
twits2$freq.twit7<-NULL
rm(twit6,twit7)
#keeping merging
cat("Merging part 8 ... \n")
load("twit8.RData")
twits2<-merge(twits2,twit8,by="term",all=TRUE)
twits2$freq.twits2<-rowSums(twits2[,c("freq.twits2","freq.twit8")],na.rm=TRUE)
twits2$freq.twit8<-NULL
rm(twit8)
#keeping merging
cat("Merging part 9 ... \n")
load("twit9.RData")
twits2<-merge(twits2,twit9,by="term",all=TRUE)
twits2$freq.twits2<-rowSums(twits2[,c("freq.twits2","freq.twit9")],na.rm=TRUE)
twits2$freq.twit9<-NULL
rm(twit9)
gc()
#keeping merging
cat("Merging part 10 ... \n")
begintime<-Sys.time()
print(begintime)
load("twit0.RData")
twits2<-merge(twits2,twit0,by="term",all=TRUE)
twits2$freq.twits2<-rowSums(twits2[,c("freq.twits2","freq.twit0")],na.rm=TRUE)
twits2$freq.twit0<-NULL
rm(twit0)
save(twits2,file="wholetwits2.RData")
print(object.size(twits2))
print(Sys.time()-begintime)
cat("Merging end ... \n")

#preparation for data cleaning for better generalization
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata")
memory.limit(300000)
library(tm)
library(RTextTools)
#library(SnowballC)
#function for normalizing common verb variants, one's, and oneself
normalize<-function(line){
line<-gsub("^brought | brought | brought$|^brought$"," bring ",line)
line<-gsub("^began | began | began$|^began$|^begun | begun | begun$|^begun$"," begin ",line)
line<-gsub("^broke | broke | broke$|^broke$|^broken | broken | broken$|^broken$"," break ",line)
line<-gsub("^built | built | built$|^built$"," build ",line)
line<-gsub("^caught | caught | caught$|^caught$"," catch ",line)
line<-gsub("^came | came | came$|^came$"," come ",line)
line<-gsub("^got | got | got$|^got$"," get ",line)
line<-gsub("^goes | goes | goes$|^goes$|^went | went | went$|^went$|^gone | gone | gone$|^gone$"," go ",line)
line<-gsub("^kept | kept | kept$|^kept$"," keep ",line)
line<-gsub("^made | made | made$|^made$"," make ",line)
line<-gsub("^ran | ran | ran$|^ran$"," run ",line)
line<-gsub("^saw | saw | saw$|^saw$|^seen | seen | seen$|^seen$"," see ",line)
line<-gsub("^sent | sent | sent$|^sent$"," send ",line)
line<-gsub("^sat | sat | sat$|^sat$"," sit ",line)
line<-gsub("^took | took | took$|^took$|^taken | taken | taken$|^taken$"," take ",line)
line<-gsub("^am | am | am$|^am$|^is | is | is$|^is$|^are | are | are$|^are$|^was | was | was$|^was$|^were | were | were$|^were$|^been | been | been$|^been$"," be ",line)
line<-gsub("^clung | clung | clung$|^clung$"," cling ",line)
line<-gsub("^heard | heard | heard$|^heard$"," hear ",line)
line<-gsub("^might | might | might$|^might$"," may ",line)
line<-gsub("^lay | lay | lay$|^lay$|^lain | lain | lain$|^lain$|^laid | laid | laid$|^laid$"," lie ",line)
line<-gsub("^fell | fell | fell$|^fell$|^fallen | fallen | fallen$|^fallen$"," fall ",line)
line<-gsub("^has | has | has$|^has$|^had | had | had$|^had$"," have ",line)
line<-gsub("^does | does | does$|^does$|^did | did | did$|^did$|^done | done | done$|^done$"," do ",line)
line<-gsub("^gave | gave | gave$|^gave$|^given | given | given$|^given$"," give ",line)
line<-gsub("^your | your | your$|^your$|^his | his | his$|^his$|^her | her | her$|^her$|^its | its | its$|^its$|^our | our | our$|^our$|^their | their | their$|^their$"," my ",line)
#line<-gsub(" the$| a$| an$","",line)
line<-gsub("^yourself | yourself | yourself$|^yourself$|^himself | himself | himself$|^himself$|^herself | herself | herself$|^herself$|^itself | itself | itself$|^itself$
	|^ourselves | ourselves | ourselves$|^ourselves$|^yourselves | yourselves | yourselves$|^yourselves$|^themselves | themselves | themselves$|^themselves$"," myself ",line)
}

#start data cleaning for better generalization (normalization)
begintime<-Sys.time()
begintime
load("wholetwits.RData")
dim(twits)
#split term into head and tail, normalize and stem the head, and combine them afterwards
splitted<-strsplit(as.character(twits$term)," ")
rm(twits)
tail<-sapply(splitted, function(words) words[length(words)])
head<-sapply(splitted, function(words) paste(words[1:(length(words)-1)],collapse=" "))
rm(splitted)
head<-normalize(head)
head<-sapply(head, function(words) paste(wordStem(unlist(strsplit(words," ")),language="english"),collapse=" "))
normterm<-paste(head,tail)
normterm<-gsub("^[[:space:]]+|[[:space:]]+$","",normterm)
normterm<-gsub("[[:space:]]+"," ",normterm)
rm(head,tail)
load("wholetwits.RData")
normtwits<-twits$freq.twits
rm(twits)
normtwits<-tapply(normtwits,normterm,sum)
length(normtwits)
save(normtwits,file="normtwits.RData")
rm(normterm,normtwits)
gc()
load("wholetwits2.RData")
dim(twits2)
#split term into head and tail, normalize and stem the head, and combine them afterwards
splitted<-strsplit(as.character(twits2$term)," ")
rm(twits2)
tail<-sapply(splitted, function(words) words[length(words)])
head<-sapply(splitted, function(words) paste(words[1:(length(words)-1)],collapse=" "))
rm(splitted)
head<-normalize(head)
head<-sapply(head, function(words) paste(wordStem(unlist(strsplit(words," ")),language="english"),collapse=" "))
normterm<-paste(head,tail)
normterm<-gsub("^[[:space:]]+|[[:space:]]+$","",normterm)
normterm<-gsub("[[:space:]]+"," ",normterm)
rm(head,tail)
load("wholetwits2.RData")
normtwits2<-twits2$freq.twits2
rm(twits2)
normtwits2<-tapply(normtwits2,normterm,sum)
length(normtwits2)
save(normtwits2,file="normtwits2.RData")
rm(normterm,normtwits2)
gc()
Sys.time()-begintime

#splitting according to gram and string order
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata")
memory.limit(30000)
load("normtwits.RData")
normname<-names(normtwits)
part1.twits<-normtwits[normname<"m"]
part2.twits<-normtwits[normname>="m"]
rm(normtwits)

normname<-names(part1.twits)
gram<-strsplit(normname," ")
gram<-sapply(gram,length)
gram2.part1.twits<-part1.twits[gram==2]
gram3.part1.twits<-part1.twits[gram==3]
gram4.part1.twits<-part1.twits[gram==4]
rm(part1.twits)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
save(gram2.part1.twits,file="gram2.part1.twits.RData")
save(gram3.part1.twits,file="gram3.part1.twits.RData")
save(gram4.part1.twits,file="gram4.part1.twits.RData")
rm(gram2.part1.twits,gram3.part1.twits,gram4.part1.twits)

normname<-names(part2.twits)
gram<-strsplit(normname," ")
gram<-sapply(gram,length)
gram2.part2.twits<-part2.twits[gram==2]
gram3.part2.twits<-part2.twits[gram==3]
gram4.part2.twits<-part2.twits[gram==4]
rm(part2.twits)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
save(gram2.part2.twits,file="gram2.part2.twits.RData")
save(gram3.part2.twits,file="gram3.part2.twits.RData")
save(gram4.part2.twits,file="gram4.part2.twits.RData")
rm(gram2.part2.twits,gram3.part2.twits,gram4.part2.twits)
#This finishes the intermediate data generation.
#Splitting.R, Normalization.R, Merging.R, TDMtransform.R are used to facilitate the process by using ReplaceALL to reuse the codes as if they are macros

################################################################################
## online part
#shrink the data for efficient prediction
lessterm<-termfreq[termfreq>1]
lessname<-names(lessterm)
gram<-strsplit(lessname," ")
gram<-sapply(gram,length)
gram1<-lessterm[gram==2]
gram2<-lessterm[gram==3]
gram3<-lessterm[gram==4]
name1<-names(gram1)
name2<-names(gram2)
name3<-names(gram3)

#devide into parts to make it faster (should be less than 200000, better less than 20000)
temp<-gram2[name2<"d"]
temp<-gram2[name2>="d" & name2<"j"]
temp<-gram2[name2>="j" & name2<"p"]
temp<-gram2[name2>="p" & name2<"thf"]
temp<-gram2[name2>="thf"]
length(temp)

## online part start
trymatch<-function(head,trmfreq,trmname){
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,trmname,value=T)
matchedTF<-trmfreq[matchednames]
matchedTF<-sort(matchedTF,decreasing=T)
matchednames<-names(matchedTF)
matchednames<-strsplit(matchednames," ")
matchednames<-sapply(matchednames, function(words) words[length(words)])
}
match<-function(string){
string<-preprocess(string)
string<-removePunctuation(string)
string<-removeNumbers(string)
string<-gsub("^[[:space:]]+|[[:space:]]+$","",string)
string<-gsub("[[:space:]]+"," ",string)
string<-tail(unlist(strsplit(string," ")),3)
string<-paste(string,collapse=" ")
string<-normalize(string)
string<-paste(wordStem(unlist(strsplit(string," ")),language="english"),collapse=" ")
string<-gsub("^[[:space:]]+|[[:space:]]+$","",string)
string<-gsub("[[:space:]]+"," ",string)
string<-tail(unlist(strsplit(string," ")),3)
#start from 4-gram
pred<-NULL
for(gram in 3:1){
head<-paste(tail(string,gram),collapse=" ")
if (gram==3) matchednames<-trymatch(head,gram3,name3)
else if (gram==2) matchednames<-trymatch(head,gram2,name2)
else if (gram==1) matchednames<-trymatch(head,gram1,name1)
matchednames<-setdiff(matchednames,pred)
pred<-c(pred,matchednames)
if (length(pred)>=3) break;
}
if (length(pred)<3) {
matchednames<-setdiff(c("the","to","and"),pred)
pred<-c(pred,matchednames)
}
pred[1:3]
}

#test the algorithm
incomplete<-"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
incomplete<-"You are the reason why I smile every day. Can you follow me please? It would mean the"
incomplete<-"Ohhhhh #PointBreak is on tomorrow at 3pm. Love that film and haven't seen it in quite some"
incomplete<-"Every inch of you is perfect from the bottom to the"
match(incomplete)









#load("sampletwitter.RData")
lessname<-findFreqTerms(termfreq,20) #should be less than 200000, better less than 20000
length(lessname)
lessterm<-termfreq[lessname,]

preponline<-function(line){
line<-tolower(line)
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
line<-gsub('[^-a-z ]',' ',line) #retain only words
line<-gsub('-+ | -+',' ',line) #remove heading or tailing -
#profanity words removal
line<-gsub('ass|asshole|damn|goddamn|damnit|damned|fuck|fucked|fucking',' ',line)
}

trymatch<-function(regexpr,trmfreq,trmname){
matchednames<-grep(regexpr,trmname,value=T)
matchedTF<-trmfreq[matchednames,]
matchednames<-sapply(strsplit(matchednames," "),tail,1)
matchedTF<-as.vector(matchedTF)
names(matchedTF)<-matchednames
matchedTF<-sort(matchedTF,decreasing=T)
}

match<-function{string){
string<-preponline(string)
#prepare the regular expression for search (use 1-gram?)
string<-unlist(strsplit(string," "))
strhead<-string[length(string)-1]
strtail<-string[length(string)]
regexpr<-paste0("(^",strhead,"| ",strhead,") ",strtail," [a-z]+$")
#try matching using the small tdm
pred<-trymatch(regexpr,lessterm,lessname)
if (length(pred)<3) {pred<-trymatch(regexpr,termfreq,termname)}
    else {

#under development
matchednames<-names(pred)
string<-unique(string)
tdm.string<-tdm.idf[termname.idf %in% string,]
tdm.candct<-tdm.ct[termname.ct %in% matchednames,]
temp<-tcrossprod_simple_triplet_matrix(tdm.string,tdm.candct)
as.matrix(temp)





         Terms
Terms       airport    beach      best      city      end      game      gym      mall       new       next     number    people     point   public      show    store
  bottom  0.0000000 0.000000  3.218383 0.9752677  0.00000  3.796042 0.000000 0.8939954  1.191994  0.7151963   6.759086  9.233695 0.8939954 0.000000  1.690464 0.000000
  every   0.4299787 1.747215 31.312044 9.6787072 11.79668 28.481402 2.790084 0.0000000 49.736649 12.6335884 141.762738 31.650065 4.5776579 2.374004 25.533542 2.014757
  inch    0.0000000 0.000000  0.000000 0.0000000  0.00000  0.000000 0.000000 0.0000000  2.230737  0.0000000   2.560498  0.000000 1.2802492 0.000000  0.000000 0.000000
  perfect 0.0000000 5.464962  9.855043 4.5917811 14.39340 25.287690 0.000000 0.0000000 13.314765  6.4781341  78.374461 15.737406 5.6362636 1.034629  6.371046 0.000000
         Terms
Terms           top    weekend    world
  bottom  18.323115  0.8939954  0.00000
  every    2.541652 14.6142875 19.10449
  inch     0.000000  0.8534995  0.00000
  perfect  1.442210 20.0872716 11.06262



}
}

incomplete<-"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
incomplete<-"You are the reason why I smile every day. Can you follow me please? It would mean the"
incomplete<-"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
incomplete<-"Every inch of you is perfect from the bottom to the"
match("quite some",termfreq,termname)
match("to the",lessterm,lessname) #use the maximum occurence term with "bottom"


#search for the best n-gram to use
match<-function(string,trmfreq,trmname){
denominator<-as.vector(trmfreq[string,])
splitted<-unlist(strsplit(string,' '))
strhead<-splitted[1]
strtail<-gsub("^.*? ","",string)
regexpr<-paste0("(^",strhead,"| ",strhead,") ",strtail," [a-z]+$")
matchedindex<-grep(regexpr,trmname)
matchedTF<-trmfreq[matchedindex,]
matchednames<-trmname[matchedindex]
matchedTF<-as.vector(matchedTF)/denominator
names(matchedTF)<-matchednames
matchedTF<-sort(matchedTF,decreasing=T)
matchedTF
}

match("quite some",lessterm,lessname)
match("it in quite some",termfreq,termname)

#to make the matching more generalizable
#recode word variants
#line<-gsub(" am|is|are|was|were|been|being "," am ",line)
#line<-gsub(" my|his|her|our|your|their "," my ",line)
#unifying money, time, and number
#line<-gsub('\\$[0-9]+((,[0-9]+)*)(.[0-9]+)?','#money#',line)
#line<-gsub('([0-9]){1,2}(:([0-9]){1,2})+','#time#',line)
#line<-gsub('[0-9]+((,[0-9]+)*)(.[0-9]+)?','#number#',line)
#library(SnowballC)
#line<-stemDocument(line,language="english")

#profanitylist<-readLines("profanity.txt") #removing profanity words
#someblogs<-tm_map(someblogs,removeWords,profanitylist)
#discrimwords<-tm_map(someblogs,removeWords,stopwords("english"))
##discrimwords<-tm_map(discrimwords,removeWords,c("also", "but", "just","however", "just"))

#merge the first five and the last five
#load("wholeblogs1.RData")
#load("wholeblogs2.RData")
#begintime<-Sys.time()
#blogs<-merge(blogs,blogs2,by="term",all=TRUE)
#rm(blogs2)
#blogs$freq.blogs<-rowSums(blogs[,c("freq.blogs","freq.blogs2")],na.rm=TRUE)
#blogs$freq.blogs2<-NULL
#save(blogs,file="wholeblogs.RData")
#Sys.time()-begintime

