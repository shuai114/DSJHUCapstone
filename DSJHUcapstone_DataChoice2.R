#data preparation
setwd("C:/shuai personal stuff/My documents/jobs/en_US/finaldata")
memory.limit(30000)
library(tm)
library(RTextTools)

#shrink the data for efficient prediction
load("normblogs.RData")
lessterm<-normblogs[normblogs>1]
rm(normblogs)
lessname<-names(lessterm)
gram<-strsplit(lessname," ")
rm(lessname)
gram<-sapply(gram,length)
gram1<-lessterm[gram==2]
gram2<-lessterm[gram==3]
gram3<-lessterm[gram==4]
rm(lessterm)
save(gram1,gram2,gram3,file="blogsgrams.RData")
name1<-names(gram1)
name2<-names(gram2)
name3<-names(gram3)

#approximate 1-gram to be used in ordering
begintime<-Sys.time()
begintime
load("normnewss.RData")
#memory.limit(30000)
gram<-strsplit(names(normnewss)," ")
gram<-sapply(gram,length)
unigram<-normnewss[gram==2]
rm(normnewss,gram)
terms<-strsplit(names(unigram)," ")
terms<-sapply(terms, function(words) words[2])
names(unigram)<-terms
unigram<-tapply(unigram,terms,sum)
rm(terms)
save(unigram,file="unigram_newss.RData")
Sys.time()-begintime

###############################################################################3
## online part start
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
onlinepreproc<-function(string){
string<-preprocess(string)
string<-removePunctuation(string)
string<-removeNumbers(string)
string<-gsub("^[[:space:]]+|[[:space:]]+$","",string)
string<-gsub("[[:space:]]+"," ",string)
}
onlinepostproc<-function(string){
string<-normalize(string)
string<-paste(wordStem(unlist(strsplit(string," ")),language="english"),collapse=" ")
string<-gsub("^[[:space:]]+|[[:space:]]+$","",string)
string<-gsub("[[:space:]]+"," ",string)
}
trymatch<-function(head,trmfreq,trmname){
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,trmname,value=T)
if (length(matchednames)>0) {
matchedword<-strsplit(matchednames," ")
matchedword<-sapply(matchedword, function(words) words[length(words)])
matchedTF<-trmfreq[matchednames]+1/(unigram[matchedword]+1)
names(matchedTF)<-matchedword
matchedTF<-sort(matchedTF,decreasing=T)
matchednames<-names(matchedTF)
} else {matchednames<-character()
}
matchednames
}
match<-function(string){
string<-onlinepreproc(string)
string<-tail(unlist(strsplit(string," ")),3)
string<-paste(string,collapse=" ")
string<-onlinepostproc(string)
string<-tail(unlist(strsplit(string," ")),3)
#start from 4-gram
pred<-character()
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

#load the training data
memory.limit(30000)
library(tm)
library(RTextTools)
load("twitsgrams.RData")
load("unigram_twits.RData")
name1<-names(gram1)
name2<-names(gram2)
name3<-names(gram3)

#test the algorithm and the number of correct predictions
#twitsgram: 8/20
#newssgram: 2/20
#blogsgram: 9/20
#twits_newssgrams: 6/20
#twits_blogsgrams: 8/20
#using match function for combined data from twits and blogs: 10/20
##the first ten are predicted correctly by using match function for combined data from twits and blogs
##predicted correctly by twitter and blogs
string<-"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
match(string)
string<-"You are the reason why I smile every day. Can you follow me please? It would mean the"
match(string)
string<-"Ohhhhh #PointBreak is on tomorrow at 3pm. Love that film and haven't seen it in quite some"
match(string)
string<-"Every inch of you is perfect from the bottom to the"
match(string)
string<-"Hey sunshine, can you follow me and make me the"
match(string)
#combined twits and blogs missed it, but each can predict it separately
string<-"I like how the same people are in almost all of Adam Sandler's"
match(string)
#blogs missed predicting it
string<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
match(string)
#blogs missed predicting it, so did the combined twits and blogs
string<-"I'd give anything to see arctic monkeys this"
match(string)
##predicted correctly by blogs (and the combined twits and blogs), not twitter
string<-"When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
match(string)
string<-"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
match(string)
##predicted correctly by blogs, not twitter (or the combined twits and blogs)
string<-"I'm thankful my childhood was filled with imagination and bruises from playing"
match(string)

##not predicted correctly
string<-"When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
match(string)
string<-"Go on a romantic date at the"
match(string)
string<-"Very early observations on the Bills game: Offense still struggling but the"
match(string)
string<-"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
match(string)
string<-"Be grateful for the good times and keep the faith during the"
match(string)
string<-"If this isn't the cutest thing you've ever seen, then you must be"
match(string)
string<-"Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
match(string)
string<-"Talking to your mom has the same effect as a hug and helps reduce your"
match(string)
string<-"I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
match(string)

#use combined training set
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/training")
load("gram4.part1.all.RData")
load("gram4.part2.all.RData")
gram3<-c(gram4.part1.all,gram4.part2.all)
rm(gram4.part1.all,gram4.part2.all)
gram3<-gram3[gram3>1]
load("gram3.part1.all.RData")
load("gram3.part2.all.RData")
gram2<-c(gram3.part1.all,gram3.part2.all)
rm(gram3.part1.all,gram3.part2.all)
gram2<-gram2[gram2>1]
load("gram2.part1.all.RData")
load("gram2.part2.all.RData")
gram1<-c(gram2.part1.all,gram2.part2.all)
rm(gram2.part1.all,gram2.part2.all)
gram1<-gram1[gram1>1]
save(gram1,gram2,gram3,file="allgrams.RData")

begintime<-Sys.time()
begintime
load("gram2.part1.all.RData")
load("gram2.part2.all.RData")
unigram<-c(gram2.part1.all,gram2.part2.all)
rm(gram2.part1.all,gram2.part2.all)
terms<-strsplit(names(unigram)," ")
terms<-sapply(terms, function(words) words[2])
names(unigram)<-terms
unigram<-tapply(unigram,terms,sum)
rm(terms)
save(unigram,file="unigram_all.RData")
Sys.time()-begintime

#altered match function
match<-function(string){
#string<-onlinepreproc(string) #the only change for evaluation
string<-tail(unlist(strsplit(string," ")),3)
string<-paste(string,collapse=" ")
string<-onlinepostproc(string)
string<-tail(unlist(strsplit(string," ")),3)
#start from 4-gram
pred<-character()
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

#evaluation using random testset
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
library(tm)
library(RTextTools)
memory.limit(30000)
load("twits_newssgrams.RData")
load("unigram_twits_newss.RData")
name1<-names(gram1)
name2<-names(gram2)
name3<-names(gram3)

#prediction accuracy (first word, second word, third word, total):
#newssgrams for newstest: (0.17, 0.08, 0.05, 0.30)
#twitsgrams for twittest: (0.14, 0.12, 0.05, 0.31)
#newssgrams for test1: (0.14, 0.07, 0.05, 0.26)
#twitsgrams for test1: (0.16, 0.06, 0.03, 0.25)
#blogsgrams for test1: (0.12, 0.05, 0.05, 0.22)
#twits_newssgrams for test1: (0.13, 0.12, 0.04, 0.29)
#twits_blogsgrams for test1: (0.17, 0.04, 0.03, 0.24)
#allgrams for test1: (0.15, 0.09, 0.03, 0.27)
#using new trymatch and match function for combined data from twits and newss for test1: (0.15, 0.09, 0.03, 0.27)
load("testset.RData")
test<-test1 #only change here
test<-sapply(test,onlinepreproc)
splitted<-strsplit(test," ")
nextword<-sapply(splitted, function(words) words[length(words)])
string<-sapply(splitted, function(words) paste(words[1:(length(words)-1)],collapse=" "))
begintime<-Sys.time()
predicted<-sapply(string,match)
sum(predicted[1,]==nextword)/length(nextword)
sum(predicted[2,]==nextword)/length(nextword)
sum(predicted[3,]==nextword)/length(nextword)
Sys.time()-begintime

##########################################################################################################################################
#new trymatch and match function for combined data
trymatch<-function(head,trmfreq,trmname,trmgram1){
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,trmname,value=T)
if (length(matchednames)>0) {
matchedword<-strsplit(matchednames," ")
matchedword<-sapply(matchedword, function(words) words[length(words)])
matchedTF<-trmfreq[matchednames]+1/(trmgram1[matchedword]+1)
names(matchedTF)<-matchedword
} else {matchedTF<-numeric()
}
matchedTF
}
match<-function(string){
string<-onlinepreproc(string)
string<-tail(unlist(strsplit(string," ")),3)
string<-paste(string,collapse=" ")
string<-onlinepostproc(string)
string<-tail(unlist(strsplit(string," ")),3)
#start from 4-gram
pred<-character()
for(gram in 3:1){
head<-paste(tail(string,gram),collapse=" ")
if (gram==3) matchedTF<-c(trymatch(head,twits.gram3,twits.name3,twits.unigram),trymatch(head,newss.gram3,newss.name3,newss.unigram))
else if (gram==2) matchedTF<-c(trymatch(head,twits.gram2,twits.name2,twits.unigram),trymatch(head,newss.gram2,newss.name2,newss.unigram))
else if (gram==1) matchedTF<-c(trymatch(head,twits.gram1,twits.name1,twits.unigram),trymatch(head,newss.gram1,newss.name1,newss.unigram))
matchednames<-names(matchedTF)
matchedTF<-tapply(matchedTF,matchednames,max)
matchedTF<-sort(matchedTF,decreasing=T)
matchednames<-names(matchedTF)
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

#get the data for new trymatch and match function (for combined data)
memory.limit(30000)
library(tm)
library(RTextTools)
setwd("C:/shuai personal stuff/My documents/jobs/en_US/finaldata/training")
load("twitsgrams.RData")
load("unigram_twits.RData")
twits.gram1<-gram1
twits.gram2<-gram2
twits.gram3<-gram3
twits.unigram<-unigram
twits.name1<-names(twits.gram1)
twits.name2<-names(twits.gram2)
twits.name3<-names(twits.gram3)

load("blogsgrams.RData")
load("unigram_blogs.RData")
blogs.gram1<-gram1
blogs.gram2<-gram2
blogs.gram3<-gram3
blogs.unigram<-unigram
blogs.name1<-names(blogs.gram1)
blogs.name2<-names(blogs.gram2)
blogs.name3<-names(blogs.gram3)
rm(gram1,gram2,gram3,unigram)

load("newssgrams.RData")
load("unigram_newss.RData")
newss.gram1<-gram1
newss.gram2<-gram2
newss.gram3<-gram3
newss.unigram<-unigram
newss.name1<-names(newss.gram1)
newss.name2<-names(newss.gram2)
newss.name3<-names(newss.gram3)
rm(gram1,gram2,gram3,unigram)

