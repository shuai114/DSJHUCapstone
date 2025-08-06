#preparation for data cleaning for better generalization
setwd("C:/shuai personal stuff/My documents/jobs/en_US")
memory.limit(30000)
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

#start data cleaning for better generalization
begintime<-Sys.time()
begintime
load("wholetwits1.RData")
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
load("wholetwits1.RData")
normtwits<-twits$freq.twits
rm(twits)
normtwits<-tapply(normtwits,normterm,sum)
rm(normterm)
save(normtwits,file="normtwits.RData")
Sys.time()-begintime

#shrink the data for efficient prediction
load("normtwits.RData")
lessterm<-normtwits[normtwits>1]
rm(normtwits)
lessname<-names(lessterm)
gram<-strsplit(lessname," ")
rm(lessname)
gram<-sapply(gram,length)
gram1<-lessterm[gram==2]
gram2<-lessterm[gram==3]
gram3<-lessterm[gram==4]
rm(lessterm)
save(gram1,gram2,gram3,file="twitgrams.RData")
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

#approximate 1-gram to be used in ordering
load("normtwits.RData")
memory.limit(30000)
gram<-strsplit(names(normtwits)," ")
gram<-sapply(gram,length)
unigram<-normtwits[gram==2]
rm(normtwits,gram)
terms<-unlist(strsplit(names(unigram)," "))
unigram<-rep(unigram,each=2)
names(unigram)<-terms
unigram<-tapply(unigram,terms,sum)
save(unigram,file="unigram.RData")

## online part start
memory.limit(30000)
library(tm)
library(RTextTools)
load("twitgrams.RData")
load("unigram.RData")
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

#test the algorithm
string<-"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
string<-"You are the reason why I smile every day. Can you follow me please? It would mean the"
string<-"Ohhhhh #PointBreak is on tomorrow at 3pm. Love that film and haven't seen it in quite some"
string<-"Every inch of you is perfect from the bottom to the"
string<-"Hey sunshine, can you follow me and make me the"
string<-"I like how the same people are in almost all of Adam Sandler's"
string<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
string<-"I'd give anything to see arctic monkeys this"

string<-"When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
string<-"When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
string<-"Go on a romantic date at the"
string<-"Very early observations on the Bills game: Offense still struggling but the"
string<-"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
string<-"Be grateful for the good times and keep the faith during the"
string<-"If this isn't the cutest thing you've ever seen, then you must be"
string<-"Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
string<-"Talking to your mom has the same effect as a hug and helps reduce your"
string<-"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
string<-"I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
string<-"I'm thankful my childhood was filled with imagination and bruises from playing"
match(string)

#preparation for Katz's back-off model
#calculate the dr
load("normtwits.RData")
Nr<-tapply(normtwits,normtwits,length)
rm(normtwits)
r<-as.numeric(names(Nr))
q<-c(0,r[1:(length(r)-1)])
lastqr<-tail(r,2)
t<-c(r[2:length(r)],2*lastqr[2]-lastqr[1])
logZr<-log(Nr*2/(t-q))
rm(q,t,lastqr)
logr<-log(r)
regmodel<-lm(logZr~logr)
rm(logZr,logr)
SNr<-exp(predict(regmodel,newdata=data.frame(logr=log(1:(max(r)+1)))))
rm(regmodel)
rrp1<-1:max(r)+1
y<-rrp1/SNr[1:max(r)]*SNr[2:length(SNr)]
rm(SNr)
#nextfreqexist<-c(r[2:length(r)]-r[1:(length(r)-1)]==1,FALSE)
rNr<-data.frame(r=r,Nr=Nr)
rm(Nr)
Base<-data.frame(r=1:max(r))
rNr<-merge(Base,rNr,by="r",all=T)
rm(Base)
xNr<-c(rNr$Nr,NA)
rm(rNr)
Nrr<-xNr[1:max(r)]
Nrrp1<-xNr[2:length(xNr)]
rm(xNr)
x<-rrp1/Nrr*Nrrp1
threshold<-qnorm(0.975)*sqrt(rrp1^2/Nrr^2*Nrrp1*(1+Nrrp1/Nrr)) #or use qnorm(0.95)
rm(Nrr,Nrrp1)
crit<-abs(x-y)>threshold
rm(threshold)
crit<-!is.na(crit)&crit
y[crit]=x[crit]
rm(x,crit)
dr<-y[r]/r
save(dr,file="dr.RData")

#get data ready
load("normtwits.RData")
normname<-names(normtwits)
gram<-strsplit(normname," ")
rm(normname)
gram<-sapply(gram,length)
gram1<-normtwits[gram==2]
gram2<-normtwits[gram==3]
gram3<-normtwits[gram==4]
rm(gram,normtwits)
save(gram1,gram2,gram3,file="twitgramsincl1.RData")
name1<-names(gram1)
name2<-names(gram2)
name3<-names(gram3)
load("dr.RData")
load("unigram.RData")

#Katz's back-off model (not fully implemented, just test it and show it is not promising)
string<-onlinepreproc(string)
string<-tail(unlist(strsplit(string," ")),3)
string<-paste(string,collapse=" ")
string<-onlinepostproc(string)
string<-tail(unlist(strsplit(string," ")),3)
#probability calculation based on 3-gram
gram<-3
head<-paste(tail(string,gram),collapse=" ")
head
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,name3,value=T)
length(matchednames)
matchedTF<-gram3[matchednames]
denom<-gram2[head]
matchedTF<-matchedTF*dr[as.character(matchedTF)]/denom
matchedTF<-sort(matchedTF,decreasing=T)
head(matchedTF,20)
#pred<-matchedTF
#probability calculation based on 2-gram
#beta<-1-sum(matchedTF)
gram<-2
head<-paste(tail(string,gram),collapse=" ")
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,name2,value=T)
length(matchednames)
matchedTF<-gram2[matchednames]
denom<-gram1[head]
matchedTF<-matchedTF*dr[as.character(matchedTF)]/denom
matchedTF<-sort(matchedTF,decreasing=T)
head(matchedTF,20)
#probability calculation based on 1-gram
gram<-1
head<-paste(tail(string,gram),collapse=" ")
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,name1,value=T)
length(matchednames)
matchedTF<-gram1[matchednames]
denom<-unigram[head]
matchedTF<-matchedTF*dr[as.character(matchedTF)]/denom
matchedTF<-sort(matchedTF,decreasing=T)
head(matchedTF,20)

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

