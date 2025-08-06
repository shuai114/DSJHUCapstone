#final app is lauched at https://shuai114.shinyapps.io/JHUDSCapstone
#the corresponding slide decks are at http://rpubs.com/shuai114/104748
setwd("C:/shuai personal stuff/My documents/jobs/en_US/finaldata/finalapp")
library(tm)
library(RTextTools)
load("finalterms_thin2.RData")

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
trymatch<-function(head,trmname,lasttrm){
regexpr<-paste0("^",head,"$")
matchedindex<-grep(regexpr,trmname)
lasttrm[matchedindex]
}
match<-function(string){
#annotate the following line when doing evaluation
string<-onlinepreproc(string)
string<-tail(unlist(strsplit(string," ")),3)
string<-paste(string,collapse=" ")
string<-normalize(string)
string<-paste(wordStem(unlist(strsplit(string," ")),language="english"),collapse=" ")
string<-gsub("^[[:space:]]+|[[:space:]]+$","",string)
string<-gsub("[[:space:]]+"," ",string)
string<-tail(unlist(strsplit(string," ")),3)
#start from 4-gram
pred<-character()
for(gram in 3:1){
head<-paste(tail(string,gram),collapse=" ")
if (gram==3 & head<"at") matchednames<-trymatch(head,name41,lastword41)
else if (gram==3 & head>="at" & head<"che") matchednames<-trymatch(head,name42,lastword42)
else if (gram==3 & head>="che" & head<"foo") matchednames<-trymatch(head,name43,lastword43)
else if (gram==3 & head>="foo" & head<"i") matchednames<-trymatch(head,name44,lastword44)
else if (gram==3 & head>="i" & head<"ma") matchednames<-trymatch(head,name45,lastword45)
else if (gram==3 & head>="ma" & head<"of") matchednames<-trymatch(head,name46,lastword46)
else if (gram==3 & head>="of" & head<"re") matchednames<-trymatch(head,name47,lastword47)
else if (gram==3 & head>="re" & head<"ta") matchednames<-trymatch(head,name48,lastword48)
else if (gram==3 & head>="ta" & head<"ti") matchednames<-trymatch(head,name49,lastword49)
else if (gram==3 & head>="ti" & head<"w") matchednames<-trymatch(head,name40,lastword40)
else if (gram==3 & head>="w") matchednames<-trymatch(head,name4a,lastword4a)
else if (gram==2 & head<"at") matchednames<-trymatch(head,name31,lastword31)
else if (gram==2 & head>="at" & head<"che") matchednames<-trymatch(head,name32,lastword32)
else if (gram==2 & head>="che" & head<"foo") matchednames<-trymatch(head,name33,lastword33)
else if (gram==2 & head>="foo" & head<"i") matchednames<-trymatch(head,name34,lastword34)
else if (gram==2 & head>="i" & head<"ma") matchednames<-trymatch(head,name35,lastword35)
else if (gram==2 & head>="ma" & head<"of") matchednames<-trymatch(head,name36,lastword36)
else if (gram==2 & head>="of" & head<"re") matchednames<-trymatch(head,name37,lastword37)
else if (gram==2 & head>="re" & head<"ta") matchednames<-trymatch(head,name38,lastword38)
else if (gram==2 & head>="ta" & head<"ti") matchednames<-trymatch(head,name39,lastword39)
else if (gram==2 & head>="ti" & head<"w") matchednames<-trymatch(head,name30,lastword30)
else if (gram==2 & head>="w") matchednames<-trymatch(head,name3a,lastword3a)
else if (gram==1 & head<"at") matchednames<-trymatch(head,name21,lastword21)
else if (gram==1 & head>="at" & head<"che") matchednames<-trymatch(head,name22,lastword22)
else if (gram==1 & head>="che" & head<"foo") matchednames<-trymatch(head,name23,lastword23)
else if (gram==1 & head>="foo" & head<"i") matchednames<-trymatch(head,name24,lastword24)
else if (gram==1 & head>="i" & head<"ma") matchednames<-trymatch(head,name25,lastword25)
else if (gram==1 & head>="ma" & head<"of") matchednames<-trymatch(head,name26,lastword26)
else if (gram==1 & head>="of" & head<"re") matchednames<-trymatch(head,name27,lastword27)
else if (gram==1 & head>="re" & head<"ta") matchednames<-trymatch(head,name28,lastword28)
else if (gram==1 & head>="ta" & head<"ti") matchednames<-trymatch(head,name29,lastword29)
else if (gram==1 & head>="ti" & head<"w") matchednames<-trymatch(head,name20,lastword20)
else if (gram==1 & head>="w") matchednames<-trymatch(head,name2a,lastword2a)
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

#Profiling the program
Rprof()
Rprof(NULL)
summaryRprof()

#test the algorithm and the number of correct predictions
#finaldata_thin: 7/20
#0.03 to 0.06 seconds most of the time, all less than 0.12 seconds
string<-"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"You are the reason why I smile every day. Can you follow me please? It would mean the"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Ohhhhh #PointBreak is on tomorrow at 3pm. Love that film and haven't seen it in quite some"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Every inch of you is perfect from the bottom to the"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Hey sunshine, can you follow me and make me the"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"I like how the same people are in almost all of Adam Sandler's"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
begintime<-Sys.time()
match(string)
Sys.time()-begintime

#not predicted correctly by finaldata_thin, but predicted correctly by other dataset
string<-"I'd give anything to see arctic monkeys this"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"I'm thankful my childhood was filled with imagination and bruises from playing"
begintime<-Sys.time()
match(string)
Sys.time()-begintime

##not predicted correctly
string<-"When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Go on a romantic date at the"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Very early observations on the Bills game: Offense still struggling but the"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Be grateful for the good times and keep the faith during the"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"If this isn't the cutest thing you've ever seen, then you must be"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"Talking to your mom has the same effect as a hug and helps reduce your"
begintime<-Sys.time()
match(string)
Sys.time()-begintime
string<-"I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
begintime<-Sys.time()
match(string)
Sys.time()-begintime

#evaluation using random testset
#prediction accuracy (first word, second word, third word, total):
#finalterms_thin2 for test1: (0.16, 0.10, 0.04, 0.30)
#finalterms_thin2 for test2: (0.17, 0.02, 0.04, 0.23)
load("testset.RData")
test<-test1 #only change here
test<-sapply(test,onlinepreproc)
splitted<-strsplit(test," ")
nextword<-sapply(splitted, function(words) words[length(words)])
string<-sapply(splitted, function(words) paste(words[1:(length(words)-1)],collapse=" "))
predicted<-sapply(string,match)
sum(predicted[1,]==nextword)/length(nextword)
sum(predicted[2,]==nextword)/length(nextword)
sum(predicted[3,]==nextword)/length(nextword)

