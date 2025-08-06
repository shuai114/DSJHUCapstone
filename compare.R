trymatch<-function(head,trmfreq,trmname){
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,trmname,value=T)
if (length(matchednames)>0) {
matchedword<-strsplit(matchednames," ")
matchedword<-sapply(matchedword, function(words) words[length(words)])
matchedTF<-trmfreq[matchednames]/denom
matchedTF<-matchedTF/sum(matchedTF)+1/(unigram[matchedword]+sum(matchedTF))
names(matchedTF)<-matchedword
matchedTF<-sort(matchedTF,decreasing=T)
} else {matchedTF<-numeric()
}
matchedTF
}

#blogs missed predicting it
string<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
#blogs missed predicting it, so did the combined twits and blogs
string<-"I'd give anything to see arctic monkeys this"
##predicted correctly by blogs (and the combined twits and blogs), not twitter
string<-"When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
string<-"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
##predicted correctly by blogs, not twitter (or the combined twits and blogs)
string<-"I'm thankful my childhood was filled with imagination and bruises from playing"

string<-onlinepreproc(string)
string<-tail(unlist(strsplit(string," ")),3)
string<-paste(string,collapse=" ")
string<-onlinepostproc(string)
string<-tail(unlist(strsplit(string," ")),3)

gram<-3
head<-paste(tail(string,gram),collapse=" ")
trymatch(head,gram3,name3)

gram<-2
head<-paste(tail(string,gram),collapse=" ")
trymatch(head,gram2,name2)

gram<-1
head<-paste(tail(string,gram),collapse=" ")
trymatch(head,gram1,name1)[1:7]



gram==3:
      the      beer    miller     water 
32.000002  4.000335  2.001527  2.000395 
gram==1:
         is             weekend                week             morning                year                time                 one              summer 
7921.000004         3823.000090         3592.000070         3418.000082         2881.000076         1222.000028         1168.000026         1131.000208 
gram==3:
       nap      break     shower       look    picture     moment        pic       trip      stand       shit       joke       test       walk        few       lead     chance 
 54.000872  40.000270  31.001028  25.000086  18.000438  17.000306  16.000488  10.000381   8.000489   8.000123   7.000685   7.000546   7.000352   7.000175   6.000606   6.000319 
gram==3: score: 2.000904; gram==2: score: 3.000904; gram==1:
  best   same  world    new  first    day follow 
  9313   6311   5399   4980   4797   4585   4120 
 matter 
71.0004 
incident 
   9.008 
gram==2: the: 4.000002; gram==2: the 1.000002; gram==2: the  0.2352941; gram==2: the 0.1768415;







