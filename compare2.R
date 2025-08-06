trymatch<-function(head,trmfreq,trmname){
regexpr<-paste0("(^",head,"| ",head,") [a-z]+$")
matchednames<-grep(regexpr,name1,value=T)
if (length(matchednames)>0) {
matchedword<-strsplit(matchednames," ")
matchedword<-sapply(matchedword, function(words) words[length(words)])
matchedTF<-gram1[matchednames]+1/(unigram[matchedword]+1)

matchedTF<-gram2[matchednames]
matchedTF<-matchedTF*dr[as.character(matchedTF)]/denom
#matchedTF<-matchedTF/sum(matchedTF)+1/(unigram[matchedword]+sum(matchedTF))
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
trymatch(head,gram1,name1)


gram==3:
      the      wine         a       sly    canned   crossed   bottles   waiting      beer       too        an       one      this 
17.000001  3.000549  3.000002  2.012821  2.004739  2.001855  2.001686  2.000371  2.000194  2.000054  2.000019  2.000017  2.000008 
gram==1:
          is              was             year             week             time              one             book          morning             blog             post 
15729.000004      3768.000007      3111.000063      3106.000078      2648.000023      2639.000017      1746.000073      1566.000171      1534.000104      1499.000115 
gram==3:
   picture       look      break        few      photo     moment        nap       step     shower     chance       long      stand         cl       walk       trip     little 
 70.000254  61.000080  55.000371  52.000064  34.000385  31.000227  28.002336  21.000421  18.001311  18.000341  16.000082  15.000392  15.000126  14.000290  13.000336  13.000044 
gram==2:
  matter     bill    issue     case 
5.000236 3.000628 2.000385 2.000188 


gram==2:
      to     with  outside 
4.000002 3.000007 2.000259 
gram==2:
       to      with   outside 
0.4444463 0.3333404 0.2224803 
gram==2:
        to       with    outside 
0.16666667 0.12500000 0.08333333 
gram==2:
        to       with    outside 
0.12571331 0.08390478 0.04246866 










