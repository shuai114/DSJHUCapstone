#presort and prearrange the data
load("finaldata_thin.RData")
name1<-names(gram1)
name2<-names(gram2)
name3<-names(gram3)
name4<-names(gram4)

lastword<-function(name,len){
name<-strsplit(name," ")
last<-sapply(name, function(words) words[len])
}
lastword2<-lastword(name2,2)
lastword3<-lastword(name3,3)
lastword4<-lastword(name4,4)

matchwords<-function(name,len){
name<-strsplit(name," ")
head<-sapply(name, function(words) paste(words[1:(len-1)],collapse=" "))
}
matchwords2<-matchwords(name2,2)
matchwords3<-matchwords(name3,3)
matchwords4<-matchwords(name4,4)

gram2<-data.frame(gram=gram2,name=name2,matchwords=matchwords2,lastword=lastword2,stringsAsFactors=F)
gram3<-data.frame(gram=gram3,name=name3,matchwords=matchwords3,lastword=lastword3,stringsAsFactors=F)
gram4<-data.frame(gram=gram4,name=name4,matchwords=matchwords4,lastword=lastword4,stringsAsFactors=F)

presort<-function(trmfreq){
matchedword<-trmfreq[,"lastword"]
trmfreq$matchedTF<-trmfreq[,"gram"]+1/(gram1[matchedword]+1)
trmfreq<-trmfreq[order(trmfreq$matchedTF,decreasing=T),]
trmfreq
}
gram2<-presort(gram2)
gram3<-presort(gram3)
gram4<-presort(gram4)

len<-dim(gram3)[1]
sum(gram3$matchwords<"at")/len
sum(gram3$matchwords>="at" & gram3$matchwords<"che")/len
sum(gram3$matchwords>="che" & gram3$matchwords<"foo")/len
sum(gram3$matchwords>="foo" & gram3$matchwords<"i")/len
sum(gram3$matchwords>="i" & gram3$matchwords<"ma")/len
sum(gram3$matchwords>="ma" & gram3$matchwords<"of")/len
sum(gram3$matchwords>="of" & gram3$matchwords<"re")/len
sum(gram3$matchwords>="re" & gram3$matchwords<"ta")/len
sum(gram3$matchwords>="ta" & gram3$matchwords<"ti")/len
sum(gram3$matchwords>="ti" & gram3$matchwords<"w")/len
sum(gram3$matchwords>="w")/len

gram2$gram<-NULL
gram2$name<-NULL
gram2$matchedTF<-NULL
gram3$gram<-NULL
gram3$name<-NULL
gram3$matchedTF<-NULL
gram4$gram<-NULL
gram4$name<-NULL
gram4$matchedTF<-NULL

##repeat this section for gram2, gram3 and gram4
gram41<-gram4[gram4$matchwords<"at",]
gram42<-gram4[gram4$matchwords>="at" & gram4$matchwords<"che",]
gram43<-gram4[gram4$matchwords>="che" & gram4$matchwords<"foo",]
gram44<-gram4[gram4$matchwords>="foo" & gram4$matchwords<"i",]
gram45<-gram4[gram4$matchwords>="i" & gram4$matchwords<"ma",]
gram46<-gram4[gram4$matchwords>="ma" & gram4$matchwords<"of",]
gram47<-gram4[gram4$matchwords>="of" & gram4$matchwords<"re",]
gram48<-gram4[gram4$matchwords>="re" & gram4$matchwords<"ta",]
gram49<-gram4[gram4$matchwords>="ta" & gram4$matchwords<"ti",]
gram40<-gram4[gram4$matchwords>="ti" & gram4$matchwords<"w",]
gram4a<-gram4[gram4$matchwords>="w",]
alldim<-dim(gram41)+dim(gram42)+dim(gram43)+dim(gram44)+dim(gram45)+dim(gram46)+dim(gram47)+dim(gram48)+dim(gram49)+dim(gram40)+dim(gram4a)
alldim[1]==dim(gram4)[1]

save(gram21,gram22,gram23,gram24,gram25,gram26,gram27,gram28,gram29,gram20,gram2a,
gram31,gram32,gram33,gram34,gram35,gram36,gram37,gram38,gram39,gram30,gram3a,
gram41,gram42,gram43,gram44,gram45,gram46,gram47,gram48,gram49,gram40,gram4a,file="finalterms_thin.RData")
setwd("C:/shuai personal stuff/My documents/jobs/en_US/finaldata/finalapp")
load("finalterms_thin.RData")

shrink<-function(gram){
uniquename<-unique(gram$matchwords)
cat("Number of Records:",dim(gram)[1],"\n")
cat("Number of Unique terms:",length(uniquename),"\n")
shrinkdata<-NULL
for (term in uniquename) {
matchedindex<-grep(term,gram$matchwords,fixed=T)
temp<-gram[matchedindex,]
temp<-temp[1:min(3,dim(temp)[1]),]
shrinkdata<-rbind(shrinkdata,temp)
}
shrinkdata
}
gram21<-shrink(gram21)
gram22<-shrink(gram22)
gram23<-shrink(gram23)
gram24<-shrink(gram24)
gram25<-shrink(gram25)
gram26<-shrink(gram26)
gram27<-shrink(gram27)
gram28<-shrink(gram28)
gram29<-shrink(gram29)
gram20<-shrink(gram20)
gram2a<-shrink(gram2a)
gram31<-shrink(gram31)
gram32<-shrink(gram32)
gram33<-shrink(gram33)
gram34<-shrink(gram34)
gram35<-shrink(gram35)
gram36<-shrink(gram36)
gram37<-shrink(gram37)
gram38<-shrink(gram38)
gram39<-shrink(gram39)
gram30<-shrink(gram30)
gram3a<-shrink(gram3a)
gram41<-shrink(gram41)
gram42<-shrink(gram42)
gram43<-shrink(gram43)
gram44<-shrink(gram44)
gram45<-shrink(gram45)
gram46<-shrink(gram46)
gram47<-shrink(gram47)
gram48<-shrink(gram48)
gram49<-shrink(gram49)
gram40<-shrink(gram40)
gram4a<-shrink(gram4a)

lastword21<-gram21$lastword
lastword22<-gram22$lastword
lastword23<-gram23$lastword
lastword24<-gram24$lastword
lastword25<-gram25$lastword
lastword26<-gram26$lastword
lastword27<-gram27$lastword
lastword28<-gram28$lastword
lastword29<-gram29$lastword
lastword20<-gram20$lastword
lastword2a<-gram2a$lastword
lastword31<-gram31$lastword
lastword32<-gram32$lastword
lastword33<-gram33$lastword
lastword34<-gram34$lastword
lastword35<-gram35$lastword
lastword36<-gram36$lastword
lastword37<-gram37$lastword
lastword38<-gram38$lastword
lastword39<-gram39$lastword
lastword30<-gram30$lastword
lastword3a<-gram3a$lastword
lastword41<-gram41$lastword
lastword42<-gram42$lastword
lastword43<-gram43$lastword
lastword44<-gram44$lastword
lastword45<-gram45$lastword
lastword46<-gram46$lastword
lastword47<-gram47$lastword
lastword48<-gram48$lastword
lastword49<-gram49$lastword
lastword40<-gram40$lastword
lastword4a<-gram4a$lastword

name21<-gram21$matchwords
name22<-gram22$matchwords
name23<-gram23$matchwords
name24<-gram24$matchwords
name25<-gram25$matchwords
name26<-gram26$matchwords
name27<-gram27$matchwords
name28<-gram28$matchwords
name29<-gram29$matchwords
name20<-gram20$matchwords
name2a<-gram2a$matchwords
name31<-gram31$matchwords
name32<-gram32$matchwords
name33<-gram33$matchwords
name34<-gram34$matchwords
name35<-gram35$matchwords
name36<-gram36$matchwords
name37<-gram37$matchwords
name38<-gram38$matchwords
name39<-gram39$matchwords
name30<-gram30$matchwords
name3a<-gram3a$matchwords
name41<-gram41$matchwords
name42<-gram42$matchwords
name43<-gram43$matchwords
name44<-gram44$matchwords
name45<-gram45$matchwords
name46<-gram46$matchwords
name47<-gram47$matchwords
name48<-gram48$matchwords
name49<-gram49$matchwords
name40<-gram40$matchwords
name4a<-gram4a$matchwords

save(name21,name22,name23,name24,name25,name26,name27,name28,name29,name20,name2a,
name31,name32,name33,name34,name35,name36,name37,name38,name39,name30,name3a,
name41,name42,name43,name44,name45,name46,name47,name48,name49,name40,name4a,
lastword21,lastword22,lastword23,lastword24,lastword25,lastword26,lastword27,lastword28,lastword29,lastword20,lastword2a,
lastword31,lastword32,lastword33,lastword34,lastword35,lastword36,lastword37,lastword38,lastword39,lastword30,lastword3a,
lastword41,lastword42,lastword43,lastword44,lastword45,lastword46,lastword47,lastword48,lastword49,lastword40,lastword4a,
file="finalterms_thin2.RData")



