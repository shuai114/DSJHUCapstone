#setwd("C:/Users/Wei/Documents/DSJHU/en_US/finaldata")
setwd("C:/shuai personal stuff/My documents/jobs/en_US/finaldata")
#setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US")
sampcon<-file("evaluation.txt","a")
set.seed(2345)

con<-file("twit0.txt","r")
lines<-readLines(con, encoding="latin1")
close(con)
testindex<-sample.int(length(lines),40)
lines<-lines[testindex]
writeLines(lines,sampcon)

con<-file("news0.txt","r")
lines<-readLines(con, encoding="latin1")
close(con)
testindex<-sample.int(length(lines),40)
lines<-lines[testindex]
writeLines(lines,sampcon)

close(sampcon)

#truncation of samples randomly
#(need manually changed one line before truncation,
#because this line contains only one word)
con<-file("evaluation.txt","r")
lines<-readLines(con, encoding="latin1")
close(con)
test1<-lines[c(1:50,201:250)]
test2<-lines[c(51:100,251:300)]
twittest<-lines[c(101:200)]
newstest<-lines[c(301:400)]

set.seed(2345)
sampletext<-function(words){
splitted<-unlist(strsplit(words," "))
cutpoint<-sample.int(length(splitted)-1,1)
c(paste(splitted[1:(cutpoint+1)],collapse=" "))
}
test1<-sapply(test1,sampletext)
names(test1)<-c(rep("twits",50),rep("news",50))
test2<-sapply(test2,sampletext)
names(test2)<-c(rep("twits",50),rep("news",50))
twittest<-sapply(twittest,sampletext)
names(twittest)<-rep("twits",100)
newstest<-sapply(newstest,sampletext)
names(newstest)<-rep("news",100)

save(test1,test2,twittest,newstest,file="testset.RData")

