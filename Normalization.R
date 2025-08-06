#start data cleaning for better generalization
begintime<-Sys.time()
begintime
load("wholeblogs.RData")
#split term into head and tail, normalize and stem the head, and combine them afterwards
splitted<-strsplit(as.character(blogs$term)," ")
rm(blogs)
tail<-sapply(splitted, function(words) words[length(words)])
head<-sapply(splitted, function(words) paste(words[1:(length(words)-1)],collapse=" "))
rm(splitted)
head<-normalize(head)
head<-sapply(head, function(words) paste(wordStem(unlist(strsplit(words," ")),language="english"),collapse=" "))
normterm<-paste(head,tail)
normterm<-gsub("^[[:space:]]+|[[:space:]]+$","",normterm)
normterm<-gsub("[[:space:]]+"," ",normterm)
rm(head,tail)
load("wholeblogs.RData")
normblogs<-blogs$freq.blogs
rm(blogs)
normblogs<-tapply(normblogs,normterm,sum)
save(normblogs,file="normblogs.RData")
Sys.time()-begintime

begintime<-Sys.time()
begintime
load("wholeblogs2.RData")
#split term into head and tail, normalize and stem the head, and combine them afterwards
splitted<-strsplit(as.character(blogs2$term)," ")
rm(blogs2)
tail<-sapply(splitted, function(words) words[length(words)])
head<-sapply(splitted, function(words) paste(words[1:(length(words)-1)],collapse=" "))
rm(splitted)
head<-normalize(head)
head<-sapply(head, function(words) paste(wordStem(unlist(strsplit(words," ")),language="english"),collapse=" "))
normterm<-paste(head,tail)
normterm<-gsub("^[[:space:]]+|[[:space:]]+$","",normterm)
normterm<-gsub("[[:space:]]+"," ",normterm)
rm(head,tail)
load("wholeblogs2.RData")
normblogs2<-blogs2$freq.blogs2
rm(blogs2)
normblogs2<-tapply(normblogs2,normterm,sum)
save(normblogs2,file="normblogs2.RData")
Sys.time()-begintime

