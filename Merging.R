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

