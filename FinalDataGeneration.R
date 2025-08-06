begintime<-Sys.time()
begintime
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
memory.limit(30000)
load("gram2.part1.twits.RData")
load("gram2.part1.twits2.RData")
load("gram2.part1.newss.RData")
load("gram2.part1.newss2.RData")
gram2.part1.twits.newss<-c(gram2.part1.twits,gram2.part1.twits2,gram2.part1.newss,gram2.part1.newss2)
rm(gram2.part1.twits,gram2.part1.twits2,gram2.part1.newss,gram2.part1.newss2)
normname<-names(gram2.part1.twits.newss)
gram2.part1.twits.newss<-tapply(gram2.part1.twits.newss,normname,sum)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
save(gram2.part1.twits.newss,file="gram2.part1.twits.newss.RData")
rm(gram2.part1.twits.newss)
Sys.time()-begintime

setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
load("gram2.part2.twits.RData")
load("gram2.part2.twits2.RData")
load("gram2.part2.newss.RData")
load("gram2.part2.newss2.RData")
gram2.part2.twits.newss<-c(gram2.part2.twits,gram2.part2.twits2,gram2.part2.newss,gram2.part2.newss2)
rm(gram2.part2.twits,gram2.part2.twits2,gram2.part2.newss,gram2.part2.newss2)
normname<-names(gram2.part2.twits.newss)
gram2.part2.twits.newss<-tapply(gram2.part2.twits.newss,normname,sum)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
save(gram2.part2.twits.newss,file="gram2.part2.twits.newss.RData")
rm(gram2.part2.twits.newss)
Sys.time()-begintime

setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
load("gram3.part1.twits.RData")
load("gram3.part1.twits2.RData")
load("gram3.part1.newss.RData")
load("gram3.part1.newss2.RData")
gram3.part1.twits.newss<-c(gram3.part1.twits,gram3.part1.twits2,gram3.part1.newss,gram3.part1.newss2)
rm(gram3.part1.twits,gram3.part1.twits2,gram3.part1.newss,gram3.part1.newss2)
normname<-names(gram3.part1.twits.newss)
gram3.part1.twits.newss<-tapply(gram3.part1.twits.newss,normname,sum)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
save(gram3.part1.twits.newss,file="gram3.part1.twits.newss.RData")
rm(gram3.part1.twits.newss)
Sys.time()-begintime

setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
load("gram3.part2.twits.RData")
load("gram3.part2.twits2.RData")
load("gram3.part2.newss.RData")
load("gram3.part2.newss2.RData")
gram3.part2.twits.newss<-c(gram3.part2.twits,gram3.part2.twits2,gram3.part2.newss,gram3.part2.newss2)
rm(gram3.part2.twits,gram3.part2.twits2,gram3.part2.newss,gram3.part2.newss2)
normname<-names(gram3.part2.twits.newss)
gram3.part2.twits.newss<-tapply(gram3.part2.twits.newss,normname,sum)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
save(gram3.part2.twits.newss,file="gram3.part2.twits.newss.RData")
rm(gram3.part2.twits.newss)
Sys.time()-begintime

setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
load("gram4.part1.twits.RData")
load("gram4.part1.twits2.RData")
load("gram4.part1.newss.RData")
load("gram4.part1.newss2.RData")
gram4.part1.twits.newss<-c(gram4.part1.twits,gram4.part1.twits2,gram4.part1.newss,gram4.part1.newss2)
rm(gram4.part1.twits,gram4.part1.twits2,gram4.part1.newss,gram4.part1.newss2)
normname<-names(gram4.part1.twits.newss)
gram4.part1.twits.newss<-tapply(gram4.part1.twits.newss,normname,sum)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
save(gram4.part1.twits.newss,file="gram4.part1.twits.newss.RData")
rm(gram4.part1.twits.newss)
Sys.time()-begintime

begintime<-Sys.time()
begintime
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
memory.limit(30000)
load("gram4.part2.twits.RData")
load("gram4.part2.twits2.RData")
load("gram4.part2.newss.RData")
load("gram4.part2.newss2.RData")
gram4.part2.twits.newss<-c(gram4.part2.twits,gram4.part2.twits2,gram4.part2.newss,gram4.part2.newss2)
rm(gram4.part2.twits,gram4.part2.twits2,gram4.part2.newss,gram4.part2.newss2)
normname<-names(gram4.part2.twits.newss)
gram4.part2.twits.newss<-tapply(gram4.part2.twits.newss,normname,sum)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
save(gram4.part2.twits.newss,file="gram4.part2.twits.newss.RData")
#rm(gram4.part2.twits.newss)
Sys.time()-begintime

begintime<-Sys.time()
begintime
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata/finaldata")
memory.limit(30000)
load("gram2.part1.twits.newss.RData")
load("gram2.part2.twits.newss.RData")
gram2<-c(gram2.part1.twits.newss,gram2.part2.twits.newss)
rm(gram2.part1.twits.newss,gram2.part2.twits.newss)

gram1<-gram2
terms<-strsplit(names(gram1)," ")
terms<-sapply(terms, function(words) words[2])
names(gram1)<-terms
gram1<-tapply(gram1,terms,sum)
rm(terms)
gram2<-gram2[gram2>1]

load("gram3.part1.twits.newss.RData")
load("gram3.part2.twits.newss.RData")
gram3<-c(gram3.part1.twits.newss,gram3.part2.twits.newss)
rm(gram3.part1.twits.newss,gram3.part2.twits.newss)
gram3<-gram3[gram3>1]

load("gram4.part1.twits.newss.RData")
load("gram4.part2.twits.newss.RData")
gram4<-c(gram4.part1.twits.newss,gram4.part2.twits.newss)
rm(gram4.part1.twits.newss,gram4.part2.twits.newss)
gram4<-gram4[gram4>1]
save(gram1,gram2,gram3,gram4,file="finaldata.RData")
Sys.time()-begintime

