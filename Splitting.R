begintime<-Sys.time()
begintime
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata")
memory.limit(30000)
load("normblogs2.RData")
normname<-names(normblogs2)
part1.blogs2<-normblogs2[normname<"m"]
part2.blogs2<-normblogs2[normname>="m"]
rm(normblogs2)

normname<-names(part1.blogs2)
gram<-strsplit(normname," ")
gram<-sapply(gram,length)
gram2.part1.blogs2<-part1.blogs2[gram==2]
gram3.part1.blogs2<-part1.blogs2[gram==3]
gram4.part1.blogs2<-part1.blogs2[gram==4]
rm(part1.blogs2)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
save(gram2.part1.blogs2,file="gram2.part1.blogs2.RData")
save(gram3.part1.blogs2,file="gram3.part1.blogs2.RData")
save(gram4.part1.blogs2,file="gram4.part1.blogs2.RData")
rm(gram2.part1.blogs2,gram3.part1.blogs2,gram4.part1.blogs2)

normname<-names(part2.blogs2)
gram<-strsplit(normname," ")
gram<-sapply(gram,length)
gram2.part2.blogs2<-part2.blogs2[gram==2]
gram3.part2.blogs2<-part2.blogs2[gram==3]
gram4.part2.blogs2<-part2.blogs2[gram==4]
rm(part2.blogs2)
setwd("C:/Users/shuai/Documents/Shuai/jobs/en_US/finaldata/finaldata")
save(gram2.part2.blogs2,file="gram2.part2.blogs2.RData")
save(gram3.part2.blogs2,file="gram3.part2.blogs2.RData")
save(gram4.part2.blogs2,file="gram4.part2.blogs2.RData")
rm(gram2.part2.blogs2,gram3.part2.blogs2,gram4.part2.blogs2)
Sys.time()-begintime

