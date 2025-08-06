setwd("C:/Users/Wei/Documents/DSJHU/en_US")

#number of lines containing a specific pattern
numlines.for.pattern<-function(txtfile,pattern,notregexp=F){
con<-file(txtfile,"r")
linenum<-0
line<-readLines(con, 1, encoding="UTF-8")
while(length(line)){
    if(grepl(pattern,line,fixed=notregexp)) linenum<-linenum+1
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
linenum
}
numlines.for.pattern("en_US.twitter.txt","jade",T)

#print out the lines containing a specific pattern
lines.for.pattern<-function(txtfile,pattern,notregexp=F,limit=2360148){
con<-file(txtfile,"r")
#default limit is the maximum number of lines in the three txt files
linenum<-0
line<-readLines(con, 1, encoding="UTF-8")
while(length(line) && linenum<limit){
    if(grepl(pattern,line,fixed=notregexp)) {linenum<-linenum+1; print(line)}
    line<-readLines(con, 1, encoding="UTF-8")
}
close(con)
}
lines.for.pattern("en_US.twitter.txt","to the point",T,limit=10)
