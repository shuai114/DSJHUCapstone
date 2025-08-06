setwd("C:\\Users\\Wei\\Documents\\DSJHU\\en_US")
con<-file("en_US.twitter.txt","r")

#the number of lines
linenum<-0
line<-readLines(con, 1)
while(length(line)){
    linenum<-linenum+1
    line<-readLines(con, 1)
}
close(con)
linenum

#the longest line length
maxlen<-0
line<-readLines(con, 1)
while(length(line)){
    linelen<-nchar(line)
    if(linelen>maxlen) maxlen<-linelen
    line<-readLines(con, 1)
}
close(con)
maxlen

#ratio of number of lines containing a specific word
lovelinenum<-0
hatelinenum<-0
line<-readLines(con, 1)
while(length(line)){
    if(grepl('love',line,fixed=T)) lovelinenum<-lovelinenum+1
    if(grepl('hate',line,fixed=T)) hatelinenum<-hatelinenum+1
    line<-readLines(con, 1)
}
close(con)
lovelinenum/hatelinenum

#get the tweet matching "biostats"
line<-readLines(con, 1)
while(length(line)){
    if(grepl('biostats',line,fixed=T)) print(line)
    line<-readLines(con, 1)
}
close(con)

#number of lines containing a specific sentence
linenum<-0
line<-readLines(con, 1)
while(length(line)){
    if(grepl('A computer once beat me at chess, but it was no match for me at kickboxing',line,fixed=T)) linenum<-linenum+1
    line<-readLines(con, 1)
}
close(con)
linenum

