#setwd("C:/Users/Wei/Documents/DSJHU/JHUDSCapstone")
library(tm)
library(RTextTools)
load("finalterms_thin2.RData")

preprocess<-function(line){
    line<-gsub(']','}',line) #change [] to {} to facilitate strange character removing
    line<-gsub('\\[','{',line)
    line<-gsub('[^-A-Za-z0-9 ,.!?%&()+=_:;\'\"~`@#$^*<>\\/|{}]',' ',line) #remove strange characters (cannot specify [] in this expression)
    line<-tolower(line)
    line<-gsub('[^ ]*<u\\+([0-9a-f]){4,4}>[^ ]*','',line) #remove unrecognized UTF-8 codes
    line<-gsub('-+ | -+',' ',line) #remove heading or tailing -
    #expand the contracted forms (choose is between is/has, and would between had/would)
    line<-gsub("`","'",line)
    line<-gsub("i'm","i am",line)
    line<-gsub("i'd","i would",line) #had/would
    line<-gsub("he's","he is",line) #is/has
    line<-gsub("he'd","he would",line) #had/would
    line<-gsub("she's","she is",line) #is/has
    line<-gsub("she'd","she would",line) #had/would
    line<-gsub("it's","it is",line) #is/has
    line<-gsub("it'd","it would",line) #had/would
    line<-gsub("you're","you are",line)
    line<-gsub("you'd","you would",line) #had/would
    line<-gsub("we're","we are",line)
    line<-gsub("we'd","we would",line) #had/would
    line<-gsub("they're","they are",line)
    line<-gsub("they'd","they would",line) #had/would
    line<-gsub("'ve"," have",line)
    line<-gsub("'ll"," will",line)
    line<-gsub("can't|cannot","can not",line)
    line<-gsub("shan't","shall not",line)
    line<-gsub("won't","will not",line)
    line<-gsub("n't"," not",line)
    line<-gsub("let's","let us",line)
    #line<-gsub('#',' number ',line) #so '#1' can be recoded as 'number 1' (keep it for twits)
    #separate the punctuations (codes removed)
    #profanity words removal
    line<-gsub('ass|asshole|damn|goddamn|damnit|damned|fuck|fucked|fucking|f[*]+ck|f[*]+cked|f[*]+cking',' ',line)
}
normalize<-function(line){
    line<-gsub("^brought | brought | brought$|^brought$"," bring ",line)
    line<-gsub("^began | began | began$|^began$|^begun | begun | begun$|^begun$"," begin ",line)
    line<-gsub("^broke | broke | broke$|^broke$|^broken | broken | broken$|^broken$"," break ",line)
    line<-gsub("^built | built | built$|^built$"," build ",line)
    line<-gsub("^caught | caught | caught$|^caught$"," catch ",line)
    line<-gsub("^came | came | came$|^came$"," come ",line)
    line<-gsub("^got | got | got$|^got$"," get ",line)
    line<-gsub("^goes | goes | goes$|^goes$|^went | went | went$|^went$|^gone | gone | gone$|^gone$"," go ",line)
    line<-gsub("^kept | kept | kept$|^kept$"," keep ",line)
    line<-gsub("^made | made | made$|^made$"," make ",line)
    line<-gsub("^ran | ran | ran$|^ran$"," run ",line)
    line<-gsub("^saw | saw | saw$|^saw$|^seen | seen | seen$|^seen$"," see ",line)
    line<-gsub("^sent | sent | sent$|^sent$"," send ",line)
    line<-gsub("^sat | sat | sat$|^sat$"," sit ",line)
    line<-gsub("^took | took | took$|^took$|^taken | taken | taken$|^taken$"," take ",line)
    line<-gsub("^am | am | am$|^am$|^is | is | is$|^is$|^are | are | are$|^are$|^was | was | was$|^was$|^were | were | were$|^were$|^been | been | been$|^been$"," be ",line)
    line<-gsub("^clung | clung | clung$|^clung$"," cling ",line)
    line<-gsub("^heard | heard | heard$|^heard$"," hear ",line)
    line<-gsub("^might | might | might$|^might$"," may ",line)
    line<-gsub("^lay | lay | lay$|^lay$|^lain | lain | lain$|^lain$|^laid | laid | laid$|^laid$"," lie ",line)
    line<-gsub("^fell | fell | fell$|^fell$|^fallen | fallen | fallen$|^fallen$"," fall ",line)
    line<-gsub("^has | has | has$|^has$|^had | had | had$|^had$"," have ",line)
    line<-gsub("^does | does | does$|^does$|^did | did | did$|^did$|^done | done | done$|^done$"," do ",line)
    line<-gsub("^gave | gave | gave$|^gave$|^given | given | given$|^given$"," give ",line)
    line<-gsub("^your | your | your$|^your$|^his | his | his$|^his$|^her | her | her$|^her$|^its | its | its$|^its$|^our | our | our$|^our$|^their | their | their$|^their$"," my ",line)
    #line<-gsub(" the$| a$| an$","",line)
    line<-gsub("^yourself | yourself | yourself$|^yourself$|^himself | himself | himself$|^himself$|^herself | herself | herself$|^herself$|^itself | itself | itself$|^itself$
               |^ourselves | ourselves | ourselves$|^ourselves$|^yourselves | yourselves | yourselves$|^yourselves$|^themselves | themselves | themselves$|^themselves$"," myself ",line)
}
onlinepreproc<-function(string){
    string<-preprocess(string)
    string<-removePunctuation(string)
    string<-removeNumbers(string)
    string<-gsub("^[[:space:]]+|[[:space:]]+$","",string)
    string<-gsub("[[:space:]]+"," ",string)
}
trymatch<-function(head,trmname,lasttrm){
    regexpr<-paste0("^",head,"$")
    matchedindex<-grep(regexpr,trmname)
    lasttrm[matchedindex]
}
match<-function(string){
    #annotate the following line when doing evaluation
    string<-onlinepreproc(string)
    string<-tail(unlist(strsplit(string," ")),3)
    string<-paste(string,collapse=" ")
    string<-normalize(string)
    string<-paste(wordStem(unlist(strsplit(string," ")),language="english"),collapse=" ")
    string<-gsub("^[[:space:]]+|[[:space:]]+$","",string)
    string<-gsub("[[:space:]]+"," ",string)
    if (string=="") {pred<-c("I","Hey","The")} else {
    string<-tail(unlist(strsplit(string," ")),3)
    #start from 4-gram
    pred<-character()
    for(gram in 3:1){
        head<-paste(tail(string,gram),collapse=" ")
        if (gram==3 & head<"at") matchednames<-trymatch(head,name41,lastword41)
        else if (gram==3 & head>="at" & head<"che") matchednames<-trymatch(head,name42,lastword42)
        else if (gram==3 & head>="che" & head<"foo") matchednames<-trymatch(head,name43,lastword43)
        else if (gram==3 & head>="foo" & head<"i") matchednames<-trymatch(head,name44,lastword44)
        else if (gram==3 & head>="i" & head<"ma") matchednames<-trymatch(head,name45,lastword45)
        else if (gram==3 & head>="ma" & head<"of") matchednames<-trymatch(head,name46,lastword46)
        else if (gram==3 & head>="of" & head<"re") matchednames<-trymatch(head,name47,lastword47)
        else if (gram==3 & head>="re" & head<"ta") matchednames<-trymatch(head,name48,lastword48)
        else if (gram==3 & head>="ta" & head<"ti") matchednames<-trymatch(head,name49,lastword49)
        else if (gram==3 & head>="ti" & head<"w") matchednames<-trymatch(head,name40,lastword40)
        else if (gram==3 & head>="w") matchednames<-trymatch(head,name4a,lastword4a)
        else if (gram==2 & head<"at") matchednames<-trymatch(head,name31,lastword31)
        else if (gram==2 & head>="at" & head<"che") matchednames<-trymatch(head,name32,lastword32)
        else if (gram==2 & head>="che" & head<"foo") matchednames<-trymatch(head,name33,lastword33)
        else if (gram==2 & head>="foo" & head<"i") matchednames<-trymatch(head,name34,lastword34)
        else if (gram==2 & head>="i" & head<"ma") matchednames<-trymatch(head,name35,lastword35)
        else if (gram==2 & head>="ma" & head<"of") matchednames<-trymatch(head,name36,lastword36)
        else if (gram==2 & head>="of" & head<"re") matchednames<-trymatch(head,name37,lastword37)
        else if (gram==2 & head>="re" & head<"ta") matchednames<-trymatch(head,name38,lastword38)
        else if (gram==2 & head>="ta" & head<"ti") matchednames<-trymatch(head,name39,lastword39)
        else if (gram==2 & head>="ti" & head<"w") matchednames<-trymatch(head,name30,lastword30)
        else if (gram==2 & head>="w") matchednames<-trymatch(head,name3a,lastword3a)
        else if (gram==1 & head<"at") matchednames<-trymatch(head,name21,lastword21)
        else if (gram==1 & head>="at" & head<"che") matchednames<-trymatch(head,name22,lastword22)
        else if (gram==1 & head>="che" & head<"foo") matchednames<-trymatch(head,name23,lastword23)
        else if (gram==1 & head>="foo" & head<"i") matchednames<-trymatch(head,name24,lastword24)
        else if (gram==1 & head>="i" & head<"ma") matchednames<-trymatch(head,name25,lastword25)
        else if (gram==1 & head>="ma" & head<"of") matchednames<-trymatch(head,name26,lastword26)
        else if (gram==1 & head>="of" & head<"re") matchednames<-trymatch(head,name27,lastword27)
        else if (gram==1 & head>="re" & head<"ta") matchednames<-trymatch(head,name28,lastword28)
        else if (gram==1 & head>="ta" & head<"ti") matchednames<-trymatch(head,name29,lastword29)
        else if (gram==1 & head>="ti" & head<"w") matchednames<-trymatch(head,name20,lastword20)
        else if (gram==1 & head>="w") matchednames<-trymatch(head,name2a,lastword2a)
        matchednames<-setdiff(matchednames,pred)
        pred<-c(pred,matchednames)
        if (length(pred)>=3) break;
    }
    if (length(pred)<3) {
        matchednames<-setdiff(c("the","to","and"),pred)
        pred<-c(pred,matchednames)
    }
    }
    pred[1:3]
}

shinyServer(
    function(input,output){
        output$phrase <- renderText({input$phrase})
        pred<-reactive({match(input$phrase)})
        output$word1 <- renderText({pred()[1]})
        output$word2 <- renderText({pred()[2]})
        output$word3 <- renderText({pred()[3]})
    }
)