# 한 csv를 전처리하여 데이터프레임으로 만들기

library(dplyr)

item7 <- read.csv(header=F,"item7.csv",stringsAsFactors = F)

item7 <- unlist(item7)
str(item7)
item7

item7 <- strsplit(item7,split="\t")
item7 <- unlist(item7)

item7 <- matrix(item7,ncol=3,,byrow=T)
item7 <- data.frame(item7,stringsAsFactors = F)

item7

item7 <- sapply(item7,function(x){gsub("찬성","1",x)},simplify = F) 
item7 <- sapply(item7,function(x){gsub("반대","-1",x)},simplify = F)
item7 <- sapply(item7,function(x){gsub("기권","0",x)},simplify = F)

gsub("찬성","1",item7)

item7 <- data.frame(item7,stringsAsFactors = F)
item7

item7$X3 <- as.numeric(item7$X3)

item7
str(item7)

colnames(item7) <- c("name","party","vote")
item7



#-------------------------여기까지


