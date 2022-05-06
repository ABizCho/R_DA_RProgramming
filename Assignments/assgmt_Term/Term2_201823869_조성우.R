#스크래핑을위한 R셀레니엄 실행
install.packages("RSelenium")

library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)
library(stringr)

remDr <- remoteDriver(remoteServerAddr = "localhost" ,port = 4445L,   # port 번호 입력
                      browserName = "chrome")  

# browserName : 실행 브라우저 입력

remDr$open()

#----------

p_title <- NULL
p_date <- NULL
p_text <- NULL

#--------------------
urls<- vector(length=840)

for (i in 1:840){
  url <- "https://english1.president.go.kr/BriefingSpeeches/Speeches/"
  urls[i] <- paste0(url,i)
}

urls
#---------------
for (i in c(1:829)){
  remDr$navigate(urls[i])
  frontpage = remDr$getPageSource()
  
  #p_title - 제목가져오기
  #p_title에 제목을 직접넣으려할 시 1~829까의 페이지를 스크래핑 중 삭제된 페이지가 있을경우 xpath를 가져오지못하는 문제로 오류가발생하여 해당 반복문이 중단되기 때문에 조건문으로 a에 먼저 불러온 후 length가 0이 아닌경우에만 p_title에 넣도록 한다
  a <- read_html(frontpage[[1]]) %>% html_nodes(xpath = '//*[@id="print_area"]/div[1]/h2/text()') %>%  
    html_text()
  
  if (length(a) > 0 ){
    p_title[i] <- a} 
  
  #p_date -날짜 가져오기
  #p_title과 마찬가지로 조건문을 건다
  a <- read_html(frontpage[[1]]) %>% html_nodes(xpath = '//*[@id="print_area"]/div[1]/div/p') %>%  
    html_text()
  
  if (length(a) > 0 ){
    p_date[i] <- a}
  
  a <- read_html(frontpage[[1]]) %>% html_nodes(xpath = '//*[@id="print_area"]/div[2]') %>%  
    html_text()
  
  if (length(a) > 0 ){
    a = stringr::str_replace_all(a, '[\t\n\r]','')  #연설문 내용 내 특수문자 제거
    a = stringr::str_replace_all(a, '(Unofficial Translation)','')
    p_text[i] <- a
  }
  
  
  
}

p_title
p_date
p_text

write.csv(p_text,file="p_text.csv") #p_text csv로 저장 및 csv파일 내에서 간단한 전처리
p_text <- read.csv("p_text_R2.csv")

write.csv(p_date,file="p_d.csv")
write.csv(p_title,file="p_title.csv")


#날짜데이터 정리
install.packages("lubridate")
library(lubridate)

p_date <- mdy(p_date)
p_date_R<-p_date[!is.na(p_date)==T]
p_date_R<- p_date_R[1:397]




president_discourse <-data.frame(p_title,p_date,p_text) #제목,날짜,내용 을 담은 데이터프레임 생성

View(head(president_discourse))


president_discourse

#---------------

#2. 대통령담화 텍스트마이닝
president_discourse
p_text

getwd()
setwd("C:\\Users\\JSW\\Desktop\\텀과제\\문재인대통령 연설 감성분석")




# 텍스트마이닝을 위한 패키지 설치

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(tidyr)
library(stringr)
library(tibble)


tlist=0
tname=0

for (i in 1:397){
  tname[i] <- paste0('text',i) 
}

for (i in 1:397){
  tlist[i] <- list()
  
}



#
President_text_tm <-cbind(list)
President_text_tm
#연설문 내용 전처리

View(p_text[1,2])





#x에 기존텍스트 y에 텍스트번호 1~892 입력하여 전처리하는 함수 정의
text_minning <- function(x,y){p_text_tm <-Corpus(VectorSource(x[y,2]))  #Corpus란 tm패키지에서 문서를 관리하는 기본구조

p_text_tm <-tm_map(p_text_tm,stripWhitespace) #공백제거

p_text_tm <- tm_map(p_text_tm,tolower) #소문자로 변경 -> 사용하는 Lexicon의 단어들과 비교하기위함
p_text_tm <- tm_map(p_text_tm,removeNumbers) #숫자제거 
p_text_tm <- tm_map(p_text_tm,removeWords,stopwords("SMART")) #띄어쓰기,시제 제거

p_text_tm <-  tm_map(p_text_tm,removePunctuation) # 구두점제거

p_text_tm <- TermDocumentMatrix(p_text_tm) #term document matrix 생성
p_text_tm_m <- as.matrix(p_text_tm)

#term document matrix의 결과를 합해서 내림차순으로 정렬
p_text_tm_v <- sort(rowSums(p_text_tm_m),decreasing=TRUE) 
p_text_tm_d <- data.frame(word=names(p_text_tm_v),freq=p_text_tm_v) #이름부여
p_text_tm_d

return(p_text_tm_d)
}

p_text_tm_total <-NULL


#textminning 함수 반복문 적용으로 마이닝된 연설문 총모음집 만들기
p_text_tm_total <- NULL
p_text_tm_total <- text_minning(p_text)





# 임기간 연설문 총모음 분석 단어 워드클라우드 시각화

wordcloud(words=p_text_tm_total$word,
          freq = p_text_tm_total$freq,
          minfreq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))



#----------------------

p_text_tm_list <- NULL   #각 연설문 텍스트 빈도수(주요키워드) 데이터프레임을 총괄하는 dataframe


p_text_tm_list <- vector("list",length = 397)
for (i in 1:397){
  p_text_tm_list[[i]] <- text_minning(p_text,i)
}



#----------------------

#연설문 순서대로 텍스트리스트의 주요키워드와 그 빈도 상위 10개를 할당한 dataframe 만들기 

p_text_top10_list <- NULL


p_text_top10_list <- vector("list", length = length(p_text_tm_list))
for(i in seq_along(p_text_tm_list)){
  p_text_top10_list[[i]] <- head(p_text_tm_list[[i]], 10)
}
#----------------------------\\\
#3. 대통령연설 감성분석

install.packages("tidytext")
library(tidytext)

install.packages("textdata") #영어 감성사전 설치
library(textdata)


library(dplyr)
library(tidyr)


get_sentiments("bing")#bing 감성사전
get_sentiments("nrc") #nrc 감성사전  positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
get_sentiments("afinn")#affin 감성사전


#///---bing사전과 Melon 가사 단어와의 매치를 통한 긍/부정 할당

a<- NULL
text_sentiment_bing <- NULL



text_sentiment_bing

for (i in 1:397){
  if (p_text_tm_list[[i]] != NULL){
    text_sentiment_bing[[i]] <- p_text_tm_list[[i]] %>%
      inner_join(get_sentiments("bing"))
  }
  else{text_sentiment_bing[[i]] <- NULL}
  
  
  
}



test <- p_text_tm_list[[1]] %>%
  inner_join(get_sentiments("afinn"))



sum(test$freq*test$value)


str(text_sentiment_bing)

str(p_text_tm_list)

library(dplyr)
library(tidyr)
library(tm)


test <- p_text_tm_list[[1]] %>%
  inner_join(get_sentiments("afinn"))


boxplot(test$spirit)

#한 연설문당 감성값 결과 by affin
sum(test$freq*test$value)


a<-NULL
text_sentiment_score<-NULL
p_text_tm_list

for (i in 1:397){
  a <- inner_join(p_text_tm_list[[i]],get_sentiments("afinn"))
  text_sentiment_score[[i]]<-  sum(a$freq*a$value)
}

sentiment_score <- data.frame(text_sentiment_score,p_date_R)

names(sentiment_score) <- c("sentimentScore","date")

library(ggplot2)
ggplot(sentiment_score, aes(x=date,y=sentimentScore))+geom_line(color="black",size=1)
#--------------

#--------------감성값 결과 by nrc
for (i in 1:2){
  a <- inner_join(p_text_tm_list[[i]],get_sentiments("nrc"))
  a<- sum(a$word.freq=="positive") 
  
}
