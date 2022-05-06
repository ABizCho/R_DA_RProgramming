#2. 대통령담화 텍스트마이닝
president_discourse
p_text

getwd()
setwd("C:\\Users\\JSW\\Desktop\\텀과제\\문재인대통령 연설 감성분석")




# 텍스트마이닝을 위한 패키지 설치
install.packages("tm") #텍스트마이닝 패키지
install.packages("SnowballC") #어근추출 패키지
install.packages("wordcloud") #워드클라우드
install.packages("RColorBrewer")

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


#감성분석


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



Melon_sentiment_bing <- d %>%
  inner_join(get_sentiments("bing"))

Melon_sentiment_bing
#----------////



#////---nrc사전의 joy 감정 카테고리 단어와 연설문 단어와의 매치를 통한 joy단어판별

nrcjoy <- get_sentiments('nrc') %>%   #nrc 감성사전에서 joy카테고리 의 단어만 가져오기
  filter(sentiment == "joy")

sentiment_nrc_joy <- d %>% #Melon단어를 nrc의 joy카테고리와 매칭
  inner_join(nrcjoy)


#---
#positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
sentiment_nrc_1 <- d %>% inner_join(get_sentiments('nrc')) %>% #Melon단어를 nrc의 joy카테고리와 매치
  filter(sentiment=="joy")

#
sentiment_nrc_2 <- d %>% inner_join(get_sentiments('nrc')) %>% #nrc의 joy카테고리와 매치
  filter(sentiment=="anger")

sentiment_nrc_3 <- d %>% inner_join(get_sentiments('nrc')) %>% #nrc의 joy카테고리와 매치
  filter(sentiment=="anticipation")

sentiment_nrc_4 <- d %>% inner_join(get_sentiments('nrc')) %>% #nrc의 joy카테고리와 매치
  filter(sentiment=="disgust")

sentiment_nrc_5 <- d %>% inner_join(get_sentiments('nrc')) %>% #nrc의 joy카테고리와 매치
  filter(sentiment=="fear")

sentiment_nrc_6 <- d %>% inner_join(get_sentiments('nrc')) %>% #nrc의 joy카테고리와 매치
  filter(sentiment=="Sadness")

sentiment_nrc_7 <- d %>% inner_join(get_sentiments('nrc')) %>% #nrc의 joy카테고리와 매치
  filter(sentiment=="surprise")

sentiment_nrc_8 <- d %>% inner_join(get_sentiments('nrc')) %>% #nrc의 joy카테고리와 매치
  filter(sentiment=="trust")




sentiment_nrc <- rbind(sentiment_nrc_1,sentiment_nrc_2,sentiment_nrc_3,sentiment_nrc_4,sentiment_nrc_5,sentiment_nrc_6,sentiment_nrc_7,sentiment_nrc_8)


Melon_sentiment_nrc 

sentiment_nrc_totalfreq <-NULL


sentiment_nrc_totalfreq <- sum(freq[sentiment_nrc$sentiment=="joy"]) #joy의 총빈도
sentiment_nrc_totalfreq<-cbind(sentiment_nrc_totalfreq,sum(freq[sentiment_nrc$sentiment=="anger"])) #joy의 총빈도
sentiment_nrc_totalfreq<-cbind(sentiment_nrc_totalfreq,sum(freq[sentiment_nrc$sentiment=="anticipation"])) #joy의 총빈도
sentiment_nrc_totalfreq<-cbind(sentiment_nrc_totalfreq,sum(freq[sentiment_nrc$sentiment=="disgust"])) #joy의 총빈도
sentiment_nrc_totalfreq<-cbind(sentiment_nrc_totalfreq,sum(freq[sentiment_nrc$sentiment=="fear"])) #joy의 총빈도
sentiment_nrc_totalfreq<-cbind(sentiment_nrc_totalfreq,sum(freq[sentiment_nrc$sentiment=="sadness"])) #joy의 총빈도
sentiment_nrc_totalfreq<-cbind(sentiment_nrc_totalfreq,sum(freq[sentiment_nrc$sentiment=="surprise"])) #joy의 총빈도
sentiment_nrc_totalfreq<-cbind(sentiment_nrc_totalfreq,sum(freq[sentiment_nrc$sentiment=="trust"])) #joy의 총빈도



sentiment_nrc_totalfreq

#-------------------------///


#////-----Melon 가사 감성분석결과 시각화

#geom_point 빈도
install.packages("gglot2")
library(ggplot2)

g <-ggplot(data=sentiment_nrc,aes(x=sentiment,y=freq))
g + geom_point()




str(Melon_sentiment_bing)

sentiment_nrc_total$sentiment <- as.factor(Melon_sentiment_nrc_total$sentiment)


