library(leaflet)
library(dplyr)

setwd("C:\\Users\\JSW\\Desktop\\강의자료\\R프로그래밍\\R 실습 및 과제\\PR")
BC_cust<- read.csv("BGCON_CUST_DATA.csv")
head(BC_cust)
BC_cust1 <- select(BC_cust,SIU,CTPR) 
BC_cust1

BC_cust2<- select(BC_cust,SIU,CTPR)
BC_cust2
BC_cust3 <- select
A<- subset(BC_cust,SIU=='Y')
B<- subset(BC_cust,SIU=="N")

str(A)
str(B)
#각 시도별 보험사기고객 비율    "강원" "경기" "경남" "경북" "광주" "대구" "대전" "부산" "서울" "세종" "울산" "인천" "전남" "전북" "제주" "충남" "충북"

#경기 
BC_cust1 <- subset(BC_cust1,SIU=='Y')

ratiofunc <- function(a,b) {#a 데이터셋 b는 지역명
  ratio1 = NULL
  ratio1.1 = NULL
  proportion = 0.1
  ratio1 <- select(subset(a,CTPR==b),SIU)#지역에서 입력지역의 SIU만 추출한 데이터프레임으로 재구성
  
  c <- (length(ratio1$SIU) / length(a$SIU)) * 100
  return(c)

}


Region <- c("강원","경기","경남","경북","광주","대구","대전","부산","서울","세종","울산","인천","전남","전북","제주","충남","충북")

ratioGroup <- 0
for (i in 1:length(Region)){
  ratioGroup[i] <- ratiofunc(BC_cust1,Region[i])
} 

ratioDf <- data.frame(Region,ratioGroup)
ratioDf

L.L <- read.csv("KOR_LAT_LON.csv")
L.L
colnames(L.L) <- c('Region','LAT','LON')
ratioDf

Lat <- c(37.88630,37.27461,35.23829,35.89250,35.16008,35.87145,36.35042,35.18376,37.54100,36.59248,35.53874,37.45622,34.81621,35.82100,33.48901,36.32676,36.63558)
Lon <- c(127.7301,127.0095,128.6924,128.6006,126.8515,128.6014,127.3845,129.0755,126.9860,127.2921,129.3114,126.7052,126.4629,127.1114,126.4983,127.4208,127.4914)


ratioDf<- data.frame(ratioDf,Lat,Lon)
ratioDf




#--

colorB

pal3<-colorBin(palette="YlOrRd", domain=c(0,100), bins = 8, pretty=F, alpha = T)


FF
m=0
m = leaflet() %>% addTiles() %>%
setView(126.9860,37.54100,zoom=7) #서울의 경도와 위도 설정

m = m %>% addLegend(pal = pal3,
                    values  = FF$ratioGroup,
                    position = "bottomright",
                    title = "ratio",
                    labFormat = labelFormat(suffix="%",between="%&ndash;"))
m %>% addCircleMarkers(data=FF,radius=5,color=~pal(class),)  #circleMarkers 사용




# 0  - 12
# 12 - 25
# 25 - 38
# 38 - 50
# 50 - 62
# 62 - 75
# 75 - 88
# 88 - 100

#------------------------------------------------



#-----------------------------------------------


str(BC_cust$CTPR)

l1<- subset(BC_cust,CTPR=="강원")
l2<-subset(BC_cust,CTPR=="경기")
l3<-subset(BC_cust,CTPR=="경남")
l4<-subset(BC_cust,CTPR=="경북")
l5<-subset(BC_cust,CTPR=="광주")
l6<-subset(BC_cust,CTPR=="대구")
l7<-subset(BC_cust,CTPR=="대전")
l8<-subset(BC_cust,CTPR=="부산")
l9<-subset(BC_cust,CTPR=="서울")
l10<-subset(BC_cust,CTPR=="세종")
l11<-subset(BC_cust,CTPR=="울산")
l12<-subset(BC_cust,CTPR=="인천")
l13<-subset(BC_cust,CTPR=="전남")
l14<-subset(BC_cust,CTPR=="전북")
l15<-subset(BC_cust,CTPR=="제주")
l16<-subset(BC_cust,CTPR=="충남")
l17<-subset(BC_cust,CTPR=="충북")

processD <- function(a,b){ #a는 처리하고자하는 데이터셋, b는 CTPR 즉 해당 지역에 한해 처리
  a<- a$FP_
}


