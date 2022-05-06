getwd()
setwd("C:\\Users\\JSW\\Desktop\\강의자료\\R프로그래밍\\R 실습 및 과제\\PR")
BC_cust<- read.csv("BGCON_CUST_DATA.csv")

BC_cust1 <- BC_cust[BC_cust$RESI_COST>100000,]

library(ggplot2)

p <- ggplot(data=BC_cust1,aes(x=AGE,y=RESI_COST,colour=CHLD))

p <- p + geom_point() 


#------------------------------
BC_cust2 <- BC_cust[BC_cust$SIU=="Y",]

p <- ggplot(BC_cust2,aes(AGE))
p <- p + geom_bar()
p <- p + facet_grid(~GENDER)
#_------------------------------
BC_cust3 <- BC_cust2[BC_cust2$PAYM_AMT > 4000000 & BC_cust2$PAYM_AMT < 14000000,]

BC_cust3
p <- ggplot(data=BC_cust3,aes(x=AGE,y=PAYM_AMT))
p <- p + geom_point(aes(colour=CTPR,size=CHLD))

p <- p+geom_smooth(method="loess") #라인 그래프 smooth 그리기
p
#-------------------------------

BC_cust4 <- BC_cust[BC_cust$FP_CA=="Y",]
BC_cust4
p <- ggplot(BC_cust4,aes(factor(CTPR)))
p + geom_bar(aes(fill=SIU))


#-------------------------------

BC_cust5 <- BC_cust[BC_cust$SIU=="N",] 
View(BC_cust5)

p <- ggplot(BC_cust5,aes(AGE))
p <- p + geom_histogram(aes(y=..density..,fill=..count..))
p <- p + geom_density(colour="black")
p <- ggplot()

#------------------------------
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

L.L <- read.csv("KOR_LAT_LON.csv")

L.L
