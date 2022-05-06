getwd()
setwd("C:\\Users\\JSW\\Desktop\\강의자료\\R프로그래밍\\R 실습 및 과제\\HW\\HW안내및소스\\HW1\\1-2")

install.packages("tidyverse")
install.packages("tidyr")


virtualData <- read.csv(header=F,"virtualData.csv",stringsAsFactors = F)

str(virtualData)
virtualData

#새로 데이터프레임을 만들지 않고 받아온 데이터를 그대로 사용하려고 노력했습니다.
#이 과정에서 문제1과 문제2의 해결 순서가 바뀌게 됐습니다.
#감안하고 봐주시면 감사하곘습니다.

#문제 2 : 전처리
virtualData <- sapply(virtualData,function(x){gsub("찬성","1",x)}) 
virtualData <- sapply(virtualData,function(x){gsub("반대","-1",x)})
virtualData <- sapply(virtualData,function(x){gsub("기권","0",x)})
virtualData

virtualData0 <- as.numeric(virtualData)
virtualData0
virtualData0[is.na(virtualData0)] <-virtualData0[is.na(virtualData0)] <- 0 #NA값을 0으로 바꿔준다
virtualData0

virtualData
member <- virtualData[1:10]
party <- virtualData[11:20]
item1 <- virtualData0[21:30]
item2 <- virtualData0[31:40]
item3 <- virtualData0[41:50]
item4 <- virtualData0[51:60]
item5 <- virtualData0[61:70]
item6 <- virtualData0[71:80]
item7 <- virtualData0[81:90]
item8 <- virtualData0[91:100]

workedVD<- data.frame(member,party,item1,item2,item3,item4,item5,item6,item7,item8,stringsAsFactors = F) # 전처리가 완료된 데이터프레임 생성
workedVD

#문제 1
workedVD <- workedVD[,2:10] 
workedVD
str(workedVD)


#문제 3------------------------------------------------------

#계산의 용이성을 위해 데이터프레임 workedVD에서 행렬계산의 대상이 될 데이터만 subset하겠습니다.
c <- c(workedVD$item1,workedVD$item2,workedVD$item3,workedVD$item4,workedVD$item5,workedVD$item6,workedVD$item7,workedVD$item8)
c
MatVD <- matrix(c,10,8,byrow=F) #구상한 계산을 위해 매트릭스를 열기준으로 만들겠습니다.
MatVD



#--------반복문으로 행렬곱 구현
result <- matrix(NA,10,10) #반복문을 통한 내적 계산의 결과를 할당하기위한 matrix를 선언( 그 결과는 100개가 될것으로 10 X 10)

#a로 행렬곱셈의 대상 행을 고정한채로 곱해줄 i에 해당하는 행을 모두 곱해주는 계산을 위해 a와 i로 두단계로 이루어진 반복문을 만들겠습니다.

for (a in 1:10){      
  for (i in 1:10){
    result[i,a] <- sum(MatVD[a,]*MatVD[i,])}}
result

innerVD <- data.frame(result) #결과를 다시 데이터프레임으로 변환합니다. 
innerVD

colnames(innerVD) <-c("김우동","박수박","이족발","홍라면","박보쌈","김피자","장감바스","백된장","이초밥","송돈까스") #결과에 해당하는  이름
row.names(innerVD) <-c("김우동","박수박","이족발","홍라면","박보쌈","김피자","장감바스","백된장","이초밥","송돈까스") #결과에 해당하는 이름

innerVD

str(innerVD)

#--------------------------------------------

내적을 계산한 결과 

   * 표결간 Most positive한 관계는  김우동-홍라면(6) / 김우동-이초밥(6) 
          Most negative한 관계는  김우동-이족발(-6) / 김우동-박보쌈(-6) / 이족발-이초밥(-6)  으로 보인다

   * 서로간 가장 무관한 표결을 보인 관계는 이족발-장감바스(0)/김피자-이초밥(0) 이다.
          
   * 
          