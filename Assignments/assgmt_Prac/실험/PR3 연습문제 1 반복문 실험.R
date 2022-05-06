prices <- c(11905000.0, 11973000.0, 12190000.0, 12700000.0, 12303000.0, 12604000.0)

price_today <- prices[1] # 벡터의 첫번째값을 2일 종가에 저장
price_yesterday <- prices[length(prices)] #벡터의 마지막값을 1일 종가에 저장


for (i in 1:10) {
  returns[1] = ((price_today - price_yesterday) / price_yesterday) * 100 #2일의 수익률
  
  prices <- prices[i+1:lengh(prices)] # prices의 첫번째 항이 사라진다
  
  price_yesterday<- price_today # 해당회차때 종료된 계산에서 금일의 종가였던것을 다음계산을 위해 전일 종가로 만든다
  price_today <- prices[1] #다음계산을 위해  금일의 종가에 prices의 새로운 첫번째항을 넣는다 
  
  returns[2] = ((price_today - price_yesterday) / price_yesterday) * 100 ; price_yesterday
}



#------------------------------------------------------
returns[1] <- ((prices[1]-prices[6])/prices[6])*100
returns[2] <- ((prices[2]-prices[1])/prices[1])*100
returns[3] <- ((prices[3]-prices[2])/prices[2])*100
returns[4] <- ((prices[4]-prices[3])/prices[3])*100
returns[5] <- ((prices[5]-prices[4])/prices[4])*100

returns



#----------------------------------------------------
  
a = 6
l = 1

for(i in 1:2){
  returns[i] <- ((prices[a-(i-1)] - prices[l+(i-1)]) / prices[l+(i-1)]) * 100      # returns[]
  returns[i+1] <- ((prices[l+i] - prices[a])/ prices[a]) * 100     # 해당 절의 코드가 첫번째실행됐을떄 returns[2] 인데 ,반복문이 두번째 실행될때 첫번째 실행코드가 returns[2] 이기 때문에 코드진행의 시간흐름상 잘못 덮어씌워지는것을 해결해야함 
  if (i==2){
    returns[5] <- ((prices[i+2]-prices[i+1])/prices[i+1])*100
    
    print(returns)
  }  
}
