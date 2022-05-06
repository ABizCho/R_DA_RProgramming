prices <- c(11905000.0, 11973000.0, 12190000.0, 12700000.0, 12303000.0, 12604000.0) # prices에 주어진 6개 값이 0301~3006 까지의 비트코인종가(순서는 무작위)

#전제1. 주어진 벡터로 수익률은 1~6일간 5개의 값을 구할 수 있다.
#전제2. Hint2가 말하는 첫번째 값을 삭제하고 price_today에 할당하라는 말은, 벡터의 첫항을 삭제하지 않고 수익률계산을 반복하려할 때 price_today <- prices[1:5]를 계산마다 차례로 할당한다고 생각한다.
#전제3. 수익률계산의 두번째 계산에서 사용하는 전날의 종가(prices_yesterday)는 직전계산에서 사용했던 금일의 종가(prices_today)와 같을 것이므로 Hint3는 Hint2와 달리 첫번째 계산(반복문에 해당되지않는)에만 마지막 항을 할당하기로하고 그후 계산식부터는 직전계산의 금일종가를 해당계산의 전날종가에 할당하는 반복문을 사용하겠다.


returns = numeric(5) #리턴즈 변수 5칸할당 선언

price_today = prices[1] # Hint2를 따라 prices의 첫번째 값을 price_today에 할당
price_yesterday = prices[6] #Hint3를 따라 prices의 마지막 값을 price_yesterday에 할당

returns[1] = ((price_today - price_yesterday) / price_yesterday) * 100 #returns의 첫번째 항에  1~2일간의 수익률(첫번째 수익률) 할당

for (i in 2:5){                                                  #이후의 수익률계산은 i가 2~5까지의 반복문사용
  price_yesterday = price_today                                  #해당회차 계산의 전날종가 <- 직전회차 계산의 금일종가
  price_today = prices[i]                                        #Hint2 를 반복적용
  
  returns[i] = ((price_today - price_yesterday)/price_yesterday) * 100 # 2~5까지 수익률 계산 후 returns에 차례로 할당
}
returns
names(returns) <-c("2020-03-02","2020-03-03","2020-03-04","2020-03-05","2020-03-06") ; returns
