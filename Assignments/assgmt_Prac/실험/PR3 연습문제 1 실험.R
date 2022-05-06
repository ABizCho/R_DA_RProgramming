#PR3 실험

prices <- c(11905000.0, 11973000.0, 12190000.0, 12700000.0, 12303000.0, 12604000.0)

returns <- numeric(5)

for( i in 1:5){
  price_today <- prices[i+] ; price_yesterday <- prices[length(prices+1)-i] ; prices <- prices[i+1:length(prices)-i] ; prices
  returns[i] <- ((price_today - price_yesterday) / price_yesterday) * 100 ; returns
  
}

returns[1] <- ((price_today - price_yesterday) / price_yesterday) * 100 ; returns
returns[2] <- ((price_today - price))

returns[1] <- ((0302 - 0301) / 0301) * 100
returns[2] <- ((0303 - 0302) / 0302) * 100
returns[3] <- ((0304 - 0303) / 0303) * 100
returns[4] <- ((0305 - 0304) / 0304) * 100
returns[5] <- ((0306 - 0305) / 0305) * 100

returns

