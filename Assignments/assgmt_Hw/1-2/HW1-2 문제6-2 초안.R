a <- merge(item1,item2,by='name',)
a <- merge(a,item3,by='name')
a <- merge(a,item4,by='name')
a <- merge(a,item5,by='name')
a <- merge(a,item6,by='name')
a <- merge(a,item7,by='name')
a <- merge(a,item8,by='name')
a <- merge(a,item9,by='name')
a <- merge(a,item10,by='name')

b <- a[,c(1,2,3,5,7,9,11,13,15,17,19,21)]
colnames(b) <- c('name','party','vote1','vote2','vote3','vote4','vote5','vote6','vote7','vote8','vote9','vote10')
b

b[is.na(b)] <-b[is.na(b)] <- 0 #na값을 0으로 만들어줍니다
b

c <- b[,-c(1:2)] #계산을 위해 name, party 잠시 배제

str(c)

length(c)

result <- matrix(NA,239,239)
result

for (a in 1:239){      
  for (i in 1:239){
    result[i,a] <- sum(c[a,]*c[i,])}}

result



b[is.na(b)] <-b[is.na(b)] <- 0 #na값을 0으로 만들어줍니다
b

c <- b[,-c(1:2)] #계산을 위해 name, party 잠시 배제

str(c)

length(c)

result <- matrix(NA,239,239)
result


for (a in 1:239){      
  for (i in 1:239){
    result[i,a] <- sum(c[a,]*c[i,])}}

str(result)

df.result<- data.frame(result,stringsAsFactors = F)

str(df.result)
