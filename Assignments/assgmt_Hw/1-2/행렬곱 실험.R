a <-"1	-1	1	-1	-1	1	1	1
1	1	1	1	0	1	1	1
1	1	-1	1	1	-1	-1	-1
1	1	1	-1	-1	1	1	1
-1	1	-1	1	-1	-1	-1	-1
-1	-1	-1	1	-1	-1	1	-1
0	-1	0	0	0	0	0	0
-1	1	1	1	1	-1	-1	-1
0	-1	1	-1	-1	1	0	1
1	0	1	-1	-1	1	1	-1"

str(a)

b <- gsub("\t"," ",a)
b <- gsub("\n"," ",b)

library(tidyr)
b <- strsplit(b,split=" ")

b <- unlist(b)
c <- as.numeric(b)
str(c)

d <- matrix(c,10,8,byrow=T)
d



#--------반복문으로 행렬곱 구현
result <- matrix(,10,10)
for (a in 1:10){
  for (i in 1:10){
    result[i,a] <- sum(d[a,]*d[i,])}}
d
result

#-----------------------


