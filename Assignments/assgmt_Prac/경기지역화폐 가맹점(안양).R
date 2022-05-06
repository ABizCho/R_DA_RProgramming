install.packages("dplyr")
install.packages("leaflet")
library(dplyr)
library(leaflet)
getwd()
setwd("c:\\Users\\JSW\\Desktop\\강의자료\\R프로그래밍\\source")
snam<-read.csv("snam.csv")
str(snam)
#b: 시설명 d: 교습과정
names(snam) <- paste0("c",1:10)
str(snam)

#지도에 표시

head(snam)
ss1 <-head(snam,500)

m <- leaflet() %>%
  addTiles() %>% #Add default OpenStreetMap map tiles
  addMarkers(lng = ss1$c9, lat = ss1$c8,popup =ss1$c2,)
m
