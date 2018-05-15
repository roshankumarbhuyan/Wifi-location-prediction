setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M3T3")
#=================== Loading libraries====================================

library(tidyverse) 
library(caret)  
library(corrplot)
library(ggplot2)
mydata <- read.csv(file="trainingData.csv", header=TRUE, sep=",")
mydata[mydata==100]<- -110

high_signal <- function(wifi){
  wifiname <- wifi
  #wifiname <- deparse(substitute(wifi))
  Long <- mydata[which(mydata[wifiname] == max(mydata[wifiname]))[1], which(colnames(mydata)=="LONGITUDE")]
  Lat <- mydata[which(mydata[wifiname] == max(mydata[wifiname]))[1], which(colnames(mydata)=="LATITUDE")]
  build <- mydata[which(mydata[wifiname] == max(mydata[wifiname]))[1], which(colnames(mydata)=="BUILDINGID")]
  floor<- mydata[which(mydata[wifiname] == max(mydata[wifiname]))[1], which(colnames(mydata)=="FLOOR")]
  cewcl <- c(wifiname,Long,Lat,build,floor)
  
  #print(paste(deparse(substitute(wifi)), Long, Lat,floor))
  # df <- mydata %>% filter(mydata$LATTITUDE== Lat, mydata$LONGITUDE== Long)
  
  #mydata1 <- df %>% mutate(Wifiname = deparse(substitute(wifi)))
  #mydata1 <- mydata1%>% 
  #   select(Wifiname,LATITUDE,LONGITUDE,FLOOR,BUILDING)/
  return(cewcl)
}
z<- c("Wifiname","LATITUDE","LONGITUDE","FLOOR","BUILDING")
X= names(mydata[,1:520])
for (names in X){
  z<-cbind(z,high_signal(names))
  #print(high_signal(names))
}
z<-t(z)
z1<- as.data.frame(z)
names(z1) <- z[1, ]
z2<-z1[2:521,]
z2$FLOOR <- as.factor(z2$FLOOR)
z2$BUILDING <- as.factor(z2$BUILDING)

ggplot(z2,aes(x=LATITUDE, y=LONGITUDE )) + #geom_point(aes( shape = BUILDING,  )) + #+ facet_grid(BUILDINGID~FLOOR)
                                      geom_text(aes(label = Wifiname,colour=FLOOR))
