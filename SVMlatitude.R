setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M3T3")
#=================== Loading libraries====================================
library(tidyverse) 
library(caret)  
library(corrplot)
library(dplyr)
mydata0 <- read.csv(file="trainingData.csv", header=TRUE, sep=",")
mydata00 <- read.csv(file="validationData.csv", header=TRUE, sep=",")
mydata <- rbind(mydata0,mydata00)
mydata[mydata==100]<- -100
mydata <- mydata %>% mutate(LATITUDE=LATITUDE- min(LATITUDE),LONGITUDE=LONGITUDE- min(LONGITUDE))
#=================== functions====================================
floorname <- function(df){
  
  b0 <- df %>% filter(BUILDINGID==0 ) 
  b0$FLOOR[b0$FLOOR == 0] <- 'b0f0'
  b0$FLOOR[b0$FLOOR == 1] <- 'b0f1'
  b0$FLOOR[b0$FLOOR == 2] <- 'b0f2'
  b0$FLOOR[b0$FLOOR == 3] <- 'b0f3'
  b0$FLOOR[b0$FLOOR == 4] <- 'b0f4'
  
  
  b1 <- df %>% filter(BUILDINGID==1 ) 
  b1$FLOOR[b1$FLOOR == 0] <- 'b1f0'
  b1$FLOOR[b1$FLOOR == 1] <- 'b1f1'
  b1$FLOOR[b1$FLOOR == 2] <- 'b1f2'
  b1$FLOOR[b1$FLOOR == 3] <- 'b1f3'
  b1$FLOOR[b1$FLOOR == 4] <- 'b1f4'
  
  b2 <- df %>% filter(BUILDINGID==2 ) 
  b2$FLOOR[b2$FLOOR == 0] <- 'b2f0'
  b2$FLOOR[b2$FLOOR == 1] <- 'b2f1'
  b2$FLOOR[b2$FLOOR == 2] <- 'b2f2'
  b2$FLOOR[b2$FLOOR == 3] <- 'b2f3'
  b2$FLOOR[b2$FLOOR == 4] <- 'b2f4'
  
  df2 <- rbind(b0,b1,b2)
  return(df2)
}
mydata1<- floorname(mydata)
wapcol <- function(df){
  indx <- grepl('WAP', colnames(df))
  df2<- df[indx]
  return(ncol(df2))
}
#=================== sampledata====================================
set.seed(111)
mydata2 <- sample_n(mydata1, 50 , replace = FALSE, .env = NULL)
mydata2 <- mydata2[, !apply(mydata2 == -104, 2, all)]
#mydata2$BUILDINGID <- as.factor(mydata2$BUILDINGID)
#mydata2$FLOOR <- as.factor(mydata2$FLOOR)
wapcol(mydata2)
mydatalat <- mydata1[ c(1:520,which( colnames(mydata1)=="LATITUDE"))]
#====================partition floor============
set.seed(346)
indexes <- createDataPartition(y=mydatalat$LATITUDE, times=1,p=0.7,list=FALSE)
trainSetlat<- mydatalat[indexes,]
testSetlat <- mydatalat[-indexes,]
str(trainSetlat)

#====================svm BuildingID============
set.seed(2334)

trctrl <- trainControl(method = "cv", number = 2,verbose = FALSE)
start.time <- Sys.time()
svm_tune <- train(LATITUDE~., data = trainSetlat, method= "svmLinear", trControl=trctrl, preProcess = c("zv", "medianImpute"))
svm_tune
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

svmpred = predict(svm_tune, newdata=testSetlat, trControl=trctrl,
                  preProcess = c("center", "scale"),tuneLength = 8)

threshold <- 0.5
postResample(svmpred, testSetlat$LATITUDE)
saveRDS(svmpred, file = "SVMlat")
my_model2 <- readRDS("SVMlat")



my_floors <- as.data.frame(table(mydata1$FLOOR))
ggplot() + geom_col(data = my_floors, aes(x = Var1, y = Freq))
