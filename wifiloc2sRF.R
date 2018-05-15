setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M3T3")
#=================== Loading libraries====================================
library(tidyverse) 
library(caret)  
library(corrplot) 
mydata0 <- read.csv(file="trainingData.csv", header=TRUE, sep=",")
mydata00 <- read.csv(file="validationData.csv", header=TRUE, sep=",")
mydata <- rbind(mydata0,mydata00)
mydata[mydata==100]<- -110
#=================== data wrangling====================================

set.seed(111)
mydata1 <- sample_n(mydata, 500 , replace = FALSE, .env = NULL)
mydata1$BUILDINGID <- as.factor(mydata1$BUILDINGID)
mydata1$FLOOR <- as.factor(mydata1$FLOOR)
mydata2 <- mydata1[, !apply(mydata1 == -110, 2, all)]

building <-  mydata2[ c(1:361,which( colnames(mydata2)=="BUILDINGID"))]
floor <- mydata2[ c(1:361,which( colnames(mydata2)=="FLOOR"))]
latitude <- mydata2[ c(1:255,which( colnames(mydata2)=="LATITUDE")) ]
longitude <- mydata2[ c(1:255,which( colnames(mydata2)=="LONGITUDE"))]
str(mydata)

#====================partition building============
set.seed(346)
indexes <- createDataPartition(y=building$BUILDINGID, times=1,p=0.7,list=FALSE)
trainSetBuild<- building[indexes,]
testSetBuild<- building[-indexes,]
str(trainSetBuild)

#====================svm BuildingID============
set.seed(2334)

svm_tune <- train(BUILDINGID~., data = trainSetBuild, method= "rf")
svm_tune

rfpred = predict(svm_tune, newdata=testSetBuild, trControl=trctrl,
                  preProcess = c("center", "scale"),tuneLength = 8)
rfpred
threshold <- 0.5
confusionMatrix(rfpred, testSetBuild$BUILDINGID, positive="TRUE")
saveRDS(rfpred, file = "rfpred")
my_model2 <- readRDS("rfpred")
#====================CORRcheck============