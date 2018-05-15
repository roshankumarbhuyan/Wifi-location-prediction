setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M3T3")
#=================== Loading libraries====================================
library(tidyverse) 
library(caret)  
library(corrplot)
mydata0 <- read.csv(file="trainingData.csv", header=TRUE, sep=",")
mydata00 <- read.csv(file="validationData.csv", header=TRUE, sep=",")
mydata <- rbind(mydata0,mydata00)
mydata[mydata==100]<- -110
building <-  mydata[ c(1:520,which( colnames(mydata)=="BUILDINGID"))]

#====================partition building============
set.seed(346)
indexes <- createDataPartition(y=building$BUILDINGID, times=1,p=0.7,list=FALSE)
trainSetBuild<- building[indexes,]
testSetBuild<- building[-indexes,]
str(trainSetBuild)
#====================model building============
set.seed(2334)
svm_tune <- train(BUILDINGID ~., data = trainSetBuild, method= "svmLinear")
svm_tune
svmpred = predict(svm_tune, newdata=testSetBuild, trControl=trctrl,
                  preProcess = c("center", "scale"),tuneLength = 8)
svmpred
threshold <- 0.5
confusionMatrix(svmpred, testSetBuild$BUILDINGID, positive="TRUE")
saveRDS(svmpred, file = "SVMbuildingID")
my_model2 <- readRDS("SVMbuildingID")