setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M3T3")
#=================== Loading libraries====================================
library(tidyverse) 
library(caret)  
library(corrplot)
mydata0 <- read.csv(file="trainingData.csv", header=TRUE, sep=",")
mydata00 <- read.csv(file="validationData.csv", header=TRUE, sep=",")
mydata <- rbind(mydata0,mydata00)
mydata[mydata==100]<- -110
#=================== Sample data====================================
set.seed(111)
mydata1 <- sample_n(mydata, 5000 , replace = FALSE, .env = NULL)
mydata1$BUILDINGID <- as.factor(mydata1$BUILDINGID)
mydata1$FLOOR <- as.factor(mydata1$FLOOR)
mydata2 <- mydata1[, !apply(mydata1 == -110, 2, all)]



building <-  mydata2[ c(1:462,which( colnames(mydata2)=="BUILDINGID"))]
floor <- mydata2[ c(1:255,which( colnames(mydata2)=="FLOOR"))]
latitude <- mydata2[ c(1:255,which( colnames(mydata2)=="LATITUDE")) ]
longitude <- mydata2[ c(1:255,which( colnames(mydata2)=="LONGITUDE"))]
str(mydata)
#====================partition building============
set.seed(346)
indexes <- createDataPartition(y=building$BUILDINGID, times=1,p=0.7,list=FALSE)
trainSetBuild<- building[indexes,]
testSetBuild<- building[-indexes,]
str(trainSetBuild)
#====================partition Lon============
set.seed(345)
indexes <- createDataPartition(y=mydata2long$LONGITUDE, times=1,p=0.7,list=FALSE)
trainSetlon<- mydata[indexes,]
testSetlon <- mydata[-indexes,]
str(trainSetlon)
#====================partition Lat============
set.seed(340)
indexes <- createDataPartition(y=mydata2Lat$LATITUDE, times=1,p=0.7,list=FALSE)
trainSetlat<- mydata2Lat[indexes,]
testSetlat <- mydata2Lat[-indexes,]
str(trainSet)

#====================partition floor============
set.seed(346)
indexes <- createDataPartition(y=mydata2flr$FLOOR, times=1,p=0.7,list=FALSE)
trainSetflr<- mydata2flr[indexes,]
testSetflr <- mydata2flr[-indexes,]
str(trainSetflr)

#====================svm BuildingID============
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
#====================CORRcheck============

corr_check <- function(yourdataset, threshold){
  cor_matrix <- cor(yourdataset)
  cor_matrix
  
  for (i in 1:nrow(cor_matrix)){
    correlations <-  which((abs(cor_matrix[i,i:ncol(cor_matrix)]) > threshold) & (cor_matrix[i,i:ncol(cor_matrix)] != 1))
    
    if(length(correlations)> 0){
      lapply(correlations,FUN =  function(x) (cat(paste(colnames(yourdataset)[i], "with",colnames(yourdataset)[x]), "\n")))
    }
  }
}

print(corr_check(building, 0.85))
