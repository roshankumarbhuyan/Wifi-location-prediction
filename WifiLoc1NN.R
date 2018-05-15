setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M3T3")
#=================== Loading libraries====================================
library(tidyverse) 
library(caret)  
library(kimisc)
require(neuralnet)
library(ggplot2)
#====================Data import==========================================
mydata0 <- read.csv(file="trainingData.csv", header=TRUE, sep=",")
mydata00 <- read.csv(file="validationData.csv", header=TRUE, sep=",")
mydata <- rbind(mydata0,mydata00)
mydata[mydata==100]<- -104
mydata <- mydata %>% mutate(LATITUDE=LATITUDE- min(LATITUDE),LONGITUDE=LONGITUDE- min(LONGITUDE))
#====================functions==========================================
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
#====================functions==========================================
ggplot(mydata) + geom_point(aes(y=LONGITUDE, x= LATITUDE)) #+ facet_grid(BUILDINGID~FLOOR)
#====================Data clustering==========================================

#newdf <- mydata %>% filter(BUILDINGID==1)
#mydata1 <- newdf[, !apply(newdf == 100, 2, all)]
set.seed(100)
mydata1 <- sample_n(mydata, 100, replace = FALSE, weight = LONGITUDE, .env = NULL)
mydata2 <- mydata1[, !apply(mydata1 == -110, 2, all)]
wapcol(mydata2)
mydatalong <- mydata2[c(1:283),which( colnames(mydata2)=="FLOOR")]



#====================Neural network==========================================
set.seed(123)
maxs <- apply(mydata2, 2, max) 
mins <- apply(mydata2, 2, min)
fun
scaled <- as.data.frame(scale(mydata2, center = mins, scale = maxs - mins))

index <- createDataPartition(scaled$LONGITUDE, p = 0.7, list = FALSE)


trainset <- scaled[index,]
testset <- scaled[-index,]
X=trainset[,1:226]



#predict longitude==================
n <- names(X)
f <- as.formula(paste("LONGITUDE ~", paste(n[!n %in% "LONGITUDE"], collapse = " + ")))
nn <- neuralnet(f,data=trainset,hidden=c(100,5),linear.output=T)
plot(nn)
#predict latitude==================
n <- names(X)
f1 <- as.formula(paste("LATITUDE ~", paste(n[!n %in% "LATITUDE"], collapse = " + ")))
nn1 <- neuralnet(f1,data=trainset,hidden=c(100,5),linear.output=T)
plot(nn1)



nn_results_long <- compute(nn, testset[,1:226])
pred_long <- nn_results_long$net.result*(max(mydata2$LONGITUDE)-min(mydata2$LONGITUDE))+min(mydata2$LONGITUDE)
testdatalong <- (testset$LONGITUDE)*(max(mydata2$LONGITUDE)-min(mydata2$LONGITUDE))+min(mydata2$LONGITUDE)
MSE_nn_long <- sum((testdatalong - pred_long)^2)/nrow(testset)

nn_results_lat <- compute(nn, testset[,1:226])
pred_lat <- nn_results_lat$net.result*(max(mydata2$LATITUDE)-min(mydata2$LATITUDE))+min(mydata2$LATITUDE)
testdatalat <- (testset$LATITUDE)*(max(mydata2$LATITUDE)-min(mydata2$LATITUDE))+min(mydata2$LATITUDE)
MSE_nn_lat <- sum((testdatalat - pred_lat)^2)/nrow(testset) 

actual_lat =  testset$LATITUDE*(max(mydata2$LATITUDE)-min(mydata2$LATITUDE))+min(mydata2$LATITUDE)
actual_long = testset$LONGITUDE *(max(mydata2$LONGITUDE)-min(mydata2$LONGITUDE))+min(mydata2$LONGITUDE)
results <- data.frame(actual_lat, 
                      prediction_lat = pred_lat ,
                      actual_long, 
                      prediction_long = pred_long)

plot(results$actual_lat, results$actual_long, main='Actual Lat and Long',pch=18,cex=0.7)
plot(results$prediction_lat, results$prediction_lat, main='Predicted',pch=18,cex=0.7 )


ggplot(results) + geom_point(aes(x=actual_lat,y= actual_long )) 

#a [! a %in% remove]
#====================misc==========================================



rf_model<-train(Target~., data = df_tree_train, method = "ranger",
                trControl = trainControl(method = "oob"
                                         , verboseIter  = TRUE
                                         , allowParallel = TRUE
                                         , classProbs = TRUE
                )
                , verbose = T
                , tuneGrid = tuneGrid
                , num.trees = 50
                , num.threads = 7  # <- This one
)

#====================correlation matrix==========================================

corrData <- cor(newdf1) 
corrplot(corrData, method = "circle")

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

print(corr_check(mydata1, 0.85))
