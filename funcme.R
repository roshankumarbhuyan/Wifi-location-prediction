createmodel <- function(b,f){
  text = paste0("b",b,"f",f)
  newdf<-filter(mydata1,FLOOR==text ) 
  mydatalat <- newdf[ c(1:520,which(colnames(newdf)=="LATITUDE"))]
  set.seed(346)
  indexes <- createDataPartition(y=mydatalat$LATITUDE, times=1,p=0.7,list=FALSE)
  trainSetlat<- mydatalat[indexes,]
  testSetlat <- mydatalat[-indexes,]
  set.seed(2334)
  trctrl <- trainControl(method = "cv", number = 2,verbose = FALSE)
  start.time <- Sys.time()
  svm_tune <- train(LATITUDE~., data = trainSetlat, method= "rf", trControl=trctrl, preProcess = c("zv", "medianImpute"))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  y= paste(toString(time.taken),text)
  saveRDS(svm_tune, file = paste0("long",text) )
  z<-c(text,time.taken)
  return(z)
}

createmodel(0,0)
createmodel(0,1)
createmodel(0,2)
createmodel(0,3)
createmodel(1,0)
createmodel(1,1)
createmodel(1,2)
createmodel(1,3)
createmodel(2,1)
createmodel(2,2)
createmodel(2,3)
createmodel(2,4)


#List = list()
 
#for(building in 0:2){
    my_list <- list()  
     for(floor in 0:3){
        LM = createmodel(0,floor)
        my_list[[length(my_list)+1]] = LM
     }
 #    List <- append(List, list(my_list))
#}

