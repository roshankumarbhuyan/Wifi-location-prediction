ModelTest<- function(model){
  
  svm_tune <- readRDS(model)
  svmpred = predict(svm_tune, newdata=testSetlat, trControl=trctrl,
                    preProcess = c("center", "scale"),tuneLength = 8)
  
  threshold <- 0.5
  z<- postResample(svmpred, testSetlat$LATITUDE)
  return(z)
  
}



Z <- c("latb0f0","latb0f1","latb0f2","latb0f3","latb1f0","latb1f1","latb1f2",
       "latb1f3","latb2f0","latb2f1","latb2f2","latb2f3","latb2f4")
for (items in Z) {
  #train<- readRDS(items)
 assd<- ModelTest(items)
 #print(train)
  print(assd)
}
