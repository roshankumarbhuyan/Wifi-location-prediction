plot(mydata$LATITUDE,mydata$LONGITUDE)
mydata <- mydata %>% mutate(LATITUDE=LATITUDE- min(LATITUDE),LONGITUDE=LONGITUDE- min(LONGITUDE))
library("Directional")
library("dplyr")
ksi= c(0,0)
theta = 30

ksie <- rot.matrix(ksi, theta, rads = FALSE)
#rota <- function(a){
  a=12
mydata3 <- mydata1 %>% mutate(LATITUDE=LATITUDE*cos(a)-LONGITUDE*sin(a),
                             LONGITUDE=LATITUDE*sin(a)+ LONGITUDE*cos(a))
plot(mydata3$LATITUDE,mydata3$LONGITUDE)
#}
#rota(12)
