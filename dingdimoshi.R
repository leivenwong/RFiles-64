FANGXIANG <- 0
DINGDIBI <- 0
DIFENXING <- 0
DINGFENXING <- 0
ADJCOUNTER <- 0

library("xts")

DATETIME <- as.POSIXct(IF.5M$TIME,tz="")
HIGH <-as.xts(IF.5M$HIGH,DATETIME)
LOW <- as.xts(IF.5M$LOW,DATETIME)
REFHIGH1<-lag(HIGH,k = 1,na.pad=TRUE) 
REFHIGH2<-lag(HIGH,k = 2,na.pad=TRUE) 
REFLOW1<-lag(LOW,k = 1,na.pad=TRUE) 
REFLOW2<-lag(LOW,k = 2,na.pad=TRUE) 
CLOSE <- as.xts(IF.5M$CLOSE,DATETIME)
MACLOSE<-rollapply(CLOSE,3,mean)

HIGH <- as.numeric(HIGH)
LOW <- as.numeric(LOW)
REFHIGH1 <- as.numeric(REFHIGH1)
REFHIGH2 <- as.numeric(REFHIGH2)
REFLOW1 <- as.numeric(REFLOW1)
REFLOW2 <- as.numeric(REFLOW2)
CLOSE <- as.numeric(CLOSE)
MACLOSE <- as.numeric(MACLOSE)

FANGXIANG[1] <- 1
DINGDIBI[1] <- 0
FANGXIANG[2] <- 1
DINGDIBI[2] <- 0
DINGFENXING[1] <- 0
DINGFENXING[2] <- 0
DIFENXING[1] <- 0
DIFENXING[2] <- 0

i <- 3
bl <- length(DATETIME)
while(i<=bl)
{
  DINGFENXING[i] <- DINGFENXING[i-1]
  DIFENXING[i] <- DIFENXING[i-1]
  FANGXIANG[i] <- FANGXIANG[i-1]
  DINGDIBI[i] <- 1
  if(HIGH[i] > REFHIGH1[i] && REFHIGH1[i] > REFHIGH2[i] && LOW[i] > REFLOW1[i] && REFLOW1[i] > REFLOW2[i])
  {
    DINGDIBI[i] <- 1
    FANGXIANG[i] <- 1
  }
  
  if(HIGH[i] < REFHIGH1[i] && REFHIGH1[i] < REFHIGH2[i] && LOW[i] < REFLOW1[i] && REFLOW1[i] < REFLOW2[i])
  {
    DINGDIBI[i] <- 1
    FANGXIANG[i] <- 0-1
  }
  
  if(DINGFENXING[i-1] == 1 && LOW[i] < REFLOW1[i] && HIGH[i] < REFHIGH1[i])
  {
    DINGDIBI[i] <- 1
    FANGXIANG[i] <- 0-1
    DINGFENXING[i] <- 0
  }
  if(DIFENXING[i-1] == 1 && LOW[i] > REFLOW1[i] && HIGH[i] > REFHIGH1[i])
  {
    DINGDIBI[i] <- 1
    FANGXIANG[i] <- 1
    DIFENXING[i] <- 0
  }
  
  if(FANGXIANG[i] == 1 && HIGH[i] < REFHIGH1[i] && REFHIGH1[i] > REFHIGH2[i] && LOW[i] < REFLOW1[i] && REFLOW1[i] > REFLOW2[i])
  {
    DINGFENXING[i] <- 1
    DINGDIBI[i] <- 0
  }
  if(FANGXIANG[i] == 0-1 && HIGH[i] > REFHIGH1[i] && REFHIGH1[i] < REFHIGH2[i] && LOW[i] > REFLOW1[i] && REFLOW1[i] < REFLOW2[i])
  {
    DIFENXING[i] <- 1
    DINGDIBI[i] <- 0
  }
  
  if(FANGXIANG[i] == 1 && REFHIGH1[i] < REFHIGH2[i] && REFLOW1[i] > REFLOW2[i] && LOW[i] < REFLOW1[i] && HIGH[i] < REFHIGH2[i])
  {
    DINGFENXING[i] <- 1
    DINGDIBI[i] <- 0
  }
  if(FANGXIANG[i] == 1 && REFHIGH1[i] > REFHIGH2[i] && REFLOW1[i] < REFLOW2[i] && LOW[i] < REFLOW2[i] && HIGH[i] < REFHIGH1[i])
  {
    DINGFENXING[i] <- 1
    DINGDIBI[i] <- 0
  }
  if(FANGXIANG[i] == 0-1 && REFHIGH1[i] < REFHIGH2[i] && REFLOW1[i] > REFLOW2[i] && HIGH[i] > REFHIGH1[i] && LOW[i] > REFLOW2[i])
  {
    DIFENXING[i] <- 1
    DINGDIBI[i] <- 0
  }
  if(FANGXIANG[i] == 0-1 && REFHIGH1[i] > REFHIGH2[i] && REFLOW1[i] < REFLOW2[i] && HIGH[i] > REFHIGH2[i] && LOW[i] > REFLOW1[i])
  {
    DIFENXING[i] <- 1
    DINGDIBI[i] <- 0
  }  
    
  i <- i+1
}
IF.5M$FANGXIANG <- FANGXIANG
IF.5M$DINGDIBI <- DINGDIBI
