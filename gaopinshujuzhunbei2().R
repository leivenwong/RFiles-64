gaopinshujuzhunbei2<-function(id="SH000001",cy="60M",JC=10,FZ1=2.3,FZ2=1.4,FZ3=0.5)
{
  DAIMA<-id
  ZHOUQI<-cy
  library("RODBC")
  library("xts")
  channel<-odbcConnectExcel(paste(DAIMA,ZHOUQI,"xls",sep="."))
  PINZHONG<-sqlFetch(channel,"Sheet2")
  odbcClose(channel)
  DATETIME<-as.POSIXct(PINZHONG$DATE,tz="")
  
  
  CLOSE<-as.xts(PINZHONG$CLOSE,DATETIME)
  REFCLOSE<-lag(CLOSE,k = 1,na.pad=TRUE)  
  ZF<-xts((as.numeric(CLOSE)-as.numeric(REFCLOSE))/as.numeric(CLOSE)*100,DATETIME)   
  MACLOSE<-rollapply(CLOSE,JC,mean)
  REFMACLOSE<-lag(MACLOSE,k = 1,na.pad=TRUE)  
  HIGH<-as.xts(PINZHONG$HIGH,DATETIME)
  MAHIGH<-rollapply(HIGH,JC,mean)
  
  
  MS<-NA
  ZF<-as.numeric(ZF)
  CLOSE<-as.numeric(CLOSE)
  MAHIGH<-as.numeric(MAHIGH)
  i<- (JC+1)
  bl<-length(DATETIME)
  while(i<=bl)
  {
    
    if(ZF[i]>=FZ1 
       & CLOSE[i]>=MAHIGH[i])  
      MS[i]<-"QS"
    else if(ZF[i]>=FZ2 
            & ZF[i]<FZ1  
            & CLOSE[i]>=MAHIGH[i])  
      MS[i]<-"QT"
    else if(ZF[i]<FZ1 
            & ZF[i]>FZ3 
            & CLOSE[i]>=MAHIGH[i])  
      MS[i]<-"QU"
    else if(ZF[i]<=FZ3 
            & ZF[i]>=0 
            & CLOSE[i]>=MAHIGH[i])
      MS[i]<-"QV"
    else if(ZF[i]<0 
            & ZF[i]>=-FZ3 
            & CLOSE[i]>=MAHIGH[i])  
      MS[i]<-"QW"
    else if(ZF[i]<0-FZ3 
            & ZF[i]>-FZ2 
            & CLOSE[i]>=MAHIGH[i])  
      MS[i]<-"QX"
    else if(ZF[i]<=0-FZ2 
            & ZF[i]>-FZ1 
            & CLOSE[i]>=MAHIGH[i]) 
      MS[i]<-"QY"
    else if(ZF[i]<=-FZ1 
            & CLOSE[i]>=MAHIGH[i]) 
      MS[i]<-"QZ" 
    
    
    else if(ZF[i]>=FZ1 
            & CLOSE[i]<MAHIGH[i])  
      MS[i]<-"RS"
    else if(ZF[i]>=FZ2 
            & ZF[i]<FZ1  
            & CLOSE[i]<MAHIGH[i])  
      MS[i]<-"RT"
    else if(ZF[i]<FZ2 
            & ZF[i]>FZ3 
            & CLOSE[i]<MAHIGH[i])  
      MS[i]<-"RU"
    else if(ZF[i]<=FZ3 
            & ZF[i]>=0 
            & CLOSE[i]<MAHIGH[i]) 
      MS[i]<-"RV"
    else if(ZF[i]<0 
            & ZF[i]>=-1 
            & CLOSE[i]<MAHIGH[i]) 
      MS[i]<-"RW"
    else if(ZF[i]<0-FZ3 
            & ZF[i]>-FZ2 
            & CLOSE[i]<MAHIGH[i]) 
      MS[i]<-"RX"
    else if(ZF[i]<=-FZ2 
            & ZF[i]>-FZ1 
            & CLOSE[i]<MAHIGH[i]) 
      MS[i]<-"RY"
    else if(ZF[i]<=-FZ1 
            & CLOSE[i]<MAHIGH[i])
      MS[i]<-"RZ" 
    else MS[i]<-NA;
    i<-i+1
  }
  
  
  NEXTMS<-0
  for(i in 1:(length(MS)-1))
    NEXTMS[i]<-MS[i+1]
  NEXTMS[length(MS)]<-"WAIT"
  
  
  
  
  
  PINZHONG$ZF<-as.numeric(ZF)  
  PINZHONG$MS<-MS
  PINZHONG$NEXTMS<-NEXTMS
  PINZHONG$DATETIME<-DATETIME
  PINZHONG$DATE<-as.character(DATETIME)
  PINZHONG$HIGH<-as.numeric(HIGH)
  PINZHONG$MAHIGH<-as.numeric(MAHIGH)
  
  PINZHONG<-na.omit(PINZHONG)
  
  write.csv(PINZHONG,paste(DAIMA,cy,"csv",sep="."),row.names = FALSE)
  return(as.data.frame(PINZHONG))
}
