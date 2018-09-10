shujuzhunbei23<-function(id="SH000001",cy="1D",JC=10,FZ1=5,FZ2=3,FZ3=1)
{
  DAIMA<-id
  ZHOUQI<-cy
  library("RODBC")
  library("xts")
  channel<-odbcConnectExcel(paste(DAIMA,ZHOUQI,"xls",sep="."))
  PINZHONG<-sqlFetch(channel,"Sheet2")
  odbcClose(channel)
  DATETIME<-as.Date(PINZHONG$DATE)
  
  
  CLOSE<-as.xts(PINZHONG$CLOSE,DATETIME)
  OPEN<-as.xts(PINZHONG$OPEN,DATETIME)  
  REFCLOSE<-lag(CLOSE,k = 1,na.pad=TRUE)  
  ZF<-xts((as.numeric(CLOSE)-as.numeric(OPEN))/as.numeric(OPEN)*100,DATETIME)  
  NEXTCLOSE<-lag(CLOSE,k = -1,na.pad=TRUE)  
  MACLOSE<-rollapply(CLOSE,JC,mean)
  VOL<-as.xts(PINZHONG$VOL,DATETIME)
  
  REFMACLOSE<-lag(MACLOSE,k = 1,na.pad=TRUE)  
  REFMAVOL<-lag(MAVOL,k = 1,na.pad=TRUE)
  
  
  MS<-NA
  ZF<-as.numeric(ZF)
  MACLOSE<-as.numeric(MACLOSE)
  REFMACLOSE<-as.numeric(REFMACLOSE)
  MAVOL<-as.numeric(MAVOL)
  REFMAVOL<-as.numeric(REFMAVOL)
  i<- (JC+1)
  bl<-length(DATETIME)
  while(i<=bl)
  {
    
    
    if(ZF[i]>= FZ1 
       & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QS"
    else if(ZF[i]>= FZ2 
            & ZF[i]< FZ1  
            & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QT"
    else if(ZF[i]<FZ2 
            & ZF[i]>FZ3 
            & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QU"
    else if(ZF[i]<=FZ3 
            & ZF[i]>=0 
            & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QV"
    else if(ZF[i]<0 
            & ZF[i]>= (0-FZ3) 
            & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QW"
    else if(ZF[i]< (0-FZ3) 
            & ZF[i]> (0-FZ2) 
            & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QX"
    else if(ZF[i]<= (0-FZ2) 
            & ZF[i]> (0-FZ1) 
            & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QY"
    else if(ZF[i]<= (0-FZ1) 
            & MACLOSE[i]>=REFMACLOSE[i])  
      
      MS[i]<-"QZ" 
    
    else if(ZF[i]>=FZ1 
            & MACLOSE[i]<REFMACLOSE[i])  
      
      
      
      MS[i]<-"RS"
    else if(ZF[i]>=FZ2 
            & ZF[i]<FZ1  
            & MACLOSE[i]<REFMACLOSE[i])  
      
      MS[i]<-"RT"
    else if(ZF[i]<FZ2 
            & ZF[i]>FZ3 
            & MACLOSE[i]<REFMACLOSE[i])  
      
      MS[i]<-"RU"
    else if(ZF[i]<=FZ3 
            & ZF[i]>=0 
            & MACLOSE[i]<REFMACLOSE[i])  
      
      MS[i]<-"RV"
    else if(ZF[i]<0 
            & ZF[i]>= (0-FZ3) 
            & MACLOSE[i]<REFMACLOSE[i])  
      
      MS[i]<-"RW"
    else if(ZF[i]< (0-FZ3) 
            & ZF[i]> (0-FZ2) 
            & MACLOSE[i]<REFMACLOSE[i])  
      
      MS[i]<-"RX"
    else if(ZF[i]<= (0-FZ2) 
            & ZF[i]> (0-FZ1) 
            & MACLOSE[i]<REFMACLOSE[i])  
      
      MS[i]<-"RY"
    else if(ZF[i]<= (0-FZ1) 
            & MACLOSE[i]<REFMACLOSE[i])  
      
      MS[i]<-"RZ" 
    else MS[i]<-NA;
    i<-i+1
  }
  
  
  NEXTMS<-0
  for(i in 1:(length(MS)-1))
    NEXTMS[i]<-MS[i+1]
  NEXTMS[length(MS)]<-"WAIT"  
  
  
  PINZHONG$REFCLOSE<-as.numeric(REFCLOSE)
  PINZHONG$ZF<-as.numeric(ZF)   
  PINZHONG$MACLOSE<-as.numeric(MACLOSE)
  
  PINZHONG$REFMACLOSE<-as.numeric(REFMACLOSE)
  
  PINZHONG$MS<-MS
  PINZHONG$NEXTMS<-NEXTMS
  PINZHONG$DATETIME<-DATETIME
  PINZHONG$DATE<-as.character(DATETIME)
  
  PINZHONG<-na.omit(PINZHONG)
  
  write.csv(PINZHONG,paste(DAIMA,cy,"csv",sep="."),row.names = FALSE)
  return(as.data.frame(PINZHONG))
}
