shujuzhunbei3<-function(id="SH000001",cy="1D",JC=10,FZ1=5,FZ2=3,FZ3=1)
{
  library("RODBC")
  library("xts")
  channel<-odbcConnect("ctp_merged_mq",uid="ctp_user",pwd="ctp_password")
  IF.1D<<-sqlQuery(channel,"select * from if_1d order by utc_string")
  odbcClose(channel)
  
  IFTIME <- IF.1D$utc_string
  IFOPEN <- IF.1D$open_price
  IFHIGH <- IF.1D$high_price
  IFLOW <- IF.1D$low_price
  IFCLOSE <- IF.1D$close_price
  IFVOL <- IF.1D$volumn
  
  PINZHONG <- list()
  
  PINZHONG$TIME<-IFTIME
  PINZHONG$OPEN<-IFOPEN
  PINZHONG$HIGH<-IFHIGH
  PINZHONG$LOW<-IFLOW
  PINZHONG$CLOSE<-IFCLOSE
  PINZHONG$VOL<-IFVOL
  
  PINZHONG <- as.data.frame(PINZHONG)
  
  IF.1D <<- PINZHONG
  
  
  DATETIME<-as.Date(IFTIME)
  
  
  CLOSE<-as.xts(PINZHONG$CLOSE,DATETIME)
  OPEN<-as.xts(PINZHONG$OPEN,DATETIME)  
  REFCLOSE<-lag(CLOSE,k = 1,na.pad=TRUE)  
  ZF<-xts((as.numeric(CLOSE)-as.numeric(REFCLOSE))/as.numeric(CLOSE)*100,DATETIME)  
  NEXTCLOSE<-lag(CLOSE,k = -1,na.pad=TRUE)  
  MACLOSE<-rollapply(CLOSE,JC,mean)
  VOL<-as.xts(PINZHONG$VOL,DATETIME)
  
  REFMACLOSE<-lag(MACLOSE,k = 1,na.pad=TRUE)  
  
  
  
  MS<-NA
  ZF<-as.numeric(ZF)
  MACLOSE<-as.numeric(MACLOSE)
  REFMACLOSE<-as.numeric(REFMACLOSE)
  
  
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
  
  IF.1D.3 <<- as.data.frame(PINZHONG)
  return(as.data.frame(PINZHONG))
}
