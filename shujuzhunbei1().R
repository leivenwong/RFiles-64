shujuzhunbei1<-function(id="SH000001",cy="1D",JC=10)
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
  
  PINZHONG <- list()
  
  PINZHONG$TIME<-IFTIME
  PINZHONG$OPEN<-IFOPEN
  PINZHONG$HIGH<-IFHIGH
  PINZHONG$LOW<-IFLOW
  PINZHONG$CLOSE<-IFCLOSE
  
  PINZHONG <- as.data.frame(PINZHONG)
  IF.1D <<- PINZHONG
  
  
  DATETIME<-as.Date(PINZHONG$TIME)
  
  
  CLOSE<-as.xts(PINZHONG$CLOSE,DATETIME)
  REFCLOSE<-lag(CLOSE,k = 1,na.pad=TRUE)  
  ZF<-xts((as.numeric(CLOSE)-as.numeric(REFCLOSE))/as.numeric(CLOSE)*100,DATETIME)   
  MACLOSE<-rollapply(CLOSE,JC,mean)
  REFMACLOSE<-lag(MACLOSE,k = 1,na.pad=TRUE)  
  LOW<-as.xts(PINZHONG$LOW,DATETIME)
  MALOW<-rollapply(LOW,JC,mean)
  
  
  MS<-NA
  ZF<-as.numeric(ZF)
  CLOSE<-as.numeric(CLOSE)
  MALOW<-as.numeric(MALOW)

  i<- (JC+1)
  bl<-length(DATETIME)
  while(i<=bl)
  {
    if(ZF[i]>=5 
            & CLOSE[i]>=MALOW[i])  
      MS[i]<-"QS"
    else if(ZF[i]>=3 
            & ZF[i]<5  
            & CLOSE[i]>=MALOW[i])  
      MS[i]<-"QT"
    else if(ZF[i]<3 
            & ZF[i]>1 
            & CLOSE[i]>=MALOW[i])  
      MS[i]<-"QU"
    else if(ZF[i]<=1 
            & ZF[i]>=0 
            & CLOSE[i]>=MALOW[i])
      MS[i]<-"QV"
    else if(ZF[i]<0 
            & ZF[i]>=-1 
            & CLOSE[i]>=MALOW[i])  
      MS[i]<-"QW"
    else if(ZF[i]<0-1 
            & ZF[i]>-3 
            & CLOSE[i]>=MALOW[i])  
      MS[i]<-"QX"
    else if(ZF[i]<=0-3 
            & ZF[i]>-5 
            & CLOSE[i]>=MALOW[i]) 
      MS[i]<-"QY"
    else if(ZF[i]<=-5 
            & CLOSE[i]>=MALOW[i]) 
      MS[i]<-"QZ" 
    
        
    else if(ZF[i]>=5 
            & CLOSE[i]<MALOW[i])  
      MS[i]<-"RS"
    else if(ZF[i]>=3 
            & ZF[i]<5  
            & CLOSE[i]<MALOW[i])  
      MS[i]<-"RT"
    else if(ZF[i]<3 
            & ZF[i]>1 
            & CLOSE[i]<MALOW[i])  
      MS[i]<-"RU"
    else if(ZF[i]<=1 
            & ZF[i]>=0 
            & CLOSE[i]<MALOW[i]) 
      MS[i]<-"RV"
    else if(ZF[i]<0 
            & ZF[i]>=-1 
            & CLOSE[i]<MALOW[i]) 
      MS[i]<-"RW"
    else if(ZF[i]<0-1 
            & ZF[i]>-3 
            & CLOSE[i]<MALOW[i]) 
      MS[i]<-"RX"
    else if(ZF[i]<=-3 
            & ZF[i]>-5 
            & CLOSE[i]<MALOW[i]) 
      MS[i]<-"RY"
    else if(ZF[i]<=-5 
            & CLOSE[i]<MALOW[i])
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
  PINZHONG$LOW<-as.numeric(LOW)
  PINZHONG$MALOW<-as.numeric(MALOW)
  
  PINZHONG<-na.omit(PINZHONG)
  IF.1D.1 <<- as.data.frame(PINZHONG)
  
  

  return(as.data.frame(PINZHONG))
}
