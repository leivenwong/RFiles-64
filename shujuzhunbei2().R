shujuzhunbei2<-function(id="SH000001",cy="1D",JC=10,LC=10)
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
  ZF<-xts((as.numeric(CLOSE)-as.numeric(REFCLOSE))/as.numeric(CLOSE)*100,DATETIME)  
  NEXTCLOSE<-lag(CLOSE,k = -1,na.pad=TRUE)  
  MACLOSE<-rollapply(CLOSE,JC,mean)
  VOL<-as.xts(PINZHONG$VOL,DATETIME)
  MAVOL<-rollapply(VOL,LC,mean)  
  REFMACLOSE<-lag(MACLOSE,k = 1,na.pad=TRUE)  
  REFMAVOL<-lag(MAVOL,k = 1,na.pad=TRUE)
  
  
  MS<-NA
  ZF<-as.numeric(ZF)
  MACLOSE<-as.numeric(MACLOSE)
  REFMACLOSE<-as.numeric(REFMACLOSE)
  MAVOL<-as.numeric(MAVOL)
  REFMAVOL<-as.numeric(REFMAVOL)
  i<- max(JC,LC)+1
  bl<-length(DATETIME)
  while(i<=bl)
  {
    if(ZF[i]>=5 
       & MACLOSE[i]>=REFMACLOSE[i] 
       & MAVOL[i]>=REFMAVOL[i]) 
      MS[i]<-"QA"
    else if(ZF[i]>=3 
            & ZF[i]<5 
            & MACLOSE[i]>=REFMACLOSE[i] 
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"QB" 
    else if(ZF[i]<3 
            & ZF[i]>1 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"QC"
    else if(ZF[i]<=1 
            & ZF[i]>=0 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"QD"
    else if(ZF[i]<0 
            & ZF[i]>=-1 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"QE"
    else if(ZF[i]<0-1 
            & ZF[i]>0-3 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"QF"
    else if(ZF[i]<=-3 
            & ZF[i]>0-5 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"QG"
    else if(ZF[i]<=-5 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"QH" 
    
    else if(ZF[i]>=5 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QS"
    else if(ZF[i]>=3 
            & ZF[i]<5  
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QT"
    else if(ZF[i]<3 
            & ZF[i]>1 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QU"
    else if(ZF[i]<=1 
            & ZF[i]>=0 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QV"
    else if(ZF[i]<0 
            & ZF[i]>=-1 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QW"
    else if(ZF[i]<0-1 
            & ZF[i]>-3 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QX"
    else if(ZF[i]<=0-3 
            & ZF[i]>-5 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QY"
    else if(ZF[i]<=-5 
            & MACLOSE[i]>=REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"QZ" 
    
    else if(ZF[i]>=5 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RA"
    else if(ZF[i]>=3 
            & ZF[i]<5 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RB"
    else if(ZF[i]<3 
            & ZF[i]>1 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RC"
    else if(ZF[i]<=1 
            & ZF[i]>=0 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RD"
    else if(ZF[i]<0 
            & ZF[i]>=-1 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RE"
    else if(ZF[i]<0-1 
            & ZF[i]>-3 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RF"
    else if(ZF[i]<=0-3 
            & ZF[i]>-5 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RG"
    else if(ZF[i]<=-5 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]>=REFMAVOL[i])
      MS[i]<-"RH"
    
    else if(ZF[i]>=5 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"RS"
    else if(ZF[i]>=3 
            & ZF[i]<5  
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"RT"
    else if(ZF[i]<3 
            & ZF[i]>1 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"RU"
    else if(ZF[i]<=1 
            & ZF[i]>=0 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"RV"
    else if(ZF[i]<0 
            & ZF[i]>=-1 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"RW"
    else if(ZF[i]<0-1 
            & ZF[i]>-3 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"RX"
    else if(ZF[i]<=-3 
            & ZF[i]>-5 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
      MS[i]<-"RY"
    else if(ZF[i]<=-5 
            & MACLOSE[i]<REFMACLOSE[i]  
            & MAVOL[i]<REFMAVOL[i])
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
  PINZHONG$MAVOL<-as.numeric(MAVOL) 
  PINZHONG$REFMACLOSE<-as.numeric(REFMACLOSE)
  PINZHONG$REFMAVOL<-as.numeric(REFMAVOL)
  PINZHONG$MS<-MS
  PINZHONG$NEXTMS<-NEXTMS
  PINZHONG$DATETIME<-DATETIME
  PINZHONG$DATE<-as.character(DATETIME)
  
  PINZHONG<-na.omit(PINZHONG)
  
  write.csv(PINZHONG,paste(DAIMA,cy,"csv",sep="."),row.names = FALSE)
  return(as.data.frame(PINZHONG))
}
