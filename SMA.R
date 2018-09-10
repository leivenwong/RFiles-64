SMA<-function(BIANLIANG=CLOSE,N=9,M=1)
{ 
  
  SMAZHI<-0
  
  BIANLIANG<-as.numeric(BIANLIANG)
  SMAZHI[1]<-BIANLIANG[1]
  
  for(i in 2:length(BIANLIANG))
    
    SMAZHI[i]<-(BIANLIANG[i]*M+(N-M)*SMAZHI[i-1])/N
  
  return(SMAZHI)
  
}