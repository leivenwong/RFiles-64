EMA<-function(BIANLIANG=CLOSE,N=5)
{ 

  EMAZHI<-0

  BIANLIANG<-as.numeric(BIANLIANG)
  EMAZHI[1]<-BIANLIANG[1]
      
  for(i in 2:length(BIANLIANG))
  
  EMAZHI[i]<-(2*BIANLIANG[i]+(N-1)*EMAZHI[i-1])/(N+1)  
  
  return(EMAZHI)
  
}