ATR <- function(H=HIGH,L=LOW,C=CLOSE,N=14){
  C <- as.numeric(C)
  L <- as.numeric(L)
  H <- as.numeric(H)
  TR<-0
  ATR<-0
  TR[1]<-0
  ATR[1]<-0
  for(i in 2:length(C)){
    TR[i] <- max((H[i]-L[i]),abs(C[i-1]-H[i]),abs(C[i-1]-L[i]))
  }  
  for(j in 2:length(C)){
    if(j <= N)
    {
      ATR[j] <- TR[j]
    }
    if(j > N)
    {
      ATR[j] <- mean(TR[j-N]:TR[j])
    }   
  }     
  return(ATR)
}