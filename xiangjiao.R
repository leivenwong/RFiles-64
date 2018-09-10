xiangjiao<-function(DATE="2013-06-24")
{
  PINZHONG<-shujuzhunbei.shangpin3("RU","1D",10,3,2,1)
  MS3<-moshi3.shangpin(PINZHONG,"1997-01-01",DATE)
  PINZHONG<-shujuzhunbei.shangpin1("RU","1D",5,3,2,1)
  MS1<-moshi1.shangpin(PINZHONG,"1997-01-01",DATE)

  
  if (MS3>58) MS3JG<-"UP"
  if (MS3<58) MS3JG<-"DOWN"
  if (MS3==58) MS3JG<-"58"
  if (MS3==0) MS3JG<-"NO SIGNAL"
  if (MS1>56) MS1JG<-"UP"
  if (MS1<56) MS1JG<-"DOWN"
  if (MS1==56) MS1JG<-"56"
  if (MS1==0) MS1JG<-"NO SIGNAL"
  MS<-c("MS3","MS1")
  ADVANTAGE<-c(MS3,MS1)
  SIGNAL<-c(MS3JG,MS1JG)
  jg<-data.frame(MS,ADVANTAGE,SIGNAL)

  return(jg)
}
