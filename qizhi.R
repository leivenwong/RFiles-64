qizhi<-function(DATE="2013-06-24")
{
  PINZHONG<-shujuzhunbei3(10,5,3,1)
  MS3<-moshi3(PINZHONG,"1997-01-01",DATE)
  PINZHONG<-shujuzhunbei1(10)
  MS1<-moshi1(PINZHONG,"1997-01-01",DATE)
  
  
  if (MS3>57) MS3JG<-"UP"
  if (MS3<57) MS3JG<-"DOWN"
  if (MS3==57) MS3JG<-"57"
  if (MS3==0) MS3JG<-"NO SIGNAL"
  if (MS1>55) MS1JG<-"UP"
  if (MS1<55) MS1JG<-"DOWN"
  if (MS1==55) MS1JG<-"55"
  if (MS1==0) MS1JG<-"NO SIGNAL"
  MS<-c("MS3","MS1")
  ADVANTAGE<-c(MS3,MS1)
  SIGNAL<-c(MS3JG,MS1JG)
  jg<-data.frame(MS,ADVANTAGE,SIGNAL)
  
  return(jg)
}
