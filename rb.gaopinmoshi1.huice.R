rb.gaopinmoshi1.huice<-function(id=RB.60M,DATABegt="2010-05-04 10:00:00",CSBegt="2012-01-01 10:00:00",CSEndt="2013-07-29 15:00:00")
{
  MS.ADVANTAGE<-0
  MS.DATE<-CSBegt
  jishu<-1
  PINZHONG<-gaopinqushu(x=id,Begt=CSBegt,Endt=CSEndt)
  DATETIME<-as.character(PINZHONG$DATETIME)
  for(i in as.character(DATETIME))
  {
    MS.DATE[jishu]<-as.character(i);
    MS.ADVANTAGE[jishu]<-rb.gaopinmoshi1(id,DATABegt,as.character(i));
    jishu<-jishu+1;print(jishu)  
  }
  MS.HUICE<-list()
  MS.HUICE$DATE<-MS.DATE
  MS.HUICE$ADVANTAGE<-MS.ADVANTAGE
  MS.HUICE<-as.data.frame(MS.HUICE)
  return(MS.HUICE)
}
