gaopinmoshi3.huice<-function(id=IF01.15M,DATABegt="2010-04-16 9:00:00",CSBegt="2012-07-03 09:00:00",CSEndt="2013-07-03 15:15:00")
{
  MS.ADVANTAGE<-0
  MS.DATE<-CSBegt
  jishu<-1
  PINZHONG<-gaopinqushu(x=id,Begt=CSBegt,Endt=CSEndt)
  DATETIME<-as.character(PINZHONG$DATETIME)
  for(i in as.character(DATETIME))
  {
    MS.DATE[jishu]<-as.character(i);
    MS.ADVANTAGE[jishu]<-gaopinmoshi3(id,DATABegt,as.character(i));
    jishu<-jishu+1;print(jishu)  
  }
  MS.HUICE<-list()
  MS.HUICE$DATE<-MS.DATE
  MS.HUICE$ADVANTAGE<-MS.ADVANTAGE
  MS.HUICE<-as.data.frame(MS.HUICE)
  return(MS.HUICE)
}
