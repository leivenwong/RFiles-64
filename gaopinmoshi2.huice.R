gaopinmoshi2.huice<-function(id=SH000001.60M,DATABegt="2003-03-12 9:00:00",CSBegt="2009-01-01 09:00:00",CSEndt="2013-06-24 15:00:00")
{
  MS.ADVANTAGE<-0
  MS.DATE<-CSBegt
  jishu<-1
  PINZHONG<-gaopinqushu(x=id,Begt="2009-01-01 09:00:00",Endt="2013-06-24 15:00:00")
  DATETIME<-as.character(PINZHONG$DATETIME)
  for(i in DATETIME)
  {
    MS.DATE[jishu]<-i;
    MS.ADVANTAGE[jishu]<-gaopinmoshi2(id,DATABegt,i);
    jishu<-jishu+1;
    print(jishu)
  }
  MS.HUICE<-list()
  MS.HUICE$DATE<-MS.DATE[which(MS.ADVANTAGE!=0)]
  MS.HUICE$ADVANTAGE<-MS.ADVANTAGE[which(MS.ADVANTAGE!=0)]
  MS.HUICE<-as.data.frame(MS.HUICE)
  return(MS.HUICE)
}
