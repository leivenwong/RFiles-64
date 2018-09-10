gaopinqushu<-function(x=SH000001,Begt="2005-04-18 14:00:00",Endt="2013-06-24 15:00:00")
{
  Begtdate<-as.POSIXct(Begt,tz="")
  Endtdate<-as.POSIXct(Endt,tz="")
  newdata<-subset(x,DATETIME>=Begtdate&DATETIME<=Endtdate)
  return(newdata)
}  