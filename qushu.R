qushu<-function(x=SH000001,Begt="1997-01-01",Endt="2013-06-24")
{
Begtdate<-as.Date(Begt)
Endtdate<-as.Date(Endt)
newdata<-subset(x,DATETIME>=Begtdate&DATETIME<=Endtdate)
return(newdata)
}  