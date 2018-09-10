canshubianli<-function(HUICE.id="SH000001",NEXT.id="HS300",cy="1D",MS1="3",MS2="1",QIBUZICHAN=300000,
                 MS1FZ=57,MS2FZ=56,DUOZHISUN=-0.9,KONGZHISUN=0.8,GAOKAIFZ=1.2,DIKAIFZ=-1.4,CHONGJICHENGBEN=1)
{
  JG<-matrix(nrow=21,ncol=21)
  for(i in 50:70)
  { 
    for(j in 50:70)
    {
      JG[i-49,j-49]<-youhua(HUICE.id="SH000001",NEXT.id="HS300",cy="1D",MS1="3",MS2="1",QIBUZICHAN=300000,
                     MS1FZ=i,MS2FZ=j,DUOZHISUN=-0.9,KONGZHISUN=0.8,GAOKAIFZ=1.2,DIKAIFZ=-1.4,CHONGJICHENGBEN=1)
      print(i) 
      print(j)
    }
  }  
}