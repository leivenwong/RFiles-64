moshi5<-function(id=SH000001.1D,Begt="1997-01-01",Endt="2013-06-19")
{
  DANGQIANSHIJIAN<-Endt
  KAISHISHIJIAN<-Begt
  DAIMA<-id
  PINZHONG<-qushu(DAIMA,KAISHISHIJIAN,DANGQIANSHIJIAN)
  WEEK=weekdays(as.Date(DANGQIANSHIJIAN))
  jishu<-length(PINZHONG$DATETIME)-1
  MS<-PINZHONG$MS
  NEXTMS<-PINZHONG$NEXTMS
  DANGTIANMS<-MS[which(as.Date(PINZHONG$DATETIME)==as.Date(DANGQIANSHIJIAN))]
  if(any(as.Date(DANGQIANSHIJIAN)==PINZHONG$DATETIME)==FALSE)
    return(0)
  qa1<-0
  qb1<-0
  qc1<-0
  qd1<-0
  qe1<-0
  qf1<-0
  qg1<-0
  qh1<-0
  qs1<-0
  qt1<-0
  qu1<-0
  qv1<-0
  qw1<-0
  qx1<-0
  qy1<-0
  qz1<-0
  ra1<-0
  rb1<-0
  rc1<-0
  rd1<-0
  re1<-0
  rf1<-0
  rg1<-0
  rh1<-0
  rs1<-0
  rt1<-0
  ru1<-0
  rv1<-0
  rw1<-0
  rx1<-0
  ry1<-0
  rz1<-0
  jg<-0
  i<- 1
  while(i<jishu)
  {
    if (NEXTMS[i]=="QA" &  MS[i]==DANGTIANMS)  qa1<-qa1+1
    if (NEXTMS[i]=="QB" &  MS[i]==DANGTIANMS)  qb1<-qb1+1
    if (NEXTMS[i]=="QC" &  MS[i]==DANGTIANMS)  qc1<-qc1+1
    if (NEXTMS[i]=="QD" &  MS[i]==DANGTIANMS)  qd1<-qd1+1
    if (NEXTMS[i]=="QE" &  MS[i]==DANGTIANMS)  qe1<-qe1+1
    if (NEXTMS[i]=="QF" &  MS[i]==DANGTIANMS)  qf1<-qf1+1
    if (NEXTMS[i]=="QG" &  MS[i]==DANGTIANMS)  qg1<-qg1+1
    if (NEXTMS[i]=="QH" &  MS[i]==DANGTIANMS)  qh1<-qh1+1
    if (NEXTMS[i]=="QS" &  MS[i]==DANGTIANMS)  qs1<-qs1+1
    if (NEXTMS[i]=="QT" &  MS[i]==DANGTIANMS)  qt1<-qt1+1
    if (NEXTMS[i]=="QU" &  MS[i]==DANGTIANMS)  qu1<-qu1+1
    if (NEXTMS[i]=="QV" &  MS[i]==DANGTIANMS)  qv1<-qv1+1
    if (NEXTMS[i]=="QW" &  MS[i]==DANGTIANMS)  qw1<-qw1+1
    if (NEXTMS[i]=="QX" &  MS[i]==DANGTIANMS)  qx1<-qx1+1
    if (NEXTMS[i]=="QY" &  MS[i]==DANGTIANMS)  qy1<-qy1+1
    if (NEXTMS[i]=="QZ" &  MS[i]==DANGTIANMS)  qz1<-qz1+1
    if (NEXTMS[i]=="RA" &  MS[i]==DANGTIANMS)  ra1<-ra1+1
    if (NEXTMS[i]=="RB" &  MS[i]==DANGTIANMS)  rb1<-rb1+1
    if (NEXTMS[i]=="RC" &  MS[i]==DANGTIANMS)  rc1<-rc1+1
    if (NEXTMS[i]=="RD" &  MS[i]==DANGTIANMS)  rd1<-rd1+1
    if (NEXTMS[i]=="RE" &  MS[i]==DANGTIANMS)  re1<-re1+1
    if (NEXTMS[i]=="RF" &  MS[i]==DANGTIANMS)  rf1<-rf1+1
    if (NEXTMS[i]=="RG" &  MS[i]==DANGTIANMS)  rg1<-rg1+1
    if (NEXTMS[i]=="RH" &  MS[i]==DANGTIANMS)  rh1<-rh1+1
    if (NEXTMS[i]=="RS" &  MS[i]==DANGTIANMS)  rs1<-rs1+1
    if (NEXTMS[i]=="RT" &  MS[i]==DANGTIANMS)  rt1<-rt1+1
    if (NEXTMS[i]=="RU" &  MS[i]==DANGTIANMS)  ru1<-ru1+1
    if (NEXTMS[i]=="RV" &  MS[i]==DANGTIANMS)  rv1<-rv1+1
    if (NEXTMS[i]=="RW" &  MS[i]==DANGTIANMS)  rw1<-rw1+1
    if (NEXTMS[i]=="RX" &  MS[i]==DANGTIANMS)  rx1<-rx1+1
    if (NEXTMS[i]=="RY" &  MS[i]==DANGTIANMS)  ry1<-ry1+1
    if (NEXTMS[i]=="RZ" &  MS[i]==DANGTIANMS)  rz1<-rz1+1;
    i<-i+1
  }
  if (qa1+qb1+qc1+qd1+qe1+qf1+qg1+qh1+qs1+qt1+qu1+qv1+qw1+qx1+qy1+qz1+
        ra1+rb1+rc1+rd1+re1+rf1+rg1+rh1+rs1+rt1+ru1+rv1+rw1+rx1+ry1+rz1!=0)
    zonghe1<- qa1+qb1+qc1+qd1+qe1+qf1+qg1+qh1+qs1+qt1+qu1+qv1+qw1+qx1+qy1+qz1+
    ra1+rb1+rc1+rd1+re1+rf1+rg1+rh1+rs1+rt1+ru1+rv1+rw1+rx1+ry1+rz1
  if (qa1+qb1+qc1+qd1+qe1+qf1+qg1+qh1+qs1+qt1+qu1+qv1+qw1+qx1+qy1+qz1+
        ra1+rb1+rc1+rd1+re1+rf1+rg1+rh1+rs1+rt1+ru1+rv1+rw1+rx1+ry1+rz1==0)
    zonghe1<- 1
  
  if ((qa1+qb1+qc1+qd1+qs1+qt1+qu1+qv1
       +ra1+rb1+rc1+rd1+rs1+rt1+ru1+rv1)/zonghe1*100>100)
    jg<-100
  
  if ((qa1+qb1+qc1+qd1+qs1+qt1+qu1+qv1
       +ra1+rb1+rc1+rd1+rs1+rt1+ru1+rv1)/zonghe1*100<=100)
    jg<-(qa1+qb1+qc1+qd1+qs1+qt1+qu1+qv1
         +ra1+rb1+rc1+rd1+rs1+rt1+ru1+rv1)/zonghe1*100
  return(jg)
}
