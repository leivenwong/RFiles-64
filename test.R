test<-function()
{
  rongqi<-0
  jishu<-0
  for(i in 1:9)
  {
    for(j in 1:9)
    {
      for(k in 1:9)
      {
        rongqi[jishu]<-i*100+j*10+k
        jishu<-jishu+1
      }
    }
  }
  return (rongqi)
}
