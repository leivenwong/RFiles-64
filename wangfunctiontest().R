WangFuncitontest <- function(type="IF1308", level="MQ5S", time=Sys.time(), time_string="2013-08-14 15:15:00", open=2222, high=2222, low=2222, close=2222) {
  
  
  # 1 type string Ʒ��
  # 2 level string ����MQ1S��MQ5S��MQ5M��MQ15M
  # 3 double time utcʱ���΢����Ŀ
  # 4 string time_string utcʱ����ַ��������ʽ
  # 5 double open���̼�
  # 6 double high ��߼�
  # 7 double low ��ͼ�
  # 8 double close ���̼�
  
  out <- list()
  
  if(type=="IF1308" && level=="MQ5S")
  {
    
    
    DATE[length(DATE)+1] <<- time_string
    OPEN[length(OPEN)+1] <<- open
    HIGH[length(HIGH)+1] <<- high
    LOW[length(LOW)+1] <<- low
    CLOSE[length(CLOSE)+1] <<- close
    
    PINZHONG<-CLOSE[(length(CLOSE)-300):length(CLOSE)]
    jisuanMMM<-MMM(BIANLIANG=PINZHONG,SHORT=12,LONG=26,MID=9)
    jisuanRRR<-RRR(BIANLIANG=PINZHONG,N=9)    
    
    M[length(M)+1]<<-jisuanMMM[length(jisuanMMM)]
    R[length(R)+1]<<-jisuanRRR[length(jisuanRRR)]
    
    
    #��ʵֻ��Ҫ1��2��7��8��9��10��12
    
    
    #1 name string ���Ե����� ��ms.successive.one��
    #2 type string Ʒ��
    #3 action int 1��ʾ��ȡ�����ж���0��ʾ���ж�
    #4 oc int 1��ʾ���֣�0��ʾ0ƽ��
    #5 bs int 1��ʾ��0��ʾ��
    #6 action_price double ��ʾִ�л�׼�۸�
    #7 buy_limit_price double ��ʾ�����˼۸���ȡ��
    #8 sell_limit_price double ��ʾ���ڴ˼۸���ȡ��
    #9 position double ��ʾһ�ֶ�Ӧ���ʽ��������ʽ���/�����ֵ = ����������
    #10 force_close_percent double ��ʾ����ٷֱȣ���������ǿƽ
    #11 auto_close int 1��ʾ����ǰ�Զ�ƽ��
    #12 buy_clean_sell int 1��ʾ����ֶ�֣�0 ��ʾ����ղ� -1 ��ʾ����ֿղ�
    
    out[[1]] <-CreateTLVStringObject("ms.successive.one")
    out[[2]] <-CreateTLVStringObject("IF1308")
    out[[3]] <- CreateTLVIntObject(1)
    out[[4]] <- CreateTLVIntObject(100)
    out[[5]] <- CreateTLVIntObject(100)
    out[[6]] <- CreateTLVDoubleObject(close)
    out[[7]] <- CreateTLVDoubleObject(0.01)
    out[[8]] <- CreateTLVDoubleObject(-0.01)
    out[[9]] <- CreateTLVDoubleObject(300000)
    out[[10]] <- CreateTLVDoubleObject(0.005)
    out[[11]] <- CreateTLVIntObject(0)
    print(M[length(M)])
    print(R[length(R)])
    if(M[length(M)] > 50 && R[length(R)] > 50) 
    {out[[12]] <- CreateTLVIntObject(1)}
    if(M[length(M)] < 50 && R[length(R)] < 50)
    {out[[12]] <-CreateTLVIntObject(0-1)}
    else 
    {out[[12]] <- CreateTLVIntObject(0)}
  }
  else
  {
    
    out[[1]] <-CreateTLVStringObject("ms.successive.one")
    out[[2]] <-CreateTLVStringObject(type)
    out[[3]] <- CreateTLVIntObject(0)
    out[[4]] <- CreateTLVIntObject(100)
    out[[5]] <- CreateTLVIntObject(100)
    out[[6]] <- CreateTLVDoubleObject(0)
    out[[7]] <- CreateTLVDoubleObject(0)
    out[[8]] <- CreateTLVDoubleObject(0)
    out[[9]] <- CreateTLVDoubleObject(0)
    out[[10]] <- CreateTLVDoubleObject(0)
    out[[11]] <- CreateTLVIntObject(0)
    out[[12]] <- CreateTLVIntObject(0)
    
  }
  
  print(DATE[length(DATE)])
  return (out)
}