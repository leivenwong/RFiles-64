WangFuncitontest <- function(type="IF1308", level="MQ5S", time=Sys.time(), time_string="2013-08-14 15:15:00", open=2222, high=2222, low=2222, close=2222) {
  
  
  # 1 type string 品种
  # 2 level string 级别，MQ1S，MQ5S，MQ5M，MQ15M
  # 3 double time utc时间的微秒数目
  # 4 string time_string utc时间的字符串，秒格式
  # 5 double open开盘价
  # 6 double high 最高价
  # 7 double low 最低价
  # 8 double close 收盘价
  
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
    
    
    #其实只需要1、2、7、8、9、10、12
    
    
    #1 name string 策略的名字 “ms.successive.one”
    #2 type string 品种
    #3 action int 1表示采取交易行动，0表示无行动
    #4 oc int 1表示开仓，0表示0平仓
    #5 bs int 1表示买，0表示卖
    #6 action_price double 表示执行基准价格
    #7 buy_limit_price double 表示超过此价格，买取消
    #8 sell_limit_price double 表示低于此价格，卖取消
    #9 position double 表示一手对应的资金量，总资金量/这个数值 = 报单的手数
    #10 force_close_percent double 表示亏损百分比，超过导致强平
    #11 auto_close int 1表示休市前自动平仓
    #12 buy_clean_sell int 1表示必须持多仓，0 表示必须空仓 -1 表示必须持空仓
    
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