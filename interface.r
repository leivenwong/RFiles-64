WangFunciton <- function(type, level, time, time_string, open, high, low, close) {
  
  
  # 1 type string 品种
  # 2 level string 级别，MQ1S，MQ5S，MQ5M，MQ15M
  # 3 double time utc时间的微秒数目
  # 4 string time_string utc时间的字符串，秒格式
  # 5 double open开盘价
  # 6 double high 最高价
  # 7 double low 最低价
  # 8 double close 收盘价
  
  out <- list()
  
  out[[1]] <- CreateTLVStringObject("ms.successive.one")
  out[[2]] <- CreateTLVStringObject("os.pursueprice")
  out[[3]] <- CreateTLVStringObject(type)
  out[[4]] <- CreateTLVStringObject(level)
  out[[5]] <- CreateTLVDoubleObject(close)
  out[[6]] <- CreateTLVDoubleObject(0.01)
  out[[7]] <- CreateTLVDoubleObject(300000)
  out[[8]] <- CreateTLVIntObject(0)
  out[[9]] <- CreateTLVIntObject(0)
  
  if (type == "IF1404" && level == "MQ1M") {
    
    IFDATE[length(IFDATE)+1] <<- time_string
    IFDATE <<- IFDATE[(length(IFDATE)-480):length(IFDATE)]
    IFOPEN[length(IFOPEN)+1] <<- open
    IFOPEN <<- IFOPEN[(length(IFOPEN)-480):length(IFOPEN)]
    IFHIGH[length(IFHIGH)+1] <<- high
    IFHIGH <<- IFHIGH[(length(IFHIGH)-480):length(IFHIGH)]
    IFLOW[length(IFLOW)+1] <<- low
    IFLOW <<- IFLOW[(length(IFLOW)-480):length(IFLOW)]
    IFCLOSE[length(IFCLOSE)+1] <<- close
    IFCLOSE <<- IFCLOSE[(length(IFCLOSE)-480):length(IFCLOSE)]
    
    PINZHONG<-IFCLOSE[(length(IFCLOSE)-300):length(IFCLOSE)]
    jisuanMMM<-MMM(BIANLIANG=PINZHONG,SHORT=12,LONG=26,MID=9)
    jisuanRRR<-RRR(BIANLIANG=PINZHONG,N=9)    
    
    IFM[length(IFM)+1]<<-jisuanMMM[length(jisuanMMM)]
    IFM <<- IFM[(length(IFM)-480):length(IFM)]
    IFR[length(IFR)+1]<<-jisuanRRR[length(jisuanRRR)]
    IFR <<- IFR[(length(IFR)-480):length(IFR)]
    
    
    #1 name string 策略的名字 “ms.successive.one”
    #2 os_name string 下单策略的名字 “os.fixprice” "os.pursueprice" 分别为定价，追价
    #3 type string 品种
    #4 level string 周期
    #5 price double 表示执行价格
    #6 limit_percent double 表示超过此百分比，交易取消
    #7 position_hand double 表示一手对应的资金量，总资金量/这个数值 = 报单的手数
    #8 action int 1表示有效ms，0表示无效ms
    #9 buy_clean_sell int 1表示必须持多仓，0 表示必须空仓 -1 表示必须持空仓
    
    out[[1]] <-CreateTLVStringObject("ms.successive.one")
    out[[2]] <-CreateTLVStringObject("os.fixprice")
    out[[3]] <-CreateTLVStringObject(type)
    out[[4]] <- CreateTLVStringObject(level)
    out[[5]] <- CreateTLVDoubleObject(close)
    out[[6]] <- CreateTLVDoubleObject(0.01)
    out[[7]] <- CreateTLVDoubleObject(300000)
    out[[8]] <- CreateTLVIntObject(1)
    
    IFout9 <- 0
    if(IFM[length(IFM)] < 50 && IFM[length(IFM)] > IFM[length(IFM)-1] ) 
      IFout9 <- 1
    if(IFM[length(IFM)] >= 50 && IFM[length(IFM)] < IFM[length(IFM)-1] )
      IFout9 <- (0-1)
    if(IFstop == 1 && IFout9 != IFstopdirection)
      IFstop <<- 0
    if(IFstop == 1 && IFout9 == IFstopdirection)
      IFout9 <- 0
    
    out[[9]] <- CreateTLVIntObject(IFout9)
  }
        
    
  
  if(type=="rb1410" && level=="MQ1M")
  {
    
    
    RBDATE[length(RBDATE)+1] <<- time_string
    RBOPEN[length(RBOPEN)+1] <<- open
    RBHIGH[length(RBHIGH)+1] <<- high
    RBLOW[length(RBLOW)+1] <<- low
    RBCLOSE[length(RBCLOSE)+1] <<- close
    
    PINZHONG<-RBCLOSE[(length(RBCLOSE)-300):length(RBCLOSE)]
    jisuanMMM<-MMM(BIANLIANG=PINZHONG,SHORT=12,LONG=26,MID=9)
    jisuanRRR<-RRR(BIANLIANG=PINZHONG,N=9)    
    
    RBM[length(RBM)+1]<<-jisuanMMM[length(jisuanMMM)]
    RBR[length(RBR)+1]<<-jisuanRRR[length(jisuanRRR)]
    
    
    #1 name string 策略的名字 “ms.successive.one”
    #2 os_name string 下单策略的名字 “os.fixprice” "os.pursueprice" 分别为定价，追价
    #3 type string 品种
    #4 level string 周期
    #5 price double 表示执行价格
    #6 limit_percent double 表示超过此百分比，交易取消
    #7 position_hand double 表示一手对应的资金量，总资金量/这个数值 = 报单的手数
    #8 action int 1表示有效ms，0表示无效ms
    #9 buy_clean_sell int 1表示必须持多仓，0 表示必须空仓 -1 表示必须持空仓
    
    out[[1]] <-CreateTLVStringObject("ms.successive.one")
    out[[2]] <-CreateTLVStringObject("os.fixprice")
    out[[3]] <-CreateTLVStringObject(type)
    out[[4]] <- CreateTLVStringObject(level)
    out[[5]] <- CreateTLVDoubleObject(close)
    out[[6]] <- CreateTLVDoubleObject(0.01)
    out[[7]] <- CreateTLVDoubleObject(4000)
    out[[8]] <- CreateTLVIntObject(1)

    rbout9 <- 0
    if(RBM[length(RBM)] < 100 && RBR[length(RBR)] < 50 ) 
      rbout9 <- 1
    if(RBM[length(RBM)] > 0 && RBR[length(RBR)] >= 50 )
      rbout9 <- (0-1)
    if(rbstop == 1 && rbout9 != rbstopdirection)
      rbstop <<- 0
    if(rbstop == 1 && rbout9 == rbstopdirection)
      rbout9 <- 0
 
    out[[9]] <- CreateTLVIntObject(rbout9)
    
    
  }
  
  if(type=="ru1409" && level=="MQ5M")
  {
    
    
    RUTIME[length(RUTIME)+1] <<- time_string
    RUOPEN[length(RUOPEN)+1] <<- open
    RUHIGH[length(RUHIGH)+1] <<- high
    RULOW[length(RULOW)+1] <<- low
    RUCLOSE[length(RUCLOSE)+1] <<- close
    
    PINZHONG<-RUCLOSE[(length(RUCLOSE)-300):length(RUCLOSE)]
    jisuanMMM<-MMM(BIANLIANG=PINZHONG,SHORT=12,LONG=26,MID=9)
    jisuanDDF<-DDF(BIANLIANG=PINZHONG,SHORT=12,LONG=26,MID=9)    
    jisuanDDA<-DDA(BIANLIANG=PINZHONG,SHORT=12,LONG=26,MID=9)
    jisuanEMA5<-EMA(BIANLIANG=PINZHONG,N=5)
    jisuanEMA10<-EMA(BIANLIANG=PINZHONG,N=10)
    
    RUM[length(RUM)+1]<<-jisuanMMM[length(jisuanMMM)]
    RUM <<- RUM[(length(RUM)-480):length(RUM)]
    RUDDF[length(RUDDF)+1]<<-jisuanDDF[length(jisuanDDF)]
    RUDDF <<- RUDDF[(length(RUDDF)-480):length(RUDDF)]
    
    
    
    
    
    #1 name string 策略的名字 “ms.successive.one”
    #2 os_name string 下单策略的名字 “os.fixprice” "os.pursueprice" 分别为定价，追价
    #3 type string 品种
    #4 level string 周期
    #5 price double 表示执行价格
    #6 limit_percent double 表示超过此百分比，交易取消
    #7 position_hand double 表示一手对应的资金量，总资金量/这个数值 = 报单的手数
    #8 action int 1表示有效ms，0表示无效ms
    #9 buy_clean_sell int 1表示必须持多仓，0 表示必须空仓 -1 表示必须持空仓
    
    out[[1]] <-CreateTLVStringObject("ms.successive.one")
    out[[2]] <-CreateTLVStringObject("os.fixprice")
    out[[3]] <-CreateTLVStringObject(type)
    out[[4]] <- CreateTLVStringObject(level)
    out[[5]] <- CreateTLVDoubleObject(close)
    out[[6]] <- CreateTLVDoubleObject(0.01)
    out[[7]] <- CreateTLVDoubleObject(100000)
    out[[8]] <- CreateTLVIntObject(1)
    
    RUout9 <- 0
    if( RUDDF[length(RUDDF)] > 0  )
      RUout9 <- 1
    if( RUDDF[length(RUDDF)] < 0  ) 
      RUout9 <- (0-1)
    
    if(RUstopwin == 1 && RUout9 == 0-RUstopwindirection)
      RUstopwin <<- 0
    if(RUstopwin == 1 && RUout9 == RUstopwindirection)
      RUout9 <- 0    
    
    if(RUstop == 1 && RUout9 ==  0-RUstopdirection)
      RUstop <<- 0
    if(RUstop == 1 && RUout9 == RUstopdirection)
      RUout9 <- 0
    
    
    out[[9]] <- CreateTLVIntObject(RUout9)
    
    
  }

  return (out)
}