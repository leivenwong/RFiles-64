WangAdjustStop <-function(type, level, time, time_string, open, high, low, close) {
  # wang：你要实现一个这样的函数，并把这个函数删除
  # 这个函数用来通知R，现在的5s行情，你要根据之前WangPositionCost获得的信息，判断是否要调整新的止损值
  
  return (NULL)
  
  if (IFdirection.two == 1 && close > IFclost.two*(1+IFpercent.two) 
  {
    out <- list(length=5)
    out[[1]] <- CreateTLVStringObject("R2CTP.Position.StopPrice")
    out[[2]] <- CreateTLVStringObject("ms.successive.if.two")
    out[[3]] <- CreateTLVStringObject(type)
    out[[4]] <- CreateTLVIntObject(IFdirection.two)
    out[[5]] <- CreateTLVDoubleObject(IFcost.two)
    return (out)
  }
    
  if (IFdirection.two == 0-1 && close < IFclost.two*(1-IFpercent.two)
  {
    out <- list(length=5)
    out[[1]] <- CreateTLVStringObject("R2CTP.Position.StopPrice")
    out[[2]] <- CreateTLVStringObject("ms.successive.if.two")
    out[[3]] <- CreateTLVStringObject(type)
    out[[4]] <- CreateTLVIntObject(IFdirection.two)
    out[[5]] <- CreateTLVDoubleObject(IFcost.two)
    return (out)
  }
}