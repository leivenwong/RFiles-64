WangAdjustStop <-function(type, level, time, time_string, open, high, low, close) {
  # wang����Ҫʵ��һ�������ĺ����������������ɾ��
  # �����������֪ͨR�����ڵ�5s���飬��Ҫ����֮ǰWangPositionCost��õ���Ϣ���ж��Ƿ�Ҫ�����µ�ֹ��ֵ
  
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